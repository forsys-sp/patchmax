#' @title Patchmax Class
#'
#' @description Patchmax patch selection object. Use params active binding to
#' view class parameters or set mutiple parameters at once (using a named list).
#' 
#' @examples 
#' geom <- patchmax::test_forest
#' pm <- patchmax$new(geom, 'stand_id', 'priority1', 'area_ha', 10000)
#' pm <- pm$params = list(constraint_field = 'priority4', constraint_max = 6, area_min=800)
#' pm$search(sample_frac = 1, plot_search = T)
#' pm$build()$plot(show_seed = TRUE)$record()
#'
#' @import R6
#' @import dplyr
#' @import ggplot2
#' @import cppRouting
#' @import sf
#' @import furrr
#' @importFrom igraph V V<- vertex_attr graph_from_data_frame edge_attr<- vertex_attr<- delete_vertices E
#'
#' @export

patchmax_generator <- R6::R6Class(
  classname = "patch_generator",
  
  # //////////////////////////////////////////////////////////////////////////
  # PUBLIC ELEMENTS
  # external methods for running patchmax
  # //////////////////////////////////////////////////////////////////////////
  
  public = list(
    
    #' @description Initialize new patchmax object for building patches
    #' @param geom sf Dataframe like sf object with geomerty
    #' @param id_field character Field name containing unique IDs
    #' @param objective_field character Field name containing objective values
    #' @param area_field character Field name containing area
    #' @param area_max numeric Size of patch
    #' 
    initialize = function(geom, id_field=NULL, objective_field=NULL, area_field=NULL, area_max=NULL){
      if(!missing(geom)){
        
        # save geometry
        private$..geom <- geom
        
        # parameters
        private$..param_id_field = id_field
        private$..param_objective_field = objective_field
        private$..param_area_field = area_field
        private$..param_area_max = area_max 
        
        # setup key fields in geometry object
        private$..geom$stand_id = as.character(dplyr::pull(private$..geom, id_field))
        private$..geom$patch_id = 0
        private$..geom$include = 1
        
        # build adjacency network
        private$..net <- calc_adj_network_func(
          shp = geom, 
          St_id = dplyr::pull(geom, id_field), 
          calc_dist = TRUE)
        
        # add addition fields to adjacency network
        vertex_attr(private$..net) <- bind_cols(vertex_attr(private$..net), st_drop_geometry(geom))
        vertex_attr(private$..net, name = 'exclude') = 0
        vertex_attr(private$..net, name = 'patch_id') = 0
        vertex_attr(private$..net, name = 'include') = 1
        
        private$..refresh_net_attr()
        
      } else {
        stop('Geometry required')
      }
    },
    
    
    #' @description Build patch at selected node
    #' @param node character. Stand ID used to build patch (optional)
    #' @details If node is NULL, use stand id found during search
    #' 
    build = function(node=NULL){
      
      if(is.null(node)){
        node <- private$..pending_origin
      }
      
      private$..check_req_fields()
      
      patch <- build_patch_func(
        start_node = node, 
        cpp_graph = private$..update_dist(), 
        net = private$..update_net(),
        a_max = private$..param_area_max,
        a_min = private$..param_area_min,
        c_max = private$..param_constraint_max,
        c_min = private$..param_constraint_min)
      
      #evaluate patch here
      summarize_patch <- function(){
        table(patch$constraint_met)
        patch[1:max(which(patch$constraint_met == TRUE)),] %>% 
          dplyr::select(matches('^area$|^objective$|^constraint$|^include$')) %>%
          summarize_if(is.numeric, sum)
      }
      
      # append auxiliary data
      patch_dat <- vertex_attr(private$..net) %>% 
        dplyr::select(node = name, private$..param_objective_field, include)
      patch <- dplyr::left_join(patch, patch_dat, by='node')
      
      private$..pending_patch <- patch
      
      message(glue::glue('Patch stats: start {patch$node[1]}, area {round(sum(patch$area),3)}, score {round(sum(patch$objective),3)}, constraint {round(sum(patch$constraint),3)}, excluded {100-round(sum(patch$include)/nrow(patch)*100)}%'))
      message(glue::glue('Constraints met: area {patch$area_met[nrow(patch)]}, secondary: {patch$constraint_met[nrow(patch)]}'))
      
      return(invisible(self))
    },
    
    
    #' @description Search patch origin with highest objective
    #' @param sample_frac numeric Fraction of stands to evaluate (0-1)
    #' @param return_all logical Return search results
    #' @param show_progress logical Show search progress bar
    #' @param plot_search logical Map search results
    #' 
    search = function(sample_frac=0.1, return_all=FALSE, show_progress=FALSE, plot_search=FALSE){
      
      private$..check_req_fields()
      private$..update_dist()
      
      if(plot_search){
        return_all = TRUE
      }
      
      nodes <- sample_frac(private$..geom, sample_frac, TRUE)
      
      search_out <- search_best_func(
        cpp_graph = private$..update_dist(), 
        net = private$..update_net(), 
        nodes = nodes,
        objective_field = private$..param_objective_field, 
        a_max = private$..param_area_max,
        a_min = private$..param_area_min,
        c_max = private$..param_constraint_max,
        c_min = private$..param_constraint_min,
        return_all = return_all,
        show_progress = show_progress)
      
      best_out = names(search_out)[which.max(search_out)]
      message(glue::glue('\nBest start: {best_out} ({round(sample_frac*100)}% search)'))
      private$..pending_origin <- best_out
      
      # plot search results (mainly for debugging)
      if(plot_search){
        pdat <- dplyr::inner_join(
          x = private$..geom, 
          y = data.frame(search_out) %>% tibble::rownames_to_column(), 
          by=c('stand_id'='rowname'))
        origin <- pdat[pdat$search_out == max(pdat$search_out, na.rm=TRUE),]
        p1 <- ggplot() + 
          geom_sf(data=pdat, aes(fill=search_out), linewidth=0) +
          geom_sf(data=suppressWarnings(st_centroid(origin)), size=4, shape=5) +
          scale_fill_gradientn(colors = sf.colors(10)) +
          theme(legend.position = 'bottom') +
          theme_void() 
        p2 <- data.frame(score = sort(pdat$search_out, decreasing = T)) %>% 
          dplyr::mutate(rank = 1:n()) %>%
          ggplot(aes(x=rank, y=score, color=score)) + 
          geom_point(size=5, shape=15) +
          scale_color_gradientn(colors = sf.colors(10)) + 
          theme_minimal()
        print(cowplot::plot_grid(p1, p2, ncol = 1))
      }
      
      if(return_all){
        x = private$..geom
        index = match(names(search_out), dplyr::pull(x,private$..param_id_field))
        x[index,'search_score'] <- search_out
        private$..geom = x
      }
      
      return(invisible(self))
    },
    
    #' @description Search, build, and record multiple patches in sequence
    #' @param n_projects integer. Number of patches to build
    #' @param sample_frac numeric. Fraction of stands to evaluate for patches
    
    simulate = function(n_projects = 1, sample_frac = 0.1){
      for(i in 1:n_projects){
        self$search(sample_frac = sample_frac)$build()$record() 
      }
    },
    
    #' @description Plot patch and stand map
    #' @param plot_field character Field name to plot
    #' @param return_plot logical Return ggplot object
    #' @param enforce_constraint logical Apply secondary constraint
    #' @param show_seed logical Show origin used to build selected patch?
    
    plot = function(plot_field = NULL, 
                    return_plot = FALSE, 
                    enforce_constraint = TRUE, 
                    show_seed = FALSE){
      
      if(is.null(private$..pending_patch)){
        message('No patch currently selected')
      } else {
        patch = private$..geom %>% 
          dplyr::select(stand_id) %>% 
          dplyr::rename(node = 1) %>% 
          inner_join(private$..pending_patch, by='node') %>%
          mutate(include = factor(include)) %>%
          arrange(dist)
      }
      
      plot_field <- ifelse(is.null(plot_field),  private$..param_objective_field, plot_field)
      
      patches = private$..geom %>% 
        group_by(patch_id) %>% 
        summarize() %>% 
        filter(patch_id != 0)
      
      plot = ggplot() + 
        geom_sf(data=private$..geom, aes(fill=get(plot_field)), linewidth=0) + 
        scale_fill_gradientn(colors = sf.colors(10)) +
        guides(fill = guide_legend(plot_field)) +
        theme(legend.position = 'bottom') +
        theme_void() 
      
      if(!is.null(private$..pending_patch)){
        plot = plot + 
          geom_sf(data=suppressWarnings(st_centroid(patch)), aes(shape=include), size=5) +
          scale_shape_manual(values=c(4,16), breaks=c(0,1))
        if(show_seed){
          node_0 = suppressWarnings(st_centroid(patch[patch$dist == 0,]))
          plot = plot + geom_sf(data = node_0, color='black', size=5, shape=15)
        }
      }
      
      if(nrow(patches) > 0){
        plot = plot +
          geom_sf(data=patches, fill=NA, linewidth=1, color='black') +
          geom_sf_text(data=patches, aes(label=patch_id))
      }
      
      if(return_plot){
        return(plot)
      } else {
        print(plot) 
      }
      return(invisible(self))
    },
    
    # RECORD METHOD --------------------------------------------------------
    #' @description 
    #' Record selected patch
    #' @param patch_id integer/character Patch name. If null, add one to highest
    #' @param enforce_constraint logical Apply secondary constraint
    #' 
    record = function(patch_id = NULL, enforce_constraint = TRUE){
      if(is.null(private$..pending_patch))
        stop('No patch. Run search or select first.')
      
      # generate patch id if missing
      if(is.null(patch_id)){
        patch_id = max(V(private$..net)$patch_id) + 1
      }
      
      patch <- private$..pending_patch
      
      # record patch id
      m = match(private$..pending_patch$node, vertex_attr(private$..net, private$..param_id_field))
      V(private$..net)$patch_id[m] = patch_id
      
      m = match(private$..pending_patch$node, private$..geom %>% pull(private$..param_id_field))
      private$..geom$patch_id[m] = patch_id
      
      message(glue::glue('Patch {patch_id} recorded\n-------------'))
      private$..pending_patch <- NULL
      
      return(invisible(self))
    },
    
    #' @description 
    #' Summarize recorded patches
    describe = function(){
      private$..geom %>% 
        st_drop_geometry() %>% 
        group_by(patch_id) %>% 
        summarize_if(is.numeric, sum)
    },
    
    #' @description 
    #' Reset recorded patches
    reset = function(){
      message('Selected patches have been deleted')
      private$..geom$patch_id = 0
      V(private$..net)$patch_id = 0
      private$..pending_patch = NULL
      private$..pending_origin = NULL
    }
  ),
  
  # //////////////////////////////////////////////////////////////////////////
  # PRIVATE ELEMENTS
  # internal aspects of the patchmax generator
  # ////////////////////////////////////////////////////////////////////////// 
  
  private = list(
    ..net = NULL,
    ..geom = NULL,
    ..cpp_graph = NULL,
    ..param_id_field = NULL,
    ..param_objective_field = NULL,
    ..param_area_field = NULL,
    ..param_area_max = NULL,
    ..param_area_min = -Inf,
    ..param_threshold = NULL,
    ..param_threshold_area_adjust = 0,
    ..param_threshold_objective_adjust = 0,
    ..param_constraint_field = NULL,
    ..param_constraint_max = Inf,
    ..param_constraint_min = -Inf,
    ..param_sdw = 0,
    ..param_epw = 0,
    ..pending_patch = NULL,
    ..pending_origin = NULL,
    ..record_stands = NULL,
    ..record_patches = NULL,

    # build cpp graph object     
    ..update_dist = function(){
      cpp_graph <- build_graph_func(
        net = delete_vertices(private$..net, V(private$..net)$patch_id > 0),
        objective_field = private$..param_objective_field, 
        sdw = private$..param_sdw, 
        epw = private$..param_epw)
      return(cpp_graph)
    },
    
    # update adjacency network
    ..update_net = function(){
      
      net = private$..net
      
      # modify area and distance based on threshold 
      net <- set_threshold_func(
        net = net, 
        include = V(net)$include, 
        area_adjust = private$..param_threshold_area_adjust, 
        objective_adjust = private$..param_threshold_objective_adjust)
      
      # remove stands assigned a patch id
      net <- delete_vertices(net, V(net)$patch_id > 0)
      return(net)
    },
    
    # set threshold provided by the user
    ..set_threshold = function(){
      if(!is.null(private$..param_threshold)){
        net <- private$..net
        s_txt = private$..param_threshold
        id = private$..param_id_field
        all_ids = dplyr::pull(vertex_attr(net), id)   
        include_ids = subset(vertex_attr(net), eval(parse(text = s_txt))) %>% pull(id)
        V(private$..net)$include = ifelse(all_ids %in% include_ids, 1, 0)
      } else
        V(private$..net)$include = 1
    },

    # check that required fields are provided
    ..check_req_fields = function(){
      if(is.null(private$..param_id_field))
        stop('id field is missing')
      if(is.null(private$..param_objective_field))
        stop('objective field is missing')
      if(is.null(private$..param_area_max))
        stop('patch area is missing')
      if(is.null(private$..param_area_field))
        stop('area field is missing')
    },

    # (re)assign objective, area, and constraints to indicated fields
    ..refresh_net_attr = function(){
      if(!is.null(private$..param_objective_field)){
        vertex_attr(private$..net, name = 'objective') <- 
          vertex_attr(private$..net, private$..param_objective_field)
      }
      if(!is.null(private$..param_area_field)){
        vertex_attr(private$..net, name = 'area') <- 
          vertex_attr(private$..net, private$..param_area_field)
      }
      if(!is.null(private$..param_constraint_field)){
        vertex_attr(private$..net, name = 'constraint') <- 
          vertex_attr(private$..net, private$..param_constraint_field)
      }
    }
  ),

  # //////////////////////////////////////////////////////////////////////////
  # ACTIVE BINDINGS 
  # used for getting and setting private elements
  # //////////////////////////////////////////////////////////////////////////
  
  active = list(
    
    #' @field net Get igraph object. Read only
    #' 
    net = function(){
      private$..net
    },
    #' @field geom Get sf geometry object. Read only
    geom = function(){
      private$..geom
    },
    #' @field best Get pending stand id representing best patch origin. Read only
    best = function(){
      private$..pending_origin
    },
    #' @field patch Get stands in pending patch. Read only
    patch = function(){
      if(is.null(private$..pending_patch)){
        message('No project selected. Please run pm$build()')
      } else {
        private$..pending_patch
      }
    },
    #' @field id_field Get/set stand ID field
    id_field = function(value){
      if(missing(value)){
        private$..param_id_field
      } else {
        assertive::assert_is_character(value)
        assertive::is_of_length(value, 1)
        private$..param_id_field <- value
        private$..refresh_net_attr()
      }
    },
    #' @field objective_field Get/set objective field
    objective_field = function(value){
      if(missing(value)){
        private$..param_objective_field
      } else {
        assertive::assert_is_character(value)
        assertive::is_of_length(value, 1)
        private$..param_objective_field <- value
        private$..refresh_net_attr()
      }
    },
    #' @field area_field Get/set area field
    area_field = function(value){
      if(missing(value)){
        private$..param_area_field
      } else {
        assertive::assert_is_character(value)
        assertive::is_of_length(value, 1)
        private$..param_area_field <- value
        private$..refresh_net_attr()
      }
    },
    #' @field threshold Get/set threshold boolean statement
    threshold = function(value){
      if(missing(value)){
        private$..param_threshold
      } else {
        private$..param_threshold <- value
        private$..refresh_net_attr()
        private$..set_threshold()
      }
    },
    #' @field threshold_area_adjust Get/set fraction of area to count within excluded stands
    threshold_area_adjust = function(value){
      if(missing(value)){
        private$..param_threshold_area_adjust
      } else {
        private$..param_threshold_area_adjust <- value
        private$..refresh_net_attr()
        private$..set_threshold()
      }
    },
    #' @field threshold_objective_adjust Get/set fraction of objective to count within excluded stands
    threshold_objective_adjust = function(value){
      if(missing(value)){
        private$..param_threshold_objective_adjust
      } else {
        private$..param_threshold_objective_adjust <- value
        private$..refresh_net_attr()
        private$..set_threshold()
      }
    },
    #' @field constraint_field Get/set secondary constraint field. Optional
    constraint_field = function(value){
      if(missing(value)){
        private$..param_constraint_field
      } else {
        assertive::assert_is_character(value)
        assertive::is_of_length(value, 1)
        private$..param_constraint_field <- value
        private$..refresh_net_attr()
      }
    },
    #' @field constraint_max Get/set max value for secondary constraint (e.g., max budget)
    constraint_max = function(value){
      if(missing(value)){
        private$..param_constraint_max
      } else {
        assertive::assert_is_numeric(value)
        assertive::is_of_length(value, 1)
        private$..param_constraint_max <- value
        private$..refresh_net_attr()
      }
    },
    #' @field constraint_min Get/set min value for secondary constraint
    constraint_min = function(value){
      if(missing(value)){
        private$..param_constraint_min
      } else {
        assertive::assert_is_numeric(value)
        assertive::is_of_length(value, 1)
        private$..param_constraint_min <- value
        private$..refresh_net_attr()
      }
    },
    #' @field area_max Get/set max value for area constraint
    area_max = function(value){
      if(missing(value)){
        private$..param_area_max
      } else {
        assertive::assert_is_numeric(value)
        assertive::is_of_length(value, 1)
        assertive::is_positive(value)
        private$..param_area_max <- value
        private$..refresh_net_attr()
      }
    },
    #' @field area_min Get/set min value for area constraint
    area_min = function(value){
      if(missing(value)){
        private$..param_area_min
      } else {
        assertive::assert_is_numeric(value)
        assertive::is_of_length(value, 1)
        assertive::is_in_range(value, 0, 1, FALSE, FALSE)
        private$..param_area_min <- value
        private$..refresh_net_attr()
      }
    },
    #' @field epw Get/set exclusion penality weight between -1 and 1. Default 0. At 0, patches neither privilege or penalize excluded ares. Values closer to 1 enact a greater cost on projects spanning areas excluded by the project stand threshold. Values less than 0 readily search out excluded areas.
    epw = function(value){
      if(missing(value)){
        private$..param_epw
      } else {
        assertive::assert_is_numeric(value)
        assertive::assert_is_of_length(value, 1)
        assertive::assert_all_are_in_range(value, -1, 1, F, F)
        private$..param_epw <- value
      }
    },
    #' @field sdw Get/set spatial distance weight between -1 and 1. Default 0. Patches are highly constrained by distance at -1 and unconstrained by distance at 1. 
    sdw = function(value){
      if(missing(value)){
        private$..param_sdw
      } else {
        assertive::assert_is_numeric(value)
        assertive::assert_is_of_length(value, 1)
        assertive::assert_all_are_in_range(value, -1, 1, F, F)
        private$..param_sdw <- value
      }
    },
    #' @field params Get/set list of all patch parameters. Arguement is a named list if setting mutiple parameters at once through params active bindings. Ex: patchmax$params = list(constraint_field = 'constraint1', constraint_max = 1000).
    params = function(value){
      if(missing(value)){
        nm <- names(private)
        nm_p <- nm[grepl('..param', nm)]
        params <- nm_p %>% map(function(x) get(x, envir = private))
        names(params) <- gsub('..param_','', nm_p)
        str(params)
      } else {
        assertive::assert_is_list(value)
        for(i in seq_along(value)){
          tryCatch({
            # nm_p = paste0('..param_',names(value)[i])
            nm = names(value)[i]
            assign(nm, value = value[i][[1]], envir = self)
          }, error = function(e){
            # message(paste0('Parameter ', i, ' ', names(value)[i], ' not found'))
            print(e)
          })
        }
      }
    }
  )
)
    