#' @title Patchmax Class
#'
#' @description Patchmax patch selection object. Use params active binding to
#' view class parameters or set multiple parameters at once (using a named list).
#' 
#' @examples 
#' geom <- patchmax::test_forest
#' pm <- patchmax$new(geom, 'id', 'p1', 'ha', 20000)
#' pm$params = list(constraint_field = 'p4', constraint_max = 50, area_min=10000)
#' pm$search(sample_frac = .1, show_progress = T)
#' pm$build()$record()
#' pm$plot()
#'
#' @import R6
#' @import dplyr
#' @import ggplot2
#' @import cppRouting
#' @import sf
#' @import furrr
#' @import assertive
#' @importFrom data.table data.table
#' @importFrom future plan multisession
#' @importFrom igraph V V<- vertex_attr graph_from_data_frame edge_attr<- vertex_attr<- delete_vertices E
#'
#' @export

patchmax <- R6::R6Class(
  classname = "patchmax",
  
  # //////////////////////////////////////////////////////////////////////////
  # PUBLIC ELEMENTS ------------------------------
  # external methods for running patchmax
  # //////////////////////////////////////////////////////////////////////////
  
  public = list(
    
    # ..........................................................................
    #' @description Initialize new patchmax object for building patches
    #' @param geom sf Dataframe like sf object with geometry
    #' @param id_field character Field name containing unique IDs
    #' @param objective_field character Field name containing objective values
    #' @param area_field character Field name containing area
    #' @param area_max numeric Size of patch
    #' @param ... Additional parameters to pass to `patchmax$params`
    #'
    initialize = function(
      geom, 
      id_field=NULL, 
      objective_field=NULL, 
      area_field=NULL, 
      area_max=NULL,
      ...
    ){

      # save parameters
      private$..param_id_field = id_field
      private$..param_objective_field = objective_field
      private$..param_area_field = area_field
      private$..param_area_max = area_max 
      private$..param_constraint_field = area_field

      # save geometry
      self$geom <- geom

      # build adjacency network
      private$..net <- create_network(geom = private$..geom, id_field = id_field)
      
      # add fields to adjacency network
      a <- vertex_attr(private$..net) %>% data.frame()
      b <- st_drop_geometry(private$..geom) %>% rename(name = private$..param_id_field)
      vertex_attr(private$..net) <- left_join(a, b, by='name')
      
      # save optional parameters
      self$params <- list(...)
      private$..refresh_net_attr()
    },
    
    # ..........................................................................
    #' @description Build patch at selected node
    #' @param node character. Stand ID used to build patch (optional)
    #' @details If node is NULL, use stand id found during search
    #' 
    build = function(node=NULL){
      
      if(is.null(node)){
        node <- private$..pending_seed
      }
      
      # early exist and kill switch
      if(length(node)==0){
        message('Cancelling build and flagging stop')
        private$..kill_switch = TRUE
        return(invisible(self))
      }
      
      private$..check_req_fields()
      
      patch <- build_func(
        seed = node, 
        edge_dat = private$..get_edgelist(), 
        node_dat = private$..get_nodelist(),
        a_max = private$..param_area_max,
        a_min = private$..param_area_min,
        c_max = private$..param_constraint_max,
        c_min = private$..param_constraint_min)
      
      # append auxiliary stand data
      aux_data <- vertex_attr(private$..net) %>% 
        select(node = name, 
               private$..param_objective_field, 
               original_area = ..area)
      
      patch <- left_join(patch, aux_data, by='node', suffix = c('','.x'))
      
      # save patch stat and stand data
      private$..pending_patch_stands <- patch
      private$..pending_patch_stats <- calc_patch_stats(patch)
      
      return(invisible(self))
    },
    
    # ..........................................................................
    #' @description Search for patch seed with highest objective
    #' @param show_progress logical Show search progress bar
    #' @param search_plot logical Map search results
    #' @param print_errors logical Print search errors to console
    #' 
    #' @details By default, `set_search_seeds` is NULL. If specified, `sample_frac` is
    #'   overwritten and only the specified stand IDs are used to in the search.
    #'   
    search = function(
      search_plot = FALSE, 
      show_progress = FALSE,
      print_errors = FALSE
    ) {
      
      private$..check_req_fields()
      
      # seed nodes to search
      x <- V(private$..net)$..sample
      nodes <- V(private$..net)$name[x == 1]
      
      message(glue::glue('Searching {round(sum(x)/length(x) * 100)}% of stands...'))
      
      search_out <- search_func(
        edge_dat = private$..get_edgelist(),
        node_dat = private$..get_nodelist(),
        net = private$..get_net(), 
        nodes = nodes,
        objective_field = private$..param_objective_field, 
        a_max = private$..param_area_max,
        a_min = private$..param_area_min,
        c_max = private$..param_constraint_max,
        c_min = private$..param_constraint_min,
        t_limit = private$..param_exclusion_limit,
        show_progress = show_progress, 
        print_errors = print_errors)
      
      # record best patch seed
      best_out = names(search_out)[which.max(search_out)]
      message(glue::glue('Best seed: {best_out}'))
      private$..pending_seed <- best_out
      
      if(search_plot){
        private$..search_plot(search_out)
      }
      
      return(invisible(self))
    },
    
    # ..........................................................................
    #' @description Search, build, and record multiple patches in sequence
    #' @param n_projects integer. Number of patches to build
    #'
    simulate = function(n_projects = 1){
      for(i in 1:n_projects){
        self$search()$build()$record() 
      }
      return(invisible(self))
    },
    
    # ..........................................................................
    #' @description Plot patch and stand map
    #' @param plot_field character Field name to plot
    #' @param return_plot logical Return ggplot object
    #' @param show_seed logical 
    #' @param apply_threshold logical
    #'
    plot = function(
      plot_field = NULL, 
      return_plot = FALSE,
      show_seed = FALSE,
      apply_threshold = FALSE
    ) {

      # base data
      plot_field <- ifelse(is.null(plot_field),  private$..param_objective_field, plot_field)
      geom <- private$..geom
      net <- private$..net
      
      if(apply_threshold){
        patches = geom %>% filter(..include == 1, ..patch_id != 0) 
      } else {
        patches = geom %>% filter(..patch_id != 0)
      }
      
      patches <- patches %>% group_by(..patch_id) %>% summarize()
      
      # add include field for plotting
      geom$..include = vertex_attr(
        net, '..include', 
        match(pull(geom, private$..param_id_field), V(net)$name))
      geom$..include = factor(geom$..include, c(0,1))
      
      plot = ggplot() + 
        geom_sf(data=geom, 
                aes(fill=get(plot_field), alpha=..include), 
                linewidth=0) + 
        scale_alpha_manual(values=c(0.5, 1), breaks=c(0,1)) +
        guides(fill = guide_legend(plot_field)) +
        theme(legend.position = 'bottom') +
        theme_void() 
      
      if(!class(geom %>% pull(get(plot_field))) %in% c('character','logical','factor')){
        plot = plot + scale_fill_gradientn(colors = sf.colors(10))
      }
      
      if(nrow(patches) > 0){
        
        x <- private$..record_patch_stats$seed
        seeds = geom %>% filter(pull(geom, private$..param_id_field) %in% x) %>% select(..patch_id)
        excluded <- geom %>% filter(..patch_id != 0, ..include == 0)
        
        plot <- plot +
          geom_sf(data=patches, fill=rgb(0,0,0,.2), linewidth=1, color='black') +
          geom_sf(data=suppressWarnings(st_centroid(excluded)), shape=4, size=1, alpha=0.5) +
          geom_sf_label(data=seeds, aes(label=..patch_id), label.r = unit(.5, "lines"))
      }
      
      if(return_plot){
        return(plot)
      } else {
        print(plot) 
      }
      return(invisible(self))
    },
    
    # ..........................................................................
    #' @description 
    #' Record selected patch
    #' @param patch_id integer/character Patch name. If null, add one to highest
    #' @param enforce_constraint logical Apply secondary constraint
    #'
    record = function(patch_id = NULL, enforce_constraint = TRUE){
      
      if(is.null(private$..pending_patch_stands)){
        message('No patch has been built')
        return(invisible(self))
      }
      
      # generate patch id if missing
      if(is.null(patch_id)){
        patch_id = max(V(private$..net)$..patch_id) + 1
      }
      
      # pull pending patch and stand data
      patches <- private$..pending_patch_stats
      stands <- private$..pending_patch_stands %>%
        select(!!private$..param_id_field := node, include, objective, area, constraint)
      
      # record data
      patch_stats <- data.frame(patch_id = patch_id, patches)
      patch_stands <- data.frame(patch_id = patch_id, stands)

      private$..record_patch_stats <- bind_rows(private$..record_patch_stats, patch_stats)
      private$..record_patch_stands <- bind_rows(private$..record_patch_stands, patch_stands)
      
      # update adjacency network
      m = match(private$..pending_patch_stands$node, vertex_attr(private$..net, 'name'))
      V(private$..net)$..patch_id[m] = patch_id
      V(private$..net)$..include[m] = patch_stands$include
      
      # update geometry data
      m = match(private$..pending_patch_stands$node, pull(private$..geom, private$..param_id_field))
      private$..geom$..patch_id[m] = patch_id
      private$..geom$..include[m] = patch_stands$include
      
      # reset pending data
      private$..pending_patch_stands <- NULL
      private$..pending_patch_stats <- NULL
      
      message(glue::glue('Patch {patch_id} recorded'))
      return(invisible(self))
    },
    
    # ..........................................................................
    #' @description 
    #' Summarize recorded patches
    #' @param group_vars character vector Field names to group by
    #' @param sum_vars character vector Field names to summarize
    #'
    summarize = function(group_vars = NULL, sum_vars = NULL){
      
      stands <- private$..geom %>% 
        st_drop_geometry() %>% 
        select(-..include) %>%
        inner_join(self$patch_stands) %>%
        rename(DoTreat = include)
      
      sum_vars <- c(private$..param_objective_field,
                private$..param_area_field,
                private$..param_constraint_field,
                sum_vars)
      
      if(!is.null(private$..param_threshold)){
        group_vars <- c('patch_id','DoTreat', group_vars)
      } else {
        group_vars <- c('patch_id', group_vars)
      }
      
      sum_out <- stands %>% 
        group_by_at(vars(group_vars)) %>%
        summarize_at(vars(sum_vars), sum)
      
      return(sum_out)
    },
    
    # ..........................................................................
    #' @description Sample fraction of stands
    #' @param sample_frac numeric Fraction of stands to evaluate (0-1)

    random_sample = function(sample_frac = 1){
      
      sample_nodes <- sample_func(
        geom = private$..geom, 
        sample_frac = sample_frac, 
        id_field = private$..param_id_field, 
        spatial_grid = TRUE,
        rng_seed = private$..param_rng_seed)
      
      sample_val <- ifelse(V(private$..net)$name %in% sample_nodes, 1, 0)
      message(paste0(sum(sample_val), ' (', 
        round(sum(sample_val)/length(sample_val) * 100), 
        '%) stands randomly selected for search'))
      
      private$..net <- igraph::set_vertex_attr(
        graph = private$..net,
        name = '..sample',
        value = sample_val)
    },
    
    # ..........................................................................
    #' @description Set search seeds manually
    #' @param ids character vector Specified set of stands IDs to search
    
    set_sample = function(ids){
      
      sample_val <- ifelse(V(private$..net)$name %in% ids, 1, 0)
      
      message(paste0(sum(sample_val), ' (', 
                     round(sum(sample_val)/length(sample_val) * 100), 
                     '%) manually selected for search'))
      
      private$..net <- igraph::set_vertex_attr(
        graph = private$..net,
        name = '..sample',
        value = sample_val)
    },

    # ..........................................................................
    #' @description Reset recorded patches
    #' @param patch_id optional. Patch ID to delete
    #' @details If blank, delete all patches. If negative, delete that number of
    #'   the most recent patches. Else, delete patch ID equal argument.
    #'
    reset = function(patch_id = NULL){
      if(is.null(patch_id)){
        private$..geom$..patch_id = 0
        V(private$..net)$..patch_id = 0
        private$..record_patch_stands = NULL
        private$..record_patch_stats = NULL
        message('All patches reset')
      } else {
        if(patch_id < 0){
          ids = sort(unique(private$..geom$..patch_id), decreasing = T)
          ids_s = ids[1:abs(patch_id)]
          private$..geom$..patch_id[private$..geom$..patch_id %in% ids_s] = 0
          V(private$..net)$..patch_id[V(private$..net)$..patch_id %in% ids_s] = 0
          message(paste0('Patches ', ids_s, ' deleted'))
        } else {
          private$..geom$..patch_id[private$..geom$..patch_id %in% patch_id] = 0
          V(private$..net)$..patch_id[V(private$..net)$..patch_id %in% patch_id] = 0
          message(paste0('Patch ', patch_id, ' deleted'))
        }
      }

      private$..kill_switch = FALSE
      private$..pending_patch_stands = NULL
      private$..pending_patch_stats = NULL
      private$..pending_seed = NULL
      return(invisible(self))
    }
  ),
  
  # //////////////////////////////////////////////////////////////////////////
  # PRIVATE ELEMENTS ------------------------------
  # internal aspects of the patchmax generator
  # ////////////////////////////////////////////////////////////////////////// 
  
  private = list(
    ..net = NULL,
    ..geom = NULL,
    ..cpp_graph = NULL,
    ..param_id_field = NULL,
    ..param_objective_field = NULL,
    ..param_area_field = NULL,
    ..param_area_max = Inf,
    ..param_area_min = -Inf,
    ..param_threshold = NULL,
    ..param_threshold_area_adjust = 0,
    ..param_threshold_objective_adjust = 0,
    ..param_exclusion_limit = 0.5,
    ..param_constraint_field = NULL,
    ..param_constraint_max = Inf,
    ..param_constraint_min = -Inf,
    ..param_sdw = 0.5,
    ..param_epw = 0.5,
    ..param_rng_seed = NULL,
    ..pending_patch_stands = NULL,
    ..pending_patch_stats = NULL,
    ..pending_seed = NULL,
    ..record_patch_stands = NULL,
    ..record_patch_stats = NULL,
    ..kill_switch = FALSE,

    #' Update network adjacency network object
    ..get_net = function(){
      net <- private$..net
      net <- delete_vertices(net, V(net)$..patch_id > 0)
      return(net)
    },
    
    #' Update edgelist distances
    ..get_edgelist = function(){
      dst <- distance_func(
        net = private$..get_net(),
        objective_field = '..objective',
        sdw = private$..param_sdw, 
        epw = private$..param_epw)
      return(dst)
    },

    #' Update stand data table template
    ..get_nodelist = function(){
      
      # get network and edge list
      net <- private$..get_net()
      cpp_graph <- private$..get_edgelist()
      
      # create node table
      i = match(cpp_graph$dict$ref, V(net)$name)
      node_dt <- data.table::data.table(
        node = cpp_graph$dict$ref, 
        dist = NA, 
        area = vertex_attr(net, '..area', i), 
        include = vertex_attr(net, '..include', i),
        objective = vertex_attr(net, '..objective', i), 
        constraint = vertex_attr(net, '..constraint', i),
        constraint_met = TRUE,
        row.names = NULL) 
      
      return(node_dt)
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

    # update network attributes
    ..refresh_net_attr = function(){
      
      if(!is.null(private$..param_objective_field)){
        obj <- vertex_attr(private$..net, private$..param_objective_field)
        vertex_attr(private$..net, name = '..objective') <- obj
      }
      
      if(!is.null(private$..param_area_field)){
        vertex_attr(private$..net, name = '..area') <- 
          vertex_attr(private$..net, private$..param_area_field)
      }
      
      if(!is.null(private$..param_constraint_field)){
        vertex_attr(private$..net, name = '..constraint') <- 
          vertex_attr(private$..net, private$..param_constraint_field)
      }
      
      if(!is.null(private$..param_threshold)){
        net <- private$..net
        s_txt = private$..param_threshold
        id = private$..param_id_field
        all_ids = dplyr::pull(vertex_attr(net), 'name')   
        include_ids = subset(vertex_attr(net), eval(parse(text = s_txt))) %>% pull(name)
        V(private$..net)$..include = ifelse(all_ids %in% include_ids, 1, 0)
      } else
        V(private$..net)$..include = 1
    },
    
    # plot useful seeing search results
    ..search_plot = function(search_out){
      
      search_dat <- data.frame(
        names(search_out), 
        search_out = as.numeric(search_out)
      ) %>%
        rename(!!private$..param_id_field := 1)
      
      pdat <- dplyr::inner_join(
        x = private$..geom, 
        y = search_dat, 
        by=private$..param_id_field)
      
      # identify best seed
      seed <- pdat[pdat$search_out == max(pdat$search_out, na.rm=TRUE),]
      
      # build best patch
      best_out = names(search_out)[which.max(search_out)]
      self$build(best_out)
      patch_geom <- self$geom %>% 
        filter(get(private$..param_id_field) %in% self$pending_stands$node) %>%
        summarize()
      
      # plot search results
      ggplot() + 
        geom_sf(data=pdat, aes(fill=search_out), linewidth=0) +
        geom_sf(data=suppressWarnings(st_centroid(seed)), size=4, shape=5) +
        scale_fill_gradientn(colors = sf.colors(10)) +
        theme(legend.position = 'bottom') +
        theme_void() + 
        geom_sf(data=patch_geom, fill=NA, color='black', linewidth=2)
    }
  ),

  # //////////////////////////////////////////////////////////////////////////
  # ACTIVE BINDINGS ------------------------------
  # used for getting and setting private elements
  # //////////////////////////////////////////////////////////////////////////
  
  active = list(
    
    #' @field net Get igraph object. Read only
    net = function(){
      private$..net
    },
    
    #' @field geom Get sf geometry object. Read only
    geom = function(value){
      if(missing(value)){
        private$..geom
      } else {
        
        if(!any(class(value) == 'sf')){
          stop('Geometry must be an sf object')
        }
        
        # set key fields in geometry object
        value <- value %>% 
          mutate(!!private$..param_id_field := as.character(get(private$..param_id_field))) %>%
          mutate(..patch_id = 0) %>%
          mutate(..include = 1) %>%
          mutate(..objective = 0) %>%
          mutate(..area = 0) %>%
          mutate(..constraint = 0)
        
        if(pull(value, private$..param_id_field) %>% n_distinct() < nrow(value)){
          stop('Stand IDs must be unique')
        }
        
        private$..geom <- value
      }
    },
    
    #' @field kill_switch = Flag for stopping patchmax
    kill_switch = function(value){
      if(missing(value)){
        private$..kill_switch
      } else {
        assertive::is_logical(value)
        private$..kill_switch = value
      }
    },
    
    #' @field best Get pending stand id representing best patch seed. Read only
    best = function(){
      private$..pending_seed
    },
    
    #' @field pending_stands Get stands in pending patch. Read only
    pending_stands = function(){
      private$..pending_patch_stands
    },
    
    #' @field pending_patch Get pending patch stats. Read only
    pending_patch = function(){
      private$..pending_patch_stats
    },
    
    #' @field patch_stands Get list of recorded stands
    patch_stands = function(){
      private$..record_patch_stands
    },
    
    #' @field patch_stats Get list of recorded patches
    patch_stats = function(){
      private$..record_patch_stats
    },
    
    #' @field patch_count Get list of recorded patches
    patch_count = function(){
      ps <- private$..record_patch_stats
      cnt <- ifelse(is.null(ps), 0, nrow(ps))
      return(cnt)
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
      }
    },
    
    #' @field exclusion_limit Get/set threshold limit 
    exclusion_limit = function(value){
      if(missing(value)){
        private$..param_exclusion_limit
      } else {
        private$..param_exclusion_limit <- value
        private$..refresh_net_attr()
      }
    },
    
    #' @field threshold_area_adjust Get/set fraction of area to count within excluded stands
    threshold_area_adjust = function(value){
      if(missing(value)){
        private$..param_threshold_area_adjust
      } else {
        private$..param_threshold_area_adjust <- value
        private$..refresh_net_attr()
      }
    },
    
    #' @field threshold_objective_adjust Get/set fraction of objective to count within excluded stands
    threshold_objective_adjust = function(value){
      if(missing(value)){
        private$..param_threshold_objective_adjust
      } else {
        assertive::assert_is_numeric(value)
        assertive::assert_is_of_length(value, 1)
        assertive::assert_all_are_in_range(value, 0, 1, F, F)
        private$..param_threshold_objective_adjust <- value
        private$..refresh_net_attr()
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
    
    #' @field epw Get/set exclusion penalty weight between 0 and 1. Default 0.5. Values closer to 1 enact a greater cost on projects spanning areas excluded by the project stand threshold.
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
    
    #' @field sdw Get/set spatial distance weight between 0 and 1. Default 0.5. At 0, patches are highly constrained by distance, resulting in compact shapes. At 1, patches are less constrained by distance and seek out areas with higher objectives. 
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
    
    #' @field seed Get/set the seed used in random number generator
    seed = function(value){
      if(missing(value)){
        private$..param_rng_seed
      } else {
        if((assertive::is_null(value) | assertive::is_numeric(value)) == FALSE){
          stop('Seed must be numeric, integer, or NULL')
        }
        private$..param_rng_seed <- value
      }
    },
    
    #' @field params Get/set list of all patch parameters. Argument is a named list if setting multiple parameters at once through params active bindings. Ex: patchmax$params = list(constraint_field = 'constraint1', constraint_max = 1000).
    params = function(value){
      if(missing(value)){
        nm <- names(private)
        nm_p <- nm[grepl('..param', nm)]
        params <- nm_p %>% lapply(function(x) get(x, envir = private))
        names(params) <- gsub('..param_','', nm_p)
        str(params)
      } else {
        assertive::assert_is_list(value)
        for(i in seq_along(value)){
          tryCatch({
            nm = names(value)[i]
            assign(nm, value = value[i][[1]], envir = self)
          }, error = function(e){
            print(e)
          })
        }
      }
    }
  )
)
    