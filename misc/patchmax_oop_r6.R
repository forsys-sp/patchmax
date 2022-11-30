# scenario object generator (read this into memory)
patchmax_generator <- R6Class(
  classname = "patch_generator",
  
  #############################################################################
  # PRIVATE ELEMENTS
  # internal aspects to the patchmax generator
  #############################################################################
  
  private = list(
    ..net = NULL,
    ..geom = NULL,
    ..cpp_graph = NULL,
    ..param_id_field = NULL,
    ..param_obj_field = NULL,
    ..param_area_field = NULL,
    ..param_cnst_field = NULL,
    ..param_proj_area = NULL,
    ..param_sdw = 1,
    ..param_epw = 1,
    ..proj_mem = NULL,
    ..best_mem = NULL,
    ..output = list(
      patches = NULL,
      stands = NULL
    ),
    ..check_req_fields = function(){
      if(is.null(private$..param_id_field) | 
         is.null(private$..param_obj_field) | 
         is.null(private$..param_proj_area) |
         is.null(private$..param_area_field)){
        stop('param must include id_field, obj_field, area_field, & proj_area')
      }
    },
    ..refresh_net_attr = function(){
      # attribute adjacency network
      if(!is.null(private$..param_obj_field)){
        vertex_attr(private$..net, name = 'objective') <- 
          vertex_attr(private$..net, private$..param_obj_field)
      }
      
      if(!is.null(private$..param_cnst_field)){
        vertex_attr(private$..net, name = 'constraint') <- 
          vertex_attr(private$..net, private$..param_cnst_field)
      }
      
      if(!is.null(private$..param_area_field)){
        vertex_attr(private$..net, name = 'area') <- 
          vertex_attr(private$..net, private$..param_area_field)
      }
    }
  ),
  
  #############################################################################
  # PUBLIC ELEMENTS
  # externally available methods for interacting with patchmax
  #############################################################################
  
  public = list(
    
    # INITIALIZE METHOD --------------------------------------------------------
    
    initialize = function(geom, id_field=NULL, obj_field=NULL, area_field=NULL, cnst_field=NULL, proj_area=NULL){
      if(!missing(geom)){
        
        # build adjacency network
        private$..geom <- geom
        private$..geom$patch_id <- 0
        
        # parameters
        private$..param_id_field = id_field
        private$..param_obj_field = obj_field
        private$..param_area_field = area_field
        private$..param_cnst_field = cnst_field
        private$..param_proj_area = proj_area 
        
        # build adjacency network
        private$..net <- calc_adj_network_func(
          shp = geom, 
          St_id = dplyr::pull(geom, id_field), 
          calc_dist = TRUE)
        
        vertex_attr(private$..net) <- 
          bind_cols(vertex_attr(private$..net), st_drop_geometry(geom))
        vertex_attr(private$..net, name = 'exclude') <- 0
        vertex_attr(private$..net, name = 'patch_id') <- 0
        
        private$..refresh_net_attr()
        
      } else {
        stop('Geometry required')
      }
    },
    
    # UDPATE METHOD --------------------------------------------------------
    
    update = function(){
      
      message(glue::glue('\nUpdating costs using sdw = {private$..param_sdw} and epw = {private$..param_epw}'))
      
      private$..cpp_graph <- build_graph_func(
        net = private$..net, 
        obj_field = private$..param_obj_field, 
        sdw = private$..param_sdw, 
        epw = private$..param_epw)
      
      return(invisible(self))
    },
    
    # SELECT METHOD --------------------------------------------------------
    
    select = function(node=NULL){
      
      if(is.null(node)){
        node <- private$..best_mem
      }
      
      self$update()
      
      out <- grow_project_func(
        cpp_graph = private$..cpp_graph, 
        net = private$..net, 
        start_node = node , 
        proj_area = private$..param_proj_area)
      
      obj_dat <- st_drop_geometry(private$..geom) %>% 
        dplyr::select(
          node = private$..param_id_field, 
          private$..param_obj_field)
      
      out <- dplyr::left_join(out, obj_dat, by='node')
      private$..proj_mem <- out
      
      message(glue::glue('Project built from: {private$..best_mem}, area: {round(sum(private$..proj_mem$area),3)}, score: {round(sum(private$..proj_mem$objective),3)}'))
      
      return(invisible(self))
    },
    
    # SEARCH METHOD --------------------------------------------------------
    
    search = function(sample_frac=0.1, return_all=FALSE){
      
      self$update()
      
      out <- search_best_func(
        cpp_graph = private$..cpp_graph, 
        net = private$..net, 
        obj_field = private$..param_obj_field, 
        proj_area = private$..param_proj_area,
        sample_frac = sample_frac,
        return_all = return_all)
      
      message(glue::glue('\n\n Best start node: {names(out)}'))
      private$..best_mem <- names(out)[which.max(out)]
      
      return(invisible(self))
    },
    
    # PLOT METHOD --------------------------------------------------------
    
    plot = function(plot_field = NULL){
      
      if(is.null(private$..proj_mem)){
        message('No project currently selected')
        return()
      }

      border <- private$..geom$stand_id %in% private$..proj_mem$node
      
      if(is.null(plot_field)){
        plot(private$..geom[,private$..param_obj_field], border=border)
      } else {
        plot(private$..geom[,plot_field], border=border)
      }
      
      return(invisible(self))
    },
    
    # APPLY METHOD --------------------------------------------------------
    # (work in progress)
    
    apply_project = function(patch_id = NULL){
      if(is.null(private$..proj_mem))
        stop('Project must be built first')

      V(private$..net)$patch_id[match(private$..proj_mem$node, V(private$..net)$stand_id)] = patch_id
      private$..geom$patch_id[match(private$..proj_mem$node, private$..geom$stand_id)] = patch_id
      private$..net <- subtract_project(private$..net, private$..proj_mem)
      private$..proj_mem
      
      return(invisible(self))
    },
    
    # DESCRIBE METHOD --------------------------------------------------------
    # (work in progress)
    
    describe_projects = function(){
      
    }
  ),
  
  #############################################################################
  # ACTIVE BINDINGS 
  # used for getting and setting internal elements
  #############################################################################
  
  active = list(
    net = function(){
      private$..net
    },
    geom = function(){
      private$..geom
    },
    best = function(){
      private$..best_mem
    },
    selected = function(){
      private$..proj_mem
    },
    id_field = function(value){
      if(missing(value)){
        private$..param_id_field
      } else {
        private$..param_id_field <- value
        private$..refresh_net_attr()
      }
    },
    obj_field = function(value){
      if(missing(value)){
        private$..param_obj_field
      } else {
        private$..param_obj_field <- value
        private$..refresh_net_attr()
      }
    },
    area_field = function(value){
      if(missing(value)){
        private$..param_area_field
      } else {
        private$..param_area_field <- value
        private$..refresh_net_attr()
      }
    },
    cnst_field = function(value){
      if(missing(value)){
        private$..param_cnst_field
      } else {
        private$..param_cnst_field <- value
        private$..refresh_net_attr()
      }
    },
    proj_area = function(value){
      if(missing(value)){
        private$..param_proj_area
      } else {
        private$..param_proj_area <- value
        private$..refresh_net_attr()
      }
    },
    epw = function(value){
      if(missing(value)){
        private$..param_epw
      } else {
        private$..param_epw <- value
      }
    },
    sdw = function(value){
      if(missing(value)){
        private$..param_sdw
      } else {
        private$..param_sdw <- value
      }
    }
  )
)
    