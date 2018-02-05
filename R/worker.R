R6_worker <- R6::R6Class(
  "R6_worker",
  public = list(
    id = NULL,
    target = NULL,
    meta = NULL,
    config = NULL,
    evaluator = NULL,
    value = NULL,
    future = NULL,
    start = NULL
    initialize = function(
      id, target, config = list(), lazy = FALSE, evaluator = future::plan("next")
    ){
      self$id <- id
      self$target <- target
      self$config <- config
      self$lazy <- lazy
      if (!is.NULL(evaluator)){
        self$evaluator <- evaluator
      }
    },
    assign = function(target){
      self$target <- target
    },
    deploy = function(){
      prune_envir(targets = target, config = config)
      self$meta <- drake_meta(target = target, config = config)
      should_build <- should_build_target(
        target = self$target,
        meta = self$meta,
        config = self$config
      )
      self$start <- proc.time()
      set_progress(target = target, value = "in progress", config = config)
      if (should_build){
        self$future <- future::future(
          drake_build(target = self$target, config = self$config),
          packages = config$packages,
          lazy = self$lazy,
          evaluator = self$evaluator
        )
      }
    },
    set_status = function(status){
      self$config$cache$get(
        key = self$id,
        value = status,
        namespace = "workers"
      )
   },
   get_status = function(){
     self$config$cache$get(
       key = self$id,
       namespace = "workers"
     )
   }
   status = function(){
      resolved <- future::resolved(self$future)
      idle <- self$get_status()
      if (resolved && idle){
        "done"
      } else if (!resolved && !idle){
        "running"
      } else {
        "error"
      }
    },
    finalize = function(){
      value <- future::value(self$future)
      assign(x = self$target, value = value, envir = self$config$envir)
      store_target(
        target = target, value = value, meta = meta,
        start = start, config = config)
      )
      self$assign(target = NULL)
    }
  )
)
