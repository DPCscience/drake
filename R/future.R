run_future <- function(config){
  prepare_distributed(config = config)
  run_parallel(config = config, worker = worker_future)
  finish_distributed(config = config)
}

worker_future <- function(targets, meta_list, config){
  targets <- intersect(targets, config$plan$target)
  # Probably will not encounter this, but it is better to have:
  if (!length(targets)){ # nocov # nolint
    return()             # nocov
  }                      # nocov
  futures <- lightly_parallelize(
    X = targets,
    FUN = deploy_future,
    jobs = config$jobs,
    meta_list = meta_list,
    config = config
  )
  values <- future::values(futures)
  rm(list = "values")
  invisible()
}

deploy_future <- function(target, meta_list, config){
  future::future(
    expr = {
      drake:::build_distributed(
        target = target,
        meta_list = meta_list,
        cache_path = config$cache$driver$path
      )
    },
    evaluator = get_evaluator(target = target, config = config)
  )
}

get_evaluator <- function(target, config){
  if (is.null(config$plan[["evaluator"]])){
    return(future::plan("next"))
  }
  evaluator <- config$plan[["evaluator"]][config$plan$target == target]
  if (evaluator %in% names(config$evaluators)){
    config$evaluators[[evaluator]]
  } else {
    future::plan("next")
  }
}
