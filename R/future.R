make_future <- function(config){
  config$queue <- R6_queue$new(nodes = toposort_targets(config))
  config$workers <- new_future_workers(config = config)
  while (length(config$queue)){
    config <- monitor_future_workers(config = config)
    sleep(1e-9)
  }
}

new_future_workers <- function(config){
  num_workers <- min(length(config$queue), config$max_hpc_workers)
  lightly_parallelize(
    X = seq_len(num_workers),
    FUN = function(id) worker$new(id = id, config = config),
    jobs = config$max_local_workers
  )
}

monitor_future_workers <- function(config){
  status <- lightly_parallelize(
    X = config$workers,
    FUN = function(worker){
      worker$status()
    },
    jobs = config$max_local_workers
  )
  if (any(status == "error")){
    report_failed_targets(status, config)
  }
  is_idle <- status == "idle"
  n_deploy <- min(sum(is_idle), config$queue$n())
  next_targets <- config$queue$pop(n = n_deploy)
  idle_workers <- config$workers[is_idle]
  config$workers[is_idle] <- lightly_parallelize(
    X = seq_along(next_targets),
    FUN = function(index){
      idle_workers[[i]]$assign(target = next_targets[[i]])
      idle_workers[[i]]$deploy()
    },
    jobs = config$max_local_workers
  )
  lightly_parallelize(
    X = config$workers,
    FUN = function(worker){
      worker$finalize()
    },
    jobs = config$max_local_workers
  )
  config
}
