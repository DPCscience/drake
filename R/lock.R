set_lock <- function(target, config){
  config$cache$set(key = target, value = 0, namespace = "locks")
}

unlock <- function(target = target, config = config){
  config$cache$del(key = target, namespace = "locks")
}

is_locked <- function(target, config){
  config$cache$exists(key = target, namespace = "locks")
}

lock_successful <- function(target, config){
  wait_for_dependencies(
    target = target,
    config = config
  )
  if (is_locked(target = target, config = config)){
    FALSE
  } else {
    set_lock(target = target, config = config)
    TRUE # We're locked! That means we should try to build the target.
  }
}

wait_for_dependencies <- function(target, config){
  deps <- dependencies(target = target, config = config)
  while (length(deps)){
    sleep(1e-9)
    deps <- Filter(x = deps, f = function(key){
      get_progress_single(target = key, cache = config$cache) != "finished"
    })
  }
  if (is_locked(target = target, config = config)){
    stop( # nocov
      "Target ", target, " started to build before its dependencies were ready." # nocov
    ) # nocov
  }
}
