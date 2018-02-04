assert_dir <- function(path){
  if (!file.exists(path)){
    dir.create(path)
  }
}

lock_dir <- "locks"

unlock_all <- function(config){
  main_cache <- config$cache_path
  lock_cache <- file.path(main_cache, lock_dir)
  assert_dir(main_cache)
  assert_dir(lock_cache)
  dir_empty(lock_cache)
}

lock_target <- function(target, config){
  main_cache <- config$cache_path
  lock_cache <- file.path(main_cache, lock_dir)
  lock_entry <- base64url::base64_urlencode(target)
  lock_file <- file.path(main_cache, lock_cache, lock_entry)
  assert_dir(cache)
  assert_dir(locks)

  wait_for_dependencies(
    target = target,
    config = config,
    lock_file = lock_file
  )

  # Check if the target is locked. As soon as possible afterwards,
  # lock the target if applicable.
  if (file.exists(lock_file)){
    FALSE
  } else {
    file.create(lock_file)
    TRUE # We're locked! That means we should try to build the target.
  }
}

wait_for_dependencies <- function(target, config, lock_file){
  deps <- dependencies(target = target, config = config)
  while (length(deps)){
    sleep(1e-9)
    deps <- Filter(x = deps, f = function(key){
      get_process_single(target = key, cache = config$cache) == "finished"
    })
  }
  if (file.exists(lock_file)){
    stop( # nocov
      "Target ", target, " started to build before its dependencies were ready." # nocov
    ) # nocov
  }
}
