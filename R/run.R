run_command <- function(target, command, seed, config){
  retries <- 0
  max_retries <- drake_plan_override(
    target = target,
    field = "retries",
    config = config
  ) %>%
    as.numeric
  while (retries <= max_retries){
    value <- one_try(
      target = target,
      command = command,
      seed = seed,
      config = config
    )
    if (!inherits(value, "error")){
      return(value)
    }
    write(
      x = paste0("Error building target ", target, ": ", value$message),
      file = stderr()
    )
    retries <- retries + 1
    console_retry(target = target, retries = retries, config = config)
  }
  value
}

one_try <- function(target, command, seed, config){
  withr::with_seed(seed, {
    with_timeout(
      target = target,
      command = command,
      config = config
    )
  })
}

with_timeout <- function(target, command, config){
  env <- environment()
  timeouts <- resolve_timeouts(target = target, config = config)
  R.utils::withTimeout({
      value <- eval(parse(text = command), envir = env)
    },
    timeout = timeouts["timeout"],
    cpu = timeouts["cpu"],
    elapsed = timeouts["elapsed"],
    onTimeout = "error"
  )
}

resolve_timeouts <- function(target, config){
  keys <- c("timeout", "cpu", "elapsed")
  timeouts <- lapply(
    X = keys,
    FUN = function(field){
      drake_plan_override(
        target = target,
        field = field,
        config = config
      ) %>%
        as.numeric
    }
  )
  names(timeouts) <- keys
  for (field in c("cpu", "elapsed")){
    if (!length(timeouts[[field]])){
      timeouts[[field]] <- timeouts$timeout
    }
  }
  timeouts
}
