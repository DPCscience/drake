run_mclapply <- function(config){
  jobs <- safe_jobs(config$jobs)
  tmp <- mclapply(
    X = seq_len(jobs),
    FUN = drake_build_worker,
    meta_list = meta_list,
    config = config,
    mc.cores = jobs
  )
}

warn_mclapply_windows <- function(
  parallelism,
  jobs,
  os = this_os()
){
  parallelism <- match.arg(
    parallelism,
    choices = parallelism_choices(distributed_only = FALSE)
  )
  if (parallelism == "mclapply" & jobs > 1 & os == "windows"){
    warning(
      "Demoting to one job at a time (no parallel computing). ",
      "The mclapply parallel backend does not support ",
      "multiple jobs on Windows. Windows users should use the ",
      "parLapply backend intead (Windows default), or an other ",
        "Windows-compatible backend.",
      call. = FALSE
    )
  }
}
