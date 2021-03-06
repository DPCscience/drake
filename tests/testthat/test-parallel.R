drake_context("parallel")

test_with_dir("parallelism not found for testrun()", {
  config <- list(parallelism = "not found")
  expect_error(testrun(config))
})

test_with_dir("parallelism_choices", {
  expect_true(
    length(parallelism_choices(distributed_only = TRUE)) <
    length(parallelism_choices(distributed_only = FALSE))
  )
})

test_with_dir("parallelism warnings", {
  config <- dbug()
  suppressWarnings(parallelism_warnings(config))
  expect_silent(
    warn_mclapply_windows(parallelism = "mclapply", jobs = 1)
  )
  expect_warning(
    warn_mclapply_windows(parallelism = "mclapply", jobs = 2, os = "windows")
  )
})

test_with_dir("shell_file() writes correctly", {
  expect_false(file.exists("shell.sh"))
  shell_file()
  expect_true(file.exists("shell.sh"))
  unlink("shell.sh", force = TRUE)

  d <- "exdir"
  dir.create(d)
  p <- file.path(d, "script.txt")
  expect_false(file.exists(p))
  shell_file(p)
  expect_true(file.exists(p))
  unlink(d, recursive = TRUE, force = TRUE)

  expect_silent(shell_file(overwrite = TRUE))
  expect_silent(shell_file(overwrite = FALSE))
  expect_warning(shell_file(overwrite = TRUE))
})

test_with_dir("mclapply and lapply", {
  config <- dbug()
  make(plan = config$plan, envir = config$envir, verbose = FALSE,
    jobs = 1, parallelism = "mclapply", session_info = FALSE)
  expect_true(is.numeric(readd(final)))
  clean()

  # should demote to 1 job on Windows
  suppressWarnings(
    make(plan = config$plan, envir = config$envir, verbose = FALSE,
      jobs = 2, parallelism = "mclapply", session_info = FALSE)
  )
  expect_true(is.numeric(readd(final)))
  clean()

  make(plan = config$plan, envir = config$envir, verbose = FALSE,
    jobs = 2, parallelism = "parLapply", session_info = FALSE)
  expect_true(is.numeric(readd(final)))
  clean()

  make(plan = config$plan, envir = config$envir, verbose = FALSE,
    jobs = 1, parallelism = "parLapply", session_info = FALSE)
  expect_true(is.numeric(readd(final)))
})

test_with_dir("lightly_parallelize_atomic() is correct", {
  withr::with_seed(seed = 2017, code = {
    x <- sample(LETTERS[1:3], size = 1e3, replace = TRUE)
    append <- function(x){
      paste0(x, "_text")
    }
    out0 <- lightly_parallelize(X = x, FUN = append, jobs = 2)
    out1 <- lightly_parallelize_atomic(X = x, FUN = append, jobs = 2)
    out2 <- lapply(X = x, FUN = append)
    expect_identical(out0, out1)
    expect_identical(out0, out2)
    y <- gsub("_text", "", unlist(out1))
    expect_identical(x, y)
  })
})
