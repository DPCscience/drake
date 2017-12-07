drake_context("future")

test_with_dir("future backend", {
  pl <- plan_drake(a = 1, b = 2)
  pl$evaluator = c("e1", "e2")
  evaluators = list(e1 = future::sequential, e2 = future::multisession)
  config <- make(pl, parallelism = "future", session_info = FALSE)
  expect_equal(readd(a), 1)
  expect_equal(readd(b), 2)
  config$plan$evaluator[1] = "not_found"
  expect_silent(tmp <- get_evaluator("a", config))
  config$plan$evaluator <- NULL
  expect_silent(tmp <- get_evaluator("a", config))
})

test_with_dir("future_lapply functionality", {
  future::plan(future::sequential)
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  load_basic_example(envir = e)
  expect_silent(
    withr::with_options(
      new = list(mc.cores = 2), code = {
        config <- make(
          e$my_plan,
          envir = e,
          parallelism = "future_lapply",
          jobs = 1,
          verbose = FALSE,
          session_info = FALSE
        )
      }
    )
  )
  expect_equal(
    outdated(config),
    character(0)
  )
})
