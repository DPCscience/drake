drake_context("workflow plan")

test_with_dir("empty plan", {
  expect_equal(
    drake_plan(),
    data.frame(
      target = character(0),
      command = character(0)
    )
  )
})

test_with_dir("plan set 1", {
  for (tidy_evaluation in c(TRUE, FALSE)){
    x <- drake_plan(
      a = c,
      b = "c",
      list = c(c = "d", d = "readRDS('e')"),
      tidy_evaluation = tidy_evaluation
    )
    y <- data.frame(
      target = letters[1:4],
      command = c("c", "'c'",
      "d", "readRDS('e')"),
      stringsAsFactors = F)
    expect_equal(x, y)
  }
})

test_with_dir("plan set 2", {
  for (tidy_evaluation in c(TRUE, FALSE)){
    x <- drake_plan(a = c,
                    b = "c",
                    list = c(c = "d", d = "readRDS('e')"),
                    strings_in_dots = "literals",
                    tidy_evaluation = tidy_evaluation)
    y <- data.frame(
      target = letters[1:4],
      command = c("c", "\"c\"",
                  "d", "readRDS('e')"), stringsAsFactors = F)
    expect_equal(x, y)
  }
})

test_with_dir("plan set 3", {
  for (tidy_evaluation in c(TRUE, FALSE)){
  x <- drake_plan(
    a = c,
    b = "c",
    list = c(c = "d", d = "readRDS('e')"),
    strings_in_dots = "literals", file_targets = TRUE,
    tidy_evaluation = tidy_evaluation)
  y <- data.frame(
    target = drake::drake_quotes(letters[1:4], single = TRUE),
    command = c("c", "\"c\"", "d", "readRDS('e')"),
    stringsAsFactors = F)
  expect_equal(x, y)
  }
})

test_with_dir("plan set 4", {
  for (tidy_evaluation in c(TRUE, FALSE)){
    x <- drake_plan(
      a = c,
      b = "c",
      list = c(c = "d", d = "readRDS('e')"),
      strings_in_dots = "filenames", file_targets = TRUE,
      tidy_evaluation = tidy_evaluation)
    y <- data.frame(
      target = drake::drake_quotes(letters[1:4], single = TRUE),
      command = c("c", "'c'", "d", "readRDS('e')"), stringsAsFactors = F)
    expect_equal(x, y)
    expect_warning(check_plan(x, verbose = FALSE))
  }
})

test_with_dir("drake_plan() trims outer whitespace in target names", {
  for (tidy_evaluation in c(TRUE, FALSE)){
    x <- drake_plan(list = c(` a` = 1, `b \t\n` = 2),
                    tidy_evaluation = tidy_evaluation)
    y <- drake_plan(a = 1, b = 2, tidy_evaluation = tidy_evaluation)
    expect_equal(x$target, y$target)
  }
})

test_with_dir("make() and check_plan() trim outer whitespace in target names", {
  x <- data.frame(target = c("a\n", "  b", "c ", "\t  d   "),
                  command = 1)
  expect_silent(make(x, verbose = FALSE, session_info = FALSE))
  expect_equal(sort(cached()), letters[1:4])
  stat <- c(a = "finished", b = "finished", c = "finished",
            d = "finished")
  expect_equal(progress(), stat)

  expect_warning(
    make(
      x,
      verbose = FALSE,
      targets = c("a", "nobody_home"),
      session_info = FALSE
    )
  )

  x <- data.frame(target = c("a", " a"), command = 1)
  expect_error(check_plan(x, verbose = FALSE))
})

test_with_dir("make() plays nicely with tibbles", {
  skip_if_not_installed("pillar")
  skip_if_not_installed("tibble")
  x <- tibble::tribble(~target, ~command, "nothing", 1)
  expect_silent(check_plan(x, verbose = FALSE))
  expect_silent(make(x, verbose = FALSE, session_info = FALSE))
})

test_with_dir("check_plan() finds bad symbols", {
  x <- data.frame(
    target = c("gotcha", "b", "\"targs\"", "a'x'", "b'x'"),
    command = 1)
  expect_warning(o <- check_plan(x, verbose = FALSE))
  x <- data.frame(
    target = c("\"targs\""),
    command = 1)
  expect_warning(o <- check_plan(x, verbose = FALSE))
  x <- data.frame(
    target = c("gotcha", "b", "targs"),
    command = 1)
  expect_silent(o <- check_plan(x, verbose = FALSE))
})

test_with_dir("illegal target names get fixed", {
  pl <- data.frame(
    target = c("_a", "a^", "a*", "a-"),
    command = 1,
    stringsAsFactors = FALSE
  )
  cache <- storr::storr_environment()
  expect_warning(
    con <- make(pl, cache = cache, session_info = FALSE)
  )
  expect_equal(
    sort(con$plan$target),
    sort(con$targets),
    sort(cached(cache = cache)),
    sort(c("a", "a_", "a__1", "a__2"))
  )
})

test_with_dir("issue 187 on Github (from Kendon Bell)", {
  test <- drake_plan(test = run_it(wc__))
  out <- expect_warning(
    evaluate_plan(test, rules = list(wc__ = list(1:4, 5:8, 9:12)))
  )
  out2 <- data.frame(
    target = c("test_1_4", "test_5_8", "test_9_12"),
    command = c("run_it(1:4)", "run_it(5:8)", "run_it(9:12)"),
    stringsAsFactors = FALSE
  )
  expect_equal(out, out2)
})

test_with_dir("file names with weird characters do not get mangled", {
  out <- data.frame(
    target = c("'is:a:file'", "not:a:file"),
    command = as.character(1:2),
    stringsAsFactors = FALSE
  )
  out2 <- expect_warning(sanitize_plan(out))
  out3 <- data.frame(
    target = c("'is:a:file'", "not_a_file"),
    command = as.character(1:2),
    stringsAsFactors = FALSE
  )
  expect_equal(out2, out3)
})

test_with_dir("can use semicolons for multi-line commands", {
  plan <- drake_plan(list = c(x = "a<-1; a", y = "b<-2\nb"))
  make(plan, verbose = FALSE, session_info = FALSE)
  expect_false(any(c("a", "b") %in% ls()))
  expect_true(all(cached(x, y, search = FALSE)))
  expect_equal(cached(search = FALSE), c("x", "y"))
})

test_with_dir("can use braces for multi-line commands", {
  small_plan <- drake_plan(
    small_target = {
      local_object <- 1 + 1
      2 + local_object
    }
  )
  make(small_plan, session_info = FALSE)
  expect_true("small_target" %in% cached())
  expect_false("local_object_target" %in% cached())
  expect_equal(readd(small_target), 4)
  expect_false("local_object" %in% ls())
})
