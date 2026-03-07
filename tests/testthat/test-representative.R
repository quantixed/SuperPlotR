test_that("representative returns expected structure and ranks", {
  df <- data.frame(group = rep("G", 5),
                   repl = rep(1, 5),
                   value = c(1,2,3,4,5))
  res <- representative(df, meas = "value", cond = "group", repl = "repl")
  expect_true(is.data.frame(res))
  expect_true("rank" %in% names(res))
  expect_true(all(res$rank >= 1))

  # test with a label column
  df2 <- df
  df2$label <- letters[1:5]
  res2 <- representative(df2, meas = "value", cond = "group", repl = "repl", label = "label")
  expect_true(is.data.frame(res2))
  expect_true("label" %in% names(res2))
})
