test_that("superplot returns a ggplot and contains expected data rows", {
  set.seed(42)
  df <- data.frame(
    condition = rep(c("C1", "C2"), each = 4),
    replicate = rep(1:4, times = 2),
    value = rnorm(8)
  )
  p <- superplot(df, meas = "value", cond = "condition", repl = "replicate")
  expect_s3_class(p, "ggplot")

  gb <- ggplot2::ggplot_build(p)
  # ensure at least one layer contains data rows
  total_rows <- sum(vapply(gb$data, nrow, integer(1)))
  expect_true(total_rows >= 8)
})
