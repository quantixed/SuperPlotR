test_that("superplot_spec returns an S3 spec object", {
  set.seed(10)
  df <- data.frame(
    condition = rep(c("A", "B"), each = 6),
    replicate = rep(1:3, times = 4),
    value = rnorm(12)
  )

  spec <- superplot_spec(df, meas = "value", cond = "condition", repl = "replicate")

  expect_s3_class(spec, "superplot_spec")
  expect_true(is.data.frame(spec$data))
  expect_true(is.data.frame(spec$summary))
})

test_that("sp_modify updates advanced options and autoplot renders", {
  set.seed(20)
  df <- data.frame(
    condition = rep(c("A", "B"), each = 6),
    replicate = rep(1:3, times = 4),
    value = rnorm(12, mean = 2)
  )

  spec <- superplot_spec(df, meas = "value", cond = "condition", repl = "replicate")
  spec <- sp_modify(
    spec,
    summary_params = list(size = 5),
    legend_position = "right",
    y_limits_from_zero = FALSE
  )

  p <- autoplot(spec)
  expect_s3_class(p, "ggplot")

  gb <- ggplot2::ggplot_build(p)
  total_rows <- sum(vapply(gb$data, nrow, integer(1)))
  expect_true(total_rows >= 12)
})

test_that("summary points use replicate fill and black outline by default", {
  df <- data.frame(
    condition = rep(c("A", "B"), each = 6),
    replicate = rep(1:3, times = 4),
    value = c(1, 2, 3, 4, 5, 6, 2, 3, 4, 5, 6, 7)
  )

  spec <- superplot_spec(df, meas = "value", cond = "condition", repl = "replicate")
  p <- autoplot(spec)
  gb <- ggplot2::ggplot_build(p)

  summary_n <- nrow(spec$summary)
  summary_layers <- gb$data[vapply(gb$data, nrow, integer(1)) == summary_n]

  fill_counts <- vapply(summary_layers, function(layer_df) {
    if (!"fill" %in% names(layer_df)) {
      return(0L)
    }
    length(unique(layer_df$fill))
  }, integer(1))

  black_outline_present <- vapply(summary_layers, function(layer_df) {
    if (!"colour" %in% names(layer_df)) {
      return(FALSE)
    }
    all(layer_df$colour == "black")
  }, logical(1))

  expect_true(any(fill_counts > 1L))
  expect_true(any(black_outline_present))
})
