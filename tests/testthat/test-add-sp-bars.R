test_that("add_sp_bars adds errorbar and crossbar layers", {
  df <- data.frame(group = rep(c("A","B"), each = 5),
                   value = c(rep(1,5), rep(2,5)),
                   repl = rep(1:5, 2))
  p0 <- ggplot2::ggplot(df, ggplot2::aes(x = group, y = value))
  p <- add_sp_bars(p0, bars = "mean_sem", df = df, cond = "group", rep_summary = "value")
  expect_s3_class(p, "ggplot")
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(any(grepl("GeomErrorbar|GeomCrossbar", layer_classes)))
})
