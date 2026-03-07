test_that("get_sp_stats prints sensible messages for edge cases and tests", {
  df1 <- data.frame(condition = rep("A", 3), rep_mean = c(1,2,3))
  expect_output(get_sp_stats(df1, "rep_mean", "condition", NULL, 1, 1, "para_unpaired"),
                "Only one condition")

  df2 <- data.frame(condition = rep(c("X","Y"), each = 2), rep_mean = c(1,2,3,4))
  # nrepl < 3 should report less than three replicates
  expect_output(get_sp_stats(df2, "rep_mean", "condition", NULL, 2, 2, "para_unpaired"),
                "Less than three replicates")

  # a valid two-group t-test path emits 'Performing t-test'
  df3 <- data.frame(condition = rep(c("X","Y"), each = 3),
                    rep_mean = c(1,2,3, 4,5,6))
  expect_output(get_sp_stats(df3, "rep_mean", "condition", NULL, 2, 3, "para_unpaired"),
                "Performing t-test")
})
