test_that("get_sp_colours returns correct length and repeats custom vector", {
  cols <- get_sp_colours(3, "tol_bright")
  expect_type(cols, "character")
  expect_length(cols, 3)

  # custom vector of colours is repeated to length n
  custom <- c("#FF0000", "green")
  cols2 <- get_sp_colours(5, custom)
  expect_length(cols2, 5)
  expect_equal(cols2[1:2], custom)
})

test_that("get_sp_colours errors on invalid scheme", {
  expect_error(get_sp_colours(2, "not_a_palette"))
})
