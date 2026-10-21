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

test_that("get_sp_colours order argument works", {
  base <- get_sp_colours(3, "cud")
  expect_equal(get_sp_colours(3, "cud", "reverse"), rev(base))
  expect_equal(get_sp_colours(3, "cud", c(2, 3, 1)), base[c(2, 3, 1)])
  expect_message(r <- get_sp_colours(3, "cud", "random"), "Random colour order")
  expect_setequal(r, base)
  expect_error(get_sp_colours(3, "cud", c(1, 2)), "length")
  expect_error(get_sp_colours(3, "cud", c(1, 2, 4)), "once")
  expect_error(get_sp_colours(3, "cud", "foo"))
})

test_that("tol_rainbow returns the requested number of valid colours", {
  for (n in 1:23) {
    colours <- get_sp_colours(n, "tol_rainbow")
    expect_length(colours, n)
    expect_false(anyNA(colours))
  }
})

test_that("23-colour tol_rainbow is complete", {
  colours <- get_sp_colours(23, "tol_rainbow")
  expect_length(colours, 23)
  expect_false(anyNA(colours))
  expect_true(all(nzchar(colours)))
})
