test_that("resolve_pal_order accepts supported values", {
  expect_null(resolve_pal_order(NULL, 3))
  expect_equal(resolve_pal_order("reverse", 3), c(3L, 2L, 1L))
  expect_equal(resolve_pal_order(c(2, 3, 1), 3), c(2L, 3L, 1L))
})

test_that("resolve_pal_order rejects invalid values", {
  expect_error(
    resolve_pal_order("backwards", 3),
    "must be.*random.*reverse"
  )
  expect_error(
    resolve_pal_order(c(1, 2), 3),
    "must have length 3"
  )
  expect_error(
    resolve_pal_order(c(1, 1, 2), 3),
    "contain each integer from 1 to 3 once"
  )
  expect_error(
    resolve_pal_order(c(1, 2, NA), 3),
    "vector of integers"
  )
})

test_that("random palette order returns a reusable permutation", {
  set.seed(123)
  expect_message(
    order <- resolve_pal_order("random", 3),
    "Random colour order"
  )
  expect_equal(sort(order), 1:3)
})

test_that("superplot and superplot_spec expose the same public arguments", {
  expect_identical(
    names(formals(superplot)),
    names(formals(superplot_spec))
  )
})

test_that("superplot_spec validates pal_order during construction", {
  expect_error(
    superplot_spec(
      lord_jcb,
      meas = "Speed",
      cond = "Treatment",
      repl = "Replicate",
      pal_order = c(1, 1, 2)
    ),
    "contain each integer from 1 to 3 once"
  )
})

test_that("superplot forwards pal_order to superplot_spec", {
  # SuperPlot construction/rendering includes random horizontal placement.
  # Reset the RNG before each complete construction-and-build sequence.
  set.seed(123)
  direct_data <- ggplot2::ggplot_build(
    ggplot2::autoplot(superplot_spec(
      lord_jcb,
      meas = "Speed",
      cond = "Treatment",
      repl = "Replicate",
      pal_order = "reverse"
    ))
  )$data

  set.seed(123)
  wrapped_data <- ggplot2::ggplot_build(superplot(
    lord_jcb,
    meas = "Speed",
    cond = "Treatment",
    repl = "Replicate",
    pal_order = "reverse"
  ))$data

  expect_equal(wrapped_data, direct_data)
})
