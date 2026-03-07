test_that("vdiffr visual test for a small superplot (skipped if vdiffr missing)", {
  testthat::skip_if_not_installed("vdiffr")
  data(lord_jcb, package = "SuperPlotR")
  p <- superplot(lord_jcb, meas = "Speed", cond = "Treatment", repl = "Replicate")
  vdiffr::expect_doppelganger("superplot-lord_jcb", p)
})
