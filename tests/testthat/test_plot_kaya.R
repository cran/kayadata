context("Plotting figures of Kaya variables")

if (requireNamespace("vdiffr", quietly = TRUE)) {
  expect_doppelganger <- vdiffr::expect_doppelganger
} else {
  expect_doppelganger <- function(title, fig, path = NULL, ...,
                                  verbose = NULL, writer = NULL) {
  }
}

test_that("kaya plots have correct output", {
  kd <- get_kaya_data("World")
  pk <-  plot_kaya(kd, "F", start_year = 1970, stop_year = 2000,
                   log_scale = FALSE, trend_line = TRUE,
                   points = TRUE)
  expect_doppelganger("kaya-plot", pk)
  pk2 <-  plot_kaya(kd, "e", start_year = 1970, stop_year = 2000,
                    log_scale = TRUE, trend_line = TRUE,
                    points = TRUE,
                    pre_color = "limegreen", post_color = "magenta",
                    in_range_color = "cadetblue", trend_color = "orange"
  )
  expect_doppelganger("kaya-plot-with-colors", pk2)
})

test_that("fuel mix plots have correct output", {
  fm <- get_fuel_mix("World", FALSE)
  pfm <-  plot_fuel_mix(fm, TRUE)
  expect_doppelganger("fuel-mix-collapsed", pfm)
  pfm2 <-  plot_fuel_mix(fm, FALSE)
  expect_doppelganger("fuel-mix-full", pfm2)
  pfm3 <- plot_fuel_mix(fm, collapse_renewables = FALSE,
                        colors = c(Coal = "black", "Natural Gas" = "gray60",
                                   Oil = "gray30", Nuclear = "forestgreen",
                                   Hydro = "royalblue", Renewables="palegreen")
  )
  expect_doppelganger("fuel-mix-colors", pfm3)
})
