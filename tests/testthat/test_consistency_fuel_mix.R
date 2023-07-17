context("Fuel mix consistency")

none.na <- function(...) {
  lst <- list(...)
  f <- function(x, y) x & ! is.na(y)
  purrr::reduce(lst, f, .init = TRUE)
}

test_that("Fuel mix adds up", {
  regions <- kaya_region_list()
  for (r in regions) {
    fm <- get_fuel_mix(r, quiet = TRUE)
    if (nrow(fm) > 0) {
      y <- fm$year[1]
      fm <- fm %>% dplyr::summarize_at(dplyr::vars(quads, frac),
                                       list(~sum(., na.rm = TRUE)))

      kd <- get_kaya_data(r, quiet = TRUE) %>%
        dplyr::filter(none.na(E), year == y)
      #
      # Kludge because the 2023 spreadsheet is inconsistent for Hong Kong and
      # Sri Lanka.
      #
      if (fm$year[[1]] == 2022 && r %in% c("Hong Kong", "Sri Lanka")) {
        tol = c(quads = 1E-2, pct = 3E-2)
      } else {
        tol = c(quads = 1E-2, pct = 1E-2)
      }
      if (nrow(kd) > 0) {
        expect_equal(fm$quads, kd$E, tolerance = tol['quads'],
                     label = "Sum of primary energy sources",
                     expected.label = "Total primary energy consumption",
                     info = stringr::str_c("Region: ", r))
      }
      expect_equal(fm$frac, 1, tolerance = tol['pct'],
                   label = "Sum of percentages",
                   expected.label = "100",
                   info = stringr::str_c("Region: ", r))
    }
  }
})
