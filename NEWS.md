# kayadata 0.5.0

This release updates to use the 2020 release of BP Energy Statistics. This
release has data through 2019.

* The World Bank is missing GDP data for a number of nations, such as Syria and 
  Taiwan. Because of this and the  incommensurability of the regions used for 
  aggregate statistics in the World Bank data and the BP data, regional data 
  should be treated with caution. 

  This problem does not hold for individual nations, where missing data appears as
  NA values.
* Fix a bug where `testthat` tests would throw errors on systems where the 
  suggested `vdiffr` package cannot be installed.
  The package now passes `R CMD check` with the environment variable
  `_R_CHECK_DEPENDS_ONLY_` set to `TRUE`.
  

# kayadata 0.4.4

This release is a minor bug fix. 

* Previously there hadn't been an emissions 
  factor for hydroelectricity if renewables were not collapsed in the fuel mix.
  Now there's a factor for hydro if `emissions_factors()` is called with 
  `collapse_renewables = FALSE`.

# kayadata 0.4.3 

This release is mostly a bug fix for changes in the way that the latest releases
of `ggplot2` and `scales` packages handle color names.

* Fix color in `plot_kaya`. Since `scales` v. 1.1.0, `ggplot2` no longer 
  recognizes `"dark blue"` as a color (now it must be `"darkblue"`).
* Add visual regression tests for plotting functions (using `vdiffr`).
* Add optional arguments to plotting functions to override default colors.
* Add documentation site on GitHub Pages via `pkgdown`.
* Add code of conduct and guidelines for contributing.

# kayadata 0.4.2

Update historical data (World Bank and BP) to include the latest data.

# kayadata 0.4.1

Update historical data (World Bank and BP) to include 2018.

# kayadata 0.4.0

Initial release
