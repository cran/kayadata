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
