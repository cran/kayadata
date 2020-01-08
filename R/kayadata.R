# kayadata is open-source software; you can redistribute it and/or
# modify it under the terms of the MIT License as published by the Open Source
# Initiative.
#
# kayadata is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the MIT License for more details.
#
# You should have received a copy of the MIT License along with this program; if
# not, see <https://opensource.org/licenses/MIT>.

#' kayadata package
#'
#' @docType package
#' @name kayadata-package
#' @references
#' Kaya, Yoichi and Keiichi Yokobori, _Environment, Energy, and Economy:
#' Strategies for Sustainability_ (United Nations University Press, 1998).
#'
#' Nakicenovic, Nebojsa, and Rob Swart (Eds.),
#' _Special Report on Emissions Scenarios_
#' (Cambridge University Press, 2000).
#' <https://www.ipcc.ch/report/emissions-scenarios/>
#'
#' Raupach, Michael R., _et al._, "Global and regional drivers of accelerating
#' CO2 emissions," PNAS **104**, 10288--10293 (2007)
#' [doi: `10.1073/pnas.0700609104`](https://doi.org/10.1073/pnas.0700609104).
#'
#' @description
#'
#' kayadata is a package for working with Kaya identity data for many countries
#' and regions.
#'
#' The Kaya identity, named for the economist Yoichi Kaya, who introduced it
#' (Kaya, 1998); It decomposes the energy-related carbon dioxide emissions from
#' a nation, region, or the world into the product of four components:
#' \deqn{F = P \times g \times e \times f,}{F = P * g * e * f}
#' where _F_ is the total emissions, _P_ is the population, _g_ is the
#' per-capita GDP, _e_ is the energy intensity of the economy, and
#' _f_ is the emissions-intensity of the energy supply.
#' (Nakicenovic and Swart, 2000,
#' [Ch. 3, p. 105](https://www.ipcc.ch/report/emissions-scenarios/);
#' Raupach _et al_, 2007)
#'
#'
#' The data in this packages covers 1960-2018 for population and GDP, and
#' 1965-2018 for energy and fossil-fuel CO2 emissions.
#'
#' The package uses data on population and GDP from the World Bank,
#' using market exchange rates (MER) for GDP because those data go back to
#' 1960. From 1990 onward, Purchasing-Power-Parity (PPP) GDP figures are
#' available as `G_ppp` but using these would require re-calculating
#' `G`, `g`, `e`, and `ef` in the `kaya_data`
#' data frame.
#'
#' The package uses data on energy consumption and fossil-fuel CO2 emissions
#' from the
#' [2019 BP Statistical Review of World Energy](https://www.bp.com/en/global/corporate/energy-economics/statistical-review-of-world-energy.html)
#'
#' @section License:
#'
#'  The \pkg{kayadata} package is open source licensed under the
#'  MIT License.
#'
#' @section Bug reports:
#' \itemize{
#'  \item kayadata issue tracker (<https://github.com/jonathan-g/kayadata/issues>)
#' }
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr distinct filter mutate select summarize summarize_at top_n
#' @importFrom forcats fct_recode
#' @import dplyr
#' @import ggplot2
#' @importFrom stats approx
#' @importFrom tidyr gather spread
#' @importFrom stringr str_to_upper str_c str_trim
#'
NULL
