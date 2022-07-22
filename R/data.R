#' Kaya identity data for many countries and regions
#'
#' A dataset containing Kaya identity parameters
#' P, G, E, F, g, e, f, and ef for many countries
#'
#' @format A tibble containing 5160 rows and 14 variables:
#' \describe{
#'   \item{region}{Country or region name}
#'   \item{region_code}{Three-letter country or region code}
#'   \item{geography}{Geographic category: "nation", "region", or "world"}
#'   \item{year}{The year}
#'   \item{P}{Population, in billions}
#'   \item{G}{Gross domestic product, in trillions of constant 2015 U.S. dollars.}
#'   \item{E}{Total primary energy consumption, in quads}
#'   \item{F}{CO2 emissions from fossil fuel consumption, in millions of tons}
#'   \item{g}{Per-capita GDP, in thousands of constant 2015 U.S. dollars per person.}
#'   \item{e}{Energy intensity of the economy, in quads per trillion dollars.}
#'   \item{f}{Emissions intensity of the energy supply, in million metric tons
#'            per quad.}
#'   \item{ef}{Emissions intensity of the economy, in metric tons per
#'             million dollars of GDP.}
#'   \item{G_ppp}{Gross domestic product adjusted for purchasing power parity,
#'                in trillions of constant 2017 international dollars}
#'   \item{G_mer}{Gross domestic product at market-exchange-rate,
#'                in trillions of constant 2015 U.S. dollars}
#' }
#' @seealso regions
#' @source <https://data.worldbank.org/indicator/SP.POP.TOTL>,
#' <https://data.worldbank.org/indicator/NY.GDP.MKTP.KD>, and
#' <https://www.bp.com/en/global/corporate/energy-economics/statistical-review-of-world-energy/downloads.html>
"kaya_data"


#' Mix of fuels contributing to primary energy supply for many countries and
#' regions
#'
#' A dataset containing the fuel mix of how many quads and what fraction of
#' total primary energy supply comes from coal, gas, oil, nuclear, and
#' renewable sources.
#'
#' @format A tibble containing 948 rows and 7 variables
#' \describe{
#'   \item{region}{Country or region name}
#'   \item{region_code}{Three-letter country or region code}
#'   \item{geography}{Geographic category: "nation", "region", or "world"}
#'   \item{year}{The year}
#'   \item{fuel}{The fuel: "Coal", "Natural Gas", "Oil", "Nuclear", "Hydro",
#'               and "Renewables"}
#'   \item{quads}{The number of quads of that fuel consumed in the given country
#'                or region and year}
#'   \item{frac}{The fraction of that country or region's total primary
#'              energy consumption from the fuel}
#' }
#' @source <https://www.bp.com/en/global/corporate/energy-economics/statistical-review-of-world-energy/downloads.html>
#' @seealso regions
"fuel_mix"

#' Top-down projections of future Kaya variables for many countries and regions
#'
#' A dataset containing top-down projections of P, G, and E, from the
#' EIA's International Energy Outlook 2017.
#'
#' @format A tibble containing 640 rows and 12 variables
#' \describe{
#'   \item{region}{Country or region name}
#'   \item{region_code}{Three-letter country or region code}
#'   \item{geography}{Geographic category: "nation", "region", or "world"}
#'   \item{year}{The year}
#'   \item{P}{Population, in billions}
#'   \item{G}{Gross domestic product, in trillions of constant 2015 U.S. dollars}
#'   \item{E}{Total primary energy consumption, in quads}
#'   \item{F}{Total CO2 emissions, in millions of metric tons}
#'   \item{g}{Per-capita GDP, in thousands of constant 2015 U.S. dollars per person.}
#'   \item{e}{Energy intensity of the economy, in quads per trillion dollars.}
#'   \item{f}{Emissions intensity of the energy supply, in million metric tons
#'            per quad.}
#'   \item{ef}{Emissions intensity of the economy, in metric tons per
#'             million dollars of GDP.}
#' }
#' @source <https://www.eia.gov/outlooks/archive/ieo17/>
#' @seealso regions
"td_values"

#' Top-down projections of trends in Kaya variables for many countries and regions
#'
#' A dataset containing top-down projections of trends in P, G, and E,
#' from the EIA's International Energy Outlook 2017.
#'
#' @format A tibble containing 226 rows and 11 variables
#' \describe{
#'   \item{region}{Country or region name}
#'   \item{region_code}{Three-letter country or region code}
#'   \item{geography}{Geographic category: "nation", "region", or "world"}
#'   \item{P}{Trend in population, in fraction per year}
#'   \item{G}{Trend in gross domestic product, in fraction per year}
#'   \item{E}{Trend in total primary energy consumption, in fraction per year}
#'   \item{F}{Trend in CO2 emissions, in fraction per year}
#'   \item{g}{Trend in per-capita GDP, in fraction per year}
#'   \item{e}{Trend in energy intensity of the economy, in fraction per year}
#'   \item{f}{Trend in emissions intensity of the energy supply, in fraction per year}
#'   \item{ef}{Trend in emissions intensity of the economy, in fraction per year}
#' }
#' @source <https://www.eia.gov/outlooks/archive/ieo17/>
#' @seealso regions
"td_trends"
