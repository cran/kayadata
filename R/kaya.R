#' Conversion factor: quads per MTOE
#' @keywords internal
mtoe = 1 / 25.2 # quads

globalVariables(c("fuel_mix", "kaya_data", "region", "region_code",
                  "geography", "year", "P", "G", "E", "F", "g", "e", "f", "ef",
                  "td_values", "td_trends"))

#' Look up country or region name from code
#'
#' @param region_code The three-letter country or region code
#' @param data Data frame in which to look up `region_code`
#' @param quiet       Suppress warnings if there is no such country or region.
#'
#' @return The corresponding country or region name, or NULL if there is no
#'         such country or region
#' @keywords internal
#' @importFrom magrittr %>% %$%
lookup_region_code <- function(region_code, data = kayadata::kaya_data, quiet = FALSE) {
  region_name <- data %>%
    dplyr::select(region, region_code) %>% dplyr::distinct() %>%
    dplyr::filter(region_code %in% (!!region_code)) %$% region %>%
    as.character()
  if (is.null(region_name) || length(region_name) == 0) {
    region_name = NULL
    if (!quiet) {
      warning("There is no country or region with code ", region_code, ".")
    }
  }
  region_name
}

#' Get a list of countries in the Kaya data
#'
#' @return a vector of country and region names
#' @export
kaya_region_list <- function() {
  levels(kayadata::kaya_data$region) %>%
    # eliminate levels without instances
    intersect(unique(kaya_data$region)) %>%
    as.character()
}

#' Get Kaya data for a country or region
#'
#' @param region_name The name of a country or region to look up
#' @param gdp         Use market exchange rates (`MER`) or purchasing power
#'                    parity (`PPP`). Default is `MER`.
#' @param quiet       Suppress warnings if there is no such country or region.
#' @param region_code Optional three-letter country or region code to look up
#'                     instead of the `region_name`
#'
#' @return a tibble of Kaya identity data for the country or region:
#' \describe{
#'   \item{region}{The name of the country or region}
#'   \item{year}{The year}
#'   \item{P}{Population, in billions}
#'   \item{G}{Gross domestic product, in trillions of constant 2010 U.S.
#'            dollars.}
#'   \item{E}{Total primary energy consumption, in quads}
#'   \item{F}{CO2 emissions from fossil fuel consumption, in millions of metric
#'            tons}
#'   \item{g}{Per-capita GDP, in thousands of dollars per
#'            person.}
#'   \item{e}{Energy intensity of the economy, in quads per trillion dollars.}
#'   \item{f}{Emissions intensity of the energy supply, in million metric tons
#'            per quad.}
#'   \item{ef}{Emissions intensity of the economy, in metric tons per
#'             million dollars of GDP.}
#' }
#'
#' @details Units for G, g, e, and ef depend on whether the data is requested
#'          in MER or PPP dollars: For MER, dollars are constant 2010 U.S.
#'          dollars. For PPP, dollars are constant 2011 international dollars.
#'
#'          P and MER values for GDP and related quantities are available from
#'          1960 onward.
#'
#'          PPP values for GDP and related quantities are only available from
#'          1990 onward.
#'
#'          Energy-related values (E, F, and derived quantities) are
#'          available from 1965 onward.
#'
#'          Note that emissions (F, f, and ef) use millions of metric tons of
#'          carbon dioxide, not carbon.
#'
#' @examples
#' get_kaya_data("Brazil")
#' get_kaya_data("United Kingdom", "PPP")
#' get_kaya_data(region_name = "United States")
#' get_kaya_data(region_code = "MYS")
#' @export
get_kaya_data <- function(region_name, gdp = c("MER", "PPP"), quiet = FALSE,
                          region_code = NULL) {
  gdp = match.arg(gdp)
  if (! is.null(region_code)) {
    region_name <- lookup_region_code(region_code)
    if (is.null(region_name)) {
      region_name = ""
    }
  }

  data <- kayadata::kaya_data %>%
    dplyr::select(-region_code, -geography) %>%
    dplyr::filter(region %in% region_name)
  if (nrow(data) == 0) {
    if (!quiet) {
      warning("There is no data for country or region ",
              str_c(
                ifelse(isTRUE(region_name == ""), region_code, region_name),
                collapse = ", "))
    }
  }
  if (gdp == "PPP") {
    data <- data %>% mutate(G = .data$G_ppp, g = G / P, e = E / G, ef = F / G)
  }
  # change select call to avoid spurious R CMD check note.
  data <- data %>% select(-.data$G_ppp, -.data$G_mer)
  data
}

#' Get fuel mix for a country or region
#'
#' @param region_name The name of a country or region to look up
#' @param collapse_renewables Combine hydroelectricity and other renewables into
#'   a single category.
#' @param quiet       Suppress warnings if there is no data for that country or
#'   region.
#' @param region_code Optional three-letter country or region code to look up
#'   instead of the `region_name`
#'
#' @return a tibble of fuel mix for the country or region.
#'   That is, the number of quads of each fuel and the
#'   fraction of total primary energy coming from that fuel.
#' @examples
#' get_fuel_mix("United States")
#' get_fuel_mix("World", collapse_renewables = FALSE)
#' get_fuel_mix(region_code = "LCN")
#' @export
get_fuel_mix <- function(region_name, collapse_renewables = TRUE,
                         quiet = FALSE, region_code = NULL) {
  if (! is.null(region_code)) {
    region_name <- lookup_region_code(region_code,
                                      kayadata::fuel_mix)
    if (is.null(region_name) || length(region_name) == 0) {
      region_name = ""
    }
  }
  data <- kayadata::fuel_mix %>%
    select(region, year, fuel, quads, frac) %>%
    dplyr::filter(region %in% region_name) %>%
    group_by(region) %>% dplyr::top_n(1, year) %>% ungroup()


  if (collapse_renewables && nrow(data) > 0) {
    levs <- levels(data$fuel)
    data <- data %>%
      mutate(fuel = forcats::fct_recode(fuel, Renewables = "Hydro") %>%
               forcats::lvls_expand(levs)) %>%
      group_by(region, year, fuel) %>%
      summarize_at(vars(quads, frac), list(~sum(., na.rm = T))) %>%
      ungroup()
  }
  if (nrow(data) == 0 && is.null(region_code)) {
    warning("There is no data for country or region ",
            str_c(
              ifelse(isTRUE(region_name == ""), region_code, region_name),
              collapse = ", "))
  }
  data %>% arrange(region, fuel)
}

#' Get top-down trends for Kaya variables for a country or region, using
#' projections from U.S. Energy Information Administration's International
#' Energy Outlook report.
#'
#' @param region_name The name of a country or region to look up
#' @param quiet       Suppress warnings if there is no data for that country or
#'                    region.
#' @param region_code Optional three-letter country or region code to look up
#'                     instead of the `region_name`
#'
#' @return a tibble of trends for P, G, E, F, g, e, f, and ef for the country,
#' or region in percent per year.
#' @examples
#' get_top_down_trends("Spain")
#' get_top_down_trends(region_code = "RUS")
#' @export
get_top_down_trends <- function(region_name, quiet = FALSE,
                                region_code = NULL) {
  if (! is.null(region_code)) {
    region_name <- lookup_region_code(region_code,
                                      kayadata::td_trends)
    if (is.null(region_name) || length(region_name) == 0) {
      region_name = ""
    }
  }
  data <- kayadata::td_trends %>%
    dplyr::filter(region %in% region_name) %>%
    dplyr::mutate(g = G - P, e = E - G, f = F - E, ef = F - G) %>%
    dplyr::select(region, P, G, g, E, F, e, f, ef)
  if (nrow(data) == 0 && is.null(region_code)) {
    if (!quiet) {
      warning("There is no data for country or region ",
              str_c(
                ifelse(isTRUE(region_name == ""), region_code, region_name),
                collapse = ", "))
    }
  }
  data
}

#' Get top-down projections of Kaya variables for a country or region
#'
#' @param region_name The name of a country or region to look up
#' @param quiet       Suppress warnings if there is no data for that country or
#'                    region.
#' @param region_code Optional three-letter country or region code to look up
#'                     instead of the `region_name`
#'
#' @return a tibble of values for P, G, E, F, g, e, f, and ef for the country
#' or region:
#' \describe{
#'   \item{region}{The name of the country or region}
#'   \item{P}{Population, in billions}
#'   \item{G}{Gross domestic product, in trillions of constant 2010 U.S.
#'            dollars.}
#'   \item{E}{Total primary energy consumption, in quads}
#'   \item{F}{CO2 emissions from fossil fuel consumption, in millions of metric
#'            tons }
#'   \item{g}{Per-capita GDP, in thousands of constant 2010 U.S. dollars per
#'            person.}
#'   \item{e}{Energy intensity of the economy, in quads per trillion dollars.}
#'   \item{f}{Emissions intensity of the energy supply, in million metric tons
#'            per quad.}
#'   \item{ef}{Emissions intensity of the economy, in metric tons per
#'             million dollars of GDP.}
#' }
#'
#' @examples
#' get_top_down_values("New Zealand")
#' get_top_down_values("OECD")
#' get_top_down_values(region_code = "PAK")
#'
#' @export
get_top_down_values <- function(region_name, quiet = FALSE, region_code = NULL) {
  if (! is.null(region_code)) {
    region_name <- lookup_region_code(region_code,
                                      kayadata::td_values)
    if (is.null(region_name) || length(region_name) == 0) {
      region_name = ""
    }
  }
  data <- kayadata::td_values %>%
    dplyr::filter(region %in% region_name) %>%
    dplyr::mutate(g = G/P, e = E/G, f = F/E, ef = F/G) %>%
    dplyr::select(region, year, P, G, g, E, F, e, f, ef)
  if (nrow(data) == 0 && is.null(region_code)) {
    if (!quiet) {
      warning("There is no data for country or region ",
              str_c(
                ifelse(isTRUE(region_name == ""), region_code, region_name),
                collapse = ", "))
    }
  }
  data
}

#' Get top-down projections of Kaya variables for a country or region for a
#' given year
#'
#' @param region_name The name of a country or region to look up
#' @param quiet       Suppress warnings if there is no data for that country or
#'                    region.
#' @param region_code Optional three-letter country or region code to look up
#'                     instead of the `region_name`
#'
#' @param year The year to project to
#'
#' @return a tibble of values for P, G, E, F, g, e, f, and ef for the country
#' or region:
#' \describe{
#'   \item{region}{The name of the country or region}
#'   \item{year}{The year}
#'   \item{P}{Population, in billions}
#'   \item{G}{Gross domestic product, in trillions of constant 2010 U.S.
#'            dollars.}
#'   \item{E}{Total primary energy consumption, in quads}
#'   \item{F}{CO2 emissions from fossil fuel consumption, in millions of metric
#'            tons }
#'   \item{g}{Per-capita GDP, in thousands of constant 2010 U.S. dollars per
#'            person.}
#'   \item{e}{Energy intensity of the economy, in quads per trillion dollars.}
#'   \item{f}{Emissions intensity of the energy supply, in million metric tons
#'            per quad.}
#'   \item{ef}{Emissions intensity of the economy, in metric tons per
#'             million dollars of GDP.}
#' }
#' @examples
#' project_top_down("China", 2037)
#' project_top_down(region_code = "VNM", year = 2043)
#' @export
project_top_down <- function(region_name, year, quiet = FALSE, region_code = NULL) {
  if (! is.null(region_code)) {
    region_name <- lookup_region_code(region_code,
                                      kayadata::td_values)
    if (is.null(region_name) || length(region_name) == 0) {
      region_name = ""
    }
  }
  if (year < min(kayadata::td_values$year, na.rm=T) ||
      year > max(kayadata::td_values$year, na.rm=T)) {
    stop("Projecting top-down values only works for year betweeen ",
         min(kayadata::td_values$year, na.rm = T), " and ",
         max(kayadata::td_values$year, na.rm = T), ".")
  }

  ytmp <- year
  data <- kayadata::td_values %>%
    dplyr::filter(region %in% region_name) %>%
    dplyr::select(-region_code, -geography) %>%
    dplyr::group_by(region) %>%
    # dplyr::summarize_at(vars(-year),
    #                     list(~approx(x = year, y = ., xout = (!!year))$y)) %>%
    dplyr::summarize_at(vars(-year),
                        list(~approx(x = year, y = ., xout = ytmp)$y)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(year = (!!year)) %>%
    dplyr::select(region, year, P, G, g, E, F, e, f, ef)
  if (nrow(data) == 0 && is.null(region_code)) {
    if (!quiet) {
      warning("There is no data for country or region ", region_name)
    }
  }
  data
}


#' Get emission factors for different energy sources
#'
#' @return a tibble of values for emissions factors, in million metric
#'         tons of carbon dioxide per quad of energy.
#' @examples
#' e_fac <- emissions_factors()
#' e_fac
#' @export
emissions_factors <- function() {
  tibble(
    fuel = c("Coal", "Oil", "Natural Gas", "Nuclear", "Renewables"),
    emission_factor = c(94.4, 70.0, 53.1, 0.0, 0.0)
  ) %>%
    mutate(fuel = ordered(fuel, levels = c("Coal", "Natural Gas", "Oil",
                                           "Nuclear", "Renewables")))
}

#' Get power output from generation sources
#'
#' Nameplate capacity and capacity factors for different electrical generation
#' technologies. The average power supplied over a year is the nameplate
#' capacity times the capacity factor.
#'
#' Data for fossil fuels comes from EIA
#'
#' @return a tibble of values for generation sources
#' \describe{
#'     \item{fuel}{Energy source: Coal, Nuclear, Gas, Solar Thermal, or Wind}
#'     \item{description}{Text description of the power source}
#'     \item{nameplate_capacity}{Maximum sustained power output, in megawatts}
#'     \item{capacity_factor}{Capacity factor: the fraction of the nameplate
#'         capacity that the plant can provide, averaged over a typical year}
#' }
#' @examples
#' gc <- generation_capacity()
#' gc
#' @references
#' Environmental Protection Agency (2018) "Electric Power Monthly,"
#' (October 2018) <https://www.eia.gov/electricity/monthly/current_month/epm.pdf>,
#' \href{Table 6.7.A}{https://www.eia.gov/electricity/monthly/epm_table_grapher.php?t=epmt_6_07_a}.
#'
#' Pielke, Jr., Roger A., \emph{The Climate Fix} (Basic Books, 2010).
#' @export
generation_capacity <- function() {
  tibble(
    fuel = c("Coal", "Nuclear", "Natural Gas", "Solar Thermal", "Wind"),
    description = c("Large coal-fired power plant",
                    "Large nuclear power plant",
                    "Gas-fired power plant",
                    "Concentrated solar-thermal power plant",
                    "Wind turbine"),
    nameplate_capacity = c(1000, 1000, 500,  200,  2.5),
    capacity_factor    = c(0.53, 0.75, 0.56, 0.30, 0.30)
  )
}

#' The number of megawatts it takes to replace a quad.
#'
#' The number of megawatts of average power output over a year to
#' produce one quad of energy
#'
#' @return The number of megawatts equivalent to one quad per year.
#' @examples
#' mwe <- megawatts_per_quad()
#' mwe
#' @export
megawatts_per_quad <- function() {
  1.1E4
}
