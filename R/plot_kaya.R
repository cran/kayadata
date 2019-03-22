globalVariables(c("in_range", "fuel", "quads", "frac", "label",
                  "qmin", "qmax", "."))


#' Plot Kaya-identity variable
#'
#' @param kaya_data A tibble with Kaya-identity data
#' @param variable The name of the variable to plot (character)
#' @param start_year The year to start highlighting the data (should correspond
#' to the beginning of the trend calculation). Set to \code{NULL} to turn off
#' highlighting.
#' @param stop_year The year to stop highlighting the data (should correspond
#' to the beginning of the trend calculation). Set to \code{NULL} to turn off
#' highlighting.
#' @param y_lab Optional label for the y-axis
#' @param log_scale Use log scale for y axis
#' @param trend_line Include a trend line
#' @param points Plot points in addition to the line.
#' @param font_size Base size of the font for axis labels and titles.
#'
#' @return A plot object.
#'
#' @examples
#' china <- get_kaya_data("China")
#' plot_kaya(china, "F", 2001, 2011)
#' uk <- get_kaya_data("United Kingdom")
#' plot_kaya(uk, "e", log_scale = TRUE, trend_line = TRUE)
#' world <- get_kaya_data("World")
#' plot_kaya(world, "g", 1982, log_scale = TRUE, trend_line = TRUE)
#' @export
#' @importFrom magrittr %>%
plot_kaya <- function(kaya_data, variable,
                      start_year = NA, stop_year = NA,
                      y_lab = NULL,
                      log_scale = FALSE, trend_line = FALSE,
                      points = TRUE, font_size = 20) {
  labels <- c(P =  'Population (billions)',
              G =  'Gross Domestic Product ($ trillion)',
              E =  'Energy consumption (quads)',
              F =  'Fossil-fuel carbon emissions (million metric tons)',
              g =  'Per-capita GDP ($ thousand)',
              e =  'Energy intensity of economy (quads per $trillion)',
              f =  'Carbon intensity of energy supply (MMT per quad)',
              ef = 'Carbon intensity of economy (tons CO2 per $million)'
  )

  if (is.null(y_lab)) y_lab <- labels[variable]
  if (is.null(start_year) || is.null(stop_year)) {
    start_year = NULL
    stop_year = NULL
  } else {
    if (is.na(start_year)) start_year = 1980
    if (is.na(stop_year)) stop_year = max(kaya_data$year)
  }

  se <- FALSE
  if (is.character(trend_line)) {
    trend_line <- str_to_upper(trend_line) %>% str_trim()
    if (trend_line %in% c("T", "TRUE")) {
      trend_line <- TRUE
      se <- FALSE
    } else if (trend_line %in% c("F", "FALSE")) {
      trend_line <- FALSE
      se <- FALSE
    } else {
      if (trend_line %in% c("SE","SE:TRUE","SE:T")) {
        trend_line <- TRUE
        se <- TRUE
      } else {
        if (trend_line %in% c("SE:FALSE", "SE:F")) {
          trend_line <- TRUE
          se <- FALSE
        }
      }
    }
  }

  color_scale = scale_color_manual(values = c("TRUE" = "dark blue",
                                              "PRE" = "cornflowerblue",
                                              "POST" = "cornflowerblue",
                                              "TREND" = "orchid4"
  ))
  legend = guides(color = FALSE, shape = FALSE)

  if (!any(is.null(start_year), is.null(stop_year))) {
    df <- bind_rows(
      kaya_data %>% filter(year <= start_year) %>% mutate(in_range = "PRE"),
      kaya_data %>% filter(year >= stop_year) %>% mutate(in_range = "POST"),
      kaya_data %>% filter(between(year, start_year, stop_year)) %>%
        mutate(in_range = "TRUE")
    )
  } else {
    df <- kaya_data %>% mutate(in_range = TRUE)
  }

  variable <- sym(variable)
  p <- ggplot(df, aes(x = year, y = !!variable, color = in_range))
  p <- p +
    geom_line(size = 1, na.rm = TRUE)

  if (points) {
    p <- p + geom_point(size = 3, na.rm = TRUE)
  }
  p <- p + color_scale + legend

  if (log_scale) {
    p <- p + scale_y_log10()
  }

  if (trend_line) {
    p <- p + geom_smooth(method = "lm", data = filter(df, in_range == "TRUE"),
                         na.rm = TRUE, se = se, mapping = aes(color = "TREND"))
  }

  p <- p +
    labs(x = "Year", y = y_lab) +
    theme_bw(base_size = font_size) +
    theme(axis.title.y = element_text(vjust=1.2),
          axis.title.x = element_text(vjust=-0.1),
          legend.key = element_rect(color = NA))
  p
}


#' Plot fuel mix
#'
#' @param fuel_mix A tibble with the mixture of fuels:
#' \describe{
#'   \item{fuel}{The name of the fuel}
#'   \item{quads}{The number of quads per year the country or region consumes}
#'   \item{frac}{The percentage of the country's energy that comes from that fuel}
#' }
#' @param collapse_renewables Combine hydroelectricity and other renewables into
#'        a single category.
#' @param title Include a title on the plot. If `title` is `NULL` (default)
#' or `TRUE`, a default title is created from the names of the regions in
#' `fuel_mix`.
#' If `title` is a character string, that string is used.
#' If `title` is `FALSE`, the plot is produced with no title.
#' @return A plot object.
#' @examples
#' usa_fuel <- get_fuel_mix("United States", collapse_renewables = FALSE)
#' plot_fuel_mix(usa_fuel)
#' plot_fuel_mix(usa_fuel, collapse_renewables = FALSE)
#'
#' @export
plot_fuel_mix <- function(fuel_mix, collapse_renewables = TRUE, title = NULL) {
  if (is.null(title) || title == TRUE) {
    title <- fuel_mix$region %>% unique() %>% str_c(collapse = ", ")
  } else if (!is.character(title)) {
    title = NULL
  }
  if (collapse_renewables) {
    fuel_mix <- fuel_mix %>%
      mutate(fuel = fct_recode(fuel, Renewables = "Hydro"))
    color_scale <- c("Coal" = "#e31a1c", "Natural Gas" = "#fdbf6f",
                     "Oil" = "#ff7f00", "Nuclear" = "#33a04c",
                     "Renewables" = "#b2dfca", "Total" = "#a6cee3")
  } else {

    color_scale <- c("Coal" = "#e31a1c", "Natural Gas" = "#fdbf6f",
                     "Oil" = "#ff7f00", "Nuclear" = "#33a04c",
                     "Hydro" = "#69d9a4", "Renewables" = "#b2dfca",
                     "Total" = "#a6cee3")

  }
  fuel_mix <- fuel_mix %>% group_by(fuel) %>%
    summarize(quads = sum(quads), frac = sum(frac)) %>% ungroup()
  fd <- fuel_mix %>%
    arrange(fuel) %>%
    mutate(qmin = cumsum(lag(quads, default=0)), qmax = cumsum(quads))
  labels <- fd %>% mutate(label = paste0(fuel, ": ", round(quads,2),
                                         " quads (", scales::percent(frac, 0.1),
                                         ")")) %>%
    arrange(fuel) %>% select(fuel, label) %>%
    tidyr::spread(key = fuel, value = label) %>% unlist()
  if (FALSE) {
    message(paste0(levels(fd$fuel), collapse=", "))
  }
  p <- ggplot(fd, aes(ymin = qmin, ymax = qmax, fill = fuel)) +
    geom_rect(xmin = 2, xmax = 4, na.rm = TRUE) +
    coord_polar(theta = "y") +
    xlim(c(0,4)) +
    scale_fill_manual(values = color_scale,
                      breaks = names(labels), labels = labels, name = "Fuel") +
    theme_bw(base_size = 20) +
    theme(panel.grid=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank())

  if (! is.null(title)) p <- p + ggtitle(title)
  p
}
