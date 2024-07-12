globalVariables(c("in_range", "fuel", "quads", "frac", "label",
                  "qmin", "qmax", "."))


#' Plot Kaya-identity variable
#'
#' @param data A tibble with Kaya-identity data or the name of a region or a
#'   region code.
#' @param variable The name of the variable to plot (character)
#' @param start_year The year to start highlighting the data (should correspond
#' to the beginning of the trend calculation). Set to `NULL` to turn off
#' highlighting.
#' @param stop_year The year to stop highlighting the data (should correspond
#' to the beginning of the trend calculation). Set to `NULL` to turn off
#' highlighting.
#' @param y_lab Optional label for the y-axis
#' @param log_scale Use log scale for y axis
#' @param trend_line Include a trend line
#' @param points Plot points in addition to the line.
#' @param font_size Base size of the font for axis labels and titles.
#' @param colors Named vector of colors to use for the plot. Elements should
#'   include
#'   `PRE`, `POST`, `IN-RANGE`, and `TREND`,
#'   which respectively give the colors for the portion of the plot before
#'   `start_year`, after `stop_year`, between `start_year` and `stop_year`,
#'   and the trend line.
#' @param pre_color Override default color for the portion of the chart before
#'   `start_year`.
#' @param post_color Override default color for the portion of the chart after
#'   `stop_year`.
#' @param in_range_color Override default color for the portion of the chart
#'   between `start_year` and `stop_year`.
#' @param trend_color Override default color for the trend line.
#' @param line_sizes Named vector of sizes to use for the lines in the plot.
#'   Elements should include
#'   `PRE`, `POST`, `IN-RANGE`, and `TREND`,
#'   which respectively give the sizes for lines in the portion of the plot
#'   before `start_year`, after `stop_year`, between
#'   `start_year` and `stop_year`, and the trend line.
#' @param pre_line_size Override default line size for the portion of the chart
#'   before `start_year`.
#' @param post_line_size Override default line size for the portion of the
#'   chart after `stop_year`.
#' @param in_range_line_size Override default line size for the portion of the
#'   chart between `start_year` and `stop_year`.
#' @param trend_line_size Override default size for the trend line.
#' @param point_sizes Named vector of sizes to use for the points in the plot.
#'   Elements should include
#'   `PRE`, `POST`, and `IN-RANGE`,
#'   which respectively give the sizes for points in the portion of the plot
#'   before `start_year`, after `stop_year`, and between
#'   `start_year` and `stop_year`.
#' @param pre_point_size Override default point size for the portion of the
#'   chart before `start_year`.
#' @param post_point_size Override default point size for the portion of the
#'   chart after `stop_year`.
#' @param in_range_point_size Override default point size for the portion of the
#'   chart between `start_year` and `stop_year`.
#'
#' @return A plot object.
#'
#' @examples
#' china <- get_kaya_data("China")
#' plot_kaya(china, "F", 2001, 2011)
#' \dontrun{
#' uk <- get_kaya_data("United Kingdom")
#' plot_kaya(uk, "e", log_scale = TRUE, trend_line = TRUE)
#' plot_kaya(uk, "e", log_scale = TRUE, trend_line = TRUE,
#'           start_year = 1970, stop_year = 2000,
#'           colors = c(PRE="limegreen", POST="darkgreen",
#'                      "IN-RANGE" = "cadetblue", TREND="orange"),
#'           line_sizes = c(PRE=0.5, POST=0.5, "IN-RANGE"=1, TREND=1.5),
#'           point_sizes = c(PRE=2, POST=2, "IN-RANGE"=3))
#' plot_kaya(uk, "e", log_scale = TRUE, trend_line = TRUE,
#'           start_year = 1970, stop_year = 2000,
#'           pre_color = "limegreen", post_color = "limegreen",
#'           trend_color = "magenta",
#'           pre_line_size = 0.5, post_line_size = 0.5,
#'           trend_line_size = 1.5,
#'           pre_point_size = 2, post_point_size = 2, in_range_point_size = 3)
#' plot_kaya("United Kingdom")
#' plot_kaya("GBR")
#' }
#'
#' world <- get_kaya_data("World")
#' plot_kaya(world, "g", 1982, log_scale = TRUE, trend_line = TRUE)
#' @export
#' @importFrom magrittr %>%
plot_kaya <- function(data, variable,
                      start_year = NA, stop_year = NA,
                      y_lab = NULL,
                      log_scale = FALSE, trend_line = FALSE,
                      points = TRUE, font_size = 20,
                      colors = NULL, pre_color = NULL,
                      post_color = NULL, in_range_color = NULL,
                      trend_color = NULL,
                      line_sizes = NULL, pre_line_size = NULL,
                      post_line_size = NULL, in_range_line_size = NULL,
                      trend_line_size = NULL,
                      point_sizes = NULL, pre_point_size = NULL,
                      post_point_size = NULL, in_range_point_size = NULL) {
  if (is.character(data)) {
    if (length(data) != 1) {
      stop("Can't plot kaya data for multiple regions.")
    }
    if (data %in% kaya_region_list()) {
      data <- get_kaya_data(region_name = data, quiet = TRUE)
    } else if (data %in% kayadata::kaya_data$region_code) {
      data <- get_kaya_data(region_code = data, quiet = TRUE)
    } else {
      stop("No region or region code ", data)
    }
  }



  labels <- c(P =  "Population (billions)",
              G =  "Gross Domestic Product ($ trillion)",
              E =  "Energy consumption (quads)",
              F =  "Fossil-fuel carbon emissions (million metric tons)",
              g =  "Per-capita GDP ($ thousand)",
              e =  "Energy intensity of economy (quads per $trillion)",
              f =  "Carbon intensity of energy supply (MMT per quad)",
              ef = "Carbon intensity of economy (tons CO2 per $million)"
  )

  if (is.null(colors)) {
    colors = c("IN-RANGE" = "darkblue",
               "PRE" = "cornflowerblue",
               "POST" = "cornflowerblue",
               "TREND" = "orchid4")
  }
  if (! is.null(pre_color)) colors['PRE'] <- pre_color
  if (! is.null(post_color)) colors['POST'] <- post_color
  if (! is.null(in_range_color)) colors['IN-RANGE'] <- in_range_color
  if (! is.null(trend_color)) colors['TREND'] <- trend_color

  if (is.null(line_sizes)) {
    line_sizes = c("IN-RANGE" = 1.5,
               "PRE" = 1,
               "POST" = 1,
               "TREND" = 1)
  }
  if (! is.null(pre_line_size)) line_sizes['PRE'] <- pre_line_size
  if (! is.null(post_line_size)) line_sizes['POST'] <- post_line_size
  if (! is.null(in_range_line_size)) line_sizes['IN-RANGE'] <- in_range_line_size
  if (! is.null(trend_line_size)) line_sizes['TREND'] <- trend_line_size

  if (is.null(point_sizes)) {
    point_sizes = c("IN-RANGE" = 3,
                   "PRE" = 3,
                   "POST" = 3,
                   "TREND" = 3)
  }
  if (! is.null(pre_point_size)) point_sizes['PRE'] <- pre_point_size
  if (! is.null(post_point_size)) point_sizes['POST'] <- post_point_size
  if (! is.null(in_range_point_size)) point_sizes['IN-RANGE'] <- in_range_point_size

  point_sizes = purrr::set_names(point_sizes,
                                 nm = stringr::str_c(names(point_sizes), "_PT"))

  if (is.null(y_lab)) y_lab <- labels[variable]
  if (is.null(start_year) || is.null(stop_year)) {
    start_year <- NULL
    stop_year <- NULL
  } else {
    if (is.na(start_year)) start_year <- 1980
    if (is.na(stop_year)) stop_year <-  max(data$year)
  }

  se <- FALSE
  if (is.character(trend_line)) {
    trend_line <- stringr::str_to_upper(trend_line) %>% stringr::str_trim()
    if (trend_line %in% c("T", "TRUE")) {
      trend_line <- TRUE
      se <- FALSE
    } else if (trend_line %in% c("F", "FALSE")) {
      trend_line <- FALSE
      se <- FALSE
    } else {
      if (trend_line %in% c("SE", "SE:TRUE", "SE:T")) {
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

  color_scale <- ggplot2::scale_color_manual(values = colors)

  legend <- ggplot2::guides(color = "none", shape = "none")

  if (!any(is.null(start_year), is.null(stop_year))) {
    df <- dplyr::bind_rows(
      data %>% dplyr::filter(year <= start_year) %>%
        dplyr::mutate(in_range = "PRE", pt_in_range = "PRE"),
      data %>% dplyr::filter(year >= stop_year) %>%
        dplyr::mutate(in_range = "POST"),
      data %>%
        dplyr::filter(dplyr::between(year, start_year, stop_year)) %>%
        dplyr::mutate(in_range = "IN-RANGE")
    )
  } else {
    df <- data %>% dplyr::mutate(in_range = "IN-RANGE")
  }

  variable <- sym(variable)
  p <- ggplot2::ggplot(df, aes(x = year, y = !!variable, color = in_range,
                               size = in_range, linewidth = in_range))
  p <- p +
    ggplot2::geom_line(na.rm = TRUE)

  if (points) {
    p <- p + ggplot2::geom_point(aes(size = stringr::str_c(in_range, "_PT")),
                                 na.rm = TRUE)
  }
  p <- p + color_scale + legend

  if (log_scale) {
    p <- p + ggplot2::scale_y_log10()
  }

  if (trend_line) {
    p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x,
                                  data = dplyr::filter(df, in_range == "IN-RANGE"),
                         na.rm = TRUE, se = se,
                         mapping = aes(color = "TREND", linewidth = "TREND"))
  }

  p <- p +
    scale_size_manual( values = point_sizes,
                       guide = "none") +
    scale_linewidth_manual(values = line_sizes,
                       guide = "none") +
    ggplot2::labs(x = "Year", y = y_lab) +
    ggplot2::theme_bw(base_size = font_size) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(vjust = 1.2),
          axis.title.x = ggplot2::element_text(vjust = -0.1),
          legend.key = ggplot2::element_rect(color = NA))

  if (length(unique(df$region)) > 1) {
    p <- p + facet_wrap(~region)
  }

  p
}


#' Plot fuel mix
#'
#' @param fuel_mix A tibble with the mixture of fuels for one or more countries or regions:
#' \describe{
#'   \item{region}{The name of the country or region}
#'   \item{fuel}{The name of the fuel}
#'   \item{quads}{The number of quads per year the country or region consumes}
#'   \item{frac}{The fraction of the country's energy that comes from that fuel}
#' }
#' @param collapse_renewables Combine hydroelectricity and other renewables into
#'        a single category.
#' @param title Include a title on the plot. If `title` is `NULL` (default)
#' or `TRUE`, a default title is created from the names of the regions in
#' `fuel_mix`.
#' If `title` is a character string, that string is used.
#' If `title` is `FALSE`, the plot is produced with no title.
#' @param colors A named vector with the colors to use for
#'   `Coal`, `Oil`, `Natural Gas`, `Nuclear`, `Hydro`, and `Renewables`.
#' @param font_size The base font size.
#' @return A plot object.
#' @examples
#' usa_fuel <- get_fuel_mix("United States", collapse_renewables = FALSE)
#' plot_fuel_mix(usa_fuel)
#' plot_fuel_mix(usa_fuel, collapse_renewables = FALSE)
#' plot_fuel_mix(usa_fuel, collapse_renewables = FALSE,
#'               colors = c(Coal = "black", "Natural Gas" = "gray60",
#'                          Oil = "gray30", Nuclear = "forestgreen",
#'                          Hydro = "royalblue", Renewables="palegreen"))
#' plot_fuel_mix("United States")
#' plot_fuel_mix("USA")
#'
#' @export
plot_fuel_mix <- function(fuel_mix, collapse_renewables = TRUE, title = NULL,
                          colors = NULL, font_size = 20) {
  if (is.character(fuel_mix)) {
    if (length(fuel_mix) != 1) {
      stop("Can't plot fuel mix for multiple regions.")
    }
    if (fuel_mix %in% kaya_region_list()) {
      fuel_mix <- get_fuel_mix(region_name = fuel_mix, quiet = TRUE,
                               collapse_renewables = collapse_renewables)
    } else if (fuel_mix %in% kayadata::kaya_data$region_code) {
      fuel_mix <- get_fuel_mix(region_code = fuel_mix, quiet = TRUE,
                               collapse_renewables = collapse_renewables)
    } else {
      stop("No region or region code ", fuel_mix)
    }
  }

  if (is.null(title) || title == TRUE) {
    title <- fuel_mix$region %>% unique() %>% stringr::str_c(collapse = ", ")
  } else if (!is.character(title)) {
    title <- NULL
  }

  if (is.null(colors)) {
    colors <- c("Coal" = "#e31a1c", "Natural Gas" = "#fdbf6f",
                "Oil" = "#ff7f00", "Nuclear" = "#33a04c",
                "Hydro" = "#69d9a4", "Renewables" = "#b2dfca",
                "Total" = "#a6cee3")
  }

  if (collapse_renewables) {
    fuel_mix <- fuel_mix %>%
      dplyr::mutate(fuel = forcats::fct_recode(.data$fuel, Renewables = "Hydro"))
    colors <- colors[names(colors) != "Hydro"]
  }
  fuel_mix <- fuel_mix %>% dplyr::group_by(fuel) %>%
    dplyr::summarize(quads = sum(quads), frac = sum(frac), .groups = "drop")
  fd <- fuel_mix %>%
    dplyr::arrange(fuel) %>%
    dplyr::mutate(qmin = cumsum(dplyr::lag(quads, default = 0)),
                  qmax = cumsum(quads))
  labels <- fd %>% dplyr::mutate(label = paste0(fuel, ": ", round(quads, 2),
                                         " quads (", scales::percent(frac, 0.1),
                                         ")")) %>%
    dplyr::arrange(fuel) %>% dplyr::select("fuel", "label") %>%
    tidyr::spread(key = fuel, value = label) %>% unlist()
  if (FALSE) {
    message(paste0(levels(fd$fuel), collapse = ", "))
  }
  p <- ggplot2::ggplot(fd, aes(ymin = qmin, ymax = qmax, fill = fuel)) +
    ggplot2::geom_rect(xmin = 2, xmax = 4, na.rm = TRUE) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::xlim(c(0, 4)) +
    ggplot2::scale_fill_manual(values = colors,
                      breaks = names(labels), labels = labels, name = "Fuel") +
    ggplot2::theme_bw(base_size = font_size) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank())

  if (! is.null(title)) p <- p + ggplot2::ggtitle(title)

  p
}
