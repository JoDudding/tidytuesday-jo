#-------------------------------------------------------------------------------
#' _setup.r
#-------------------------------------------------------------------------------
#' jo dudding
#' May 2025
#' common setup
#-------------------------------------------------------------------------------

#--- options ---

options(
  dplyr.width = Inf,
  papersize = "a4",
  tab.width = 2,
  width = 80,
  max.print = 25,
  stringsAsFactors = FALSE,
  lubridate.week.start = 6,
  tibble.print_max = 25,
  tibble.print_min = 25,
  tibble.width = Inf,
  dplyr.summarise.inform = FALSE,
  tidyverse.quiet = TRUE
)

#--- packages ---

library(tidyverse, quietly = TRUE)
library(scales, quietly = TRUE)
library(cli, quietly = TRUE)
library(glue, quietly = TRUE)
library(gt, quietly = TRUE)
library(tidytuesdayR, quietly = TRUE)
library(systemfonts, quietly = TRUE)

#--- function for new template ---

use_tt_template <- function(ref_date = lubridate::today()) {
  # convert string to date
  ref_date <- lubridate::as_date(ref_date)

  # go back to the latest tuesday
  tt_date <- withr::with_options(
    list(lubridate.week.start = 2),
    lubridate::floor_date(ref_date, "week", 2)
  ) |>
    as.character()

  # read the master quarto template
  x1 <- readLines("templates/tt_quarto.qmd")

  # update the date
  x2 <- gsub("tt_date", tt_date, x = x1)

  # and the year
  x3 <- gsub("tt_year", stringr::str_sub(tt_date, 1, 4), x = x2)
  
  # and today
  x4 <- gsub("tt_today", as.character(today()), x = x3)

  # write to this project
  writeLines(x4, glue::glue("weeks/tt_{tt_date}.qmd"))

  # open the quarto
  usethis::edit_file(glue::glue("weeks/tt_{tt_date}.qmd"))
}

#--- function to save preview image ---

gg_preview <- function(name) {
  gg_name <- glue("images/{name}.png")
  ggsave(gg_name, width = 7, height = 5, dpi = 200)
}

#--- fonts ---

# "Segoe UI",
# Roboto

#--- colours ---

pulse <- list(
  primary = "#593196",
  secondary = "#A991D4",
  success = "#13B955",
  danger = "#009CDC",
  warning = "#EFA31D",
  info = "#FC3939",
  light = "#F9F8FC",
  dark = "#17141F",
  blue = "#007bff",
  indigo = "#6610f2",
  purple = "#593196",
  pink = "#e83e8c",
  red = "#fc3939",
  orange = "#fd7e14",
  yellow = "#efa31d",
  green = "#13b955",
  teal = "#20c997",
  cyan = "#009cdc",
  black = "#000",
  white = "#fff",
  grey = "#868e96",
  grey_dark = "#343a40",
  grey_10 = "#fafafa",
  grey_20 = "#f9f8fc",
  grey_30 = "#ededed",
  grey_40 = "#cbc8d0",
  grey_50 = "#adb5bd",
  grey_60 = "#868e96",
  grey_70 = "#444",
  grey_80 = "#343a40",
  grey_90 = "#17141f"
)

#--- palette options ---

pulse_colours <- list(
  base = c(pulse$purple, pulse$pink, pulse$orange),
  all = c(
    pulse$purple, pulse$orange, pulse$teal,
    pulse$pink, pulse$cyan, pulse$yellow, pulse$red, pulse$indigo,
    pulse$green, pulse$blue
  ),
  other = c(
    pulse$orange, pulse$teal,
    pulse$pink, pulse$cyan, pulse$yellow, pulse$red, pulse$indigo,
    pulse$green, pulse$blue
  ),
  roygbiv = c(
    pulse$red, pulse$orange, pulse$yellow,
    pulse$green, pulse$blue, pulse$indigo, pulse$purple
  ),
  purples = c("#C7BADC", "#291646"),
  oranges = c("#FED4B0", "#542A06"),
  pinks = c("#F7BED8", "#6C1C41"),
  teals = c("#B4EDDC", "#0A4332"),
  purple_orange = c(pulse$purple, pulse$grey_30, pulse$orange),
  purple_pink = c(pulse$purple, pulse$grey_30, pulse$pink),
  purple_teal = c(pulse$purple, pulse$grey_30, pulse$teal),
  dpurple_lpink = c("#291646", "#F7BED8"),
  dpurple_lteal = c("#291646", "#B4EDDC")
)

#--- function to create a palette ---

pulse_palettes <- function(
    name = "purples", 
    n, 
    all_palettes = pulse_colours,
    type = c("discrete", "continuous"),
    reverse = FALSE) {
  palette <- all_palettes[[name]]
  if (missing(n)) {
    n <- length(palette)
  }
  if (reverse) {
    palette <- rev(palette)
  }
  type <- match.arg(type)
  out <- switch(type,
    continuous = grDevices::colorRampPalette(palette)(n),
    discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}

# pulse_palettes("base", type = "continuous", n = 9) |>
#  show_col()

# pulse_palettes("dpurple_lteal", type = "continuous", n = 9, reverse = TRUE) |>
#  show_col()

purple_gradient <- function(n, reverse = FALSE) {
  pulse_palettes("purples", type = "continuous", n = n, reverse = reverse)
}

pp_gradient <- function(n, reverse = FALSE) {
  pulse_palettes("dpurple_lpink", type = "continuous", n = n, reverse = reverse)
}

#--- ggplot theme ---

base_size <- 10

theme_set(
  theme_minimal(base_size = base_size, base_family = "Segoe UI") +
    theme(
      text = element_text(colour = pulse$dark, lineheight = 1.2),
      plot.title = element_text(
        size = rel(1.5), face = "bold",
        lineheight = 1.2
      ),
      plot.subtitle = element_text(size = rel(1.2)),
      plot.caption = element_text(hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.background = ggplot2::element_rect(fill = pulse$light, colour = NA),
      plot.background = ggplot2::element_rect(fill = pulse$light, colour = NA),
      legend.background = ggplot2::element_rect(fill = pulse$light, colour = NA),
      strip.background = ggplot2::element_rect(fill = pulse$grey_30, colour = NA),
      panel.grid.major.x = element_line(linewidth = 0.15),
      panel.grid.major.y = element_line(linewidth = 0.15),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )
)

#--- geom default ---

update_geom_defaults("col", aes(fill = pulse$secondary, colour = NA))
update_geom_defaults("bar", aes(fill = pulse$secondary, colour = NA))

update_geom_defaults("line", aes(colour = pulse$primary, linewidth = 1))
update_geom_defaults("segment", aes(colour = pulse$primary))

update_geom_defaults("point", aes(colour = pulse$primary, size = 2))

update_geom_defaults("text", aes(
  family = "Segoe UI", colour = pulse$dark,
  size = base_size / .pt * 0.8
))
update_geom_defaults("label", aes(
  family = "Segoe UI", colour = pulse$dark,
  size = base_size / .pt * 0.8
))

#--- custom scales ---

scale_x_c <- function(label = comma, ...) {
  scale_x_continuous(..., label = label, expand = expansion(mult = c(0, 0.05)))
}

scale_y_c <- function(label = comma, ...) {
  scale_y_continuous(..., label = label, expand = expansion(mult = c(0, 0.05)))
}

scale_colour_pulse_d <- function(name = "all") {
  ggplot2::scale_colour_manual(values = pulse_palettes(
    name = name,
    type = "discrete"
  ))
}

scale_fill_pulse_d <- function(name = "all") {
  ggplot2::scale_fill_manual(values = pulse_palettes(
    name = name,
    type = "discrete"
  ))
}

scale_colour_pulse_c <- function(name = "base") {
  ggplot2::scale_colour_gradientn(colours = pulse_palettes(
    name = name,
    type = "continuous"
  ))
}

scale_fill_pulse_c <- function(name = "base") {
  ggplot2::scale_fill_gradientn(colours = pulse_palettes(
    name = name,
    type = "continuous"
  ))
}

scale_colour_pulse_g <- function(name = "purples") {
  ggplot2::scale_colour_gradientn(colours = pulse_palettes(
    name = name,
    type = "continuous"
  ))
}

scale_fill_pulse_g <- function(name = "purples") {
  ggplot2::scale_fill_gradientn(colours = pulse_palettes(
    name = name,
    type = "continuous"
  ))
}

scale_colour_pulse_v <- function(name = "purple_orange") {
  ggplot2::scale_colour_gradientn(colours = pulse_palettes(
    name = name,
    type = "continuous"
  ))
}

scale_fill_pulse_v <- function(name = "purple_orange") {
  ggplot2::scale_fill_gradientn(colours = pulse_palettes(
    name = name,
    type = "continuous"
  ))
}

#--- summaryx function ---

summaryx <- function(data, var) {
  data |>
    summarise(
      min = min({{ var }}, na.rm = TRUE),
      p1 = quantile({{ var }}, probs = 0.01, na.rm = TRUE, names = FALSE),
      p5 = quantile({{ var }}, probs = 0.05, na.rm = TRUE, names = FALSE),
      p10 = quantile({{ var }}, probs = 0.1, na.rm = TRUE, names = FALSE),
      p25 = quantile({{ var }}, probs = 0.25, na.rm = TRUE, names = FALSE),
      p33 = quantile({{ var }}, probs = 0.33, na.rm = TRUE, names = FALSE),
      median = median({{ var }}, na.rm = TRUE),
      p67 = quantile({{ var }}, probs = 0.67, na.rm = TRUE, names = FALSE),
      p75 = quantile({{ var }}, probs = 0.75, na.rm = TRUE, names = FALSE),
      p90 = quantile({{ var }}, probs = 0.9, na.rm = TRUE, names = FALSE),
      p95 = quantile({{ var }}, probs = 0.95, na.rm = TRUE, names = FALSE),
      p99 = quantile({{ var }}, probs = 0.99, na.rm = TRUE, names = FALSE),
      max = max({{ var }}, na.rm = TRUE),
      n_obs = n(),
      sum = sum({{ var }}, na.rm = TRUE),
      mean = mean({{ var }}, na.rm = TRUE),
      sd = sd({{ var }}, na.rm = TRUE),
      lci_95 = mean({{ var }}, na.rm = TRUE) -
        qnorm(0.975) * sd({{ var }}, na.rm = TRUE),
      uci_95 = mean({{ var }}, na.rm = TRUE) +
        qnorm(0.975) * sd({{ var }}, na.rm = TRUE),
      n_miss = sum(is.na({{ var }})),
      n_zero = sum({{ var }} == 0, na.rm = TRUE),
      pct_miss = mean(is.na({{ var }})),
      .groups = "drop"
    ) |>
    mutate(
      pct_zero = n_zero / n_obs
    )
}
