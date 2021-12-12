# Setup -------------------------------------------------------------------

#' Recreation of data visualization created bu Susie Lu
#' Link to original artwork: https://bl.ocks.org/susielu/625aa4814098671290a8c6bb88a6301e
#' Recreation by Muhammad Aswan Syahputra

library(tidyverse)
library(jsonlite)
library(ggtext)
library(ggrepel)
library(Rcpp)

detach('package:stats')

emmys <- 
  read_json(
    "https://bl.ocks.org/susielu/raw/625aa4814098671290a8c6bb88a6301e/yearNetwork.json"
  ) %>% 
  as_tibble() %>% 
  unnest_auto(networkLines) %>% 
  unnest_auto(line) %>% 
  unnest_auto(line) %>% 
  mutate(year = as.numeric(year))

# Recreate visualization --------------------------------------------------

emmys_plot <- 
  df %>% 
  ggplot(aes(year, value, colour = network)) +
  geom_line(
    aes(linetype = network %in% c("Netflix", "HBO"),
        size = network %in% c("Netflix", "HBO"))
  ) +
  geom_segment(
    aes(x = 2013, xend = 2017, y = 0, yend = 0),
    colour = "gray92"
  ) +
  geom_segment(
    data = ~.x %>% 
      group_by(year) %>% 
      summarise(value = max(value)),
    aes(x = year, xend = year, y = 0, yend = value),
    colour = "gray92"
  ) +
  geom_point(
    data = ~ .x %>% 
      filter(network %in% c("Netflix", "HBO")),
    size = 9
  ) +
  geom_text(
    data = ~ .x %>% 
      filter(network %in% c("Netflix", "HBO")),
    aes(label = value),
    family = "Lato",
    colour = "white",
    size = 2.5,
    fontface = 'bold'
  ) +
  geom_text_repel(
    data = ~ .x %>% 
      filter(year == 2017) %>% 
      mutate(
        year = if_else(
          network %in% c("Netflix", "HBO"),
          2017.05,
          year
        )
      ),
    aes(label = network),
    family = "Lato",
    fontface = "bold",
    size = 2.5,
    nudge_x = 0.15,
    hjust = 0,
    direction = "y",
    segment.size = NA
  ) +
  geom_label(
    data = ~ .x %>% 
      distinct(year),
    aes(y = 75, label = year),
    family = "Lato",
    fontface = "bold",
    colour = "gray75",
    size = 1.75,
    fill = "white",
    label.size = NA,
    label.padding = unit(0.15, "lines")
  ) +
  annotate(
    geom = "richtext",
    x = 2011,
    y = 80,
    label = '<b style="color:#d32f2f">Netflix</b><br>Challenges<br><b>HBO</b> at the<br>2017 Emmys',
    hjust = 0,
    family = "Lato",
    label.size = NA,
    size = 6.75
  ) +
  scale_x_continuous(
    limits = c(2011, 2018),
    breaks = 2013:2017,
    expand = c(0, 0)
  ) +
  # dotted, twodashm longdash, dotdash, blank
  scale_linetype_manual(
    values = c("TRUE" = "solid", "FALSE" = "dotted"),
    guide = "none"
  ) +
  scale_size_manual(
    values = c("TRUE" = 3, "FALSE" = 1),
    guide = "none"
  ) +
  scale_colour_manual(
    values = c(
      HBO = "black",
      Netflix = "#D32F2F",
      NBC = "#ffc107",
      "FX Networks" = "#0097a7",
      ABC = "#00BFA5",
      CBS = "#00BCD4",
      FOX = "#3f51b5",
      Showtime = "#C5CAE9",
      AMC = "#D32F2F",
      PBS = "#B39DDB",
      Amazon = "#ffc107",
      "Nat Geo" = "#ff9800",
      Hulu = "#00BFA5"
    ),
    guide = "none"
  ) +
  theme_minimal(base_family = "Lato") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  coord_cartesian(clip = "off")

emmys_plot


# Save images -------------------------------------------------------------

ggsave(
  "outfile/dot2.png",
  #plot = emmys_plot,
  width = 8,
  height = 5,
  dpi = 300,
  type = "cairo-png"
)

