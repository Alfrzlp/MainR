# https://www.youtube.com/redirect?event=video_description&redir_token=QUFFLUhqbUNlVVpfMDhnaU5Rc1c0dE1iWDhSY2NVOHV1UXxBQ3Jtc0tsZzBwVXBDOTlpZmpTM2lnUHhKaUhoZnpTVGZXaGdldDVwNHRwa1FtblpKUEQ4Vl9vNzhkRVVoY2FjWTNSV2VmSkRzZUllMW43b3ZhaG04OFdsMkJLR3VOZHZKclVueUY2TVk2ZzlVMXFVMTBHM3hqWQ&q=https%3A%2F%2Fbl.ocks.org%2Fsusielu%2F625aa4814098671290a8c6bb88a6301e


# Load Package ------------------------------------------------------------

library(tidyverse)
library(jsonlite)
library(ggrepel)
library(ggtext)
library(Rcpp)

# Read Data ---------------------------------------------------------------

data_raw <- 
  read_json("https://bl.ocks.org/susielu/raw/625aa4814098671290a8c6bb88a6301e/yearNetwork.json") 

glimpse(data_raw)


data_raw %>% 
  as_tibble() %>% 
  # jika gatau pake apa -> auto
  unnest_auto(networkLines) %>% 
  unnest_auto(line) %>% 
  unnest_auto(line) %>% 
  type_convert()

df <- data_raw %>% 
  as_tibble() %>% 
  # jika gatau pake apa -> auto
  unnest_wider(networkLines) %>% 
  unnest_longer(line) %>% 
  unnest_wider(line) %>% 
  type_convert()




# Viz Data ----------------------------------------------------------------

df %>% 
  ggplot(aes(x = year, y = value, colour = network)) +
  geom_line(
   aes(linetype = network %in% c("HBO", "Netflix")),
  ) +
  geom_segment(
    data = ~.x %>% 
      group_by(year) %>% 
      mutate(yend = max(value)),
    aes(x = year, xend = year, y = 0, yend = yend),
    colour = "grey70",
    inherit.aes = F
  ) +
  geom_segment(
    aes(x = 2013, xend = 2017, y = 0, yend = 0),
    colour = "grey70"
  ) +
  geom_point(
    data = ~ dplyr::filter(.x, network %in% c("HBO", "Netflix")),
    size = 8
  ) +
  # geom_line(
  #   data = ~ dplyr::filter(.x, network %in% c("HBO", "Netflix")),
  #   lwd = 2
  # ) +
  geom_text(
    data = ~ dplyr::filter(.x, network %in% c("HBO", "Netflix")),
    aes(label = value),
    colour = "white",
    size = 3
  ) +
  geom_text_repel(
    data = ~ dplyr::filter(.x, year == max(year)),
    aes(label = network),
    nudge_x = 0.15,
    hjust = "left",
    direction = "y",
    segment.colour = "white",
    fontface = "bold"
  ) +
  annotate(
    geom = "richtext",
    x = 2011,
    y = 91,
    label = '<b style="color:#d32f2f">Netflix</b><br>Challenges</br> <b>HBO</b> at the<br> 2017 Emmys',
    size = 6.75
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.8, 0.3))
  ) +
  annotate(
    geom = "label",
    x = 2013:2017,
    y = 75,
    label = 2013:2017,
    colour = "grey70"
  ) +
  scale_colour_manual(
    values = c(
      HBO  = "black",
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
  scale_linetype_manual(
    values = c("TRUE" = "solid", "FALSE" = "dotted"),
    guide = "none"
  ) +
  labs(
    x = NULL, y = NULL
  ) +
  # base_family = "Lato"
  theme_minimal()  +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    #axis.title = element_blank()
  )
  
