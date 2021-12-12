87.00*0.3 + 0.35*(92.00	+ 87.00)
# Load Package ------------------------------------------------------------

library(tidyverse)
library(datasauRus)
library(scales)
library(hrbrthemes)

windowsFonts("Fira Code" = windowsFont("Fira Code"))
windowsFonts("Lato" = windowsFont("Lato"))


datasaurus_dozen %>% 
  distinct(dataset) %>% 
  pull


# Annotation --------------------------------------------------------------
summary_annotation <- 
  datasaurus_dozen %>% 
  filter(dataset == "dino") %>%
  summarise(
    across(c(x, y), list(avg = mean, std = sd)),
    r = cor(x, y)
  ) %>% 
  mutate(
    across(everything(), ~number(.x, accuracy = 0.01)),
    label = str_glue(
      "
      Mean. X: {x_avg}
      Std.  X: {x_std}
      Mean. Y: {y_avg}
      Std.  Y: {y_std}
      Corr.  : {r}
      "
    )
  ) %>% 
  pull(label)


plot_dino <- 
  datasaurus_dozen %>% 
  filter(dataset == "dino") %>% 
  ggplot(aes(x, y)) +
  geom_point(colour = "greenyellow") +
  annotate(
    geom = "label",
    label = summary_annotation,
    # posisis
    x = -Inf,
    y = -Inf,
    fill = "ivory",
    colour = "gray20",
    hjust = "left",
    vjust = "bottom",
    family = "Fira Code",
    lineheight = 1
  ) +
  scale_x_continuous(
    breaks = seq.int(0, 100, by = 25),
    # alt shift atas/bawah
    labels = seq.int(0, 100, by = 25),
    sec.axis = dup_axis()
  ) +
  scale_y_continuous(
    breaks = seq.int(0, 100, by = 25),
    labels = seq.int(0, 100, by = 25),
    sec.axis = dup_axis()
  ) +
  labs(
    x = NULL, y = NULL,
    title = "Datasaurus Dozen",
    subtitle = "Datasets : Dino"
  ) +
  theme_ft_rc(
    base_family = "Fira Code",
    grid = "XY",
    ticks = T
  ) 


ggsave(
  filename = "dino.png",
  plot = plot_dino,
  width = 8,
  height = 5,
  dpi = 300,
  type = "cairo-png"
)


# Viz ---------------------------------------------------------------------

datasaurus_dozen %>% 
  split(.$dataset) %>% 
  walk2(
    .x = .,
    .y = names(.),
    ~ {
      
      message("Generating plot for ", .y)
      
      summary_annotation <- 
        .x %>% 
        filter(dataset == .y) %>%
        summarise(
          across(c(x, y), list(avg = mean, std = sd)),
          r = cor(x, y)
        ) %>% 
        mutate(
          across(everything(), ~number(.x, accuracy = 0.01)),
          label = str_glue(
            "
      Mean. X: {x_avg}
      Std.  X: {x_std}
      Mean. Y: {y_avg}
      Std.  Y: {y_std}
      Corr.  : {r}
      "
          )
        ) %>% 
        pull(label)
      
      
      plot_dino <- 
        .x %>% 
        filter(dataset == .y) %>% 
        ggplot(aes(x, y)) +
        geom_point(colour = "greenyellow") +
        annotate(
          geom = "label",
          label = summary_annotation,
          # posisis
          x = -Inf,
          y = -Inf,
          fill = "ivory",
          colour = "gray20",
          hjust = "left",
          vjust = "bottom",
          family = "Fira Code",
          lineheight = 1
        ) +
        scale_x_continuous(
          breaks = seq.int(0, 100, by = 25),
          # alt shift atas/bawah
          labels = seq.int(0, 100, by = 25),
          sec.axis = dup_axis()
        ) +
        scale_y_continuous(
          breaks = seq.int(0, 100, by = 25),
          labels = seq.int(0, 100, by = 25),
          sec.axis = dup_axis()
        ) +
        labs(
          x = NULL, y = NULL,
          title = "Datasaurus Dozen",
          subtitle = str_glue("Datasets : {str_to_title(.y)}")
        ) + 
        theme_ft_rc(
          base_family = "Fira Code",
          grid = "XY",
          ticks = T
        ) 
      
      
      ggsave(
        filename = str_glue("Plot Dino/{janitor::make_clean_names(.y)}.png"),
        plot = plot_dino,
        width = 8,
        height = 5,
        dpi = 300,
        type = "cairo-png"
      )
    }
  )

