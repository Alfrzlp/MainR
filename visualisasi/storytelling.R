library(tidyverse)

dat <- tibble(
  id = 1:19,
  fulfilled = c(803, 865, 795, 683, 566, 586, 510, 436, 418, 364, 379, 372, 374, 278, 286, 327, 225, 222, 200),
  accuracy = c(86, 80, 84, 82, 86, 80, 80, 93, 88, 87, 85, 85, 83, 94, 86, 78, 89, 88, 91),
  error = c(10, 14, 10, 14, 10, 16, 15, 6, 11, 7, 12, 13, 8, 4, 12, 12, 7, 10, 7),
  null = 100 - accuracy - error
) %>% 
  mutate(across(accuracy:null, ~. / 100))


dat

theme_set(theme_minimal())

dat_long <- dat %>% 
  pivot_longer(
    cols = accuracy:null,
    names_to = 'type',
    values_to = 'percent'
  )
dat_long

dat_long %>% 
  ggplot(aes(id, percent, fill = factor(type, levels = c('null', 'accuracy', 'error')))) +
  geom_col() +
  labs(
    title = 'Warehouse Accuracy Rates',
    x = 'Warehouse ID',
    y = '% of total orders',
    fill = element_blank()
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.2),
    labels = function(x) scales::percent(x, accuracy = 1)
  )




# -------------------------------------------------------------------------
categorial_dat <- dat_long %>% 
  mutate(
    id = as.character(id),
  )

averages <- dat_long %>% 
  group_by(type) %>% 
  summarise(percent = mean(percent)) %>% 
  mutate(id = 'ALL') 

dat_with_summary <- categorial_dat %>% 
  bind_rows(averages)

ordered_dat <- dat_with_summary %>% 
  mutate(
    type = factor(type, levels = c('error', 'null', 'accuracy')),
    id = fct_reorder(id, percent, .desc = T)
  ) 

# Set colors as variable for easy change later
unhighlighted_color <- 'grey80'
highlighted_color <- '#E69F00'
avg_error <- 'black'
avg_rest <- 'grey40'

# Compute new column with colors of each bar
colored_dat <- ordered_dat %>% 
  mutate(
    custom_colors = case_when(
      id == 'ALL' & type == 'error' ~ avg_error,
      id == 'ALL' ~ avg_rest,
      type == 'error' & percent > 0.1 ~ highlighted_color,
      T ~  unhighlighted_color
    )
  )

p <- colored_dat %>% 
  ggplot(aes(x = percent, y = id)) +
  geom_col(
    aes(group = type),
    col = 'white',
    fill = colored_dat$custom_colors # Set colors manually
  )
p

p <- p +
  coord_cartesian(
    xlim = c(0, 1), 
    ylim = c(0.5, 20.5), 
    expand = F, # removes white spaces at edge of plot
    clip = 'off' # allows drawing outside of panel
  )
p

unhighlighed_col_darker <- 'grey60'
p <- p +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    labels = scales::percent,
    position = 'top'
  ) +
  labs(
    title = 'Accuracy rates for highest volume warehouses',
    y = 'WAREHOUSE ID',
    x = '% OF TOTAL ORDERS FULFILLED'
  ) +
  theme(
    axis.line.x = element_line(colour = unhighlighed_col_darker),
    axis.ticks.x = element_line(colour = unhighlighed_col_darker),
    axis.text = element_text(colour = unhighlighed_col_darker),
    text = element_text(colour = unhighlighed_col_darker),
    plot.title = element_text(colour = 'black')
  )
p


p <- p +
  theme(
    axis.title.x = element_text(hjust = 0),
    axis.title.y = element_text(hjust = 1),
    plot.title.position = 'plot'
    # aligns the title to the whole plot and not the (inner) panel
  )
p

p <- p +
  # Overwriting previous scale will generate a warning but that's ok
  scale_x_continuous(
    breaks = seq(0, 1, 0.2), # We still want the axes ticks
    labels = rep('', 6), # Empty strings as labels
    position = 'top'
  ) +
  annotate(
    'text',
    x = seq(0, 1, 0.2),
    y = 20.75,
    label = scales::percent(seq(0, 1, 0.2), accuracy = 1),
    size = 3,
    hjust = c(0, rep(0.5, 4), 1),
    # individual hjust here
    vjust = 0, 
    col = unhighlighed_col_darker
  ) +
  theme(
    axis.title.x = element_text(hjust = 0, vjust = 0)
    # change vjust to avoid overplotting
  )
## Scale for 'x' is already present. Adding another scale for 'x', which will
## replace the existing scale.
p



text_labels <- colored_dat %>% 
  dplyr::filter(type == 'error', percent > 0.1) %>% 
  mutate(percent = scales::percent(percent, accuracy = 1))

p <- p +
  geom_text(
    data = text_labels, 
    aes(x = 1, label = percent), 
    hjust = 1.1,
    col = 'white',
    size = 4
  )

p


library(ggtext)
p <- p +
  annotate(
    'richtext',
    x = 1,
    y = 21.25,
    label = "ACCURATE | NULL | <span style = 'color:#E69F00'>ERROR</span>",
    hjust = 1,
    vjust = 0, 
    col = unhighlighed_col_darker, 
    size = 4,
    label.colour = NA,
    fill = NA
  )
p


# Save text data in a tibble
tib_summary_text <- tibble(
  x = 0, 
  y = c(1.65, 0.5), 
  label = c("<span style = 'color:grey60'>OVERALL:</span> **The error rate is 10% across all<br>66 warehouses**. <span style = 'color:grey60'>The good news is that<br>the accuracy rate is 85% so we\'re hitting<br>the mark in nearly all our centers due to<br>the quality initiatives implemented last year.</span>",
            "<span style = 'color:#E69F00'>OPPORTUNITY TO IMPROVE:</span> <span style = 'color:grey60'>10 centers<br>have higher than average error rates of<br>10%-16%.</span> <span style = 'color:#E69F00'>We recommend investigating<br>specific details and **scheduling meetings<br>with operations managers to<br>determine what's driving this.**</span>"
  )
)

# Create text plot with geom_richtext() and theme_void()
text_plot <- tib_summary_text %>% 
  ggplot() +
  geom_richtext(
    aes(x, y, label = label),
    size = 3,
    hjust = 0,
    vjust = 0,
    label.colour = NA
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 2), clip = 'off') +
  # clip = 'off' is important for putting it together later.
  theme_void()
text_plot




# Save texts as variables for better code legibility
# Here I used Markdown syntax
# To enable its rendering, use element_markdown() in theme
title_text <- "**Action needed:** 10 warehouses have <span style = 'color:#E69F00'>high error rates</span>"
subtitle_text <- "<span style = 'color:#E69F00'>DISCUSS:</span> what are <span style = 'color:#E69F00'>**next steps to improve errors**</span> at highest volume warehouses?<br><span style = 'font-size:10pt;color:grey60'>The subset of centers shown (19 out of 66) have the highest volume of orders fulfilled</span>"
caption_text <- "SOURCE: ProTip Dashboard as of Q4/2021. See file xxx for additional context on remaining 47 warehouses<br><span style = 'font-size:6pt;color:grey60'>Original: Storytelling with Data - improve this graph! exercise | {ggplot2} remake by Albert Rapp (@rappa753)."

# Compose plot
library(patchwork)

p <- p +
  text_plot +
  # Make text plot narrower
  plot_layout(widths = c(0.6, 0.4)) +
  # Add main message via title and subtitle
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_markdown(
        margin = margin(b = 0.4, unit = 'cm'),
        # 0.4cm margin at bottom of title
        size = 16
      ),
      plot.subtitle = element_markdown(
        margin = margin(b = 0.4, unit = 'cm'),
        # 0.4cm margin at bottom of title
        size = 11.5
      ),
      plot.caption.position = 'plot',
      plot.caption = element_markdown(
        hjust = 0, 
        size = 7, 
        colour = unhighlighed_col_darker, 
        lineheight = 1.25
      ),
      plot.background = element_rect(fill = 'white', colour = NA)
      # This is only a trick to make sure that background really is white
      # Otherwise, some browsers or photo apps will apply a dark mode
    )
  ) 

remotes::install_github("thebioengineer/camcorder")
camcorder::gg_record(
  dir = 'E:/Visualisasi', dpi = 300, width = 16, height = 9, units = 'cm'
)


ragg::agg_png(
  "E:/Visualisasi/opo2.png",
  width = 9, height = 5, 
  units = "in", res = 300,
  scaling = 0.9
)
p
dev.off()


ragg::agg_png("E:/Visualisasi/opo.png", width = 9, height = 5, units = "in", res = 300)
p
dev.off()