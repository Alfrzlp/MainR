library(tidyverse)

tibble(terima = c(20000, 500 , 0),
       keluar = c(0, 1000, 1000),
       saldo = c(20000, NA, NA)) %>% 
  mutate(saldo = cumsum(terima - keluar))

windowsFonts("Arial Narrow" = windowsFont("Arial Narrow"))

# Data --------------------------------------------------------------------

s = 
"month Internal	External	Overall Goal
Jan	47,6	44,8	45,1	60
Feb	37,9	48,5	47,3	60
Mar	17,6	49,5	46,2	60
Apr	18,6	55,2	50,4	60
May	40,6	56,5	55,6	60
Jun	28,8	60,7	53,9	60
Jul	27,1	44,2	42,9	60
Aug	36,9	29,0	31,2	60
Sep	37,1	61,2	59,2	60
Oct	25,9	44,9	41,6	60
Nov	51,2	76,6	71,9	60
Dec	40,6	34,7	36,2	60
"


# Data Wragling  --------------------------------------------------------

toplot <- 
  read.table(textConnection(s), header = T) %>% 
  mutate_all(~str_replace_all(.x, ',', '.')) %>% 
  type_convert() %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    cols = 2:3,
    names_to = "candidate",
    values_to = "ndays"
  ) %>% 
  mutate(
    month = fct_inorder(month),
    candidate = str_to_title(candidate)
  )
  

toplot

# visualisasi Data --------------------------------------------------------
colpal <- c("Internal" = "darkorange", "External" = "turquoise4")

toplot %>% 
  ggplot(aes(x = month, y = ndays, group = candidate, colour = candidate)) +
  geom_line(
    size = 1.25,
    lineend = "round"
  ) +
  geom_segment(
    aes(x = "Jan", xend = "Dec", y = 60, yend = 60),
    size = 0.4,
    linetype = "dashed",
    colour = "grey30"
  ) +
  geom_text(
    data = ~ filter(.x, month == "Dec"),
    aes(label = candidate),
    family = "Arial Narrow",
    size = 4,
    hjust = "left", # biar dikiri titik
    nudge_x = 0.075
  ) +
  geom_text(
    data = data.frame(x = "Dec", y = 60, label = "GOAL"),
    aes(x, y, label = label),
    inherit.aes = FALSE, # tidak ambil yang diawal aesnya
    family = "Arial Narrow",
    colour = "gray30",
    size = 4,
    hjust = "left", # biar dikiri titik
    nudge_x = 0.075
  ) +
  labs(
    title = "Time To Fill",
    x = "2017",
    y = "TIME TO FILL (DAYS)"
  ) +
  # atur jarak kanan 0 kiri 0
  scale_x_discrete(
    #expand = expansion(mult = c(0.005, 0.1))
    expand = c(0.005, 0, 0.1, 0)
  ) +
  scale_y_continuous(
    breaks = seq.int(0, 100, by = 10),
    labels = seq.int(0, 100, by = 10)
  ) +
  scale_colour_manual(
    values = colpal,
    guide = "none"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Arial Narrow", base_size = 13) +
  theme(
    #dari pojok sampe pojok, default panel
    plot.title.position = "plot",
    plot.title = element_text(size = rel(1.75)),
    axis.title.x = element_text(hjust = 0),
    axis.title.y = element_text(hjust = 1),
    
    # garis - di sumbu y
    axis.ticks.y = element_line(colour = "black"),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    panel.grid = element_blank()
  )






# Data Part 2 -------------------------------------------------------------
toplot2 <- 
  tibble(
    type = c(rep(c("actual", "forecast"), c(8, 5))),
    year = 2010:2022,
    sales = c(59, 63, 68, 73, 85, 92, 100, 105, 115, 126, 130, 151, 166)
  )

toplot2



library(scales)
library(ggh4x)


toplot2 %>% 
  mutate(
    label = ifelse(year == max(year),
                   dollar(sales, suffix = " Billion"),
                   dollar(sales)),
    x_axis = weave_factors(year, toupper(type))
  ) %>% 
  ggplot(aes(x_axis, sales, group = 1)) + #group 1 karena diskrit
  geom_line(
    linetype = "longdash",
    colour = "steelblue"
  ) +
  geom_line(
    data = ~ .x %>% filter(type == "actual"),
    linetype = "solid",
    colour = "steelblue",
    size = 1, # deafult 0.5
  ) +
  geom_point(
    data = ~ .x %>% filter(type == "forecast"),
    colour = "steelblue",
    size = 3
  ) +
  geom_text(
    data = ~ .x %>% filter(type == "forecast"),
    aes(label = label),
    colour = "steelblue",
    family = "Arial Narrow",
    nudge_y = 8
  ) +
  geom_text(
    data = ~ .x %>% 
      filter(type == "actual") %>% 
      filter(year == max(year)),
    aes(label = label),
    colour = "steelblue",
    family = "Arial Narrow",
    nudge_y = 8
  ) +
  geom_point(
    data = ~ .x %>% 
      filter(type == "actual") %>% 
      filter(year == max(year)),
    colour = "steelblue",
    size = 5
  ) +
  scale_x_discrete(
    guide = "axis_nested",
  ) +
  scale_y_continuous(
    breaks = seq.int(0, 200, by = 20),
    labels = dollar_format(),
    # bawah 0 , atas 10%
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = NULL,
    y = "Sales (Billion)",
    title = "Sales Over Time"
  ) +
  expand_limits(y = 0) +
  # biar gak ada yang kepotong
  coord_cartesian(clip = "off") +
  theme_classic(
    base_family = "Arial Narrow",
    base_size = 13
  ) +
  theme(
    plot.title.position = "plot",
    text = element_text(colour = "gray30"),
    plot.title = element_text(family = "Arial", face = "bold",
                              colour = "black", size = rel(1.75)),
    axis.title.y = element_text(hjust = 1),
    axis.line = element_line(colour = "gray70"),
    axis.ticks = element_line(colour = 'gray70'),
    plot.margin = margin(15, 15, 15, 15),
    # lebih besar 1.25% dari base size
    ggh4x.axis.nesttext.x = element_text(size = rel(1.25), face = "bold"),
    ggh4x.axis.nestline.x = element_line(size = 0.5)
  )

