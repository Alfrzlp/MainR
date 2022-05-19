library(dm)

# bandung
# -------------------------------------------------------------------------
s <- 'Ya	2	14	1	10
Tidak	13	1	14	5'

s2 <- 'Ya 1.00	6.00	3.00	1.00
Tidak 5.00	0.00	3.00	5.00'

s3 <- 'Ya 1	18	9	1
Tidak 17	0	9	17'


# -------------------------------------------------------------------------
s <- 'Ya	0	8	2	0
Tidak	8	0	6	8'

s1 <- 'Ya	0	3	1	0
Tidak	3	0	2	3'

s3 <- 'Ya	0	5	1	0
Tidak	5	0	4	5'


# -------------------------------------------------------------------------
my_col <- c('#20ac4b', '#0a5445')
name_lvl <- c('Tanaman Pangan',	'Tanaman Hortikultura',	'Budidaya Perikanan',	'Peternakan')


dat <- read_string(s) %>% 
  setNames(
    c('status', 'Tanaman Pangan',	'Tanaman Hortikultura',	'Budidaya Perikanan', 'Peternakan')
  ) %>% 
  pivot_longer(-status) %>% 
  type_convert() %>% 
  group_by(name) %>% 
  mutate(
    p = value/sum(value)
  ) %>% 
  ungroup()



dat2 <- read_string(s2) %>% 
  setNames(
    c('status', 'Tanaman Pangan',	'Tanaman Hortikultura', 'Budidaya Perikanan', 'Peternakan')
  ) %>% 
  pivot_longer(-status) %>% 
  type_convert() %>% 
  group_by(name) %>% 
  mutate(
    p = value/sum(value)
  ) %>% 
  ungroup()
dat2

dat3 <- read_string(s3) %>% 
  setNames(
    c('status', 'Tanaman Pangan',	'Tanaman Hortikultura', 'Budidaya Perikanan', 'Peternakan')
  ) %>% 
  pivot_longer(-status) %>% 
  type_convert() %>% 
  group_by(name) %>% 
  mutate(
    p = value/sum(value)
  ) %>% 
  ungroup()




x <- seq(0.01, 1, length = 100)
library(scales)
dat3 %>% 
  mutate(
    label = ifelse(round(p, 2) %in% x, percent(p, accuracy = 1), percent(p, accuracy = 0.01))
  )


# -------------------------------------------------------------------------
dat3 %>% 
  mutate(
    label = ifelse(p %in% x, percent(p, accuracy = 1), percent(p, accuracy = 0.01))
  ) %>% 
  ggplot(aes(x = p, y = factor(name, rev(name_lvl)), fill = status)) +
  geom_bar(
    position = position_stack(), stat = 'identity',
    width = 0.85
  ) +
  geom_text(
    data = ~ .x %>%
      filter(p > 0.07),
    aes(label = label, color = status),
    position = position_stack(vjust = 0.55)
  ) +
  geom_text(
    data = ~ .x %>%
      filter(p < 0.07, p > 0),
    aes(label = label, color = status),
    position = position_stack(vjust = 0.73)
  ) +
  scale_fill_manual(
    values = my_col
  ) +
  scale_color_manual(
    values = c('black', 'white')
  ) +
  scale_y_discrete(
    labels = function(x) str_wrap(x, 10)
  ) +
  scale_x_continuous(
    labels = NULL
  ) +
  guides(
    color = 'none'
  ) +
  theme_minimal() +
  labs(
    fill = NULL,
    x = NULL,
    y = NULL
  )



ggsave(
  filename = 'E:/Visualisasi/riset3/tasik_b403.png',
  width = 8.5,
  height = 3.7,
  units = "in",
  dpi = 500,
  scale = 0.95,
  bg = 'white'
)
