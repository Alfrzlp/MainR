library(ggplot2)
library(ggExtra)

# 2 grafik
mpg
g = ggplot(mpg, aes(cty, hwy))+
  geom_count() +
  geom_smooth(method = "lm", se = F)

ggMarginal(g, type = "histogram", fill = "transparent")
ggMarginal(g, type = "boxplot", fill = "transparent")


library(treemapify)
download.file("https://raw.githubusercontent.com/selva86/datasets/master/proglanguages.csv",
              "D:/Datasets/proglangs.csv")

proglangs <- read.csv("D:/Datasets/proglangs.csv")
proglangs

# plot
ggplot(proglangs, aes(area = value, fill = id, label = paste(id, sep="\n"))) +
  geom_treemap()+
  geom_treemap_text(colour = "white", place = "centre", size = 11) +
  theme(legend.position = "none")
  scale_color_brewer(palette = "Greens")

pie <- hasil %>% 
  group_by(kat_stress) %>% 
  count() %>% 
  mutate(
    col = if_else(kat_stress == 'Sangat Parah', 'black', 'white')
  )

pie %>% 
  ggplot(aes(area = n, fill = kat_stress)) +
  geom_treemap()+
  geom_treemap_text(colour = pie$col, size = 15,
                    aes(label = kat_stress)) +
  geom_treemap_text(colour = pie$col, size = 12, place = 'left',
                    aes(label = scales::percent(n/sum(n))
                        )) +
  theme(legend.position = "none") 


slibrary(plotly)
pie <- hasil %>% 
  group_by(kat_stress) %>% 
  count()

plot_ly(
  pie,
  type='treemap',
  values = ~n,
  labels = ~ paste(kat_stress, '\n', scales::percent(n/sum(n))),
  insidetextfont = list(size = 19),
  marker = list(colors = viridis::viridis(5)),
  parents = NA
)
