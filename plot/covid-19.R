coronavirus::update_dataset()
library(coronavirus)
library(tidyverse)
library(plotly)

coronavirus
max(coronavirus$date)


covid_indo <- coronavirus %>% 
  filter(country == "Indonesia") %>% 
  group_by(type) %>% 
  select(-province) %>% 
  tidyr::pivot_wider(names_from = type, values_from = cases)

attach(covid_indo)
covid_indo$total_cases <- cumsum(confirmed)
covid_indo$active_cases <- total_cases-cumsum(recovered)-cumsum(death)

View(covid_indo)


#========================================================================
#ggplot2Y
ggplot(covid_indo, aes(x=date))+
  geom_col(aes(x=date, y=confirmed), fill="skyblue")+
  geom_line(aes(y=death), col="darkred", size=1)+
  geom_line(aes(y=recovered), col="darkgreen", size=1)
 
ggplot(covid_indo, aes(x=date))+
  geom_line(aes(y=active_cases), col="red") +
  geom_line(aes(y=total_cases), col="black")


#========================================================================
#plotly
covid <- coronavirus %>% 
  filter(country == "Indonesia", date >= "2020-03-01") %>% 
  group_by(type) %>% 
  select(-province)

ggplot(covid, aes(x=date))+
  geom_line(aes(y=cases, col=type), size=1) +
  scale_color_manual(values=c("red", "black", "skyblue"))


plot_ly(
  data = covid_indo,
  x=~date, 
  y=~confirmed, 
  type = "bar",
  name = "Kasus Terkonfirmasi" ,
  marker = list(color = "skyblue")
) %>%  add_trace(
  x=~date, 
  y=~recovered, 
  type = "scatter", 
  mode = "markers+lines",
  name = "Kasus Sembuh",
  line = list(color = "forestgreen"),
  marker = list(color = "forestgreen")
) %>%  add_trace(
  x=~date, 
  y=~death, 
  type = "scatter", 
  mode = "markers+lines",
  name = "Kasus Meninggal",
  line = list(color = "black"),
  marker = list(color = "black")
) %>% layout(
  title = "Covid-19 in Indonesia",
  yaxis = list(title = "Kasus Positif"),
  xaxis = list(title = "Tanggal"),
  hovermode = "compare",
  legend = list(x = 0.1, y = 0.9)
)


#========================================================================
#heatmap
head(covid_indo)

covid_indo %>% 
  group_by(date) %>% 
  ggplot_calendar_heatmap(
    "date",
    "confirmed"
  ) +
  scale_fill_continuous("Terkonfirmasi",low = "green", high = "red3")+
  ggtitle("Covid-19 in Indonesia") + ylab("Hari")

