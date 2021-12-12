library(ggplot2)
library(ggthemes)
library(data.table)
library(ggTimeSeries)

# creating some data
set.seed(1)
dtData = data.table(
  DateCol = seq(
    as.Date("1/01/2014", "%d/%m/%Y"),
    as.Date("31/12/2015", "%d/%m/%Y"),
    "days"
  ),
  ValueCol = runif(730)
)
dtData

dtData[, ValueCol := ValueCol + (strftime(DateCol,"%u") %in% c(6,7) * runif(1) * 0.75), .I]
dtData[, ValueCol := ValueCol + (abs(as.numeric(strftime(DateCol,"%m")) - 6.5)) * runif(1) * 0.75, .I]

view(dtData)
# base plot
p1 = ggplot_calendar_heatmap(
  dtData,
  'DateCol',
  'ValueCol'
)
p1
# adding some formatting
p1 + 
  xlab('') + 
  ylab('') + 
  scale_fill_continuous(low = 'green', high = 'red') + 
  facet_wrap(~Year, ncol = 1)


# creating some categorical data
dtData[, CategCol := letters[1 + round(ValueCol * 7)]]
dtData

# base plot
p2 = ggplot_calendar_heatmap(
  dtData,
  'DateCol',
  'CategCol'
)
p2
# adding some formatting
p2 + 
  xlab('') + 
  ylab('') + 
  facet_wrap(~Year, ncol = 1)


#===============================================================================
set.seed(1)
dfData = data.frame(x = 1:1000, y = cumsum(rnorm(1000)))

# base plot
p1 = ggplot_horizon(dfData, 'x', 'y')
p1
#If you're seeing any vertical white stripes, it's a display thing.
p1 + 
  xlab('') + 
  ylab('') + 
  scale_fill_continuous(low = 'green', high = 'red') + 
  coord_fixed( 0.5 * diff(range(dfData$x)) / diff(range(dfData$y)))

#===============================================================================
# creating some data
set.seed(1)
dfData = data.frame(x = 1:100, y = cumsum(rnorm(100)))

# base plot
p1 = ggplot_waterfall(
  dtData = dfData,
  'x',
  'y'
)

# adding some formatting
p1 + 
  xlab('') + 
  ylab('')