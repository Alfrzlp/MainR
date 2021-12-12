library(hereR)
set_key("MI1Puf0qa1RVDJDTWyNmiPZabg2IqRPPu5CEm6DRsHs")

incident(aoi, from = as.POSIXct("2018-01-01 00:00:00"))
connection(poi[1:2, ], poi[3:4, ])
intermodal_route(poi[1:2, ], poi[3:4, ])

route(poi[1:2, ], poi[3:4, ], transport_mode = "car", traffic = TRUE)

weather(poi, product = "observation")
weather(poi, product = "forecast_hourly")
weather(poi, product = "forecast_astronomy")
weather(poi, product = "alerts")
