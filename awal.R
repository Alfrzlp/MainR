library(crayon)
lime <- make_style(rgb(94, 201, 98, maxColorValue = 255), bg = F)
ivory <- make_style("ivory")
options(prompt = lime("~> "), show.signif.stars = F)