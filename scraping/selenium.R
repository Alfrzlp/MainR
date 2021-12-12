# devtools::install_github('ropensci/RSelenium')
# install.packages('wdman')

library(RSelenium)
library(wdman)

eCaps <- list(chromeOptions = list(
  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
))

cDrv <- chrome(version = "88.0.4324.27")

remDr<- remoteDriver(browserName = "chrome", port = 4444L)
remDr$open()
remDr$navigate("http://www.google.com")
remDr$navigate("C:/Users/Ridson Alfarizal/Documents/Main Python New/viz/html/2021-02-15 07-29-44.html")
remDr$screenshot(file = 'test.png')

# clean up
remDr$close()
cDrv$stop()




library(imager)
img <- load.image("test.png")
# xyzc
dim(img)

img_text <- implot(img,{text(150, 690,"08:00",col = "white", cex = 5, lwd = 1) })
img_text <- implot(img_text,{text(200, 740,"Senin, 15 Februari 2021",col = "gray", cex = 2.5) })
#mg_text <- implot(img_text,{text(200, 750,"15 Februari 2021",col = "white", cex = 2) })
plot(img_text)


library(magick)
image <- image_read("test.png")
image_text <- image_annotate(image, "08:00", color = "white", size = 50,
                             weight = 700, location = "+150+690")
image_text <- image_annotate(image_text, "15 Februari 2021", color = "gray", size = 25,
                             location = "+150+740", weight = 700)
print(image_text)

image_write(image_text, "test2.png")
