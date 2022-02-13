library(reticulate)
library(tesseract)

read.img <- function(path = "clipboard",
                     bahasa = "eng", to_df = FALSE) {
  tryCatch(
    if (match.arg(path, "clipboard") == "clipboard") {
      reticulate::py_run_string("from PIL import ImageGrab")
      reticulate::py_run_string("im = ImageGrab.grabclipboard()")
      reticulate::py_run_string("im.save('D:/text.png', 'PNG')")
    },
    error = function(e) cat("\nClipboard tidak ditemukan. Membaca gambar sebelumnya...\n\n")
  )

  path <- "D:/text.png"
  bahasa <- match.arg(bahasa, c("numbers", "ind", "eng"))
  if (bahasa == "numbers") {
    my_engine <- tesseract(options = list(tessedit_char_whitelist = " .0123456789"))
  } else {
    my_engine <- tesseract(language = bahasa)
  }

  text <- ocr(path, engine = my_engine)

  if (to_df) {
    df <- read.table(textConnection(text), header = F)
    copy2c(df)
    return(df)
  } else {
    cat(text)
    writeClipboard(text)
  }
}



# ====================================================
# tesseract_download("ind")

ind <- tesseract("ind")
eng <- tesseract(language = "eng")
text <- ocr("D:/text.jpg", engine = eng)
cat(text)

tesseract::ocr_data("D:/text.jpg", engine = eng)

tesseract_info()

img_totext <- function(path, my_engine = tesseract(language = "eng")) {
  text <- ocr(path, engine = my_engine)
  cat(text)
}
# If your image is skewed, use image_deskew() and image_rotate() make the text horizontal.
# image_trim() crops out whitespace in the margins. Increase the fuzz parameter to make it work for noisy whitespace.
# Use image_convert() to turn the image into greyscale, which can reduce artifacts and enhance actual text.
# If your image is very large or small resizing with image_resize() can help tesseract determine text size.
# Use image_modulate() or image_contrast() or image_contrast() to tweak brightness / contrast if this is an issue.
# Try image_reducenoise() for automated noise removal. Your mileage may vary.
# With image_quantize() you can reduce the number of colors in the image. This can sometimes help with increasing contrast and reducing artifacts.
# True imaging ninjas can use image_convolve() to use custom convolution methods.

# ctrl + shift + c

library(magick)
input <- image_read("D:/text.jpg")

text <- input %>%
  image_resize("2000x") %>%
  image_convert(type = "Grayscale") %>%
  image_trim(fuzz = 40) %>%
  image_write(format = "png", density = "300x300") %>%
  tesseract::ocr()

cat(text)












pngfile <- pdftools::pdf_convert("https://jeroen.github.io/images/ocrscan.pdf", dpi = 600)

text <- tesseract::ocr(pngfile)
cat(text)




numbers <- tesseract(options = list(tessedit_char_whitelist = "$.0123456789"))
cat(ocr("https://jeroen.github.io/images/receipt.png", engine = numbers))


# Do not allow any dollar sign
numbers2 <- tesseract(options = list(tessedit_char_whitelist = ".0123456789"))
cat(ocr("https://jeroen.github.io/images/receipt.png", engine = numbers2))







library(magrittr)
library(magick)
# > Linking to ImageMagick 6.9.9.38
# > Enabled features: cairo, fontconfig, freetype, fftw, ghostscript, lcms, pango, rsvg, webp, x11
# > Disabled features:

# download file
url <- "https://pbs.twimg.com/media/Dv3pIsIUwAEdu--.jpg:large"
download.file(url, destfile = "D:/table.jpg")

# preprocessing
img <- image_read("D:/table.jpg") %>%
  image_transparent("white", fuzz = 82) %>%
  image_background("white") %>%
  image_negate() %>%
  image_morphology(method = "Thinning", kernel = "Rectangle:20x1+0+0^<") %>%
  image_negate() %>%
  image_crop(geometry_area(0, 0, 80, 25))

img

data <- img %>%
  image_ocr()

# some wrangling
data %>%
  stringi::stri_split(fixed = "\n") %>%
  purrr::map(~ stringi::stri_split(str = ., fixed = "'")) %>%
  .[[1]] %>%
  purrr::map_df(~ tibble::tibble(Date = .[1], Price = .[2], Change = .[3])) %>%
  dplyr::glimpse()
