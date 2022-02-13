bookdown::publish_book(
  name = "bookdown-example",
  account = "alfrzlp",
  server = NULL,
  render = "local"
)

# b {
#   color: #e74c3c;
# }
# <U+0625><U+0630><U+0627> <b><U+0645>&zwj;</b><U+0627> <U+0637><b><U+0645>&zwj;</b><U+062D><U+062A> <U+0625><U+0644><U+0649> <b><U+063A>&zwj;</b><U+0627><U+064A><U+0629>
#
# <b> &zwj;</b>
# file.create('.nojekyll')



# Set night deafult theme -------------------------------------------------
# tambah di _output.yml dibawah config
# fontsettings:
#   theme: night
#
# tambah di _bookdown.yml
# output_dir: "docs"

x <- c(3.36, 3.29, 3.3, 3.82, 3.79, rep(3.8, 3))
mean(x)

x <- 5
as.numeric(x / (1:x))
is.integer(x / (1:x))
is.integer()

sapply(as.list(x / (1:x)), as.character)

sum(!grepl("\\.", as.character(x / (1:x))))

is.atomic(1.5)
is.numeric(1)
is.numeric(1.4)

typeof(1.4)
typeof(1)
