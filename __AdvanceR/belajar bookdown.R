bookdown::publish_book(
  name = 'bookdown-example',
  account = 'alfrzlp',
  server = NULL,
  render = "local"
)

# b {
#   color: #e74c3c;
# }
# إذا <b>م&zwj;</b>ا ط<b>م&zwj;</b>حت إلى <b>غ&zwj;</b>اية
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
