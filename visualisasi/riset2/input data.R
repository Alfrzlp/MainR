library(sf)

lama <- st_read('E:/xampp/htdocs/Web/y2/geojson-pwkbb/pwk_bb_y2.geojson')
head(lama)

gab <- st_read('E:/CitraRiset2/gab_rev2.geojson') 
head(gab)


gab <- gab %>% 
  mutate(nmkec = str_to_title(nmkec)) %>% 
  select(nmkab, nmkec, Y = y1_tahunan, 
         Y_nb = Y1_thn_nb, cluster = cluster4) %>% 
  mutate(
    pvalue = lisaY1_thn$p_vals,
    .before = geometry
  ) 

gab %>% 
  write_sf('E:/xampp/htdocs/Web/y2/geojson-pwkbb/pwk_bb_gab.geojson')


unique(gab$Y_nb)



# -------------------------------------------------------------------------

dat %>% 
  dplyr::filter(
    cluster1 == 'Low-High'
  ) %>% 
  mutate(
    lab = str_glue('[x: {x1}, y: {y1}, kec: {nmkec}]'),
    lab = str_replace_all(lab, '[', '{'),
    lab = str_replace_all(lab, ']', '}')
  ) %>% 
  pull(lab) %>% 
  paste(collapse = ', ')





dat <- readxl::read_xlsx('E:/CitraRiset2/dat_moran.xlsx')
dat
dat <- dat %>% 
  mutate(
    nmkab = str_to_title(gab$nmkab)
  )



dat %>% 
  # ganti ini
  group_by(cluster4) %>% 
  split(f = (.)$cluster4) %>% 
  lapply(
    function(x) {
      x %>% 
        mutate(
          # ganti ini
          lab = str_glue("[x: {x4}, y: {y4}, kab: '{nmkab}', kec: '{nmkec}']"),
          lab = str_replace_all(lab, '\\[', '\\{'),
          lab = str_replace_all(lab, '\\]', '\\}')
        ) %>% 
        pull(lab) %>% 
        paste(collapse = ', ')
    }
  ) %>% 
  unlist() -> hasil


hasil

hasil_final <- sapply(1:length(hasil), FUN = function(x) c(names(hasil)[x], hasil[x], ''), USE.NAMES = F)
hasil_final


# add text ----------------------------------------------------------------
now <- rstudioapi::getActiveDocumentContext()
now
now$id


add_text <- function(){
  rstudioapi::setCursorPosition(Inf)
  rstudioapi::insertText(
    text = hasil_final %>% 
      str_replace_all('\\}, \\{', '\\},\n\\{'), 
    id = now$id
  )
}


# -------------------------------------------------------------------------
add_text()



High-High
{x: 0.107119137863889, y: 1.32356424297942, kab: 'Bandung Barat', kec: 'Cihampelas'},
{x: 1.32356424297942, y: 1.14061476090054, kab: 'Bandung Barat', kec: 'Cililin'},
{x: 0.040987003181021, y: 0.725189103177352, kab: 'Bandung Barat', kec: 'Cipatat'},
{x: 0.50149836440333, y: 0.276874374093636, kab: 'Bandung Barat', kec: 'Cipeundeuy'},
{x: 1.55478535621368, y: 1.34625190962245, kab: 'Bandung Barat', kec: 'Cipongkor'},
{x: 1.80290154035233, y: 1.51407628060598, kab: 'Bandung Barat', kec: 'Gununghalu'},
{x: 0.206558698043969, y: 0.204145116486199, kab: 'Bandung Barat', kec: 'Lembang'},
{x: 0.864983747003328, y: 0.124496925079826, kab: 'Bandung Barat', kec: 'Ngamprah'},
{x: 1.22750369698022, y: 1.67884344828301, kab: 'Bandung Barat', kec: 'Rongga'},
{x: 1.92261518561766, y: 0.53126587028248, kab: 'Bandung Barat', kec: 'Saguling'},
{x: 1.75993978862404, y: 1.56041704651514, kab: 'Bandung Barat', kec: 'Sindangkerta'}

High-Low
{x: 0.425711903489386, y: -0.00197474854726539, kab: 'Bandung Barat', kec: 'Batujajar'},
{x: 0.250968598706917, y: -0.154030386686707, kab: 'Bandung Barat', kec: 'Cisarua'},
{x: 0.204145116486199, y: -0.554805604354235, kab: 'Bandung Barat', kec: 'Parongpong'},
{x: 1.14061476090054, y: -0.409870031810211, kab: 'Purwakarta', kec: 'Maniis'},
{x: 0.0134721734224557, y: -0.67256424855778, kab: 'Purwakarta', kec: 'Purwakarta'}

Low-High
{x: -0.00197474854726539, y: 0.525714632699611, kab: 'Bandung Barat', kec: 'Padalarang'},
{x: -0.541651584864394, y: 0.204949643672123, kab: 'Purwakarta', kec: 'Sukasari'}

Low-Low
{x: -0.35097864180065, y: -0.213102795313101, kab: 'Bandung Barat', kec: 'Cikalong Wetan'},
{x: -0.357736670162403, y: -0.392331339157091, kab: 'Purwakarta', kec: 'Jatiluhur'},
{x: -0.437384861568777, y: -0.499172549447661, kab: 'Purwakarta', kec: 'Babakancikao'},
{x: -1.48632740657515, y: -0.50963140286466, kab: 'Purwakarta', kec: 'Bojong'},
{x: -1.15325315160304, y: -0.485495587286971, kab: 'Purwakarta', kec: 'Bungursari'},
{x: -1.03257407371459, y: -0.62504082768531, kab: 'Purwakarta', kec: 'Campaka'},
{x: -0.978509846820567, y: -0.967697001441763, kab: 'Purwakarta', kec: 'Cibatu'},
{x: -1.13297906651778, y: -0.772293438524791, kab: 'Purwakarta', kec: 'Darangdan'},
{x: -1.39509402369148, y: -1.08446607720662, kab: 'Purwakarta', kec: 'Kiarapedes'},
{x: -0.381872485740092, y: -0.638774106749015, kab: 'Purwakarta', kec: 'Pasawahan'},
{x: -1.01760986805642, y: -0.278764281592204, kab: 'Purwakarta', kec: 'Plered'},
{x: -0.83852211646997, y: -0.81369670616149, kab: 'Purwakarta', kec: 'Pondok Salam'},
{x: -0.168029159721767, y: -0.518619120855971, kab: 'Purwakarta', kec: 'Sukatani'},
{x: -0.882449300821364, y: -0.0150080889592176, kab: 'Purwakarta', kec: 'Tegal Waru'},
{x: -1.19042230759268, y: -0.898861655414193, kab: 'Purwakarta', kec: 'Wanayasa'}
