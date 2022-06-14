ggplot(NULL, aes(x = 1:4, y = 1, fill = factor(1:4))) +
  geom_tile() +
  geom_text(
    aes(label = c("#E4CFA1", "#CDA351", "#9BCFC9", "#49A59B"))
  ) +
  scale_fill_manual(
    values = c("#E4CFA1", "#CDA351", "#9BCFC9", "#49A59B")
  ) +
  theme_void() +
  theme(
    legend.position = 'none'
  )


n_col <- 3
n_row <- 2
my_col <- c(my_col, rep('transparent', n_col*n_row - length(my_col)))
n <- length(my_col)


data.frame(
    x = rep(1:n_col, length = n),
    y = rep(n_row:1, length = n), 
    gp = factor(1:n),
    name = my_col
  )


viz_colour <- function(my_col, n_row = length(my_col), n_col = 1){
  if (n_col * n_row < length(my_col)) {
    stop('Ukuran baris * kolom terlalu sedikit dari pada panjang vector warna')
  }
  my_col <- c(my_col, rep('transparent', n_col*n_row - length(my_col)))
  n <- length(my_col)
  
  dat <- data.frame(
    x = rep(1:n_col, length = n),
    y = rep(n_row:1, each = n_col), 
    gp = factor(1:n),
    name = my_col
  )

  ggplot(
    dat, aes(x = x, y = y, fill = gp)
  ) +
  geom_tile() +
  geom_text(
    data = ~ dplyr::filter(.x, name != 'transparent'),
    aes(label = name)
  ) +
  scale_fill_manual(
    values = my_col
  ) +
  theme_void() +
  theme(
    legend.position = 'none'
  )
  
}



viz_colour(c("#E4CFA1", "#CDA351", "#9BCFC9", "#49A59B"))
viz_colour(c('#006D2C', '#20AC4B', '#7AC27F', '#A4D29F','#CFE6CA'),
           n_row = 5, n_col = 2)
viz_colour(my_col)

# versi ke2
my_col <- c('#DC6A4D', '#fba683', '#FFFFBF', "#7ccdc1", "#2f9790")
my_col <- c('#DC6A4D', '#fba683', 'khaki', "#7ccdc1", "#2f9790")
my_col <- c('#DC6A4D', '#fba683', 'burlywood1', "#7ccdc1", "#2f9790")

# "542f07","8b510a","e1c27b","f6e8c1"
my_col <- c('#8b510a', '#e1c27b', '#F6E8C1', "#7ccdc1", "#2f9790")
my_col <- c('tan3', '#e1c27b', '#F6E8C1', "#7ccdc1", "#2f9790")













guide_squarekey <- function(...) {
  # Constructor just prepends a different class
  x <- guide_legend(...)
  class(x) <- c("squarekey", class(x))
  x
}

guide_gengrob.squarekey <- function(guide, theme) {
  legend <- NextMethod()
  is_key <- startsWith(legend$layout$name, "key-")
  is_key_bg <- is_key & endsWith(legend$layout$name, "-bg")
  is_key <- is_key & !endsWith(legend$layout$name, "-bg")
  
  key_col <- unique(legend$layout$l[is_key])
  keywidth <- grid::convertUnit(legend$widths[2], "mm", valueOnly = TRUE)
  
  legend$grobs[is_key] <- lapply(legend$grobs[is_key], function(key) {
    key$height <- unit(keywidth - 0.5, "mm")
    key
  })
  legend$grobs[is_key_bg] <- lapply(legend$grobs[is_key_bg], function(bg) {
    bg$height <- unit(keywidth, "mm")
    bg
  })
  legend
}
