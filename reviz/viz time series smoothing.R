
# Naive -------------------------------------------------------------------

autoplot(jb, lwd = 1.5, colour = "white") +
  autolayer(
    fitted(r_naive),
    series = "Naive",
    linetype = "longdash",
    lwd = 1
  ) +
  autolayer(
    fitted(r_snaive),
    series = "Seasonal Naive",
    linetype = "longdash",
    lwd = 1
  ) +
  labs(
    x = NULL, y = "Ribu (Ton)",
    title = "Jumlah Barang Melalui Transportasi Kereta Api",
    subtitle = "Pulau Jawa Tahun 2006-2021"
  ) +
  guides(
    colour = guide_legend(title = "Method")
  ) +
  expand_limits(y = 0) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  scale_x_continuous(
     breaks = c(5, 10, 15),
     labels = c("2010", "2015", "2021")
  ) +
  # biar gak ada yang kepotong
  coord_cartesian(clip = "off") +
  hrbrthemes::theme_ft_rc(
    base_family = "Arial Narrow",
    base_size = 13
  ) +
  theme(
    plot.title.position = "plot",
    text = element_text(colour = "gray100"),
    plot.title = element_text(family = "Arial", face = "bold",
                              colour = "white", size = rel(1.75)),
    plot.subtitle = element_text(family = "Arial",
                                 colour = "white", size = rel(1.25)),
    axis.title.y = element_text(hjust = 1, colour = "gray100"),
    axis.line = element_line(colour = "gray70"),
    axis.ticks = element_line(colour = 'gray70'),
    plot.margin = margin(15, 15, 15, 15),
    axis.text = element_text(colour = "gray100")
  ) 


# Average -----------------------------------------------------------------

colors <- c("Simple Moving Average 3" = "orange",
            "Double Moving Average 3" = "steelblue",
            "Weighted Moving Average 3" = "red")

r_ma[[1]] %>% 
  ggplot(aes(date, jb)) +
  geom_line(
    lwd = 1.1,
    colour = "white"
  ) +
  geom_line(
    aes(y = fit, color = "Simple Moving Average 3"),
    linetype = "solid",
    lwd = 1
  ) +
  geom_line(
    data = r_dma[[1]],
    aes(date, fit, color = "Double Moving Average 3"),
    linetype = "longdash",
    lwd = 1
  ) +
  geom_line(
    data = r_wma[[1]],
    aes(date, fit, color = "Weighted Moving Average 3"),
    linetype = "longdash",
    lwd = 1
  ) +
  scale_color_manual(values = colors) +
  labs(
    x = NULL,
    y = "Ribu (Ton)",
    color = "Method",
    title = "Jumlah Barang Melalui Transportasi Kereta Api",
    subtitle = "Pulau Jawa Tahun 2006-2021"
  ) +
  expand_limits(y = 0) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  # biar gak ada yang kepotong
  coord_cartesian(clip = "off") +
  hrbrthemes::theme_ft_rc(
    base_family = "Arial Narrow",
    base_size = 13
  ) +
  theme(
    plot.title.position = "plot",
    text = element_text(colour = "gray100"),
    plot.title = element_text(family = "Arial", face = "bold",
                              colour = "white", size = rel(1.75)),
    plot.subtitle = element_text(family = "Arial",
                                 colour = "white", size = rel(1.25)),
    axis.title.y = element_text(hjust = 1, colour = "gray100"),
    axis.line = element_line(colour = "gray70"),
    axis.ticks = element_line(colour = 'gray70'),
    plot.margin = margin(15, 15, 15, 15),
    axis.text = element_text(colour = "gray100")
  ) 



# exponential -------------------------------------------------------------

autoplot(jb, lwd = 1, colour = "white") +
  autolayer(
    fitted(r_se),
    series = "Simple Exponential",
    linetype = "longdash",
    lwd = 0.7
  ) +
  autolayer(
    fitted(r_winter),
    series = "winter",
    linetype = "solid",
    lwd = 1
  ) +
  autolayer(
    fitted(r_holt),
    series = "Holt",
    linetype = "longdash",
    lwd = 0.7
  ) +
  labs(
    x = NULL, y = "Ribu (Ton)",
    title = "Jumlah Barang Melalui Transportasi Kereta Api",
    subtitle = "Pulau Jawa Tahun 2006-2021"
  ) +
  guides(
    colour = guide_legend(title = "Method")
  ) +
  expand_limits(y = 0) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  scale_x_continuous(
    breaks = c(5, 10, 15),
    labels = c("2010", "2015", "2021")
  ) +
  # biar gak ada yang kepotong
  coord_cartesian(clip = "off") +
  hrbrthemes::theme_ft_rc(
    base_family = "Arial Narrow",
    base_size = 13
  ) +
  theme(
    plot.title.position = "plot",
    text = element_text(colour = "gray100"),
    plot.title = element_text(family = "Arial", face = "bold",
                              colour = "white", size = rel(1.75)),
    plot.subtitle = element_text(family = "Arial",
                                 colour = "white", size = rel(1.25)),
    axis.title.y = element_text(hjust = 1, colour = "gray100"),
    axis.line = element_line(colour = "gray70"),
    axis.ticks = element_line(colour = 'gray70'),
    plot.margin = margin(15, 15, 15, 15),
    axis.text = element_text(colour = "gray100")
  ) 






# All ---------------------------------------------------------------------
windowsFonts("Fira Code" = windowsFont("Fira Code"))

metode = c("Naive", "Seasonal Naive", "Moving Average 3",
           "Double Moving Average 3", "Weighted Moving Average", 
           "Single Exponential", "Holt's Method", "Winter's Method")

s <- "2.935483871	93.62365591	19437.04301	14.5565
39.69142857	126.8	30497.72571	17.9850
6.061594 83.82971 14940.18 13.34008
-0.01465201 111.4811 22990.35 18.14586
6.922826 87.05217 15913.17 14.00564
78.85225185	8.025331305	14143.95089	12.3788
83.34318	6.727287	14826.37	13.6733
77.71345	5.987974	13823.97	11.5200"

jb_text <- 
  read.table(textConnection(s), header = F) %>% 
  `colnames<-`(c("Bias", "MAD", "MSE", "MAPE")) %>% 
  mutate(
    Method = metode,
    .before = Bias
  ) %>% 
  mutate(RMSE = sqrt(MSE), .keep = "unused", .before = MAPE) %>% 
  mutate_at(2:5, ~number(.x, accuracy = 0.001)) %>% 
  mutate(
    label = str_glue(
    "
    **{Method}**<br><br>
    **Bias :** {Bias}<span style='color:gray60;'></span><br>
    **MAD. :** {MAD}<span style='color:gray60;'></span><br>
    **RMSE :** {RMSE}<span style='color:gray60;'></span><br>
    **MAPE :** {MAPE}<span style='color:gray60;'></span>
    "
    )
  ) %>% 
  filter(Method == "Winter's Method") %>% 
  pull(label)

df_jb %>% 
  filter(status == "forecast") %>% 
  drop_na() %>% 
  mutate(value = round(value, 2)) %>% 
  as.data.frame() %>% 
  pull(value)

label = str_glue(
  "
    **Agt 2021 :** 962.87<span style='color:gray60;'></span><br>
    **Sep 2021 :** 967.31<span style='color:gray60;'></span><br>
    **Okt 2021 :** 985.97<span style='color:gray60;'></span><br>
    **Nov 2021 :** 957.32<span style='color:gray60;'></span><br>
    **Des 2021 :** 993.27<span style='color:gray60;'></span>
    "
)

to_plot <- 
  jb_ka %>% 
  mutate(naive = as.vector(r_naive$fitted),
         snaive = as.vector(r_snaive$fitted),
         ma = r_ma[[1]]$fit,
         dma = r_dma[[1]]$fit,
         wma = r_wma[[1]]$fit,
         se = as.vector(r_se$fitted),
         holt = as.vector(r_holt$fitted),
         winter = as.vector(r_winter$fitted)) %>% 
  pivot_longer(-2) %>% 
  mutate(metode = rep(c("Data", metode), 187)) %>% 
  arrange(name, date)

to_plot

# viz
df_jb <- 
  jb_ka %>% 
  mutate(winter = c(as.vector(r_winter$fitted)),
         status = rep("actual", 187)) %>% 
  rbind(
    .,
    data.frame(
      jb = rep(NA, 5),
      date = seq.Date(as.Date("2021-08-01"), as.Date("2021-12-01"), by = "month"),
      winter = r_winter$mean,
      status = rep("forecast", 5)
    ) 
  ) %>% 
  pivot_longer(-c(2, 4))

df_jb %>% 
  ggplot(aes(date, value, colour = name, label = value)) +
  geom_line(
    lwd = 1
  ) +
  geom_text_repel(
    data = ~ .x %>%
      filter(status == "forecast") %>%
      drop_na() %>%
      mutate(value = round(value, 2)),
    box.padding = 0.5,
    color = "white",
    hjust = "right",
    vjust = "top",
    direction = 'y',
    nudge_x = 700,
    nudge_y = 200,
    segment.ncp = 3,
    segment.curvatur = -0.1,
    segment.angle = 90,
    size = 4
  ) +
  labs(
    x = NULL,
    y = "Ribu (Ton)",
    color = NULL,
    title = "Forecast from Holt-Winters' multipicative method",
    subtitle = "Agustus-Desember 2021",
    caption = expression(italic("Sumber : bps.go.id"))
  ) +
  expand_limits(y = 0) +
  coord_cartesian(clip = "off",
                  xlim = c(as.Date("2005-10-01"), as.Date("2022-01-01"))) +
  hrbrthemes::theme_ft_rc(
    base_family = "Fira Code",
    base_size = 13,
    grid = "XY",
    ticks = T
  ) +
  scale_x_date(
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_color_discrete(
    labels = c("Data", "Winter")
  ) +
  annotate(
    geom = "richtext",
    x = as.Date.character("2023-08-01"),
    y = 600,
    label = label,
    colour = "gray20",
    #fill = NA, 
    family = "Fira Code",
    lineheight = 1,
    size = 5,
    hjust = 0,
    vjust = 1,
    label.size = NA,
    size = 6
  ) +
  theme(
    plot.caption = element_text(size = 12),
    legend.justification = "top",
    plot.title.position = "plot",
    panel.spacing.x = unit(2, "lines"),
    text = element_text(colour = "gray100"),
    plot.title = element_text(family = "Fira Code", face = "bold",
                              colour = "palegreen", size = rel(1.75)),
    plot.subtitle = element_text(family = "Fira Code",
                                 colour = "white", size = rel(1.25)),
    axis.title.y = element_text(hjust = 1, colour = "gray100"),
    axis.line = element_line(colour = "gray50"),
    axis.ticks = element_line(colour = 'gray50'),
    plot.margin = margin(15, 50, 15, 5), # ats kanan bawah kiri
    axis.text = element_text(colour = "gray100")
  ) 


ggsave(
  "~/Main R/outfile/forecast winter.jpg",
  width = 12,
  height = 6,
  limitsize = F
)



# Make all plot -----------------------------------------------------------

to_plot %>% 
  split(.$metode) %>% 
  walk2(
      .x = .,
      .y = names(.),
      ~ {
        message("Generating plot for ", .y)
        # -------------------------------------------
        jb_text <- 
          read.table(textConnection(s), header = F) %>% 
          `colnames<-`(c("Bias", "MAD", "MSE", "MAPE")) %>% 
          mutate(
            Method = metode,
            .before = Bias
          ) %>% 
          mutate(RMSE = sqrt(MSE), .keep = "unused", .before = MAPE) %>% 
          mutate_at(2:5, ~number(.x, accuracy = 0.001)) %>% 
          mutate(
            label = str_glue(
              "
    **{Method}**<br><br>
    **Bias :** {Bias}<span style='color:gray60;'></span><br>
    **MAD. :** {MAD}<span style='color:gray60;'></span><br>
    **RMSE :** {RMSE}<span style='color:gray60;'></span><br>
    **MAPE :** {MAPE}<span style='color:gray60;'></span>
    "
            )
          ) %>% 
          filter(Method == .y) %>% 
          pull(label)
        
        # -------------------------------------------
        plot_hasil <- 
          to_plot %>% 
          ggplot(aes(date, value, colour = metode)) +
          #geom_line(lwd = 1.1, colour = "white") +
          geom_line(
            data = to_plot %>% 
              filter(metode %in% c("Data", .y)),
            lwd = 1
          ) +
          labs(
            x = NULL,
            y = "Ribu (Ton)",
            color = NULL,
            title = "Jumlah Barang Melalui Transportasi Kereta Api",
            subtitle = "Pulau Jawa Tahun 2006-2021",
            caption = expression(italic("Sumber : bps.go.id"))
          ) +
          expand_limits(y = 0) +
          coord_cartesian(clip = "off",
                          xlim = c(as.Date("2005-10-01"), as.Date("2022-01-01"))) +
          hrbrthemes::theme_ft_rc(
            base_family = "Fira Code",
            base_size = 13,
            grid = "XY",
            ticks = T
          ) +
          scale_x_date(
            expand = expansion(mult = c(0, 0.05))
          ) +
          annotate(
            geom = "richtext",
            x = as.Date.character("2023-08-01"),
            y = 700,
            label = jb_text,
            colour = "gray20",
            #fill = NA, 
            family = "Fira Code",
            lineheight = 1,
            size = 5,
            hjust = 0,
            vjust = 1,
            label.size = NA,
            size = 6
          ) +
          theme(
            plot.caption = element_text(size = 12),
            legend.justification = "top",
            plot.title.position = "plot",
            panel.spacing.x = unit(2, "lines"),
            text = element_text(colour = "gray100"),
            plot.title = element_text(family = "Fira Code", face = "bold",
                                      colour = "palegreen", size = rel(1.75)),
            plot.subtitle = element_text(family = "Fira Code",
                                         colour = "white", size = rel(1.25)),
            axis.title.y = element_text(hjust = 1, colour = "gray100"),
            axis.line = element_line(colour = "gray50"),
            axis.ticks = element_line(colour = 'gray50'),
            plot.margin = margin(15, 50, 15, 5), # ats kanan bawah kiri
            axis.text = element_text(colour = "gray100")
          ) 
        # ------------------------
        ggsave(
          str_glue("~/Main R/outfile/{.y}.png"),
          width = 10,
          height = 5,
          limitsize = F,
          dpi = 300,
          type = "cairo-png"
        )
        # -----------------------
      }
  )

library(magick)
all_loc <- list.files("~/Main R/outfile/Smoothing/", full.names = T)
all_image <- lapply(all_loc, image_read)
imgs <- image_join(all_image)
gif <- image_animate(imgs, 1, loop = 1)
# simpan
image_write_gif(gif, "~/Main R/outfile/smoothing.gif")

