library(tidyverse)
library(dm)


# data --------------------------------------------------------------------

s <- 'Pendidikan KRT (1: >= SMA, 0: Lainnya)	2,67	2,07	2,83	1,28	3,17	4,73	3,74	4,76	11,27	9,94
Usia KRT (1: antara 15 sampai 65, 0: lainnya)	84,04	88,97	84,11	88,51	86,45	88,18	87,44	90,61	89,87	91,23
Jenis Kelamin KRT (1: laki-laki, 0: perempuan)	88,36	88,28	87,75	93,62	89,37	89,53	88,96	90,61	86,75	87,16
Jumlah ART (dalam orang)	5,23	5,43	4,83	5,01	4,49	4,86	4,31	4,73	3,67	3,84
Status Pernikahan KRT (1: kawin, 0: lainnya)	86,95	87,59	86,64	91,06	87,53	88,18	86,9	89,61	82,83	85,39
Sektor Pekerjaan KRT (1: Non-Pertanian, 0: Lainnya)	60,93	67,59	66,93	73,19	68,56	74,66	74,24	83,1	82,63	86,07
Kepemilikan Tabungan (1: ada, 0: tidak)	29,64	48,97	30,54	46,38	34,15	48,65	39,96	56,32	59,26	68,77
Kepemilikan Kredit (1: ada, 0: tidak)	20,13	30,34	23,71	38,72	22,6	38,18	25,48	34,17	31,71	37,74
Akses Internet (1: akses, 0: tidak)	20,52	31,03	25,62	37,45	30,83	38,51	34,28	46,68	54,51	58,8
Status Wilayah (1: perkotaan, 0: perdesaan)	22,48	40,69	30,54	48,09	33,59	57,09	36,83	56,95	47,63	56,02
Kepulauan Tempat Tinggal (1: pulau jawa, 0: lainnya)	32,78	37,24	35,04	43,83	35,94	40,54	32,48	39,92	29,61	35,07
Kulkas (1: ada, 0: tidak)	26,1	42,76	39,1	50,21	45,04	58,11	52,22	66,83	73,37	80,29
AC (1: ada, 0: tidak)	0,08	0,69	0,06	0,85	0,46	0,34	0,89	0,75	9,91	7,19
PC/komputer (1: ada, 0: tidak)	1,89	6,21	4,37	10,64	6,13	7,43	8,33	12,27	24,8	25,19
Motor (1: ada, 0: tidak)	57,08	68,97	69,52	74,89	70,91	86,15	78,19	83,98	85,06	87,11
Mobil (1: ada, 0: tidak)	1,1	1,38	2,46	1,7	2,71	2,03	3,6	4,63	21,44	15,99
Status Bangunan (1: milik sendiri, 0: lainnya)	90,8	82,07	88,73	86,81	89,06	85,47	86,69	83,23	84,09	81,09
Luas Lantai Perkapita (1: Luas > 8 m^2, 0: Lainnya)	75,31	70,34	82,45	79,57	85,84	84,12	89,71	86,61	95,27	93,58
Jenis Atap (1: beton/genteng/seng/asbes, 0: lainnya)	92,53	97,93	95,75	98,3	96,63	97,3	97,12	99	98,5	98,77
Jenis Dinding (1: tembok, 0: lainnya)	43,55	60	54,86	68,94	58,79	67,91	61,75	72,09	72,97	76,25
Jenis Lantai (1: marmer/granit/keramik/parket/vinil/karpet/ubin/tegel/teraso/semen/batu bata, 0: lainnya)	56,29	74,48	69,21	80	70,65	78,72	75,39	83,6	83,3	85,13
Fasilitas BAB (1: milik sendiri, 0: lainnya)	66,75	74,48	73,28	77,02	76,89	78,04	79,57	85,11	88,63	89,57
Sumber Air Minum (1: air kemasan bermerk/air isi ulang/leding/sumur bor/pompa/sumur terlindungi/mata air terlindungi, 0: lainnya)	72,33	79,31	78,76	83,83	78,68	89,53	81,89	90,24	88,04	91,86
Sumber Penerangan (1: listrik, 0: lainnya)	94,73	100	97,35	100	97,65	99,66	98,76	99,87	99,51	99,83
Jenis Bahan Bakar Memasak (1: tidak memasak di rumah/listrik/gas elpiji/biogas/gas kota/briket, 0: lainnya)	45,28	58,62	59,98	73,19	64,62	77,7	72,72	82,85	82,28	87,65
'

sk_levels <- c('sangat miskin', 'miskin', 'hampir miskin', 'rentan miskin', 'tidak miskin')
kat <- c('Karakteristik Sosial', 'Karakteristik Demografi', 'Karakteristik Ekonomi', 'Karakteristik Wilayah', 'Karakteristik Tempat Tinggal', 'Karakteristik Asset')
n_kat <- c(1, 4, 4, 2, 5, 9)

dat <- dm::read_pattern(
  s, pos_non_angka = 1,
  pos_angka = 2:11
) %>% 
  extract(
    v1,
    into = c("nama", "status"),
    regex = "(\\X*)\\s\\((\\X*)\\)"
  ) %>% 
  mutate(
    across(v2:v11, ~str_replace_all(., ',', '\\.'))
  ) %>% 
  mutate(
    kat = str_remove_all(rep(kat, n_kat), 'Karakteristik '),
    .before = nama
  ) %>% 
  type_convert() %>% 
  setNames(
    c('kat', 'nama', 'status',
      'sangat_miskin_0', 'sangat_miskin_1',
      'miskin_0', 'miskin_1',
      'hampir_miskin_0', 'hampir_miskin_1',
      'rentan_miskin_0', 'rentan_miskin_1',
      'tidak_miskin_0', 'tidak_miskin_1')
  ) %>% 
  dplyr::filter(nama != 'Jumlah ART') 

head(dat)




# Full Dataset -----------------------------------------------------------
# Menyimpan setiap data ke dalam list untuk siap di visualisasikan

full_data_final <- dat %>% 
  split(1:nrow(.)) %>% 
  lapply(
    FUN = function(x){
      x %>% 
        mutate(group = 1) %>% 
        add_row(
          x %>%
            mutate_at(-c(1:3), ~ 100 - .x) %>% 
            mutate(group = 0)
        ) %>% 
        pivot_longer(
          contains('miskin'),
          values_to = 'persentase',
          names_to = 'sk'
        ) %>% 
        mutate(
          bpum = substr(sk, nchar(sk), nchar(sk)),
          sk = str_remove_all(sk, "_(0|1)$"),
          sk = str_replace_all(sk, "_", " "),
          persentase = ifelse(bpum == 0, -persentase, persentase)
        ) %>% 
        type_convert()
    }
  )


full_data_final[[1]]





# viz ---------------------------------------------------------------------
# visualisasi pakai seperti looping namun lebih cepat dari lopping sepertinya
# yaitu fungsi walk2()

my_legend_text <- 
  str_split(dat$status, ', ') %>% 
  lapply(function(x){
    rev((str_remove(x, '[0-1]:\\s')))
  })

my_legend_text[j]
my_legend_text[j][[3]][2] 
my_legend_text[j][[3]][2] <- "Luas > 8 m\u00b2"
my_legend_text[j][[3]][2]

my_names <- dat$nama
# index data yang persentasenya kecil, perlu perlakuan khusus
i <- which(my_names %in% c('Pendidikan KRT', 'AC', 'PC/komputer', 'Mobil'))
i




full_data_final[j] %>% 
  walk2(
    .x = .,
    .y = as.numeric(names(.)),
    ~ {
      # Message ----------------------------------
      message("Generating plot for ", my_names[.y])
      
      # generating plot --------------------------
      if(.y %in% i){
        p <- ggplot(
          data = .x,
          aes(x = persentase, y = factor(sk, levels = sk_levels),
              fill = factor(group))
        ) +
          geom_bar(
            stat = "identity", width = 0.9
          ) +
          geom_text(
            data = ~ .x %>% 
              mutate(
                xpos = ifelse(
                  abs(persentase) < 14, 
                  persentase + 7 * sign(persentase),
                  persentase - 7 * sign(persentase)
                )
              ) %>% 
              dplyr::filter(group == 1),
            aes(
              label = paste0(abs(persentase), '%'),
              x = xpos
            ),
            color = "white", 
            fontface = 2,
            size = 3
          )
      } else {
        p <- ggplot(
          data = .x,
          aes(x = persentase, y = factor(sk, levels = sk_levels),
              fill = factor(group))
        ) +
          geom_bar(
            stat = "identity", width = 0.9
          ) +
          geom_text(
            data = ~ .x %>% dplyr::filter(group == 1),
            aes(
              label = paste0(abs(persentase), '%'),
              x = persentase - 11 * sign(persentase)
            ),
            color = "white", 
            fontface = 2,
            size = 3
          )
      }
      
      
      
      p <- p +
        geom_vline(
          xintercept = -1,
          color = 'white'
        ) +
        annotate(
          geom = 'text',
          x = c(-50, 50),
          y = 6,
          size = 4,
          label = c('Bukan Penerima BPUM', 'Penerima BPUM')
        ) +
        scale_y_discrete(
          expand = expansion(mult = c(0, 0.1)),
          labels = function(x) str_to_title(x)
        ) +
        scale_x_continuous(
          breaks = NULL,
          expand = expansion(mult = c(0.005, 0))
        ) +
        scale_fill_manual(
          NULL, 
          labels = str_wrap(my_legend_text[[.y]], 20),
          values = c("#E4CFA1", "#49A59B") 
        ) + 
        labs(
          title = my_names[.y],
          x = "Persentase",
          y = "Status Kemiskinan"
        ) +
        guides(
          fill = guide_legend(reverse = T)
        ) +
        coord_cartesian(clip = "off") +
        theme_minimal(base_family = "Arial") +
        theme(
          plot.title.position = "plot", 
          plot.title = element_text(size = rel(1.5), face = "bold", margin = margin(0, 0, b = 15, 10)),
          axis.title.x = element_text(colour = "black", size = 12.5, margin = margin(10, 0, 0, 0)),
          axis.title.y = element_text(colour = "black", size = 12.5, margin = margin(0, 15, 0, 0)),
          axis.text.y = element_text(size = 10.5),
          axis.text.x = element_text(size = 10.5),
          panel.grid = element_blank()
        ) 
      
      # save plot -------------------------
      ggsave(
        plot = p,
        filename = str_glue("E:/Visualisasi/rev/perbaikan/sk_{str_replace_all(my_names[.y], '/', '_')}.png"),
        width = 10,
        height = 4,
        units = "in", 
        dpi = 500,
        scale = 0.85,
        bg = 'white'
      )
      
      # end -------------------------------
    }
  )


