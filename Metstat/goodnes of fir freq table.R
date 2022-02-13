df <- c(
  2, 3, 0.3, 3.3, 1.3, 0.4,
  0.2, 6, 5.5, 6.5, 0.2, 2.3,
  1.5, 4, 5.9, 1.8, 4.7, 0.7,
  4.5, 0.3, 1.5, 0.5, 2.5, 5,
  1, 6, 5.6, 6, 1.2, 0.2
)
mean(data)
sd(data)
chisq.test(data)

str <- "2,5 0,7 3,4 1,8 1,9 2 1,3 1,2 2,2 0,9 2,7 2,9 1,5 1,5 2,2
3,2 0,7 2,3 3,1 1,3 4,2 3,4 1,5 2,1 1 2,4 1,8 0,9 1,3 2,6
3,6 0,8 3 2,8 3,6 3,1 2,4 3,2 4,4 4,1 1,5 1,9 3,2 1,9 1,6
3 3,7 1,7 3,1 2,4 3 1,5 3,1 2,4 2,1 2,1 2,3 0,7 0,9 2,7
1,2 2,2 1,3 3 3 2,2 1,5 2,7 0,9 2,5 3,2 3,7 1,9 2 3,7
2,3 0,6 0 1 1,4 0,9 2,6 2,1 3,4 0,5 4,1 2,2 3,4 3,3 0
2,2 4,2 1,1 2,3 3,1 1,7 2,8 2,5 1,8 1,7 0,6 3,6 1,4 2,2 2,2
1,3 1,7 3 0,8 1,6 1,8 1,4 3 1,9 2,7 0,8 3,3 2,5 1,5 2,2
2,6 3,2 1 3,2 1,6 3,4 1,7 2,3 2,6 1,4 3,3 1,3 2,4 2
1,3 1,8 3,3 2,2 1,4 3,2 4,3 0 2 1,8 0 1,7 2,6 3,1"

df <- str.to.df(str, dim = c(148, 1))

k <- 1 + 3.322 * log(length(df))
k <- round(k)
k
i <- (max(df) - min(df)) / k
i
# ======================================================================
freq.table <- function(data, k, i = NA, pisah.int = F) {
  if (!is.na(i)) {
    breaks <- seq(min(data), max(data), by = i)[-1]
  } else {
    breaks <- seq(min(data), max(data), length = k)[-k]
  }

  breaks <- c(-Inf, breaks[1:(length(breaks))], Inf)
  interval <- cut(data, breaks = breaks)
  df <- as.data.frame(table(interval))

  perbaiki <- function(x) {
    x <- gsub("\\(|\\]", "", x)
    x <- as.numeric(x)
  }

  if (pisah.int) {
    df %>%
      tidyr::separate(interval, c("bawah", "atas"), sep = ",") %>%
      mutate_at(c("bawah", "atas"), perbaiki) %>%
      return()
  } else {
    return(df)
  }
}
# ======================================================================
df <- freq.table(data, i = 1.5, pisah.int = T)
df



freq.table(df, i = 1.05, pisah.int = T) %>%
  gof.test(mean(df), sd(df)) %>%
  summarise_all(sum)

freq.table(df, k = 12)


gof.test <- function(df, mean, sd) {
  df %>%
    mutate(
      p = pnorm((atas + 0.5 - mean) / sd) - pnorm((bawah - 0.5 - mean) / sd),
      p = round(p, 5),
      ei = sum(Freq) * p
    ) %>%
    as.data.frame() %>%
    return()
}

# i = 0.5
freq <- freq.table(df, i = 0.525, pisah.int = T) %>%
  gof.test(1.8, 0.4)

Freq <-
  rbind(
    colSums(freq[1:2, ]),
    freq[3:4, ],
    colSums(freq[5:13, ])
  )

Freq %>%
  mutate(x = (Freq - ei)^2 / ei) %>%
  summarise_all(sum)


freq.table(df, i = 1, pisah.int = T) %>%
  gof.test(mean(df), sd(df))
summarise_all(sum)



# 0.8
mutate(x = (Freq - ei)^2 / ei)







freq.table(data, i = 2, pisah.int = T)
gof.test(1.8, 0.4)
