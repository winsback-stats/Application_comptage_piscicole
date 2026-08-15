library(lubridate)

d2025 <- readRDS("data/2025.rds")

decaler_annee <- function(df,n){
  if("date" %in% names(df)){
    df$date <- df$date %m-% years(n)
  }
  if("datetime" %in% names(df)){
    df$datetime <- df$datetime %m-% years(n)
  }
  df
}

## 2024
d2024 <- d2025
d2024$video <- decaler_annee(d2024$video,1)
d2024$dataset_h <- decaler_annee(d2024$dataset_h,1)
d2024$dataset_j <- decaler_annee(d2024$dataset_j,1)
d2024$dataset_sem <- decaler_annee(d2024$dataset_sem,1)
d2024$pannes <- tibble(
  debut=as.POSIXct(character()),
  fin=as.POSIXct(character())
)
d2024$hydro <- decaler_annee(d2024$hydro,1)
d2024$video_h <- decaler_annee(d2024$video_h,1)
d2024$video <- d2024$video %>%
  dplyr::sample_frac(0.8)
d2024$dataset_h$nb_mont <- round(d2024$dataset_h$nb_mont * 0.8)
d2024$dataset_j$nb_mont <- round(d2024$dataset_j$nb_mont * 0.8)
d2024$dataset_sem$nb_mont <- round(d2024$dataset_sem$nb_mont * 0.8)

d2024$dataset_h$nb_mont[is.na(d2024$dataset_h$nb_mont)] <-0
d2024$dataset_h$nb_seq[is.na(d2024$dataset_h$nb_seq)] <-0
d2024$dataset_h$taux_mont[is.na(d2024$dataset_h$taux_mont)] <-0

saveRDS(d2024,"data/2024.rds")


## 2023
d2023 <- d2025
d2023$video <- decaler_annee(d2023$video,2)
d2023$dataset_h <- decaler_annee(d2023$dataset_h,2)
d2023$dataset_j <- decaler_annee(d2023$dataset_j,2)
d2023$dataset_sem <- decaler_annee(d2023$dataset_sem,2)
d2023$pannes <- tibble(
  debut=as.POSIXct(character()),
  fin=as.POSIXct(character())
)
indices <- sample(
  seq_len(nrow(d2023$video)),
  size=round(0.2*nrow(d2023$video)),
  replace=TRUE
)

d2023$video <- rbind(d2023$video,
                     d2023$video[indices, ])

d2023$dataset_h$nb_mont <- round(d2023$dataset_h$nb_mont * 1.2)
d2023$dataset_j$nb_mont <- round(d2023$dataset_j$nb_mont * 1.2)
d2023$dataset_sem$nb_mont <- round(d2023$dataset_sem$nb_mont * 1.2)
d2023$hydro <- decaler_annee(d2024$hydro,2)
d2023$video_h <- decaler_annee(d2024$video_h,2)
d2023$dataset_h$nb_mont[is.na(d2023$dataset_h$nb_mont)] <-0
d2023$dataset_h$nb_seq[is.na(d2023$dataset_h$nb_seq)] <-0
d2023$dataset_h$taux_mont[is.na(d2023$dataset_h$taux_mont)] <-0

saveRDS(d2023,"data/2023.rds")


