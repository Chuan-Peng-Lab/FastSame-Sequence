########## import packages
library(here)
library(tidyverse)
library(jsonlite)

here::here()
path2 <- here::here("1_Data", "1_Raw_data", "2_exp2_data")
filenames <- list.files(path2, pattern = "csv", full.names = TRUE)

data <- list()


for (i in seq_along(filenames)) {
  data[[i]] <- readr::read_csv(filenames[i])
  data[[i]]$subj_idx <- jsonlite::fromJSON(data[[i]]$response[5])$Q0

  data[[i]]$gender <- jsonlite::fromJSON(data[[i]]$response[6])

  data[[i]]$year <- jsonlite::fromJSON(data[[i]]$response[7])$Q0
  data[[i]]$education <- jsonlite::fromJSON(data[[i]]$response[8])$Q0
  data[[i]]$dist <- data[[i]]$view_dist_mm[9]
  data[[i]] <- data[[i]] %>%
    dplyr::select(
      subj_idx, gender, year, education, dist, trial_type, rt, response,
      key_press, condition, correct_response, correct, word, Image
    ) %>%
    dplyr::filter(trial_type == "psychophysics")

  data[[i]] <- data[[i]] %>%
    dplyr::filter(trial_type == "psychophysics") %>%
    dplyr::mutate(
      shape_en = case_when(
        Image == "img/C_ambi40.png" ~ "circle",
        ### 这里不同实验需修改图形命名
        Image == "img/T_ambi40.png" ~ "triangle",
        Image == "img/S_ambi40.png" ~ "square"
      )
    ) %>%
    dplyr::mutate(
      valence = case_when(
        word == "好人" ~ "Good",
        ### 这里不同实验需修改label的命名
        word == "坏人" ~ "Bad",
        word == "常人" ~ "Neutral"
      )
    ) %>%
    dplyr::mutate(
      ACC = case_when(
        correct == "FALSE" ~ 0,
        correct == "TRUE" ~ 1
      )
    ) %>%
    dplyr::filter(!grepl("prac_", condition)) %>%
    dplyr::mutate(exp = "exp2")
  data[[i]]$subj_idx <- as.numeric(data[[i]]$subj_idx)
  data[[i]] <- data[[i]] %>%
    dplyr::mutate(
      matchness =
        case_when(
          subj_idx %% 2 == 1 & correct_response == "j" ~ "match",
          subj_idx %% 2 == 1 & correct_response == "f" ~ "mismatch",
          subj_idx %% 2 == 0 & correct_response == "f" ~ "match",
          subj_idx %% 2 == 0 & correct_response == "j" ~ "mismatch"
        )
    )
  data[[i]] <- data[[i]] %>% 
    dplyr::group_by(subj_idx, condition) %>% 
    dplyr::mutate(trial_id = dplyr::row_number()) %>% 
    dplyr::ungroup()
}



df_exp2 <- do.call(rbind, data)

outpath2 <- here::here("1_Data", "2_Postpro_data")
# readr::write_csv(df_exp2, paste(outpath2, "Exp2_postpro.csv", sep = "/"))
