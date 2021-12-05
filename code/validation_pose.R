library(data.table)
library(stringr)
library(dplyr)

val <- fread("../data/validation/validation_full_set.csv")
val$year_alt <- paste0(substring(val$path, 24, 27), "_", val$video)
val <- select(val, c(year_alt, hand_coded_movement_y))

load("../results/dominance/convhull_on_all_data.rdata")
df$year_alt <- str_remove(df$path, "../results/combined_fr_pd/face_recognition_")
df$year_alt <- str_remove(df$year_alt, "_landmarks.csv")
df <- select(df, c(year_alt, vertical))
df <- df[is.na(df$vertical) == F,]

val <- left_join(val, df, by = "year_alt")
val <- val[is.na(val$vertical) == F,]
val <- val[is.na(val$hand_coded_movement_y) == F,]

cor(val$hand_coded_movement_y, val$vertical)
