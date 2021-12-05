library(data.table)
library(stringr)

h2018 <- fread("../data/validation/wmp-2018-codingcreatives_3.5_v1.0_120820.csv")
h2020 <- fread("../data/validation/WMPClassic2020FinalCoded.csv")

# Handcoded data
# Get only the variables we need
h2018 <- subset(h2018, select = c(alt, f_picture, o_picture))
h2020 <- subset(h2020, select = c(alt, F_PICTURE, O_PICTURE))
names(h2020) <- c("alt", "f_picture", "o_picture")
# Combine 2018, 2020
h20182020 <- rbind(h2018, h2020)
# Kick out videos that don't have at least one of the two variables coded
h20182020 <- h20182020[is.na(h20182020$f_picture)==F | is.na(h20182020$o_picture)==F,]


# Results data
load("../results/dominance/convhull_on_all_data.rdata")

# Change video names so the two data sources can be merged
df$alt <- basename(df$path)
df$alt <- str_remove(df$alt, "_landmarks.csv")
df$alt <- str_remove(df$alt, "face_recognition_")
df$alt <- str_remove(df$alt, "2018_")
df$alt <- str_remove(df$alt, "2020_")
df <- df[!duplicated(df$alt),]

# Merge
df_overlap <- df[df$alt %in% c(h20182020$alt),]
df_overlap <- merge(df_overlap, h20182020, by = "alt", all.x = T)


# For all videos
# Check how many rows are non-na in the candidate poses
# Meaning that the candidate is detected for this frame
df_overlap$candidate_detected_frames <- NA
for(i in 1:nrow(df_overlap)){
  
  f <- df_overlap$path[i]
  if(file.exists(f)){
    ff <- fread(f)
    na_rows <- apply(ff, 1, function(x){all(is.na(x))})
    non_na_rows <- nrow(ff)-length(which(na_rows))
  }
  df_overlap$candidate_detected_frames[i] <- non_na_rows
  
  print(i)
}

#----
# Videos where the candidate is hand-coded to be pictured
df_overlap_pictured <- df_overlap[df_overlap$f_picture == 1,]
# Videos where our FR model never detects the candidate
candidate_never_detected <- length(which(df_overlap_pictured$candidate_detected_frames==0))
# Proportion
candidate_never_detected/nrow(df_overlap_pictured)

#----
# Videos where the candidate is hand-coded to NOT be pictured, but assuming they are still in the oral approval
df_overlap_oral_app <- df_overlap[df_overlap$f_picture == 0,]
# Kick out the GOV, since they dont have to have oral approval
df_overlap_oral_app <- df_overlap_oral_app[substr(df_overlap_oral_app$alt, 1, 3) != "GOV",]
# Videos where our FR model never detects the candidate
candidate_never_detected_oral_app <- length(which(df_overlap_oral_app$candidate_detected_frames==0))
# Proportion
candidate_never_detected_oral_app/nrow(df_overlap_oral_app)


save.image("../data/validation/compare_handcoded_to_fr_IMAGE.rdata")
