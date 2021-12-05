library(stringr)
library(dplyr)
library(ggplot2)
library(plm)
library(data.table)

# Load the metadata
load("../data/video_metadata_all.rdata")
# Load the movement scores
load("../results/dominance/convhull_on_all_data.rdata")

# Merge the two datasets
df$year_alt <- str_remove(df$path, "../results/combined_fr_pd/face_recognition_")
df$year_alt <- str_remove(df$year_alt, "_landmarks.csv")
df <- select(df, c(year_alt, vertical, combined_length))
df <- left_join(df, video_metadata, by = "year_alt")
# Kick out videos without a vertical movement measure
df <- df[is.na(df$vertical)==F,]
# Kick out videos without a female variable
df <- df[is.na(df$female)==F,]
# Kick out non-major party
df <- df[df$party %in% c("DEMOCRAT", "REPUBLICAN"),]
# Kick out videos that consist of only still images
# Note that not all of these videos are still in the dataset, some have already been removed before
# 2018
load("../data/problem_videos_18.rdata")
df <- df[-which(df$alt %in% problems18),]
# 2020
load("../data/problem_videos_20.rdata")
df <- df[-which(df$alt %in% problems20),]

#-----------------------------------------------------------------------------
# Descriptive statistics

office_by_year <- (xtabs(~ office_level + year, data =  df))
office_caption <- "Number of observations by office and election cycle, with all state-level offices below governor combined into 'Down-ballot'."
xt <- xtable::xtable(office_by_year, 
                     caption = office_caption)
xtable::label(xt) <- "office_by_year"
xtable::print.xtable(xt,
                     file = "../tables/office_by_year.tex")

#number of candidates
length(unique(df$st_race_cand))

#candidate stats
candidate_level <- df[!duplicated(df$st_race_cand),]
table(candidate_level$party)
table(candidate_level$female)

# candidate-level female/male assertiveness
candidate_means <- aggregate(df$vertical, by = list(df$st_race_cand, df$female), mean)
names(candidate_means) <- c("st_race_cand", "female", "vertical")
round(summary(candidate_means$vertical[candidate_means$female==T]), 2)
round(summary(candidate_means$vertical[candidate_means$female==F]), 2)
round(sqrt(var(candidate_means$vertical[candidate_means$female==T])), 2)
round(sqrt(var(candidate_means$vertical[candidate_means$female==F])), 2)


# DV
range(df$vertical)
sqrt(var(df$vertical))
mean(df$vertical)

# Number of frames
sum(df$l*25)
# Time in hours
(sum(df$l))/3600

#-----------------------------------------------------------------------------
# Regression models

#----
# PLM
m_1_plm <- plm(vertical ~ female + combined_length, data = df, index="st_race_cand", model="random")
m_2_plm <- plm(vertical ~ female + party + combined_length, data = df, index="st_race_cand", model="random")
m_3_plm <- plm(vertical ~ female + party + office_level + combined_length, data = df, index="st_race_cand", model="random")
m_4_plm <- plm(vertical ~ female + party + office_level + incumbency + combined_length, data = df, index="st_race_cand", model="random")
m_5_plm <- plm(vertical ~ female + party + office_level + incumbency + combined_length + party*female + office_level*female + incumbency*female, data = df, index="st_race_cand", model="random")
#summary(m_5_plm)

covar_labs_plm <- c("Female", "Party: Republican", "Office: Governor", 
                      "Office: House", "Office: President", "Office: Senate",
                      "Incumbent", "Open Seat", "Control: Candidate frames", "Female*Party: Republican",
                      "Female*Office: Governor", "Female*Office: House", "Female*Office: President",
                      "Female*Office: Senate", "Female*Incumbent", "Female*Open Seat")
dep_var_labs_plm <- "Vertical hand movement"
stargazer::stargazer(m_1_plm, m_2_plm, m_3_plm, m_4_plm, m_5_plm,
                     out = "../tables/handmovement_plm.tex",
                     font.size = "scriptsize",
                     covariate.labels = covar_labs_plm,
                     dep.var.labels = dep_var_labs_plm,
                     label = "handmovement_plm",
                     title = "Movement regressed on candidate level covariates, with candidate level random effects. The table shows that female politicians move less, as do Republicans as well as federal and gubernatorial candidates. Interaction effects between covariates and gender are largely not statistically significant.")

critical_value <- 1.96 # 1.645 for 90%; 1.96 for 95%

f_1 <- as.data.frame(summary(m_1_plm)$coefficients)
f_2 <- as.data.frame(summary(m_2_plm)$coefficients)
f_3 <- as.data.frame(summary(m_3_plm)$coefficients)
f_4 <- as.data.frame(summary(m_4_plm)$coefficients)
f_1$Model <- "Model (1)"
f_2$Model <- "Model (2)"
f_3$Model <- "Model (3)"
f_4$Model <- "Model (4)"
f_ols <- rbind(f_1, f_2, f_3, f_4)
f_ols$ymin <- f_ols$Estimate-critical_value*f_ols$`Std. Error`
f_ols$ymax <- f_ols$Estimate+critical_value*f_ols$`Std. Error`
f_ols_names_1 <- c("Intercept", "Female", "Control")
f_ols_names_2 <- c("Intercept", "Female", "Party: Republican", "Control")
f_ols_names_3 <- c("Intercept", "Female", "Party: Republican", 
                   "Office: Governor", "Office: House", "Office: President", "Office: Senate",
                   "Control")
f_ols_names_4 <- c("Intercept", "Female", "Party: Republican",
                   "Office: Governor", "Office: House", "Office: President", "Office: Senate",
                   "Incumbency: Incumbent", "Incumbency: Open Seat", 
                   "Control")
f_ols_names <- c(f_ols_names_1, f_ols_names_2, f_ols_names_3, f_ols_names_4)
f_ols$varname <- f_ols_names
f_ols <- f_ols[!f_ols$varname %in% c("Intercept", "Control"),]
f_ols$Significant <- 1
f_ols$Significant[f_ols$ymax>0 & f_ols$ymin<0] <- 0
f_ols$varname <- factor(f_ols$varname, levels = rev(unique(f_ols$varname)))

ggplot(f_ols, aes(y = Estimate, x = varname, alpha = Significant)) + geom_point(size=3) +
  geom_hline(yintercept=0, color = "grey", linetype = 2, size = .7) +
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), size=1, width=0) + coord_flip() +
  labs(y = "Coefficient Estimate", x = "", color = "Policy Area", shape = "Policy Area") +
  facet_wrap(~Model, ncol = 4) +
  scale_alpha(guide = "none") + #adding another alpha scale removes it from the legend
  theme_bw() + theme(legend.position = "bottom") + scale_colour_grey(start = 0.6, end = 0.2)
ggsave("../figures/plm_models.pdf", width = 12, height = 5)
