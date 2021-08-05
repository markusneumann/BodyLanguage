library(stringr)
library(dplyr)
library(ggplot2)
library(plm)

load("data/analysis.rdata")

#-----------------------------------------------------------------------------
# Descriptive statistics

office_by_year <- (xtabs(~ office_level + year, data =  wmp_movement))
office_caption <- "Number of observations by office and election cycle, with all state-level offices below governor combined into 'Down-ballot'."
xt <- xtable::xtable(office_by_year, 
                     caption = office_caption)
xtable::label(xt) <- "office_by_year"
xtable::print.xtable(xt,
                     file = "../tables/office_by_year.tex")

#number of candidates
length(unique(wmp_movement$st_race_cand))

#candidate stats
candidate_level <- wmp_movement[!duplicated(wmp_movement$st_race_cand),]
table(candidate_level$party)
table(candidate_level$female)

# DV
range(wmp_movement$vertical)
range(wmp_movement$vertical)
sqrt(var(wmp_movement$vertical))

# Number of frames
sum(wmp_movement$N_frames)
# Time in hours
(sum(wmp_movement$N_frames)/25)*3600

#-----------------------------------------------------------------------------
# Regression models

# OLS
m_1 <- lm(vertical ~ female + l, data = wmp_movement)
m_2 <- lm(vertical ~ female + party + l, data = wmp_movement)
m_3 <- lm(vertical ~ female + party + office_level + l, data = wmp_movement)
m_4 <- lm(vertical ~ female + party + office_level + incumbency + l, data = wmp_movement)
m_5 <- lm(vertical ~ female + party + office_level + incumbency + l + party*female + office_level*female + incumbency*female, data = wmp_movement)
summary(m_1)

covar_labs_main <- c("Female", "Party: Republican", "Office: Governor", 
                     "Office: House", "Office: President", "Office: Senate",
                     "Incumbent", "Open Seat", "Candidate frames","Female*Party: Republican",
                     "Female*Office: Governor", "Female*Office: House", "Female*Office: President",
                     "Female*Office: Senate", "Female*Incumbent", "Female*Open Seat")
dep_var_labs_main <- "Vertical hand movement"
stargazer::stargazer(m_1, m_2, m_3, m_4, m_5,
                     out = "../tables/handmovement_ols.tex",
                     font.size = "scriptsize",
                     covariate.labels = covar_labs_main,
                     dep.var.labels = dep_var_labs_main,
                     df = F,
                     label = "handmovement_ols",
                     title = "Movement regressed on candidate level covariates. The table shows that female politicians move less, as do Republicans as well as most federal candidates except for Presidential aspirants. Interaction effects between covariates and gender are largely not statistically significant.")

# Figure
critical_value <- 1.96 # 1.645 for 90%; 1.96 for 95%
f_1 <- as.data.frame(summary(m_1)$coefficients)
f_2 <- as.data.frame(summary(m_2)$coefficients)
f_3 <- as.data.frame(summary(m_3)$coefficients)
f_4 <- as.data.frame(summary(m_4)$coefficients)
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
                   "Control: Candidate frames")
f_ols_names_4 <- c("Intercept", "Female", "Party: Republican",
                   "Office: Governor", "Office: House", "Office: President", "Office: Senate",
                   "Incumbency: Challenger", "Incumbency: Open Seat", 
                   "Control: Candidate frames")
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
ggsave("../figures/ols_models.pdf", width = 12, height = 5)


#----
# PLM
m_1_plm <- plm(vertical ~ female + l, data = wmp_movement, index="st_race_cand", model="random")
m_2_plm <- plm(vertical ~ female + party + l, data = wmp_movement, index="st_race_cand", model="random")
m_3_plm <- plm(vertical ~ female + party + office_level + l, data = wmp_movement, index="st_race_cand", model="random")
m_4_plm <- plm(vertical ~ female + party + office_level + incumbency + l, data = wmp_movement, index="st_race_cand", model="random")
m_5_plm <- plm(vertical ~ female + party + office_level + incumbency + l + party*female + office_level*female + incumbency*female, data = wmp_movement, index="st_race_cand", model="random")
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
                     title = "Movement regressed on candidate level covariates, with candidate-level random effects. The table shows that the results are robust to the introduction of unit effects.")

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
                   "Incumbency: Challenger", "Incumbency: Open Seat", 
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


#----
#Issues
wmp_movement$healthcare <- stringr::str_detect(wmp_movement$issue, "HEALTHCARE")
wmp_movement$jobs <- stringr::str_detect(wmp_movement$issue, "JOBS/UNEMPLOYMENT")
wmp_movement$education <- stringr::str_detect(wmp_movement$issue, "EDUCATION")
wmp_movement$publicsafety <- stringr::str_detect(wmp_movement$issue, "PUBLIC SAFETY")
wmp_movement$economy <- stringr::str_detect(wmp_movement$issue, "ECONOMY")
wmp_movement$coronavirus <- stringr::str_detect(wmp_movement$issue, "CORONAVIRUS")
wmp_movement$budget <- stringr::str_detect(wmp_movement$issue, "BUDGET/GOVERNMENT SPENDING")
wmp_movement$taxes <- stringr::str_detect(wmp_movement$issue, "TAXES")
wmp_movement$energyenvironment <- stringr::str_detect(wmp_movement$issue, "ENERGY/ENVIRONMENT")
wmp_movement$protrump <- stringr::str_detect(wmp_movement$issue, "PRO-TRUMP")
wmp_movement$immigration <- stringr::str_detect(wmp_movement$issue, "IMMIGRATION")
wmp_movement$antitrump <- stringr::str_detect(wmp_movement$issue, "ANTI-TRUMP")
wmp_movement$guncontrol <- stringr::str_detect(wmp_movement$issue, "GUN CONTROL")
wmp_movement$prescriptiondrugs <- stringr::str_detect(wmp_movement$issue, "PRESCRIPTION DRUGS")
wmp_movement$abortion <- stringr::str_detect(wmp_movement$issue, "ABORTION")
wmp_movement$womensrights <- stringr::str_detect(wmp_movement$issue, "WOMEN'S RIGHTS")
wmp_movement$domesticabuse <- stringr::str_detect(wmp_movement$issue, "DOMESTIC ABUSE")


m_issue_all_f <- lm(vertical ~ economy + coronavirus + budget + taxes + energyenvironment + protrump + immigration + antitrump + guncontrol + prescriptiondrugs + abortion + womensrights + domesticabuse +
                      economy*female + coronavirus*female + budget*female + taxes*female + energyenvironment*female + protrump*female + immigration*female + antitrump*female + guncontrol*female + prescriptiondrugs*female + abortion*female + womensrights*female + domesticabuse*female + l, 
                    data = wmp_movement)

m_issue_all_f_plm <- plm(vertical ~ economy + coronavirus + budget + taxes + energyenvironment + protrump + immigration + antitrump + guncontrol + prescriptiondrugs + abortion + womensrights + domesticabuse +
                           economy*female + coronavirus*female + budget*female + taxes*female + energyenvironment*female + protrump*female + immigration*female + antitrump*female + guncontrol*female + prescriptiondrugs*female + abortion*female + womensrights*female + domesticabuse*female + l,
                         data=wmp_movement, index="st_race_cand", model="random")

m_issue_everything <- lm(vertical ~ economy + coronavirus + budget + taxes + energyenvironment + protrump + immigration + antitrump + guncontrol + prescriptiondrugs + abortion + womensrights + domesticabuse + female +
                           economy*female + coronavirus*female + budget*female + taxes*female + energyenvironment*female + protrump*female + immigration*female + antitrump*female + guncontrol*female + prescriptiondrugs*female + abortion*female + womensrights*female + domesticabuse*female +
                           party + office_level + incumbency + l,
                         data = wmp_movement)

m_issue_everything_plm <- plm(vertical ~ economy + coronavirus + budget + taxes + energyenvironment + protrump + immigration + antitrump + guncontrol + prescriptiondrugs + abortion + womensrights + domesticabuse + female +
                                economy*female + coronavirus*female + budget*female + taxes*female + energyenvironment*female + protrump*female + immigration*female + antitrump*female + guncontrol*female + prescriptiondrugs*female + abortion*female + womensrights*female + domesticabuse*female +
                                party + office_level + incumbency + l,
                              data=wmp_movement, index="st_race_cand", model="random")

# Issues - table
m_issue_labs <- c("Economy", "Coronavirus", "Budget/Government Spending", "Taxes", "Energy/Environment", "Pro-Trump", "Immigration",
                  "Anti-Trump", "Gun Control", "Prescription Drugs", "Abortion", "Womens Rights", "Domestic Abuse", "Female",
                  "Economy*Female", "Coronavirus*Female", "Budget/Government Spending*Female", "Taxes*Female", "Energy/Environment*Female", "Pro-Trump*Female", "Immigration*Female",
                  "Anti-Trump*Female", "Gun Control*Female", "Prescription Drugs*Female", "Abortion*Female", "Womens Rights*Female", "Domestic Abuse*Female")
m_issue_caption = "Regressing movement on the most common policy issues. All models include controls for the proportion of a video in which the candidate is featured. Models 3 and 4 also include the candidate covariates from above, while models 2 and 4 inlude candidate-level random effects. The table shows that certain issues, such as taxes or anti-Trump, entail a greater amount of movement, although most effects are not robust throughout all four models. Some of the issues that directly pertain to women, such as abortion and domestic abuse also have negative and statistically significant effects when interacted with female candidates."
stargazer::stargazer(m_issue_all_f, m_issue_all_f_plm, m_issue_everything, m_issue_everything_plm,
                     add.lines = list(c("Candidate covariates included", "No", "No", "Yes", "Yes"),
                                      c("Candidate random effects", "No", "Yes", "No", "Yes")),
                     font.size = "scriptsize",
                     model.names = F,
                     covariate.labels = m_issue_labs,
                     omit.stat = c("f", "ser"),
                     out = "../tables/handmovement_issues.tex",
                     dep.var.labels = dep_var_labs_main,
                     label = "handmovement_issues",
                     title = m_issue_caption,
                     omit = c("party", "office_level", "incumbency", "l"))


# Issues - figure
f_issue_all_f <- as.data.frame(summary(m_issue_all_f)$coefficients)
f_issue_all_f_plm <- as.data.frame(summary(m_issue_all_f_plm)$coefficients)
f_issue_everything <- as.data.frame(summary(m_issue_everything)$coefficients)
f_issue_everything_plm <- as.data.frame(summary(m_issue_everything_plm)$coefficients)

f_issue_all_f <- subset(f_issue_all_f, select = -c(`t value`, `Pr(>|t|)`))
f_issue_all_f_plm <- subset(f_issue_all_f_plm, select = -c(`z-value`, `Pr(>|z|)`))
f_issue_everything <- subset(f_issue_everything, select = -c(`t value`, `Pr(>|t|)`))
f_issue_everything_plm <- subset(f_issue_everything_plm, select = -c(`z-value`, `Pr(>|z|)`))

#add labels for model
f_issue_all_f$Model <- "Issues"
f_issue_all_f_plm$Model <- "Issues + Random Effects"
f_issue_everything$Model <- "Issues + Covariates"
f_issue_everything_plm$Model <- "Issues + Covariates + Random Effects"

f_issue <- rbind(f_issue_all_f, f_issue_all_f_plm, f_issue_everything, f_issue_everything_plm)

f_issue$ymin <- f_issue$Estimate-critical_value*f_issue$`Std. Error`
f_issue$ymax <- f_issue$Estimate+critical_value*f_issue$`Std. Error`

remove_rows <- c("partyREPUBLICAN","office_level", "incumbency", "Intercept")

idx <- list()
for (i in 1:nrow(f_issue)){
  if(any(str_detect(rownames(f_issue)[i], remove_rows))){
    idx[[i]] <- i
  }
}
idx <- unlist(idx)
f_issue <- f_issue[-idx,]
f_issue <- f_issue[-which(rownames(f_issue) %in% c("l", "l1", "l2", "l3")),]

f_issue_labs <- c("Economy", "Coronavirus", "Budget/Government Spending", "Taxes", "Energy/Environment", "Pro-Trump", "Immigration",
                  "Anti-Trump", "Gun Control", "Prescription Drugs", "Abortion", "Womens Rights", "Domestic Abuse", "Female",
                  "Economy", "Coronavirus", "Budget/Government Spending", "Taxes", "Energy/Environment", "Pro-Trump", "Immigration",
                  "Anti-Trump", "Gun Control", "Prescription Drugs", "Abortion", "Womens Rights", "Domestic Abuse")

f_issue$varname <- rep(f_issue_labs, 4)
f_issue <- f_issue[!f_issue$varname == "Female",]
f_issue$Gender <- "Male"
f_issue$Gender[stringr::str_detect(rownames(f_issue), "\\:female")] <- "Female"

f_issue$Significant <- 1
f_issue$Significant[f_issue$ymax>0 & f_issue$ymin<0] <- 0.99

f_issue$varname <- factor(f_issue$varname, levels = rev(unique(f_issue$varname)))
f_issue$Model <- factor(f_issue$Model, levels = unique(f_issue$Model))

ggplot(f_issue, aes(y = Estimate, x = varname, color = Gender, shape = Gender, alpha = Significant)) + geom_point(size=2, position=position_dodge(width=.2)) +
  geom_hline(yintercept=0, color = "grey", linetype = 2, size = .7) +
  geom_errorbar(mapping=aes(ymin=ymin, ymax=ymax), size=.7, width=0, position=position_dodge(width=.2)) + coord_flip() +
  labs(y = "Coefficient Estimate", x = "", color = "Gender", shape = "Gender") +
  facet_wrap(~Model) + 
  scale_alpha(guide = "none") +
  theme_bw() + theme(legend.position = "bottom") + scale_colour_grey(start = 0.6, end = 0.2)
ggsave("../figures/issues_models.pdf", height = 10, width = 10)
