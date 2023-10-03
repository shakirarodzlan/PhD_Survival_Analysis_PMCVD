
# set working directory

setwd("C:/Users/shakirarodzlan/Desktop/PhD/PhD Chapters/Chapter 3- Regression/Data Analysis/Final analysis/Analysis PMCVD/v6 analysis")

################# Analysis using pmcvd data ########################################################

# read pmcvd data
#install.packages("readxl")
library(readxl)
pmcvd <- read_excel("pmcvd2.xlsx")
str(pmcvd)

## change character to factors 
library(tidyverse)
pmcvd <- pmcvd %>%
  mutate_if(is.character, as.factor)

#re-order level some variables
pmcvd$strata <- factor(pmcvd$strata, levels = c("Urban", "Rural"))
pmcvd$occu2 <- factor(pmcvd$occu2, levels = c("Working", "Not Working"))
pmcvd$adj_income_grpB40 <- factor(pmcvd$adj_income_grpB40, levels = c("top20%", "middle40%", "bottom40%"))
pmcvd$marital <- factor(pmcvd$marital, levels = c("Never married", "Married", "Widow/Widower/Divorcee"))
pmcvd$dm_status <- factor(pmcvd$dm_status, levels = c("Normal", "Diagnosed DM", "Undiagnosed DM"))
pmcvd$hpt_status <- factor(pmcvd$hpt_status, levels = c("Normal", "Diagnosed HPT", "Undiagnosed HPT"))
pmcvd$chol_status <- factor(pmcvd$chol_status, levels = c("Normal", "Diagnosed chol", "Undiagnosed chol"))
pmcvd$education_level <- factor(pmcvd$education_level, levels = c("Tertiary", "Secondary", 
                                                                  "Primary", "No formal education"))
summary (pmcvd)

# Remove un-used variable (known, unknown and total dm, hpt, chol &  also drinker)
names (pmcvd)
pmcvd <- subset(pmcvd, select = -c(dm_known, dm_unknown_fast, dm_total,
                                   hpt_known, hpt_unknown, hpt_total,
                                   chol_known, chol_unknown, chol_total, drinker_current,
                                   glucose_reading, fasting_status,
                                   dm_unknown_fast7, high_glucose_not_fasting))
names (pmcvd)



######################################### Descriptive analysis ################################################

install.packages("gtsummary")  # Install gtsummary package (if not already installed)
library(gtsummary)             # Load gtsummary package
library(dplyr)                 # Load dplyr package for data manipulation (if not already loaded)

#overall
summary_table <- pmcvd %>%
  tbl_summary(missing = "no", digits = list(all_categorical() ~ c(0, 1))) 
summary_table

#by premature cvd

summary_table2 <- pmcvd %>%
  tbl_summary(by = status_pmcvd, missing = "no", percent = "row",
              digits = list(all_categorical() ~ c(0, 1))) 
summary_table2


#combine table
combined_table <- tbl_merge(list(summary_table, summary_table2),
                            tab_spanner = c("**Total**", "**Premature mortality status**"))
combined_table

# save table
combined_table %>%
  as_gt() %>%
  gt::gtsave(filename = "Table 1 - Descriptive2 pmcvd.html") 


## descriptive by NHMS data - for supplement file

#total
summary_table4 <- pmcvd %>%
  tbl_summary(by = data_source, missing = "no", digits = list(all_categorical() ~ c(0, 1)))
summary_table4

#by premature mortality
summary_table5 <- pmcvd %>%
  filter (data_source == "NHMS2006") %>%
  tbl_summary(by = status_pmcvd, missing = "no", percent = "row", digits = list(all_categorical() ~ c(0, 1))) 
summary_table5

summary_table6 <- pmcvd %>%
  filter (data_source == "NHMS2011") %>%
  tbl_summary(by = status_pmcvd, missing = "no", percent = "row", digits = list(all_categorical() ~ c(0, 1))) 
summary_table6

summary_table7 <- pmcvd %>%
  filter (data_source == "NHMS2015") %>%
  tbl_summary(by = status_pmcvd, missing = "no", percent = "row", digits = list(all_categorical() ~ c(0, 1))) 
summary_table7

#combine table
combined_table2 <- tbl_merge(list(summary_table4, summary_table5, summary_table6, summary_table7),
                            tab_spanner = c("**Total**", "**NHMS 2006**", "**NHMS 2011**", "**NHMS 2015**"))
combined_table2

# save table
combined_table2 %>%
  as_gt() %>%
  gt::gtsave(filename = "Table S1 - Descriptive by NHMS.html")






############################################# Survival analysis  ##################################################


################################ 1) Semi parametric (Cox model) #############################################

#################################

#   1) simple cox
library(survival)

 #SCD
cox.sex <- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ sex, data = pmcvd)
cox.age <- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ age_entry, data = pmcvd)
cox.strata <- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ strata, data = pmcvd)
cox.ethnic <- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ ethnic, data = pmcvd)
cox.education <- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ education_level, data = pmcvd)
cox.occu <- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ occu2, data = pmcvd)
cox.income <- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ adj_income_grpB40, data = pmcvd)
cox.marital<- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ marital, data = pmcvd)
 #commodities
cox.dm_status<- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ dm_status, data = pmcvd)
cox.hpt_status<- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ hpt_status, data = pmcvd)
cox.chol_status<- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ chol_status, data = pmcvd)
#other NCD risk factors
cox.obese_asian<- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ obese_asian, data = pmcvd)
cox.abd_obesity <- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ abdominal_obesity, data = pmcvd)
cox.current_smoker<- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ current_smoker, data = pmcvd)
cox.PA_cat<- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ PA_cat, data = pmcvd)


# gtsummary for univarible cox
install.packages("gtsummary")  # Install gtsummary package (if not already installed)
library(gtsummary)             # Load gtsummary package

t1 <- tbl_regression(cox.sex, exponentiate = TRUE)
t2 <- tbl_regression(cox.age, exponentiate = TRUE)
t3 <- tbl_regression(cox.strata, exponentiate = TRUE)
t4 <- tbl_regression(cox.ethnic, exponentiate = TRUE)
t5 <- tbl_regression(cox.education, exponentiate = TRUE)
t6 <- tbl_regression(cox.occu, exponentiate = TRUE)
t7 <- tbl_regression(cox.income, exponentiate = TRUE)
t8 <- tbl_regression(cox.marital, exponentiate = TRUE)
t9 <- tbl_regression(cox.dm_status, exponentiate = TRUE)
t10 <- tbl_regression(cox.hpt_status, exponentiate = TRUE)
t11 <- tbl_regression(cox.chol_status, exponentiate = TRUE)
t12 <- tbl_regression(cox.obese_asian, exponentiate = TRUE)
t13 <- tbl_regression(cox.abd_obesity, exponentiate = TRUE)
t14 <- tbl_regression(cox.current_smoker, exponentiate = TRUE)
t15 <- tbl_regression(cox.PA_cat, exponentiate = TRUE)

merged_simplecox <- tbl_stack(list(t1, t2, t3, t4, t5, 
                                   t6, t7, t8, t9, t10,
                                   t11, t12, t13, t14,
                                   t15))
merged_simplecox

# save table

merged_simplecox %>%
  as_gt() %>%
  gt::gtsave(filename = "SimpleCox.html")



## or use tbl_uvregression
library(gtsummary)

tab_uvreg <- pmcvd %>%
  dplyr::select(survival_time, status_pmcvd, sex, age_entry, strata, 
                ethnic, education_level, occu2, adj_income_grpB40,
                marital, dm_status, hpt_status, chol_status, 
                obese_asian, abdominal_obesity, current_smoker,
                PA_cat) %>%
  tbl_uvregression(
    method = coxph,
    y = Surv(survival_time, status_pmcvd == "premature cvd death"),
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  as_gt()

tab_uvreg # but cannot merge table with multicox (tbl_regression)



#########################################

## 2)  multiple cox - model selection

library(survival)

# use complete case for variables selection 
# so that we can compare the model using LRT test/chi2 test

library(dplyr)
# Drop NA
pmcvd2 <- 
  pmcvd %>% 
  drop_na()

pmcvd2 <- na.omit(pmcvd)

# a) Full model
full_model <- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ 
                      sex + age_entry + strata + ethnic + education_level +
                      occu2 + adj_income_grpB40 + marital + 
                      dm_status +  hpt_status + chol_status +
                      obese_asian + abdominal_obesity +
                      current_smoker + PA_cat, data =  pmcvd2)
summary(full_model)

# 2) Backward selection
bw_model <- step(full_model, direction= "backward") # only can run for complete case (no missing)
summary(bw_model)

# Compare full model and backward selection model
anova(full_model, bw_model, test = "LRT") #no sig difference btwn model, pick simpler model (less variable)
AIC(full_model); AIC(bw_model)

# 3) Model backward w/o occupation 
mod2 <- coxph(formula = Surv(survival_time, status_pmcvd == "premature cvd death") ~ 
                sex + age_entry + strata  + ethnic + education_level +dm_status + 
                hpt_status + chol_status + abdominal_obesity + current_smoker, data = pmcvd2)
summary (mod2)
# Compare backward model with model2 (optional, maybe backward model better?)
anova(bw_model, mod2, test = "LRT")  #no sig difference btwn model, pick simpler model (less variable)
AIC(full_model); AIC(bw_model); AIC(mod2)


# used model2 for full data (pmcvd) without complete analysis


mod3 <- coxph(formula = Surv(survival_time, status_pmcvd == "premature cvd death") ~ 
                    sex + age_entry + strata + ethnic + education_level + 
                    dm_status + hpt_status + chol_status + abdominal_obesity + 
                    current_smoker, data = pmcvd)
summary (mod3) # strata(rural) become significant
AIC(mod3)

# compare model 
  # LR test - cannot compare model3 with other model (bcoz diff s.size)
  # can compare using AIC
AIC(full_model); AIC(bw_model); AIC(mod3)

  # although AIC not so good in model 3, but, we have signifant education (important variable) and better HR for DM

# So I select model 3 as prelim model

# Preliminary model
summary(mod3)

#gtsummary
table.mod3 <- tbl_regression(mod3, exponentiate = TRUE)
table.mod3

# save table
table.mod3 %>%
  as_gt() %>%
  gt::gtsave(filename = "Adjusted_Cox_without_stratified.html")



###############################################

## 3) check interaction - check one by one 

##check interaction based on reduced model (mod3)
  #just present significant interaction

  # a) DM & Hpt
int.mod1 <- coxph(formula = Surv(survival_time, status_pmcvd == "premature cvd death") ~ 
                sex + age_entry + strata + ethnic + education_level + dm_status + 
                hpt_status + chol_status + abdominal_obesity + current_smoker +
                  dm_status*hpt_status, 
              data = pmcvd)

summary(int.mod1)

  # b) DM & abd.obesity
int.mod2 <- coxph(formula = Surv(survival_time, status_pmcvd == "premature cvd death") ~ 
                   sex + age_entry  + strata+ ethnic + education_level + dm_status + 
                   hpt_status + chol_status + abdominal_obesity + current_smoker +
                   dm_status*abdominal_obesity, 
                 data = pmcvd)
summary (int.mod2)

  # c) dm vs current smoker
int.mod3 <- coxph(formula = Surv(survival_time, status_pmcvd == "premature cvd death") ~ 
                    sex + age_entry  + strata + ethnic + education_level + dm_status + 
                    hpt_status + chol_status + abdominal_obesity + current_smoker +
                    dm_status*current_smoker, 
                  data = pmcvd)
summary(int.mod3)


#gtsummary
tab_int1 <- tbl_regression(int.mod1, exponentiate = TRUE)
tab_int2 <- tbl_regression(int.mod2, exponentiate = TRUE)
tab_int3 <- tbl_regression(int.mod3, exponentiate = TRUE)

# save table
tab_int1 %>%
  as_gt() %>%
  gt::gtsave(filename = "Inter.model1.html")

tab_int2 %>%
  as_gt() %>%
  gt::gtsave(filename = "Inter.model2.html")

tab_int3 %>%
  as_gt() %>%
  gt::gtsave(filename = "Inter.model3.html")


#combine table
combined_inter <- tbl_merge(list(tab_int1, tab_int2, tab_int3),
                             tab_spanner = c("**DMxHPT**", "**DMxAbdObesity**", "**DMxSmoker**"))
combined_inter

# save table
combined_inter %>%
  as_gt() %>%
  gt::gtsave(filename = "Table 3 - Interaction models.html")



################################################33

##### 4) Check PH assumption

# Checking the proportional hazards assumption
  #reduce model
ph_test <- cox.zph(mod3)
print(ph_test)

# Plotting Schoenfeld residuals for each predictor
par(mfrow = c(2, 3))
plot(ph_test) #reduce model

#Global test significant - (age, ethnic, education, current smoker significant)
# schoenfield residual plot shows obvious in age




##############################

# 5) Run stratified cox model - for violate PH assumption

# Stratified by age_entry 

library(survival)

str.mod3 <- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") 
                  ~ sex + strata(age_entry) + strata + ethnic + education_level +
                    dm_status +  hpt_status + chol_status +
                    abdominal_obesity + current_smoker,
                  data =  pmcvd)
summary(str.mod3)


# Compare reduced model (mode3) with str.model 
anova(mod3, str.mod3, test = "LRT")  #sig difference btwn model, 
AIC(mod3); AIC(str.mod3) #AIC much better in stratified cox

# check PH for stratified model
ph_test2 <- cox.zph(str.mod3)
print(ph_test2) # global test not sign

par(mfrow = c(2, 3))
plot(ph_test2) #all plot much better


# final model for stratified cox
summary(str.mod3)

#gtsummary
table.str.mod3 <- tbl_regression(str.mod3, exponentiate = TRUE)
table.str.mod3

# save table
table.str.mod3 %>%
  as_gt() %>%
  gt::gtsave(filename = "Table 4 - stratified_Coxmodel_strAge.html")



###########

## 5) check interaction of stratified cox 

#just present significant interaction

# a) DM & Hpt
int.str.mod1 <- coxph(formula = Surv(survival_time, status_pmcvd == "premature cvd death") ~ 
                    sex + strata(age_entry) + strata + ethnic + education_level + dm_status + 
                    hpt_status + chol_status + abdominal_obesity + current_smoker +
                    dm_status*hpt_status, 
                  data = pmcvd)
summary(int.str.mod1)

# b) DM & abd.obesity
int.str.mod2 <- coxph(formula = Surv(survival_time, status_pmcvd == "premature cvd death") ~ 
                    sex + strata(age_entry) + strata + ethnic + education_level + dm_status + 
                    hpt_status + chol_status + abdominal_obesity + current_smoker +
                    dm_status*abdominal_obesity, 
                  data = pmcvd)
summary (int.str.mod2)

# c) dm vs current smoker
int.str.mod3 <- coxph(formula = Surv(survival_time, status_pmcvd == "premature cvd death") ~ 
                    sex + strata(age_entry) + strata + ethnic + education_level + dm_status + 
                    hpt_status + chol_status + abdominal_obesity + current_smoker +
                    dm_status*current_smoker, 
                  data = pmcvd)
summary(int.str.mod3)

# d) Hpt vs abd.obesity
int.str.mod4 <- coxph(formula = Surv(survival_time, status_pmcvd == "premature cvd death") ~ 
                    sex + strata(age_entry) + strata + ethnic + education_level + dm_status + 
                    hpt_status + chol_status + abdominal_obesity + current_smoker +
                    hpt_status*abdominal_obesity, 
                  data = pmcvd)
summary (int.str.mod4)

# d) Hpt vs smoking
int.str.mod5 <- coxph(formula = Surv(survival_time, status_pmcvd == "premature cvd death") ~ 
                        sex + strata(age_entry) + strata + ethnic + education_level + dm_status + 
                        hpt_status + chol_status + abdominal_obesity + current_smoker +
                        hpt_status*current_smoker, 
                      data = pmcvd)
summary (int.str.mod5) 

# e) Abd obesity vs smoking
int.str.mod6 <- coxph(formula = Surv(survival_time, status_pmcvd == "premature cvd death") ~ 
                        sex + strata(age_entry) + strata + ethnic + education_level + dm_status + 
                        hpt_status + chol_status + abdominal_obesity + current_smoker +
                        abdominal_obesity*current_smoker, 
                      data = pmcvd)
summary (int.str.mod6) 


#gtsummary
tab_str.int1 <- tbl_regression(int.str.mod1, exponentiate = TRUE)
tab_str.int2 <- tbl_regression(int.str.mod2, exponentiate = TRUE)
tab_str.int3 <- tbl_regression(int.str.mod3, exponentiate = TRUE)
tab_str.int4 <- tbl_regression(int.str.mod4, exponentiate = TRUE)
tab_str.int5 <- tbl_regression(int.str.mod5, exponentiate = TRUE)
tab_str.int6 <- tbl_regression(int.str.mod6, exponentiate = TRUE)

# save table
tab_int1 %>%
  as_gt() %>%
  gt::gtsave(filename = "Inter.model1.html")

tab_int2 %>%
  as_gt() %>%
  gt::gtsave(filename = "Inter.model2.html")

tab_int3 %>%
  as_gt() %>%
  gt::gtsave(filename = "Inter.model3.html")

tab_int4 %>%
  as_gt() %>%
  gt::gtsave(filename = "Inter.model4.html")


#combine table
combined_str.inter <- tbl_merge(list(tab_str.int1, tab_str.int2, tab_str.int3, tab_str.int4, tab_str.int5, tab_str.int6),
                            tab_spanner = c("**Stratifed Cox DMxHPT**", "**Stratifed Cox DMxAbdObesity**", 
                                            "**Stratifed Cox DMxSmoker**", "**Stratifed Cox HPTxAbdObesity**",
                                            "**Stratifed Cox HPTxSmoker**", "**Stratifed Cox smokerxAbdObesity**"))
combined_str.inter

# save table
combined_str.inter %>%
  as_gt() %>%
  gt::gtsave(filename = "Table 3 - Interaction stratified models.html")



##### save Cox model for presentation (table 2)

combined_cox_all <- tbl_merge(list(merged_simplecox, table.str.mod3),
                              tab_spanner = c("**Unadjusted Cox Model**",
                                              "**Adjusted stratified Cox Model by age**"))
combined_cox_all
# save table
combined_cox_all %>%
  as_gt() %>%
  gt::gtsave(filename = "Table 2 - Combined all cox models.html") 






############################# 3) Parametric survival analysis ###################################################

#NOTE: 
## because of the some limitation from above package (eha and survival)
## I decided to use flexsurv package using flexsurvreg () function. Can run almost all param test
## can run almost all parametric with nice result AFT and HZ with CI

# parametric survival using  flexsurv package 

# source:  https://devinincerti.com/2019/06/18/parametric_survival.htm
# https://cran.r-project.org/web/packages/flexsurv/flexsurv.pdf


#install.packages("flexsurv")
library(flexsurv)

# 1) exponential  model 
  #PH model
exp_mod <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                       ~ sex + age_entry + strata + ethnic +
                         education_level +dm_status + 
                         hpt_status + chol_status + abdominal_obesity + 
                         current_smoker, data = pmcvd,
                       dist = "exp")
print(exp_mod)
coef (exp_mod)


# 2) Weibull model - cannot run

  # AFT model
wei_mod <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                       ~ sex + age_entry  + strata + ethnic +
                         education_level + dm_status + 
                         hpt_status + chol_status + abdominal_obesity + 
                         current_smoker, data = pmcvd,
                       dist = "weibull")
print(wei_mod)
  # PH model
wei_mod2 <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                       ~ sex + age_entry  + strata + ethnic +
                         education_level + dm_status + 
                         hpt_status + chol_status + abdominal_obesity + 
                         current_smoker, data = pmcvd,
                       dist = "weibullPH")

print(wei_mod2)

plot (wei_mod)



# 3) Gompertz  model 
  # PH Model
gomp_mod <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                        ~ sex + age_entry  + strata + ethnic +
                          education_level + dm_status + 
                          hpt_status + chol_status + abdominal_obesity + 
                          current_smoker, data = pmcvd,
                        dist = "gompertz")
print(gomp_mod)

plot (gomp_mod)


# 4) Log normal 
   # AFT model
lnorm_mod <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                         ~ sex + age_entry  + strata + ethnic +
                           education_level +dm_status + 
                           hpt_status + chol_status + abdominal_obesity + 
                           current_smoker, data = pmcvd,
                         dist = "lnorm")
print(lnorm_mod)
AIC (lnorm_mod)


# 5) log logistic 
  # AFT model
llog_mod <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                        ~ sex + age_entry  + strata + ethnic +
                          education_level + dm_status + 
                          hpt_status + chol_status + abdominal_obesity + 
                          current_smoker, data = pmcvd,
                        dist = "llogis")
print(llog_mod) 

# 6) Gamma 
  #AFT model
gam_mod <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                        ~ sex + age_entry  + strata + ethnic +
                          education_level + dm_status + 
                          hpt_status + chol_status + abdominal_obesity + 
                          current_smoker, data = pmcvd,
                        dist = "gamma")
print(gam_mod) 


# compare AIC for all models
AIC(exp_mod); AIC (wei_mod); AIC (wei_mod2); AIC (gomp_mod); AIC (lnorm_mod);  AIC (llog_mod); AIC (gam_mod)


#########

## Baseline parametric distribution plot using flexsurg package

#a) Survival plot

library(purrr)
library(ggplot2)
library(tidyr)

par_fits <- tibble(
  dist_param = c("exp", "weibull", "gompertz", "lognormal", "llogis"),
  Distributions = c("Exponential", "Weibull", "Gompertz", "Log Normal", "Log-logistic")
) %>%
  mutate(
    fit = map(dist_param, ~flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") ~ 1, data = pmcvd, dist = .x)),
    fit_smry = map(fit, ~summary(.x, ci = FALSE, tidy = TRUE)),
    AIC = map_dbl(fit, ~.x$AIC)
  )

plot_surv <- par_fits %>%
  select(-c(dist_param, fit)) %>%
  unnest(fit_smry) %>%
  ggplot(aes(x = time, y = est, color = Distributions, linetype =Distributions)) +
  geom_line(linewidth = 0.8) +
  theme_classic() +
  xlab("Time (year)") + ylab("Survival") + 
  labs(title = "(a) Parametric survival distribution") 
  


#b) Hazard plot

par_fits2 <- tibble(
  dist_param = c("exp", "weibull", "gompertz", "lognormal", "llogis"),
  Distributions = c("Exponential", "Weibull", "Gompertz", "Log Normal", "Log-logistic")
) %>%
  mutate(
    fit = map(dist_param, ~flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") ~ 1, data = pmcvd, dist = .x)),
    fit_smry = map(fit, ~summary(.x, type = "hazard", ci = FALSE, tidy = TRUE)),
    AIC = map_dbl(fit, ~.x$AIC)
  )

plot_haz <- par_fits2 %>%
  select(-c(dist_param, fit)) %>%
  unnest(fit_smry) %>%
  ggplot(aes(x = time, y = est, color = Distributions, linetype =Distributions)) +
  geom_line(size = 0.8) +  
  theme_classic() + 
  xlab("Time (year)") + ylab("Hazard") + 
  labs(title = "(b) Parametric hazard distribution")

# combine plot

library(gridExtra)
combined_image <- grid.arrange(plot_surv, plot_haz, ncol = 1)

# compare AIC 
par_fits %>%
  arrange(AIC) %>%
  select(Distributions, AIC)

# Add a note AIC value at the bottom
library(gridExtra)
library(grid)

caption <- textGrob("[AIC values: Exponential (13656), Gompertz (13635), Log-logistic (13623), Log Normal (13624), Weibull (13623)]",
                    gp = gpar(fontsize = 10))

# Combine the plots and the caption using grid.arrange()
combined_plot <- grid.arrange(arrangeGrob(plot_surv, plot_haz, ncol = 1), 
                              caption, ncol = 1, heights = c(0.9, 0.1))





###################################################################################################################



################################# Sensitivity Analysis ############################################################


################## 1)  Treat imbalance data ##################

# since the event premature CVD mortality only 1% 
#censored	62,836 (99%)			
#premature cvd death	886 (1.4%)

# I try to remove censored data randomly

# Load the dplyr package for data manipulation
library(dplyr)

# Separate the censored and event cases
censored_cases <- filter(pmcvd, status_pmcvd == "censored")
event_cases <- filter(pmcvd, status_pmcvd == "premature cvd death")

# Get the number of censored cases
num_censored <- nrow(censored_cases)

# Set the desired proportion of censored cases to retain 
desired_proportion <- 0.1 #removed 90% censored data

# Calculate the number of censored cases to retain
num_censored_retain <- round(desired_proportion * num_censored)

# Randomly select the censored cases to retain
censored_cases_subsampled <- censored_cases %>%
  sample_n(num_censored_retain)

# Combine the subsampled censored cases with the event cases
balanced_data <- rbind(censored_cases_subsampled, event_cases)

# Shuffle the rows of the balanced dataset
balanced_data <- balanced_data %>%
  sample_frac(1)

# Now I have a balanced data frame named 'balanced_data' with subsampled censored cases and the event cases.

names (balanced_data)
str(balanced_data)
table (balanced_data$status_pmcvd)

## 1) Semi para cox model

# simple cox

#SCD
cox.sex <- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ sex, data = balanced_data)
cox.age <- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ age_entry, data = balanced_data)
cox.strata <- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ strata, data = balanced_data)
cox.ethnic <- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ ethnic, data = balanced_data)
cox.education <- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ education_level, data = balanced_data)
cox.occu <- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ occu2, data = balanced_data)
cox.income <- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ adj_income_grpB40, data = balanced_data)
cox.marital<- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ marital, data = balanced_data)
#commodities
cox.dm_status<- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ dm_status, data = balanced_data)
cox.hpt_status<- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ hpt_status, data = balanced_data)
cox.chol_status<- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ chol_status, data = balanced_data)
#other NCD risk factors
cox.obese_asian<- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ obese_asian, data = balanced_data)
cox.abd_obesity <- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ abdominal_obesity, data = balanced_data)
cox.current_smoker<- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ current_smoker, data = balanced_data)
#cox.drinker_current<- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ drinker_current, data = balanced_data)
cox.PA_cat<- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") ~ PA_cat, data = balanced_data)

library(gtsummary)
# gtsummary for univarible cox
t1 <- tbl_regression(cox.sex, exponentiate = TRUE)
t2 <- tbl_regression(cox.age, exponentiate = TRUE)
t3 <- tbl_regression(cox.strata, exponentiate = TRUE)
t4 <- tbl_regression(cox.ethnic, exponentiate = TRUE)
t5 <- tbl_regression(cox.education, exponentiate = TRUE)
t6 <- tbl_regression(cox.occu, exponentiate = TRUE)
t7 <- tbl_regression(cox.income, exponentiate = TRUE)
t8 <- tbl_regression(cox.marital, exponentiate = TRUE)
t9 <- tbl_regression(cox.dm_status, exponentiate = TRUE)
t10 <- tbl_regression(cox.hpt_status, exponentiate = TRUE)
t11 <- tbl_regression(cox.chol_status, exponentiate = TRUE)
t12 <- tbl_regression(cox.obese_asian, exponentiate = TRUE)
t13 <- tbl_regression(cox.abd_obesity, exponentiate = TRUE)
t14 <- tbl_regression(cox.current_smoker, exponentiate = TRUE)
t16 <- tbl_regression(cox.PA_cat, exponentiate = TRUE)


merged_simplecox_balance <- tbl_stack(list(t1, t2, t3, t4, t5, 
                                   t6, t7, t8, t9, t10,
                                   t11, t12, t13, t14,
                                   t16))
merged_simplecox_balance

# save table imbalance vs balance 
balance_simplecox <- tbl_merge(list(merged_simplecox, merged_simplecox_balance),
                           tab_spanner = c("**Imbalance data**", "**Balance data**"))
balance_simplecox %>%
  as_gt() %>%
  gt::gtsave(filename = "balance_Imbalance_SimpleCox3.html") 


################# multiple cox #############

# From reduced cox model (mod3)

mod3_balance <- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") 
                          ~ sex + age_entry + strata + ethnic + education_level +
                            dm_status +  hpt_status + chol_status +
                            abdominal_obesity + current_smoker,
                          data =  balanced_data)
summary(mod3_balance)

#gtsummary
tab.mod3_balance <- tbl_regression(mod3_balance, exponentiate = TRUE)
tab.mod3_balance

# From stratified cox model

str.mod3_balance <- coxph(Surv(survival_time, status_pmcvd == "premature cvd death") 
                  ~ sex + strata(age_entry) + strata + ethnic + education_level +
                    dm_status +  hpt_status + chol_status +
                    abdominal_obesity + current_smoker,
                  data =  balanced_data)
summary(str.mod3_balance)

  #gtsummary
tab.str.mod3_balance <- tbl_regression(str.mod3_balance, exponentiate = TRUE)
tab.str.mod3_balance


# save table - combine imbalance imbalance full model, partial model

balance_multi_cox <- tbl_merge(list(table.mod3, tab.mod3_balance, table.str.mod3, tab.str.mod3_balance),
                               tab_spanner = c("**Imbalance data-adj cox**", "**Balance data-adj cox**",
                                               "**Imbalance data-str.cox**", "**Balance data-str.cox**"))
balance_multi_cox %>%
  as_gt() %>%
  gt::gtsave(filename = "balance_Imbalance_Multi_Cox3.html") 


# 2) Parametric model _ choose Log normal model

# Log normal  (AFT model)

library (flexsurv)
lnorm_mod_balance <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                         ~ sex + age_entry  + strata + ethnic +
                           education_level +dm_status + 
                           hpt_status + chol_status + abdominal_obesity + 
                           current_smoker, data = balanced_data,
                         dist = "lnorm")
print(lnorm_mod_balance)


### add unadjusted log normal model balance and imbalance data (for supplement file)

library (flexsurv)
# a) Imbalance Ori data
lnorm_sex <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                                 ~ sex, data = pmcvd, dist = "lnorm")
lnorm_age <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                         ~ age_entry, data = pmcvd, dist = "lnorm")
lnorm_rural <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                         ~ strata, data = pmcvd, dist = "lnorm")
lnorm_ethnic <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                           ~ ethnic, data = pmcvd, dist = "lnorm")
lnorm_edu <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                            ~ education_level, data = pmcvd, dist = "lnorm")
lnorm_occu <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                         ~ occu2, data = pmcvd, dist = "lnorm")
lnorm_income <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                          ~ adj_income_grpB40, data = pmcvd, dist = "lnorm")
lnorm_marital <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                            ~ marital, data = pmcvd, dist = "lnorm")
lnorm_dm <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                             ~ dm_status, data = pmcvd, dist = "lnorm")
lnorm_hpt <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                        ~ hpt_status, data = pmcvd, dist = "lnorm")
lnorm_chol <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                         ~ chol_status, data = pmcvd, dist = "lnorm")
lnorm_obese <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                              ~ obese_asian, data = pmcvd, dist = "lnorm")
lnorm_abdobese <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                          ~ abdominal_obesity, data = pmcvd, dist = "lnorm")
lnorm_smoker <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                              ~ current_smoker, data = pmcvd, dist = "lnorm")
lnorm_PA <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                              ~ PA_cat, data = pmcvd, dist = "lnorm")

# b) Balance data
lnorm_sex2 <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                         ~ sex, data = balanced_data, dist = "lnorm")
lnorm_age2 <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                         ~ age_entry, data = balanced_data, dist = "lnorm")
lnorm_rural2 <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                           ~ strata, data = balanced_data, dist = "lnorm")
lnorm_ethnic2 <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                            ~ ethnic, data = balanced_data, dist = "lnorm")
lnorm_edu2 <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                         ~ education_level, data = balanced_data, dist = "lnorm")
lnorm_occu2 <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                          ~ occu2, data = balanced_data, dist = "lnorm")
lnorm_income2 <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                            ~ adj_income_grpB40, data = balanced_data, dist = "lnorm")
lnorm_marital2 <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                             ~ marital, data = balanced_data, dist = "lnorm")
lnorm_dm2 <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                        ~ dm_status, data = balanced_data, dist = "lnorm")
lnorm_hpt2 <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                         ~ hpt_status, data = balanced_data, dist = "lnorm")
lnorm_chol2 <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                          ~ chol_status, data = balanced_data, dist = "lnorm")
lnorm_obese2 <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                           ~ obese_asian, data = balanced_data, dist = "lnorm")
lnorm_abdobese2 <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                              ~ abdominal_obesity, data = balanced_data, dist = "lnorm")
lnorm_smoker2 <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                            ~ current_smoker, data = balanced_data, dist = "lnorm")
lnorm_PA2 <- flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") 
                        ~ PA_cat, data = balanced_data, dist = "lnorm")


#### Baseline distribution

# paramtric distribution using flexsurg package

#a) Survival plot

par_fits_bal <- tibble(
  dist_param = c("exp", "weibull", "gompertz", "lognormal", "llogis"),
  Distributions = c("Exponential", "Weibull", "Gompertz", "Log Normal", "Log-logistic")
) %>%
  mutate(
    fit = map(dist_param, ~flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") ~ 1, data = balanced_data, dist = .x)),
    fit_smry = map(fit, ~summary(.x, ci = FALSE, tidy = TRUE)),
    AIC = map_dbl(fit, ~.x$AIC)
  )

plot_surv2 <- par_fits_bal %>%
  select(-c(dist_param, fit)) %>%
  unnest(fit_smry) %>%
  ggplot(aes(x = time, y = est, color = Distributions, linetype =Distributions)) +
  geom_line(size = 0.8) +
  theme_classic2() +
  xlab("Time (year)") + ylab("Survival") + 
  labs(title = "(a) Parametric survival distribution using balance data") 



#b) Hazard plot

par_fits2_bal <- tibble(
  dist_param = c("exp", "weibull", "gompertz", "lognormal", "llogis"),
  Distributions = c("Exponential", "Weibull", "Gompertz", "Log Normal", "Log-logistic")
) %>%
  mutate(
    fit = map(dist_param, ~flexsurvreg(Surv(survival_time, status_pmcvd == "premature cvd death") ~ 1, data = balanced_data, dist = .x)),
    fit_smry = map(fit, ~summary(.x, type = "hazard", ci = FALSE, tidy = TRUE)),
    AIC = map_dbl(fit, ~.x$AIC)
  )

plot_haz2 <- par_fits2_bal %>%
  select(-c(dist_param, fit)) %>%
  unnest(fit_smry) %>%
  ggplot(aes(x = time, y = est, color = Distributions, linetype =Distributions)) +
  geom_line(size = 0.8) +  
  theme_classic2() + 
  xlab("Time (year)") + ylab("Hazard") + 
  labs(title = "(b) Parametric hazard distribution using balance data")

# combine plot

library(gridExtra)
combined_image <- grid.arrange(plot_surv, plot_haz, ncol = 1)

# compare AIC 
par_fits2_bal %>%
  arrange(AIC) %>%
  select(Distributions, AIC)

# Add a note AIC value at the bottom
library(gridExtra)
library(grid)

caption2 <- textGrob("[AIC values: Exponential (9707), Gompertz (9675), Log-logistic (9664), Log Normal (9680), Weibull (9663)]",
                    gp = gpar(fontsize = 10))

# Combine the plots and the caption using grid.arrange()
combined_plot2 <- grid.arrange(arrangeGrob(plot_surv2, plot_haz2, ncol = 1), 
                              caption, ncol = 1, heights = c(0.9, 0.1))






















######################################   Incidence rate #####################################################


####### Calculate using R


# Load required packages
# install.packages("survival") 
library(survival)

follow_up_years <- pmcvd$survival_time

pmcvd <- pmcvd %>% 
  mutate(pm_death = ifelse(status_pmcvd == "premature cvd death", 1, 0))

premature_death <- pmcvd$pm_death

# Create a survival object
surv_data <- Surv(time = follow_up_years, event = premature_death)

# Fit a Kaplan-Meier survival curve
km_fit <- survfit(surv_data ~ 1)

# Calculate the incidence rate per 1000 person-years
total_person_years <- sum(follow_up_years, na.rm = TRUE)
total_premature_deaths <- sum(premature_death)

incidence_rate_per_1000_person_years <- (total_premature_deaths / total_person_years) * 1000
incidence_rate_per_100000_person_years <- (total_premature_deaths / total_person_years) * 100000

# Print the results
cat("Incidence rate of premature CVD mortality per 1000 person-years:", incidence_rate_per_1000_person_years, "\n")
cat("Incidence rate of premature CVD mortality per 100,000 person-years:", incidence_rate_per_100000_person_years, "\n")





##### age standardized incident rate - to manual calculate using excel

# Install and load the required packages
install.packages("writexl")
library(writexl)


#calculate total person year

personYear <- pmcvd %>% 
  mutate(age_group = cut(age_entry, breaks = c(17, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, Inf), right = FALSE)) %>%
  group_by(age_group) %>%
  summarise(person_years = sum(survival_time)) %>%
  ungroup()

num_PMCVD <- pmcvd %>% 
  mutate(age_group_death = cut(age_at_death, breaks = c(30, 35, 40, 45, 50, 55, 60, 65, Inf), right = FALSE)) %>%
  group_by(age_group_death) %>%
  summarise(incident_cases = sum(status_pmcvd == "premature cvd death")) %>%
  ungroup()

# Save the data to an Excel file
write_xlsx(personYear, path = "Total_person_year.xlsx")
write_xlsx(num_PMCVD, path = "Total_incidence_pmcvd.xlsx")

# to check missing value for age group 60-65
person_years_60_65 <- sum(pmcvd$survival_time[pmcvd$age_entry >= 60 & pmcvd$age_entry < 65])
person_years_18_19 <- sum(pmcvd$survival_time[pmcvd$age_entry >= 18 & pmcvd$age_entry < 20])
person_years_20_24 <- sum(pmcvd$survival_time[pmcvd$age_entry >= 20 & pmcvd$age_entry < 25])
person_years_65_70 <- sum(pmcvd$survival_time[pmcvd$age_entry >= 65 & pmcvd$age_entry < 70])

# for person year - use manually calculation from excel 
# for number of death for each group, use aggregate data from R, person_year



# a) Male incidence rate 

# calculate person year and mortality case for male 


male_personYear <- pmcvd %>%
  filter(sex == "Male") %>%  # Filter only male individuals
  mutate(age_group = cut(age_entry, breaks = c(17, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, Inf), right = FALSE)) %>%
  group_by(age_group) %>%
  summarise(person_years = sum(survival_time)) %>%
  ungroup()

male_numPMCVD <- pmcvd %>%
  filter(sex == "Male") %>%  # Filter only male individuals
  mutate(age_group_death = cut(age_at_death, breaks = c(30, 35, 40, 45, 50, 55, 60, 65, Inf), right = FALSE)) %>%
  group_by(age_group_death) %>%
  summarise(incident_cases = sum(status_pmcvd == "premature cvd death")) %>%
  ungroup()

# Save the data to an Excel file
write_xlsx(male_personYear, path = "Male_person_year.xlsx")
write_xlsx(male_numPMCVD, path = "Male_incidenc_PMCVD.xlsx")


# b) Female incidence rate 

# calculate person year and mortality case for female 


female_personYear <- pmcvd %>%
  filter(sex == "Female") %>%  # Filter only male individuals
  mutate(age_group = cut(age_entry, breaks = c(17, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, Inf), right = FALSE)) %>%
  group_by(age_group) %>%
  summarise(person_years = sum(survival_time)) %>%
  ungroup()

female_numPMCVD <- pmcvd %>%
  filter(sex == "Female") %>%  # Filter only male individuals
  mutate(age_group_death = cut(age_at_death, breaks = c(30, 35, 40, 45, 50, 55, 60, 65, Inf), right = FALSE)) %>%
  group_by(age_group_death) %>%
  summarise(incident_cases = sum(status_pmcvd == "premature cvd death")) %>%
  ungroup()

# Save the data to an Excel file
write_xlsx(female_personYear, path = "Female_person_year.xlsx")
write_xlsx(female_numPMCVD, path = "Female_incidence_PMCVD.xlsx")




##################################### End Analaysis #############################################################
