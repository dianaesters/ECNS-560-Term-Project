
#Loads -------------------------

library(tidyverse)
library(fixest)
library(modelsummary)
library(data.table)
library(haven)
library(kableExtra)
library(did2s)
load("DAHWpCTC.RData")

#Basic Regression Analysis -----------

#Additional key variables for regression analysis
# Add log income values, value for total credit,
# Indicator for having children, numeric value for education,
# sex indicator, indicator for eligibility for the CTC

#Full Sample
regCTC = cleanCTC |>
  mutate(
    total_credit = ctc + actc,
    recieved_credit = ifelse(ctc > 0 | actc > 0, 1, 0),
    n_child = as.numeric(as.character(n_child)),
    has_children = ifelse(n_child > 0, 1, 0),
    log_hh_income = sign(hh_income) * log1p(abs(hh_income)),
    log_family_income = sign(family_income) * log1p(abs(family_income)),
    log_total_income = sign(total_income) * log1p(abs(total_income)),
    log_welfare_income = log1p(welfare_income),
    educ_lvl = case_when(
      education == "No schooling" ~ 0, 
      education == "Less than high school" ~ 1,
      education == "High school diploma" ~ 2,
      education == "Some college/Associate" ~ 3,
      education == "Bachelor's degree" ~ 4,
      education == "Graduate degree" ~ 5
    ),
    eligible = ifelse(
      n_child > 0 & 
        ((married == 0 & total_income <= max_income_single) |
           (married == 1 & total_income <= max_income_mfj)), 1,0
    ),
    female = ifelse(sex == 2, 1, 0),
    country = "US"
  )

#Only women aged 18 to 65
workingwomen = regCTC |>
  filter(female == 1, age >= 18 & age <= 65)

#Binary treatment indicator: Received CTC
#Employment Full Sample
recieved_full_emp = feols(employed ~ recieved_credit + log_welfare_income + n_child + 
        n_child_u5 + married + age + age^2 + female + educ_lvl + not_white |
        state + year,
      data = regCTC,
      cluster = ~state
)
        
#Employment Working Women
recieved_women_emp = feols(employed ~ recieved_credit + log_welfare_income + n_child + 
       n_child_u5 + married + age + age^2 + educ_lvl + not_white |
        state + year,
      data = workingwomen,
      cluster = ~state
)
recieved_women_emp

#Labor force participation full sample
recieved_full_lf = feols(labor_force ~ recieved_credit + log_welfare_income + n_child + 
        n_child_u5 + married + age + age^2 + female + educ_lvl + not_white |
        state + year,
      data = regCTC,
      cluster = ~state
)

#Labor force participation working women
recieved_women_lf = feols(labor_force ~ recieved_credit + log_welfare_income + n_child + 
       n_child_u5 + married + age + age^2 + educ_lvl + not_white |
        state + year,
      data = workingwomen,
      cluster = ~state
)

#Binary Indicator Treatment: Eligible for credit 
#Eligibility is a function marital status and children
#Employment Full Sample
#Exclude married and children
feols(employed ~ eligible + age + age^2 + female + 
        log_welfare_income + educ_lvl + not_white |
        state + year,
      data = regCTC,
      cluster = ~state
)
#Full controls
eligibility_full_emp = feols(employed ~ eligible + n_child + n_child_u5 + log_welfare_income
        + married + age + age^2 + female + educ_lvl + not_white |
        state + year,
      data = regCTC,
      cluster = ~state
)

#Employment Working Women
#Exclude children and married
feols(employed ~ eligible + log_welfare_income +
        age + age^2 + educ_lvl + not_white |
        state + year,
      data = workingwomen,
      cluster = ~state
)

#Full Controls
eligilibility_women_emp = feols(employed ~ eligible + log_welfare_income + n_child + 
        n_child_u5 + married + age + age^2 + educ_lvl + not_white |
        state + year,
      data = workingwomen,
      cluster = ~state
)

#Labor force participation full sample
#Exclude married and children
feols(labor_force ~ eligible + age + age^2 + female + 
        log_welfare_income + educ_lvl + not_white |
        state + year,
      data = regCTC,
      cluster = ~state
)
#Full controls
eligilibility_full_lf = feols(labor_force ~ eligible + n_child + n_child_u5 + log_welfare_income
     + married + age + age^2 + female + educ_lvl + not_white |
        state + year,
      data = regCTC,
      cluster = ~state
)

#Labor force participation working women
feols(labor_force ~ eligible + age + age^2 +
        log_welfare_income + educ_lvl + not_white |
        state + year,
      data = workingwomen,
      cluster = ~state
)
#Full controls
eligilibility_women_lf = feols(labor_force ~ eligible + n_child + n_child_u5 + log_welfare_income
      + married + age + age^2 + educ_lvl + not_white |
        state + year,
      data = workingwomen,
      cluster = ~state
)

#Continuous treatment indicator:total credit received
#Employment Full Sample
cont_full_emp = feols(employed ~ total_credit + log_welfare_income + n_child + 
        n_child_u5 + married + age + age^2 + female + educ_lvl + not_white |
        state + year,
      data = regCTC,
      cluster = ~state
)

#Employment Working Women
cont_women_emp = feols(employed ~ total_credit + log_welfare_income + n_child + 
        n_child_u5 + married + age + age^2 + educ_lvl + not_white |
        state + year,
      data = workingwomen,
      cluster = ~state
)

#Labor force participation full sample
cont_full_lf =feols(labor_force ~ total_credit + log_welfare_income + n_child + 
        n_child_u5 + married + age + age^2 + female + educ_lvl + not_white |
        state + year,
      data = regCTC,
      cluster = ~state
)

#Labor force participation working women
cont_women_lf = feols(labor_force ~ total_credit + log_welfare_income + n_child + 
        n_child_u5 + married + age + age^2 + educ_lvl + not_white |
        state + year,
      data = workingwomen,
      cluster = ~state
)

#CS DiD Analysis -----------------------------

#Build DiD df setup
didCTC = regCTC |>
  group_by(CPSIDV) |>
  mutate(
    first_eligible = ifelse(any(eligible == 1),
                            min(year[eligible == 1]),
                            0)
  ) |> ungroup()

didwomen = didCTC |>
  filter(female == 1, age >= 18 & age <= 65)

#Model without controls
#Employment outcome and eligibility treatment
DDno_controls_emp = did2s(
  data       = didwomen,
  yname      = "employed",
  first_stage = ~ 0 | state + year,
  second_stage = ~ i(first_eligible, ref = 0),
  treatment  = "eligible",
  cluster_var = "state"
)
summary(DDno_controls_emp)

#Model with controls
DDcontrols_emp = did2s(
  data       = didwomen,
  yname      = "employed",
  first_stage = ~ log_welfare_income + n_child + n_child_u5 +
    married + age + I(age^2) + educ_lvl + not_white | state + year,
  second_stage = ~ i(first_eligible, ref = 0),
  treatment  = "eligible",
  cluster_var = "state"
)
summary(DDcontrols_emp)

#Model with some controls (Excludes marriage and children)

DDsome_emp = did2s(
  data       = didwomen,
  yname      = "employed",
  first_stage = ~ log_welfare_income + age + I(age^2) + educ_lvl + not_white | 
    state + year,
  second_stage = ~ i(first_eligible, ref = 0),
  treatment  = "eligible",
  cluster_var = "state"
)
summary(DDsome_emp)


#Labor Force Participation and eligibility treatment
#No controls
DDno_controls_lf = did2s(
  data       = didwomen,
  yname      = "labor_force",
  first_stage = ~ 0 | state + year,
  second_stage = ~ i(first_eligible, ref = 0),
  treatment  = "eligible",
  cluster_var = "state"
)
summary(DDno_controls_lf)

#Full controls
DDcontrols_lf = did2s(
  data       = didwomen,
  yname      = "labor_force",
  first_stage = ~ log_welfare_income + n_child + n_child_u5 +
    married + age + I(age^2) + educ_lvl + not_white | state + year,
  second_stage = ~ i(first_eligible, ref = 0),
  treatment  = "eligible",
  cluster_var = "state"
)
summary(DDcontrols_lf)

#Exclude children and marriage
DDsome_lf = did2s(
  data       = didwomen,
  yname      = "labor_force",
  first_stage = ~ log_welfare_income + age + I(age^2) + educ_lvl + not_white | 
    state + year,
  second_stage = ~ i(first_eligible, ref = 0),
  treatment  = "eligible",
  cluster_var = "state"
)
summary(DDsome_lf)


















