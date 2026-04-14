
# Combining and cleaning data sets

#Loads ----------------------------------
#Libraries
library(ipumsr)
library(readxl)
library(dplyr)
#Load IPUMS and excel data (IPUMS commented out for data conversion to r)
#ddi = read_ipums_ddi("cps_00004.xml")
#dfCTCraw = read_ipums_micro(ddi)
dfCTCraw = readRDS("dfCTCraw.rds")
excel_df = read_excel("fed_ctc_panel.xlsx")

# Cleaning CPS ASEC ---------------------
  #Remove irrelevant, garbage, or empty
  # 5 year migration status, medicare, and medicaid coverage have excess missing values and are not explicitly relevant
dfCTCraw$MIGSTA5 = NULL
dfCTCraw$HIMCARENW = NULL
dfCTCraw$HIMCAIDNW = NULL
dfCTCraw$HFLAG = NULL
  #N/A values for relevant variables for CTC, ACTC, EITC, gross income, and WIC
  #CTC and ACTC data go back only to 2005, WIC to 2001, and AGI and EITC to 1992. CTC and ACTC are main variables of interest. 
  #Limit df to 2005 - 2025
dfCTC2005 = dfCTCraw |>
  filter(YEAR >= 2005 & YEAR <= 2025)

# Merge excel data ----------------------
#Limit data to 2005 - 2025
excel2005 = excel_df |>
  filter(year >= 2005 & year <= 2025)
#Rename year, state, and FIPS
excel2005 = excel2005 |>
  rename(YEAR = year, STATEFIP = FIPS, STATE = state)
#Set STATEFIPS as integers
excel2005 = excel2005 |>
  mutate(STATEFIP = as.integer(STATEFIP))
dfCTC2005 = dfCTC2005 |>
  mutate(STATEFIP = as.integer(STATEFIP))
#Left join data frames
CTCdfs = dfCTC2005 |>
  left_join(excel2005, by = c("YEAR", "STATEFIP"))
#Missing 53431 values - statefip 11 DC
combinedCTC = CTCdfs |>
  filter(STATEFIP != 11)

#Enforce logical conditions -------------
#Arbitrary and illogical values for FOODSTMP, RACE, MARST, EMPSTAT, LABFORCE, EDUC, OFFPOV, MIGSTA1
logicCTC = combinedCTC |>
  mutate(
  # FOODSTMP values are currently 1:2 change to 0:1 for did not receive, received (no NIU values)
    FOODSTMP = ifelse(FOODSTMP == 2, 1, 0),
  #Generate binary race variable, not white
    not_white = case_when(
      RACE == 100 ~ 0,
      RACE != 100 & RACE != 999 ~ 1,
      TRUE ~ NA_real_),
  # Change numerical race values to titles
    RACE = case_when(
      RACE == 100 ~ "White",
      RACE == 200 ~ "Black",
      RACE == 300 ~ "American Indian/Aleut/Eskimo",
      RACE %in% c(650, 651, 652) ~ "Asian/Pacific Islander",
      RACE == 700 ~ "Other race",
      RACE >= 801 & RACE <= 830 ~ "Multiple races",
      RACE == 999 ~ "NIU"),
  # Generate binary marital status variable, married
  married = case_when(
    MARST %in% c(1,2) ~ 1,
    MARST %in% c(3,4,5,6,7) ~ 0,
    TRUE ~ NA_real_),
  #Set numerical values to marital status
    MARST = case_when(
      MARST == 1 ~ "Married, spouse present",
      MARST == 2 ~ "Married, spouse absent",
      MARST == 3 ~ "Separated",
      MARST == 4 ~ "Divorced",
      MARST == 5 ~ "Widowed",
      MARST == 6 ~ "Never married",
      MARST == 7 ~ "Widowed/Divorced",
      MARST == 9 ~ "NIU"),
  # Generate binary employment variable, employed
  employed = case_when(
    EMPSTAT %in% c(1,10,12) ~ 1,
    EMPSTAT %in% c(20,21,22,30,31,32,33,34,35,36) ~ 0,
    EMPSTAT == 0 ~ NA_real_, TRUE ~ 0),
  # For EMPSTAT only concerned with identifiers for employed, unemployed, and not in labor force
    EMPSTAT = case_when(
      EMPSTAT == 0 ~ "NIU",
      EMPSTAT %in% c(1,10,12) ~ "Employed",
      EMPSTAT %in% c(20,21,22) ~ "Unemployed",
      EMPSTAT %in% c(30,31,32,33,34,35,36) ~ "Not in labor force"),
  # Set LABFORCE to 0:1 binary, accounting for NIU values
    LABFORCE = case_when(
      LABFORCE == 0 ~ NA_real_,
      LABFORCE == 1 ~ 0,
      LABFORCE == 2 ~ 1),
  #Group EDUC identifiers to meaningful chunks of schooling
    EDUC = case_when(
      EDUC %in% c(0,1,2) ~ "No schooling",
      EDUC >= 10 & EDUC <= 72 ~ "Less than high school",
      EDUC == 73 ~ "High school diploma",
      EDUC %in% c(80,81,90,91,92,100) ~ "Some college/Associate",
      EDUC %in% c(110,111) ~ "Bachelor's degree",
      EDUC %in% c(120,121,122,123,124,125) ~ "Graduate degree",
      EDUC == 999 ~ "NIU"),
  # Conver OFFPOV to binary, considering NIU values
    OFFPOV = case_when(
      OFFPOV == 1 ~ 1,
      OFFPOV == 2 ~ 0,
      OFFPOV == 99 ~ NA_real_),
  # For MIGSTA1 just want to know if they migrated
    MIGSTA1 = case_when(
      MIGSTA1 == 99 ~ 0,
      MIGSTA1 %in% c(91) ~ 1,
      MIGSTA1 != 0 ~ 1,
      MIGSTA1 == 0 ~ NA_real_))

#Address NA/NIU Values-------------------
# 9999+ values for INCTOT, INCWELFR, CTCCRD, ACTCCRD, ADJGINC, EITCRED are NIU 
NACTC = logicCTC |>
  mutate(
    INCTOT   = ifelse(INCTOT   >= 999999999, NA_real_, INCTOT),
    INCWELFR = ifelse(INCWELFR >= 999999,    NA_real_, INCWELFR),
    CTCCRD   = ifelse(CTCCRD   >= 999999,    NA_real_, CTCCRD),
    ACTCCRD  = ifelse(ACTCCRD  >= 99999,     NA_real_, ACTCCRD),
    ADJGINC  = ifelse(ADJGINC  >= 99999999,  NA_real_, ADJGINC),
    EITCRED  = ifelse(EITCRED  >= 9999,      NA_real_, EITCRED),
    EMPSTAT = ifelse(EMPSTAT == "NIU", NA, EMPSTAT),)
#As CTCCRD, ACTCCRD, LABFORCE, and EMPSTAT/employment are the key variables of interest, these missing values will be dropped
#This also removes the missing values for MIGSTA1, OFFPOV, EITCRED, INCTOT, and INCWELFR
CTCvalues = NACTC |>
  filter(
    !is.na(CTCCRD),
    !is.na(ACTCCRD),
    !is.na(LABFORCE),
    !is.na(EMPSTAT),
    !is.na(employed))

#Rename Variables -----------------------
#Rename for clearer and more concise variable names (kept CPS id vars capitalized)
cleanCTC = CTCvalues |>
  rename(
    year = YEAR,
    month = MONTH,
    state_fips = STATEFIP,
    hh_income = HHINCOME,
    foodstamp = FOODSTMP,
    age = AGE,
    sex = SEX,
    race = RACE,
    marital_status = MARST,
    n_child = NCHILD,
    n_child_u5 = NCHLT5,
    eldest_child_age = ELDCH,
    emp_status = EMPSTAT,
    labor_force = LABFORCE,
    education = EDUC,
    family_income = FTOTVAL,
    total_income = INCTOT,
    welfare_income = INCWELFR,
    ctc = CTCCRD,
    actc = ACTCCRD,
    adj_gross_income = ADJGINC,
    eitc = EITCRED,
    poverty = OFFPOV,
    migrated = MIGSTA1,
    wic = GOTWIC,
    state = STATE,
    maxCTC = fedMAX,
    refund_threshold = refundthreshold,
    max_income_single = maxfedincS,
    max_income_mfj = maxfedincMFJ,
    refund_value = refundvalue
  )

#Additional Variables/Transformations -------------------
#Logs of incomes
#Using absolute values for negative values and plus one for zeroes
cleanCTC = cleanCTC |>
  mutate(
    log_hh_income = sign(hh_income) * log1p(abs(hh_income)),
    log_family_income = sign(family_income * log1p(abs(family_income))),
    log_total_income = sign(total_income * log1p(abs(total_income))),
    #Welfare income does not have negative values
    log_welfare_income = log(welfare_income +1)
  )
#Age squared
cleanCTC = cleanCTC |>
  mutate(age_sq = age^2)

#Save data ------------------------------
#Commented out for replication. If want to save: Choose wanted data format, select own document name, run without comment status
#write.csv(cleanCTC, "DAHWpCTC.csv", row.names = FALSE)
#save(cleanCTC, file = "DAHWpCTC.RData", compress = "xz")




