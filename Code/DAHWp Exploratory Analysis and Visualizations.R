
#Loads -------------------------------

library(readr)
library(skimr)
library(summarytools)
library(tidyverse)
library(gganimate)

#Load data
dfCTC = read_csv("DAHWpCTC.csv")

# Exploratory Analysis ---------------

#Key variables - Labor Status, Employment, CTC, and ACTC
#Distributions - Histograms 
ggplot(dfCTC, aes(ctc)) + geom_histogram()
ggplot(dfCTC, aes(actc)) + geom_histogram() #Reveals a majority of the sample is not receiving the CTC (2,586,164)

#Portion of sample that is eligible - has children in household
ggplot(dfCTC, aes(n_child)) + geom_histogram()
ggplot(dfCTC, aes(n_child_u5)) + geom_histogram()

#Consider distributions for women with children
#Filter data frame to only women with children
mothers = dfCTC |>
  filter(sex == 2, n_child > 0)

ggplot(mothers, aes(ctc)) + geom_histogram() # 522,485 mothers in sample don't receive CTC
freq(mothers$ctc)

#Observe the trends for women in sample that receive the CTC or ACTC
CTCmoms = dfCTC |>
  filter(sex == 2, n_child > 0, ctc > 0 | actc > 0)
freq(CTCmoms$ctc)
ggplot(CTCmoms, aes(x = year, y = ctc)) + geom_point()
ggplot(CTCmoms, aes(x = year, y = actc)) + geom_point()


#Histograms for mothers claiming the CTC/ACTC: Total claims and household income
ggplot(CTCmoms, aes(ctc)) + geom_histogram(boundary = 0)
ggplot(CTCmoms, aes(actc)) + geom_histogram(boundary = 0)
ggplot(CTCmoms, aes(hh_income)) + geom_histogram(boundary = 0)
ggplot(CTCmoms, aes(x = log(hh_income + 1))) +
  geom_histogram(bins = 50)

#How does the CTC and ACTC vary over time and state
#Averages over time
dfCTC |>
  group_by(year) |>
  summarize(avg_ctc = mean(ctc, na.rm = TRUE)) |>
  ggplot(aes(year, avg_ctc)) +
  geom_line()
CTCmoms |>
  group_by(year) |>
  summarize(avg_ctc = mean(ctc, na.rm = TRUE)) |>
  ggplot(aes(year, avg_ctc)) +
  geom_line()

dfCTC |>
  group_by(year) |>
  summarize(avg_actc = mean(actc, na.rm = TRUE)) |>
  ggplot(aes(year, avg_actc)) +
  geom_line()
CTCmoms |>
  group_by(year) |>
  summarize(avg_actc = mean(actc, na.rm = TRUE)) |>
  ggplot(aes(year, avg_actc)) +
  geom_line()

#Averages by state
dfCTC |>
  group_by(state) |>
  summarize(avg_ctc = mean(ctc, na.rm = TRUE)) |>
  ggplot(aes(x = reorder(state, avg_ctc), y = avg_ctc)) +
  geom_col() +
  coord_flip()
CTCmoms |>
  group_by(state) |>
  summarize(avg_ctc = mean(ctc, na.rm = TRUE)) |>
  ggplot(aes(x = reorder(state, avg_ctc), y = avg_ctc)) +
  geom_col() +
  coord_flip()

dfCTC|>
  group_by(state) |>
  summarize(avg_actc = mean(actc, na.rm = TRUE)) |>
  ggplot(aes(x = reorder(state, avg_actc), y = avg_actc)) +
  geom_col() +
  coord_flip()
CTCmoms |>
  group_by(state) |>
  summarize(avg_actc = mean(actc, na.rm = TRUE)) |>
  ggplot(aes(x = reorder(state, avg_actc), y = avg_actc)) +
  geom_col() +
  coord_flip()

#Considering employment and labor status (Binary)

#Employed
ggplot(dfCTC, aes(x = employed)) + geom_histogram(binwidth=.5)
#Employed, over time
dfCTC |>
  group_by(year) |>
  summarize(rate_employed = mean(employed, na.rm = TRUE)) |>
  ggplot(aes(year, rate_employed)) +
  geom_line()
#Employment and CTC recipiency
dfCTC |>
  mutate(recieved_credits = (ctc > 0 | actc > 0)) |>
  group_by(recieved_credits) |>
  summarize(rate_employed = mean(employed, na.rm = TRUE)) |>
  ggplot(aes(factor(recieved_credits), rate_employed)) +
  geom_col()
#Employed and children: total and children under 5
dfCTC |>
  group_by(n_child) |>
  summarize(rate_employed = mean(employed, na.rm = TRUE)) |>
  ggplot(aes(n_child, rate_employed)) +
  geom_line()
dfCTC |>
  group_by(n_child_u5) |>
  summarize(rate_employed = mean(employed, na.rm = TRUE)) |>
  ggplot(aes(n_child_u5, rate_employed)) +
  geom_line()
#Employed, mothers receiving CTC/ACTC
ggplot(CTCmoms, aes(x = employed)) + geom_histogram(binwidth=.5)
#Employed, mothers receiving CTC/ACTC, over time
CTCmoms |>
  group_by(year) |>
  summarize(rate_employed = mean(employed, na.rm = TRUE)) |>
  ggplot(aes(year, rate_employed)) +
  geom_line()

#Labor Status (In labor force)
#Structure of sample in labor force
ggplot(dfCTC, aes(x = labor_force)) + geom_histogram(binwidth=.5)
#Sample in labor force over time
dfCTC |>
  group_by(year) |>
  summarize(lfpr = mean(labor_force, na.rm = TRUE)) |>
  ggplot(aes(year, lfpr)) +
  geom_line()

#Employment Status (character)
#Total of sample employed, unemployed, or not in labor force
ggplot(dfCTC, aes(x=emp_status)) + geom_bar()
#Employment status over time
ggplot(dfCTC, aes(x = year, fill = emp_status)) + geom_bar(position = "fill")

#Local reg for employment rate and average CTC for those recieving the credit
dfCTC |>
  mutate(recieved_credits = (ctc > 0 | actc > 0),
         total_ctc = ctc + actc) |>
  filter(total_ctc > 0) |>
  mutate(bin = ntile(total_ctc, 40)) |>
  group_by(bin) |>
  summarize(
    ctc_avg = mean(total_ctc, na.rm = TRUE),
    emp_rate = mean(employed, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ggplot(aes(x = ctc_avg, y = emp_rate)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Employment vs CTC Amount")

# Visualizations ---------------------

#Employment trends for women with children when receiving the CTC vs not receiving CTC
dfCTC |>
  filter(sex == 2, n_child > 0) |>
  mutate(ctc_any = (ctc > 0 | actc > 0)) |>
  group_by(year, ctc_any) |>
  summarize(emp_rate = mean(employed, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = year, y = emp_rate, color = ctc_any)) +
  geom_line(size = 1.2) +
  labs(title = "Employment Trends for Women with Children",
       subtitle = "By CTC/ACTC Receipt",
       x = "Year",
       y = "Employment Rate",
       color = "Receives CTC") +
  scale_color_manual(values = c("lightgreen", "red"),
                     labels = c("Yes","No")) +
  theme_minimal()

#Employment rates and maximum CTC values
#For entire sample
dfCTC |>
  group_by(year) |>
  summarize(
    emp_rate = mean(employed == 1, na.rm = TRUE),
    avg_ctc = mean(maxCTC, na.rm = TRUE)
  ) |>
ggplot(aes(x = year)) +
  geom_line(aes(y = emp_rate, color = "Employment Rate")) +
  geom_line(aes(y = avg_ctc / max(avg_ctc), color = "CTC (scaled)")) +
  labs(title = "Employment and CTC Generosity",
       y = "Scaled Values", color = "") +
  theme_minimal()
#For moms receiving CTC
CTCmoms |>
  group_by(year) |>
  summarize(
    emp_rate = mean(employed == 1, na.rm = TRUE),
    avg_ctc = mean(maxCTC, na.rm = TRUE)
  ) |>
ggplot(aes(x = year)) +
  geom_line(aes(y = emp_rate, color = "Recipient Mothers Employment Rate")) +
  geom_line(aes(y = avg_ctc / max(avg_ctc), color = "CTC (scaled)")) +
  labs(title = "Employment and CTC Generosity for Mothers Recieving the CTC",
       y = "Scaled Values", color = "") +
  theme_minimal()

#Employment changes surrounding recent CTC expansions
#For entire sample
dfCTC |>
  group_by(year) |>
  summarize(
    emp_rate = mean(employed == 1, na.rm = TRUE),
    avg_ctc = mean(maxCTC, na.rm = TRUE)
  ) |>
ggplot (aes(x = year, y = emp_rate)) +
  geom_line() + geom_vline(aes(xintercept = 2018, color = "TCJA"),
             linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = 2021, color = "ARPA"),
             linetype = "dashed", size = 1) +
   scale_color_manual(values = c("TCJA" = "darkred",
                                "ARPA" = "lightblue"),
                     name = "Policy Change") +
   labs(title = "Employment Trends Around CTC Expansions", 
        y = "Employment Rate") +
  theme_minimal() +
  theme(legend.position = "right")
#For mothers receiving credit
CTCmoms |>
  group_by(year) |>
  summarize(
    emp_rate = mean(employed == 1, na.rm = TRUE),
    avg_ctc = mean(maxCTC, na.rm = TRUE)
  ) |>
  ggplot (aes(x = year, y = emp_rate)) +
  geom_line() + geom_vline(aes(xintercept = 2018, color = "TCJA"),
                           linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = 2021, color = "ARPA"),
             linetype = "dashed", size = 1) +
  scale_color_manual(values = c("TCJA" = "darkred",
                                "ARPA" = "lightblue"),
                     name = "Policy Change") +
  labs(title = "Employment Trends Around CTC Expansions", 
       y = "Employment Rate") +
  theme_minimal() +
  theme(legend.position = "right")


