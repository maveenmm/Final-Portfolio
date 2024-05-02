library(readr)
library(tidyverse)
library(haven)
library(knitr)
library(tibble)
library(dplyr)
library(gapminder) 
library(kableExtra) #packages 

#FACTOR
FctWhen= function(...){ #Function for knitting 
  args = rlang::list2(...)
  rhs = map(args, rlang::f_rhs)
  cases = case_when(!!!args)
  exec(fct_relevel, cases, !!!rhs)}

#IMPORTED DATA
Arab_Barometer_Wave_6_Part_1_ENG_RELEASE <- read_csv("CB/Arab_Barometer_Wave_6_Part_1_ENG_RELEASE.csv") #Data from Arab Barometer 
Arab_Barometer_Wave_6_Part_2_ENG_RELEASE <- read_csv("CB/Arab_Barometer_Wave_6_Part_2_ENG_RELEASE.csv")
Arab_Barometer_Wave_6_Part_3_ENG_RELEASE <- read_csv("CB/Arab_Barometer_Wave_6_Part_3_ENG_RELEASE.csv")

#DATA
Arab1 = read_csv('Arab_Barometer_Wave_6_Part_1_ENG_RELEASE.csv') #Rename Data
Arab2 = read_csv('Arab_Barometer_Wave_6_Part_2_ENG_RELEASE.csv')
Arab3 = read_csv('Arab_Barometer_Wave_6_Part_3_ENG_RELEASE.csv')


#DATA FILTERED BY COUNTRY 
filtered_Arab1 = Arab1 |> 
  filter(COUNTRY %in% c(1, 13, 21))
filtered_Arab2 = Arab2 |> 
  filter(COUNTRY %in% c(1, 13, 21))
filtered_Arab3 = Arab3 |> 
  filter(COUNTRY %in% c(1, 13, 21))
combined_dataset = bind_rows(filtered_Arab1, filtered_Arab2, filtered_Arab3) 

combined_dataset = replace(combined_dataset, combined_dataset == 98 | combined_dataset == 99, NA)

#COUNTRY VARIABLE 
combined_dataset = combined_dataset |> 
  rename(c_name = COUNTRY) |> 
  mutate(c_name = case_when(
    c_name == 1 ~ "Algeria",
    c_name == 13 ~ "Morocco",
    c_name == 21 ~ "Tunisia",
    TRUE ~ as.character(c_name))) #Filter for Algeria, Morocco, and Tunisia 

#RESPONDANT ID 
combined_dataset = combined_dataset |> 
  rename(r_id = ID)

#SURVEY WEIGHT 
combined_dataset = combined_dataset |> 
  rename(r_swt = WT) 

#ARABBAROMETER
combined_dataset = combined_dataset |> 
  mutate(source = "ArabBarometer")

#DATA FILTERED BY MONTH AND YEAR 
combined_dataset = combined_dataset |> 
  mutate(r_year = substr(`DATE`, 1, 4),   
         r_month = case_when(
           substr(DATE, 6, 7) == "03" ~ "March",
           substr(DATE, 6, 7) == "04" ~ "April",
           substr(DATE, 6, 7) == "07" ~ "July",
           substr(DATE, 6, 7) == "08" ~ "August",
           substr(DATE, 6, 7) == "09" ~ "September",
           substr(DATE, 6, 7) == "10" ~ "October"
         ))

#FEMALE INDICATOR 
combined_dataset = combined_dataset |> 
  rename(r_female2 = Q1002)

labels = c("Female", "Not Female") #manually insert labels 
combined_dataset <- combined_dataset |> 
  mutate(r_female = recode(r_female2, `1` = labels[1], `2` = labels[2]))

#POLITCAL IDEAOLOGY 
values = c(1, 2, 3, 4)
labels = c("US", "Russia", "China", "UK") #manually insert labels 
combined_dataset <- combined_dataset |> 
  rename(p_ideology = Q14COVID19) 

combined_dataset$p_ideology <- ifelse(combined_dataset$p_ideology == 1, labels[1],
                               ifelse(combined_dataset$p_ideology == 2, labels[2],
                               ifelse(combined_dataset$p_ideology == 3, labels[3],
                               ifelse(combined_dataset$p_ideology == 4, labels[4], NA))))



#POLITICAL INTEREST
labels = c("Throughout the day", #manually insert labels 
           "At least once daily", 
           "Several times a week", 
           "Once a week", 
           "Less than once a week", 
           "I do not use the Internet")
combined_dataset <- combined_dataset %>%
  mutate(p_interest = coalesce(Q409, Q409_NEW))

combined_dataset$p_interest <- ifelse(combined_dataset$p_interest == 1, labels[1],
                               ifelse(combined_dataset$p_interest == 2, labels[2],
                               ifelse(combined_dataset$p_interest == 3, labels[3],
                               ifelse(combined_dataset$p_interest == 4, labels[4],
                               ifelse(combined_dataset$p_interest == 5, labels[5],
                               ifelse(combined_dataset$p_interest == 6, labels[6], NA))))))


#EVALUATION OF NATURAL ECONOMEY 
labels = c("Very Good","Good","Bad", "Very Bad") #manually insert labels 
combined_dataset <- combined_dataset %>%
  rename(p_economy = Q101)

combined_dataset$p_economy <- ifelse(combined_dataset$p_economy == 1, labels[1],
                              ifelse(combined_dataset$p_economy == 2, labels[2],
                              ifelse(combined_dataset$p_economy == 3, labels[3],
                              ifelse(combined_dataset$p_economy == 4, labels[4], NA))))

#CORE VALUES
labels = c("For people like me, it doesn't matter what kind of government we have",
           "Under some circumstances, a non-democratic government can be preferable",
           "Democracy is always preferable to any other kind of government") #manually insert labels 
combined_dataset <- combined_dataset %>%
rename(core_values = Q516A)  
  
combined_dataset$core_values <- ifelse(combined_dataset$core_values == 1, labels[1],
                               ifelse(combined_dataset$core_values == 2, labels[2],
                               ifelse(combined_dataset$core_values == 3, labels[3],
                               ifelse(combined_dataset$core_values == 4, labels[4], NA))))

#ESSENTIAL DEMOCRACY
labels <- c("Absolutely Essential", "Somewhat Essential", "Not Very Essential", "Not at all Essential")
combined_dataset <- combined_dataset %>%
  mutate(d_essential = coalesce(Q512A3_1, Q512A3_2, Q512A3_5))

combined_dataset$d_essential <- ifelse(combined_dataset$d_essential == 1, labels[1],
                                ifelse(combined_dataset$d_essential == 2, labels[2],
                                ifelse(combined_dataset$d_essential == 3, labels[3],
                                ifelse(combined_dataset$d_essential == 4, labels[4], NA))))


#COUNTRY GOVERNENCE
labels <- c("Strongly Agree", "Somewhat Agree", "Somewhat Disagree", "Strongly Disagree")
combined_dataset <- combined_dataset %>%
  mutate(c_govern = coalesce(Q533_4, Q533_5))

combined_dataset$c_govern <- ifelse(combined_dataset$c_govern == 1, labels[1],
                             ifelse(combined_dataset$c_govern == 2, labels[2],
                             ifelse(combined_dataset$c_govern == 3, labels[3],
                             ifelse(combined_dataset$c_govern == 4, labels[4], NA))))


#INSTITUTIONAL TRUST
labels = c("A great deal of trust", "Quite a lot of trust", "Not a lot of trust", "No trust at all") #manually insert labels 
combined_dataset <- combined_dataset %>%
  mutate(i_trust = coalesce(Q201A_1, Q201A_2, Q201A_6A_LIB, Q201A_6B_LIB))

combined_dataset$i_trust <- ifelse(combined_dataset$i_trust == 1, labels[1],
                            ifelse(combined_dataset$i_trust == 2, labels[2],
                            ifelse(combined_dataset$i_trust == 3, labels[3],
                            ifelse(combined_dataset$i_trust == 4, labels[4], NA))))

#FINAL DATA
combined_dataset = combined_dataset |> 
  select(c_name, source, r_id, r_year, r_month, r_swt, r_female, p_ideology, p_interest, p_economy, d_essential, core_values, c_govern, i_trust)
view(combined_dataset) #new dataset


#FINAL DATA IN CSV
write.csv(combined_dataset, "dataset.csv", row.names = FALSE)
read_csv("dataset.csv")
