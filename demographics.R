#############################################################
## Process demographics data
## Andres Pinilla Palacios
## apinillapalacios@gmail.com - March 2019
#############################################################

##############################
# Load libraries
library(tidyverse)
##############################

##############################
# Change month format from string to number for compatibility with lubridate package
demographics$age_month <- as.character(as.character(demographics$age_month))
demographics$age_month <- revalue(demographics$age_month, c("    Enero     "=      1))
demographics$age_month <- revalue(demographics$age_month, c("    Febrero   "=      2))
demographics$age_month <- revalue(demographics$age_month, c("    Marzo    "=       3))
demographics$age_month <- revalue(demographics$age_month, c("    Abril    "=       4))
demographics$age_month <- revalue(demographics$age_month, c("    Mayo    "=        5))
demographics$age_month <- revalue(demographics$age_month, c("    Junio    "=       6))
demographics$age_month <- revalue(demographics$age_month, c("    Julio    "=       7))
demographics$age_month <- revalue(demographics$age_month, c("    Agosto    "=      8))
demographics$age_month <- revalue(demographics$age_month, c("    Septiembre    "=  9))
demographics$age_month <- revalue(demographics$age_month, c("    Octubre    "=     10))
demographics$age_month <- revalue(demographics$age_month, c("    Noviembre"=       11))
demographics$age_month <- revalue(demographics$age_month, c("    Diciembre"=       12))
##############################

##############################
# Calculate age of participants at the date the experiment was conducted
demographics$birthdate <- paste(demographics$age_day, demographics$age_month, demographics$age_year)
int_ages <- interval(dmy(demographics$birthdate), myd(demographics$experiment_date))
demographics$age <- time_length(int_ages, 'year')

mean_age = round(mean(demographics$age), digits = 2)
std_age = round(sd(demographics$age), digits = 2)
##############################

##############################
# Count number of participants per gender
participants_sex <- demographics %>%
  select(participant, sex_response)

colnames(participants_sex)[c(2)] <- c('gender')
##############################

