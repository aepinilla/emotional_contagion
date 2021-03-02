###################################################################
## Analyse the effect of gender using a Linear Mixed Effects Model
## Andres Pinilla Palacios
## apinillapalacios@gmail.com - March 2019
###################################################################

##############################
# Load libraries
library(tidyverse)
library(lme4)
##############################

##############################
# Identify participants' gender
participants_sex <- demographics %>%
  select(participant, sex_response)
colnames(participants_sex)[c(2)] <- c('gender')

# Create dataframe with participants' gender
emotional_contagion_gender <- merge(x=emotional_contagion_df,y=participants_sex,by="participant",all=TRUE)
levels(emotional_contagion_gender$gender)[levels(emotional_contagion_gender$gender) == "Hombre"] <- 'M'
levels(emotional_contagion_gender$gender)[levels(emotional_contagion_gender$gender) == "Mujer"] <- 'F'

# Filter Outliers previously identified (see file "2_emotional_contagion_estimates")
ecf_gender <- emotional_contagion_gender %>%
  filter(participant != 433) %>%
  filter(participant != 452) %>%
  group_by(participant, induction, face_type, gender) %>%
  summarise(emotional_contagion = mean(emotional_contagion))

##############################
# Linear mixed effects
# Create the model
ec.model <- lmer(emotional_contagion ~ induction + gender + (1|participant) + (1|face_type), data = ecf_gender, REML = FALSE)
summary(ec.model)
# Create the null model (does not contain the effect of gender)
ec.null <- lmer(emotional_contagion ~ induction + (1|participant) + (1|face_type), data = ecf_gender, REML = FALSE)
# Compare both models
anova(ec.null, ec.model)
