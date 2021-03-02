###################################################################
## Analyse the effect of gender using a Linear Mixed Effects Model
## Andres Pinilla Palacios
## apinillapalacios@gmail.com - March 2019
###################################################################

##############################
# Load libraries
library(tidyverse)
library(sjstats)
library(car)
##############################

##############################
# Identify participants' gender
participants_sex <- demographics %>%
  select(participant, sex_response)
colnames(participants_sex)[c(2)] <- c('gender')

# Linear mixed effects model
boxplot(emotional_contagion ~ induction*face_type,
        col=c("white","lightgray"), ecf)

ec.model <- lmer(emotional_contagion ~ induction + gender + (1|participant) + (1|face_type), data = ecf, REML = FALSE)
ec.null <- lmer(emotional_contagion ~ induction + (1|participant) + (1|face_type), data = ecf, REML = FALSE)
summary(ec.model)
isSingular(ec.model, tol = 1e-05)

anova(ec.null, ec.model)