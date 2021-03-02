#############################################################
## Conduct ANOVA and corresponding assumptions tests
## Andres Pinilla Palacios
## apinillapalacios@gmail.com - March 2019
#############################################################

##############################
# Load libraries
library(tidyverse)
library(sjstats)
library(car)
##############################

##############################
#Assumptions check
# Normality
negVideo_angryFace <- ecf %>%
  filter(induction == 'Negative',
         face_type == 'Angry')

posVideo_angryFace <- ecf %>%
  filter(induction == 'Positive',
         face_type == 'Angry')

negVideo_happyFace <- ecf %>%
  filter(induction == 'Negative',
         face_type == 'Happy')

posVideo_happyFace <- ecf %>%
  filter(induction == 'Positive',
         face_type == 'Happy')

shapiro.test(negVideo_angryFace$emotional_contagion)
shapiro.test(posVideo_angryFace$emotional_contagion)
shapiro.test(negVideo_happyFace$emotional_contagion)
shapiro.test(posVideo_happyFace$emotional_contagion)

# Homogeneity of variances
leveneTest(emotional_contagion ~ induction*face_type, data = ecf, center = mean) 

##############################
# Conduct ANOVA
summary(ecf)
ecf$participant <- factor(ecf$participant)

# Conduct ANOVA
aov_ecf <- aov(emotional_contagion ~ induction*face_type + Error(participant/face_type), data=ecf)
summary(aov_ecf)

eta_sq(aov_ecf, partial = T)
##############################

##############################
# Main effect of Induction Type
ecf_me_it <- ecf %>%
  group_by(participant, induction) %>%
  summarise(mean_ec = mean(emotional_contagion))
aov_me_it <- aov(mean_ec ~ induction, data = ecf_me_it)
summary(aov_me_it)
eta_sq(aov_me_it, partial = T)

# Main effect of Face Type
ecf_me_ft <- ecf %>%
  group_by(participant, face_type) %>%
  summarise(mean_ec = mean(emotional_contagion))
aov_me_ft <- aov(mean_ec ~ face_type, data = ecf_me_ft)
summary(aov_me_ft)
eta_sq(aov_me_ft, partial = T)

##############################
# Within groups effects
# Negative Group
ni_ecf <- ecf %>%
  filter(induction == "Negative")

face <- ni_ecf$face_type
emotional <- ni_ecf$emotional_contagion
pairwise.t.test(emotional, face, p.adj = "bonferroni")

# Positive Group
pi_ecf <- ecf %>%
  filter(induction == "Positive")

face <- pi_ecf$face_type
emotional <- pi_ecf$emotional_contagion
pairwise.t.test(emotional, face, p.adj = "bonferroni")
