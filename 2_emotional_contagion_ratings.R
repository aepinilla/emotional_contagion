#############################################################
## Calculate emotional contagion towards faces
## Andres Pinilla Palacios
## apinillapalacios@gmail.com - March 2019
#############################################################

##############################
# Load libraries
library(tidyverse)
##############################

##############################
# Define Function for calculating the Emotional Contagion towards faces
# nv = Negativity Video
# nf = Negativity Face
# pv = Positivity Video
# pf = Positivity Face
# av = Arousal Video
# af = Arousal Face

ec_equation <- function(nv, nf, pv, pf, av, af) {
  1 - ((sqrt((nv - nf)^2 +
               (pv - pf)^2 +
               (av - af)^2)) / (100 * sqrt(3)))
}

evaluations$emotional_contagion <- ec_equation(
  evaluations$negativity_video,
  evaluations$negativity_face,
  evaluations$positivity_video,
  evaluations$positivity_face,
  evaluations$arousal_video,
  evaluations$arousal_face
)

emotional_contagion_df <- evaluations %>%
  group_by(participant, induction, face_type) %>%
  summarise(emotional_contagion = mean(emotional_contagion))
##############################

##############################
# Outlier detection
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

outliers <- emotional_contagion_df %>% tibble::rownames_to_column(var="outlier") %>% group_by(induction) %>% mutate(is_outlier=ifelse(is_outlier(emotional_contagion), emotional_contagion, as.numeric(NA)))
outliers$outlier[which(is.na(outliers$is_outlier))] <- as.numeric(NA)

ggplot(outliers, aes(y=emotional_contagion, x=induction, color = face_type)) +
  geom_boxplot() +
  geom_text(aes(label=outlier),na.rm=TRUE,nudge_y=0.05)

# Filter Outliers
ecf <- emotional_contagion_df %>%
  filter(participant != 433) %>%
  filter(participant != 452) %>%
  group_by(participant, induction, face_type) %>%
  summarise(emotional_contagion = mean(emotional_contagion))
##############################

##############################
#Summary table of Emotional Contagion ratings
summary_ecf <- ecf %>%
  group_by(induction, face_type) %>%
  summarise(N = length(emotional_contagion/2),
            emotional_contagion_mean = mean(emotional_contagion),
            sd = sd(emotional_contagion))

# Calculate standard error
summary_ecf$se <- summary_ecf$sd / sqrt(summary_ecf$N)

# Calculate confidence intervals
conf.interval <- .95
ci_multiplier <- qt(conf.interval/2 + .5, summary_ecf$N-1)
summary_ecf$ci <- summary_ecf$se * ci_multiplier

# Error bars represent standard error of the mean
ggplot(summary_ecf, aes(x=induction, y=emotional_contagion_mean, fill=face_type)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  ylab("Mean emotional contagion") +
  xlab("Type of emotional Induction") +
  labs(fill = "Type of face") +
  ylim(0, .8) +
  scale_fill_grey(start = .4, end = .7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), text = element_text(size=16),
        axis.text.x = element_text(angle=0, hjust=.5, size = 16),
        axis.text.y = element_text(size=16)) +
  geom_errorbar(aes(ymin=emotional_contagion_mean-se, ymax=emotional_contagion_mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
