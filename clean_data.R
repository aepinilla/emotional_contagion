library(plyr)
library(tidyverse)
library(tibble)
library(corrplot)
library(psych)
library(sjstats)
library(readxl)
library(car)
library(onewaytests)
library(lubridate)
library(ez)
library(lme4)

## Import data
file_path <- "all-data.csv"
all_data <- read.csv(file_path, header = TRUE, sep = ';')

# Remove unnecesary rows
data <- all_data %>%
  filter(trialcode != 'positive_video',
         trialcode != 'negative_video',
         trialcode != 'ITI',
         trialcode != 'negative_dimension_movie',
         trialcode != 'positive_dimension_movie',
         trialcode != 'arousal_dimension_movie',
         blockcode != 'emotional_arousal',
         blockcode != 'visto_escena')

colnames(data)

# Change column names
colnames(data)[c(2, 4, 6, 16, 17, 18, 19, 20, 21, 22, 30, 32, 34)] <- c('experiment_date',
                                                                        'participant',
                                                                        'question',
                                                                        'face_name',
                                                                        'negativity_face',
                                                                        'positivity_face',
                                                                        'arousal_face',
                                                                        'negativity_video',
                                                                        'positivity_video',
                                                                        'arousal_video',
                                                                        'age_year',
                                                                        'age_month',
                                                                        'age_day')

#Assign labels to categorical variables
data$induction <- factor(data$group, levels = c(1, 2), labels = c('Negative', 'Positive'))
data$face_type <- factor(data$values.emotion, levels = c('negative', 'positive'), labels = c('Angry', 'Happy'))

# Create emotional evaluations dataframe
evaluations <- data %>%
  select(participant,
         induction,
         face_type,
         face_name,
         negativity_face,
         positivity_face,
         arousal_face,
         negativity_video,
         positivity_video,
         arousal_video,
         blockcode) %>%
  filter(blockcode == 'CE') %>%
  distinct() %>%
  select(-blockcode)

# Create questionnaires dataframe
questionnaires <- data %>%
  select(participant,
         induction,
         blockcode,
         trialnum,
         question,
         response) %>%
  filter(blockcode == 'likertIRI' || 'likertBES' || 'likertCCP' || 'likertECE',
         blockcode != 'CE') %>%
  drop_na()

  colnames(questionnaires)[3] <- 'questionnaire'
  questionnaires$questionnaire <- as.character(questionnaires$questionnaire)
  questionnaires$questionnaire[questionnaires$questionnaire == 'likertIRI'] <- 'IRI'
  questionnaires$questionnaire[questionnaires$questionnaire == 'likertCCP'] <- 'CCP'
  questionnaires$questionnaire[questionnaires$questionnaire == 'likertBES'] <- 'BES'
  questionnaires$questionnaire[questionnaires$questionnaire == 'likertECE'] <- 'ECE'
  
  questionnaires$questionnaire <- factor(questionnaires$questionnaire,
                                         levels = c('IRI',
                                                    'BES',
                                                    'CCP',
                                                    'ECE'))

# Create demographics dataframe
demographics <- data %>%
  select(participant,
         induction,
         experiment_date,
         age_year,
         age_month,
         age_day,
         sex_response) %>%
  drop_na()

demographics2 <- read.csv('demographics.csv', header = TRUE, sep = ';')
int_ages <- interval(ymd(demographics2$birthdate), ymd(demographics2$date))
demographics2$age <- time_length(int_ages, 'year')

demographics <- demographics[order(demographics$participant),]
demographics2 <- demographics2[order(demographics2$participant),]

demographics$participant
round(mean(demographics2$age), digits = 2)
round(sd(demographics2$age), digits = 2)

participants_sex <- demographics2 %>%
  select(participant, sex_response)

colnames(participants_sex)[c(2)] <- c('gender')

# Create PANAS dataframe
panas <- data %>%
  select(participant,
         induction,
         q1_panas_P_response:q20_panas_N_response) %>%
  select(participant,
         induction,
         contains('response')) %>%
  drop_na()

#Print dataframes
glimpse(evaluations)
glimpse(questionnaires)
glimpse(demographics)
glimpse(panas)


##########
#Analysis
##########

# Function for calculating the Emotional Contagion rating
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

ec <- evaluations

ec$emotional_contagion <- ec_equation(
  ec$negativity_video,
  ec$negativity_face,
  ec$positivity_video,
  ec$positivity_face,
  ec$arousal_video,
  ec$arousal_face
)

ec2 <- ec %>%
  group_by(participant, induction, face_type) %>%
  summarise(emotional_contagion = mean(emotional_contagion))

ec3 <- merge(x=ec2,y=participants_sex,by="participant",all=TRUE)

#levels(ec3$induction)[levels(ec3$induction) == "Negative"] <- 1
#levels(ec3$induction)[levels(ec3$induction) == "Positive"] <- 2

#levels(ec3$face_type)[levels(ec3$face_type) == "Angry"] <- 1
#levels(ec3$face_type)[levels(ec3$face_type) == "Happy"] <- 2

levels(ec3$gender)[levels(ec3$gender) == "Hombre"] <- 'M'
levels(ec3$gender)[levels(ec3$gender) == "Mujer"] <- 'F'

# Outliers
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

outliers <- ec3 %>% tibble::rownames_to_column(var="outlier") %>% group_by(induction) %>% mutate(is_outlier=ifelse(is_outlier(emotional_contagion), emotional_contagion, as.numeric(NA)))
outliers$outlier[which(is.na(outliers$is_outlier))] <- as.numeric(NA)

ggplot(outliers, aes(y=emotional_contagion, x=induction, color = face_type)) +
         geom_boxplot() +
         geom_text(aes(label=outlier),na.rm=TRUE,nudge_y=0.05)

# Filter Outliers
ecf <- ec3 %>%
  filter(participant != 433) %>%
  filter(participant != 452) %>%
  group_by(participant, induction, face_type, gender) %>%
  summarise(emotional_contagion = mean(emotional_contagion))

#Summary table of Emotional Contagion ratings
summary_ecf <- ecf %>%
  group_by(induction, face_type) %>%
  summarise(N = length(emotional_contagion/2),
            emotional_contagion_mean = mean(emotional_contagion),
            sd = sd(emotional_contagion))

#Summary table of Emotional Contagion ratings
summary_ecf$se <- summary_ecf$sd / sqrt(summary_ecf$N)

# Confidence interval multiplier for standard error
# Calculate t-statistic for confidence interval: 
# e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
conf.interval <- .95
ciMult <- qt(conf.interval/2 + .5, summary_ecf$N-1)
summary_ecf$ci <- summary_ecf$se * ciMult

summary_ecf

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

#ANOVA
summary(ecf)
ecf$participant <- factor(ecf$participant)

aov_ecf <- aov(emotional_contagion ~ induction*face_type + Error(participant/face_type), data=ecf)
summary(aov_ecf)

aov_ecf2 <- aov(emotional_contagion ~ induction*face_type*sex + Error(participant/face_type), data=ecf)
summary(aov_ecf2)


eta_sq(aov_ecf, partial = T)

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

# Linear mixed effects model
boxplot(emotional_contagion ~ induction*face_type,
        col=c("white","lightgray"), ecf)

ec.null <- lmer(emotional_contagion ~ induction + (1|participant) + (1|face_type), data = ecf, REML = FALSE)
ec.model <- lmer(emotional_contagion ~ induction + gender + (1|participant) + (1|face_type), data = ecf, REML = FALSE)
summary(ec.model)
isSingular(ec.model, tol = 1e-05)

anova(ec.null, ec.model)

### Empathy Questionnaires
## Dataframe with questionnaires responses

qq_df <- questionnaires
qq_df$response <- as.numeric(as.character(qq_df$response))

# ECE
ece <- qq_df %>%
  filter(questionnaire == 'ECE') %>%
  select(participant, trialnum, response) %>%
  spread(trialnum, response) %>%
  arrange(participant)

# ECE uses a Likert scale from 1 to 4, where 1 means "Always" and 4 "Never" Therefore, all
# responses must be reverse coded, so that higher scores are equivalent tohigher emotional contagion.
num_questions_ece <- 18
keys_ece <- rep(-1, num_questions_ece) # Vector to recode responses. -1 is assigned to all the values of the vector because all of them must be recoded
recoded_ece <- reverse.code(keys_ece, ece, mini = 1, maxi = 4) # Recode answers

# IRI
iri <- qq_df %>%
  filter(questionnaire == 'IRI') %>%
  select(participant, trialnum, response) %>%
  spread(trialnum, response) %>%
  arrange(participant)

# Participants and answers are separated in two dataframes.
# This step is necessary when not all of the questions are reverse coded, otherwise R will return an error.
iri_participants <- iri %>%
  select(participant)
iri_answers <- iri %>%
  select(2:29)

# Reverse coding.
num_questions_iri <- 28
keys_iri <- rep(1, num_questions_iri) # Create vector to recode responses
keys_iri[c(3, 4, 7, 12, 13, 14, 15, 18, 19)] <- -1 # Assig -1 to questions that require reverse coding.
recoded_iri <- reverse.code(keys_iri, iri_answers, mini = 1, maxi = 5) # Recode answers
recoded_iri <- iri_participants %>% # Reunite participants columns with recoded answers
  cbind(recoded_iri)

## Calculate scores
# Calculate ECE score
ece_score <- recoded_ece %>%
  as_tibble() %>% # Convert to tibble to avoid errors
  gather(question, response, -participant) %>% # Restructure the dataframe
  group_by(participant) %>% # Group by participant's code
  summarise(ece_score = sum(response)) # Sum the responses from each participant

# Calculate IRI score
iri_score <- recoded_iri %>%
  gather(question, response, -participant) %>%
  group_by(participant) %>%
  summarise(iri_score = sum(response))

# Calculate BES score
bes_score <- qq_df %>%
  filter(questionnaire == 'BES') %>%
  select(participant, trialnum, response) %>%
  group_by(participant) %>%
  summarise(bes_score = sum(response))

# Calculate CCP score
ccp_score <- qq_df %>%
  filter(questionnaire == 'CCP') %>%
  select(participant, trialnum, response) %>%
  group_by(participant) %>%
  summarise(ccp_score = sum(response))

induction_type <- qq_df %>%
  select(participant, induction) %>%
  group_by(participant) %>%
  distinct()
induction_type

all_questionnaires <- induction_type %>%
  left_join(iri_score) %>%
  left_join(bes_score) %>%
  left_join(ccp_score) %>%
  left_join(ece_score) %>%
  arrange(participant)

all_questionnaires$participant <- factor(all_questionnaires$participant)

correlations <- all_questionnaires %>%
  left_join(ecf, by = "participant") %>%
  filter(participant != '433') %>%
  filter(participant != '452')

correlations

ec_iri_lm <- lm(iri_score ~ emotional_contagion, data = correlations)
summary(ec_iri_lm)

ggplot(correlations, aes(x = iri_score, y = emotional_contagion, color = face_type)) +
  geom_point() +
  xlab('IRI Score') +
  ylab('Mean Emotional Contagion') +
  geom_smooth(method = "lm", fill = NA)

ggplot(correlations, aes(x = bes_score, y = emotional_contagion, color = face_type)) +
  geom_point()

ggplot(correlations, aes(x = ccp_score, y = emotional_contagion, color = face_type)) +
  geom_point()

ggplot(correlations, aes(x = ece_score, y = emotional_contagion, color = face_type)) +
  geom_point() +
  xlab('ECS Score') +
  ylab('Mean Emotional Contagion') +
  geom_smooth(method = "lm", fill = NA)

##

correlations_ng_af <- correlations %>%
  filter(induction.x == "Negative",
         face_type == 'Angry')

correlations_ng_hf <- correlations %>%
  filter(induction.x == "Negative",
         face_type == 'Happy')

correlations_pg_af <- correlations %>%
  filter(induction.x == "Positive",
         face_type == 'Angry')

correlations_pg_hf <- correlations %>%
  filter(induction.x == "Positive",
         face_type == 'Happy')

ece_cor_ng_af <- cor.test(correlations_ng_af$ece_score, correlations_ng_af$emotional_contagion, method = "pearson")
ece_cor_ng_hf <- cor.test(correlations_ng_hf$ece_score, correlations_ng_hf$emotional_contagion, method = "pearson")
ece_cor_pg_af <- cor.test(correlations_pg_af$ece_score, correlations_pg_af$emotional_contagion, method = "pearson")
ece_cor_pg_hf <- cor.test(correlations_pg_hf$ece_score, correlations_pg_hf$emotional_contagion, method = "pearson")

ece_cor_ng_af
ece_cor_ng_hf #Significative
ece_cor_pg_af
ece_cor_pg_hf

iri_cor_ng_af <- cor.test(correlations_ng_af$iri_score, correlations_ng_af$emotional_contagion, method = "pearson")
iri_cor_ng_hf <- cor.test(correlations_ng_hf$iri_score, correlations_ng_hf$emotional_contagion, method = "pearson")
iri_cor_pg_af <- cor.test(correlations_pg_af$iri_score, correlations_pg_af$emotional_contagion, method = "pearson")
iri_cor_pg_hf <- cor.test(correlations_pg_hf$iri_score, correlations_pg_hf$emotional_contagion, method = "pearson")

iri_cor_ng_af
iri_cor_ng_hf #Significative
iri_cor_pg_af 
iri_cor_pg_hf

bes_cor_ng_af <- cor.test(correlations_ng_af$bes_score, correlations_ng_af$emotional_contagion, method = "pearson")
bes_cor_ng_hf <- cor.test(correlations_ng_hf$bes_score, correlations_ng_hf$emotional_contagion, method = "pearson")
bes_cor_pg_af <- cor.test(correlations_pg_af$bes_score, correlations_pg_af$emotional_contagion, method = "pearson")
bes_cor_pg_hf <- cor.test(correlations_pg_hf$bes_score, correlations_pg_hf$emotional_contagion, method = "pearson")

bes_cor_ng_af
bes_cor_ng_hf
bes_cor_pg_af 
bes_cor_pg_hf

ccp_cor_ng_af <- cor.test(correlations_ng_af$ccp_score, correlations_ng_af$emotional_contagion, method = "pearson")
ccp_cor_ng_hf <- cor.test(correlations_ng_hf$ccp_score, correlations_ng_hf$emotional_contagion, method = "pearson")
ccp_cor_pg_af <- cor.test(correlations_pg_af$ccp_score, correlations_pg_af$emotional_contagion, method = "pearson")
ccp_cor_pg_hf <- cor.test(correlations_pg_hf$ccp_score, correlations_pg_hf$emotional_contagion, method = "pearson")

ccp_cor_ng_af
ccp_cor_ng_hf
ccp_cor_pg_af 
ccp_cor_pg_hf


ggplot(correlations_ng_hf, aes(x = ece_score, y = emotional_contagion, color = face_type)) +
  geom_point() +
  xlab('ECS Score') +
  ylab('Mean Emotional Contagion in the Negative Group') +
  geom_smooth(method = "lm", fill = NA)

ggplot(correlations_ng_hf, aes(x = iri_score, y = emotional_contagion, color = face_type)) +
  geom_point() +
  xlab('IRI Score') +
  ylab('Mean Emotional Contagion in the Negative Group') +
  geom_smooth(method = "lm", fill = NA)


# Correlation matrix
# Table 1

ece_cor_ng_hf #Significative
  round(-0.4263748, digits = 2)
  round(0.01496, digits = 3)
iri_cor_ng_hf #Significative
  round(-0.39778, digits = 2)
  round(0.02416, digits = 3)
bes_cor_ng_hf
  round(-0.299399, digits = 2)
  round(0.09597, digits = 3)
ccp_cor_ng_hf
  round(-0.1862788, digits = 2)
  round(0.3074, digits = 3)

ece_cor_pg_hf
  round(-0.2816708, digits = 2)
  round(0.1012, digits = 3)
iri_cor_pg_hf
  round(-0.2132892, digits = 2)
  round(0.2186, digits = 3)
bes_cor_pg_hf
  round(-0.1048828, digits = 2)
  round(0.5488, digits = 3)
ccp_cor_pg_hf
  round(-0.232042, digits = 2)
  round(0.1798, digits = 3)

  
  
ece_cor_ng_af
  round(-0.2756561, digits = 2)
  round(0.1267, digits = 3)
iri_cor_ng_af
  round(-0.1778294, digits = 2)
  round(0.3302, digits = 3)
bes_cor_ng_af
  round(-0.2965362, digits = 2)
  round(0.09935, digits = 3)
ccp_cor_ng_af
  round(-0.2618594, digits = 2)
  round(0.1477, digits = 3)

ece_cor_pg_af
  round(0.01569749, digits = 2)
  round(0.9287, digits = 3)
iri_cor_pg_af
  round(-0.04229936, digits = 2)
  round(0.8093, digits = 3)
bes_cor_pg_af 
  round(0.1023228, digits = 2)
  round(0.5586, digits = 3)
ccp_cor_pg_af 
  round(-0.07746465, digits = 2)
  round(0.6583, digits = 3)
  
# Questionnaires correlations
  
  correlations2 <- correlations %>%
    select(participant, iri_score, bes_score, ccp_score, ece_score) %>%
    distinct()
    
  cor_ece_iri <- cor.test(correlations2$ece_score, correlations2$iri_score, method = "pearson")
  cor_ece_bes <- cor.test(correlations2$ece_score, correlations2$bes_score, method = "pearson")
  cor_ece_ccp <- cor.test(correlations2$ece_score, correlations2$ccp_score, method = "pearson")
  
  cor_ece_iri
    round(0.6986295, digits = 2)
    round(2.2e-16, digits = 3)
  cor_ece_bes
    round(0.7423143, digits = 2)
    round(2.2e-16, digits = 3)
  cor_ece_ccp
    round(0.4473431, digits = 2)
    round(4.762e-08, digits = 3)
  
  cor_iri_bes <- cor.test(correlations2$iri_score, correlations2$bes_score, method = "pearson")
  cor_iri_ccp <- cor.test(correlations2$iri_score, correlations2$ccp_score, method = "pearson")
  
  cor_iri_bes
    round(0.6005542, digits = 2)
    round(4.899e-08, digits = 3)
  cor_iri_ccp
    round(0.501409, digits = 2)
    round(1.141e-05, digits = 3)
    
    
  cor_bes_ccp <- cor.test(correlations2$bes_score, correlations2$ccp_score, method = "pearson")
  
  cor_bes_ccp  
    round(0.3341871, digits = 2)
    round(0.005009, digits = 3)
    
    
####
familiarity <- all_data %>%
  select(subject, group, blockcode, response) %>%
  filter(blockcode == "visto_escena")

# Change column names
colnames(familiarity)[c(1)] <- c('participant')
familiarity$induction <- factor(familiarity$group, levels = c(1, 2), labels = c('Negative', 'Positive'))
familiarity$is_familiar <- factor(familiarity$response, levels = c("SÃ_", "No"), labels = c("Yes", "No"))


names(familiarity)

familiarity_summary <- familiarity %>%
  group_by(induction, is_familiar) %>%
  summarise(count = length(is_familiar))

familiarity_summary


### PANAS

names(panas)

# Filter Outliers
panas_f <- panas %>%
  filter(participant != 433) %>%
  filter(participant != 452)

pa_questions <- panas_f %>%
  select(ends_with('P_response')) %>%
  gather(question, score) %>%
  select(question) %>%
  unique() %>%
  unlist()

na_questions <- panas_f %>%
  select(ends_with('N_response')) %>%
  gather(question, score) %>%
  select(question) %>%
  unique() %>%
  unlist()

panas2 <- panas_f %>%
  gather(question, score, -participant, -induction)
glimpse(panas2)

panas2$type <- NA
panas2$type[which(panas2$question %in% pa_questions)] <- 'pa'
panas2$type[which(panas2$question %in% na_questions)] <- 'na'

panas3 <- panas2 %>%
  group_by(participant, induction, type) %>%
  summarise(mean_score = mean(score)) %>%
  ungroup() %>%
  group_by(induction, type) %>%
  summarise(experiment_score = mean(mean_score), experiment_sd = sd(mean_score))

panas3

reported_panas_path <- 'film-level_scores.xls'
reported_panas <- read_excel(reported_panas_path) %>%
  filter(code == '69' | code == '15') %>%
  select('FILM SCENE', 'pa', 'na')

reported_panas2

reported_panas$induction <- NA
reported_panas$induction[reported_panas$`FILM SCENE` == 'American History X'] <- "Negative"
reported_panas$induction[reported_panas$`FILM SCENE` == 'The dead Poets Society (2)'] <- "Positive"

reported_panas2 <- reported_panas %>%
  gather(type, reported_score, -'FILM SCENE', -induction) %>%
  select(induction, type, reported_score)

panas4 <- panas3 %>%
  left_join(reported_panas2, by = c('induction', 'type')) %>%
  unique()
panas4


#Puntajes calculados acá no coinciden con los reportados en el FilmStim
original_panas_path <- 'data/panas/PANAS_means_SDs.xls'
original_panas <- read_excel(original_panas_path) %>%
  filter(Code == '69' | Code == '15') %>%
  select(Film, contains('mean')) %>%
  gather(word, score, -Film)

positive_words <- c('interested mean',
                    'distressed mean',
                    'excit mean',
                    'strong mean',
                    'enthusiastic mean',
                    'proud mean',
                    'inspired mean',
                    'determined mean',
                    'attentive mean',
                    'active mean')

negative_words <- c('upset mean',
                    'guilty mean',
                    'scared mean',
                    'hostile mean',
                    'irritable mean',
                    'alert mean',
                    'ashamed mean',
                    'nervous mean',
                    'jittery mean',
                    'afraid mean')

original_panas$type <- NA
original_panas$type[which(original_panas$word %in% positive_words)] <- 'pa'
original_panas$type[which(original_panas$word %in% negative_words)] <- 'na'

original_panas %>%
  group_by(Film, type) %>%
  summarise(mean_score = mean(score))

### Arousal Check FilmStim
arousal_check <- all_data %>%
  filter(blockcode == "emotional_arousal") %>%
  select(subject, group, trialcode, response) %>%
  filter(trialcode == "emotional_arousal")
  
# Change column names
colnames(arousal_check)[c(1)] <- c('participant')
arousal_check$induction <- factor(arousal_check$group, levels = c(1, 2), labels = c('Negative', 'Positive'))

# Filter Outliers
arousal_check_f <- arousal_check %>%
  filter(participant != 433) %>%
  filter(participant != 452)

arousal_check2 <- arousal_check_f %>%
  select(participant, induction, response)

arousal_check2$response2 <- as.numeric(paste(arousal_check2$response))

arousal_check3 <- arousal_check2 %>%
  group_by(induction) %>%
  summarise(mean_response = mean(response2))

arousal_check3

reported_arousal_path <- 'film-level_scores.xls'
reported_arousal <- read_excel(reported_panas_path) %>%
  filter(code == '69' | code == '15') %>%
  select('FILM SCENE', 'arousal')

arousal_check2

