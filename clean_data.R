library(tidyverse)

## Import data
file_path <- "experiment_data.csv"
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
