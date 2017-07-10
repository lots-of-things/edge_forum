library(dplyr)
library(ggplot2)
library(tidyr)
library(binom)
library(corrplot)

data_orig0 = read.csv('edge1.1.csv') %>% 
  select(-starts_with('dummy')) %>%
  filter(Live==0,ThreadId!=310) %>%
  mutate(Academic = ifelse(Academic=="1",1,ifelse(Academic=="0",0,NA)))

# input data_orig
source('edge_preprocessing_thread.R')
# output data_thread, thread_info, pop_discipline, pop_job

# input data_thread
source('edge_hypothesis1.R')


# input data_thread
source('edge_preprocessing_participant.R')
# output data_thread_participant, participant_info

## use thread_info, participant_info
# present = data_orig0 %>% 
#   group_by(ThreadId,Id) %>%
#   summarize(Present=TRUE)
# data_full = merge(thread_info,participant_info) %>% 
#   left_join(present,by=c('ThreadId','Id')) %>%
#   mutate(Present=!is.na(Present))


source('edge_analysis.R')