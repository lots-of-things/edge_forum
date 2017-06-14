## Measurements at Participant Level

# Getting text metrics per participant (eg how many words do they use on average)
participant_text = data_thread %>% 
  group_by(Id) %>% 
  summarise_at(.cols = 69:150,
               .funs = c(Min="min",Max="max",Mean="mean")) 
names(participant_text) = paste0('Participant_',names(participant_text))

# Getting thread level metrics per participant (eg What is the FemaleParticipation of the threads they frequent)
participant_thread = data_thread %>% 
  group_by(ThreadId,Id) %>%
  arrange(desc(PreviousContributions)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  group_by(Id) %>% 
  summarise_at(.cols = vars(DebateSize,FemaleParticipation,UniqueContributors,UniqueFemaleParticipation,starts_with('Thread_Discipline'),starts_with('Thread_Job')),
               .funs = c(Min="min",Max="max",Mean="mean")) 
names(participant_thread) = paste0('Participant_',names(participant_thread))

data_thread_participant = data_thread %>% 
  left_join(participant_thread, by =c('Id'='Participant_Id')) %>%
  left_join(participant_text, by =c('Id'='Participant_Id'))

participant_info = data_thread_participant %>% 
  group_by(Id) %>%
  arrange(desc(PreviousContributions)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  select(16:41, -18, -40, starts_with('Participant_'))
