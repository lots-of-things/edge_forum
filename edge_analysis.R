
# data_orig0 raw data (dropped live info)
# data_orig data with thread info
# thread_info aggregated to thread
# participant_info aggregated to participant level

ggplot(thread_level,aes(Thread_OverrepresentedJob))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(participant_job,aes(Job_Title_S,Fraction))+
  geom_bar(stat='identity')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(participant_info,aes(DebateSize_Mean,FemaleParticipation_Mean))+
  geom_line()+
  facet_grid(Discipline ~ .)