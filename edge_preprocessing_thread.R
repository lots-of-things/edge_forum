## Measurements at thread level

pop_discipline = data_orig0 %>%
  group_by(Id) %>%
  arrange(desc(PreviousContributions)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  group_by(Discipline) %>%
  summarize(DisciplinePopSize = n()) %>%
  mutate(DisciplinePopSize = DisciplinePopSize/sum(DisciplinePopSize)) 
# Counting participants by discipline in each thread

thread_pop = data_orig0 %>%
  group_by(ThreadId,Id) %>%
  arrange(desc(PreviousContributions)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  group_by(ThreadId,Discipline) %>%
  summarize(n=n()) %>%
  left_join(pop_discipline, by='Discipline')

thread_overrep = thread_pop %>%
  group_by(ThreadId) %>%
  mutate(OverRep = binom.confint(n,sum(n),methods='wilson')$lower > DisciplinePopSize,
         Discipline_OverRep = paste0('OverRep_',Discipline)) %>%
  select(-n,-DisciplinePopSize,-Discipline) %>%
  spread(Discipline_OverRep,OverRep,fill=0) %>%
  ungroup()

thread_underrep = thread_pop %>%
  group_by(ThreadId) %>%
  mutate(UnderRep = binom.confint(n,sum(n),methods='wilson')$lower > DisciplinePopSize,
         Discipline_UnderRep = paste0('UnderRep_',Discipline)) %>%
  select(-n,-DisciplinePopSize,-Discipline) %>%
  spread(Discipline_UnderRep,UnderRep,fill=0) %>%
  ungroup()

thread_discipline = thread_overrep %>%
  left_join(thread_underrep, by='ThreadId')

# Counting participants by job in each thread
thread_job = data_orig0 %>%
  group_by(ThreadId,Id) %>%
  arrange(desc(PreviousContributions)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  group_by(ThreadId,Job_Title_S) %>%
  summarize(n=n()) %>%
  mutate(Job_Title_S = paste0('Thread_Job_',Job_Title_S)) %>%
  spread(Job_Title_S,n,fill=0)

# Summary stats for text of each thread
thread_text = data_orig0 %>%
  group_by(ThreadId) %>%
  summarise_at(.cols = 69:150,
               .funs = c(Min="min",Max="max",Mean="mean")) 
names(thread_text) = paste0('Thread_Text_',names(thread_text))

# Counting other participant metrics by thread
thread_other = data_orig0 %>%
  group_by(ThreadId,Id) %>%
  arrange(desc(PreviousContributions)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  group_by(ThreadId) %>%
  summarize(Thread_Academic = sum(Academic=='1', na.rm=TRUE),
            Thread_FemaleAcademic = sum((Academic=='1')*Female, na.rm=TRUE),
            Thread_Citations = sum(Total_Citations, na.rm=TRUE),
            Thread_FemaleCitations = sum(Total_Citations*Female, na.rm=TRUE),
            Thread_Contributions = sum(ContributionsThisYear+PreviousContributions, na.rm=TRUE),
            Thread_FemaleContributions = sum((ContributionsThisYear+PreviousContributions)*Female, na.rm=TRUE),
            Thread_Threads = sum((ThreadsThisYear+PreviousThreads), na.rm=TRUE),
            Thread_FemaleThreads = sum((ThreadsThisYear+PreviousThreads)*Female, na.rm=TRUE))

# Join thread data back to main table
# data_thread = data_orig0 %>% 
#   left_join(thread_discipline,by='ThreadId') %>%
#   left_join(thread_job, by= 'ThreadId') %>%
#   left_join(thread_other, by= 'ThreadId') %>%
#   mutate_at(.cols = vars(starts_with('Thread_')),
#             .funs = funs(./UniqueContributors)) %>%
#   left_join(thread_text, by=c('ThreadId'='Thread_Text_ThreadId'))

data_thread = data_orig0 %>% 
    left_join(thread_discipline,by='ThreadId') %>%
    left_join(thread_job, by= 'ThreadId') %>%
    left_join(thread_other, by= 'ThreadId') %>%
    left_join(thread_text, by=c('ThreadId'='Thread_Text_ThreadId'))

## Measurements at participant population level

# Counting participants by discipline overall
pop_discipline = data_orig0 %>%
  group_by(Id) %>%
  arrange(desc(PreviousContributions)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  group_by(Discipline) %>%
  summarize(DisciplinePopSize = n(),
            FractionFemale = sum(Female,na.rm=TRUE)/sum(Male + Female, na.rm=TRUE)) %>%
  mutate(DisciplinePopSize = DisciplinePopSize/sum(DisciplinePopSize)) 

ggplot(pop_discipline, aes(reorder(Discipline,FractionFemale),group=1)) + 
  geom_bar(aes(y=DisciplinePopSize),stat='identity') +
  geom_line(aes(y=FractionFemale)) +
  scale_y_continuous("Discipline Community", 
                     sec.axis = sec_axis(~ ., name = "Fraction Female")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# Counting participants by job overall
pop_job = data_orig0 %>%
  group_by(Id) %>%
  arrange(desc(PreviousContributions)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  mutate(Job_Title_S = ifelse(grepl('Entrepreneur|Management|Founder',Job_Title_S),'Management',
                       ifelse(grepl('Postdoctoral|Student', Job_Title_S),'Trainee',
                       ifelse(grepl('Other|Not Available', Job_Title_S) | Job_Title_S=='','Other',
                       Job_Title_S)))) %>%
  group_by(Job_Title_S) %>%
  summarize(JobPopSize = n(),
            FractionFemale = sum(Female,na.rm=TRUE)/sum(Male + Female, na.rm=TRUE)) %>%
  mutate(JobPopSize = JobPopSize/sum(JobPopSize)) 

ggplot(pop_job, aes(reorder(Job_Title_S,FractionFemale),group=1)) + 
  geom_bar(aes(y=JobPopSize),stat='identity') +
  geom_line(aes(y=FractionFemale)) +
  scale_y_continuous("Fraction with This Job", 
    sec.axis = sec_axis(~ ., name = "Fraction Female")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))


gender_uniquecontrib = data_orig0 %>%
  group_by(ThreadId,Id) %>%
  arrange(desc(PreviousContributions)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  mutate(Job_Title_S = ifelse(grepl('Entrepreneur|Management|Founder',Job_Title_S),'Management',
                              ifelse(grepl('Postdoctoral|Student', Job_Title_S),'Trainee',
                                     ifelse(grepl('Other|Not Available', Job_Title_S) | Job_Title_S=='','Other',
                                            Job_Title_S)))) %>%
  group_by(Id) %>%
  summarize(Female = mean(Female),
            Job_Title_S = max(Job_Title_S),
            Discipline = max(Discipline),
            TotalUniqueContributions = n(),
            YearsContributing = max(Year)-min(Year)) 

ggplot(filter(gender_uniquecontrib,Discipline==''),
       aes(YearsContributing,TotalUniqueContributions,color = (Female==1))) +
  geom_jitter()+ 
  geom_smooth()


ggplot(filter(gender_uniquecontrib,Discipline!=''),aes(YearsContributing,TotalUniqueContributions,color = (Female==1))) +
  geom_jitter()+ 
  geom_smooth()+
  facet_grid(. ~ Discipline)

ggplot(filter(gender_uniquecontrib,YearsContributing<5,!is.na(Female)),aes(Female==1,TotalUniqueContributions)) +
  stat_summary(fun.y = mean, geom = "bar") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar") + 
  facet_grid(. ~ Discipline)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

ggplot(filter(gender_uniquecontrib,YearsContributing>10,!is.na(Female)),aes(Female==1,TotalUniqueContributions)) +
  stat_summary(fun.y = mean, geom = "bar") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar") + 
  facet_grid(. ~ Discipline)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

ggplot(gender_uniquecontrib,aes(YearsContributing)) + 
  geom_bar()
## Normalize thread measurements to participant population

# Normalizing thread discipline representation to the overall community size of discipline
thread_to_discipline = data_thread %>% 
  select(ThreadId,starts_with('Thread_Discipline')) %>% 
  unique()
tmp = sweep(thread_to_discipline[,-1], 2, pop_discipline$Fraction, `/`)
thread_to_discipline = cbind(thread_to_discipline %>% select(ThreadId), tmp)
colnames(tmp) = gsub("^.*?_","",gsub("^.*?_","",colnames(tmp)))
thread_to_discipline$Thread_OverrepresentedDiscipline = colnames(tmp)[max.col(tmp,ties.method="first")]
thread_to_discipline$Thread_UnderrepresentedDiscipline = colnames(tmp)[max.col(-tmp,ties.method="first")]

# Normalizing thread job representation to the overall community size of job
thread_to_job = data_thread %>% 
  select(ThreadId,starts_with('Thread_Job')) %>% 
  unique()
tmp = sweep(thread_to_job[,-1], 2, pop_job$Fraction, `/`)
thread_to_job = cbind(thread_to_job %>% select(ThreadId), tmp)
colnames(tmp) = gsub("^.*?_","",gsub("^.*?_","",colnames(tmp)))
thread_to_job$Thread_OverrepresentedJob = colnames(tmp)[max.col(tmp,ties.method="first")]
thread_to_job$Thread_UnderrepresentedJob = colnames(tmp)[max.col(-tmp,ties.method="first")]

# Joining mormalized discipline and job metrics to main table
data_thread = data_thread %>% 
  select(-starts_with('Thread_Discipline'),-starts_with('Thread_Job')) %>%
  left_join(thread_to_discipline, by='ThreadId')%>%
  left_join(thread_to_job, by='ThreadId')


# Getting a uniquified view of thread metrics for plotting
thread_info = data_thread %>% 
  select(Year,
         Title,
         Link,
         Type,
         ThreadId,
         DebateSize,
         FemaleParticipation,
         Live,
         UniqueContributors,
         UniqueFemaleParticipation,
         starts_with('Thread_')) %>% 
  unique()



