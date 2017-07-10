## Measurements at thread level
pop_discipline = data_orig0 %>%
  group_by(Id) %>%
  arrange(desc(PreviousContributions)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  group_by(Discipline) %>%
  summarize(DisciplinePopSize = n(),
            FractionFemale = sum(Female,na.rm=TRUE)/sum(Male + Female, na.rm=TRUE)) %>%
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
  mutate(UnderRep = binom.confint(n,sum(n),methods='wilson')$upper < DisciplinePopSize,
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
  summarise_at(.vars = 69:150,
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
uniquify_contrib = data_orig0 %>%
  group_by(Id) %>%
  arrange(desc(PreviousContributions)) %>%
  filter(row_number()==1)

ggplot(uniquify_contrib, aes(reorder(Discipline, Male, sum))) + 
  geom_bar() +
  xlab('Discipline') +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

ggplot(uniquify_contrib, aes(reorder(Job_Title_S, Male, sum))) + 
  geom_bar() +
  xlab('Job Title') +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

uniquify_thread = data_orig0 %>%
  group_by(ThreadId) %>%
  arrange(desc(PreviousContributions)) %>%
  filter(row_number()==1)

ggplot(uniquify_thread, aes(Year)) + geom_bar()

ggplot(uniquify_thread, aes(DebateSize)) + 
  geom_histogram(bins = 25)+
  scale_y_log10() +
  scale_x_log10()


pop_discipline_conf_data = data_orig0 %>%
  group_by(Id) %>%
  arrange(desc(PreviousContributions)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  group_by(Discipline) %>%
  summarize(DisciplinePopSize = sum(Male + Female, na.rm=TRUE),
            DisciplineFemSize = sum(Female,na.rm=TRUE)) %>%
  mutate(DisciplinePopFrac = DisciplinePopSize/sum(DisciplinePopSize)) 

pop_discipline_conf0 = binom.confint(pop_discipline_conf_data$DisciplineFemSize,pop_discipline_conf_data$DisciplinePopSize,methods='wilson')[4:6]
pop_discipline_conf = cbind(as.character(pop_discipline_conf_data$Discipline),pop_discipline_conf_data$DisciplinePopFrac, pop_discipline_conf0)
names(pop_discipline_conf)[1]='Discipline'
names(pop_discipline_conf)[2]='DisciplinePopFrac'

ggplot(pop_discipline_conf, aes(reorder(Discipline,mean),group=1)) + 
  geom_bar(aes(y=DisciplinePopFrac),stat='identity') +
  geom_line(aes(y=mean)) +
  geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  xlab('Discipine') + 
  scale_y_continuous("Discipline Community Size", 
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
  xlab('Job Title') + 
  scale_y_continuous("Fraction with This Job", 
    sec.axis = sec_axis(~ ., name = "Fraction Female")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

pop_job_conf_data = data_orig0 %>%
  group_by(Id) %>%
  arrange(desc(PreviousContributions)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  mutate(Job_Title_S = ifelse(grepl('Entrepreneur|Management|Founder',Job_Title_S),'Management',
                              ifelse(grepl('Postdoctoral|Student', Job_Title_S),'Trainee',
                                     ifelse(grepl('Other|Not Available', Job_Title_S) | Job_Title_S=='','Other',
                                            Job_Title_S)))) %>%
  group_by(Job_Title_S) %>%
  summarize(JobPopSize = sum(Male + Female, na.rm=TRUE),
            JobFemSize = sum(Female,na.rm=TRUE)) %>%
  mutate(JobPopFrac = JobPopSize/sum(JobPopSize)) 

pop_job_conf0 = binom.confint(pop_job_conf_data$JobFemSize,pop_job_conf_data$JobPopSize,methods='wilson')[4:6]
pop_job_conf = cbind(as.character(pop_job_conf_data$Job_Title_S),pop_job_conf_data$JobPopFrac, pop_job_conf0)
names(pop_job_conf)[1]='JobTitle'
names(pop_job_conf)[2]='JobPopFrac'
ggplot(pop_job_conf, aes(reorder(JobTitle,mean),group=1)) + 
  geom_bar(aes(y=JobPopFrac),stat='identity') +
  geom_line(aes(y=mean)) +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  xlab('Job Title') + 
  scale_y_continuous("Fraction of All Users with This Job", 
                     sec.axis = sec_axis(~ ., name = "Fraction Female")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

ggplot(data_orig0, aes(Year,FemaleParticipation)) +
  stat_summary_bin()

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

ggplot(filter(gender_uniquecontrib),
       aes(YearsContributing,TotalUniqueContributions)) +
  geom_jitter()+ 
  geom_smooth()

ggplot(filter(gender_uniquecontrib),
       aes(TotalUniqueContributions)) +
  geom_bar()

ggplot(filter(gender_uniquecontrib,Discipline==''),
       aes(YearsContributing,TotalUniqueContributions,color = (Female==1))) +
  geom_jitter()+ 
  geom_smooth()



ggplot(filter(gender_uniquecontrib,Discipline!=''),aes(YearsContributing,TotalUniqueContributions,color = (Female==1))) +
  geom_jitter()+ 
  geom_smooth()+
  facet_grid(Discipline ~ .)

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






