## Measurements at thread level

# Counting participants by discipline in each thread
thread_discipline = data_orig0 %>%
  group_by(ThreadId,Id) %>%
  arrange(desc(PreviousContributions)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  group_by(ThreadId,Discipline) %>%
  summarize(n=n()) %>%
  mutate(Discipline = paste0('Thread_Discipline_',Discipline)) %>%
  spread(Discipline,n,fill=0)

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
data_thread = data_orig0 %>% 
  left_join(thread_discipline,by='ThreadId') %>%
  left_join(thread_job, by= 'ThreadId') %>%
  left_join(thread_other, by= 'ThreadId') %>%
  mutate_at(.cols = vars(starts_with('Thread_')),
            .funs = funs(./UniqueContributors)) %>%
  left_join(thread_text, by=c('ThreadId'='Thread_Text_ThreadId'))

## Measurements at participant population level

# Counting participants by discipline overall
pop_discipline = data_orig0 %>%
  group_by(Id) %>%
  arrange(desc(PreviousContributions)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  group_by(Discipline) %>%
  summarize(Fraction = n()) %>%
  mutate(Fraction = Fraction/sum(Fraction)) 

# Counting participants by job overall
pop_job = data_orig0 %>%
  group_by(Id) %>%
  arrange(desc(PreviousContributions)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  group_by(Job_Title_S) %>%
  summarize(Fraction = n()) %>%
  mutate(Fraction = Fraction/sum(Fraction)) 

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



