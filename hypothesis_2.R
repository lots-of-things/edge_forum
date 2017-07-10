## setup dataset with all the relevant data on the individual
user_wc = data_orig0 %>%
  group_by(Id) %>%
  summarize(posts = n(),
            Female = max(Female),
            WC = median(WC),
            WPS = median(WPS),
            Sixltr = median(Sixltr),
            Number.Characters = median(Number.Characters),
            AcademicHierarchyStrict = max(AcademicHierarchyStrict),
            Job_Title_S = max(as.character(Job_Title_S)),
            Discipline = max(as.character(Discipline)),
            PhD_Year = max(as.character(PhD_Year)),
            PhD_Institution_SR_Bin = min(PhD_Institution_SR_Bin),
            Workplace_SR_Bin = min(Workplace_SR_Bin),
            Total_Citations = max(Total_Citations))

#plot individual stats for word count
ggplot(user_wc,aes(reorder(Job_Title_S,WC,mean),WC))+
  stat_summary()+
  xlab('Job Title') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(user_wc,aes(reorder(Discipline,WC,mean),WC))+
  stat_summary()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(user_wc,aes(posts,WC))+
  stat_summary()+
  geom_smooth(method='lm')

ggplot(user_wc,aes(Total_Citations,WC,color=as.factor(AcademicHierarchyStrict)))+
  stat_summary_bin()+
  geom_smooth(method='lm')+
  scale_x_log10()+
  scale_y_log10()

ggplot(user_wc,aes(reorder(Job_Title_S,WPS,mean),WPS))+
  stat_summary()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(user_wc,aes(reorder(Discipline,WPS,mean),WPS))+
  stat_summary()+
  xlab('Discipline')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(user_wc,aes(posts,WPS))+
  stat_summary()+
  geom_smooth(method='lm')

ggplot(user_wc,aes(Total_Citations,WPS,color=as.factor(AcademicHierarchyStrict)))+
  stat_summary_bin()+
  geom_smooth(method='lm')+
  scale_x_log10()+
  scale_y_log10()

ggplot(user_wc,aes(reorder(Job_Title_S,Sixltr,mean),Sixltr))+
  stat_summary()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(user_wc,aes(reorder(Discipline,Sixltr,mean),Sixltr))+
  stat_summary()+
  xlab('Discipline')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(user_wc,aes(posts,Sixltr))+
  stat_summary()+
  geom_smooth(method='lm')

ggplot(user_wc,aes(Total_Citations,Sixltr,color=as.factor(AcademicHierarchyStrict)))+
  stat_summary_bin()+
  geom_smooth(method='lm',se=F)+
  scale_x_log10()+
  scale_y_log10()



thread_normalizer = data_orig0 %>% 
  group_by(ThreadId) %>%
  summarize(Thread_WC_mean = mean(WC),
            Thread_WPS_mean = mean(WPS),
            Thread_WC_sd = sd(WC),
            Thread_WPS_sd = sd(WPS))
  

## here we aggregate across thread
user_thread_wc = data_orig0 %>%
  left_join(thread_normalizer) %>%
  group_by(Id,ThreadId) %>%
  summarize(posts = n(),
            Female = max(Female),
            WC = (mean(WC)-mean(Thread_WC_mean))/mean(Thread_WC_sd),
            WPS = (mean(WPS)-mean(Thread_WPS_mean))/mean(Thread_WPS_sd),
            Sixltr = mean(Sixltr),
            Number.Characters = sum(Number.Characters),
            AcademicHierarchyStrict = max(AcademicHierarchyStrict),
            Job_Title_S = max(as.character(Job_Title_S)),
            Discipline = max(as.character(Discipline)),
            PhD_Institution_SR_Bin = min(PhD_Institution_SR_Bin),
            Workplace_SR_Bin = min(Workplace_SR_Bin),
            Total_Citations = max(Total_Citations),
            Citations_Year = max(Citations_Year),
            H_Index = max(H_Index),
            i10_Index = max(i10_Index)) %>%
  group_by(Id) %>%
  summarize(posts = sum(posts),
            Female = max(Female),
            WC = mean(WC),
            WPS = mean(WPS),
            WC_max = max(WC),
            WPS_max = max(WPS),
            Sixltr = mean(Sixltr),
            Number.Characters = median(Number.Characters),
            AcademicHierarchyStrict = max(AcademicHierarchyStrict),
            Job_Title_S = max(as.character(Job_Title_S)),
            Discipline = max(as.character(Discipline)),
            PhD_Institution_SR_Bin = min(PhD_Institution_SR_Bin),
            Workplace_SR_Bin = min(Workplace_SR_Bin),
            Total_Citations = max(Total_Citations),
            Citations_Year = max(Citations_Year),
            H_Index = max(H_Index),
            i10_Index = max(i10_Index))

## repoeat word count plots but summed over thread
ggplot(user_thread_wc,aes(Job_Title_S,WC))+
  stat_summary()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(user_thread_wc,aes(Discipline,WC))+
  stat_summary()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(user_thread_wc,aes(posts,WC))+
  stat_summary()+
  geom_smooth(method='lm')

ggplot(user_thread_wc,aes(Total_Citations,WC,color=as.factor(AcademicHierarchyStrict)))+
  stat_summary_bin()+
  geom_smooth(method='lm')+
  scale_x_log10()+
  scale_y_log10()

ggplot(user_thread_wc,aes(Total_Citations,WC,color=Job_Title_S))+
  stat_summary_bin()+
  geom_smooth(method='lm')+
  scale_x_log10()+
  scale_y_log10()


ggplot(user_thread_wc,aes(H_Index,WC,color=Job_Title_S))+
  stat_summary_bin()

ggplot(user_thread_wc,aes(i10_Index,WC,color=Job_Title_S))+
  stat_summary_bin()

ggplot(user_thread_wc,aes(i10_Index,WC))+
  stat_summary_bin()

## double checking the posts stats
# qplot((user_thread_wc = data_orig0 %>%
#         group_by(Id,Thread) %>%
#         summarize(posts = n(),
#                   Female = max(Female),
#                   WC = mean(WC),
#                   WPS = mean(WPS),
#                   Sixltr = mean(Sixltr),
#                   Number.Characters = sum(Number.Characters),
#                   AcademicHierarchyStrict = max(AcademicHierarchyStrict),
#                   Job_Title_S = max(Job_Title_S),
#                   Discipline = max(Discipline),
#                   PhD_Year = max(PhD_Year),
#                   PhD_Institution_SR_Bin = min(PhD_Institution_SR_Bin),
#                   Workplace_SR_Bin = min(Workplace_SR_Bin),
#                   Total_Citations = max(Total_Citations)))$posts)


## now look at WPS
ggplot(user_thread_wc,aes(Job_Title_S,WPS))+
  stat_summary()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(user_thread_wc,aes(Discipline,WPS))+
  stat_summary()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(user_thread_wc,aes(posts,WPS))+
  stat_summary_bin()+
  geom_smooth(method='lm')

ggplot(user_thread_wc,aes(Total_Citations,WPS,color=as.factor(AcademicHierarchyStrict)))+
  stat_summary_bin()+
  geom_smooth(method='lm')+
  scale_x_log10()+
  scale_y_log10()



## and now Six letter words
ggplot(user_wc,aes(Job_Title_S,Sixltr))+
  stat_summary()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(user_wc,aes(Discipline,Sixltr))+
  stat_summary()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(user_wc,aes(posts,Sixltr))+
  stat_summary()+
  geom_smooth(method='lm')

ggplot(user_wc,aes(Total_Citations,Sixltr,color=as.factor(AcademicHierarchyStrict)))+
  stat_summary_bin()+
  geom_smooth(method='lm')+
  scale_x_log10()+
  scale_y_log10()


ggplot(user_wc,aes(Total_Citations,Sixltr,color=as.factor(Discipline)))+
  stat_summary_bin()+
  geom_smooth(method='lm')+
  scale_x_log10()+
  scale_y_log10()

#Look by discipline
ggplot(user_wc,aes(Total_Citations,WPS,color=as.factor(Discipline)))+
  stat_summary_bin()+
  geom_smooth(method='lm')+
  scale_x_log10()+
  scale_y_log10()


model_wps = glm(WPS ~ . - Id - WPS_max - WC - WC_max, family = 'gaussian', user_thread_wc )
summary(model_wps)

model_wps_squared = glm(WPS ~ Discipline*Citations_Year, family = 'gaussian', user_thread_wc )
summary(model_wps_squared)


model_six = glm(Sixltr ~ . - Id - WPS - WC - PhD_Year - Discipline, family = 'gaussian', user_wc )
summary(model_six)

model_six_squared = glm(Sixltr ~ Discipline + Total_Citations + AcademicHierarchyStrict , family = 'gaussian', user_wc )
summary(model_six_squared)


model_six_simp = glm(Sixltr ~ Total_Citations, family = 'gaussian', user_wc )
summary(model_six_simp)
