contrib_cumsum = data_orig0 %>%
  group_by(ThreadId) %>%
  arrange(Order) %>%
  mutate(Female_CumSum = lag(cumsum(Female),k=1,default=0),
         Contrib_CumSum = lag(row_number(),k=1,default=0)) %>%
  ungroup() %>%
  group_by(ThreadId,Id) %>%
  mutate(Personal_CumSum = lag(row_number(),k=1,default=0)) %>%
  ungroup() 

uniquecontrib_cumsum = data_orig0 %>%
  group_by(ThreadId,Id) %>%
  summarize(Female = mean(Female)) %>%
  group_by(ThreadId) %>%
  mutate(UniqueContrib_CumSum = lag(row_number(),k=1,default=0),
         UniqueFemale_CumSum = lag(cumsum(Female),k=1,default=0)) %>%
  ungroup() %>%
  select(-Female) 

## not working
# discipline_cumsum = data_thread %>%
#   select(ThreadId,Discipline,Id,Order) %>%
#   group_by(ThreadId,Discipline) %>%
#   mutate(CumSum = lag(row_number(),k=1,default=0)) %>%
#   ungroup() %>%
#   mutate(Discipline_Rename = paste0('Discipline_CumSum_',Discipline)) %>%
#   spread(Discipline_Rename,CumSum,fill=0) 
# 
# uniquediscipline_cumsum = data_orig0 %>%
#   group_by(ThreadId,Id) %>%
#   filter(row_number()==1) %>%
#   ungroup() %>%
#   group_by(ThreadId,Discipline) %>%
#   mutate(CumSum = lag(row_number(),k=1,default=0)) %>%
#   ungroup() %>%
#   mutate(Discipline_Rename = paste0('Discipline_CumSum_',Discipline)) %>%
#   spread(Discipline_Rename,CumSum,fill=0) 
  
  


data_th = contrib_cumsum %>% 
  left_join(uniquecontrib_cumsum, by=c('ThreadId','Id')) %>%
  mutate(OtherFemale_CumFrac = (Female_CumSum-Personal_CumSum)/Contrib_CumSum,
         ThreadProgress = Contrib_CumSum/(Male_Contributions+Female_Contributions),
         UniqueFemale_CumFrac = UniqueFemale_CumSum/UniqueContrib_CumSum,
         UniqueThreadProgress = UniqueContrib_CumSum/UniqueContributors) %>%
  filter(Personal_CumSum==0) 

data_th2 = data_th %>%
  select(Year,22,
         OtherFemale_CumFrac,
         ThreadProgress,
         UniqueFemale_CumFrac,
         UniqueThreadProgress,
         starts_with('OverRep'),
         starts_with('UnderRep'))

data_th1 = data_th %>%
  select(Year,10,22,36,38,39,42,65,
         Discipline,
         Female_CumSum,
         Contrib_CumSum,
         UniqueContrib_CumSum,
         UniqueFemale_CumSum,
         OtherFemale_CumFrac,
         ThreadProgress,
         UniqueFemale_CumFrac,
         UniqueThreadProgress) 

names(data_th1)

model = glm(Female ~ OtherFemale_CumFrac + ThreadProgress + Year, family = 'binomial', data_th1 )
summary(model)
confint(model)

model_2 = glm(Female ~ UniqueFemale_CumFrac, family = 'binomial', data_th1 )
summary(model_2)

model_weird = glm(Female ~ Discipline, family = 'binomial', data_orig0)
summary(model_weird)

model_full = glm(Female ~ ., family = 'binomial', data_th1)
summary(model_full)

model_middle = glm(Female ~ . - PreviousContributions - PreviousThreads - ThreadsThisYear - Years_from_PhD, family = 'binomial', data_th1)
summary(model_middle)

model_test_unique = glm(Female ~ . - PreviousContributions - PreviousThreads - ThreadsThisYear - Years_from_PhD - OtherFemale_CumFrac, family = 'binomial', data_th1)
summary(model_test_unique)

model_disc = glm(Female ~ ThreadProgress + Discipline + OtherFemale_CumFrac, family = 'binomial', data_th1)
summary(model_disc)

model_disc_anon = glm(Female ~ . , family = 'binomial', data_th2)
summary(model_disc_anon)


## test if being female correlates with years from phd 

correlation = cor(data_th1[sapply(data_th1, is.numeric)],use='pairwise.complete.obs')
correlation['Female',]
corrplot(correlation, method="color")

ggplot(data_th1,aes(UniqueFemale_CumFrac,Female))+
  stat_summary_bin()+
  geom_smooth(method='lm')

ggplot(data_th1,aes(UniqueFemale_CumFrac,Female,color=Discipline))+
  geom_smooth(method='lm')

ggplot(data_th1,aes(OtherFemale_CumFrac,Female))+
  stat_summary_bin()+
  geom_smooth(method='lm')

ggplot(data_th1,aes(OtherFemale_CumFrac,Female,color=Discipline))+
  geom_smooth(method='lm')

ggplot(data_th1,aes(ThreadProgress,Female))+
  stat_summary_bin()+
  geom_smooth(method='lm')

ggplot(data_th1,aes(ThreadProgress,Female,color=Discipline))+
  geom_smooth(method='lm')
