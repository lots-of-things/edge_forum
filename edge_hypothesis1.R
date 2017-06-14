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

discipline_cumsum = data_orig0 %>%
  group_by(ThreadId,Discipline) %>%
  mutate(CumSum = lag(row_number(),k=1,default=0)) %>%
  ungroup() %>%
  mutate(Discipline_Rename = paste0('Discipline_CumSum_',Discipline)) %>%
  spread(Discipline_Rename,CumSum,fill=0) 

uniquediscipline_cumsum = data_orig0 %>%
  group_by(ThreadId,Id) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  group_by(ThreadId,Discipline) %>%
  mutate(CumSum = lag(row_number(),k=1,default=0)) %>%
  ungroup() %>%
  mutate(Discipline_Rename = paste0('Discipline_CumSum_',Discipline)) %>%
  spread(Discipline_Rename,CumSum,fill=0) 
  
  


data_th1 = data_th1_0 %>% 
  left_join(unique_cumsum, by=c('ThreadId','Id')) %>%
  mutate(OtherFemale_CumFrac = (Female_CumSum-Personal_CumSum)/Contrib_CumSum,
         ThreadProgress = Contrib_CumSum/DebateSize,
         UniqueFemale_CumFrac = UniqueFemale_CumSum/UniqueContrib_CumSum,
         UniqueThreadProgress = UniqueContrib_CumSum/UniqueContributors) %>%
  filter(Personal_CumSum==0) %>%
  select(10,
         22,23
         27
         30,31
         33,
         36,27,38,39,42,58,61,62,63,64,65
         -()
         -(66:68),
         Female_CumSum,
         Contrib_CumSum,
         Personal_CumSum,
         UniqueContrib_CumSum,
         UniqueFemale_CumSum) 

names(data_th1)

model = glm(Female ~ OtherFemale_CumFrac + UniqueFemale_CumFrac + ThreadProgress + UniqueThreadProgress + DebateSize, family = 'binomial', data_th1 )
summary(model)

model_weird = glm(Female ~ Workplace_SR, family = 'binomial', data_th1)
summary(model_weird)

model_full = glm(Female ~ ., family = 'binomial', data_th1)
summary(model_full)
