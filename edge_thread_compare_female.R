# group by individual and measure fraction of each gender not counting the individual
# Getting a uniquified view of thread metrics for plotting
thread_info = data_thread %>% 
  filter(UniqueContributors>5) %>%
  select(Year,
         Title,
         Link,
         Type,
         ThreadId,
         DebateSize,
         Female_Contributions,
         FemaleParticipation,
         Live,
         UniqueContributors,
         UniqueFemaleContributors,
         UniqueFemaleParticipation,
         starts_with('Thread_'),
         -starts_with('Thread_Text')) %>% 
  unique()

# test whether unique female participation is segmented to threads in a significant way
prop.test(thread_info$UniqueFemaleContributors,thread_info$UniqueContributors)

# display results of prp test
female_confint0 = binom.confint(sum(thread_info$UniqueFemaleContributors),sum(thread_info$UniqueContributors),methods='wilson')[5:6]
female_confint = cbind(as.character(thread_info$ThreadId),thread_info$DebateSize,thread_info$Title,binom.confint(thread_info$UniqueFemaleContributors,thread_info$UniqueContributors,methods='wilson')[,4:6])
names(female_confint)[1]='Thread'
names(female_confint)[2]='ThreadSize'
names(female_confint)[3]='Title'

ggplot(female_confint,aes(reorder(Thread,ThreadSize),mean))+
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  geom_hline(yintercept=female_confint0$lower)+
  geom_hline(yintercept=female_confint0$upper)+
  ylab('Female Fraction')+
  xlab('Thread')+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

  
# smae but for all female participation
prop.test(thread_info$Female_Contributions,thread_info$DebateSize)

# display result
female_confint0 = binom.confint(sum(thread_info$Female_Contributions),sum(thread_info$DebateSize),methods='wilson')[5:6]
female_confint = cbind(as.character(thread_info$ThreadId),binom.confint(thread_info$Female_Contributions,thread_info$DebateSize,methods='wilson')[,4:6])
names(female_confint)[1]='Thread'
ggplot(female_confint,aes(Thread,mean))+
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  geom_hline(yintercept=female_confint0$lower)+
  geom_hline(yintercept=female_confint0$upper)

# 