library(readr)
library(dplyr)
library(tidyr)

draftSurvey<-read.csv('C:/Users/User/Documents/Rstudio Files/midtermsurvey/FINAL EXCEL/RESPONDENTS.csv')

View(draftSurvey)


#Effort Expectancy Questionnaire
draftSurvey$X5..My.interaction.with.the.messaging.system.is..clear.and.understandable.
draftSurvey$X6..It.is.easy.for.me.to.become.skillful.at.using.the.messaging.system.
draftSurvey$X7...I.find.the.messaging.system.easy.to.use.
draftSurvey$X8..Learning.to.operate.the.messaging.system.is.easy.for.me.

#Mean and STD of each questions (EE)
EE_meanstd<- draftSurvey %>%
  summarise(
    Mean = c(mean(X5..My.interaction.with.the.messaging.system.is..clear.and.understandable.),
             mean(X6..It.is.easy.for.me.to.become.skillful.at.using.the.messaging.system.),
             mean(X7...I.find.the.messaging.system.easy.to.use.),
             mean(X8..Learning.to.operate.the.messaging.system.is.easy.for.me.)),
    STD = c(sd(X5..My.interaction.with.the.messaging.system.is..clear.and.understandable.),
            sd(X6..It.is.easy.for.me.to.become.skillful.at.using.the.messaging.system.),
            sd(X7...I.find.the.messaging.system.easy.to.use.),
            sd(X8..Learning.to.operate.the.messaging.system.is.easy.for.me.))
  ) %>%
  mutate(EE = c("EOU3", "EOU5", "EOU6", "EU4")) %>%
  select(EE, Mean, STD) %>%
  filter(!is.na(Mean))

#Total of the mean and STD (EE)
colnames(EE_meanstd)[1]<- "Effort Expectancy"

totalRes<- EE_meanstd %>%
  summarise(Mean = mean(Mean),
            STD = mean(STD),
  )
EE_all<- bind_rows(EE_meanstd, totalRes)
EE_all[5, 1] <- "Total"

View(EE_all)
EE_all



#Behavioral Intention to use the system Questionnaire
draftSurvey$X29...I.intend.to.use.the.messaging.system.in.the.future.
draftSurvey$X30...I.predict.I.would.use.the.messaging.system.in.the.future.
draftSurvey$X31...I.plan.to.use.the.messaging.system.in.the.next.future.

#Mean and STD of each questions (BI)
BI_meanstd<-draftSurvey %>%
  summarise(
    Mean = c(mean(X29...I.intend.to.use.the.messaging.system.in.the.future.),
             mean(X30...I.predict.I.would.use.the.messaging.system.in.the.future.),
             mean(X31...I.plan.to.use.the.messaging.system.in.the.next.future.)),
    
    STD = c(sd(X29...I.intend.to.use.the.messaging.system.in.the.future.),
             sd(X30...I.predict.I.would.use.the.messaging.system.in.the.future.),
             sd(X31...I.plan.to.use.the.messaging.system.in.the.next.future.))
  ) %>%
  mutate(BI = c("BI1", "BI2", "BI3")) %>%
  select(BI, Mean, STD) %>%
  filter(!is.na(Mean))
  
#Total of Mean and STD (BI)
colnames(BI_meanstd)[1]<- "Behavioral Intention to use the System"

totalRes5<- BI_meanstd %>%
  summarise(Mean = mean(Mean),
            STD = mean(STD),
  )
BI_all<- bind_rows(BI_meanstd, totalRes5)
BI_all[4, 1] <- "Total"

View(BI_all)

BI_all2<-BI_all[1:4, 1:3]

BI_all2


#Mutating the factors
EE_BI<- 


