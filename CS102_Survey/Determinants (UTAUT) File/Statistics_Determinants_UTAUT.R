library(readr)
library(dplyr)
library(tidyr)

messRespon<-read.csv('/cloud/project/CS102_Survey/RESPONDENTS.csv')

                              #FACTORS AND THEIR MEAN AND STANDARD DEVIATION


#FACTOR: PERFORMANCE EXPECTANCY

#Performance Expectancy Questionnaire
#X1....I.find.the.messaging.system.useful.in.terms.of.communication.
#X2...Using.the.messaging.system.enables.me.to.communicate.efficiently.
#X3...Using.the.messaging.system.increases.my.productivity. 
#X4...If.I.use.the.messaging.system..I.will.increase.my.chances.of.online.interaction.

#Getting the Mean and Standard Deviation of the Performance Expectancy (PE) 
PE_meanstd <- messRespon %>%
  summarise(
    Factors = c("U6", "RA1", "RA5", "OE7"),
    Mean = c(
      mean(X1....I.find.the.messaging.system.useful.in.terms.of.communication.),
      mean(X2...Using.the.messaging.system.enables.me.to.communicate.efficiently.),
      mean(X3...Using.the.messaging.system.increases.my.productivity.),
      mean(X4...If.I.use.the.messaging.system..I.will.increase.my.chances.of.online.interaction.)
    ),
    Standard_Dev = c(
      sd(X1....I.find.the.messaging.system.useful.in.terms.of.communication.),
      sd(X2...Using.the.messaging.system.enables.me.to.communicate.efficiently.),
      sd(X3...Using.the.messaging.system.increases.my.productivity.),
      sd(X4...If.I.use.the.messaging.system..I.will.increase.my.chances.of.online.interaction.)
    )
  ) %>%
  mutate(Questionnaire_Type = "Performance Expectancy") %>%
  select(Questionnaire_Type,Factors, Mean, Standard_Dev)

TotalresultPE <- PE_meanstd %>%
  summarise(Mean = mean(Mean),
            Standard_Dev = mean(Standard_Dev),
  )

PE_all<- bind_rows(PE_meanstd, TotalresultPE)
PE_all[5, 1] <- "PERFORMANCE EXPECTANCY"
PE_all[5, 2] <-  "TOTAL"
View(PE_all)
PE_all


#FACTOR: EFFORT EXPECTANCY 

#Effort Expectancy Questionnaire(EE)
#X5..My.interaction.with.the.messaging.system.is..clear.and.understandable.
#X6..It.is.easy.for.me.to.become.skillful.at.using.the.messaging.system.
#X7...I.find.the.messaging.system.easy.to.use.
#X8..Learning.to.operate.the.messaging.system.is.easy.for.me.

#Getting the mean and Standard Deviation
EE_meanstd <- messRespon %>%
  summarise(
    Factors = c("EOU3", "EOU5", "EOU6", "EU4"),
    Mean = c(
      mean(X5..My.interaction.with.the.messaging.system.is..clear.and.understandable.),
      mean(X6..It.is.easy.for.me.to.become.skillful.at.using.the.messaging.system.),
      mean(X7...I.find.the.messaging.system.easy.to.use.),
      mean(X8..Learning.to.operate.the.messaging.system.is.easy.for.me.)
    ),
    Standard_Dev = c(
      sd(X5..My.interaction.with.the.messaging.system.is..clear.and.understandable.),
      sd(X6..It.is.easy.for.me.to.become.skillful.at.using.the.messaging.system.),
      sd(X7...I.find.the.messaging.system.easy.to.use.),
      sd(X8..Learning.to.operate.the.messaging.system.is.easy.for.me.)
    )
  ) %>%
  mutate(Questionnaire_Type = "Effort Expectancy") %>%
  select(Questionnaire_Type,Factors, Mean, Standard_Dev)

TotalresultEE <- EE_meanstd %>%
  summarise(Mean = mean(Mean),
            Standard_Dev = mean(Standard_Dev),
  )

EE_all<- bind_rows(EE_meanstd, TotalresultEE)
EE_all[5, 1] <- "EFFORT EXPECTANCY"
EE_all[5, 2] <-  "TOTAL"
View(EE_all)
EE_all

#FACTOR: SOCIAL INFLUENCE

#Social Influence Questionnaire
#X13..People.who.influence.my.behavior.think.that.I.should.use.the.messaging.system.
#X14...People.who.are.important.to.me.think.that.I.should.use.the.messaging.system.
#X15..The.student.management.of.the.institution.had.been.helpful.in.the.use.of.the.messaging.system. 
#X16..In.general..the.organization.has.supported.the.use.of.the.messaging.system.

#Getting the mean and standard deviation
SI_meanstd <- messRespon  %>%
  summarise(
    Factors = c("SN1", "SN2","SF2","SF4"),
    Mean = c(
      mean(X13..People.who.influence.my.behavior.think.that.I.should.use.the.messaging.system.),
      mean(X14...People.who.are.important.to.me.think.that.I.should.use.the.messaging.system.),
      mean(X15..The.student.management.of.the.institution.had.been.helpful.in.the.use.of.the.messaging.system.), 
      mean(X16..In.general..the.organization.has.supported.the.use.of.the.messaging.system.)
    ),
    Standard_Dev = c(
      sd(X13..People.who.influence.my.behavior.think.that.I.should.use.the.messaging.system.),
      sd(X14...People.who.are.important.to.me.think.that.I.should.use.the.messaging.system.),
      sd(X15..The.student.management.of.the.institution.had.been.helpful.in.the.use.of.the.messaging.system.), 
      sd(X16..In.general..the.organization.has.supported.the.use.of.the.messaging.system.)
    )
  ) %>%
  mutate(Questionnaire_Type = "Social Influence") %>%
  select(Questionnaire_Type, Factors, Mean, Standard_Dev)

TotalresultSI <- SI_meanstd %>%
  summarise(Mean = mean(Mean),
            Standard_Dev = mean(Standard_Dev),
  )

SI_all<- bind_rows(SI_meanstd, TotalresultSI)
SI_all[5, 1] <- "SOCIAL INFLUENCE"
SI_all[5, 2] <-  "TOTAL"
View(SI_all)
SI_all


#FACTOR: FACILITATING CONDITIONS

#Facilitating Conditions Questionnaire
#X17...I.have.the.resources.necessary.to.use.the.messaging.system.
#X18..I.have.the.knowledge.necessary.to.use.the.messaging.system.
#X19..The.messaging.system.is.not.compatible.with.other.systems.I.use.
#X20...A.specific.person..or.group..is.available.for.assistance.with.messaging.system.difficulties.

#Getting the mean and standard deviation
FC_meanstd <- messRespon  %>%
  summarise(
    Factors = c("SN1", "SN2","SF2","SF4"),
    Mean = c(
      mean(X17...I.have.the.resources.necessary.to.use.the.messaging.system.),
      mean(X18..I.have.the.knowledge.necessary.to.use.the.messaging.system.),
      mean(X19..The.messaging.system.is.not.compatible.with.other.systems.I.use.), 
      mean(X20...A.specific.person..or.group..is.available.for.assistance.with.messaging.system.difficulties.)
    ),
    Standard_Dev = c(
      sd(X17...I.have.the.resources.necessary.to.use.the.messaging.system.),
      sd(X18..I.have.the.knowledge.necessary.to.use.the.messaging.system.),
      sd(X19..The.messaging.system.is.not.compatible.with.other.systems.I.use.), 
      sd(X20...A.specific.person..or.group..is.available.for.assistance.with.messaging.system.difficulties.)
    )
  ) %>%
  mutate(Questionnaire_Type = "Facilitating Conditions") %>%
  select(Questionnaire_Type, Factors, Mean, Standard_Dev)

TotalresultFC <- FC_meanstd %>%
  summarise(Mean = mean(Mean),
            Standard_Dev = mean(Standard_Dev),
  )

FC_all<- bind_rows(FC_meanstd, TotalresultSI)
FC_all[5, 1] <- "FACILITATING CONDITIONS"
FC_all[5, 2] <-  "TOTAL"
View(FC_all)
FC_all

#FACTOR: BEHAVIORAL INTENTION TO USE THE SYSTEM 

#Behavioral Intention Questionnaire
#X29...I.intend.to.use.the.messaging.system.in.the.future.
#X30...I.predict.I.would.use.the.messaging.system.in.the.future.
#X31...I.plan.to.use.the.messaging.system.in.the.next.future.

#Getting the mean and standard deviation

BI_meanstd <- messRespon %>%
  summarise(
    Factors = c("BI1", "BI2", "BI3"),
    Mean = c(
      mean(X29...I.intend.to.use.the.messaging.system.in.the.future.),
      mean(X30...I.predict.I.would.use.the.messaging.system.in.the.future.),
      mean(X31...I.plan.to.use.the.messaging.system.in.the.next.future.)
    ),
    Standard_Dev = c(
      sd(X29...I.intend.to.use.the.messaging.system.in.the.future.),
      sd(X30...I.predict.I.would.use.the.messaging.system.in.the.future.),
      sd(X31...I.plan.to.use.the.messaging.system.in.the.next.future.)
    )
  ) %>%
  mutate(Questionnaire_Type = "Behavioral Intention") %>%
  select(Questionnaire_Type,Factors, Mean, Standard_Dev)

TotalresultBI <- BI_meanstd %>%
  summarise(Mean = mean(Mean),
            Standard_Dev = mean(Standard_Dev),
  )
BI_all<- bind_rows(BI_meanstd, TotalresultBI)
BI_all[4, 1] <- "BEHAVIORAL INTENTION"
BI_all[4, 2] <-  "TOTAL"
View(BI_all)
BI_all


final_combined_data <- bind_rows(PE_all, EE_all, SI_all, FC_all, BI_all)
View(final_combined_data)

overall <- final_combined_data %>%
  summarise(Mean = mean(Mean),
            Standard_Dev = mean(Standard_Dev),
  )

stats_overall<- bind_rows(final_combined_data, overall)
stats_overall[25, 1] <- "OVERALL RESULT"
stats_overall[25, 2] <-  "OVERALL SUMMARY"
View(stats_overall)
stats_overall
