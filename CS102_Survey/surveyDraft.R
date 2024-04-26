library(readr)
library(dplyr)
library(tidyr)

draftSurvey<-read.csv('/cloud/project/CS102_Survey/RESPONDENTS.csv')

View(draftSurvey)

#Performance Expectancy Questionnaire
draftSurvey$X1....I.find.the.messaging.system.useful.in.terms.of.communication.
draftSurvey$X2...Using.the.messaging.system.enables.me.to.communicate.efficiently.
draftSurvey$X3...Using.the.messaging.system.increases.my.productivity.
draftSurvey$X4...If.I.use.the.messaging.system..I.will.increase.my.chances.of.online.interaction.


#Mean and STD of each questions (PP)
PP_meanstd <- draftSurvey %>%
  summarise(
    Mean = c(mean(X1....I.find.the.messaging.system.useful.in.terms.of.communication.), 
             mean(X2...Using.the.messaging.system.enables.me.to.communicate.efficiently.), 
             mean(X3...Using.the.messaging.system.increases.my.productivity.), 
             mean(X4...If.I.use.the.messaging.system..I.will.increase.my.chances.of.online.interaction.)),
    STD = c(sd(X1....I.find.the.messaging.system.useful.in.terms.of.communication.), 
            sd(X2...Using.the.messaging.system.enables.me.to.communicate.efficiently.), 
            sd(X3...Using.the.messaging.system.increases.my.productivity.), 
            sd(X4...If.I.use.the.messaging.system..I.will.increase.my.chances.of.online.interaction.))
  ) %>%
  mutate(Variable = c("U6", "RA1", "RA5", "OE7")) %>%
  select(Variable, Mean, STD) %>%
  filter(!is.na(Mean))

colnames(PP_meanstd)[1]<- "Performance Expectancy"

Totalresult <- PP_meanstd %>%
  summarise(Mean = mean(Mean),
            STD = mean(STD),
  ) 

PP_all<- bind_rows(PP_meanstd, Totalresult) 
PP_all[5, 1] <- "Total"

View(PP_all)



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


#Social Influence Questionnaire
draftSurvey$X13..People.who.influence.my.behavior.think.that.I.should.use.the.messaging.system.
draftSurvey$X14...People.who.are.important.to.me.think.that.I.should.use.the.messaging.system.
draftSurvey$X15..The.student.management.of.the.institution.had.been.helpful.in.the.use.of.the.messaging.system.
draftSurvey$X16..In.general..the.organization.has.supported.the.use.of.the.messaging.system.

#SOCIAL INFLUENCE
SN1_mean <- mean(draftSurvey$X13..People.who.influence.my.behavior.think.that.I.should.use.the.messaging.system.)
SN1_sd <- sd(draftSurvey$X13..People.who.influence.my.behavior.think.that.I.should.use.the.messaging.system.)
SN1_mean
SN1_sd

SN2_mean <- mean(draftSurvey$X14...People.who.are.important.to.me.think.that.I.should.use.the.messaging.system.)
SN2_sd <- sd( draftSurvey$X14...People.who.are.important.to.me.think.that.I.should.use.the.messaging.system.)
SN2_mean
SN2_sd

SF2_mean <- mean(draftSurvey$X15..The.student.management.of.the.institution.had.been.helpful.in.the.use.of.the.messaging.system.)
SF2_sd <- sd(draftSurvey$X15..The.student.management.of.the.institution.had.been.helpful.in.the.use.of.the.messaging.system.)
SF2_mean
SF2_sd

SF4_mean <- mean(draftSurvey$X16..In.general..the.organization.has.supported.the.use.of.the.messaging.system.)
SF4_sd <- sd(draftSurvey$X16..In.general..the.organization.has.supported.the.use.of.the.messaging.system.)
SF4_mean
SF4_sd

si_total_average <- mean(SN1_mean,SN2_mean,SF2_mean,SF4_mean)
si_total_average

total_sd <- mean(SN1_sd,SN2_sd,SF2_sd,SF4_sd)
total_sd

SI_all <- data.frame(
  Variable = c("SN1", "SN2", "SF2", "SF4", "Total"),
  Mean = c(SN1_mean, SN2_mean, SF2_mean, SF4_mean,si_total_average),
  Standard_Deviation = c(SN1_sd, SN2_sd, SF2_sd, SF4_sd, total_sd)
)

View(SI_all)


#FACILITATING CONDITIONS

#Facilitating Conditions Questionnaire
draftSurvey$X17...I.have.the.resources.necessary.to.use.the.messaging.system.
draftSurvey$X18..I.have.the.knowledge.necessary.to.use.the.messaging.system.
draftSurvey$X19..The.messaging.system.is.not.compatible.with.other.systems.I.use.
draftSurvey$X20...A.specific.person..or.group..is.available.for.assistance.with.messaging.system.difficulties.


PBC2_mean <- mean(draftSurvey$X17...I.have.the.resources.necessary.to.use.the.messaging.system.)
PBC2_sd <- sd(draftSurvey$X17...I.have.the.resources.necessary.to.use.the.messaging.system.)
PBC2_mean
PBC2_sd

PBC3_mean <- mean(draftSurvey$X18..I.have.the.knowledge.necessary.to.use.the.messaging.system.)
PBC3_sd <- sd(draftSurvey$X18..I.have.the.knowledge.necessary.to.use.the.messaging.system.)
PBC3_mean
PBC3_sd

PBC5_mean <- mean(draftSurvey$X19..The.messaging.system.is.not.compatible.with.other.systems.I.use.)
PBC5_sd <- sd(draftSurvey$X19..The.messaging.system.is.not.compatible.with.other.systems.I.use.)
PBC5_mean
PBC5_sd

FC3_mean <- mean(draftSurvey$X20...A.specific.person..or.group..is.available.for.assistance.with.messaging.system.difficulties.)
FC3_sd <- sd(draftSurvey$X19..The.messaging.system.is.not.compatible.with.other.systems.I.use.)
FC3_mean
FC3_sd


fc_total_average <- mean(PBC2_mean,PBC3_mean,PBC5_mean,FC3_mean)
fc_total_average

fc_total_sd <- mean(PBC2_sd,PBC3_sd,PBC5_sd,FC3_sd)
fc_total_sd

FC_all<- data.frame(
  Variable = c("PBC2", "PBC43", "PBC5", "FC3", "Total"),
  Mean = c(PBC2_mean, PBC3_mean, PBC5_mean, FC3_mean,fc_total_average),
  Standard_Deviation = c(PBC2_sd, PBC3_sd, PBC5_sd, FC3_sd, fc_total_sd)
)

View(FC_all)

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

#Mutating  all the factors
EE_all <- EE_all %>%
  mutate(Questionnaire_Type = "Effort Expectancy")

BI_all <- BI_all %>%
  mutate(Questionnaire_Type = "Behavioral Intention to use the System")

PP_all <- PP_all %>%
  mutate(Questionnaire_Type = "Performance Expectancy")

FC_all <- FC_all %>%
  mutate(Questionnaire_Type = "Facilitating Conditions")

SI_all <- SI_all %>%
  mutate(Questionnaire_Type = "Social Influence")


combined_data <- bind_rows(PP_all, EE_all, SI_all, FC_all, BI_all)

View(combined_data)