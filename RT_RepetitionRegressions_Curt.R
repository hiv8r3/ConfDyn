require(dplyr)
require(lme4)
require(lmerTest)

setwd("C:/Users/Curt/Box Sync/Bruce Projects/Sequential Processing/PointByPoint Processing/Manuscript/Review & Resubmission/Data")
dat = read.delim("RT_AllSubs_TBT_Cond_Prev_Rep.txt")
dat = dat[dat$ACC == 1 & dat$PrevTrialACC == 1,]

# discard trials for which RT is less than 100 and greater than 2000
dat$RTdrop = dat$RT_Raw
dat$RTdrop[dat$RT_Raw < 100] = NA
dat$RTdrop[dat$RT_Raw > 2000] = NA

# add trial information
dat$prevTrialType = NA
dat$prevTrialType[dat$PrevTrialType == 1] = "compatible"
dat$prevTrialType[dat$PrevTrialType == 2] = "incompatible"
dat$prevTrialType = factor(dat$prevTrialType)

# Fix TargetRep and FlankerRep columns to flag duplications
dat$TargetRep[dat$StimOnsetCode == dat$PrevStimOnsetCode] = 1
dat$FlankerRep[dat$StimOnsetCode == dat$PrevStimOnsetCode] = 1

# select subjects in Condition 4 and 5, take out 92 and 64
dat.use = dat[dat$Condition == 4|dat$Condition == 5,] %>% 
  filter(Subject != 92) %>% 
  filter(Subject != 64)

dat.use$Condition = paste(dat.use$prevTrialType, dat.use$TrialType, sep="_")

tapply(dat.use$RTdrop100_2000, dat.use$TrialType, mean)
tapply(dat.use$RTdrop100_2000, dat.use$Condition, mean)

# make effect coding variables
dat.use$Current.e = NA
dat.use$Current.e[dat.use$TrialType == "compatible"] = -1
dat.use$Current.e[dat.use$TrialType == "Incompatible"] = 1

dat.use$Previous.e = NA
dat.use$Previous.e[dat.use$prevTrialType == "compatible"] = -1
dat.use$Previous.e[dat.use$prevTrialType == "incompatible"] = 1

# add label for trial condition
dat.use$TrialCondition = "Previous compatible - Current incompatible"
dat.use$TrialCondition[dat.use$prevTrialType == "compatible" & dat.use$TrialType == "compatible"] = 
  "Previous compatible - Current compatible"
dat.use$TrialCondition[dat.use$prevTrialType == "incompatible" & dat.use$TrialType == "Incompatible"] = 
  "Previous incompatible - Current incompatible"
dat.use$TrialCondition[dat.use$prevTrialType == "incompatible" & dat.use$TrialType == "compatible"] = 
  "Previous incompatible - Current compatible"
dat.use$TrialCondition = factor(dat.use$TrialCondition)

# data set up in 8 blocks, trial goes 1-100 in each block
# add column for trial number across whole experiment so it goes 1-800
dat.use$ExpTrial = dat.use$Trial + (100*(dat.use$Block - 1))

# rescale trial
dat.use$Trial.begin = (dat.use$ExpTrial-2)/100
# shift trial to look at fixed effects at middle and end of task as well
dat.use$Trial.middle = dat.use$Trial.begin - 4
dat.use$Trial.end = dat.use$Trial.begin - 8


################################################################################################
#####################################Analysis Metropolis###########################################################
###############Just repetition varialbes and RT#############################
middle.1 = lm(RTdrop ~ TargetRep, data = dat.use)
middle.2 = lm(RTdrop ~ FlankerRep, data = dat.use)
middle.3 = lm(RTdrop ~ CompleteRep, data = dat.use)
middle.4 = lm(RTdrop ~ AnyRep, data = dat.use)
middle.5 = lm(RTdrop ~ TargetRep + FlankerRep, data = dat.use)

summary(middle.1)
summary(middle.2)
summary(middle.3)
summary(middle.4)
summary(middle.5)

x <- dat.use[c(32,31,29,30,35,36)]
y <- dat.use[c(32,31,29,30,35,36)]
cor(x, y)

#Adding a random slope. Should have done this above.
TargetRep.Rand = lmer(RTdrop ~ TargetRep + (1|Subject), data = dat.use)
summary(TargetRep.Rand)
anova(TargetRep.Rand)
FlankerRep.Rand = lmer(RTdrop ~ FlankerRep + (1|Subject), data = dat.use)
summary(FlankerRep.Rand)
anova(FlankerRep.Rand)

# Code used for calculating repetition variables with and without complete reps for SAS frequency/orthgonality checks.
#dat.use$TargetCompleteRep <- dat.use$TargetRep 
#dat.use$FlankerCompleteRep <- dat.use$FlankerRep 
#dat.use$TargetCompleteRep[dat.use$StimOnsetCode == dat.use$PrevStimOnsetCode] = 1
#dat.use$FlankerCompleteRep[dat.use$StimOnsetCode == dat.use$PrevStimOnsetCode] = 1
#write.table(dat.use, "RT_RepetitionCorrelations_forSAS.txt", sep = "\t", row.names = F)


#########Four models using covariate approach and including random slopes of subject.
#########These don't converge.
#TargetRep.Rand = lmer(RTdrop ~ Current.e * Previous.e * TargetRep + (Current.e*Previous.e|Subject), data = dat.use)
#summary(TargetRep.Rand)
#anova(TargetRep.Rand)

#FlankerRep.Rand = lmer(RTdrop ~ Current.e * Previous.e * FlankerRep + (Current.e*Previous.e*FlankerRep|Subject), data = dat.use)
#summary(FlankerRep.Rand)
#anova(FlankerRep.Rand)

#CompleteRep.Rand = lmer(RTdrop ~ Current.e * Previous.e * CompleteRep + (Current.e*Previous.e*CompleteRep|Subject), data = dat.use)
#summary(CompleteRep.Rand)
#anova(CompleteRep.Rand)

#AnyRep.Rand = lmer(RTdrop ~ Current.e * Previous.e * AnyRep + (Current.e*Previous.e*AnyRep|Subject), data = dat.use)
#summary(AnyRep.Rand)
#anova(AnyRep.Rand)


##############Same as above but only using  a random intercept of Subject.
TargetRep = lmer(RTdrop ~ Current.e * Previous.e * TargetRep + (1|Subject), data = dat.use)
summary(TargetRep)
anova(TargetRep)

FlankerRep = lmer(RTdrop ~ Current.e * Previous.e * FlankerRep + (1|Subject), data = dat.use)
summary(FlankerRep)
anova(FlankerRep)

#Doesn't work since correlated with trialtype.
#CompleteRep = lmer(RTdrop ~ Current.e * Previous.e * CompleteRep + (1|Subject), data = dat.use)
#summary(CompleteRep)
#anova(CompleteRep)

#Doesn't work since correlated with trialtype.
#AnyRep = lmer(RTdrop ~ Current.e * Previous.e * AnyRep + (1|Subject), data = dat.use)
#summary(AnyRep)
#anova(AnyRep)


###################Repetition by current by previous by trial###################.
TargetRep = lmer(RTdrop ~ Current.e * Previous.e * TargetRep * Trial.begin + (1|Subject), data = dat.use)
summary(TargetRep)

FlankerRep = lmer(RTdrop ~ Current.e * Previous.e * FlankerRep * Trial.begin + (1|Subject), data = dat.use)
summary(FlankerRep)

CompleteRep = lmer(RTdrop ~ Current.e * Previous.e * CompleteRep * Trial.begin + (1|Subject), data = dat.use)
summary(CompleteRep)


##########Breaking down four-way for Flanker rep as a function of trial.
FlankerRep_Begin = lmer(RTdrop ~ Current.e * Previous.e * FlankerRep * Trial.begin + (1|Subject), data = dat.use)
summary(FlankerRep_Begin)
FlankerRep_Mid = lmer(RTdrop ~ Current.e * Previous.e * FlankerRep * Trial.middle + (1|Subject), data = dat.use)
summary(FlankerRep_Mid)
FlankerRep_End = lmer(RTdrop ~ Current.e * Previous.e * FlankerRep * Trial.end + (1|Subject), data = dat.use)
summary(FlankerRep_End)
#######Breaking down the significant three-ways that occured at beginning and middle.
##At beginning.
FlankerRep_0 = lmer(RTdrop ~ Current.e * Previous.e * Trial.begin + (1|Subject), data = filter(dat.use, FlankerRep == 0))
summary(FlankerRep_0)
FlankerRep_1 = lmer(RTdrop ~ Current.e * Previous.e * Trial.begin + (1|Subject), data = filter(dat.use, FlankerRep == 1))
summary(FlankerRep_1)
##At middle. 
FlankerRep_0 = lmer(RTdrop ~ Current.e * Previous.e * Trial.middle + (1|Subject), data = filter(dat.use, FlankerRep == 0))
summary(FlankerRep_0)
FlankerRep_1 = lmer(RTdrop ~ Current.e * Previous.e * Trial.middle + (1|Subject), data = filter(dat.use, FlankerRep == 1))
summary(FlankerRep_1)

#######Breaking down the significant three-way for TARGET repetitions (a few paragraphs above).
##At beginning.
TargetRep_0 = lmer(RTdrop ~ Current.e * Previous.e + (1|Subject), data = filter(dat.use, TargetRep == 0))
summary(TargetRep_0)
TargetRep_1 = lmer(RTdrop ~ Current.e * Previous.e + (1|Subject), data = filter(dat.use, TargetRep == 1))
summary(TargetRep_1)



