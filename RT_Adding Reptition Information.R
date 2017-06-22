# Identifies repetition trials in RT data

# read in quantified N2 TBT data
RTdat = read.delim("FlankerRT_forR.txt")

RTdat$PrevStimOnsetCode = NA
for (i in unique(RTdat$Subject)){   # subject
  for (j in 1:8) {                  # block
    for (k in 2:100) {                # trial
      RTdat$PrevStimOnsetCode[RTdat$Subject == i & RTdat$Block == j & RTdat$Trial == k] = 
        RTdat$StimOnsetCode[RTdat$Subject == i & RTdat$Block == j & RTdat$Trial == k-1]
    }
  }
}


#Adding repetition type.
#Comp: Target Left, Flanker Left = 110.
#Comp: Target Right, Flanker Right = 130.
#Incomp: Target Left, Flanker Right = 150.
#Incomp: Target Right, Flanker Left = 170.
RTdat$CompleteRep = NA
RTdat$CompleteRep[RTdat$StimOnsetCode == 110 & RTdat$PrevStimOnsetCode == 110] = 1
RTdat$CompleteRep[RTdat$StimOnsetCode == 110 & RTdat$PrevStimOnsetCode == 130] = 0
RTdat$CompleteRep[RTdat$StimOnsetCode == 110 & RTdat$PrevStimOnsetCode == 150] = 0
RTdat$CompleteRep[RTdat$StimOnsetCode == 110 & RTdat$PrevStimOnsetCode == 170] = 0
RTdat$CompleteRep[RTdat$StimOnsetCode == 130 & RTdat$PrevStimOnsetCode == 110] = 0
RTdat$CompleteRep[RTdat$StimOnsetCode == 130 & RTdat$PrevStimOnsetCode == 130] = 1
RTdat$CompleteRep[RTdat$StimOnsetCode == 130 & RTdat$PrevStimOnsetCode == 150] = 0
RTdat$CompleteRep[RTdat$StimOnsetCode == 130 & RTdat$PrevStimOnsetCode == 170] = 0
RTdat$CompleteRep[RTdat$StimOnsetCode == 150 & RTdat$PrevStimOnsetCode == 110] = 0
RTdat$CompleteRep[RTdat$StimOnsetCode == 150 & RTdat$PrevStimOnsetCode == 130] = 0
RTdat$CompleteRep[RTdat$StimOnsetCode == 150 & RTdat$PrevStimOnsetCode == 150] = 1
RTdat$CompleteRep[RTdat$StimOnsetCode == 150 & RTdat$PrevStimOnsetCode == 170] = 0
RTdat$CompleteRep[RTdat$StimOnsetCode == 170 & RTdat$PrevStimOnsetCode == 110] = 0
RTdat$CompleteRep[RTdat$StimOnsetCode == 170 & RTdat$PrevStimOnsetCode == 130] = 0
RTdat$CompleteRep[RTdat$StimOnsetCode == 170 & RTdat$PrevStimOnsetCode == 150] = 0
RTdat$CompleteRep[RTdat$StimOnsetCode == 170 & RTdat$PrevStimOnsetCode == 170] = 1

RTdat$AnyRep = NA
RTdat$AnyRep[RTdat$StimOnsetCode == 110 & RTdat$PrevStimOnsetCode == 110] = 1
RTdat$AnyRep[RTdat$StimOnsetCode == 110 & RTdat$PrevStimOnsetCode == 130] = 0
RTdat$AnyRep[RTdat$StimOnsetCode == 110 & RTdat$PrevStimOnsetCode == 150] = 1
RTdat$AnyRep[RTdat$StimOnsetCode == 110 & RTdat$PrevStimOnsetCode == 170] = 1
RTdat$AnyRep[RTdat$StimOnsetCode == 130 & RTdat$PrevStimOnsetCode == 110] = 0
RTdat$AnyRep[RTdat$StimOnsetCode == 130 & RTdat$PrevStimOnsetCode == 130] = 1
RTdat$AnyRep[RTdat$StimOnsetCode == 130 & RTdat$PrevStimOnsetCode == 150] = 1
RTdat$AnyRep[RTdat$StimOnsetCode == 130 & RTdat$PrevStimOnsetCode == 170] = 1
RTdat$AnyRep[RTdat$StimOnsetCode == 150 & RTdat$PrevStimOnsetCode == 110] = 1
RTdat$AnyRep[RTdat$StimOnsetCode == 150 & RTdat$PrevStimOnsetCode == 130] = 1
RTdat$AnyRep[RTdat$StimOnsetCode == 150 & RTdat$PrevStimOnsetCode == 150] = 1
RTdat$AnyRep[RTdat$StimOnsetCode == 150 & RTdat$PrevStimOnsetCode == 170] = 0
RTdat$AnyRep[RTdat$StimOnsetCode == 170 & RTdat$PrevStimOnsetCode == 110] = 1
RTdat$AnyRep[RTdat$StimOnsetCode == 170 & RTdat$PrevStimOnsetCode == 130] = 1
RTdat$AnyRep[RTdat$StimOnsetCode == 170 & RTdat$PrevStimOnsetCode == 150] = 0
RTdat$AnyRep[RTdat$StimOnsetCode == 170 & RTdat$PrevStimOnsetCode == 170] = 1

RTdat$TargetRep = NA
RTdat$TargetRep[RTdat$StimOnsetCode == 110 & RTdat$PrevStimOnsetCode == 110] = 0
RTdat$TargetRep[RTdat$StimOnsetCode == 110 & RTdat$PrevStimOnsetCode == 130] = 0
RTdat$TargetRep[RTdat$StimOnsetCode == 110 & RTdat$PrevStimOnsetCode == 150] = 1
RTdat$TargetRep[RTdat$StimOnsetCode == 110 & RTdat$PrevStimOnsetCode == 170] = 0
RTdat$TargetRep[RTdat$StimOnsetCode == 130 & RTdat$PrevStimOnsetCode == 110] = 0
RTdat$TargetRep[RTdat$StimOnsetCode == 130 & RTdat$PrevStimOnsetCode == 130] = 0
RTdat$TargetRep[RTdat$StimOnsetCode == 130 & RTdat$PrevStimOnsetCode == 150] = 0
RTdat$TargetRep[RTdat$StimOnsetCode == 130 & RTdat$PrevStimOnsetCode == 170] = 1
RTdat$TargetRep[RTdat$StimOnsetCode == 150 & RTdat$PrevStimOnsetCode == 110] = 1
RTdat$TargetRep[RTdat$StimOnsetCode == 150 & RTdat$PrevStimOnsetCode == 130] = 0
RTdat$TargetRep[RTdat$StimOnsetCode == 150 & RTdat$PrevStimOnsetCode == 150] = 0
RTdat$TargetRep[RTdat$StimOnsetCode == 150 & RTdat$PrevStimOnsetCode == 170] = 0
RTdat$TargetRep[RTdat$StimOnsetCode == 170 & RTdat$PrevStimOnsetCode == 110] = 0
RTdat$TargetRep[RTdat$StimOnsetCode == 170 & RTdat$PrevStimOnsetCode == 130] = 1
RTdat$TargetRep[RTdat$StimOnsetCode == 170 & RTdat$PrevStimOnsetCode == 150] = 0
RTdat$TargetRep[RTdat$StimOnsetCode == 170 & RTdat$PrevStimOnsetCode == 170] = 0

RTdat$FlankerRep = NA
RTdat$FlankerRep[RTdat$StimOnsetCode == 110 & RTdat$PrevStimOnsetCode == 110] = 0
RTdat$FlankerRep[RTdat$StimOnsetCode == 110 & RTdat$PrevStimOnsetCode == 130] = 0
RTdat$FlankerRep[RTdat$StimOnsetCode == 110 & RTdat$PrevStimOnsetCode == 150] = 0
RTdat$FlankerRep[RTdat$StimOnsetCode == 110 & RTdat$PrevStimOnsetCode == 170] = 1
RTdat$FlankerRep[RTdat$StimOnsetCode == 130 & RTdat$PrevStimOnsetCode == 110] = 0
RTdat$FlankerRep[RTdat$StimOnsetCode == 130 & RTdat$PrevStimOnsetCode == 130] = 0
RTdat$FlankerRep[RTdat$StimOnsetCode == 130 & RTdat$PrevStimOnsetCode == 150] = 1
RTdat$FlankerRep[RTdat$StimOnsetCode == 130 & RTdat$PrevStimOnsetCode == 170] = 0
RTdat$FlankerRep[RTdat$StimOnsetCode == 150 & RTdat$PrevStimOnsetCode == 110] = 0
RTdat$FlankerRep[RTdat$StimOnsetCode == 150 & RTdat$PrevStimOnsetCode == 130] = 1
RTdat$FlankerRep[RTdat$StimOnsetCode == 150 & RTdat$PrevStimOnsetCode == 150] = 0
RTdat$FlankerRep[RTdat$StimOnsetCode == 150 & RTdat$PrevStimOnsetCode == 170] = 0
RTdat$FlankerRep[RTdat$StimOnsetCode == 170 & RTdat$PrevStimOnsetCode == 110] = 1
RTdat$FlankerRep[RTdat$StimOnsetCode == 170 & RTdat$PrevStimOnsetCode == 130] = 0
RTdat$FlankerRep[RTdat$StimOnsetCode == 170 & RTdat$PrevStimOnsetCode == 150] = 0
RTdat$FlankerRep[RTdat$StimOnsetCode == 170 & RTdat$PrevStimOnsetCode == 170] = 0

write.table(RTdat, "RT_AllSubs_TBT_Cond_Prev_Rep.txt", sep = "\t", row.names = F)
