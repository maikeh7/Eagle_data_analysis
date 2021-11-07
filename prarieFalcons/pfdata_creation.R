pfdat = read.csv("C:/Users/Maike/Box Sync/for_Pappa/prarieFalcons/PFBehavior8487.csv")
pfdat = filter(pfdat, ACTIV %in% c(80, 81, 9))
newcol = paste(pfdat$YEARN, pfdat$AREA, pfdat$LOCAT, pfdat$DATEN, sep = "-")
pfdat$ID = newcol

#take out weird row
badrow = pfdat[3255, ]
pfdat = pfdat[-3255, ]
pfdat$DIF = as.numeric(as.character(pfdat$DIF))

#we do not want stage 4
pfdat = filter(pfdat, STAGE != 4)
#add 60 to SUNT
pfdat$SUNT = pfdat$SUNT + 60

###########################
#main data construction
###########################
84-1-1-840605


bads=c("84-1-1-840506")
day1 = filter(pfdat, ID == bads)
day1
d=1
i=7
j=2



PFIDS = unique(pfdat$ID)

counter = 1
biglist=list()
for (d in 1:length(PFIDS)){
  
  day1 = filter(pfdat, ID == PFIDS[d])
    Males = filter(day1, SEX == 1)
    Females = filter(day1, SEX == 2)
    if ((dim(Females)[1] > 0) & (dim(Males)[1] > 0 )){
      print("males and females dfs")
      indexDF = data.frame()
      for (i in 1:nrow(Males)){
        MaleTime = Males[i, ]$TIME2:Males[i,]$TIME1
        for (j in 1:nrow(Females)){
          FemaleTime = Females[j, ]$TIME2:Females[j,]$TIME1
          if (length(intersect(MaleTime, FemaleTime))>0){
          #if (MaleTime <= Females$TIME1[j] & MaleTime >= Females$TIME2[j]){
            
            print("duplicate")
            indices = c(i, j)
            indexDF = rbind(indexDF, indices)
          }
          
        }
       
      }
      if(dim(indexDF)[1] > 0){
   
      names(indexDF) = c("Maledf", "Femaledf")
      MaleNew = Males[indexDF$Maledf, ]

      for (k in 1:nrow(indexDF)){
        MaleGone = Males[indexDF$Maledf[k], ]$TIME2:Males[indexDF$Maledf[k], ]$TIME1
        FemaleGone =  Females[indexDF$Femaledf[k], ]$TIME2:Females[indexDF$Femaledf[k], ]$TIME1
        bothGone = intersect(MaleGone, FemaleGone)
        GoneLength = length(bothGone)

        MaleNew$SEX[k] = 12
        MaleNew$DIF[k] = GoneLength
        MaleNew$TIME2[k] = bothGone[1]
        MaleNew$TIME1[k] = bothGone[length(bothGone)]
        }
      alldf = rbind(MaleNew, Males, Females)
      }else{
        print("no duplicates")
        alldf = day1
        newrow = day1[1, ]
        newrow$SEX[1] = 12
        newrow$TIME1 = 0
        newrow$TIME2 = 0
        newrow$DIF = 0 #add in zeros for both where appropriate
        alldf = rbind(alldf, newrow)
      }
    }
    #if no females OR no males...then add a zero for no  males/females to the data
    else{
      if (dim(Males)[1] > 0){
        alldf = Males
        newrow = alldf[1, ]
        newrow2 = alldf[1, ]
        newrow2$SEX[1] = 2
        newrow2$TIME1 = 0
        newrow2$TIME2 = 0
        newrow2$DIF = 0
        alldf = rbind(alldf, newrow2)
      }else{
        alldf = Females
        newrow = alldf[1, ]
      }
      newrow$SEX[1] = 12
      newrow$TIME1 = 0
      newrow$TIME2 = 0
      newrow$DIF = 0
      alldf = rbind(alldf, newrow)
    }
    
    biglist[[counter]] = alldf
    counter = counter +1
}
library(data.table)



pftest = rbindlist(biglist)
#merge w/ new day length dataset

nrow(pftest)
head(pftest)
daylengthNew = read.csv("C:/Users/Maike/Box Sync/for_Pappa/prarieFalcons/DayLengthNew.csv")
daylengthNew = daylengthNew %>% dplyr::select(YEARN, AREA, LOCAT, DATEN, Day_Length)
newcol = paste(daylengthNew$YEARN, daylengthNew$AREA, daylengthNew$LOCAT, daylengthNew$DATEN, sep = "-")
daylengthNew$ID = newcol
daylengthNew = filter(daylengthNew, ID %in% PFIDS)
daylengthNew = daylengthNew[, c("Day_Length", "ID")]
head(daylengthNew)
pftest2= right_join(pftest, daylengthNew, by = "ID")
head(pftest2)
pftest2$SUNT=NULL

write.csv(pftest2, "pfRawDat.csv")
pftest = pftest2
falconIDs = pftest[!(duplicated(pftest$ID)), ] %>% dplyr::select(DAYN, STAGE, Day_Length, ID)

pfsums = pftest %>% group_by(ID, SEX) %>% summarise(SumAbsent = sum(DIF))
pfmerge = right_join(pfsums, falconIDs, by = "ID")
head(pfmerge)
#calculate percent absent
pfmerge$PercentAbsent = pfmerge$SumAbsent / (pfmerge$Day_Length)
write.csv(pfmerge, "pfmerge.csv")
overdf = pfmerge[which(pfmerge$PercentAbsent > 1), ] #132 obs over 100 %
#set values of percent absent > 1 to 1
pfmerge$PercentAbsent_Corr = ifelse(pfmerge$PercentAbsent > 1, 1, pfmerge$PercentAbsent)

#fix days to correspond to day ranges
DayRanges = read.csv("DayRanges.csv")

gooddays = as.data.frame(filter(pfmerge, DAYN %in% DayRanges$Day)) #days for which DAYN is ok
otherdays = filter(pfmerge, !(DAYN %in% DayRanges$Day)) #need to change DAYN for these obs
altList = list()

for (k in 1:3){
  stageDF = filter(otherdays, STAGE == k)
  corrNameDF = filter(DayRanges, STAGE == k)
  for (j in 1:nrow(stageDF)){
    testval = stageDF$DAYN[j]
    for(i in 1:nrow(corrNameDF)){
      if( (testval >= corrNameDF$Lower.Range[i]) & (testval <= corrNameDF$Upper.Range[i])){
        print('true')
        newval = corrNameDF$Day[i]
        stageDF$DAYN[j] = newval
        #counter = counter+1
      }
    }
    altList[[k]] = stageDF
  }
}

newnamedf = rbindlist(altList)
pfmergeCorr = rbind(gooddays, newnamedf)
merge3 = filter(pfmergeCorr, STAGE == 3)

#ADD 34 to DAYN for STAGE == 3
merge3$DAYN = merge3$DAYN + 34
merge12 = filter(pfmergeCorr, STAGE %in% c(1,2))
pfmergeCorr = rbind(merge12, merge3)

#get no. of days by stage, dayn, sex
numdays = pfmergeCorr %>% group_by(STAGE, DAYN, SEX) %>% tally()

#calculate mean/sd by stage, dayn, sex
pfMetrics = pfmergeCorr %>% group_by(STAGE, DAYN, SEX) %>% summarise(meanPropAbsent = mean(PercentAbsent_Corr),
                                                                 sdPropAbsent = sd(PercentAbsent_Corr))

#set NAN SDs == 0
pfMetrics$SDnoNANS = ifelse(is.na(pfMetrics$sdPropAbsent), 0 , pfMetrics$sdPropAbsent)
pfMetrics = right_join(pfMetrics, numdays, by = c("STAGE", "DAYN", "SEX"))
#calculate SD
pfMetrics$SE = pfMetrics$SDnoNANS / sqrt(pfMetrics$n)
write.csv(pfmergeCorr, "pfmergeCorr.csv")
write.csv(pfMetrics, "pfMetrics.csv")

males = filter(pfMetrics, SEX == 1)
females = filter(pfMetrics, SEX == 2)
parents = filter(pfMetrics, SEX == 12)
plot(males$meanPropAbsent, type = "l")

test = filter(pfmerge, DAYN == -53)
ggplot(data = pfMetrics, aes(x = DAYN, y = meanPropAbsent)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~SEX)


