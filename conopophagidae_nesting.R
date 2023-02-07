# Author: Odilon Vieira 
# Date: February 7, 2023

#libraries required
library(dplyr)
library(data.table)

#measurements datasets
conopophagidaeNest = fread("nest_conopophagidae.csv",check.names = T)
conopophagidaeEggs = fread("eggs_conopophagidae.csv",check.names = T)
conopophagidaeClutches = select(conopophagidaeEggs, c(Species, Clutch.Size, Clutches)) %>%
  na.omit()

#function to calculate the statistics (mean, sample count, standard deviation and 
#range) of a sample
statistics = function(smpl){
  smplLength = length(na.omit(smpl))
  if(smplLength > 1){
    m = round(mean(smpl, na.rm = T),2)
    rng = paste(range(smpl,na.rm = T), collapse = "–")
    stdv = round(sd(smpl, na.rm = T),2) 
    if(stdv > 0){
      formated = paste(m," (± ",stdv,", ",rng,", n = ",smplLength,")", sep="") 
    }else{
      formated = paste(m," (n = ",smplLength,")",sep="")
    }
  }else if (smplLength == 1){
    m = rng = na.omit(smpl)[1]
    formated = paste(m," (n = ",smplLength,")",sep="")
    stdv = 0
  }else{
    m = rng = stdv = formated = NA
  }
  
  list(
    mean = m,
    count = smplLength,
    range = rng,
    standardDeviation = stdv,
    textFormat =  as.character(formated)
  )
}

#replicates the clutch sizes in relate to the number of clutches
totalClutches = data.frame()
for(r in 1:nrow(conopophagidaeClutches)){
  clutch = conopophagidaeClutches[r,]
  clutchInfo = clutch[,1:2]
  clutchCount = as.numeric(clutch[,3])

  if(clutchCount > 1){
    clutchRep = data.frame(Species=rep(as.character(clutchInfo[,1]),clutchCount),
                           Clutch.Size=rep(as.numeric(clutchInfo[,2]),clutchCount))
    totalClutches = rbind(totalClutches,clutchRep)
  }else{
    totalClutches = rbind(totalClutches,clutchInfo)
  }
}
#format sources
sources = select(conopophagidaeEggs,c(Species,Source)) %>%
  bind_rows(select(conopophagidaeNest,c(Species,Source))) %>%
  distinct_all() %>%
  group_by(Species) %>%
  summarise(Source = paste(Source, collapse = ", "))

#statistics for nest
summarizedNests_txt = select(conopophagidaeNest, -Source) %>%
  group_by(Species) %>%
  summarise_all(function(x){statistics(x)$textFormat})
#statistics for eggs
summarizedEggs_txt = select(conopophagidaeEggs, -c(Source, Clutch.Size, Clutches)) %>%
  group_by(Species) %>%
  summarise_all(function(x){statistics(x)$textFormat})
#statistics for clutchess
summarizedClutches_txt = group_by(totalClutches, Species) %>%
  summarise(clutchSize = statistics(Clutch.Size)$textFormat)
#merge datasets
nestingConopophagidaeStatistics = left_join(summarizedNests_txt,summarizedEggs_txt,by="Species") %>%
  left_join(summarizedClutches_txt,by="Species") %>%
  left_join(sources,by="Species")
