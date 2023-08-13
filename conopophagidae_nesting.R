# Author: Odilon Vieira 
# Date: February 7, 2023

#libraries required
library(dplyr)
library(data.table)
library(ggplot2)

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

#View(nestingConopophagidaeStatistics)

#Specimens of Conopophaga cearae on MHNCE and MN collections with information about gonads
gonads = tibble(
  Voucher = c("MHNCE-0200","MHNCE-0201","MHNCE-0341",rep("MN-42745",2),rep("MN-42746",2),
              "MN-34554",rep("MN-34555",2),rep("MN-35001",2),"MN-36378","MN-36938",
              rep("MN-43276",2),"MN-43309"),
  width = c(2.8, 2.5, 5, 5.69, 5.08, 3.83, 3.43, 3.25, 1.68, 2.73, 3.05, 3.47, 3.63,
            3.52, 2.47, 2.66, 3.47),
  length = c(3, 7, 6.5, 8.70, 9.17, 8.18, 8.14, 4.6, 2.85, 4.13, 4.63, 5.43, 5,
             3.92, 4, 4, 3.88),
  sex = c(rep("Male",7), "Female",rep("Male",4),rep("Female",2),rep("Male",2),"Female"),
  monthNumber = c(10, 12, 1, rep(12,4), rep(2,5), 4, 2, rep(5,2), 7),
  monthText = c("Oct.", "Dec.", "Jan.", rep("Dec.",4), rep("Feb.",5), "Apr.", "Feb.", rep("May",2), "Jul.")
)

monthOrder = c("Aug.","Sep.","Oct.","Nov.",
               "Dec.","Jan.","Feb.","Mar.",
               "Apr.","May","Jun.","Jul.")
breeding = c("Dec.","Jan.","Feb.","Mar.")
#Plot the gonad 
ggplot(gonads) +
  aes(x = factor(monthText, levels = monthOrder), shape=sex) +
  geom_point(aes(y = width), colour="red", size=6, show.legend = F) +
  geom_point(aes(y = length), colour="black", size=6, show.legend = F) +
  xlab("Month") +
  ylab("Gonad size (mm)") +
  scale_x_discrete(breaks=monthOrder,drop=F) +
  scale_y_continuous(breaks = 0:10) +
  scale_shape_manual(values=c("Female"=17,"Male"=19)) +
  theme_bw() +
  theme(text = element_text(color="black", size=20),
    axis.text = element_text(color="black", size=20),
    axis.text.x = element_text(face=ifelse(monthOrder %in% breeding,"bold","plain")),
    panel.border = element_rect(colour="black", linewidth = 1))
