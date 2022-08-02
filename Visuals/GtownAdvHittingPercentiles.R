#Visuals Displaying Percentiles
library(readr)
library(dplyr)
library(tidyverse)
library(scales)

#Finding Percentiles for Teams
hittingStats <- read_csv("ADV_d1_batting_teams.csv") #CSV from 'NCAACalcAdvTeamStats' code 
hittingStats <- hittingStats %>% drop_na()

#Batting Average
summary(hittingStats$BA)
ecdf(hittingStats$BA)(.269) #35th percentile
#OBP
summary(hittingStats$OBPct)
ecdf(hittingStats$OBPct)(.379) #59th perentile
#SLG
summary(hittingStats$SlgPct)
ecdf(hittingStats$SlgPct)(.481) #83rd percentile
#wOBA
summary(hittingStats$wOBA)
ecdf(hittingStats$wOBA)(.386) #74th percentile
#wRAA
summary(hittingStats$wRAA)
ecdf(hittingStats$wRAA)(33.1) #74th percentile
#wRC_plus
quantile(hittingStats$wRC_plus, c(.25,.50,.75,1.00)) 
summary(hittingStats$wRC_plus)
ecdf(hittingStats$wRC_plus)(107) #76th percentile
#BABIP
summary(hittingStats$BABIP)
ecdf(hittingStats$BABIP)(.323) #33rd percentile
#ISO
summary(hittingStats$ISO)
ecdf(hittingStats$ISO)(.212) #94th percentile
#create DF
Metric <- c("BA", "OBP", "SLG", "wOBA","wRAA", "wRC_plus","BABIP","ISO")
Percentile <- c(.35,.59,.83,.74,.74,.76,.33,.94)
percentiles <- data.frame(Metric,Percentile) %>% arrange(desc(Percentile))

#Plot
ggplot(percentiles, aes(x=reorder(Metric,Percentile),y=Percentile))+
  geom_point(col="green",size=14)+
  geom_point(data=percentiles[6,],col="yellow",size=14)+
  geom_point(data=percentiles[7:8,],col="red",size=14)+
  ylim(0,1)+
  ggtitle("Georgetown Hitting Percentiles in Division 1 Baseball")+
  theme(legend.position="none")+
  geom_text(aes(label=percent(Percentile,1)),vjust=.5,color="black",size=5)+
  coord_flip()+
  ylab("Percentile")+
  xlab("Hitting Metric")


#Finding Percentiles for each player
hittingStats <- read_csv("Baseball Stuff 22/ADV_d1_batting_players.csv") #CSV created in 'NCAACalcAdvPlayerStats' code
hittingStats$BAPercentile<- round(ecdf(hittingStats$BA)(hittingStats$BA),2)
hittingStats$OBPpercentile<- round(ecdf(hittingStats$OBPct)(hittingStats$OBPct),2)
hittingStats$SlgPercentile <- round(ecdf(hittingStats$SlgPct)(hittingStats$SlgPct),2)
hittingStats$wOBApercentile <- round(ecdf(hittingStats$wOBA)(hittingStats$wOBA),2)
hittingStats$wRAApercentile <- round(ecdf(hittingStats$wRAA)(hittingStats$wRAA),2)
hittingStats$wRCpercentile <- round(ecdf(hittingStats$wRC_plus)(hittingStats$wRC_plus),2)
hittingStats$BABIPpercentile <- round(ecdf(hittingStats$BABIP)(hittingStats$BABIP),2) 
hittingStats$ISOpercentile <- round(ecdf(hittingStats$ISO)(hittingStats$ISO),2)
  #Filter for only Georgetown players, select certain stats
hittingStats <- hittingStats %>% filter(school=="Georgetown") %>% select(Player,BAPercentile,OBPpercentile,SlgPercentile,wOBApercentile,wRAApercentile,wRCpercentile,BABIPpercentile,ISOpercentile)

