#Part 3: Calculating NCAA Team Stats
library(tidyverse)
library(baseballr)
library(readr)
library(dplyr)
#Gather team stats
d1_batting_teams <- read_csv("d1_batting_stats.csv") #CSV created in 'NCAA2022BattingScrape' file
d1_batting_teams <- d1_batting_2022 %>% filter(str_detect(Player,"Totals"))
d1_batting_teams <- d1_team_batting %>% filter(str_detect(Player, "Opponent Totals"))

#remove empty columns
d1_batting_teams <- subset(d1_team_batting, select = -c(Jersey,Yr,Pos,GP,GS,player_id,player_url))

#calculate TB for teams that are missing it
d1_batting_teams <- d1_team_batting %>%
  mutate(TB=(H-d1_team_batting$'2B'-d1_team_batting$'3B'-HR)+2*d1_team_batting$'2B'+3*d1_team_batting$'3B'+4*HR)

d1_batting_teams <- d1_team_batting %>% mutate_if(is.numeric, replace_na, replace = 0)


#Calculating advanced metrics for teams
#Stat 1: WOBA


#obtain IBB column for teams
d1_batting_players <- read_csv("ADV_d1_batting_players.csv") # CSV created in 'NCAACalcAdvPlayerStats' code

teamIBB <- d1_batting_players %>% group_by(school) %>% summarise(IBB = sum(IBB))

d1_batting_teams <- left_join(d1_batting_teams, teamIBB, by = "school")



#Create singles column
d1_batting_teams$'1B' = d1_batting_teams$H-d1_batting_teams$'2B'-d1_batting_teams$'3B'-d1_batting_teams$HR

#team ABs
teamABs <- d1_batting_players %>% group_by(school) %>% summarise(AB= sum(AB))
d1_batting_teams <- left_join(d1_batting_teams, teamABs, by = "school")
d1_batting_teams <- d1_batting_teams %>% mutate(PA = AB+BB+HBP+SF+SH)

#calculate wOBA (2022 linear weights)
get_woba <- function(df) {
  df$wOBA <- round(((.81 * df$BB) + (.83 * df$HBP) + (.95 * df$'1B') + (1.26 * df$'2B') + 	(1.54 * df$'3B') + (1.76 * df$HR))/(df$PA-df$IBB),3)
  return(df)
}

d1_batting_teams <- get_woba(d1_batting_teams)

d1_batting_teams %>% filter(AB >= 200) %>% arrange(desc(wOBA)) %>% select(school, conference, Player, wOBA) %>% top_n(10)


#Stat #2: wRAA, lg wOBA and wOBA scale are from 2022
get_wRAA <- function(df) {
  df$wRAA <- round((((df$wOBA-.372)/.986)*df$PA),1)
  return(df)
}
d1_batting_teams <- get_wRAA(d1_batting_teams)

#Stat #3: wRC+
d1_schools <- ncaa_team_lu %>% dplyr::filter(division == 1, year == 2022)
possibly_park_scrape <- purrr::possibly(.f = get_ncaa_park_factor, otherwise = NULL)
parkFactors <- 1:nrow(d1_schools) %>% purrr::map(function(x) possibly_park_scrape(d1_schools$school_id[x],c(2019:2020)))
parkFactors <- bind_rows(parkFactors)
parkFactors <- parkFactors %>% select(school, final_pf)
d1_batting_teams <- left_join(d1_batting_teams,parkFactors,"school")

#runsPA by conference (as part of wRC+ formula)
runsPAConfDF <- d1_batting_teams %>% group_by(conference) %>% summarise(runsPAConf=sum(R)/sum(PA))
d1_batting_teams <- left_join(d1_batting_teams,runsPAConfDF,"conference")


d1_batting_teams <- d1_batting_teams %>% mutate(wRC_plus = ((((wRAA/PA)+.158)+(.158-(final_pf*.158)))/(runsPAConf))*100)
d1_batting_teams <- d1_batting_teams %>% mutate(wRC_plus = round(wRC_plus),2)
d1_batting_teams %>% filter(school == "Georgetown") %>% arrange(desc(wRC_plus)) %>% select(Player, wRC_plus)

#Stat 4: BABIP
d1_batting_teams <- d1_batting_teams %>% mutate(BABIP = (H-HR)/(AB-HR-K+SF))
d1_batting_teams <- d1_batting_teams %>% mutate(BABIP = round(BABIP,3))

#Stat 5: ISO
d1_batting_teams <- d1_batting_teams %>% mutate(ISO = SlgPct - BA)

#Stat 6: wRC
d1_batting_teams <- d1_batting_teams %>% mutate(wRC = (((wOBA-.372)/.986)+.158)*PA)
d1_batting_teams$wRC <- round(d1_batting_teams$wRC)

#Save as CSV
d1_batting_teams <- d1_batting_teams %>% select(-c(final_pf, runsPAConf,'2'))
readr::write_csv(ADV_d1_batting_teams,"ADV_d1_batting_teams.csv")


