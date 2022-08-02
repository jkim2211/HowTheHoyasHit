#Part 2: Calculating NCAA Player Stats 
#Credit to Robert Frey for his NCAA baseball scraping tutorial on Youtube (Twitter:RobertFrey40)
library(tidyverse)
library(baseballr)
library(readr)
library(dplyr)
library(rvest)


#Stat 1: WOBA
d1_batting_players <- read_csv("d1_batting_players.csv") #CSV created in 'NCAA2022BattingScrape'code
 

#obtain IBB column
get_IBB <- function(player_id,year) {
  year_id <- subset(ncaa_season_id_lu, season == year, select = id)
  batting_id <- subset(ncaa_season_id_lu, season == year, select = batting_id)
  batting_url <- paste0("http://stats.ncaa.org/player/index?id=", year_id,"&stats_player_seq=",player_id,"&year_stat_category_id=", batting_id)
  batting_payload <- xml2::read_html(batting_url)
    payload_df <- batting_payload %>%
      rvest::html_nodes('table') %>%
      .[3] %>%
      rvest::html_table(fill=T) %>%
      as.data.frame() %>%
      .[-1,]
    
    names(payload_df) <- payload_df[1,]
    payload_df <- payload_df[-1,]
    year_df = data.frame(Year = c("2021-22"),
                         Season = c(2022), stringsAsFactors = F)
    payload_df <- left_join(payload_df,year_df,by="Year")
    ibb <- payload_df %>% filter(Season== {{year}}) %>% pull(IBB)
    
  return(ibb)
}

get_IBB <- purrr::safely(get_IBB)

IBBTeam <- d1_batting_players %>% group_by(team) %>% summarise(teamIBB = sum(IBB))
#install.packages("svMisc")
for (i in 1:nrow(d1_batting_players)) {
  svMisc::progress(i,nrow(d1_batting_players))
  d1_batting_players$IBB[[i]] <- get_IBB(d1_batting_players$player_id[i],d1_batting_players$year[i])
}

d1_batting_players$IBB[[1]]
get_IBB(d1_batting_players$player_id[1],d1_batting_players$year[1])


#Edit IBB
d1_batting_players$IBB <- sub('", error = NULL.*',"",d1_batting_players$IBB)
d1_batting_players$IBB <- sub(".*result = ","",d1_batting_players$IBB)
d1_batting_players$IBB <- sub('"',"",d1_batting_players$IBB)
d1_batting_players$IBB <- as.numeric(d1_batting_players$IBB)
d1_batting_players <- d1_batting_players %>% replace_na(list(IBB=0))

#Create singles column
d1_batting_players$'1B' = d1_batting_players$H-d1_batting_players$'2B'-d1_batting_players$'3B'-d1_batting_players$HR
d1_batting_players %>%
  mutate(PA = AB+BB+HBP+SF+SH)

#calculate wOBA (2022 linear weights, courtesy of Driveline's David Besky)
get_woba <- function(df) {
  df$wOBA <- round((((.81 * df$BB) + (.83 * df$HBP) + (.95 * df$'1B') + (1.26 * df$'2B') + 	(1.54 * df$'3B') + (1.76 * df$HR))/(df$PA-df$IBB)),3)
  return(df)
}

d1_batting_players <- get_woba(d1_batting_players)
d1_batting_players %>% filter(AB >= 200) %>% arrange(desc(wOBA)) %>% select(school, conference, Player, wOBA) %>% top_n(10)


#Stat #2: wRAA, lg wOBA and wOBA scale are from 2022
get_wRAA <- function(df) {
  df$wRAA <- round((((df$wOBA-.372)/.986)*df$PA),1)
  return(df)
}
d1_batting_players <- get_wRAA(d1_batting_players)

#list stat leaders
d1_batting_players %>% filter(AB >= 200) %>% arrange(desc(wRAA)) %>% select(school, conference, Player, wRAA) %>% top_n(10)

#Stat #3: wRC+
d1_schools <- ncaa_team_lu %>% dplyr::filter(division == 1, year == 2022)
possibly_park_scrape <- purrr::possibly(.f = get_ncaa_park_factor, otherwise = NULL)


parkFactors <- 1:nrow(d1_schools) %>% purrr::map(function(x) possibly_park_scrape(d1_schools$school_id[x],c(2019:2020)))

parkFactors <- bind_rows(parkFactors)
parkFactors <- parkFactors %>% select(school, final_pf)
d1_batting_players <- left_join(d1_batting_players,parkFactors,"school")
  #runsPA by conference
runsPAConfDF <- d1_batting_players %>% group_by(conference) %>% summarise(runsPAConf=sum(R)/sum(PA))
d1_batting_players <- left_join(d1_batting_players,runsPAConfDF,"conference")


d1_batting_players <- d1_batting_players %>% mutate(wRC_plus = ((((wRAA/PA)+.158)+(.158-(final_pf*.158)))/(runsPAConf))*100)
d1_batting_players <- d1_batting_players %>% mutate(wRC_plus = round(wRC_plus, 2))
d1_batting_players %>% filter(school == "Georgetown") %>% arrange(desc(wRC_plus)) %>% select(Player, wRC_plus)


#Stat 4: BABIP
d1_batting_players <- d1_batting_players %>% mutate(BABIP = (H-HR)/(AB-HR-K+SF))
d1_batting_players <- d1_batting_players %>% mutate(BABIP = round(BABIP,3))

#Stat 5: ISO
d1_batting_players <- d1_batting_players %>% mutate(ISO = SlgPct - BA)

#Stat 6: wRC
d1_batting_players <- d1_batting_players %>% mutate(wRC = (((wOBA-.372)/.986)+.158)*PA)
d1_batting_players$wRC <- round(d1_batting_players$wRC)

#Save as CSV
d1_batting_players <- d1_batting_players %>% select(-c(final_pf,runsPAConf,'2'))
readr::write_csv(d1_batting_players,"ADV_d1_batting_players.csv")

