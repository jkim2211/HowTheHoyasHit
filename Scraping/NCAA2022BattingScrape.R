#Part 1
#Credit to Robert Frey for his NCAA baseball scraping tutorial on Youtube (Twitter:RobertFrey40)
#devtools::install_github("BillPetti/baseballr")
library(baseballr)
#install.packages('tidyverse')
library(tidyverse)

#Data with D1 School IDs
d1_schools <- ncaa_team_lu %>% dplyr::filter(division == 1, year == 2022)

#Run function through errors
safe_ncaa_scrape <- purrr::safely(ncaa_scrape)

#Define function that uses safe_ncaa_scrape function, prints progress of scraping 
ncaa_scraper <- function(schoolid,school,type){
  if (type == "batting"){
    message(paste("Getting Batting Stats for ", school))
    
    stats <- safe_ncaa_scrape(teamid = schoolid,year = 2022, type = "batting")
  } else{
    message(paste("Getting Pitching stats for ",school))
    stats <- safe_ncaa_scrape(teamid = schoolid,year = 2022, type = "pitching")
  }
  
  Sys.sleep(sample(seq(.005,.02,.001),1))
  
  return(stats)
}

#gather data
batting_stats <- 1:nrow(d1_schools) %>% purrr::map(function(x) ncaa_scraper(d1_schools$school_id[x],
                                                                            d1_schools$school[x],
                                                                            type = "batting"))
#delete elements causing error
batting_stats <- batting_stats[-c(113,153)]

#turn data into data set
d1_batting_stats <- batting_stats %>% map("result") %>%
  bind_rows()

#Only player totals
d1_batting_player_stats <- d1_batting_stats %>% dplyr::filter(str_detect(Player,"(Totals)")==FALSE,
                                                       str_detect(Player,"(Opponent Totals)")==FALSE)
#Filtering data for players 
sapply(d1_batting_player_stats, class)

#Replace NA with 0
d1_batting_player_stats <- d1_batting_player_stats %>% mutate_if(is.numeric, replace_na, replace = 0)

#Create Plate Appearance (PA) column, filter for batters with at least 1 PA
d1_batting_player_stats <- d1_batting_player_stats %>% mutate(PA =AB+BB+HBP+SF+SH) %>% filter(PA>0)

readr::write_csv(d1_batting_player_stats,"d1_batting_players.csv") #CSV for player stats
readr::write_csv(d1_batting_stats,"d1_batting_stats.csv")
