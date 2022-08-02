#Team Hitting metric correlations

#packages
library(utils)
library(ggplot2)
library(tidyverse)
library(ggrepel)

#import data
team_hitting <- read_csv("ADV_d1_batting_teams.csv") #CSV from 'NCAACalcAdvTeamStats' code

#wOBA and Runs correlation, Runs, R^2 = .9227
ggplot() +
  geom_point(data = team_hitting, aes(x=wOBA, y=RunsPerGame)) +
  geom_smooth(data = team_hitting, aes(x=wOBA, y=RunsPerGame),method = "lm") +
  geom_label(
    data = team_hitting, aes(x=wOBA, y=RunsPerGame),
    label = "R^2 = .9227",
    x = .34,
    y=9,
    label.padding = unit(.5, "lines"),
    color = "blue") +
  geom_point(aes(x=.386,y=7.142857),
             color='red',
             size=2, show.legend = TRUE
  ) +
  geom_label_repel(aes(x = .386, y = 7.142857),
                   color = "red",label="Georgetown", nudge_x = -.04
  )+
  ggtitle("D1 Baseball Weighted On-base Average")
  
Lm <- lm(team_hitting$RunsPerGame ~ team_hitting$wOBA)
modsum = summary(Lm)
print(modsum)

#wRC+, RunsPerGame, R^2 = .6194
wrcTeamHitting <- team_hitting %>% drop_na()
ggplot() +
  geom_point(data = wrcTeamHitting, aes(x=wRC_plus, y= RunsPerGame)) +
  geom_smooth(data = wrcTeamHitting, aes(x=wRC_plus, y= RunsPerGame),method = "lm") +
  geom_label(
    data = wrcTeamHitting, aes(x=wRC_plus, y= RunsPerGame),
    label = "R^2 = .6194",
    x = 60,
    y=8.5,
    label.padding = unit(.5, "lines"),
    color = "blue",
    ) +
  geom_point(aes(x=107,y=7.142857),
             color='red',
             size=2, show.legend = TRUE
    
  ) +
  geom_label_repel(aes(x = 107, y = 7.142857),
     color = "red",label="Georgetown", nudge_x = 20, nudge_y = -.2
  ) +
  ggtitle("D1 Baseball Weighted Runs Created + ")

Lm <- lm(wrcTeamHitting$RunsPerGame ~ wrcTeamHitting$wRC_plus)
modsum = summary(Lm)
print(modsum)

#wRAA and RunsPerGame correlation, Runs, R^2 = .908.
ggplot() +
  geom_point(data = team_hitting, aes(x=wRAA, y=RunsPerGame)) +
  geom_smooth(data = team_hitting, aes(x=wRAA, y=RunsPerGame),method = "lm") +
  geom_label(
    data = team_hitting, aes(x=wOBA, y=RunsPerGame),
    label = "R^2 = .908",
    x = -50,
    y = 9.5,
    label.padding = unit(.5, "lines"),
    color = "blue") +
  geom_point(aes(x=33.1,y=7.142857),
             color='red',
             size=2, show.legend = TRUE
  ) +
  geom_label_repel(aes(x = 33.1, y = 7.142857),
                   color = "red",label="Georgetown", nudge_x = 75, nudge_y = -.25
  )+
  ggtitle("D1 Baseball Weighted Runs Above Average")

Lm <- lm(team_hitting$RunsPerGame ~ team_hitting$wRAA)
modsum = summary(Lm)
print(modsum)

#ISO, R^2 = .6005
ggplot() +
  geom_point(data = team_hitting, aes(x=ISO, y=RunsPerGame)) +
  geom_smooth(data = team_hitting, aes(x=ISO, y=RunsPerGame),method = "lm") +
  geom_label(
    data = team_hitting, aes(x=ISO, y=RunsPerGame),
    label = "R^2 = .6005",
    x = .12,
    y = 9.5,
    label.padding = unit(.5, "lines"),
    color = "blue") +
  geom_point(aes(x=.212,y=7.142857),
             color='red',
             size=2, show.legend = TRUE
  ) +
  geom_label_repel(data = filter(team_hitting,school=="Georgetown"),
                   aes(ISO,RunsPerGame,label = "Georgetown"),color="red",nudge_x = .02
  )+
  ggtitle("D1 Baseball Isolated Power")

Lm <- lm(team_hitting$RunsPerGame ~ team_hitting$ISO)
modsum = summary(Lm)
print(modsum)
