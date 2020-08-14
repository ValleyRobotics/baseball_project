# Libraries included ####
library(dplyr)
library(tidyverse)


options(max.print=1000)

# Load files ####
batting_stats<- read.csv(file = 'data/batting.csv')
people_stats<- read.csv(file = 'data/People.csv')
team_stats<- read.csv(file = 'data/Teams.csv')
salaries_stats<- read.csv(file = 'data/Salaries.csv')

# Create Player Table ####
bat_filter <- batting_stats %>% 
  filter(., yearID>1900) %>% 
  group_by(.,playerID) %>%
  summarise(.,avg_AB = mean(AB), avg_H =mean(H), 
            avg_HR=mean(HR), max_HR= max(HR), 
            min_HR=min(HR), tot_HR=sum(HR), 
            tot_H=sum(H), tot_RBI=sum(RBI), 
            tot_BB=sum(BB), tot_SO=sum(SO), 
            First_year=min(yearID),Last_year=max(yearID), n_years=n(),
            avg_BA=mean(H/AB), tot_SB=sum(SB), avg_SB=mean(SB),
            years=(max(yearID)-min(yearID)+1))
bat <- bat_filter%>% 
  filter(., n_years>10)%>% 
  arrange(.,desc(tot_HR))


# End Player Table

# Data Searching ####
summary(batting_stats)
summary(bat)
bat
# allows viewing of all the data... ####
View(bat)
head(bat,50)
head(batting_stats)
batting_stats[which.max(batting_stats$HR),]
batting_stats[which.max(batting_stats$H),]
batting_stats[which.max(batting_stats$SO),]
batting_stats[which.max(batting_stats$BB),]
batting_stats[batting_stats$playerID=='bondsba01',]
batting_stats[batting_stats$playerID=='suzukic01',]
batting_stats[batting_stats$playerID=='ruthba01',]
batting_stats[batting_stats$playerID=='mcgwima01',]
batting_stats[batting_stats$playerID=='sosasa01',]
batting_stats[batting_stats$playerID=='rodrial01',]
batting_stats[batting_stats$playerID=='kinerra01',]
batting_stats[batting_stats$playerID=='pujolal01',]

# people gets date of birth full name 
head(people_stats)
# Team shows team totals per season
head(team_stats)
# salaries for player starting 1985
head(salaries_stats)
# need to mutate my main dataframe to include age in years by joining people to batting

# need to mutate my main to include salaries from 85 forward

# thinking I'll include team averages per year with player - can do percent of team total



# Team Stats ####
head(team_stats)
team_stats %>% group_by(.,)

