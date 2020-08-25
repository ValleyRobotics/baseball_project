# Libraries included ####
library(dplyr)
library(tidyverse)
library(ggplot2)
library(DBI)
library(RSQLite)

connector <- function(con, db){
  con <- dbConnect(SQLite(), db)
  return (con)
  }

dbcon <-connector(con, "baseball_stats.db")
as.data.frame(dbListTables(dbcon))

stats <- dbReadTable(dbcon, 'bat_p')
the_list <- read_csv('steroid_list.csv')
dbDisconnect(dbcon)
# The steriod list from - https://bleacherreport.com/articles/232808-steroidology-l-hoops-projects-all-104-players-on-the-2003-steroid-list ####
the_list <- read_csv('steroid_list.csv')
the_list <- the_list %>% rename(nameLast=last, nameFirst=first)
view(the_list)
stats1 <- stats
the_list
stats1<-merge(x=stats1, y=the_list, c("nameLast", "nameFirst"), all.x=TRUE)
stats1
view(stats1 %>% filter(theList) %>% group_by(playerID) %>% summarise(n=n()))
count(stats1)
count(stats)
#stats1<-stats1 %>% filter(is.na(theList)) %>% mutate(theList=FALSE)
stats<-stats1
stats %>% filter(HR>35) %>% summarise(n=n(), m_HR=mean(HR), m_age=(mean(age)))
stats
# eliminating columns ####
stats <- stats %>% select(playerID, yearID, nameFirst, nameLast, age, theList, weight, birthYear, teamID, num_years, G, AB, H, HR)
# HR by at Bats ####
# must have at least one at bat!
stats <- stats %>% filter(AB>0) %>% mutate(per_at_bat=HR/AB)
view(stats)
# mean number of at bats per season ####
num_AB_per_season <- stats %>% filter(HR>25) %>% group_by(yearID) %>% summarise(n=n(), mean(AB), mean(HR), min(AB), max(AB))
view(num_AB_per_season)
# adding new stat to all batters HR per 500 at bats ####
stats <- stats %>% mutate(HR_per_500 = per_at_bat * 500)
top_seasons<-stats %>% filter(HR>35, AB>200) %>% arrange(desc(HR_per_500))
top_seasons<-head(top_seasons,500)
sum_top<-top_seasons %>% group_by(.,playerID, theList) %>% summarise(n=n(), max(HR), max(HR_per_500),min(HR_per_500), mean(HR_per_500), mean_age_per_top=mean(age))
view(sum_top %>% filter(n>4))
stats_grouped<-stats %>% group_by(playerID) %>% summarise(n=n(), car_mean_HR=mean(HR), car_mean_AB=mean(AB), car_mean_HRp500=mean(HR_per_500))
sum_top_c <-merge(x=sum_top, y=stats_grouped, "playerID", all.x=TRUE)
view(sum_top_c %>% filter(n.x>4))
# HR breaks ####
breaks=c(0,30,40,50,80)
#added_HR_bin = cut(stats$HR, breaks=breaks, include.lowest=TRUE, right=FALSE)

added_HR_bin = stats %>% mutate(HR_bin=cut(stats$HR, breaks=breaks, include.lowest=TRUE, right=FALSE,labels=c("under 30", "30 to 40", "40 to 50", "over 50")))
added_HR_bin %>% group_by(HR_bin) %>% summarise(n=n(), m_HR=mean(HR), m_age=mean(age), min_age=min(age), max_age=max(age))

mean_age_of_all_batters <- added_HR_bin %>% summarise(mean(age))
mean_age_of_all_batters
y = c(seq(1920, 2020, by=5))


y
x = s%>% mutate(y_bin=cut(yearID, breaks=y, include.lowest=TRUE, right=FALSE))
view(x%>%filter (yearID>1920, HR>40) %>% arrange(desc(HR)))
# max year ####
view(added_HR_bin)
x = added_HR_bin%>% mutate(y_bin=cut(yearID, breaks=y, include.lowest=TRUE, right=FALSE))
max_hr_year_stat<-(x %>% filter(HR> 20) %>% group_by(playerID) %>% top_n(1,HR) %>% arrange(desc(HR)))
max_hr_year_stat %>% group_by(y_bin) %>% summarize(mean(age), mean(HR),n=n())
view(max_hr_year_stat %>% group_by(y_bin, HR_bin) %>% summarize(mean(age), mean(HR),n=n()))
view(max_hr_year_stat)
#head(x %>% select(playerID,HR, yearID) %>% filter(HR>40) %>% which.max(HR),100)

z=x%>% group_by(y_bin, HR_bin) %>% summarise(n=n(), m_HR=mean(HR), m_age=mean(age), min_age=min(age), max_age=max(age))
view(z)

xx=x%>% filter(HR>40) %>% group_by(y_bin) %>% summarise(n=n(), m_HR=mean(HR), m_age=mean(age), min_age=min(age), max_age=max(age))
view(xx)

players_over_44_HR = x%>%filter (yearID>1900, HR>44) %>% group_by(playerID) %>% summarise(n=n(), mean_age=(mean(age)), min_year=min(yearID), max_year=max(yearID)) %>% arrange(desc(n))

view(players_over_44_HR)
#Team Stats ####
team_stats<- read.csv(file = 'data/teams.csv')
TS_year=team_stats %>% filter(yearID>1919) %>% group_by(yearID) %>% summarise(tot_HR=sum(HR),m_HR=(mean(HR)))
view(TS_year)
plot(TS_year)
ggplot(TS_year, aes(x=yearID, y=m_HR)) +
  geom_line()

plot(x=players_over_44_HR$mean_age,players_over_44_HR$n)
s = s %>% filter(HR>40)
hist(s$age)
s20<-s %>% filter(age<=25) %>% summarise(n=n())
s25<-s %>% filter(age>25, age<=30) %>% summarise(n=n())
s30<-s %>% filter(age>30, age<=35) %>% summarise(n=n())
s35<-s %>% filter(age>35) %>% summarise(n=n())





options(max.print=1000)
s_year=1985
min_hr=50

# Load files ####
batting_stats<- read.csv(file = 'data/batting.csv')
people_stats<- read.csv(file = 'data/People.csv')
team_stats<- read.csv(file = 'data/Teams.csv')
salaries_stats<- read.csv(file = 'data/Salaries.csv')

# join batting and people ####
new_batting <- merge(batting_stats, people_stats, by = 'playerID')
view(new_batting)
above_min_hr<-new_batting %>% filter(.,HR>min_hr, yearID>s_year)
head(above_min_hr)
above_min_hr <- above_min_hr %>% mutate(.,age=(yearID-birthYear) )
view(above_min_hr)
amh <- above_min_hr %>% summarise_if(.,is.numeric, mean, na.rm=T)
view(amh)
ggplot(above_min_hr, aes(x=age, y=HR, color=yearID, size=HR)) + geom_point()+scale_color_gradient(low="blue", high="red")




# Create Player Table ####
bat_filter <- batting_stats %>% 
  filter(., yearID>s_year) %>% 
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
ts <- team_stats %>% filter(., yearID>s_year) %>% group_by(.,yearID) %>% summarise_if(.,is.numeric, mean, na.rm=T)
view(ts)
