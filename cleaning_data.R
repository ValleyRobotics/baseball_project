# library ####
library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(DBI)
library(RSQLite)
library(DT)
library(shinythemes)
library(shinydashboard)
library(latticeExtra)
library(plotly)
library(viridis)
#library(hrbrthemes)


# data connection to db ####
connector <- function(con, db) {
  con <- dbConnect(SQLite(), db)
  return (con)
}
dbcon <- connector(con, "baseball_stats.db")
as.data.frame(dbListTables(dbcon))
stats <- dbReadTable(dbcon, 'bat_p')
# The steroid list from - https://bleacherreport.com/articles/232808-steroidology-l-hoops-projects-all-104-players-on-the-2003-steroid-list ####
the_list <- read_csv('steroid_list.csv')
dbDisconnect(dbcon) # disconnect
# setting up data####
# two data frames stats and the list - need to merge into one!
the_list <- the_list %>% rename(nameLast = last, nameFirst = first)
stats <-
  merge(x = stats,
        y = the_list,
        c("nameLast", "nameFirst"),
        all.x = TRUE)
# eliminating columns ####
stats <-
  stats %>% select(playerID, yearID, nameFirst, nameLast, age, theList,
    weight, birthYear, teamID, num_years, G, AB, H, HR)
# HR by at Bats ####
# must have at least one at bat!
stats <- stats %>% filter(AB > 0) %>% mutate(per_at_bat = HR / AB, b_avg=(H/AB))
stats <- stats %>% mutate(theList=ifelse(is.na(theList), FALSE, TRUE))
view(stats)
# mean number of at bats per season ####
# used this to decide on how many at bats to use for a season
num_AB_per_season <-
  stats %>% filter(HR > 10) %>% group_by(yearID) %>% summarise(n = n(), mean(AB), mean(HR), min(AB), max(AB)) %>% arrange(desc(`mean(AB)`))
num_AB_per_season
mean(num_AB_per_season$`mean(AB)`) # 487 for at least 10 at bats -> using 500 for the data
# adding new stat to all batters HR per 500 at bats ####
stats <- stats %>% mutate(HR_per_500 = per_at_bat * 500)
top_seasons <-
  stats %>% filter(HR > 35, AB > 200) %>% arrange(desc(HR_per_500)) %>% top_n(500, HR_per_500)
sum_top <-
  top_seasons %>% group_by(., playerID, theList) %>% summarise(
    n = n(),
    max(HR),
    max(HR_per_500),
    min(HR_per_500),
    mean(HR_per_500),
    mean_age_per_top = mean(age),
    max_age=max(age),
  )
view(sum_top %>% filter(n>4))
stats<-stats %>% mutate(HR_after_31=ifelse(age>31,HR,0))
stats_grouped <-
  stats %>% group_by(playerID) %>% summarise(
    n = n(),
    car_mean_HR = mean(HR),
    car_mean_AB = mean(AB),
    car_mean_HRp500 = mean(HR_per_500),
    tot_HR = sum(HR),
    car_avg = mean(b_avg)*100,
    max_age = max(age),
    HR_after_31 = sum(HR_after_31),
    percent_after_31 = sum(HR_after_31/sum(HR)*100),
    last_year = max(yearID)
  )
view(stats)
# sum_top_c data and change names####
sum_top_c <-
  merge(x = sum_top,
        y = stats_grouped,
        "playerID",
        all.x = TRUE) %>% mutate_if(is.numeric, round, digits = 1)
sum_top_c
names(sum_top_c)[3] <- "n_yrs_top"
names(sum_top_c)[4] <- "Max_HR-T"
names(sum_top_c)[5] <- "Max_HR(500)-T"
names(sum_top_c)[6] <- "Min_HR(500)-T"
names(sum_top_c)[7] <- "Mean_HR(500)-T"
names(sum_top_c)[8] <- "Mean_Age-T"
names(sum_top_c)[9] <- "Max_age_in_top"
names(sum_top_c)[10] <- "n_yrs"
names(sum_top_c)[11] <- "Mean_HR"
names(sum_top_c)[12] <- "Mean_AB"
names(sum_top_c)[13] <- "Mean_HR(500)"
sum_top_c<-sum_top_c %>% mutate(car_avg=car_avg/100, percent_after_31=percent_after_31/100)

# HR breaks ####
breaks = c(0, 30, 40, 50, 80)
#added_HR_bin = cut(stats$HR, breaks=breaks, include.lowest=TRUE, right=FALSE)
added_HR_bin = stats %>% mutate(HR_bin = cut(
  stats$HR,
  breaks = breaks,
  include.lowest = TRUE,
  right = FALSE,
  labels = c("under 30", "30 to 40", "40 to 50", "over 50")
))
added_HR_bin %>% group_by(HR_bin) %>% summarise(
  n = n(),
  m_HR = mean(HR),
  m_age = mean(age),
  min_age = min(age),
  max_age = max(age)
)
# max year ####
#y = c(seq(1920, 2020, by = 5))
y=c(1919,1945, 1994,2006, 2020)
x = added_HR_bin %>% mutate(y_bin = cut(
  yearID,
  breaks = y,
  include.lowest = TRUE,
  right = FALSE,
  labels = c("1920_1945", "The_50yrs_before_roids", "during_steroids", "after_steroids")
))
max_hr_year_stat <-
  (x %>% filter(HR > 20) %>% group_by(playerID) %>% top_n(1, HR) %>% arrange(desc(HR)))
max_hr_year_stat
max_hr_year_stat %>% group_by(y_bin) %>% summarize(mean(age), mean(HR), n =
                                                     n())
by_bins <-
  x %>% group_by(y_bin, HR_bin) %>% summarize(mean(age), mean(HR), n =
                                                               n()) %>% arrange(desc(HR_bin))
# Stats to select from ####
mean_age_of_all_batters <- added_HR_bin %>% summarise(mean(age))
mean_age_of_top_hr_hitters_ster_era <-
  added_HR_bin %>% filter(., HR > 50, yearID > 1993, yearID < 2006) %>% summarise(mean(age))
mean_age_of_top_not_ster_era <-
  added_HR_bin %>% filter(., HR > 50, yearID < 1994) %>% summarise(mean(age))
min_hr <- 10
batting_stats <- read.csv(file = 'data/batting.csv')
over_x_hr <- batting_stats %>% filter(., HR > min_hr)
# end of data setup
# view data ####
view(sum_top_c %>% filter(tot_HR>400))
HR_31_before_and_after_1994 <- sum_top_c %>% filter(tot_HR>400) %>% mutate(yr_before_1994=ifelse(last_year<1994, TRUE, FALSE))
head(HR_31_before_and_after_1994)
#some graphing fun ####
#by_bins<-x%>% filter(HR_bin!="under 30") %>% arrange(HR_bin)
#x
# cloud(n~HR_bin
#       +y_bin, x, panel.3d.cloud=panel.3dbars, col.facet='grey', 
#       xbase=0.4, ybase=0.4, scales=list(arrows=FALSE, col=1),
#       par.settings = list(axis.line = list(col = "transparent")))
# 
# plot_ly(x, x=~y_bin, y=~HR_bin, z=~n, type="scatter3d", mode="markers", color=~y_bin)
# 
# # Point colors
# marker <- list(color = ~y_bin, colorscale = c('#FFE1A1', '#683531'), 
#                showscale = TRUE)
# # Create the plot
# p <- plot_ly(x, x = ~y_bin, y = ~HR_bin, z = count(n), marker = marker) %>%
#   add_markers() %>%
#   layout(
#     scene = list(xaxis = list(title = 'Weight'),
#                  yaxis = list(title = 'Gross horsepower'),
#                  zaxis = list(title = '1/4 mile time'))
#   )
# p
#by_bins

by_bins %>% filter(HR_bin!="under 30") %>% ggplot(aes(fill=HR_bin, y=n, x=y_bin, label = n)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("MLB Long Balls!") +
  ylab("Number of palyers that hit at least 30 homeruns") +
  xlab("Four different periods of baseball (20 years, 50 years, 11 years, and 14 years")+
  geom_text(size = 4, position = position_stack(vjust = 0.5))
  
by_bins %>% filter(HR_bin!="under 30") %>% ggplot(aes(fill=HR_bin, y=n, x=HR_bin, label = n)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option="E") +
  facet_wrap(~y_bin) +
  ggtitle("MLB Long Balls!") +
  ylab("Number of palyers that hit at least 30 homeruns") +
  xlab("Four different periods of baseball (20 years, 50 years, 11 years, and 14 years") +
  geom_text(size = 4, position = position_stack(vjust = 0.5))


