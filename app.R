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
y=c(1914,1945, 1994,2006, 2020)
x = added_HR_bin %>% mutate(y_bin = cut(
    yearID,
    breaks = y,
    include.lowest = TRUE,
    right = FALSE,
    labels = c("1914_1945", "The_50yrs_before_roids", "during_steroids", "after_steroids")
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

# # library ####
# library(shiny)
# library(dplyr)
# library(tidyverse)
# library(ggplot2)
# library(DBI)
# library(RSQLite)
# library(DT)
# library(shinythemes)
# library(shinydashboard)
# 
# 
# # data connection to db ####
# connector <- function(con, db) {
#     con <- dbConnect(SQLite(), db)
#     return (con)
# }
# dbcon <- connector(con, "baseball_stats.db")
# as.data.frame(dbListTables(dbcon))
# stats <- dbReadTable(dbcon, 'bat_p')
# # The steroid list from - https://bleacherreport.com/articles/232808-steroidology-l-hoops-projects-all-104-players-on-the-2003-steroid-list ####
# the_list <- read_csv('steroid_list.csv')
# dbDisconnect(dbcon) # disconnect
# # setting up data####
# # two data frames stats and the list - need to merge into one!
# the_list <- the_list %>% rename(nameLast = last, nameFirst = first)
# stats <-
#     merge(x = stats,
#           y = the_list,
#           c("nameLast", "nameFirst"),
#           all.x = TRUE)
# # eliminating columns ####
# stats <-
#     stats %>% select(playerID, yearID, nameFirst, nameLast, age, theList,
#                      weight, birthYear, teamID, num_years, G, AB, H, HR)
# # HR by at Bats ####
# # must have at least one at bat!
# stats <- stats %>% filter(AB > 0) %>% mutate(per_at_bat = HR / AB, b_avg=(H/AB))
# stats <- stats %>% mutate(theList=ifelse(is.na(theList), FALSE, TRUE))
# view(stats)
# # mean number of at bats per season ####
# # used this to decide on how many at bats to use for a season
# num_AB_per_season <-
#     stats %>% filter(HR > 10) %>% group_by(yearID) %>% summarise(n = n(), mean(AB), mean(HR), min(AB), max(AB)) %>% arrange(desc(`mean(AB)`))
# num_AB_per_season
# mean(num_AB_per_season$`mean(AB)`) # 487 for at least 10 at bats -> using 500 for the data
# # adding new stat to all batters HR per 500 at bats ####
# stats <- stats %>% mutate(HR_per_500 = per_at_bat * 500)
# top_seasons <-
#     stats %>% filter(HR > 35, AB > 200) %>% arrange(desc(HR_per_500)) %>% top_n(500, HR_per_500)
# sum_top <-
#     top_seasons %>% group_by(., playerID, theList) %>% summarise(
#         n = n(),
#         max(HR),
#         max(HR_per_500),
#         min(HR_per_500),
#         mean(HR_per_500),
#         mean_age_per_top = mean(age),
#         max_age=max(age),
#     )
# view(sum_top %>% filter(n>4))
# stats<-stats %>% mutate(HR_after_31=ifelse(age>31,HR,0))
# stats_grouped <-
#     stats %>% group_by(playerID) %>% summarise(
#         n = n(),
#         car_mean_HR = mean(HR),
#         car_mean_AB = mean(AB),
#         car_mean_HRp500 = mean(HR_per_500),
#         tot_HR = sum(HR),
#         car_avg = mean(b_avg)*100,
#         max_age = max(age),
#         HR_after_31 = sum(HR_after_31),
#         percent_after_31 = sum(HR_after_31/sum(HR)*100),
#         last_year = max(yearID)
#     )
# view(stats)
# # sum_top_c data and change names####
# sum_top_c <-
#     merge(x = sum_top,
#           y = stats_grouped,
#           "playerID",
#           all.x = TRUE) %>% mutate_if(is.numeric, round, digits = 1)
# sum_top_c
# names(sum_top_c)[3] <- "n_yrs_top"
# names(sum_top_c)[4] <- "Max_HR-T"
# names(sum_top_c)[5] <- "Max_HR(500)-T"
# names(sum_top_c)[6] <- "Min_HR(500)-T"
# names(sum_top_c)[7] <- "Mean_HR(500)-T"
# names(sum_top_c)[8] <- "Mean_Age-T"
# names(sum_top_c)[9] <- "Max_age_in_top"
# names(sum_top_c)[10] <- "n_yrs"
# names(sum_top_c)[11] <- "Mean_HR"
# names(sum_top_c)[12] <- "Mean_AB"
# names(sum_top_c)[13] <- "Mean_HR(500)"
# sum_top_c<-sum_top_c %>% mutate(car_avg=car_avg/100, percent_after_31=percent_after_31/100)
# view(sum_top_c %>% filter(tot_HR>400))
# # HR breaks ####
# breaks = c(0, 30, 40, 50, 80)
# #added_HR_bin = cut(stats$HR, breaks=breaks, include.lowest=TRUE, right=FALSE)
# added_HR_bin = stats %>% mutate(HR_bin = cut(
#     stats$HR,
#     breaks = breaks,
#     include.lowest = TRUE,
#     right = FALSE,
#     labels = c("under 30", "30 to 40", "40 to 50", "over 50")
# ))
# added_HR_bin %>% group_by(HR_bin) %>% summarise(
#     n = n(),
#     m_HR = mean(HR),
#     m_age = mean(age),
#     min_age = min(age),
#     max_age = max(age)
# )
# # max year ####
# y = c(seq(1920, 2020, by = 5))
# x = added_HR_bin %>% mutate(y_bin = cut(
#     yearID,
#     breaks = y,
#     include.lowest = TRUE,
#     right = FALSE
# ))
# max_hr_year_stat <-
#     (x %>% filter(HR > 20) %>% group_by(playerID) %>% top_n(1, HR) %>% arrange(desc(HR)))
# max_hr_year_stat
# max_hr_year_stat %>% group_by(y_bin) %>% summarize(mean(age), mean(HR), n =
#                                                        n())
# scat_stat <-
#     max_hr_year_stat %>% group_by(y_bin, HR_bin) %>% summarize(mean(age), mean(HR), n =
#                                                                    n())
# # Stats to select from ####
# mean_age_of_all_batters <- added_HR_bin %>% summarise(mean(age))
# mean_age_of_top_hr_hitters_ster_era <-
#     added_HR_bin %>% filter(., HR > 50, yearID > 1993, yearID < 2006) %>% summarise(mean(age))
# mean_age_of_top_not_ster_era <-
#     added_HR_bin %>% filter(., HR > 50, yearID < 1994) %>% summarise(mean(age))
# min_hr <- 10
# batting_stats <- read.csv(file = 'data/batting.csv')
# over_x_hr <- batting_stats %>% filter(., HR > min_hr)
# end of data setup
#text1 <- ""
# UI ####
ui <- navbarPage(
    "MLB - Hitting With Juice",
    # Tab Long Ball ####
    tabPanel(
        "The Long Ball",
        fluidPage(
            theme = shinytheme("united"),
            # Application title
            titlePanel("MLB Stats"),
            # Sidebar with a slider input for number of bins
            sidebarLayout(
                sidebarPanel(
                    sliderInput(
                        "bins",
                        "Number of bins:",
                        min = 5,
                        max = 40,
                        value = 22
                    ),
                    sliderInput(
                        "min_hr",
                        "Min Number of Homeruns",
                        min = 10,
                        max = 60,
                        value = 45
                    ),
                    checkboxInput(
                        inputId = "addmedian",
                        label = "Add median line",
                        value = FALSE
                    ),
                    checkboxInput(
                        inputId = "addsteroidyears",
                        label = "Add Lines for Steroid Years",
                        value = FALSE
                    ),
                    width = 3
                ),
                mainPanel(
                    fluidRow(
                        h1("Hitting The Long Balls"),
                        align = "center",
                        br(),
                        h3(textOutput("notes")),
                        br(),
                        h3(textOutput("notes1")),
                        br(),
                        textOutput("notes2"),
                        textOutput("notes3"),
                        br(),
                        textOutput("notes4")
                    ),
                    width = 9,
                    fluidRow(),
                    fluidRow(plotOutput("distPlot"), width = 12)
                )
            )
        )
    ),
    # Tab Per 500 ####
    tabPanel(
        "Per 500 at Bats",
        fluidPage(
            titlePanel("Homeruns Per at Bat If They Had 500 at Bats"),
            fluidRow(
                column(10, "Player HR/AB*500 - makes all players on same playing field")
            ),
            fluidRow(
                column(
                    10,
                    "This table then has the top 500 seasons and number of times the top players have had one of these seasons"
                )
            ),
            fluidRow(br()),
            fluidRow(column(12, DT::dataTableOutput("per500")))
        )
    ),
    # Tab scat plot ####
    tabPanel(
        "Age At Best",
        fluidPage(
            titlePanel("Before 1995, How Old Was a Player When They Hit Their Most Homeruns in a Season?"),
            titlePanel(
                checkboxInput(
                    inputId = "adddots",
                    label = "Add age points",
                    value = FALSE
                )
            ),
            
            fluidRow(plotOutput("plot_hrbin_hr")),
            titlePanel("1995 and on, Players Age of Most Homeruns?"),
            fluidRow(plotOutput("plot_hrbin_age"))
        )
    ),
    tabPanel(
        "Not Sure!",
        fluidPage(
            titlePanel("Hitting the longball throughout the career"),
            fluidRow(plotOutput("plot_player")
                
            )
        )
          ),
    tabPanel(
        "by Time Period",
        fluidPage(
            titlePanel("Breaking it down by time period"),
            tabsetPanel(
                tabPanel("Stacked Barplot", plotOutput("stacked_plot")),
                tabPanel("Wrapped Barplot", plotOutput("wrapped_plot"))
            )
        )
    )
)

# Define server logic required to draw a histogram
# Server ####
server <- function(input, output) {
    plot_data <- reactive({
        over_x_hr %>% filter(HR > input$min_hr)
    })
    
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x <- plot_data()[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # draw the histogram with the specified number of bins
        #h3(textOutput("Dude"))
        hist(
            x,
            breaks = bins,
            col = 'blue',
            border = 'white',
            xlab = "Years",
            main = "Number of players hitting at Least selected homeruns"
        )#, title = 'Number of HR seasons')
        if (input$addsteroidyears) {
            abline(
                v = 1994,
                lwd = 5,
                lty = 1,
                col = "red"
            )
            abline(
                v = 2005,
                lwd = 5,
                lty = 1,
                col = "red"
            )
        }
        if (input$addmedian) {
            abline(v = median(x),
                   lwd = 2,
                   lty = 2)
        }
    })
    
    # output notes ####
    output$notes <-
        renderText({
            paste0("Number of batters with at least ->",
                   input$min_hr,
                   " homeruns!")
        })
    output$notes1 <-
        renderText({
            paste0("Each bin represents ",
                   sprintf("%#0.1f", 100 / input$bins),
                   " years")
        })
    output$notes2 <-
        renderText({
            paste0("Steroid Years are considered to be from early 90's to mid 2000's,")
        })
    output$notes3 <-
        renderText({
            paste0("and are said to be at the peak between 1994 to 2005 with up to 60% of players using")
        })
    output$notes4 <-
        renderText({
            paste0(
                "Mean age of all batters ",
                sprintf("%0.1f", mean_age_of_all_batters),
                " * Batters > 50 HR in Steriod Era ",
                sprintf("%0.1f", mean_age_of_top_hr_hitters_ster_era),
                " * Batters > 50 HR before 1994 ",
                sprintf("%0.1f", mean_age_of_top_not_ster_era)
            )
        })
    # output per500 ####
    output$per500 <-
        renderDataTable({
            sum_top_c %>% filter(n_yrs_top > 4) %>% arrange(desc(n_yrs_top))
        })
    output$plot_hrbin_hr <- renderPlot({
        p = ggplot(max_hr_year_stat %>% filter(yearID < 1995),
                   aes(
                       x = HR_bin,
                       y = age,
                       fill = HR_bin
                   )) + geom_boxplot() + stat_summary(
                       fun.y = mean,
                       geom = "point",
                       shape = 23,
                       size = 4
                   ) +
            theme(legend.position = "top") + labs(x = "Homerun Bins", y =
                                                      "Age of Top Year")+
            geom_hline(yintercept=30, linetype="dashed", 
                       color = "red", size=2)
        if (input$adddots) {
            p + geom_dotplot(
                binaxis = 'y',
                stackdir = 'center',
                position = position_dodge(1)
            )
        } else{
            p
        }
    })
    # plot HR Bin ####
    output$plot_hrbin_age <- renderPlot({
        p <-
            ggplot(max_hr_year_stat %>% filter(yearID > 1994),
                   aes(
                       x = HR_bin,
                       y = age,
                       fill = HR_bin
                   )) + geom_boxplot() + stat_summary(
                       fun.y = mean,
                       geom = "point",
                       shape = 23,
                       size = 4
                   ) +
            theme(legend.position = "bottom") + labs(x = "Homerun Bins", y =
                                                         "Age of Top Year")+
            geom_hline(yintercept=30, linetype="dashed", 
                       color = "red", size=2)
        if (input$adddots) {
            p + geom_dotplot(
                binaxis = 'y',
                stackdir = 'center',
                position = position_dodge(1)
            )
        } else{
            p
        }
    })
    output$plot_player <- renderPlot({
        x4<-x %>% group_by(playerID) %>% mutate(tot_hr=sum(HR)) %>% arrange(desc(tot_hr))
        ggplot(x4 %>% filter(tot_hr>550)%>% group_by(playerID) %>% arrange(desc(tot_hr)),
               aes(x= reorder(playerID,-tot_hr), y=HR, fill=age))+geom_bar(stat="identity")+
            scale_fill_gradient2(low='white', mid='yellow', high='blue')
    })
    output$stacked_plot <-renderPlot({
        by_bins %>% filter(HR_bin!="under 30") %>% 
        ggplot(aes(fill=HR_bin, y=n, x=y_bin, label = n)) + 
        geom_bar(position="stack", stat="identity") +
        scale_fill_viridis(discrete = T) +
        ggtitle("MLB Long Balls!") +
        ylab("Number of palyers that hit at least 30 homeruns") +
        xlab("Four different periods of baseball (20 years, 50 years, 11 years, and 14 years")+
        geom_text(size = 4, position = position_stack(vjust = 0.5))
    })
    
    output$wrapped_plot <- renderPlot({
        by_bins %>% filter(HR_bin!="under 30") %>% 
        ggplot(aes(fill=HR_bin, y=n, x=HR_bin, label = n)) + 
        geom_bar(position="dodge", stat="identity") +
        scale_fill_viridis(discrete = T, option="E") +
        facet_wrap(~y_bin) +
        ggtitle("MLB Long Balls!") +
        ylab("Number of palyers that hit at least 30 homeruns") +
        xlab("Four different periods of baseball (20 years, 50 years, 11 years, and 14 years") +
        geom_text(size = 4, position = position_stack(vjust = 0.5))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
