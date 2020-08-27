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
        max_age=max(age)
    )
view(sum_top %>% filter(n>4))
stats_grouped <-
    stats %>% group_by(playerID) %>% summarise(
        n = n(),
        car_mean_HR = mean(HR),
        car_mean_AB = mean(AB),
        car_mean_HRp500 = mean(HR_per_500),
        tot_HR = sum(HR),
        car_avg = mean(b_avg),
        max_age = max(age)
    )
# sum_top_c data and change names####
sum_top_c <-
    merge(x = sum_top,
          y = stats_grouped,
          "playerID",
          all.x = TRUE) %>% mutate_if(is.numeric, round, digits = 1)
names(sum_top_c)[3] <- "n_yrs_top"
names(sum_top_c)[4] <- "Max_HR-T"
names(sum_top_c)[5] <- "Max_HR(500)-T"
names(sum_top_c)[6] <- "Min_HR(500)-T"
names(sum_top_c)[7] <- "Mean_HR(500)-T"
names(sum_top_c)[8] <- "Mean_Age-T"
names(sum_top_c)[9] <- "Total_Yrs"
names(sum_top_c)[10] <- "Mean_HR"
names(sum_top_c)[11] <- "Mean_AB"
names(sum_top_c)[12] <- "Mean_HR(500)"
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
y = c(seq(1920, 2020, by = 5))
x = added_HR_bin %>% mutate(y_bin = cut(
    yearID,
    breaks = y,
    include.lowest = TRUE,
    right = FALSE
))
max_hr_year_stat <-
    (x %>% filter(HR > 20) %>% group_by(playerID) %>% top_n(1, HR) %>% arrange(desc(HR)))
max_hr_year_stat
max_hr_year_stat %>% group_by(y_bin) %>% summarize(mean(age), mean(HR), n =
                                                       n())
scat_stat <-
    max_hr_year_stat %>% group_by(y_bin, HR_bin) %>% summarize(mean(age), mean(HR), n =
                                                                   n())
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
# 
# dbcon <- connector(con, "baseball_stats.db")
# as.data.frame(dbListTables(dbcon))
# 
# stats <- dbReadTable(dbcon, 'bat_p')
# # The steriod list from - https://bleacherreport.com/articles/232808-steroidology-l-hoops-projects-all-104-players-on-the-2003-steroid-list ####
# the_list <- read_csv('steroid_list.csv')
# dbDisconnect(dbcon)
# # disconnect
# # setting up data####
# the_list <- the_list %>% rename(nameLast = last, nameFirst = first)
# stats1 <- stats
# stats1 <-
#     merge(x = stats1,
#           y = the_list,
#           c("nameLast", "nameFirst"),
#           all.x = TRUE)
# stats <- stats1
# stats %>% filter(HR > 35) %>% summarise(n = n(),
#                                         m_HR = mean(HR),
#                                         m_age = (mean(age)))
# #stats
# # eliminating columns ####
# stats <-
#     stats %>% select(
#         playerID,
#         yearID,
#         nameFirst,
#         nameLast,
#         age,
#         theList,
#         weight,
#         birthYear,
#         teamID,
#         num_years,
#         G,
#         AB,
#         H,
#         HR
#     )
# # HR by at Bats ####
# # must have at least one at bat!
# stats <- stats %>% filter(AB > 0) %>% mutate(per_at_bat = HR / AB)
# #view(stats)
# # mean number of at bats per season ####
# num_AB_per_season <-
#     stats %>% filter(HR > 25) %>% group_by(yearID) %>% summarise(n = n(), mean(AB), mean(HR), min(AB), max(AB))
# #view(num_AB_per_season)
# # adding new stat to all batters HR per 500 at bats ####
# stats <- stats %>% mutate(HR_per_500 = per_at_bat * 500)
# top_seasons <-
#     stats %>% filter(HR > 35, AB > 200) %>% arrange(desc(HR_per_500))
# top_seasons <- head(top_seasons, 500)
# sum_top <-
#     top_seasons %>% group_by(., playerID, theList) %>% summarise(
#         n = n(),
#         max(HR),
#         max(HR_per_500),
#         min(HR_per_500),
#         mean(HR_per_500),
#         mean_age_per_top = mean(age)
#     )
# #view(sum_top %>% filter(n>4))
# stats_grouped <-
#     stats %>% group_by(playerID) %>% summarise(
#         n = n(),
#         car_mean_HR = mean(HR),
#         car_mean_AB = mean(AB),
#         car_mean_HRp500 = mean(HR_per_500)
#     )
# # sum_top_c data ####
# sum_top_c <-
#     merge(x = sum_top,
#           y = stats_grouped,
#           "playerID",
#           all.x = TRUE) %>% mutate_if(is.numeric, round, digits = 1)
# names(sum_top_c)[3] <- "n_yrs_top"
# names(sum_top_c)[4] <- "Max_HR-T"
# names(sum_top_c)[5] <- "Max_HR(500)-T"
# names(sum_top_c)[6] <- "Min_HR(500)-T"
# names(sum_top_c)[7] <- "Mean_HR(500)-T"
# names(sum_top_c)[8] <- "Mean_Age-T"
# names(sum_top_c)[9] <- "Total_Yrs"
# names(sum_top_c)[10] <- "Mean_HR"
# names(sum_top_c)[11] <- "Mean_AB"
# names(sum_top_c)[12] <- "Mean_HR(500)"
# 
# #sum_top_c %>% mutate_if(is.numeric, round, digits = 0)
# #view(sum_top_c %>% filter(n.x>4))
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
# view(max_hr_year_stat %>% group_by(y_bin, HR_bin) %>% summarize(mean(age), mean(HR), n =
#                                                                     n()))
# view(max_hr_year_stat)
# scat_stat <-
#     max_hr_year_stat %>% group_by(y_bin, HR_bin) %>% summarize(mean(age), mean(HR), n =
#                                                                    n())
# 
# # Stats to select from ####
# mean_age_of_all_batters <- added_HR_bin %>% summarise(mean(age))
# mean_age_of_top_hr_hitters_ster_era <-
#     added_HR_bin %>% filter(., HR > 50, yearID > 1993, yearID < 2006) %>% summarise(mean(age))
# mean_age_of_top_not_ster_era <-
#     added_HR_bin %>% filter(., HR > 50, yearID < 1994) %>% summarise(mean(age))
# y = c(seq(1920, 2020, by = 5))
# min_hr <- 10
# batting_stats <- read.csv(file = 'data/batting.csv')
# over_x_hr <- batting_stats %>% filter(., HR > min_hr)
# # end of data setup
# Define UI for application that draws a histogram
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
                        max = 31,
                        value = 20
                    ),
                    sliderInput(
                        "min_hr",
                        "Min Number of Homeruns",
                        min = 20,
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
        "Plots",
        fluidPage(
            titlePanel("Homeruns top year by age before 1995"),
            titlePanel(
                checkboxInput(
                    inputId = "adddots",
                    label = "Add age points",
                    value = FALSE
                )
            ),
            
            fluidRow(plotOutput("plot_hrbin_hr")),
            titlePanel("Homeruns top year by age after 1994"),
            fluidRow(plotOutput("plot_hrbin_age"))
        )
    ),
    tabPanel(
        "Player Path",
        fluidPage(
            titlePanel("Hitting the longball throughout the career"),
            fluidRow(plotOutput("plot_player")
                
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
                v = 2004,
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
            paste0("and are said to be at the peak between 1994 to 2004 with up to 60% using")
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
                       color = "red", size=2)#geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(1))
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
        #x4<-x4 %>% filter(age>=32) %>% mutate(hr_over_32=sum(HR))
        #x4<-x4 %>% filter(age<32) %>% mutate(hr_under_32=sum(HR))
        # x4 = x4 %>% filter(tot_hr>550) %>% mutate(age_bin = cut(
        #     x4$age,
        #     breaks = c(0,25,32),
        #     include.lowest = TRUE,
        #     right = FALSE,
        #     labels = c("under 25", "25 to 31", "over 32")))
        
        ggplot(x4 %>% filter(tot_hr>550)%>% group_by(playerID) %>% arrange(desc(tot_hr)),
               aes(x= reorder(playerID,-tot_hr), y=HR, fill=age))+geom_bar(stat="identity")+
            scale_fill_gradient2(low='white', mid='yellow', high='blue')
    })
}

# Run the application
shinyApp(ui = ui, server = server)
