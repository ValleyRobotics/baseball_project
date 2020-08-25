library(shiny)
library(dplyr)

min_hr<-10
batting_stats<- read.csv(file = 'data/batting.csv')
over_x_hr <- batting_stats %>% filter(., HR>min_hr)
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("MLB Stats"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 5,
                        max = 31,
                        value = 20),
            sliderInput("min_hr",
                       "Min Number of Homeruns",
                       min = 20,
                       max = 60,
                       value = 45),
            checkboxInput(inputId = "addmedian",
                          label = "Add median line",
                          value = FALSE),
            checkboxInput(inputId = "addsteroidyears",
                          label = "Add Lines for Steroid Years",
                          value = FALSE),
            checkboxInput(inputId = "shownotes",
                          label = "Show Notes",
                          value = FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                align = "center",
                plotOutput("distPlot"),
                textOutput("notes"),
                textOutput("notes1"),
                textOutput("notes2")
        ), width = 8)
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    plot_data <- reactive({
        over_x_hr %>% filter(HR>input$min_hr)
    })
    

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R 
        x <- plot_data()[,2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # draw the histogram with the specified number of bins
        #h3(textOutput("Dude"))
        hist(x, breaks = bins, col = 'blue', border = 'white',
             xlab = "Years",
             main = "Histogram of number of players hitting selected homeruns")#, title = 'Number of HR seasons')
        if (input$addsteroidyears){
            abline(v=1994, lwd = 4, lty = 1, col = "red")
            abline(v=2004, lwd = 4, lty = 1, col = "red")
        }
        if(input$addmedian){
            abline(v=median(x),
            lwd = 2,
            lty = 2)
        }
    })
    #text = " "
    output$notes <- renderText({paste("Frequency of number of at least ->", input$min_hr, " homeruns!")})
    output$notes1 <- renderText({paste("Each bin represents ", sprintf("%#0.1f", 100/input$bins), " years")})
    output$notes2 <- renderText({paste0("Steroid Years are considered to be from early 90's to mid 2000's, 
                                       and are said to be at the peak between 1994 to 2004 with up to 60% using")})
}

# Run the application 
shinyApp(ui = ui, server = server)
