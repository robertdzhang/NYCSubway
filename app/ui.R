#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("New York Subway Ridership"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            shinyWidgets::airDatepickerInput("daterange",
                                             "Start and End of Selected Period",
                                             minDate = "2020-02-01",
                                             maxDate = "2020-06-06",
                                             range = TRUE
                                             )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            textOutput("percentageEntryChangeWeekday"),
            textOutput("percentageExitChangeWeekday"),
            textOutput("percentageEntryChangeWeekend"),
            textOutput("percentageExitChangeWeekend")
        ),
    )
))
