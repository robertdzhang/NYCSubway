#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(tidyverse)

shinyServer(function(input, output) {
    turnstile_summaries_shiny <- readRDS("turnstile-summaries-shiny.RDS")
    turnstile_summaries_filtered <- reactive({
        if (length(input$daterange) == 2) {
            turnstile_summaries_filtered <-
                turnstile_summaries_shiny %>% filter(date(datetime) >= input$daterange[[1]],
                                                     date(datetime) <= input$daterange[[2]])
        } else if (length(input$daterange) == 1) {
            turnstile_summaries_filtered <-
                turnstile_summaries_shiny %>% filter(date(datetime) == input$daterange[[1]])
        } else {
            turnstile_summaries_filtered <- turnstile_summaries_shiny
        }
    })
    # preCovidWeekdayData <-
    #     reactive({
    preCovidWeekdayData <-
        turnstile_summaries_shiny %>% filter(month(datetime) == 2, IsWeekday)
    # })
    # preCovidWeekdayAverage <-
    #     reactive({
    preCovidWeekdayAverage <- preCovidWeekdayData %>%
        summarize(
            avg_entry_sum = sum(entry_sum) /
                (datetime %>% date() %>% unique() %>% length()),
            avg_exit_sum = sum(exit_sum) /
                (datetime %>% date() %>% unique() %>% length())
        ) # length(unique(date(datetime))))
    # })
    # preCovidWeekendData <-
    #     reactive({
    preCovidWeekendData <-
        turnstile_summaries_shiny %>% filter(month(datetime) == 2, !IsWeekday)
    # })
    # preCovidWeekendAverage <- reactive({
    preCovidWeekendAverage <- preCovidWeekendData %>%
        summarize(
            avg_entry_sum = sum(entry_sum) /
                (datetime %>% date() %>% unique() %>% length()),
            avg_exit_sum = sum(exit_sum) /
                (datetime %>% date() %>% unique() %>% length())
        )
    # })
    currData <- reactive({
        currData <- turnstile_summaries_filtered() %>%
            group_by(the_date = date(datetime)) %>%
            summarize(entry_sum = sum(entry_sum),
                      exit_sum = sum(exit_sum))
    })
    currWeekdayAverage <- reactive({
        currWeekdayAverage <- turnstile_summaries_filtered() %>%
            filter(IsWeekday == TRUE) %>%
            summarize(
                avg_entry_sum = sum(entry_sum) /
                    (datetime %>% date() %>% unique() %>% length()),
                avg_exit_sum = sum(exit_sum) /
                    (datetime %>% date() %>% unique() %>% length())
            )
    })
    currWeekendAverage <- reactive({
        currWeekendAverage <- turnstile_summaries_filtered() %>%
            filter(IsWeekday == FALSE) %>%
            summarize(
                avg_entry_sum = sum(entry_sum) /
                    (datetime %>% date() %>% unique() %>% length()),
                avg_exit_sum = sum(exit_sum) /
                    (datetime %>% date() %>% unique() %>% length())
            )
    })
    output$percentageEntryChangeWeekday <-
        renderText({
            paste0(
                "Percentage Weekday Entry Change from February Average: ",
                100 * (
                    currWeekdayAverage()$avg_entry_sum[[1]] -
                        preCovidWeekdayAverage$avg_entry_sum[[1]]
                ) / preCovidWeekdayAverage$avg_entry_sum[[1]]
            )
        })
    output$percentageExitChangeWeekday <-
        renderText({
            paste0(
                "Percentage Weekday Exit Change from February Average: ",
                100 * (
                    currWeekdayAverage()$avg_exit_sum[[1]] -
                        preCovidWeekdayAverage$avg_exit_sum[[1]]
                ) / preCovidWeekdayAverage$avg_exit_sum[[1]]
            )
        })
    output$percentageEntryChangeWeekend <-
        renderText({
            paste0(
                "Percentage Weekend Entry Change from February Average: ",
                100 * (
                    currWeekendAverage()$avg_entry_sum[[1]] -
                        preCovidWeekendAverage$avg_entry_sum[[1]]
                ) / preCovidWeekendAverage$avg_entry_sum[[1]]
            )
        })
    output$percentageExitChangeWeekend <-
        renderText({
            paste0(
                "Percentage Weekend Exit Change from February Average: ",
                100 * (
                    currWeekendAverage()$avg_exit_sum[[1]] -
                        preCovidWeekendAverage$avg_exit_sum[[1]]
                ) / preCovidWeekendAverage$avg_exit_sum[[1]]
            )
        })
        
    output$distPlot <- renderPlot({
        ggplot(
            data = currData() %>% pivot_longer(ends_with("_sum"),
                                            names_to = 'sumtype',
                                            values_to = 'the_sum')
        ) +
            geom_line(mapping = aes(
                x = the_date,
                y = the_sum,
                group = sumtype,
                color = sumtype
            )) +
            geom_point(mapping = aes(
                x = the_date,
                y = the_sum,
                group = sumtype,
                color = sumtype
            )) +
            labs(
                title = "Total Daily Turnstile Entries and Exits",
                x = "Date",
                y = "Total Daily Count",
                color = "Legend",
                subtitle = paste(
                    "Orange Vertical Line is when State of Emergency was Declared in NYC (March 12)",
                    "Red Vertical Line is when Statewide Stay-At-Home Order was Enacted (March 22)",
                    sep = "\n"
                )
            ) +
            scale_color_manual(
                values = c("#7cae00", "#00bfc4"),
                labels = c("Entries", "Exits")
            ) +
            theme(legend.position = "right") +
            scale_y_continuous(labels = scales::comma) +
            geom_vline(
                mapping = aes(xintercept = mdy("03-12-2020")),
                color = "dark orange",
                linetype = "dashed"
            ) +
            geom_vline(
                mapping = aes(xintercept = mdy("03-22-2020")),
                color = "red",
                linetype = "dashed"
            )
        
    })
    
})
