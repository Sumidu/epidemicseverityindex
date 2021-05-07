#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(coronavirus)
library(zoo)
library(scales)
source("corona_api.R")


europe <- sort(c("Germany", "Belgium", "Spain", "Italy", "UK", "France", "Denmark", "Norway", "Sweden", "Finland", "Portugal",
            "Slovakia", "Albania", "Andorra", "Austria", "Bosnia and Herzegovina", "Croatia", "Cyprus",
            "Czechia", "Greece", "Estonia", "Ireland", "Luxembourg", "Malta", "Montenegro", "Monaco", "Netherlands",
            "Poland", "Romania", "Serbia", "Switzerland"))


# UI ----
ui <- dashboardPage(
    dashboardHeader(title = "Epidemic Dashboard"),
    dashboardSidebar(sidebarMenu(
        menuItem("ESI heatmap", tabName = "dashboard", icon = icon("th")),
        menuItem("ESI Line Comparison", tabName = "lines", icon = icon("chart-line")),
        menuItem("Details on countries", tabName = "countries", icon = icon("flag"))
    )),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    fluidRow(
                        box(width = 4,
                            #title = "Country Selection",
                            selectInput("countries", "Select Countries", choices = europe, selected = europe, multiple = T,
                                        selectize = T)),
                        box(width = 4,
                            title = "Date Range", collapsible = T, collapsed = F,
                            uiOutput("dates")
                        ),
                        box(width = 4,
                            title = "ESI Calculation", collapsible = T, collapsed = F,
                            selectInput("algorithm", "ESI-Calculation",
                                        choices = c("Single Day", "Cumulative", "Rolling Window"), selected = "Rolling Window"),
                            sliderInput("window_size", "Rolling Windows Size", 2, 60, 7),
                            sliderInput("sev_param", "ESI Severity Parameter:", 1, 20, 13, step = 0.1)
                        ),
                        box(title = "ESI Heatmap", width = 12,
                            plotOutput("plot1", height = 500))
                    )
            ),

            # Second tab content
            tabItem(tabName = "lines",
                    fluidRow(
                        box(title = "ESI Line plot", width = 9,
                            plotOutput("plot_line", height = 500)),
                        box(width = 3,
                            title = "Controls",
                            selectInput("countries_highlight", "Highlight Countries", choices = europe, selected = c("Germany", "France"), multiple = T,
                                        selectize = T)
                            ),
                        box(title = "Total deaths", width = 9,
                            plotOutput("death_line", height = 500)),
                        box(title = "Total recoveries", width = 9,
                            plotOutput("recovery_line", height = 500))
                    )
            ),
            # Tird tab content
            tabItem(tabName = "countries",
                    fluidRow(
                        box(title = "Main Plot", width = 9,
                            plotOutput("plot_comp", height = 500)),
                        box(width = 3,
                            title = "Controls",
                            selectInput("countries_compare", "Compare countries", choices = europe, selected = c("Germany", "France"), multiple = T,
                                        selectize = T),
                            uiOutput("variable_control")
                            )
                    )
            )
        )
    )
)

server <- function(input, output) {

    # reactives ----
    cdata <- reactive({
        #coronavirus::coronavirus %>%
        #    filter(country %in% europe) %>%
        #    pivot_wider(names_from = "type", values_from = "cases")
        load_data() %>% filter(country %in% europe)
    })

    esi_data <- reactive({

        window_size <- input$window_size

        s_param <- input$sev_param
        req(input$countries)
        cdata() %>%
            filter(country %in% input$countries) %>%
            filter(date >= input$start_date) %>%
            filter(date <= input$end_date) %>%
            group_by(country, date) %>%
            summarise(death = sum(death),
                      recovered = sum(recovered), .groups = "drop") %>%
            group_by(country) %>%
            mutate(death_roll = rollapplyr(death, window_size, mean, partial = TRUE, na.rm = TRUE), # use of a 7-day rolling average
                   recovered_roll = rollapplyr(recovered, window_size, mean, partial = TRUE, na.rm = TRUE)) %>%
            mutate(death_cum = cumsum(death),
                   recovered_cum = cumsum(recovered)) %>%
            ungroup() %>%
            mutate(date = as.Date(date)) %>%
            mutate(esi = esi(death, recovered, severity = s_param)) %>%
            mutate(esi_cum = esi(death_cum, recovered_cum, severity = s_param)) %>%
            mutate(esi_roll = esi(death_roll, recovered_roll, severity = s_param))

    })


    # ui generators----
    output$dates <- renderUI({
        start <- min(cdata()$date)
        list(
            sliderInput("start_date", "Start date (d-m-y)", min = start, max = lubridate::today(), value = start, timeFormat="%d-%m-%y"),
            sliderInput("end_date", "End date", min = start, max = lubridate::today(), value = lubridate::today(), timeFormat="%d-%m-%y")
        )
    })


    # plot 1 -----
    output$plot1 <- renderPlot({

        req(input$countries)
        req(input$start_date)
        req(input$end_date)
        if(input$algorithm == "Single Day"){
            my_aes <- aes(x = date, fill = esi, y = country)
        }
        if(input$algorithm ==  "Cumulative"){
            my_aes <- aes(x = date, fill = esi_cum, y = country)
        }
        if(input$algorithm == "Rolling Window"){
            my_aes <- aes(x = date, fill = esi_roll, y = country)
        }

        s_param <- input$sev_param
        esi_data() %>%
            ggplot() +
            my_aes +
            geom_tile() +
            #guides(color = FALSE) +
            NULL +
            scale_fill_viridis_c("ESI", option = "D") +
            scale_x_date(date_breaks = "month", labels = label_date_short()) +
            scale_y_discrete(limits = rev) +
            cowplot::theme_minimal_hgrid() +
            labs(x = "Date", y= NULL, caption = paste("ESI S-Parameter = ",s_param, "- Gray areas denote data errors."),
                 title = "Epidemic Severity Index (ESI) in Europe over time")
    })

    output$plot_line <- renderPlot({

        if(input$algorithm == "Single Day"){
            my_aes <- aes(x = date, y = esi, color = country)
        }
        if(input$algorithm ==  "Cumulative"){
            my_aes <- aes(x = date, y = esi_cum, color = country)
        }
        if(input$algorithm == "Rolling Window"){
            my_aes <- aes(x = date, y = esi_roll, color = country)
        }

        s_param <- input$sev_param

        esi_data() %>%
        #filter(country %in%  c("Germany", "Belgium", "Spain", "France", "Switzerland")) %>%
            ggplot() +
            my_aes +
            geom_line() +
            gghighlight::gghighlight(country %in% input$countries_highlight) +
            cowplot::theme_minimal_hgrid() +
            labs(x = "Date", y= "ESI", caption = paste("ESI S-Parameter = ",s_param),
                 title = "Epidemic Severity Index (ESI) in Europe over time")
    })

    output$death_line <- renderPlot({
        if(input$algorithm == "Single Day"){
            my_aes <- aes(x = date, y = death, color = country)
        }
        if(input$algorithm ==  "Cumulative"){
            my_aes <- aes(x = date, y = death_cum, color = country)
        }
        if(input$algorithm == "Rolling Window"){
            my_aes <- aes(x = date, y = death_roll, color = country)
        }

        s_param <- input$sev_param

        esi_data() %>%
            #filter(country %in%  c("Germany", "Belgium", "Spain", "France", "Switzerland")) %>%
            ggplot() +
            my_aes +
            geom_line() +
            gghighlight::gghighlight(country %in% input$countries_highlight) +
            cowplot::theme_minimal_hgrid() +
            labs(x = "Date", y= "Deaths", caption = paste("Data shown using",input$algorithm),
                 title = "Deaths in Europe over time")
    })


    output$recovery_line <- renderPlot({
        if(input$algorithm == "Single Day"){
            my_aes <- aes(x = date, y = recovered, color = country)
        }
        if(input$algorithm ==  "Cumulative"){
            my_aes <- aes(x = date, y = recovered_cum, color = country)
        }
        if(input$algorithm == "Rolling Window"){
            my_aes <- aes(x = date, y = recovered_roll, color = country)
        }

        s_param <- input$sev_param

        esi_data() %>%
            #filter(country %in%  c("Germany", "Belgium", "Spain", "France", "Switzerland")) %>%
            ggplot() +
            my_aes +
            geom_line() +
            gghighlight::gghighlight(country %in% input$countries_highlight, use_group_by = FALSE) +
            cowplot::theme_minimal_hgrid() +
            labs(x = "Date", y= "Recoveries", caption = paste("Data shown using",input$algorithm),
                 title = "Recoveries in Europe over time")
    })



    # last page ----
    long_data <- reactive({
        req(input$window_size)
        req(input$sev_param)
        s_param <- input$sev_param
        window_size <- input$window_size
        cdata() %>% group_by(country) %>%
            mutate(death_roll = rollapplyr(death, window_size, mean, partial = TRUE, na.rm = TRUE), # use of a 7-day rolling average
                   death_roll_pm = rollapplyr(death_pm, window_size, mean, partial = TRUE, na.rm = TRUE),
                   recovered_roll = rollapplyr(recovered, window_size, mean, partial = TRUE, na.rm = TRUE),
                   confirmed_roll = rollapplyr(confirmed, window_size, mean, partial = TRUE, na.rm = TRUE),
                   confirmed_roll_pm = rollapplyr(confirmed_pm, window_size, mean, partial = TRUE, na.rm = TRUE)) %>%
            group_by(country, date) %>%
            mutate(esi = esi(death, recovered, severity = s_param)) %>%
            mutate(esi_cum = esi(death_cum, recovered_cum, severity = s_param)) %>%
            mutate(esi_roll = esi(death_roll, recovered_roll, severity = s_param)) %>%
            pivot_longer(cols = c("death", "confirmed", "recovered", "confirmed_cum", "recovered_cum", "death_cum", "active",
                                  "death_pm", "confirmed_pm", "recovered_pm", "death_roll_pm",
                                  "death_cum_pm", "confirmed_cum_pm", "recovered_cum_pm",  "active_pm",
                                  "death_roll", "recovered_roll", "confirmed_roll", "confirmed_roll_pm",
                                  "esi", "esi_cum", "esi_roll"
            ), names_to = "variable", values_to = "value")
    })



    clist <- list(`Deaths` = "death",
                  `Deaths cumulated` = "death_cum",
                  `Deaths rolling avg` = "death_roll",
                  `Deaths per million` = "death_pm",
                  `Deaths cumulated per million` = "death_cum_pm",
                  `Deaths rolling avg per million` = "death_roll_pm",
                  `Confirmed` = "confirmed",
                  `Confirmed cumulated` = "confirmed_cum",
                  `Confirmed rolling avg` = "confirmed_roll",
                  `Confirmed per million` = "confirmed_pm",
                  `Confirmed cumulated per million` = "confirmed_cum_pm",
                  `Confirmed rolling avg per million` = "confirmed_roll_pm",
                  `Confirmed` = "confirmed",
                  `Recovered cumulated` = "recovered_cum",
                  `Recovered rolling avg` = "recovered_roll",
                  `Recovered per million` = "recovered_pm",
                  `Recovered cumulated per million` = "recovered_cum_pm",
                  `Active cases` = "active",
                  `ESI` = "esi",
                  `ESI cumulated` = "esi_cum",
                  `ESI rolling avg` = "esi_roll"
    )

    llist <- enframe(clist) %>%
        select(2:1) %>%
        deframe()

    output$variable_control <- renderUI({
        ld <- long_data()
        #ld$variable %>% unique() -> variables

        selectInput("select_var", "Select variable", choices = clist, selected = clist[c(3,15,21)],
                    multiple = T,
                    selectize = T)
    })

    output$plot_comp <- renderPlot({
        req(input$countries_compare)
        req(input$select_var)
        my_aes <- aes(x = date, y = value, color = country, group = interaction(country,variable))

        long_data() %>%
            filter(country %in% input$countries_compare ) %>%
            filter(variable %in% input$select_var) -> plot_data

        plot_data %>%
            ggplot() +
            my_aes +
            geom_line() +
            cowplot::theme_minimal_hgrid() +
            facet_wrap(vars(variable), scales = "free_y", ncol = 1, labeller = labeller(variable = llist)) +
            labs(x = "Date", y = "Value", #, caption = paste("Data shown:", input$select_var),
                 title = "Comparison in Europe over time")
    })

}

shinyApp(ui, server)
