#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(labelled)
library(here)
countries_of_interest <-
  c(
    "australia",
    "brazil",
    "canada",
    "china",
    "denmark",
    "finland",
    "france",
    "germany",
    #"hong-kong",
    "india",
    "israel",
    "indonesia",
    "italy",
    "japan",
    "malaysia",
    "mexico",
    "netherlands",
    "norway",
    "philippines",
    "saudi-arabia",
    "singapore",
    "south-korea",
    "spain",
    "sweden",
    "taiwan",
    "thailand",
    "united-arab-emirates",
    "united-kingdom",
    "united-states",
    "vietnam"
  ) %>% sort()


cases_dat <-
  read_csv(here("imperialdata/Gathered Data/case numbers/cases.csv"))
dat <- read_csv(here("temp.csv")) %>%
#dat <- read_csv(here("imperialdata/all_countries_numerical.csv"), guess_max = 20000) %>%
  mutate(date = str_sub(endtime, start = 0, end = 10)) %>%
  mutate(date = dmy(date)) %>%
  rowwise() %>%
  mutate(behavior = 6-mean(c(
    i12_health_1,
    i12_health_2,
    i12_health_3,
    i12_health_4,
    i12_health_5,
    i12_health_6,
    i12_health_7,
    i12_health_8,
    i12_health_9,
    i12_health_10,
    i12_health_11,
    i12_health_12,
    #i12_health_13,
    #i12_health_14,
    #i12_health_15,
    #i12_health_16,
    #i12_health_17,
    #i12_health_18,
    #i12_health_19,
    #i12_health_20,
    na.rm = TRUE

    ))) %>%
  mutate(age_group = cut(age, breaks=c(-Inf, 35, Inf),
                        labels=c("younger adults","older adults"))) %>%
  filter(country %in% countries_of_interest)




# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Cases vs. behavior"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "country",
        "Select Country",
        choices = countries_of_interest,
        selected = "germany"
      ),
      shiny::checkboxInput("logscale", "Logscale", value = FALSE),
      varSelectInput("variable", "Variable:", dat, selected = "behavior"),
      varSelectInput("facetvar", "Color by:", dat, selected = "gender"),
      sliderInput(
        "limity",
        "Limit",
        min = 0,
        max = 100,
        value = 100,
        step = 1
      )

    ),

    # Show a plot of the generated distribution
    mainPanel(plotOutput("casesPlot", height = "200px"),
              plotOutput("plot2", height = "200px"),
              plotOutput("sd_plot", height = "200px"),
              plotOutput("plotCount", height = "200px"))
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  max_date <- reactive({
    max1 <- dat %>%
      filter(country == input$country) %>%
      select(date,!!input$variable) %>%
      filter(!is.na(!!input$variable)) %>%
      pull(date) %>% max()
    max2 <- cases_dat %>%
      filter(country == input$country) %>%
      pull(date) %>% max()

    min(max1, max2)
  })


  min_date <- reactive({
    min1 <- dat %>%
      filter(country == input$country) %>%
      select(date,!!input$variable) %>%
      filter(!is.na(!!input$variable)) %>%
      pull(date) %>% min()
    min2 <- cases_dat %>%
      filter(country == input$country) %>%
      pull(date) %>% min()

    max(min1, min2)
  })

  filtered_data <- reactive({
    dat %>%
      #filter(country == "germany") %>%
      filter(country == input$country) %>%
      # update variable
      select(!!input$facetvar, date,!!input$variable) %>%
      filter(!is.na(!!input$variable)) %>%
      #mutate(!!input$variable = as_factor(!!input$variable)) %>%
      filter(!!input$variable < input$limity)
  })

  output$plot2 <- renderPlot({
     filtered_data() %>%
      ggplot() +
      aes(x = date,
          y = !!input$variable,
          color = !!input$facetvar) +

      scale_x_date(limits = c(min_date(), max_date())) +
      geom_jitter(alpha = 0.1, width = 0.01, height = 0.1) +

      #geom_boxplot() +
      geom_smooth(span = 0.1) +
      theme(legend.position = "bottom") +
      # facet_wrap(~gender, ncol = 1) +
      NULL
  })



  output$casesPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    p <- cases_dat %>%
      filter(country == input$country) %>%
      ggplot() +
      aes(x = date, y = confirmed_roll) +
      scale_x_date(limits = c(min_date(), max_date())) +
      geom_point()

    if(input$logscale){
      p <- p + ggplot2::scale_y_log10()
    }
    p
  })


  output$sd_plot <- renderPlot({
    filtered_data()  %>%
      #mutate(!!input$variable = as_factor(!!input$variable)) %>%
      mutate(bin_date = floor_date(date, "week")) %>%
      mutate(bin_date = (bin_date)) %>%
      ggplot() +
      aes(group = format(bin_date, "%d %m %y"),
          y = !!input$variable,
          x = bin_date
      ) +

      scale_x_date(limits = c(min_date(), max_date())) +
      #geom_violin() +
      geom_boxplot() +
      #stat_summary_bin(fun.data = "mean_sdl") +
      theme(legend.position = "bottom") +
      # facet_wrap(~gender, ncol = 1) +
      NULL
  })

  output$plotCount <- renderPlot({
    filtered_data() %>%
      mutate(date = floor_date(date, "week")) %>%
      ggplot() +
      aes(x = date) +
      scale_x_date(limits = c(min_date(), max_date())) +
      stat_count() +
      theme(legend.position = "bottom") +
      # facet_wrap(~gender, ncol = 1) +
      NULL
  })

}

# Run the application
shinyApp(ui = ui, server = server)
