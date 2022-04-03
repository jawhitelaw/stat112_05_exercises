library(shiny)
library(tidyverse)

covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
census_pop_est_2018 <- read_csv("https://www.dropbox.com/s/6txwv3b4ng7pepe/us_census_2018_state_pop_est.csv?dl=1") %>% 
  separate(state, into = c("dot","state"), extra = "merge") %>% 
  select(-dot) %>% 
  mutate(state = str_to_title(state))

covid19_clean <- covid19 %>% 
  left_join(census_pop_est_2018, by = c("state" = "state")) %>% 
  mutate(cases_per_100k = (cases/est_pop_2018)*100000)

ui <- fluidPage(
  sliderInput("Dates",
              "Dates:",
              min = as.Date("2020-01-21","%Y-%m-%d"),
              max = as.Date("2022-03-31","%Y-%m-%d"),
              value=as.Date(c("2020-01-21","2022-03-31")),
              timeFormat="%Y-%m-%d"),
  selectInput("state", 
              "State", 
              choices = unique(covid19$state), multiple = TRUE),
  submitButton(text = "Create my plot!"),
  plotOutput(outputId = "timeplot")
)

server <- function(input, output) {
  output$timeplot <- renderPlot({
    covid19_clean %>% 
      filter(state %in% input$state) %>% 
      ggplot() +
      geom_line(aes(x = date, y = cases_per_100k, color = state)) +
      scale_x_continuous(limits = input$Dates) +
      labs(x = "Date", y = "Cases per 100,000 residents") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)