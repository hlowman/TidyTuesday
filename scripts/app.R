# Shiny tutorial
# Heili Lowman
# October 27, 2020

# Shiny application = Packages/Data + User Interface + Server + shinyApp()

# Setup
# Load packages.
library(tidyverse)
library(shiny)
library(shinythemes)

# Load data.
spooky <- read_csv("spooky_data.csv")

# User interface:
ui <- fluidPage(
  theme = shinytheme("slate"),
  titlePanel("Spooky Shiny App"), # title
  sidebarLayout( # created a sidebar
    sidebarPanel("Widgets go here.",
      selectInput(inputId = "state_select", # variable name
        label = "Choose a state", # title for user
        choices = unique(spooky$state) # choices displayed
        ), # dropdown menu widget
      radioButtons(inputId = "region_select",
        label = "Choose a region",
        choices = unique(spooky$region_us_census))
      ), # multiple choice widget
    mainPanel("Outputs go here.", # created a main panel
      p("State's top candies:"), # table output
      tableOutput(outputId = "candy_table"),
      p("Region's top costumes:"), # plot output
      plotOutput(outputId = "costume_graph")
      )
  )
)

# Server:
server <- function(input, output){
  # create a new dataset
  state_candy <- reactive({
    spooky %>%
      filter(state == input$state_select) %>%
      select(candy, pounds_candy_sold)
  })
  # create a table
  output$candy_table <- renderTable({
    state_candy()
  })
  # create yet another dataset
  region_costume <- reactive({
    spooky %>%
      filter(region_us_census == input$region_select) %>%
      count(costume, rank)
  })
  # create a plot
  output$costume_graph <- renderPlot({
    ggplot(region_costume(), aes(x = costume, y = n)) +
      geom_col(aes(fill=rank)) +
      coord_flip() + # flips axes
      scale_fill_manual(values = c("black", "purple", "orange")) +
      theme_minimal()
  })
  
}

# Combine the user interface and server:
shinyApp(ui = ui, server = server)

# Additional Shiny resources:
# https://shiny.rstudio.com/gallery/widget-gallery.html
# http://shinyapps.dreamrs.fr/shinyWidgets/
# https://shiny.rstudio.com/gallery/#user-showcase

# End of script.