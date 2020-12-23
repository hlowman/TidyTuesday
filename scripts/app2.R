# Shiny Hacky Hour
# December 22nd, 2020
# Heili Lowman


# Setup -------------------------------------------------------------------

# Anything that should happen only ONCE when the site is first launched should be placed in the setup section.

# Load packages
library(palmerpenguins)
library(tidyverse)
library(shiny)
library(shinythemes)
library(calecopal)

# Load data
penguins <- palmerpenguins::penguins

# User interface ----------------------------------------------------------

# Anything from here on should be the reactive/interactive parts of the application.

# Create the user interface - this is what the user will see.
ui <- fluidPage(
  
  # New theme/background color
  theme = shinytheme("superhero"),
  
  # App title
  titlePanel("*PalmerPenguins Shiny Polar Playground*"),
  
  # Line break
  br(),
  
  # Sidebar - widgets will go here
  sidebarLayout(
    sidebarPanel("Widgets:",
      
      # Line break
      br(),
      
      # URL for code - using HTML so that we can insert a hyperlink
      HTML("<p>Code for shiny widgets can be found at <a href='https://shiny.rstudio.com/gallery/widget-gallery.html'>https://shiny.rstudio.com/gallery/widget-gallery.html</a>.<p>"),
      
      # Adding some more widget resources
      HTML("<p>If you'd like to get even fancier with your widgets and possible selection options, check out the <i>shinyWidgets</i> package at <a href='http://shinyapps.dreamrs.fr/shinyWidgets/'>http://shinyapps.dreamrs.fr/shinyWidgets/</a>.<p>"),
      
      # Text input
      textInput("text", label = h3("Add notes to display here."), value = "Enter text..."),
      
      # Radio button input/widget
      radioButtons("yradio", label = h3("Figure 1: Choose the boxplot y-axis."),
        choices = list("Bill Length (mm)" = "bill_length_mm", 
          "Bill Depth (mm)" = "bill_depth_mm", 
          "Flipper Length (mm)" = "flipper_length_mm",
          "Body Mass (g)" = "body_mass_g"), 
        selected = "bill_length_mm")),
    
    # Main panel - outputs will go here
    mainPanel("Outputs:",
      
      # Sample output - changed to htmlOutput() for a cleaner rendering
      htmlOutput(outputId = "TT1"),
      
      # Divider
      hr(),
      
      # Text output
      fluidRow(column(6, verbatimTextOutput("value"))),
      # Note - the column call is used to denote the width of your output.
      # In Shiny, the page is split into 12 units, so using the "6" above tells Shiny to use half of the width of the page for the chosen output.
      
      # Divider
      hr(),
      
      # Plot output
      plotOutput(outputId = "customplot"))
  )
)

# Remember to always keep track of your parentheses!
# Be sure to fully close your user interface with parentheses before starting the server code.

# Server ------------------------------------------------------------------

# Create the server function - this is what's going on behind the scenes.
server <- function(input, output){
  
  # reactive datasets, graphs, tables, etc. may be created here
  
  # first text output - prints the text below
  output$TT1 <- renderText({
    paste0("You can add outputs like this. Every output (text, table, plot) for the user interface has a render function equivalent (renderText, renderTable, renderPlot) you should use in the server.")
  })
  
  # second text output - prints the text provided by the user
  output$value <- renderPrint({paste0(input$text) })
  
  # ggplot output - uses y-axis chosen by the user
  output$customplot <- renderPlot({

    penguins %>% # use our original dataset
    mutate(year_f = factor(year)) %>% # make year a factor
    pivot_longer(cols = c(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g),
      names_to = "stats", values_to = "stats_value") %>% # need to pivot to be able to select for y axis
    filter(stats == input$yradio) %>% # filter the new column named "stats" by the user's selection for y axis
    ggplot(aes(x = year_f, y = stats_value)) + # and now create the resulting ggplot
    geom_boxplot(aes(color = island, fill = island), alpha = 0.35) +
    scale_color_manual(values = cal_palette(name = "lake")) + # requires calecopal palette
    scale_fill_manual(values = cal_palette(name = "lake")) +  
    labs(x = "Year",
      y = case_when(input$yradio == "bill_length_mm" ~ "Bill Length (mm)",
        input$yradio == "bill_depth_mm" ~ "Bill Depth (mm)",
        input$yradio == "flipper_length_mm" ~ "Flipper Length (mm)",
        input$yradio == "body_mass_g" ~ "Body Mass (g)"),
      title = "Figure 1: Penguin Stats by Antarctic Island",
      caption = "Data Source: Gorman et al., 2014") +
    theme_classic() +
    guides(color = FALSE, fill = FALSE) +
    facet_grid(~island)
    
  }) # closes out ggplot output code
  
} # closes out server


# Unified app -------------------------------------------------------------

# Combine the user interface and the server into a Shiny app.
shinyApp(ui = ui, server = server)

# End of script.
