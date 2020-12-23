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
library(calecopal) # You may need to run library(devtools) following by devtools::install_github("an-bui/calecopal") in your console.
library(lubridate) # You may need to run install.packages("lubridate") in your console.
library(ggbeeswarm) # You may need to run install.packages("ggbeeswarm") in your console.

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
      
      # Radio button input - Figure 1
      radioButtons("yradio", label = h3("Figure 1: Choose the boxplot y-axis."),
        choices = list("Bill Length (mm)" = "bill_length_mm", 
          "Bill Depth (mm)" = "bill_depth_mm", 
          "Flipper Length (mm)" = "flipper_length_mm",
          "Body Mass (g)" = "body_mass_g"), 
        selected = "bill_length_mm"),
    
      # Radio button input - Figure 2
      radioButtons(inputId = "radGeom", label = h3("Figure 2: Choose your plot type."),
        choices = c(geom_boxplot = "Boxplot", 
          geom_violin = "Violinplot",
          geom_beeswarm = "Beeswarm"), 
        selected = "Boxplot"),
    
      # Dropdown menu (dependent on radio button) - Figure 2
      conditionalPanel(condition = "input.radGeom == 'Beeswarm'",
        selectInput(inputId = "HappySad",
        label = h3("Figure 2: If you chose a 'beeswarm' plot, would you prefer happy, neutral, or sad bees?"),
        choices = c("smiley" = "happy","frowny" ="sad", "quasirandom" = "neutral"),
        selected = "happy")),
    
      # Slider - Figure 2
      sliderInput(inputId = "slider",
        label = h3("Figure 2: Choose the year you would like to display."),
        min = 2007,
        max = 2009,
        step = 1,
        value = 2007,
        timeFormat = TRUE)),
    
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
      
      # Figure 1 output
      plotOutput(outputId = "plot1"),
      
      # Divider
      hr(),
      
      # Figure 2 output
      plotOutput(outputId = "plot2")
      
    ) # closes out mainPanel
  ) # closes out sidebarLayout
) # closes out fluidPage

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
  
  # first ggplot output - uses y-axis chosen by the user
  output$plot1 <- renderPlot({

    penguins %>% # use our original dataset
    mutate(year_f = factor(year)) %>% # make year a factor
    pivot_longer(cols = c(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g),
      names_to = "stats", values_to = "stats_value") %>% # need to pivot to be able to select for y axis
      # I've pivoted here because it's easier to select from values within a column than select a column itself.
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
    
  }) # closes out first ggplot output code
  
  # second ggplot output - uses plot type, beeswarm type, and year chosen by user
  output$plot2 <- renderPlot({
    
    # changes type of beeswarm
    bee.type <- switch(input$HappySad,
      "happy" 	= "smiley",
      "sad" = "frowney",
      "neutral" = "quasirandom")
    
    # changes type of plot
    plot.type <- switch(input$radGeom,
      "Boxplot" 	= geom_boxplot(alpha = 0.7),
      "Violinplot" = geom_violin(alpha = 0.7),
      "Beeswarm" = geom_quasirandom(alpha = 0.7, method = bee.type, groupOnX = TRUE, cex = 2))
    #groupOnX specifies groups on y axis
    
    # assigns year chosen by user
    yearSlider <- input$slider
    
    penguins %>% # uses loaded dataset
      rename(flipper = flipper_length_mm, bill_length = bill_length_mm, bill_depth = bill_depth_mm) %>% # renames columns
      mutate(mass_kg = body_mass_g/1000, bill_ratio = bill_length/bill_depth) %>% # creates new body mass column in Kg
      mutate(yearDate = years(year)) %>% # creates new formatted year column
      na.omit() %>% # removes NAs
      filter(year == yearSlider) %>% # filter year by user input
      ggplot(aes(x = species, y = flipper, color = species)) + # begins the plot code 
      plot.type + # assigns geometry based on user input
      labs(y = "Flipper Length (mm)", 
        x = "Species", 
        title = "Figure 2: Penguin Stats by Species") +
      scale_y_continuous(limits = c(160, 240), breaks = c(160, 180, 200, 220, 240)) +
      scale_color_manual(values = c("darkorange", "darkorchid", "cyan4")) +
      theme_classic() +
      theme(axis.text.y=element_text(size=20, color= 'black'), 
        axis.text.x=element_text(size=20, color= 'black'), 
        axis.line.y=element_line(color = 'black',size=0.5),
        axis.line.x=element_line(color = 'black',size=0.5),
        axis.ticks.y=element_line(size=0.5),
        axis.ticks.x=element_line(size=0),
        axis.line = element_line(colour = "black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        legend.text = element_text(size = 18),
        legend.title = element_blank())
    
  }) # closes out second ggplot output code
  
} # closes out server


# Unified app -------------------------------------------------------------

# Combine the user interface and the server into a Shiny app.
shinyApp(ui = ui, server = server)

# End of script.
