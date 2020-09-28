# Tidy Tuesday Week 1
# Heili Lowman
# August 11, 2020

#### Intro ####

# Everything preceded by a "#" is considered a comment and will not be run by R.

# Note: The "####" notation above will help you to create a table of contents for your script.
# Click the table of contents looking button to the top right of your screen to see it expanded.

# Create a new variable.
leaf <- 11

# To run the current line:
# Mac: Command + Enter
# PC: Control + Enter

waterfall <- 20

# To run the entire script:
# Mac: Command + Shift + Enter
# PC: Control + Shift + Enter

forest <- leaf * waterfall

# Notice how these variables are stored in your Environment.

#### Datasets ####

# R has lots of built in datasets that you can explore by running the command data() in your console.
# More information about these is also available online.

# Look at our data.
View(iris)

# See the top part of dataset.
head(iris)

# See the bottom part of dataset.
tail(iris)

# See one column.
iris$Sepal.Length

# The dollar sign ("$") tells R to pull out a particular column.

# Assign a column to a new variable.
new <- iris$Sepal.Width

# Calculate the mean value of a column and assign to a new variable.
mean_sl <- mean(iris$Sepal.Length)

# Calculate the minimum - min()
# Calculate the maximum - max()

# Examine the structure of your data.
str(iris)

# num refers to numerical.
# Factor refers to categorical.
# Other structures include dates, characters, integers, complex, etc. 
# If a cell is empty, you might see NULL or NA.

#### Plot ####

# Attach the tidyverse package.
library(tidyverse)

# The tidyverse package contains lots of other useful packages and functions, like ggplot().

# Load dataset.
survey <- read_csv("survey_data.csv")

# Examine dataset in new tab.
View(survey)

# Created a basic plot and assign it to variable "survey_graph" so it's stored in your Environment.
survey_graph <- ggplot(survey, aes(x = Tools, y = Know)) + # Pulls in the basic dataset.
  geom_point() + # Instructs R to create a scatterplot.
  ggtitle("Familiar Data Science Tools") + # Adds a title.
  xlab("Data Tools") + # Labels x axis.
  ylab("Number of Respondents") # Labels y axis.

# Call the plot to your "Plots" tab on the right hand side of your screen.
survey_graph

# Created a plot with larger, colored points, y axis adjusted, and grey background removed.
survey_graph2 <- ggplot(survey, aes(x = Tools, y = Know)) +
  geom_point(color = "coral1", size = 5) + # Edit point size and color.
  ggtitle("Familiar Data Science Tools") +
  xlab("Data Tools") +
  ylim(0,25) + # Edit y axis bounds.
  ylab("Number of Respondents") +
  theme_bw() # Remove grey background.

# Call the new plot.
survey_graph2

# End of script.