# Tidy Tuesday Tutorial
# Led by Kris Taniguchi-Quan
# September 15, 2020

#### For Loops ####

# For loops allow you to automate code (batch process) when there's a lot of repetition.

# The "for" in for loops means that you know how many times (i.e., iterations) that you're going to run a given loop.

# There are other, more advanced options to iterate (e.g., apply functions, map() from the purrr package), but today will serve as the building blocks for learning about how iteration works more generally.

# In the example discussed in the powerpoint today, we also learned about ifelse statements; more info about ifelse statements can be found here: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/ifelse.

# Install packages.
library(tidyverse)

#### Simple Loop Example ####

# print out "This year is [year]" where [year] is from 2010 to 2015.

# inefficient way
print(paste("This year is", 2010))
print(paste("This year is", 2011))
print(paste("This year is", 2012))
print(paste("This year is", 2013))
print(paste("This year is", 2014))
print(paste("This year is", 2015))

# Usually, a good rule of thumb, is if you are copy/pasting more than 2-3 times, it may be time to consider automating your work.

# for loop automates repetitive code chunks
for(year in 2010:2015) {
  print(paste("This year is", year))
}

# you can use any variable name - often folks will use "i" for "iteration"
for(i in 2010:2015) {
  print(paste("This year is", i))
}

# run loop based on a vector - useful if you want to do something with every value in a list
year <- 2010:2025

# be sure to use a different variable name to prevent errors!
for(j in year) {
  print(paste("This year is", j))
}

# if you want to create output vector, you have to do that manually
x <- 2:20

# output vector y which will be filled in by the for loop
y <- rep(NA, length(x)) # create an empty vector of NAs that is the same length as x

# concept: indexing
x[1] # first element of vector x
x[1] <- 100 # set value of first element of vector x
z <- 1
x[z] # you can use variable values to perform the same indexing as above

# matrix[1,2] # whenever you're calling values from a full dataset, you first call the row element, then the column element. so this statement would call the value from the first row in the second column of dataframe matrix.

# for loop
for(k in 1:length(x)) { # iterate from first to last element in vector x
  y[k] <- x[k]^2 # save input of x^s to output vector y for every iteration of k
}

y <- x^2 # also accomplishes the same as above, since R can process vectors

#### Loops with GGplot ####

# for loops are very helpful for data exploration
# today, we'll use R's built-in "iris" dataset
View(iris)

# save iris to environment
iris <- iris

# for loop to create multiple boxplots

# (1) create a list of variables you'd like to plot
col.names <- names(iris) # output column names in a dataframe
variables <- col.names[1:4] # create a vector of the column names you intend to plot

# (2) tidy the dataframe
iris_tidy <- pivot_longer(iris, col = variables, names_to = "attribute", values_to = "values")

# (3) for loop to plot each attribute
for(z in 1:length(variables)){
  # subset the data by attribute
  subset <- subset(iris_tidy, attribute == variables[z]) # subset the larger iris_tidy dataset by each attribute from the variables list (total of 4)
  
  # create boxplots for attribute z
  boxplot <- ggplot(subset, aes(x = Species, y = values, fill = Species)) + # use the data from above
    geom_boxplot() + # create a series of boxplots
    scale_fill_brewer(palette = "Dark2") + # designate a set color palette
    theme_minimal()
  
  # create a customized y-axis
  yaxis.label <- gsub("\\.", " ", variables[z])
  boxplot <- boxplot +
    labs(y = yaxis.label)
  
  # display plot
  print(boxplot)
  
  # export plots
  # create a file name and set working directory (can be different from your project directory)
  file.name <- paste0("/Users/heilil/Desktop/R_figures/TidyTuesday/", variables[z], "_boxplot.jpg") # create file name
  # set specifications for size and resolution of your figure
  ggsave(boxplot,
       filename = file.name,
       dpi = 300,
       width = 4,
       height = 6,
       units = "in"
     )
}

# End of script.
