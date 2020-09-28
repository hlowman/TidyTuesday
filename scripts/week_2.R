# Tidy Tuesday Week 2
# Heili Lowman
# August 18, 2020

# This week's lesson will be focused on data tidying and manipulation, as an alternative to data modification in Excel.

#### dplyr() tutorial [Robert] ####

# Use the "####" or "----" notation to create a table of contents/outline for your script.

# Note: the functions below actually belong to the dplyr package, and the tidyverse is a bundle of multiple packages (dplyr, ggplot2, etc.).
# Often, packages are compared to a wizard's spell books. You need to first take the book off the shelf to use it (attach using the library() function) and then you can start using the spells in the book (the many functions within a given package).

# Load the tidyverse package, which all of the following functions are a part of.
library(tidyverse)

# Glimpse the iris data in the console. This is one of R's built in datasets. If you want to see the full listing, run data() in your console.
glimpse(iris)

# Open the data in a new tab.
View(iris)

# ----- Select -----
# Use the select() function to select column(s).

# select only one column
select(iris, Petal.Length)

# ----- Filter -----
# Use the filter() function to filter the data based on a certain condition that you specify.

# filter by a condition
filter(iris, Petal.Length > 5) # filters the full dataset for petal lengths greater than 5.

# filter by multiple conditions
filter(iris, Sepal.Length > 3, Species != 'setosa') # filters the full dataset for sepal lengths greater than 3 and all species that are not (!=) setosa. You can use the "!" to negate logic operators, so != means "does not equal".

filter(iris, Sepal.Length >= 3, Petal.Length < 1.5, Species == 'setosa') # filters the full dataset for sepal lengths greater than or equal to 3, petal lengths less than 1.5, and only the setosa species.

# ----- Rename -----
# Use the rename() function to rename columns.
# rename(data, newcolname = oldcolname)

rename(iris, taxa = Species) # rename the Species column to be taxa.

rename(iris, taxa = Species, s.length = Sepal.Length) # also rename the Sepal.Length column to be s.length.

# ----- Mutate -----
# Use the mutate() function to create new columns.
# mutate(data, newcolumn = stuff you did with existing columns)

mutate(iris, total.length = Sepal.Length + Petal.Length) # create a new column equal to the sum of sepal and petal lengths.

mutate(iris, sum.Length = Sepal.Length + Species) # create a new column equal to the sum of the sepal length and species.
# WARNING: This will yield an error message since Species is not a number.

# FUN FACT: To call back a previous line of code in the console (the section below the script), place your cursor in the console, hit the up arrow, and it will cycle through the lines of code you've recently run.

#### piping tutorial [Annie] ####

# When you're manipulating a dataset, it's best practice to keep a copy of the raw data and then make edits to newly created datasets.

# call in the original dataset (again, we're using iris, which is built into R)
iris <- iris

# create new dataset with new column called "total.length"
iris_1 <- mutate(iris, total.length = Sepal.Length + Petal.Length)

# filter the dataset by this newly created column for values greater than 10
iris_2 <- filter(iris_1, total.length > 10)

# Step-by-step manipulations can be tedious, so we can use piping to streamline our code.
# Pipe operators (%>%) allow you to run all your manipulations in a single code chunk.
# %>% Shortcuts:  
# Mac: Command + Shift + M
# PC: Ctrl + Shift + M

# Create a new column total.length, keep values in this new column greater than 10, select only species and total.length columns to include in the final dataset.
iris_2_pipe <- mutate(iris, total.length = Sepal.Length + Petal.Length) %>%
  filter(total.length > 10) %>%
  select(Species, total.length)

# Create a new column petal.area, keep values from this new column that are less than 10.
iris_ex <- mutate(iris, petal.area = Petal.Length * Petal.Width) %>%
  filter(petal.area < 10)

# Another way of structuring your piping is to state your dataset first. Comments below translate what is happening in words.

iris_demo <- iris %>% # Create a new dataset called "iris_demo". First, start with your "iris" dataset and then...
  mutate(Lengthx10 = Sepal.Length * 10) %>% # Create a new column (mutate) with sepal length values multiplied by ten and then...
  filter(Species == "virginica") # Pull out (filter) entries for the virginica species only. End of pipe.

#### aggregating ####

# Next, we'll be using the CO2 dataset built into R.

# Examine the top few rows of the dataset in the console.
head(CO2)

# Type ?CO2 in the console to get more information about the dataset.
# Information should appear in the "Help" panel in the bottom right of your screen.

# Store this dataset as a variable so it appears in the Environment.
CO2 <- CO2

# Create new dataset grouped by Treatment and conc, calculating the mean uptake based on those groupings, with a new column showing how many samples went into the mean calculation.
CO2_summary <- group_by(CO2, Treatment, conc) %>% 
  summarize(mean_uptake = mean(uptake), group_size = n()) %>%
  ungroup()

# Compare uptake of Quebec and Mississippi plants, calculating the mean uptake and the number of samples in each sample again.
CO2_ex <- group_by(CO2, Type) %>%
  summarize(mean_uptake = mean(uptake), group_size = n()) %>%
  ungroup()

# Exercise: Use the CO2 dataset to see how type/location and ambient CO2 concentration influence median CO2 uptake.
CO2_ex2 <- group_by(CO2, Type, conc) %>% # Group by Type and concentration.
  summarize(median_uptake = median(uptake)) %>% # Calculate the median based on the groupings above.
  filter(Type == "Quebec") %>% # Filter out records for Quebec samples only.
  # Use quotation marks for values. "==" means comparison. "=" refers to an assignment.
  ungroup() # ALWAYS ungroup() when using the function group_by() so that future datasets aren't continually grouped.

# Base statistical functions in R:
# mean = mean()
# maximum = max()
# minimum = min()
# median = median()
# standard deviation = sd()

# End of script.