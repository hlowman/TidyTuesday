# Annie and Jhen's Tidy Tuesday
# Week 5
# September 8, 2020

#### What is tidy data? ####

# Load packages.
library(tidyverse)
library(ggpmisc)

# Data formatting is very important for using the tidyverse - functions within this library were designed specifically with this format of data in mind.

# Tidy data:
# (1) Each variable in its own column.
# (2) Each observation in its own row.
# (3) Each value in its own cell.

# An example of a pre-tidied dataset is "iris" in base R.
View(iris)

# An example of a not-quite tidied dataset is "mtcars" in base R.
mtcars_og <- mtcars
View(mtcars_og) # Notice rownames are car models.

mtcars_tidy <- mtcars_og %>% # Using the original dataset.
  mutate(carmodel = rownames(mtcars_og)) %>% # Creating a new column with rownames from original dataset.
  select(carmodel, mpg, wt, cyl) %>% # Select only the named columns.
  pivot_longer(cols = c("mpg", "wt", "cyl"), names_to = "attribute", values_to = "value") # Pivoting only allows you to pivot similar data formats - numeric in this case. The "carmodel" column is characters, so we will keep that as is.

mtcars_wider <- mtcars_tidy %>% # Using the tidied dataset from above.
  pivot_wider(names_from = "attribute", values_from = "value") # Pivoting back into the format we had previously, going from mtcars_tidy back to a format similar to mtcars_og.

# When you're dealing with issues relating to unique observations in each cell, the unite() and separate() functions can also be very helpful.

# Now, let's use the dataset that was sent out to perform some more tidying.

growth_data_og <- read_csv("growth_rates.csv") # Load in this week's dataset.
# Extracted dataset from a manuscript about duckweed growth in different environmental conditions.
# This is a pre-tidied dataset.

# Additional pivoting practice.
growth_longer <- growth_data_og %>% # Using the new dataset.
  pivot_longer(cols = c("flow.cfs", "depth.cm", "water.temp.C", "turbidity.NTU", "DO.mg.L", "pH", "sp.cond.uS.cm"),
    names_to = "analyte_name", values_to = "value") # Pivot numeric values into the new "value" column and name its corresponding column "analyte_name" and put the column names in as values instead.

# Bottom line: Data types matter.

#### Plot away!! ####

View(growth_data_og) # Let's again examine the dataset loaded in for today's tutorial.

fig1 <- ggplot(growth_data_og, aes(x = pH, y = growth.rate.per.day)) +
  labs(y = "Growth rate per day") +
  geom_point() # straightforward basic plot.

fig1

fig2 <- ggplot(growth_data_og, aes(x = pH, y = growth.rate.per.day, color = Site)) +
  labs(y = "Growth rate per day") +
  geom_point() # straightforward basic plot colored by Site.

fig2

# Running a linear model to examine the relationship in the data.

modelpH <- lm(growth.rate.per.day ~ pH, data = growth_data_og) # Create a linear model and assign it the name "modelpH". The basic structure is y ~ x in the lm() function.

modelpH # Display the model output.

fig3 <- ggplot(growth_data_og, aes(x = pH, y = growth.rate.per.day, color = Site)) +
  labs(y = "Growth rate per day") +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) # plot with linear model added. Remove 95% confidence intervals with the "se = FALSE" call.

fig3

# Now let's add the regression statistics to the plot itself using the stat_poly_eq() from the ggmisc package (loaded above).

myformula <- y ~ x # Create standard formula to use below.

fig4 <- ggplot(growth_data_og, aes(x = pH, y = growth.rate.per.day, color = Site)) +
  labs(y = "Growth rate per day") +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  stat_poly_eq(formula = myformula, 
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    parse = TRUE) # plot with linear models and equations added.

fig4

# Now let's panel out the data by site using facets.

fig5 <- ggplot(growth_data_og, aes(x = pH, y = growth.rate.per.day)) +
  labs(y = "Growth rate per day") +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  stat_poly_eq(formula = myformula, 
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    parse = TRUE) +
  facet_wrap(~Site) # plot with linear models and equations added and facet!

fig5

# Now, let's make some additional plots.

View(growth_longer) # Examine pivoted dataset.

fig6 <- ggplot(growth_longer, aes(x = value, y = growth.rate.per.day, color = Site)) +
  labs(y = "Growth rate per day",
    title = "Growth rates of duckweed in response to environmental conditions") +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  stat_poly_eq(formula = myformula, 
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    parse = TRUE) +
  facet_wrap(~analyte_name, scales = "free") # Scales here are letting each axes (x & y) be whatever range it needs to be.

fig6  

# End of script.