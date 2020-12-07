# Tidy Tuesday - November 24,2020
# Southern California Coastal Water Research Project
# "fun with ggplot" Hacky Hour
# Code contributed by: Annie Holt, Dana Schulz, Jayme Smith, 
# Karen McLaughlin, Wayne Lao, Heili Lowman

# Load necessary packages.
library(palmerpenguins) # for data
library(tidyverse) # for piping/tidying
library(plotly) # for interactive plot creation
library(rphylopic) # for adding organism silhouettes to plots
library(scales) # for modify x and y axes
library(wesanderson) # for color palette
library(gganimate) # for animated plot

# If you don't currently have one of the above packages, 
# you can install them by running the following in your console:
# install.packages("packagenamehere")

# Please Note: Data is loaded differently according to each plot (and its author).

# For more information on the dataset used in the following plots, please visit:
# https://allisonhorst.github.io/palmerpenguins/articles/intro.html

# Scatter Plot ------------------------------------------------------------

# Load data
penguins_data <- penguins %>% 
  #borrow some of the dataset modifications from ggplot tutorial
  rename(flipper = flipper_length_mm, 
    bill_length = bill_length_mm, 
    bill_depth = bill_depth_mm) %>% 
  mutate(mass_kg = body_mass_g/1000) 

# Examine data
View(penguins_data)

# Make a simple scatter plot using normal axis scaling
ratio <- ggplot(penguins_data,aes(x=flipper,y=mass_kg, color = species)) + 
  geom_point() + 
  theme_bw()+ 
  xlab("Flipper Length (mm)") + 
  ylab("Body mass (kg)")

ratio

# Then plot with log10 scale y axis
ratio_log <- ggplot(penguins_data,aes(x=flipper,y=mass_kg, color = species)) + 
  geom_point() + 
  theme_bw()+ 
  scale_y_continuous(trans='log10', name = "Log of body mass (kg)", breaks = c(3, 3.5, 4, 4.5, 5, 5.5)) +
  labs(x ="Flipper Length (mm)") 

ratio_log

# Bar Plots ----------------------------------------------------------------

# Read in our "penguins" dataset
penguins <- palmerpenguins::penguins 

# Make a plot of bill length with bars representing counts and colored by sex
bill_length <- ggplot(penguins, aes(bill_length_mm)) + 
  geom_bar(aes(fill = sex)) +
  labs(x = "Bill Length (mm)",
    y = "Penguin Count",
    fill = "Sex") +
  theme_classic()

bill_length

# Make a graph of body mass by species and sex
penguins2 <- penguins %>% 
  select(species, sex, body_mass_g) %>% 
  na.omit()

# Mean of body mass by species and sex for bar graph
penguin_mean <- penguins2 %>% 
  group_by(species, sex) %>% 
  summarize(BMmean = mean(body_mass_g))

# Create bar plot with mean values as points
bodymass <- ggplot() +
  geom_bar(data = penguin_mean, 
    aes(x = species, y =BMmean, color = sex, fill = sex), 
    alpha = 0.5, 
    stat = "identity",
    position = "dodge2") +
  geom_point(data = penguins2, 
    aes(x = species, y = body_mass_g, color = sex),
    stat = "identity", 
    position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("red", "blue")) +
  scale_color_manual(values = c("red", "blue")) +
  labs(y = "Body Mass (g)", 
    x = "Species") +
  theme_bw()

# Examine plot
bodymass

# Violin Plot -------------------------------------------------------------

# load data
penguins <- penguins

# body mass distribution plot, by sex and island
penguin_violin <- ggplot(data = penguins,
  mapping = aes(x = sex, y = body_mass_g, color = sex))+
  geom_violin(aes(fill = sex), alpha = 0.1)+
  geom_jitter(width = 0.2, show.legend = FALSE, size = 2, alpha = 0.8)+
  facet_grid(~island)+
  scale_x_discrete(limits = c("female", "male"))+
  scale_fill_brewer(palette = "Accent")+
  scale_color_brewer(palette = "Accent")+
  xlab("Sex")+
  ylab("Body Mass (g)")+
  theme_bw()+
  theme(legend.position = "none")

# examine plot
penguin_violin

# Plotly (interactive) ----------------------------------------------------

# Find a penguin image by taking end of phylopic.org url
# http://phylopic.org/assets/images/submissions/21c50828-58d8-42df-9219-2c1b0fb57c99.512.png
img <- image_data("21c50828-58d8-42df-9219-2c1b0fb57c99", size = "64")[[1]]

# Save dataset as variable penguins
penguins <- penguins

# Take a look at the data
view(penguins)

# Goal 1: Boxplot of body mass for each type of penguin
p1 <- ggplot(data = penguins, aes(x = species, y = body_mass_g,
  color = species))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, width = 0.05)+
  labs(y = "Body Mass (g)", x= "Species of Penguin",
    caption = "Data Source: Gorman et al., 2014\nImage Source: Steven Traver") +
  scale_color_brewer(palette = "Dark2")+
  theme(legend.title = element_blank())+
  add_phylopic(img, x = 0.75, y = 5900, ysize = 650, alpha = 0.5)

# View plot
p1

# Create an interactive plotly plot from p1
# Use this plot to hover over specific points to see details,
# Get an overview of boxplot stats,
# Select/deselect data from the legend
ggplotly(p1)

# Goal 2: add the ability to filter by year
# Couldn't figure this one out for a boxplot, but maybe for points alone?
# Create a new column where year is a factor
penguins <- penguins %>%
  mutate(year_f = factor(year, levels = c(2007, 2008, 2009)))

# Name this plot p2
p2<-ggplot(data = penguins, aes(x = species, y = body_mass_g,
  color = species, shape = year_f))+
  #geom_boxplot()+
  geom_jitter(alpha = 0.4, width = 0.05)+
  labs(y = "Body Mass (g)", x= "Species of Penguin",
    caption = "Data Source: Gorman et al., 2014") +
  scale_color_brewer(palette = "Dark2")

# View plot
p2

# Create interactive plotly plot
ggplotly(p2)


# Animated Plot -----------------------------------------------------------

# Load dataset.

penguins <- palmerpenguins::penguins

# Create simple ggplot for sex and body mass

p <- ggplot(penguins, aes(x = sex, y = body_mass_g)) +
  geom_point()

# Animated plot of sex and body mass - changing by year

anim <- p +
  transition_states(year,
    transition_length = 2, 
    state_length = 1) + ggtitle('Now showing {closest_state}',
      subtitle = 'Frame {frame} of {nframes}')

anim

#Helpful link for using gganimate: 
# https://gganimate.com/articles/gganimate.html

# Histogram ---------------------------------------------------------------

# Take the original dataset.
penguin_data <- penguins %>% 
  # Create a new column where we assign "missing" to NAs.
  mutate(sex_f1 = fct_explicit_na(factor(sex), na_level="missing")) %>% 
  # And create a new column with sex as a factor that has appropriate names for the legend.
  mutate(sex_f = factor(case_when(
    sex_f1 == "female" ~ "Female",
    sex_f1 == "male" ~ "Male",
    sex_f1 == "missing" ~ "Not Recorded")))

# Create histogram figure.
# Load in data and specify x axis.
penguin_hist <- ggplot(penguin_data, aes(x = body_mass_g)) + 
  # Specify to create histogram and color by sex column created above.
  # Alpha makes the bars somewhat transparent.
  # The "position = "identity" call adds the ability to see through from one layer to another.
  geom_histogram(aes(fill = sex_f), alpha = 0.75, position = "identity") + 
  # Assign the Grand Budapest Hotel color palette from the wesanderson package.
  # The "name = XX" call allows you to specify the legend title.
  scale_fill_manual(values = wes_palette("GrandBudapest1"),
    name = "Sex") +
  # Set x axis breaks to display so that the numbers don't overlap across panels.
  scale_x_continuous(breaks = c(3000, 4000, 5000, 6000)) +
  # Add x axis, y axis, title, and subtitle labels.
  labs(x = "Penguin Body Mass (g)",
    y = "Count of Observations",
    title = "Antarctic Penguin Body Mass by Sex and Species",
    subtitle = "Data Sources: Gorman et al., 2014; Horst et al., 2020") +
  # Facet by penguin species.
  facet_wrap(~species) +
  # Remove the interior grey background of the plot.
  theme_classic() +
  # Move the legend above the plot.
  theme(legend.position = 'top')

# Call the new plot.
penguin_hist

# Export most recently called plot.
# ggsave("penguin_histogram.png",
#      path = "/Users/heilil/Desktop/R_figures",
#      width = 25,
#      height = 15,
#      units = "cm"
#    )

# End of script.
