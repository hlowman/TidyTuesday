# Tidy Tuesday: Data Wrangling
# Name: Leah Thornton Hampton
# Date: 2/9/21

#Goal #1 - Plot the number of particles found in each spiked sample
#Goal #2 - Blank correct the data and plot

#### Load Packages ####

library(tidyverse)

#### Import Data ####

Raw_Data <- read_csv("microplastics_data.csv", guess_max = 100000)

#Before you start - know your data set! 

#### Select Data #### 

#Select only the columns that you want to work with for now so that your data is easier to work with 

#By Location
  #Be careful that you update this if column location is ever modified! 

Data_Select <- Raw_Data %>% 
  select(2:3, 5:8, 11, 18:20)

#By Name
Data_Select <- Raw_Data %>% 
  select(labid, sampleid, particleid, sizefraction, color, morphology, polymerid, microscopy, nilered, chemicalid_method)

#You can also remove columns that you don't want rather than listing the ones that you do want

#By Name
Data_Select <- Raw_Data %>%
  select(-c(objectid, sampletype, photoid, chemicalid, plastic, comments, comments, length, width, timeimagesmeasurements, instrumenttype)) %>% 
  
  #We have a mix of spiked and blank samples. All blank samples have the number 4 that we can use to mark them as blanks 
  mutate(blank = ifelse(grepl("4|8", sampleid),"Y", "N"))
  
  #grep() and similar functions are used to find and manipulate character strings more info here: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/grep

#### Plot the Number of Particles in Each Sample ####

#Each particle has a unique id that we can take advantage of to make sure that we do not "double count"

Count <- Data_Select %>%
  #Group the data by sample
  group_by(sampleid) %>%
  #Count the number of unique particles
  mutate(total_particles = n_distinct(particleid)) %>%
  #Be sure to ungroup
  ungroup()

#Make a plot for total particle count in each spiked sample

Count_Plot <- Count %>% 
  #Drop blank samples
  filter(blank != "Y") %>% 
  ggplot(aes(x = labid, y = total_particles)) +
  geom_point()

plot(Count_Plot)  

#### Blank Correct the Data ####

#Make a separate data frame containing only blank data 

Count_Blank <- Count %>% 
  filter(blank == "Y") %>% 
  group_by(labid, total_particles) %>% 
  summarise() %>% 
  rename("total_particles_blank" = total_particles) %>%
  ungroup()

#Filter results to include spike samples only 

Count_Spiked <- Count %>% 
  filter(blank == "N")

#Join data frames back together

#https://dplyr.tidyverse.org/reference/join.html

Count_Joined <- left_join(Count_Spiked, Count_Blank, by = c("labid" = "labid"))

Count_Joined <- Count_Joined %>% 
  mutate(total_particles_corrected = (total_particles - total_particles_blank)) %>% 
  mutate(total_particles_corrected = ifelse(total_particles_corrected < 0, 0, total_particles_corrected))

#Plot Blank Corrected Values

Count_Plot_BlankCorr <- Count_Joined %>% 
  ggplot(aes(x = labid, y = total_particles_corrected)) +
  geom_point()

plot(Count_Plot_BlankCorr)

#Set any negative values to 0 

#### Add a Reference Line from Another Data Set ####

#Import reference data

Spike <- read_csv("microplastics_reference_data.csv")

#Calculate average number of particles spiked into each sample 

Spike <- Spike %>% 
  #Count the number of particles spiked into each sample
  group_by(sampleid) %>% 
  mutate(total_particle_spike = sum(averagenumber)) %>% 
  #Don't forget to ungroup!
  ungroup() %>% 
  mutate(average_total_spike = mean(total_particle_spike))

Count_Plot_BlankCorr <- Count_Joined %>% 
  ggplot(aes(x = labid, y = total_particles_corrected)) +
  geom_point()+
  #Add horizontal line that pulls data from a separate data frame - for some functions data must be same length
  geom_hline(data = Spike, aes(yintercept = average_total_spike), linetype = "dotted")

plot(Count_Plot_BlankCorr)

#### Random things that might be helpful! ####

#Any sphere originally designated as grey renamed to be white
# mutate(color = case_when(morphology == "Sphere" & color == "Grey" ~ "White", TRUE ~ color)) 

#Add mean lines to your points
# stat_summary(fun="mean", geom="segment", mapping=aes(xend=..x.. - 0.25, yend=..y..))+
# stat_summary(fun="mean", geom="segment", mapping=aes(xend=..x.. + 0.25, yend=..y..))+

