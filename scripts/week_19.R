# Tidy Tuesday Tutorial: Heatmaps
# December 15, 2020
# By Ashley Parks


# Load packages
library(RColorBrewer)
library(pheatmap)

# Upload data file
sample_size <- read.csv("heatmap_data.csv")
View(sample_size)

# Make column 1 equal to the row names and remove column 1
All_data <- data.frame(sample_size, row.names = 1)
View(All_data)

# Turn data into matrix format
All_matrix <- data.matrix(All_data)

# Plot data to heatmap
All_heatmap <- heatmap(All_matrix, # Default R heatmap function
                    main = "Sample sizes by watershed",   # Add title and custom colors with 9 colors in range
                    col = brewer.pal(n=9, name = "PuBu"))    

# Plot data to a pretty heatmap (pheatmap function)
Pretty_heatmap <- pheatmap(All_matrix,  # basic pheatmap plot
                           main = "Sample sizes by watershed",  # Add main title
                           fontsize = 12,                       # Change main font size for entire plot
                           cluster_row = FALSE, cluster_cols = FALSE,  # Removes clustering and dendrograms
                           display_numbers =TRUE,                      # Adds raw data values to heatmap
                           number_format = "%.0f", fontsize_number = 10)  # Format raw data values (i.e., decimal places, fontsize, etc.)

# Plot heatmap with specific color breaks
breaks = c(0, 50, 100, 500, 1000, 2000, 4000) # Create breaks in your data to coincide with colors

color = brewer.pal(n=7, name = "GnBu")   # Choose color package and number of colors in heatmap scale

Pretty_heatmap <- pheatmap(All_matrix,
                           main = "Sample sizes by watershed",
                           fontsize = 12,
                           cluster_row = FALSE, cluster_cols = FALSE,
                           display_numbers =TRUE, 
                           number_format = "%.0f", fontsize_number = 10,
                           breaks = breaks, # Add in custom breaks and colors
                           color = color)

# How to save your heatmap (indicate filename first, then the code for the image, then end the process with dev.off)
jpeg(filename = "All_watershed.jpeg", # You can use other image formats as well
     height = 4, width = 7, units = "in",
     res = 300)

Pretty_heatmap <- pheatmap(All_matrix,
                           main = "Sample sizes by watershed",
                           fontsize = 12,
                           cluster_row = FALSE, cluster_cols = FALSE,
                           display_numbers =TRUE, 
                           number_format = "%.0f", fontsize_number = 10,
                           breaks = breaks,
                           color = color)
dev.off() # Very important to include this, otherwise the file won't be created and will show up blank or as a partial image


# Resources
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/heatmap
# https://www.r-graph-gallery.com/215-the-heatmap-function.html
# https://www.rdocumentation.org/packages/pheatmap/versions/1.0.12/topics/pheatmap
# https://statistics.berkeley.edu/computing/saving-plots-r
# https://www.datanovia.com/en/lessons/heatmap-in-r-static-and-interactive-visualization/


# End of script
