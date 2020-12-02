# SCCWRP Tidy Tuesday: "gt" package Tutorial
# December 1, 2020
# Heili Lowman

# Load packages
library(tidyverse) # for data tidying
library(lubridate) # for date wrangling
library(gt) # for table creation
library(paletteer) # for access to thousands of color palettes

# Get more info (will appear in "Help" pane)
?pizzaplace

# Load and examine data
pizza <- gt::pizzaplace
View(pizza)

# Today, we are interested in making a table of pizza sales by month and by type of pizza.

# (1) Make a new column for month.

pizza <- pizza %>% # Take the original dataset
  mutate(date_format = as_date(date)) %>% # Create newly formatted date column
  mutate(month_num = month(date_format)) # Create new month column

# (2) Create a summary stats table.

pizza_money <- pizza %>% # Take the original dataset
  group_by(month_num, type) %>% # Group by month and pizza type
  mutate(type = factor(type,
    levels = c("classic", "veggie", "chicken", "supreme"))) %>% # Relevel pizza types
  summarize(sold = n(), profit = sum(price)) %>% # Summarize profits by defined groups
  ungroup() # Always make sure to ungroup after you group!! Otherwise these will carry over to your future figures/analyses and can cause invisible headaches.

# (3) Build our table.

pizza_table <- pizza_money %>%
  
  # Make some edits to underlying dataset.
  filter(month_num > 9) %>% # Filter for Q4 sales only (post-September)
  
  mutate(month_name = case_when(month_num == 10 ~ "October",
    month_num == 11 ~ "November",
    month_num == 12 ~ "December")) %>% # Create new column of month names rather than numbers
  
  gt(groupname_col = "month_name", rowname_col = "type") %>% # Base table creation/call
  # Also, assigning data to be grouped by month and rows to be by pizza type
  
  # Format the table layout
  cols_hide(columns = vars(month_num)) %>% # Hide column
  fmt_currency(columns = vars(profit), currency = "USD") %>% # Format profit into dollars
  
  # Add helpful (sub)titles
  tab_header(title = "Q4 Monthly Pizza Sales",
    subtitle = "by Heili Lowman") %>% # Adds title & subtitle
  tab_source_note(md("More information can be found at `?pizzaplace`.")) %>% # Adds a nice footer.

  # Add helpful labels
  cols_label(sold = "Pizzas Sold",
    profit = "Profits") %>% # Adds column names
  tab_stubhead(label = "Month") %>% # Renames stubhead
  tab_spanner(label = "Pizzas Sold + Revenue",
    columns = vars(sold, profit)) %>% # Adds spanner
  
  # Add more summary statistics
  summary_rows(groups = TRUE,
    columns = vars(profit),
    fns = list("TOTAL" = "sum"),
    formatter = fmt_currency, currency = "USD",
    use_seps = TRUE) %>% # Monthly summary rows
  grand_summary_rows(
    columns = vars(profit),
    fns = list("GRAND TOTAL" = "sum"),
    formatter = fmt_currency, currency = "USD",
    use_seps = TRUE) %>% # Grand summary row
  
  # Add some color
  data_color(
    columns = vars(profit), # Specifying which column to color in
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::teal_material") %>% # Specify which color palette to use
        as.character(),
      domain = NULL),
      alpha = 0.75) # Makes cell colors slightly transparent

pizza_table

# (4) Save our table.
gtsave(pizza_table,
  "pizza_table.png",
  path = "/Users/heilil/Desktop/R_figures") # Insert your filepath here!

# End of script.
