#Code and documentation authored by Paul Pop.

#This research was carried out under the BIRD lab, ATREE, Bengaluru (PI: Rajkamal Goswami).

#Version 1.0 - last updated 2026-04-07
#Last update - Introduction

#View the most current version at https://github.com/paulvpop/bird_point_count_analysis_and_reporting/blob/main/R_script_bird_point_analysis_and_reporting.R

# This is an R script for creating a species accumulation curve for your study area plotted on 
# the same plot as richness (no. of species) and abundance (no. of individuals of each 
# species).

# This script assumes that you collect the information using eBird (either uploaded immediately after
# the surveys when there is connectivity and device charge, or uploaded when back in the field station or
# area with connectivity). When creating checklists, note down the weather or site covariates that you need
# for any analysis in the comment section using a consistent delimiter like semi-colon. For example,
# Moderate breeze; clear blue skies; slightly warm; quite sunny. Since there are most likely going to be 
# checklists prepared outside of the specific point count duration or point count sites also, but within the 
# entire study duration in the broader study area, create an eBird account specifically for the point counts 
# (usually using the lab/institutional email). You should create the checklists using your own account but 
# share only the checklists within the point sites and point count duration with the point count eBird account. 
# In addition, even during the point counts, note down the species heard or seen outside the point count radius 
# in your personal eBird checklist. Make notes on how many species were heard/seen outside or inside the plot so 
# the ones outside the radius can be removed from the checklist in the point count eBird account to have a cleaned 
# data record for analysis. This two-account procedure ensure that you can contribute useful complete eBird checklists
# to broader publicly-available scientific data, helping aid participatory science. This also ensures that you have 
# a cleaned dataset for your specific single-season point count analysis. 

# Download the eBird data from both your personal and the point count data profile.
# See the procedure for downloading eBird data in my documentation here: 
# https://github.com/paulvpop/supporting-code-for-A-large-scale-crowd-sourced-acoustic-dataset-of-Indian-fauna/blob/main/annotations_to_final_submission_file.md#download-your-ebird-data

# Before running this script, make sure that you have a cleaned versions of the eBird data from
# only the point count surveys as well as including everything (from personal account). This includes thorough check for 
# correctness in identification of the species. The following steps can be used to do this.

# Set the working directory to wherever you like (ideally something related to the code):
setwd("D:/Paul_Pop_Birds/Paul_Pop_Bagepalli")

# Check the current working to see if the changes have been made (from where files will be loaded to 
# and saved by default)
getwd()

# List the files in the working directory to see if data needed to be loaded is there. 
list.files()

# and copy-paste the filtered and cleaned eBird data file for only the point counts (renamed 
# "within_plot_data_winter_2026.csv" from "MyEBirdData.csv" in this case) 
# to the below argument:
pc_data <- read.csv( "within_plot_data_winter_2026.csv")

# Load the library dplyr needed for data wrangling
library(dplyr)

# View a sample of the data (OPTIONAL):
head(pc_data)
#OR
dplyr::glimpse(pc_data)

# Check the class of the data (OPTIONAL)
class(pc_data)

# Load the library dplyr needed for data wrangling
library(dplyr)

# If the pc_data contains data for only one season, then further processing is not necessary. However,
# if it contains data from multiple seasons/years, only the data for the current season (winter 2026
# in this example) should be retained. Filter out the rest.

# Convert to date format and filter to the selected data range in one step:
pc_data <- pc_data %>%
            mutate(Date = as.Date(Date)) %>%
            filter(Date >= "2026-01-19", Date <= "2026-01-24")

# Similarly, load in the complete data from the person account which contains data from outside the 
# point count surveys but within the study area and duration:
all_data <- read.csv( "all_data_winter_2026.csv")

# Filter it to the required date range:
all_data <- all_data %>%
            mutate(Date = as.Date(Date)) %>%
            filter(Date >= "2026-01-19", Date <= "2026-01-24")

# Count the unique number of species
length(unique(filtered_data$Scientific.Name))
[1] 109

#See the unique species:
unique(filtered_data$Scientific.Name)

# If any of them are not at the species level, and there are others from the same
# genus at the species level, remove them from the total count in the previous step.
# In this case, "Aves sp." is not at the species level. So, total count of species observed
# during the study duration was 108.

# Convert the data into a usable form

# Create another table with only the location, common name and count of the individual birds.
pc_data2 <- pc_data %>% 
            group_by(Location, Common.Name) %>% 
            summarize(Count = mean(Count))

# Check the last 15 rows of the this table
tail(pc_data2, 15)
# # A tibble: 15 × 3
# # Groups:   Location [2]
# Location Common.Name           Count
# <chr>    <chr>                 <dbl>
#  1 G8-2     Purple-rumped Sunbird   1  
#  2 G8-2     Red-vented Bulbul       2.5
#  3 G8-2     Ring-necked Parakeet    8  
#  4 G8-2     River Tern              1  
#  5 G8-2     Sykes's Warbler         1  
#  6 G8-2     White-browed Bulbul     2  
#  7 G8-2     Yellow-billed Babbler   1  
#  8 G9-1     Asian Green Bee-eater   1  
#  9 G9-1     Common Myna             3  
# 10 G9-1     Grey Wagtail            1  
# 11 G9-1     Indian Robin            1  
# 12 G9-1     Lesser Whitethroat      2  
# 13 G9-1     Purple Sunbird          1.5
# 14 G9-1     Red-vented Bulbul       2  
# 15 G9-1     Sykes's Warbler         1   

# The Count column needs to be rounded up
pc_data2$Count <- ceiling(pc_data2$Count) 

# Convert it to a matrix:

# For that, load the package "tidyr":
library(tidyr)

# Pivot the table:
pc_data2 <- pivot_wider(pc_data2, names_from = Location, values_from = Count)

# Convert all the NAs to zeroes:
pc_data2[is.na(pc_data2)] <- 0

# Convert to a dataframe:
pc_data2 <- as.data.frame(pc_data2)

# Check the structure (OPTIONAL)
str(pc_data2)

# Remove the row with 'bird sp.' (if you have a 'bird sp.' in your data). 
# In a similar way, you can remove the species identified to only the genus level if you want to.
pc_data2 <- pc_data2 %>% filter(Common.Name != "bird sp.")

# If you have any location names that need renaming, you can do the following. 
# In this case, the column "G20-1 Latest" is being renamed to "G20-1".
pc_data2 <- rename(pc_data2, "G20-1" = "G20-1 Latest")

# The location names can be reordered based on the numbers.
# In this case, we will reorder gomala numbers

# For this, get all column names except "Common.Name"
col_names <- names(pc_data2)[-1]

# Function to extract numeric parts for sorting
extract_numbers <- function(x) {
  parts <- strsplit(sub("G", "", x), "-")[[1]]  # Removes the letter G (substitutes it with nothing),
  # and then splits the remaining terms into two and retains it as a list.For example, "20-2" → list(c("20", "2"))
  as.numeric(parts[1]) * 100 + as.numeric(parts[2]) # parts[1] is the first element (i.e. "20")
  # as.numeric(parts[1]) converts it to the number 12
  # parts[2] is the second element (i.e., "2")
  # as.numeric(parts[2]) converts it to number 2
  # Then it performs: 20 * 100 + 2 = 2002 (multiplied by 100 to prioritise the first number)            
}

# Sort column names numerically
sorted_cols <- col_names[order(sapply(col_names, extract_numbers))]

# You will get a warning message like below if you have any location number different
# from the usual alphanumeric pattern with a space-less hyphen in between
# Warning message:
#   In FUN(X[[i]], ...) : NAs introduced by coercion

# Fix it before you proceed.

# Reorder the dataframe columns
pc_data2 <- pc_data2[, c("Common.Name", sorted_cols)]

# Verify the new order
names(pc_data2)

# Install a package necessary for the species accumulation curve
# install.packages("fossil")
library("fossil")

# spp.est is the R equivalent of the software EstimateS. It is found within the package "fossil".
# Reference for the spp.est function 
# https://palaeo-electronica.org/2011_1/238/estimate.htm#:~:text=The%20spp.,of%20randomizations%20should%20be%20run

# Remove the species name column (the first column)
pc_data3 <- pc_data2[,-1]

# Use the spp.est function
est <- as.data.frame(spp.est(pc_data3, rand = 999, abund = TRUE, counter = FALSE))
# If you see it, ignore the warnings which reads "This data appears to be presence/absence based, 
# but this estimator is for abundance data only". It's likely misreading abundance data as 
# "presence/absence" due to the high number of 0 and 1 (if your data has a lot of species with 
# 0 and 1s.

# Plotting SAC with 95% CI

# Have a look at the estimates.
glimpse(est)

# Rows: 31
# Columns: 13
# $ N.obs          <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 2…
# $ S.obs          <dbl> 10.76076, 18.78779, 24.79680, 29.68669, 33.69970, 37.41241, 40.10911, 43.07708, 45.28328, 47.74575, 49.4…
# $ `S.obs(+95%)`  <dbl> 18.73942, 27.95522, 34.09281, 39.03291, 42.56419, 46.20413, 48.95082, 51.82819, 53.68379, 56.20120, 57.8…
# $ `S.obs(-95%)`  <dbl> 2.782101, 9.620357, 15.500787, 20.340464, 24.835207, 28.620698, 31.267400, 34.325960, 36.882775, 39.2902…
# $ Chao1          <dbl> 25.89281, 37.16486, 43.89432, 47.31142, 50.90992, 54.50970, 57.27784, 60.40768, 62.30072, 64.48579, 65.9…
# $ `Chao1(upper)` <dbl> 31.69348, 44.35052, 50.91360, 53.49442, 56.73971, 60.17010, 62.96200, 66.06398, 67.88345, 69.87248, 71.3…
# $ `Chao1(lower)` <dbl> 20.09214, 29.97921, 36.87504, 41.12842, 45.08013, 48.84929, 51.59367, 54.75137, 56.71799, 59.09911, 60.6…
# $ ACE            <dbl> 35.32108, 32.00151, 34.43157, 37.88226, 41.64959, 45.02603, 47.29106, 50.03867, 51.87322, 54.08598, 55.6…
# $ `ACE(upper)`   <dbl> 105.57857, 63.23891, 50.04772, 50.81732, 53.43591, 56.64647, 59.01748, 61.46310, 62.55311, 64.83100, 66.…
# $ `ACE(lower)`   <dbl> -34.9364012, 0.7641114, 18.8154298, 24.9471956, 29.8632818, 33.4055873, 35.5646357, 38.6142405, 41.19332…
# $ Jack1          <dbl> 17.53835, 29.19405, 37.33370, 43.28415, 48.19875, 52.52365, 55.43996, 58.88763, 61.24350, 63.88442, 65.7…
# $ `Jack1(upper)` <dbl> 30.91454, 43.46089, 51.42452, 57.56340, 61.72669, 66.24275, 69.27235, 72.54485, 74.35457, 77.18826, 78.8…
# $ `Jack1(lower)` <dbl> 4.162147, 14.927207, 23.242874, 29.004899, 34.670821, 38.804546, 41.607560, 45.230399, 48.132421, 50.580…

# Get the meta-data (no. of species and no. of individuals per site)
# Look at structure of the data first (OPTIONAL)
View(pc_data2)

# Calculate total individuals per site (column sums) where pc_data3 is the site data
individuals_per_site <- colSums(pc_data3, na.rm = TRUE)

# Calculate number of species per site (count non-zero entries)
species_per_site <- apply(pc_data3, 2, function(x) sum(x > 0, na.rm = TRUE))

# Combine into a new dataframe
meta <- data.frame(
  site = names(pc_data3),
  abundance = individuals_per_site,
  richness = species_per_site,
  row.names = NULL
)

# View the result
print(meta)
# site abundance richness
# 1   G3-1        13        8
# 2   G3-2        34       19
# 3   G3-3        21       17
# 4   G3-4        72        8
# 5   G4-1        49       13
# 6   G4-2        73        8
# 7   G5-1        40       19
# 8   G6-1        19       10
# 9   G6-2        26       12
# 10  G7-1        30       15
# 11  G7-2        23       13
# 12  G8-1        23       14
# 13  G8-2        40       21
# 14  G9-1        13        8
# 15 G10-1        13        7
# 16 G10-2        16        8
# 17 G11-1        33       16
# 18 G11-2        15       10
# 19 G12-1        20       15
# 20 G13-1        15        8
# 21 G14-1        14       10
# 22 G15-1        38       21
# 23 G15-2        27       10
# 24 G15-3       167       22
# 25 G20-1        19       13
# 26 G20-4       123       25
# 27 G20-7        35       22

#Combine "est" and "meta"
metest <- cbind(est, meta)
head(metest)

# Plot the species accumulation curve
                          
# Load in the "ggplot2" package for plotting the species accumulation curve with the site-wise 
# abundance and species counts
library(ggplot2)

SAC <- ggplot(metest, aes(x = factor(site, levels = unique(site)), y = S.obs)) +
  # Bars for species count (black)
  geom_bar(aes(y = richness, fill = "No. of species"), 
           stat = "identity", 
           width = 0.4, 
           position = position_nudge(x = -0.2)) +
  # Bars for individual count (red)
  geom_bar(aes(y = abundance, fill = "No. of individuals"), 
           stat = "identity", 
           width = 0.4, 
           position = position_nudge(x = 0.2)) +
  # Confidence intervals (ribbon)
  geom_ribbon(aes(ymin = est[,4], ymax = est[,3], group = 1),
              alpha = 0.5, fill = "gray") +
  # Observed richness line (connected across sites)
  geom_line(aes(group = 1)) +
  # Customize colors and legend
  scale_fill_manual(values = c("No. of species" = "black", "No. of individuals" = "red")) +
  labs(x = "Site", y = "") +
  # Adjust y-axis breaks
  scale_y_continuous(breaks = seq(0, 168, 6)) +  #Adjust the second value based on the 
  #maximum number of individuals in a site (abundance) i.e. 167 in G15-3, but round up
  # to a multiple of 4
  # Theme adjustments
  theme(
    legend.title = element_blank(),
    axis.text = element_text(colour = "black", size = rel(1)),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate site labels for readability
    axis.title.y = element_text(size = rel(1), angle = 90),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_line(color = "white")
  )

print(SAC)

# Save in a specified width and height format with publication ready 300 dots per
# inch resolution
ggsave("SAC_winter_2026.png", width = 12, height = 7, dpi = 300)

# Print out the table of species and their numbers in each site.
# We will have to split the data since there are too many sites to represented
# in an A4 size sheet.

# Check the structure of pc_data2 (OPTIONAL).
str(pc_data2)

# Rename the "Common.Name" column to "Species":
pc_data2 <- pc_data2 %>% rename('Species' = Common.Name)

# Select only the required columns (splitting so that the tables don't look cluttered)
# The first column contains the species names. So, that's selected for both the objects:
data1 <- pc_data2[c(1:17)]
data2 <- pc_data2[c(1,18:28)]

# Load in a a package necessary for converting the data1 and data2 to flextable and then docx format:
library(flextable)

# Set flextable defaults to change size

# Find the current defaults first                          
get_flextable_defaults()

# Change the font size
set_flextable_defaults(font.size = 8)

# data1
# Convert to data1 into a flextable
ft <- data1 %>%
  flextable() %>%
  # Merge the "Gomala Number" header
  add_header_row(
    values = c("", "Gomala Number"),
    colwidths = c(1, ncol(data1) - 1)
  ) %>%
  # Formatting
  theme_box() %>%
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body") %>%
  autofit()

# Print the table
ft

# Add the width of the columns based on the width of the number and names of sites
doc1 <-width(ft, j=2:15, width = 0.32)
doc1 <-width(doc1, j=16:17, width = 0.38)
doc1 <-width(doc1, j=1, width = 2) # Larger width for site names

# We need in landscape orientation since there are so many sites
# Load the "officer" package for this purpose
library(officer)

# Edit the section properties
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape", width = 8.3, height = 11.7),
  type = "continuous", # the table continues in the next pages (with each page having the header row)
  page_margins = page_mar()
)   

# Highlight numbers greater than 2 (in a light shade of yellow)
for (col in 2:ncol(data1)) {
  doc1 <- bg(doc1,
           j = col,
           i = which(data1[[col]] > 2),  # Explicitly find rows > 2
           bg = "#FFEE99", # shade of yellow
           part = "body")
}

# Save this table as a docx file
save_as_docx(doc1, path = "Gomala_Species_Counts1_winter_2026.docx", align = "center", 
             pr_section = sect_properties)

# You can also save this as an html file which can be viewed in any browser
save_as_html(doc1, path = "Gomala_Species_Counts1_winter_2026.html")

#data2
# Convert data2 to flextable
ft <- data2 %>%
  flextable() %>%
  # Merge the "Gomala Number" header
  add_header_row(
    values = c("", "Gomala Number"),
    colwidths = c(1, ncol(data2) - 1)
  ) %>%
  # Formatting
  theme_box() %>%
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body") %>%
  autofit()

# Print the table
ft

# Adjust the width of the columns
doc2 <-width(ft, j=2:12, width = 0.39)
doc2 <-width(doc2, j=1, width = 2)

# Highlight numbers greater than 2 (in a light shade of yellow)
for (col in 2:ncol(data2)) {
  doc2 <- bg(doc2,
             j = col,
             i = which(data2[[col]] > 2),  # Explicitly find rows > 2
             bg = "#FFEE99", # light shade of yellow
             part = "body")
}

# Save the table in a docx format
save_as_docx(doc2, path = "Gomala_Species_Counts2_winter_2026.docx", align = "center", 
             pr_section = sect_properties)

# Save as html
save_as_html(doc2, path = "Gomala_Species_Counts2_winter_2026.html")

# Note that you can even save the entire pc_data2 as in the html format instead of splitting as it 
# will be easy to view such large tables in browser.

# Find out the proportion of sites occupied by the species and the total % of abundance
# Total abundance = all the number of individuals (averaged across the two temporal
# replicates)
                          
n <- 27 #where 27 = no. of sites (edit it according to how many sites you have)
percent_occ <- round((rowSums(pc_data3 > 0)/n), 4)*100 # where it is rounded to 2 decimals
print(percent_occ)
# [1] 48.15 40.74 11.11 88.89 55.56 33.33 40.74 44.44 62.96 29.63  7.41 59.26 74.07 51.85 22.22  3.70 66.67 18.52  7.41  7.41 40.74
# [22] 14.81 11.11 18.52  7.41  3.70 22.22  7.41  3.70  7.41 14.81  7.41 11.11 11.11 18.52 18.52  7.41 29.63  3.70 18.52 22.22 11.11
# [43] 18.52  3.70  3.70  7.41 11.11  3.70  3.70 11.11 29.63  3.70  3.70  3.70 14.81  3.70 11.11  7.41 11.11 11.11  7.41  3.70  3.70
# [64]  7.41  3.70  3.70  3.70  3.70  3.70  3.70 11.11  7.41  3.70  3.70  7.41  7.41  3.70  3.70  3.70  3.70  3.70  3.70  3.70  3.70
# [85]  3.70  3.70  3.70  3.70
                          
total_abun <- sum(meta$abundance)
# See the total number of individual birds seen within the entire bird survey duration
print(total_abun) 
# [1] 1011
# See the combined number of individual birds of each species seen within all the sites
rowSums(pc_data3)
#  [1]  30  17   5  39  38  14  28  14  36  12   2  24  35  20  12   1 197   8   2   3  15  17   3   7   2   3  26   7   2   5   5
# [32]   2  16   3   5   5   2  10   1   5  72   3  26   1   2   2   4   1   2   8  14   1   1   1   5   1  34   2   3   4   2   1
# [63]   9   2  10   2   1   1   1   1   3   2   1   2  80   4   1   1   1  12   1   1   2   1   1   1   1   1

# Calculate the percentage abundance of each species                         
percent_abun <- round((rowSums(pc_data3)/total_abun)*100,2)

# Create a dataframe containing the percentage occurrence and percentage abundance for each species
proportions <- as.data.frame(cbind(pc_data2$Species, percent_occ, percent_abun))

# Check the first 6 observations of this datafrae
head(proportions)
#                      V1 percent_occ percent_abun
# 1 Asian Green Bee-eater       48.15         2.97
# 2        Booted Warbler       40.74         1.68
# 3          Feral Pigeon       11.11         0.49
# 4        Purple Sunbird       88.89         3.86
# 5  Ring-necked Parakeet       55.56         3.76
# 6       Sykes's Warbler       33.33         1.38

# Rename the colummn headers
proportions <- proportions %>% rename(Species = V1,
                       "% occurrence across point counts" = percent_occ,
                       "% of total abundance" = percent_abun)

# Create a flextable with 3 repeating columns
ft <- proportions %>%
  flextable() %>%
  # Format numeric columns
  colformat_num(j = 2:3, digits = 2, suffix = "%") %>%
  # Theme and alignment
  theme_alafoli() %>%
  align(align = "center", part = "all") %>%
  # Font size adjustments
  fontsize(size = 8, part = "all") %>%
  # Column widths
  width(width = c(2.5, 1.2, 1.2)) %>%  # in inches
  # Header formatting
  bold(part = "header") %>%
  set_header_labels(
    `% occurrence across point counts` = "% Occurrence",
    `% of total abundance` = "% Abundance"
  )

# Split into multiple columns per page
ft <- ft %>%
  # 3 columns per "page" (actually 3 text columns)
  autofit() %>%
  hrule(rule = "exact") %>%
  # Reduce padding between cells
  padding(padding = 1, part = "all")

# Landscape orientation with narrow margins
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape", width = 14, height = 8.5),
  type = "continuous",
  page_margins = page_mar(bottom = 0.5, top = 0.5, right = 0.5, left = 0.5)
)

# Save to Word with 3 text columns
save_as_docx(ft, path = "compact_species_report_winter_2026.docx", 
             pr_section = sect_properties)

# Check the structure of the 'proportions' object
str(proportions)
# 'data.frame':	88 obs. of  3 variables:
# $ Species                         : chr  "Asian Green Bee-eater" "Booted Warbler" "Feral Pigeon" "Purple Sunbird" ...
# $ % occurrence across point counts: chr  "48.15" "40.74" "11.11" "88.89" ...
# $ % of total abundance            : chr  "2.97" "1.68" "0.49" "3.86" ...

# The 2nd and 3rd column has to be converted to numeric, since they are currently
# shown as characters:
proportions$`% occurrence across point counts` <- as.numeric(proportions$`% occurrence across point counts`)
proportions$`% of total abundance` <- as.numeric(proportions$`% of total abundance`)

# Check the species having the minimum % abundance:
proportions$Species[proportions$`% of total abundance` == min(proportions$`% of total abundance`)]
# [1] "Asian Woolly-necked Stork" "Barred Buttonquail"        "Indian Golden Oriole"      "Red-whiskered Bulbul"     
# [5] "Coppersmith Barbet"        "Greater Coucal"            "Grey Heron"                "Pied Kingfisher"          
# [9] "Blue-tailed Bee-eater"     "Painted Stork"             "Black-rumped Flameback"    "Great White Egret"        
# [13] "Streak-throated Swallow"   "Wood Sandpiper"            "Red-wattled Lapwing"       "Common Babbler"           
# [17] "Pin-tailed Snipe"          "Rufous Treepie"            "Common Iora"               "Crested Honey-buzzard"    
# [21] "Grey Francolin"            "Indian Peafowl"            "River Tern"                "Grey Wagtail"   
# So, there are 24 such species.
# Find the min proportion of the species with the minimum % abundance:
min(proportions$`% of total abundance`)
#[1] 0.1
# Check the species having the maximum % abundance:
proportions$Species[proportions$`% of total abundance` == max(proportions$`% of total abundance`)]
# [1] "Barn Swallow"
# Find the max proportion of the species with the maximum % abundance
max(proportions$`% of total abundance`)
# [1] 19.49

# Check the species having the minimum % occurrence (this corresponds to the species 
# which appears in the least number of sites):
proportions$Species[proportions$`% occurrence across point counts` == min(proportions$`% occurrence across point counts`)]
# [1] "Asian Woolly-necked Stork" "Rock Bush-Quail"           "Baya Weaver"               "Barred Buttonquail"       
# [5] "Indian Golden Oriole"      "Indian Grey Hornbill"      "Red-whiskered Bulbul"      "Indian Cormorant"         
# [9] "Coppersmith Barbet"        "Greater Coucal"            "Grey Heron"                "Pied Kingfisher"          
# [13] "Blue-tailed Bee-eater"     "Glossy Ibis"               "Indian Spot-billed Duck"   "Little Cormorant"         
# [17] "Painted Stork"             "Black-rumped Flameback"    "Great White Egret"         "Streak-throated Swallow"  
# [21] "Wood Sandpiper"            "Yellow-wattled Lapwing"    "Red-wattled Lapwing"       "Common Babbler"           
# [25] "Pin-tailed Snipe"          "House Crow"                "Rufous Treepie"            "Common Iora"              
# [29] "Yellow-eyed Babbler"       "Crested Honey-buzzard"     "Grey Francolin"            "Indian Peafowl"           
# [33] "River Tern"                "Grey Wagtail"  
# So, there are 34 such species.
# Find the min proportion of the species with the minimum % occurrence:
min(proportions$`% occurrence across point counts`)
#[1] 3.7
# Check the species having the maximum % occurrence (this corresponds to the species 
# which appears in the maximum number of sites):
proportions$Species[proportions$`% occurrence across point counts` == max(proportions$`% occurrence across point counts`)]
# [1] "Purple Sunbird"
# Find the max proportion of the species with the maximum % occurrence:
max(proportions$`% occurrence across point counts`)
# [1] 88.89
                          
# Create a column in pc_data2 which shows the number of individuals of each species
# across sites
pc_data2$total_count <- rowSums(pc_data2[, -1])

# Check the number of individuals of the species with the maximum number
# of individuals recorded across the sites
max(pc_data2$total_count)
# [1] 197
# Check the number of individuals of the species with the minimum number
# of individuals recorded across the sites (this will most certainly remain 1 every time)
min(pc_data2$total_count)
# [1] 1

# The information that you can get from the following section can also be derived from
# species accumulation plot. But it makes the job easier.
                          
# Create a column in pc_data2 which shows the number of sites where a species
# was present/detected
pc_data2$presence_count <- rowSums(pc_data2[, c(-1, -ncol(pc_data2))] > 0)

# Check the number of sites occupied by the species occurring in the maximum number
# of sites
max(pc_data2$presence_count)
# [1] 24
# Check the number of sites occupied by the species occurring in the minimum number
# of sites (this will most certainly remain 1 every time)
min(pc_data2$presence_count)
# [1] 1

# Create a row in pc_data2 which shows the total number of species detected in
# in each point count site

# Calculate column sums for all numeric columns (number of species)
species_totals <- colSums(pc_data2[, -1] > 0)

species_totals
# G3-1           G3-2           G3-3           G3-4           G4-1           G4-2           G5-1           G6-1 
# 8             19             17              8             13              8             19             10 
# G6-2           G7-1           G7-2           G8-1           G8-2           G9-1          G10-1          G10-2 
# 12             15             13             14             21              8              7              8 
# G11-1          G11-2          G12-1          G13-1          G14-1          G15-1          G15-2          G15-3 
# 16             10             15              8             10             21             10             22 
# G20-1          G20-4          G20-7    total_count presence_count 
# 13             25             22             88             88 

# Remove the "total_count" and presence_count" elements
species_totals <- species_totals[names(species_totals) != "presence_count" &
                                 names(species_totals) != "total_count"]

# Add an empty element at the beginning and two at the end so that species_totals can be bound to pc_data2
species_totals <- c("", species_totals,"","")

# Bind this is a row to pc_data2
final_data <- rbind(pc_data2, species_totals)

# Save this dataframe as a csv file:
write.csv(final_data, "final_data.csv", row.names = FALSE)




