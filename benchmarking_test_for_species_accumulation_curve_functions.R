# Authored by Paul Pop
# This R script tests which of the functions available in different R packages does the job of the creation of
# species accumulation curves the fastest and without error.

# This was tested using data of multiple point counts of birds in a single day at the Choodasandra.

#Find the current working directly (from where files will be loaded to and saved by default)
getwd()

#Change the working directory to wherever you like (ideally something related to the code):
setwd("D:/Paul_Pop_Birds/Paul_Pop_Choodasandra lake")

#List the files in the working directory to see if data needed to be loaded is there 
list.files()

#and copy-paste that data file ("Choodasandra_4thJan2025_Table.csv") in this case 
#to the below argument:
sp_data <- read.csv( "Choodasandra_4thJan2025_Table.csv")

# Load necessary packages 
library(dplyr)
library(tibble)
library(vegan)

#View a sample of the data:
head(sp_data)
#     Point.Count.Numbers X1 X2 X3 X4 X5 X6 X7
# 1 Asian Green Bee-eater  0  0  0  0  1  0  0
# 2            Asian Koel  0  0  1  0  0  0  0
# 3          Barn Swallow  0  0  0  0  0  0  1
# 4          Black Drongo  0  0  1  0  0  0  0
# 5            Black Kite  1  0  0  0  0  0  1
# 6  Blyth's Reed Warbler  0  1  1  0  0  0  0
#OR
dplyr::glimpse(sp_data)
class(sp_data)

#Transpose the data for the next step:
sp_data <- as.data.frame(t(sp_data))
#Glimpse the data again:
dplyr::glimpse(sp_data)

#convert rownames to column
sp_data <- rownames_to_column(sp_data)

sp_data2 <- sp_data[-1, ]

#Remove columns not containing species abundance data, and only input the columns
#containing species abundances for calculating the Species Accumulation Curve:

sac_raw <- sp_data2 %>%
  # Remove the point count site numbers 
  dplyr::select(-rowname) %>%
  # Compute Species Accumulation Curve:
  vegan::poolaccum(.)

summary(sac_raw, display = "chao")
# $chao
#      N     Chao     2.5%     97.5%   Std.Dev
# [1,] 3 60.54222 30.33333 127.00000 26.477212
# [2,] 4 74.16000 41.00000 172.17500 37.682868
# [3,] 5 73.20840 45.34800 165.40000 27.570724
# [4,] 6 65.81524 50.80952  86.25000  9.966099
# [5,] 7 63.57143 63.57143  63.57143  0.000000

plot(sac_raw)

obs <- data.frame(summary(sac_raw)$S, check.names = FALSE)
colnames(obs) <- c("N", "S", "lower2.5", "higher97.5", "std")
# N = No. of sites (i.e. survey effort)
# S = Observed species richness
# lower2.5 = lower 95% confidence interval of S
# upper97.5 = upper 95% confidence interval of S
head(obs)
# N     S lower2.5 higher97.5      std
# 1 3 14.71    9.000     19.000 2.709113
# 2 4 18.79   14.000     23.525 2.731393
# 3 5 22.76   19.475     27.000 2.323007
# 4 6 25.93   24.000     29.000 1.646883
# 5 7 29.00   29.000     29.000 0.000000
class(obs)
# [1] "data.frame"

#Plotting SAC with 95% CI
obs %>%
  ggplot(data = ., aes(x = N,
                       y = S)) +
  scale_x_continuous(breaks = 7) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = lower2.5,
                  ymax = higher97.5),
              alpha = 0.5,
              color = "lightblue",
              fill = "lightblue") +
  theme_classic()+
  # Add observed richness line 
  geom_line() +
  labs(x = "No. of sites",
       y = "No. of species",
       subtitle = "Species accumulation across point counts in Choodasandra lake")

# Testing spp.est (EstimateS equivalent in R)
# Uncomment (remove hash) and install if not installed already
install.packages("fossil")
# Load the package
library("fossil")

# Reference for the spp.est function 
# https://palaeo-electronica.org/2011_1/238/estimate.htm#:~:text=The%20spp.,of%20randomizations%20should%20be%20run.

# Another package parallises the spp.est function
# https://www.rdocumentation.org/packages/parfossil/versions/0.2.0/topics/par.spp.est
# this example is for a multicore Windows computer
# Load the necessary library (install if not present already) 
library(remotes)
#install_version("parfossil", "0.2.0") #removed from CRAN. So, installed from archive.
library(parfossil)
library(doSNOW)   # Parallel backend for Windows (SOCK cluster)
library(snow)     # Basic snow cluster support

# The following step creates a SOCK (socket) cluster with 2 nodes on your local machine.
# "localhost" repeated twice = using 2 CPU cores/processors.
# This allows R to run calculations simultaneously on multiple cores.
cl <- makeCluster(c("localhost","localhost"), type = "SOCK")
# Now, register the cluster with the foreach parallel backend
# Tell R to use this cluster for any parallel operations
registerDoSNOW(cl)
data(fdata.mat)
# fdata.mat is a community data matrix (rows = sites, columns = species)
system.time({spp.est(fdata.mat, rand = 100, abund = TRUE, counter = FALSE)})
# rand = 100: Performs 100 randomizations (rarefaction/re-sampling)
# abund = TRUE: Uses abundance data (not just presence/absence)
# counter = FALSE: Doesn't show progress counter (reduces overhead)

# user  system elapsed 
# 0.03    0.00    0.05 
# There were 23 warnings (use warnings() to see them) 
#Warning (ignore): This data appears to be presence/absence based, but this estimator 
#is for abundance data only

system.time({par.spp.est(fdata.mat, rand = 100, abund = TRUE, counter = FALSE)})
# user  system elapsed 
# 0.02    0.00    0.17 

# Always stop the cluster after use:
stopCluster(cl)

#From the speed test (performed multiple times), it looks like serial processing is better than parallelized 
#function. So, the function spp.est is faster than par.spp.est
