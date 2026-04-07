>Code and documentation authored by Paul Pop.
>
>This research was carried out under the BIRD lab, ATREE, Bengaluru (PI: Rajkamal Goswami).
>
>*Version 1.0 - last updated 2026-04-07 <br>
>Last update - Introduction*
>
>View the most current version at https://github.com/paulvpop/bird_point_count_analysis_and_reporting/blob/main/bird_point_count_analysis_and_reporting.md

This R script tests which of the functions available in different R packages does the job of the creation of
species accumulation curve the fastest and without error.

This was tested using data of multiple point counts of birds in a single day at the Choodasandra.

Find the current working directly (from where files will be loaded to and saved by default)
```
getwd()
```
Change the working directory to wherever you like (ideally something related to the code):
```
setwd("D:/Paul_Pop_Birds/Paul_Pop_Choodasandra lake")
```
List the files in the working directory to see if data needed to be loaded is there... 
```
list.files()
```
and copy-paste that data file ("Choodasandra_4thJan2025_Table.csv" in this case) to the below argument
```
sp_data <- read.csv( "Choodasandra_4thJan2025_Table.csv")
```
Load necessary packages 
```
library(dplyr)
library(tibble)
library(vegan)
```
View a sample of the data:
```
head(sp_data)
#     Point.Count.Numbers X1 X2 X3 X4 X5 X6 X7
# 1 Asian Green Bee-eater  0  0  0  0  1  0  0
# 2            Asian Koel  0  0  1  0  0  0  0
# 3          Barn Swallow  0  0  0  0  0  0  1
# 4          Black Drongo  0  0  1  0  0  0  0
# 5            Black Kite  1  0  0  0  0  0  1
# 6  Blyth's Reed Warbler  0  1  1  0  0  0  0
```
OR
```
dplyr::glimpse(sp_data)
```
Check the class (OPTIONAL)
```
class(sp_data)
```
Transpose the data for the next step:
```
sp_data <- as.data.frame(t(sp_data))
```
Glimpse the data again:
```
dplyr::glimpse(sp_data)
```
Convert rownames to column
```
sp_data <- rownames_to_column(sp_data)
```
Remove the first row containing species names
```
sp_data2 <- sp_data[-1, ]
```

Remove columns not containing species abundance data, and only input the columns
containing species abundances for calculating the Species Accumulation Curve:
```
sac_raw <- sp_data2 %>%
  # Remove the point count site numbers 
  dplyr::select(-rowname) %>%
  # Compute Species Accumulation Curve:
  vegan::poolaccum(.)
```
View a summary of the sac_raw file created
```
summary(sac_raw, display = "chao")
# $chao
# N     Chao     2.5%     97.5%  Std.Dev
# [1,] 3 60.32333 30.33333 127.00000 23.77699
# [2,] 4 71.77281 41.00000 155.37500 34.35253
# [3,] 5 71.95793 45.12000 165.40000 29.48814
# [4,] 6 66.74482 50.80952  86.25000 10.72660
# [5,] 7 63.57143 63.57143  63.57143  0.00000
```
plot(sac_raw)

<img width="1040" height="510" alt="sac_raw" src="https://github.com/user-attachments/assets/8d5c1860-d261-4ec9-a966-3dc826260d5c" />

Create a dataframe for the species accumulation curve
```
obs <- data.frame(summary(sac_raw)$S, check.names = FALSE)
colnames(obs) <- c("N", "S", "lower2.5", "higher97.5", "std") # rename the columns
```
Here, N = No. of sites (i.e. survey effort),
S = Observed species richness,
lower2.5 = lower 95% confidence interval of S,
upper97.5 = upper 95% confidence interval of S

View the data
```
head(obs)
# N     S lower2.5 higher97.5      std
# 1 3 14.52        9     19.000 2.815828
# 2 4 18.56       15     23.525 2.749913
# 3 5 22.38       20     27.000 2.210078
# 4 6 25.91       24     29.000 1.576949
# 5 7 29.00       29     29.000 0.000000
```

Plot the data
```
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
```
<img width="742" height="471" alt="species_accumulation_curve_Choodasandra" src="https://github.com/user-attachments/assets/6fc61759-d75f-462b-b361-1ded0faf2b86" />

Testing spp.est (EstimateS equivalent in R)
Uncomment (remove hash) and install if not installed already
```
#install.packages("fossil")
```
Load the package
```
library("fossil")
```
[Here's the Reference](https://palaeo-electronica.org/2011_1/238/estimate.htm#:~:text=The%20spp.,of%20randomizations%20should%20be%20run) for the spp.est function.

Another package parallises the spp.est function. [Here's the documentation](https://www.rdocumentation.org/packages/parfossil/versions/0.2.0/topics/par.spp.est) for the same.
This example is for a multi-core Windows operating system. The same has not been tested in apple or Linux operating systems.
Load the necessary library (install if not present already) 
```
library(remotes)
#install_version("parfossil", "0.2.0") #removed from CRAN. So, installed from archive.
library(parfossil)
library(doSNOW)   # Parallel backend for Windows (SOCK cluster)
library(snow)     # Basic snow cluster support
```
The following step creates a SOCK (socket) cluster with 2 nodes on your local machine.
"localhost" repeated twice = using 2 CPU cores/processors.
This allows R to run calculations simultaneously on multiple cores.
```
cl <- makeCluster(c("localhost","localhost"), type = "SOCK")
```
Now, register the cluster with the foreach parallel backend
Tell R to use this cluster for any parallel operations
```
registerDoSNOW(cl)
data(fdata.mat)
```
fdata.mat is a community data matrix (rows = sites, columns = species)
```
system.time({spp.est(fdata.mat, rand = 100, abund = TRUE, counter = FALSE)})
```
rand = 100: Performs 100 randomizations (rarefaction/re-sampling)
abund = TRUE: Uses abundance data (not just presence/absence)
counter = FALSE: Doesn't show progress counter (reduces overhead)
```
# user  system elapsed 
# 0.03    0.00    0.05 
# There were 23 warnings (use warnings() to see them) 
#Warning (ignore): This data appears to be presence/absence based, but this estimator 
#is for abundance data only

system.time({par.spp.est(fdata.mat, rand = 100, abund = TRUE, counter = FALSE)})
# user  system elapsed 
# 0.02    0.00    0.17 
```
# Always stop the cluster after use:
```
stopCluster(cl)
```
From the speed test (performed multiple times), it looks like serial processing is better than parallelized 
function. So, the function spp.est is faster than par.spp.est
