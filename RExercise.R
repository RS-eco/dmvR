#' ---
#' title: "R Exercise"
#' author: "RS-eco"
#' date: "08.04.2019"
#' ---

#'
#' 1. Piping
#' ========================================================

#' Install and load the magrittr package
#install.packages("magrittr", dep=T)
library(magrittr)

#' ####################

#' Turn the following code into a 
#' streamlined version using piping:

# Without piping
mean_width <- mean(iris$Sepal.Width)
iris_sub <- subset(iris, Sepal.Width < mean_width)
plot(iris_sub)
cor_iris <- cor(iris_sub$Sepal.Length, 
    iris_sub$Sepal.Width)

# With piping



######################################################################

#'
#' 2. Read data
#' ========================================================

#' Install and load the readr package
#+ message=F, warning=F
#install.packages(readr, dep=T)
library(readr)

#' Set working directory
# setwd("")

#' ####################

#' Read species_info.csv.xz into a data.frame called species_info
#' Note: species_info contains information on more than 40000 vertebrate species



#' ####################

#' Check class and structure of species_info



#' ####################

#' Read first 10 entries of species_info.csv.xz



#' ####################

#' Read last 10 lines of species_info.csv.xz



######################################################################

#'
#' 3. tidyr
#' ========================================================

#' Install and load the tidyr package
#+ message=F, warning=F
#install.packages("tidyr", dep=T)
library(tidyr)

#' ####################

#' Using species_info, replace NAs of presence, origin and 
#' season column with 0s.
#' Complete binomial, presence, origin and season columns of species_info
#' Drop missing values from species_info
#' Separate binomial into genus and species, but keep binomial



#' ####################

#' Why do you get an error message and how could we avoid it?



#' ####################

#' Read species_data.csv.xz into a data.frame called species_data
#' Note: species_data contains gridded species occurrence data for Bavaria
#' at a spatial resolution of 0.5 degree



#' ####################

#' 2. Turn species_data from wide into long format, drop missing values and 
#' save to a data.frame called species_long



#' Why do we have to drop NAs after we turn data into a long format?



#' ####################

#' Turn species_long back into wide format and 
#' write to a data.frame called species_wide



######################################################################

#'
#' 4. dplyr
#' ========================================================
  
# Install and load dplyr package
#+ warning=F, message=F
#install.packages("dplyr", dep=T)
library(dplyr)

#' ####################

#' Using species_long calculate the number of occurrences per species 
#' using group_by() and summarise



#' Of course, this could also be done with colSums() or count()

#' ####################

#' Using species_long, identify how many species have less than 10 occurrences?



#' ####################

#' Using species_long calculate species richness per grid cell



#' ####################

#' Extract data of the following 5 species from species_long:
species <- c("Anas crecca", "Lacerta agilis", "Mustela nivalis", 
             "Pelophylax lessonae", "Vulpes vulpes")



#' ####################

#' Using species_wide, extract data for all species starting with "Ac"



#' Why do you have to use species_wide here?



#' ####################

#' Calculate the number of species per class in species_info and 
#' output as table using the kable function of the knitr package



######################################################################

#'
#' 5. dplyr - Part II
#' ========================================================

#' ####################

#' Calculate the number of species per class in species_data
#' Hint: To do this, you first need to join species_data with species_info



#' ####################

#' Calculate the number of species per grid cell for each class
#' Hint: To do this, species_info must contain distinct species names (binomial)



#' ####################

#' Identify the most widely distributed species for each class



#' Note: This should not be higher than the maximum grid cells of Bavaria (189)

######################################################################

#'
#' 6. Making plots with ggplot2
#' ========================================================

#' Install and load ggplot2
#+ warning=F
#install.packages(ggplot2, dep=T)
library(ggplot2)

#' ####################

#' Plot barchart of the number of species per class

# Same as before
#data_plot <- species_long %>% 
#  left_join(species_info, by=("binomial")) %>% 
#  group_by(class_name) %>% distinct(binomial) %>%
#  summarise(no_species= n())   

# Now, we add a plot



#' ####################

#' Plot number of occurrences for the 10 least common species



#' ####################

#' Plot species richness against latitude using different geoms
#' Adjust labels and theme



#' ####################

#' Use faceting to split the previous plots by class



#' ####################

#' Use patchwork to create individual plots for each class

#' Install and load the patchwork package

#+ eval=F
#install.packages("devtools")
#devtools::install_github("thomasp85/patchwork")
library(patchwork)



#' ####################

#' Plot boxplot of species richness against class and add significant
#' differences with geom_signif() from the ggsignif package

#install.packages("ggsignif", dep=T)
library(ggsignif)



#' ####################

#' Make map of species richness



######################################################################

#'
#' 7. Data analysis with dplyr
#' ========================================================

#' ####################

#' Create linear model using do(), 
#' i.e. of species richness against latitude 
#' for each class



#' ####################

#' Extract coefficients of linear model



#' ####################

#' Run and plot gam of species richness against latitude for each class 
#' using ggplot2



#' ####################

#' Run and plot lm of species richness against latitude for each class
#' using ggplot2 and add equation to plot using ggpmisc



#' We can do the same using a quadratic term



#' ####################

#' Run ANOVA to compare linear models of linear versus quadratic term
#' using the tidy() function of the broom package

#install.packages("broom", dep=T)
library(broom)



######################################################################

#'
#' 8. Spatio-temporal data with stars and sf
#' ========================================================

#' ####################

#' Read tas_ewembi_deu_1981_2010.nc using the read_ncdf() function of 
#' the stars package, turn into data.frame and drop NAs

#' tas_ewembi_deu_1981_2010.nc contains daily temperature data for Germany
#' at a spatial resolution of 0.5 degree from 1981 - 2010

#+ warning=F, message=F
#install.packages("stars", dep=T)
library(stars)



#' ####################

#' Define timestamp of date variable (z) using as.Date()



#' ####################

#' Calculate mean temperature per date
#' Split date into yday and year using the lubridate package
#' And make a polar plot of daily mean temperature coloured by year



#' ####################

#' Plot map of 30-year average temperature
#' Read gadm36_DEU_adm1.shp into R using st_read() of the sf pacage
#' Add polygon of Germany to map using geom_sf

#install.packages("sf", dep=T)
library(sf)



#' ####################

#' For every grid cell calculate monthly mean temperature per year using 
#' as.yearmon() from the zoo package.
#' Make a linear model using do() and extract Intercept, Slope 
#' and R2 values per grid cell.



#' ####################

#' Plot map of slope and R2



#' ####################

#' Join species_long with species_info and then 
#' join 30-year average temperature per year with species data



#' ####################

#' Plot richness against temperature



#' ####################

#' Plot richness against temperature per class


