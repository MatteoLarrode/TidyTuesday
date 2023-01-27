# ------------------------------------------------------------------------
# WEEK 2 #TidyTuesday
# AUTHOR: Matteo Larrode
# THEME: "Project FeederWatch"
# ------------------------------------------------------------------------

# load packages ----------------------------------------------------------

library("tidyverse")
library("tidytuesdayR") #get data


# data wrangling ----------------------------------------------------------

#load data
feederwatch_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv')


#IDEA: keep migratory birds
#animate map with sightings for those birds




# visualization ----------------------------------------------------------



#The maps are surface maps interpolated from the locations as inverse distance weighted averages using an influence radius of 100 kilometers and a power of -2 decay of influence. 
#In other words, each point on the surface maps is the mean of all values with a radius of 100 kilometers weighted by the inverse of the square root of the distance to each point. 


#animate using gganimate
