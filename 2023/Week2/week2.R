# ------------------------------------------------------------------------
# WEEK 2 #TidyTuesday
# AUTHOR: Matteo Larrode
# THEME: "Project FeederWatch"
# ------------------------------------------------------------------------

# load packages ----------------------------------------------------------

library("tidyverse")
library("tidytuesdayR") #get data
library("lubridate") #make_date()
library("ggplot2")
library("gganimate")
library("sf")
library("usmap")

#borders of US for mapping
us <- rgeoboundaries::gb_adm1("usa") %>% 
  st_crop(xmin = -125, xmax = -67, ymin = 23, ymax = 52) 


# data wrangling ----------------------------------------------------------

#load data
feederwatch_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv')

#extra data from earlier years (downloaded from FeederWatch)
feederwatch_2016_2020 <- read_csv("PFW_2016_2020_public.csv")

#keep 3 species of migratory birds (looking on internet & dictionary for species code) -> Sparrows
  #1: Golden-crowned Sparrow: gocspa, Zonotrichia atricapilla
  #2: Fox Sparrow: foxspa, Passerella iliaca
  #3: Field Sparrow, fiespa, Spizella pusilla

feederwatch_16_20_sparrows <- feederwatch_2016_2020 %>%
  filter(SPECIES_CODE == "gocspa" |
         SPECIES_CODE == "foxspa" |
          SPECIES_CODE == "fiespa") %>%
  select(LATITUDE, LONGITUDE, OBS_ID, Month, Day, Year, SPECIES_CODE,
                HOW_MANY, VALID)

feederwatch_20_21_sparrows <- feederwatch_raw %>%
  filter(species_code == "gocspa" |
           species_code == "foxspa" |
           species_code == "fiespa") %>%
  select(latitude, longitude, obs_id, Month, Day, Year, species_code,
         how_many, valid)


#join both datasets
#standardising column names
colnames(feederwatch_16_20_sparrows) <- colnames(feederwatch_20_21_sparrows)

#bind by rows
feederwatch_sparrows <- bind_rows(feederwatch_16_20_sparrows,feederwatch_20_21_sparrows)

#create new csv for future use 
write_csv(feederwatch_sparrows, "feederwatch_sparrows.csv")
feederwatch_sparrows <- read_csv("feederwatch_sparrows.csv")

#create date with month & year variables & filter nonsensical lat and longitude
feederwatch_sparrows <- feederwatch_sparrows %>%
  mutate(obs_date = make_date(Year, Month)) 

#TO DO -> check lat & long

#set coordinate system that fits usmap
feederwatch_sparrows <- usmap_transform(
  feederwatch_sparrows,
  input_names = c("longitude", "latitude"),
  output_names = c("x", "y")
)


# visualization ----------------------------------------------------------
us_map <- plot_usmap()

sparrow_plot <- us_map +
  geom_point(data = feederwatch_sparrows, 
             aes(x=x, y=y, 
             col = species_code, 
             size = how_many))



sparrow_plot


#animate map with sightings for those birds (gganimate)



