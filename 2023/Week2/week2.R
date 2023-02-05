# ------------------------------------------------------------------------
# WEEK 2 #TidyTuesday
# AUTHOR: Matteo Larrode
# THEME: "Project FeederWatch"
# ------------------------------------------------------------------------

# load packages ----------------------------------------------------------

library("tidyverse")
library("tidytuesdayR") #get data
library("lubridate") #make_date()
library("zoo") #as.yearmon() for Year-Month format
library("ggplot2")
library("gganimate")
library('gifski')
library('png')
library("usmap")

#borders of US for mapping
us <- rgeoboundaries::gb_adm1("usa") %>% 
  st_crop(xmin = -125, xmax = -67, ymin = 23, ymax = 52) 


# pre-cleaning ----------------------------------------------------------

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



# data wrangling ----------------------------------------------------------
feederwatch_sparrows <- read_csv("2023/Week2/feederwatch_sparrows.csv")

#create date with month & year variables & filter nonsensical lat and longitude
#check lat & long
  #mainland boundaries: 50°N, 20°N // 130°W, 75°W
  #Alaska boundaries: 72°N, 54°N // 174°W, 126°W
feederwatch_sparrows <- feederwatch_sparrows %>%
  filter(Year < 2019) %>%
  mutate(obs_date = make_date(Year, Month, Day))%>%
  filter((latitude < 50 & latitude > 20 & longitude > -130 & longitude < -75) |
           (latitude < 72 & latitude > 54 & longitude > -174 & longitude < -126) )


#set coordinate system that fits usmap
feederwatch_sparrows <- usmap_transform(
  feederwatch_sparrows,
  input_names = c("longitude", "latitude"),
  output_names = c("x", "y"))


# visualization ----------------------------------------------------------
us_map <- plot_usmap()

sparrow_plot <- us_map +
  geom_point(data = feederwatch_sparrows, 
             aes(x=x, y=y, 
             col = species_code, 
             size = how_many))+
  transition_time(obs_date) + #for animation
  shadow_wake(wake_length = 0.1)+
  theme(text=element_text(family="Helvetica", color = "white"),
        plot.title=element_text(hjust=0.5, face="bold", size=25, margin=margin(t=15)),
        plot.subtitle=element_text(hjust=0.5, size=19, margin=margin(t=15), face="italic"),
        plot.caption=element_text(hjust=0.95, size=13, margin=margin(b=12)),
        plot.margin = margin(r = 40,
                             b = 10, 
                             l= 40),
        plot.background = element_rect(fill = "#1a4169", color = NA),
        legend.title = element_blank(),
        legend.text = element_text(size=13),
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.position = "bottom")+
  labs(title = "Observations of Migratory Birds on Feederwatch",
    subtitle = "Month: {as.yearmon(frame_time)}",
    caption="Data from feederwatch.org | Chart by @matteoStats")+
  scale_color_discrete(labels = c("Field Sparrow", 
                                  "Fox Sparrow", 
                                  "Golden-crowned Sparrow"))
  


#animate map (gganimate)
animate(sparrow_plot, 
        duration = 17, # = 365 days/yr x 4 years x 0.02 sec/day = 25 seconds
        height = 800, width = 800)

anim_save("2023/Week2/feederwatch.gif")