# ------------------------------------------------------------------------
# WEEK 3 #TidyTuesday
# AUTHOR: Matteo Larrode
# THEME: "Art History"

# CITATION: Lemus S, Stam H (2022). arthistory: Art History Textbook Data.

# load packages ----------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(ggridges)
library(forcats)


# pre-cleaning ----------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2023-01-17')
arthistory <- tuesdata$artists

selected_years <- c("1936", "1948", "1959", "1970", "1975", "1991", "1995", "2005", "2009", "2013", "2020")

# data wrangling ----------------------------------------------------------

#pre-selection of variables
arthistory_df <- arthistory %>%
  select(-artist_nationality, -artist_nationality_other, -artist_ethnicity, -artist_unique_id) %>%
  filter(artist_gender != "N/A" & artist_race != "N/A") %>% #na.omit() not working because of the "/"
  filter(space_ratio_per_page_total >= 0 & space_ratio_per_page_total <= 1) %>%
  mutate(edition_number = as.factor(edition_number),
         yearFct = fct_rev(as.factor(sort(year))),
         genderFct = as.factor(artist_gender)) #change some variables to factor for fill aes()

#further variable selection & reduce number of years displayed (22 -> 15)
arthistory_final <- arthistory_df %>%
  select(yearFct, genderFct, artist_race_nwi, space_ratio_per_page_total) %>%
  filter(yearFct %in% selected_years)


# visualization----------------------------------------------------------

#1) Representation as page ratio ------

  #!! density grouped by gender so does NOT show differences in total representations genders

ridge_plot <- ggplot(arthistory_final, 
                     aes(y = yearFct))+
  scale_y_discrete(breaks = selected_years)+
  geom_density_ridges(aes(x = space_ratio_per_page_total,
                          fill = genderFct), # fill created later
                      rel_min_height = 0.05,
                      alpha = .7,
                      color = "white",
                      scale = 2,
                      from = 0, to = 1)+
  scale_fill_cyclical(
    breaks = c("Male", "Female"),
    values = c("#ff0000", "#0000ff", "#ff8080", "#8080ff"),
    labels = c(`Male` = "Male Artists", `Female` = "Female Artists"),
    name = NULL , guide = "legend") +
  labs(x = "Page Space Ratio (area of text & figure of the artist / area of page)",
       y = NULL,
       title = "Gender representation in popular art history textbooks",
       subtitle = "Differences in space taken by artists on the pages of the different editions of \n Janson’s History of Art and Gardner’s Art Through the Ages",
       caption = "Matteo Larrode (@matteoStats) | Source: Lemus S, Stam H (2022). arthistory: Art History Textbook Data")+
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
  theme(text=element_text(family="Roboto", color = "#22211d"),
        plot.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        plot.margin = margin(l = 30, t = 15, b = 15, r = 15),
        plot.title=element_text(hjust=0.5, face="bold", size=25,  color = "#4e4d47"),
        plot.subtitle=element_text(hjust=0.5, size=19, margin=margin(t=12, b = 15), color = "#4e4d47"),
        plot.caption=element_text(hjust=0.9, size=13, margin=margin(t = 30, b=15), color = "#4e4d47"),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1, 'cm'),
        legend.position = c(0.87, 0.5),
        legend.text = element_text(size = 13),
        axis.text.y = element_text(size = 13))


ridge_plot


ggsave("2023/Week3/gender_page_ratio.png", height = 9, width = 15)
