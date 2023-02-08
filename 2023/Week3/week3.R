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

selected_years <- c("1926", "1936", "1948", "1959", "1970", "1977", "1986", "1995", "2001", "2007", "2013", "2020")

# data wrangling ----------------------------------------------------------

#pre-selection of variables
arthistory <- arthistory %>%
  select(-artist_nationality, -artist_nationality_other, -artist_ethnicity, -artist_unique_id) %>%
  filter(artist_gender != "N/A" & artist_race != "N/A") %>% #na.omit() not working because of the "/"
  filter(space_ratio_per_page_total >= 0 & space_ratio_per_page_total <= 1) %>%
  mutate(edition_number = as.factor(edition_number),
         yearFct = fct_rev(as.factor(sort(year))),
         genderFct = as.factor(artist_gender)) #change some variables to factor for fill aes()

#further variable selection & reduce number of years displayed (22 -> 15)
arthistory_df <- arthistory %>%
  select(yearFct, genderFct, artist_race_nwi, space_ratio_per_page_total)%>%
  filter(yearFct %in% selected_years)



# visualization----------------------------------------------------------
#INTERSECTIONAL ANALYSIS
  #most viz focus on either gender or ethnicity
  #consider both as interrelated

#ridgeline plot!
  #x axis: measure of importance in history books, separated by gender
  #y axis: evolution over time
  #add ethnicity with facet?

ridge_plot <- ggplot(arthistory_df, 
                     aes(y = yearFct))+
  scale_y_discrete(breaks = c("2020", "2007", "1995", "1986", "1970", "1948", "1926"))+
  
  geom_density_ridges(aes(x = space_ratio_per_page_total,
                          fill = genderFct), # fill created later
                      rel_min_height = 0.07,
                      alpha = .8,
                      color = "white",
                      scale = 1.5,
                      quantile_lines = TRUE, 
                      quantiles = 2,
                      from = 0, to = 1)+
  
  scale_fill_cyclical(
    breaks = c("Male", "Female"),
    values = c("#ff0000", "#0000ff", "#ff8080", "#8080ff"),
    labels = c(`Male` = "Male Artists", `Female` = "Female Artists"),
    name = "Gender", guide = "legend"
  ) +
  labs(
    x = "Page space ratio (area of both text and figure of the artist / area of a page)",
    y = "Year",
    title = "Gender representation in popular art history textbooks",
    subtitle = "Differences in space taken by artists on the pages of Janson’s History of Art and Gardner’s Art Through the Ages",
    caption = "Matteo Larrode (@matteoStats) | Source: Lemus S, Stam H (2022). arthistory: Art History Textbook Data")+
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
  theme(text=element_text(family="Helvetica"),
        plot.title=element_text(hjust=0.5, face="bold", size=17),
        plot.subtitle=element_text(hjust=0.5, size=13, margin=margin(t=15, b = 15), face="italic"),
        plot.caption=element_text(size=10, hjust=0.95, margin=margin(t = 15, b=12)))
  

ridge_plot



