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

# data wrangling ----------------------------------------------------------

arthistory_df <- arthistory %>%
  select(-artist_nationality, -artist_nationality_other, -artist_ethnicity, -artist_unique_id) %>%
  filter(artist_gender != "N/A" & artist_race != "N/A") %>%
  filter(space_ratio_per_page_total >= 0 & space_ratio_per_page_total <= 1) %>%
  mutate(edition_number = as.factor(edition_number),
         yearFct = fct_rev(as.factor(sort(year))))


# visualization----------------------------------------------------------
#INTERSECTIONAL ANALYSIS
  #most viz focus on either gender or ethnicity
  #consider both as interrelated

#ridgeline plot!
  #x axis: measure of importance in history books, separated by gender
  #y axis: evolution over time
  #add ethnicity with facet?

ridge_plot <- ggplot(arthistory_df, 
                     aes(group = year,
                         y = year,
                         x = space_ratio_per_page_total))+
  scale_y_reverse(breaks = c(2020, 2005, 1991, 1970, 1948, 1926))+
  geom_density_ridges(rel_min_height = 0.01,
                      scale = 1.5,
                      quantile_lines = TRUE, 
                      quantiles = 2)+
  #facet_wrap(~artist_race_nwi)+
  theme_ridges(grid = FALSE)
  

ridge_plot


geom_density_ridges(
  aes(x = Percent, fill = paste(YearFct, Option)), 
  alpha = .8, color = "white", from = 0, to = 100
)

