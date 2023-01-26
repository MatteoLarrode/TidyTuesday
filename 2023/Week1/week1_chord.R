# ------------------------------------------------------------------------
# WEEK 1 #TidyTuesday
# AUTHOR: Matteo Larrode
# THEME: "Bring your own data from 2022!"
# ------------------------------------------------------------------------

# load packages ----------------------------------------------------------

library("tidyverse")
library("tidytuesdayR") #get data
library("countrycode") #convert country code to name
library("circlize") #draw chord diagram
library("ggplotify") #convert basic plot to ggplot
library("cowplot") #help with ggplot
library("extrafont")

# load dataset ------------------------------------------------------------

data <- tidytuesdayR::tt_load('2022-03-08')
erasmus_raw <- data$erasmus

# wrangle data ------------------------------------------------------------

erasmus_reduced <- erasmus_raw %>%
  select(academic_year, sending_country_code, receiving_country_code, participants)%>%
  #change country code to name: #use 'countrycode' library
  mutate(sending_country = countrycode(sending_country_code, origin = "iso2c", destination="iso.name.en"),
         receiving_country = countrycode(receiving_country_code, origin = "iso2c", destination="iso.name.en")) %>%
  #United Kingdom not translated with ISO country codes, override with dplyr "replace"
  mutate(sending_country = replace(sending_country, sending_country_code=="UK","United Kingdom"),
         receiving_country = replace(receiving_country, receiving_country_code=="UK","United Kingdom"))
  

#create dataset with countries either in top 10 receiving or sending

#top10 sending:
erasmus_top_sending <- erasmus_reduced %>%
  filter(sending_country_code != receiving_country_code)%>%
  group_by(sending_country_code)%>%
  summarize(sum_sending = sum(participants))%>%
  arrange(-sum_sending)%>%
  head(10)
  
#top10 receiving:
erasmus_top_receiving <- erasmus_reduced %>%
  filter(sending_country_code != receiving_country_code)%>%
  group_by(receiving_country_code)%>% 
  summarize(sum_receiving = sum(participants))%>%
  arrange(-sum_receiving)%>%
  head(10)

#list of top countries
top_codes <- unique(c(
  erasmus_top_sending$sending_country_code,
  erasmus_top_receiving$receiving_country_code
))

#filter dataset to only include those countries
#include exchange if EITHER countries are in the top 10
final_data_union <- erasmus_reduced %>%
  filter(sending_country_code != receiving_country_code)%>%
  filter(
    sending_country_code %in% top_codes |
    receiving_country_code %in% top_codes
  )%>%
  group_by(sending_country, receiving_country)%>%
  summarise(flow=sum(participants))%>%
  arrange(-flow)


#include exchange if BOTH countries are in the top 10
final_data_intersect <- erasmus_reduced %>%
  filter(sending_country_code != receiving_country_code)%>%
  filter(
    sending_country_code %in% top_codes &
    receiving_country_code %in% top_codes
  )%>%
  group_by(sending_country, receiving_country)%>%
  summarise(flow=sum(participants))%>%
  arrange(flow)


# plot --------------------------------------------------------------------

#create palette depending on flag colour (https://coolors.co/)
grid.col = c(France = "#318CE7", Italy = "#90be6d", Lithuania = "#4d908e",
             Luxembourg = "#f4f3ee", "Netherlands (the)" = "#B33F62", Poland = "#F5EDED",
             Romania = "#F3C677", "United Kingdom" = "#577590", Austria = "#7B1E7A", 
             Belgium = "#000000", Czechia = "#B33F62", Germany = "#c1121f", 
             Spain = "#F9564F")


chordDiagram(final_data_intersect, 
             grid.col = grid.col, 
             transparency = 0.4)

#convert to ggplot object to add ggplot layers 
chord <- recordPlot()

font_import()
loadfonts()


ggplotify::as.ggplot(cowplot::ggdraw(chord))+
  labs(title="The Erasmus Spiderweb",
       subtitle="Mobility of students participating in the Erasmus programme among top participating countries (2014-2020)",
       caption="Data from Data.Europa.eu | Chart by @matteoStats")+
  theme(text=element_text(family="Helvetica"),
        plot.title=element_text(hjust=0.5, face="bold", size=20),
        plot.subtitle=element_text(hjust=0.5, size=12, margin=margin(t=15), face="italic"),
        plot.caption=element_text(size=10, hjust=0.95, margin=margin(b=12)),
        plot.margin=margin(t=20))

ggsave("erasmus.jpeg", height=9, width=9)



