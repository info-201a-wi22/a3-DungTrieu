
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(data.table)

incarceration_trends <- read.csv("~/documents/INFO-201-Exercises/a3-DungTrieu/source/incarceration_trends.csv")

incarceration_trends_by_county <- incarceration_trends %>%
  group_by(county_name) %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  select(black_jail_pop, white_jail_pop) %>%
  mutate(blacktowhiteratio = black_jail_pop / white_jail_pop)
  

biggest_jail_pop_race_2018 <- incarceration_trends %>%
  filter(year == 2018) %>%
  filter(total_pop == max(total_pop)) %>%
  select(county_name, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop
         , other_race_jail_pop)

biggest_prison_adm_race_2018 <- incarceration_trends %>%
  filter(total_prison_adm == max(total_prison_adm, na.rm = TRUE)) %>%
  select(county_name, aapi_prison_adm, black_prison_adm, latinx_prison_adm, native_prison_adm, white_prison_adm
         , other_race_prison_adm)

adm_race_count <- incarceration_trends %>%
  select(aapi_prison_adm, black_prison_adm, latinx_prison_adm, native_prison_adm, white_prison_adm
         , other_race_prison_adm) %>%
  colSums(na.rm = TRUE)

total_prison_adm_race <- data.frame(t(adm_race_count))

jail_pop_count <- incarceration_trends %>%
  select(aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop
         , other_race_jail_pop) %>%
  colSums(na.rm = TRUE)

total_jail_pop_race <- data.frame(t(jail_pop_count))

biggest_jail_ice <- incarceration_trends %>%
  filter(year == max(year)) %>%
  filter(!total_jail_from_ice == 0, na.rm = TRUE) %>%
  arrange(desc(total_jail_from_ice)) %>%
  head(10) %>%
  select(county_name, state, total_jail_from_ice)


top_5_jail_pop_data_WA_2018 <- incarceration_trends %>%
  group_by(state) %>%
  filter(state == "WA") %>%
  filter(year == "2018") %>%
  select(county_name,year,aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop
         , other_race_jail_pop, total_jail_pop) %>%
  arrange(desc(total_jail_pop)) %>%
  head(5) 

jail_pop_data_WA <- incarceration_trends %>%
  filter(state == "WA") %>%
  filter( county_name == "King County") %>%
  filter(  year =="2014"|
           year =="2015"| 
           year =="2016"|
           year =="2017"|
           year =="2018") %>%
  select(year, black_jail_pop_rate, white_jail_pop_rate,-state) %>%
  setnames(old = c('black_jail_pop_rate','white_jail_pop_rate'), 
           new = c('Black','White')) %>%
  gather(key = race, value = rate, -year)

trend_chart <- ggplot(data=jail_pop_data_WA, aes(x=year, y=rate, group=race)) +
  geom_line(aes(color=race))+
  geom_point(aes(color=race)) +
  ggtitle("Trend of black incarceration rates vs white incarceration rates in King County from 2014-2018") +
  labs(y= "Incarceration Rates", x = "Year", fill="Race")

trend_chart

  
