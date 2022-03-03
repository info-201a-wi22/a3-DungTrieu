setwd("/Users/dtrieu99/documents/a3-DungTrieu")

library(dplyr)
library(tidyverse)
library(ggplot2)

incarceration_trends <- read.csv("/Users/dtrieu99/documents/a3-DungTrieu/source/incarceration_trends.csv")

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
