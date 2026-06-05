#this script calculates descriptive statistics for the systematic map

pacman::p_load(tidyverse)

#load data
cleaned_data<-read_csv("data/processed/cleaned_data.csv")
location_count<-read_csv("data/processed/study_location_counts.csv")

#study context

#calculate the mean number of countries per study
cleaned_data|>
  group_by(rayyan_key)|>
  summarise(n_countries=n())|>
  ungroup()|>
  summarise(mean_countries=mean(n_countries),
            sd_countries=sd(n_countries))

#summarise the number of studies per continent and percentage of studies
cleaned_data|>
  group_by(continent_raw)|>
  summarise(n_studies=n())|>
  ungroup()|>
  mutate(perc_studies=(n_studies/475)*100)|>
  print(n=10)

#calculate the mean number of biomes per study
sys_biome_subset_mean<-sys_map_data%>%
  filter(!is.na(biome), biome != "") %>%
  separate_rows(biome, sep = ",\\s*") %>%
  filter(biome!="Not reported",biome!="not reported",biome!="Not mentioned")%>%
  group_by(rayyan_key)%>%
  summarise(n_biomes=n())%>%
  ungroup()%>%
  summarise(mean_biomes=mean(n_biomes),
            sd_biomes=sd(n_biomes))  