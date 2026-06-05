#this script cleans and reformats data for the systematic map, ready for analysis elsewhere
rm(list=ls())
#first load packages
pacman::p_load(tidyverse,countrycode,rnaturalearth,rnaturalearthdata,
               tidyr,tidytext,forcats)

#load data
sys_map_data<-read.csv("data/extracted_data_2026_01_21.csv")
factor_categories<-read.csv("data/evidence_factor_categories.csv")
articles<-read.csv("data/articles_2026_01_26.csv")

#########################################################
# 1- Tidy and clean data#################################
#########################################################

#1.1 Clean column names

#convert all column names to lower case, replace spaces with underscores, and dashes with underscores
colnames(sys_map_data) <- colnames(sys_map_data) %>%str_trim() %>%tolower() %>% str_replace_all("[ .\\/-]+", "_")|>       
  str_replace_all("_+", "_")|>             
  str_replace_all("^_|_$", "")

# reorder/remove columns in the data frame
sys_map_data <- sys_map_data|>
#reorder columns so that rayyan_key is first
  relocate(rayyan_key)|>
#remove columns "marca_temporal" and "reviewer_name"
  select(-marca_temporal, -reviewer_name)


#1.2 Standardise variables

#1.2.1 Study location

#tidy up study location names - removing all those that are global or don't mention a specific location
sys_location_subset<-sys_map_data%>%separate_rows(location, sep = ",\\s*")|>
  filter(location!="Global",
         location!="None mentioned",
         location!="Africa",
         location!="Asia",
         location!="Asia-Pacific",
         location!="Central America",
         location!="Europe",
         location!="North America",
         location!="Oceania",
         location!="South America",
         location!="Not details",
         location!="South-East Asia",
         location!="European Commission",
         location!="Not mentioned")%>%
  #remove any white space from location names
  mutate(location=str_trim(location))|>
  #remove any blank locations
  filter(!is.na(location),location!="",location!=" ")

#fix country names
sys_location_subset<-sys_location_subset|>
  mutate(
    location = str_squish(location),
    location = na_if(location, ""),
    
    # standardise common abbreviations/variants
    location_std = case_when(
      location %in% c("USA", "U.S.A.", "US", "U.S.") ~ "United States",
      location %in% c("UK", "U.K.")                  ~ "United Kingdom",
      location == "denmark"                          ~ "Denmark",
      location == "Lao"                              ~ "Laos",
      location %in% c("Congo","the Congo","Republic of Congo","Republic of the Congo") ~ "Republic of the Congo",
      location %in% c("the Democratic Republic of the Congo","Democratic Republic of Congo") ~ "Democratic Republic of the Congo",
      location == "Saint Martin" ~ "Saint-Martin",
      location == "Ivory Coast" ~ "Côte d'Ivoire",
      TRUE ~ location
    ))

#add information on the continent of each location
sys_location_subset_continent<-sys_location_subset|>
  mutate(
    continent_raw = countrycode(
      location_std,
      origin = "country.name",
      destination = "continent",
      warn = FALSE
    ))

#1.2.2 biomes
sys_map_data <- sys_map_data|>
  mutate(biome_original = biome)


sys_biome_long <- sys_map_data|>
  filter(!is.na(biome), biome != "")|>
  separate_rows(biome, sep = ",\\s*")|>
  mutate(
    biome_std = case_when(
      str_detect(
        biome,
        regex("agricultur|agro ?ecosystems?",
              ignore_case = TRUE)
      ) ~ "Agroecosystem",
      
      str_detect(
        biome,
        regex("not mentioned|not reported",
              ignore_case = TRUE)
      ) ~ "Not reported",
      
      str_detect(
        biome,
        regex(
          "mangroves?|mountains?|floodplains?|terrestrial ecosystems|grasslands?|shrublands?|deserts?|coastal|polar regions?|peatlands?",
          ignore_case = TRUE
        )
      ) ~ "Other",
      
      TRUE ~ biome
    )
  )


#clean biome related data
#calculate number of studies with biome data

  group_by(biome_std) %>%
  summarise(
    perc_studies = (n() / no_studies) * 100,
    n_studies=n(),
    .groups = "drop"
  ) %>%
  arrange(desc(perc_studies))
 
#save this as the clean version of the extracted data
write.csv(sys_location_subset_continent,"data/processed/cleaned_data.csv",row.names = FALSE)


###############################################################
#2 study context###############################################
###############################################################

#remove columns that are not related to location
location_data<-sys_location_subset_continent|>
  select(location_std,continent_raw)|>
#count number of studies per location
  group_by(location_std)|>
  summarise(n_studies=n())|>
  ungroup()|>
  #order by number of studies
  arrange(desc(n_studies))|>
  #calculate percentage of studies per location
  mutate(perc_studies=(n_studies/475)*100)

#add ISO3 country name
location_data$iso_a3_eh<-countrycode(location_data$location_std,origin = "country.name",destination = "iso3c")
#manually fix code for Saint Martin
location_data$iso_a3_eh[location_data$location_std=="Saint-Martin"]<-"MAF"

#save this data as a csv for later analyses
write.csv(location_data,"data/processed/study_location_counts.csv",row.names = FALSE)




