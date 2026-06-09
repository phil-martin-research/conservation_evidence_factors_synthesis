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

#1.2.1 biomes
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


#1.2.2
#reclassify conservation problems into broader categories
sys_biome_con_long <- sys_biome_long |>
  separate_rows(conservation_problem, sep = ",\\s*") |>
  filter(!is.na(conservation_problem), conservation_problem != "") |>
  mutate(
    conservation_problem = str_trim(conservation_problem),
    conservation_problem_lower = str_to_lower(conservation_problem),
    conservation_problem_std = case_when(
      str_detect(
        conservation_problem,
        regex("land[- ]?use change", ignore_case = TRUE)
      ) ~ "Land use change",
      
      conservation_problem_lower %in% str_to_lower(c(
        "habitat degradation", "Forest restoration", "Agriculture impacts",
        "Marine spatial planning", "Urbanisation", "Restoration",
        "Ecosystem restoration", "Infrastructure"
      )) ~ "Land use change",
      
      conservation_problem_lower %in% str_to_lower(c(
        "Sustainable shark fisheries", "Overfishing"
      )) ~ "Overexploitation",
      
      conservation_problem_lower %in% str_to_lower(c(
        "climate change mitigation", "climate change impacts", "Climate Change"
      )) ~ "Climate change",
      
      conservation_problem_lower %in% str_to_lower(c(
        "Protected area managment", "wetland conservation",
        "Marie Protected Area management",
        "Multiple: forest management and governdance",
        "Not mentioned", "managment of protected forests",
        "Assessment of conservation status", "Plant reintroduction",
        "Forest managment", "Protected area designation",
        "Protected areas management", "Natural resource management",
        "Endangered species managemnt", "Biodiversity proected",
        "Development of decision-support tools",
        "Protected area management", "River management", "River regulation"
      )) ~ "Not reported",
      
      conservation_problem_lower %in% str_to_lower(c(
        "Eutrophication"
      )) ~ "Pollution",
      
      conservation_problem_lower %in% str_to_lower(c(
        "Water use", "Human-wildlife conflict", "Fire",
        "Identification of vulnerable ecosystems", "Predation of birds",
        "Pest control", "GMOs", "Species conservation prioritisation",
        "Various: Grazing impacts of reindeer"
      )) ~ "Other",
      
      TRUE ~ conservation_problem
    )
  ) |>
  select(-conservation_problem_lower)


#1.2.3 Study locations

# create long-format location dataset, keeping all location rows
sys_location_long_all <- sys_biome_con_long |>
  separate_rows(location, sep = ",\\s*") |>
  mutate(
    # clean raw location text
    location = str_squish(location),
    location = na_if(location, ""),
    
    # standardise common abbreviations/variants
    location_std = case_when(
      location %in% c("USA", "U.S.A.", "US", "U.S.") ~ "United States",
      location %in% c("UK", "U.K.") ~ "United Kingdom",
      location == "denmark" ~ "Denmark",
      location == "Lao" ~ "Laos",
      location %in% c(
        "Congo", "the Congo", "Republic of Congo", "Republic of the Congo"
      ) ~ "Republic of the Congo",
      location %in% c(
        "the Democratic Republic of the Congo",
        "Democratic Republic of Congo"
      ) ~ "Democratic Republic of the Congo",
      location == "Saint Martin" ~ "Saint-Martin",
      location == "Ivory Coast" ~ "Côte d'Ivoire",
      TRUE ~ location
    ),
    
    # add continent where countrycode can recognise the standardised location
    continent_raw = countrycode(
      location_std,
      origin = "country.name",
      destination = "continent",
      warn = FALSE
    )
  )

# save full long-format version, including regional/global/not-reported rows
write.csv(
  sys_location_long_all,
  "data/processed/full_data_set.csv",
  row.names = FALSE
)

# create country-only subset for country-level analyses
non_country_locations <- c(
  "Global",
  "None mentioned",
  "Africa",
  "Asia",
  "Asia-Pacific",
  "Central America",
  "Europe",
  "North America",
  "Oceania",
  "South America",
  "South-East Asia",
  "European Commission",
  "Not details",
  "Not mentioned"
)

sys_location_country <- sys_location_long_all |>
  filter(
    !is.na(location_std),
    !location_std %in% non_country_locations
  )

# save country-only version
write.csv(
  sys_location_country,
  "data/processed/cleaned_country_data.csv",
  row.names = FALSE
)


###############################################################
#2 study context###############################################
###############################################################

#2.1 location data

#remove columns that are not related to location
location_data<-sys_location_country|>
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

#2.2 biome data

#calculate number of studies with biome data

no_studies_biome<-length(unique(sys_biome_con_long$rayyan_key))

sys_biome_summary<-sys_biome_con_long|>
  group_by(biome_std)|>
  summarise(
    perc_studies = (n() / no_studies_biome) * 100,
    n_studies=n(),
    .groups = "drop"
  ) %>%
  arrange(desc(perc_studies))

#save this data as a csv for later analyses
write.csv(sys_biome_summary,"data/processed/study_biome_counts.csv",row.names = FALSE)


#calculate percentage of studies per conservation problem
sys_con_summary<-sys_biome_con_long|>
  group_by(conservation_problem_std)|>
  summarise(perc_studies = (n() / no_studies_biome) * 100, 
            n_studies=n(),
            .groups = "drop")|>
  arrange(desc(perc_studies))

#save this data as a csv for later analyses
write.csv(sys_con_summary,"data/processed/study_con_prob_counts.csv",row.names = FALSE)

