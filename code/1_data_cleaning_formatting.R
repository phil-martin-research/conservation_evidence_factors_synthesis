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

#1.2.4 actors

#first organise data for upset plots

#first data for actors

# Build a single, readable regex for things I want to collapse to "Other"
other_pat <- regex(
  # use groups, plurals and small spelling variants
  "\\b(Indigenous (groups|representatives)|Knowledge[- ]?brokers?|Advisors?|Recrational fishers|Mining and agriculture representatives?|Policy analysts?|Science communicators?|Journalist?|Funder?|Grant funders?|Law enforcement|Journalists?)\\b",
  ignore_case = TRUE
)

sys_actor_subset <- sys_map_data %>%
  select(rayyan_key, stakeholder_population) %>%
  # 1keep rows with at least some content
  filter(!is.na(stakeholder_population), stakeholder_population != "") %>%
  # split on commas into long form (one actor per row)
  separate_longer_delim(stakeholder_population, delim = ",") %>%
  # trim whitespace so matching works
  mutate(stakeholder_population = str_squish(stakeholder_population)) %>%
  # collapse many specific labels into "Other"
  mutate(
    stakeholder_population = case_when(
      str_detect(stakeholder_population, other_pat) ~ "Other_actors",
      TRUE ~ stakeholder_population
    )
  ) %>%
  #avoid double‐counting a study that lists the same actor multiple times
  distinct(rayyan_key, stakeholder_population) %>%
  # mark presence and pivot to wide 0/1 incidence matrix
  mutate(mentioned = 1L) %>%
  pivot_wider(
    names_from  = stakeholder_population,
    values_from = mentioned,
    values_fill = 0,
  # if duplicates slipped through, ensure the result is still 0/1
    values_fn   = ~ as.integer(any(. == 1))
  )

#save this data as a csv for later analyses
write.csv(sys_actor_subset,"data/processed/study_actor_counts.csv",row.names = FALSE)


#1.2.5 organisations

#second data for organisations

#list unique values for organisation types

Organisation_matrix <- sys_map_data %>%
  select(rayyan_key, type_of_organisation_studied) %>%
  filter(!is.na(type_of_organisation_studied), type_of_organisation_studied != "") %>%
  separate_rows(type_of_organisation_studied, sep = ",\\s*") %>%
  
  # Clean the raw text and create a normalised key for matching
  mutate(
    type_of_organisation_studied = str_squish(type_of_organisation_studied),
    
    # normalised key: lower-case and treat space/_/-// as separators
    org_key = type_of_organisation_studied %>%
      str_to_lower() %>%
      str_replace_all("[_\\-/]+", " ") %>%
      str_squish()
  ) %>%
  
  # Recode to standardised groups
  mutate(
    org_type_std = case_when(
      # Community/local organisation (include both spaced and underscored variants via org_key)
      # classify range of groups with lower representation as "other"
      org_key %in% c("community local organisation","indigenous groups", "first nations communities", "indigenous organisation","first nations group","local recreational fishers",
                     "indigenous group","recreational fisheries","first nation fisheries","research funders", "intergovernmental organisation", "media organisation","private sector") ~
        "Other",
      
      # Otherwise keep the original label
      TRUE ~ type_of_organisation_studied
    )
  ) %>%
  
  # Avoid double-counting having one row per study and org type
  distinct(rayyan_key, org_type_std) %>%
  
  # Pivot to 0/1 incidence matrix
  mutate(mentioned = 1L) %>%
  pivot_wider(
    names_from  = org_type_std,
    values_from = mentioned,
    values_fill = 0,
    values_fn   = ~ as.integer(any(. == 1))
  ) %>%
  
  # Clean column names: trim + spaces/slashes/dashes/dots -> underscores
  rename_with(~ str_trim(.x)) %>%
  rename_with(~ str_replace_all(.x, "[ .\\/-]+", "_")) %>%
  rename_with(~ str_replace_all(.x, "_+", "_")) %>%
  rename_with(~ str_replace_all(.x, "^_|_$", "")) %>%
  rename_with(~ str_to_lower(.x))


#save this data as a csv for later analyses
write.csv(Organisation_matrix,"data/processed/study_organisation_counts.csv",row.names = FALSE)

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

################################################################################
#3 factors influencing evidence use#############################################
################################################################################

n_studies<-nrow(sys_map_data)

mentions_factors <- sys_map_data %>%
  select(rayyan_key, factors_influencing_evidence_use) %>%
  filter(!is.na(factors_influencing_evidence_use)) %>%
  # Protect the multi-comma phrase with a token that has no commas
  mutate(factors_influencing_evidence_use =
           str_replace_all(factors_influencing_evidence_use,
                           fixed("Social, political, and economic context"),
                           "SOC_POL_ECON__TOKEN")) %>%
  # IMPORTANT: separate_longer_delim uses a literal delimiter (not regex)
  separate_longer_delim(factors_influencing_evidence_use, delim = ",") %>%
  mutate(
    # trim whitespace
    factors_influencing_evidence_use = str_trim(factors_influencing_evidence_use),
    # drop empty fragments that can appear from ",," etc.
    factors_influencing_evidence_use = na_if(factors_influencing_evidence_use, ""),
    # Restore the protected phrase
    factors_influencing_evidence_use =
      str_replace_all(factors_influencing_evidence_use,
                      fixed("SOC_POL_ECON__TOKEN"),
                      "Social, political, and economic context"),
    # Rename "Scale"
    factors_influencing_evidence_use =
      str_replace(factors_influencing_evidence_use,
                  regex("^Scale$", ignore_case = TRUE),
                  "Spatial/temporal scale of evidence"),
    # Rename "Timelieness of evidence"
    factors_influencing_evidence_use =
      str_replace(factors_influencing_evidence_use,
                  regex("^Timelieness of evidence$", ignore_case = TRUE),
                  "Timeliness of evidence"),
    # Rename "Characteristics"
    factors_influencing_evidence_use =
      str_replace(factors_influencing_evidence_use,
                  regex("^Decision-maker characteristics$", ignore_case = TRUE),
                  "Practitioner/policymaker personal characteristics"))%>%
  filter(!is.na(factors_influencing_evidence_use)) %>%
  group_by(factors_influencing_evidence_use) %>%
  summarise(
    n_mentions = n(),
    perc_mentions = 100 * n() / n_studies,
    .groups = "drop"
  ) %>%
  arrange(desc(perc_mentions))

#join to categories
mentions_factors_categories <- mentions_factors %>%
  left_join(factor_categories, by = c("factors_influencing_evidence_use" = "factor"))
mentions_factors_categories%>%
  print(n=100)

mentions_factors_categories$factors_label<-c("Scientist-actor",
                                             "Relevance",
                                             "Capacity & resources",
                                             "Social, political,& economic context",
                                             "Format & language",
                                             "Rigour",
                                             "Accessibility",
                                             "Research skills",
                                             "Uncertainty",
                                             "Attitude to evidence use",
                                             "Existence",
                                             "Timeliness",
                                             "Source",
                                             "Between colleagues",
                                             "Spatial/temporal scale",
                                             "Practitioner-stakeholder",
                                             "Time lag",
                                             "Other stakeholder values",
                                             "Management",
                                             "Personal characteristics",
                                             "Quantity of information",
                                             "Skills & awareness",
                                             "Implementation capacity",
                                             "Language barrier",
                                             "Culture",
                                             "Nature of decision",
                                             "Decision process",
                                             "Awareness of evidence",
                                             "Academic demands",
                                             "Conclusiveness",
                                             "Culture",
                                             "Attitude to evidence use",
                                             "Difficulty finding evidence")

#add alternative high-level categories
mentions_factors_categories <- mentions_factors_categories %>%
  mutate(
    category = fct_recode(
      as_factor(category),
      
      # --- Merge + rename practitioner/policymaker & decision context ---
      "Practitioner/policymaker characteristics,\norganisation, and decisions" = "Practitioner/policymaker",
      "Practitioner/policymaker characteristics,\norganisation, and decisions" = "Management organization",
      "Practitioner/policymaker characteristics,\norganisation, and decisions" = "Decision context",
      
      # --- Standardise researcher label ---
      "Researcher &\nresearch organisations" = "Researcher and research organizations",
      
      # --- Rename evidence facet ---
      "Characteristics of evidence" = "Nature of evidence"
    ))%>%
  #arrange in descending order of percentage mentions
  arrange(desc(perc_mentions))


#save this data as a csv for later analyses
write.csv(mentions_factors_categories,"data/processed/factors_influencing_evidence_use.csv",row.names = FALSE)


#change in factors over time
clean_factors_data<-sys_map_data %>%
  select(rayyan_key, factors_influencing_evidence_use) %>%
  filter(!is.na(factors_influencing_evidence_use)) %>%
  # Protect the multi-comma phrase with a token that has no commas
  mutate(factors_influencing_evidence_use =
           str_replace_all(factors_influencing_evidence_use,
                           fixed("Social, political, and economic context"),
                           "SOC_POL_ECON__TOKEN")) %>%
  # IMPORTANT: separate_longer_delim uses a literal delimiter (not regex)
  separate_longer_delim(factors_influencing_evidence_use, delim = ",") %>%
  mutate(
    # trim whitespace
    factors_influencing_evidence_use = str_trim(factors_influencing_evidence_use),
    # drop empty fragments that can appear from ",," etc.
    factors_influencing_evidence_use = na_if(factors_influencing_evidence_use, ""),
    # Restore the protected phrase
    factors_influencing_evidence_use =
      str_replace_all(factors_influencing_evidence_use,
                      fixed("SOC_POL_ECON__TOKEN"),
                      "Social, political, and economic context"),
    # Rename "Scale"
    factors_influencing_evidence_use =
      str_replace(factors_influencing_evidence_use,
                  regex("^Scale$", ignore_case = TRUE),
                  "Spatial/temporal scale of evidence"),
    # Rename "Timelieness of evidence"
    factors_influencing_evidence_use =
      str_replace(factors_influencing_evidence_use,
                  regex("^Timelieness of evidence$", ignore_case = TRUE),
                  "Timeliness of evidence"),
    # Rename "Characteristics"
    factors_influencing_evidence_use =
      str_replace(factors_influencing_evidence_use,
                  regex("^Decision-maker characteristics$", ignore_case = TRUE),
                  "Practitioner/policymaker personal characteristics"))%>%
  filter(!is.na(factors_influencing_evidence_use))%>%
  left_join(factor_categories, by = c("factors_influencing_evidence_use" = "factor"))%>%
  mutate(
    category = fct_recode(
      as_factor(category),
      
      # --- Merge + rename practitioner/policymaker & decision context ---
      "Decision-maker characteristics,\norganisations, and decisions" = "Practitioner/policymaker",
      "Decision-maker characteristics,\norganisations, and decisions" = "Management organization",
      "Decision-maker characteristics,\norganisations, and decisions" = "Decision context",
      
      # --- Standardise researcher label ---
      "Researcher &\nresearch organisations" = "Researcher and research organizations",
      
      # --- Rename evidence facet ---
      "Characteristics of evidence" = "Nature of evidence"
    ))

#now link this data to bibliographic data to get publication year
clean_factors_data_studies<-clean_factors_data%>%
  left_join(articles,by="rayyan_key",keep=FALSE)%>%
  #only keep ID, factors, and year
  select(rayyan_key,factors_influencing_evidence_use,category,year)

#now count the number of studies per category per year
unique(factors_over_time$category)

factors_over_time <- clean_factors_data_studies %>%
  filter(category != "Characteristics of other stakeholders") %>%
  mutate(category = fct_drop(category)) %>%
  group_by(year, category) %>%
  summarise(n_studies = n_distinct(rayyan_key), .groups = "drop") %>%
  complete(
    year = full_seq(range(year, na.rm = TRUE), 1),
    category = unique(category),
    fill = list(n_studies = 0)
  ) %>%
  arrange(year, category)

#cumulative number of studies over years
factors_over_time_cum <- factors_over_time %>%
  group_by(category) %>%
  arrange(year) %>%
  mutate(cum_studies = cumsum(n_studies)) %>%
  ungroup()

#save this data as a csv for later analyses
write.csv(factors_over_time_cum,"data/processed/factors_over_time_cumulative.csv")
