# this script produces all the figures and analyses for the 
# systematic map manuscript on evidence use in conservation

#first load packages
pacman::p_load(tidyverse,ggtext,ggspatial,cowplot,lemon,ggmap,scales,sf,
               countrycode,rnaturalearth,rnaturalearthdata,rcartocolor,
               tidyr,UpSetR,ComplexUpset,ggplot2,BaseSet,ggh4x)


#load data
sys_map_data<-read_csv("data/extracted_data_2025-11-03.csv")
factor_categories<-read_csv("data/evidence_factor_categories.csv")

#########################################################
# 1- Tidy and clean data#################################
#########################################################

#convert all column names to lower case, replace spaces with underscores, and dashes with underscores
colnames(sys_map_data)<-colnames(sys_map_data) %>% tolower() %>% str_replace_all(" ", "_")%>%str_replace_all("-", "_")

########################################################
#2 - Figure 1 - Study context###########################
########################################################

#Figure 1a - map of study locations

#tidy up study location names - removing all those that are global or don't mention a specific location
sys_location_subset<-sys_map_data%>%separate_rows(location, sep = ",\\s*")%>%
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
         location!="South-East Asia")

#count number of studies per location
sys_location_subset<-sys_location_subset%>%
  group_by(location)%>%
  summarise(n_studies=n())%>%
  ungroup()

sys_location_subset%>%
  print(n=120)

#add ISO3 country name
sys_location_subset$iso_a3_eh<-countrycode(sys_location_subset$location,origin = "country.name",destination = "iso3c")

#make global map
world<-ne_countries(scale="medium",returnclass = "sf")

#join geocoded data to shapefile
world_map_join<-world%>%
  left_join(sys_location_subset,by = "iso_a3_eh",keep=FALSE)

#plot map
location_plot<-ggplot(world_map_join,aes(fill=n_studies))+
  geom_sf(colour="grey70",linewidth=0.1)+
  theme_void()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2,"cm"),
        legend.key.height = unit(0.25,"cm"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12))+
  coord_sf(xlim = c(-180,180),ylim=c(-55,85),expand = FALSE)+
  scale_fill_carto_c(palette = "Purp",name="No. of studies",breaks=c(0,5,10,15,20,25))

#figure 1b - bar chart of biomes in which studies were done
#count percentage of studies per biome

#calculate number of studies with biome data
no_studies<-nrow(sys_map_data%>%filter(!is.na(biome)))

#calculate percentage of studies per biome
sys_biome_subset <- sys_map_data %>%
  filter(!is.na(biome), biome != "") %>%
  separate_rows(biome, sep = ",\\s*") %>%
  mutate(
    biome = str_trim(biome),
    biome_std = case_when(
      str_detect(biome, regex("Agricultur|Mangrove|Mountains?|Floodplains?|Terrestrial ecosystems|Grassland|
                              Shrubland|Desert|Coastal|Shrubland",
                              ignore_case = TRUE)) ~ "Other",
      str_detect(biome,regex("Not mentioned",ignore_case = TRUE)) ~ "Not reported",
      TRUE ~ biome
    )
  ) %>%
  group_by(biome_std) %>%
  summarise(perc_studies = (n() / no_studies) * 100, .groups = "drop") %>%
  arrange(desc(perc_studies))

#plot biome data
biome_plot<-ggplot(sys_biome_subset,aes(x=reorder(biome_std,perc_studies),y=perc_studies))+
  geom_bar(stat="identity")+
  theme_cowplot()+
  coord_flip()+
  labs(x="Biome",y="Percentage of studies (%)")+
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

#figure 1c - bar chart with conservation problems addressed in studies

#calculate percentage of studies per conservation problem

#this data needs tidying before it can be plotted - separate out multiple problems in one row and standardise names

sys_map_data%>%
  filter(!is.na(conservation_problem))%>%
  separate_rows(conservation_problem, sep = ",\\s*")%>%
  select(conservation_problem)%>%
  distinct()%>%
  print(n=100)


#reclassify categories
sys_map_data%>%
  filter(!is.na(conservation_problem))%>%
  separate_rows(conservation_problem, sep = ",\\s*")%>%
  mutate(conservation_problem = str_trim(conservation_problem),
         conservation_problem_std = case_when(
           str_detect(conservation_problem, regex("land-use change", ignore_case = TRUE)) ~ "Land use change",
           conservation_problem %in% c("habitat degradation", "Forest restoration",
                                       "Agriculture impacts","Marine spatial planning",
                                       "Urbanisation","Restoration","Ecosystem restoration") ~ "Land use change",
           conservation_problem %in% c("Sustainable shark fisheries", "Overfishing",) ~ "Overexploitation",
           conservation_problem %in% c("climate change") ~ "Climate change",
           conservation_problem %in% c("Protected area managment","wetland conservation") ~ "Not reported",
           conservation_problem %in% c("Eutrophication") ~ "Pollution",
           conservation_problem %in% c("Assessment of conservation status","Plant reintroduction",
                                       "Water use","Human-wildlife conflict","Fire",
                                       "Identification of vulnerable ecosystems","Predation of birds",
                                       "Pest control","Forest managment","Protected area designation",                 
                                       "Protected areas management","Natural resource management",
                                       "Endangered species managemnt","GMOs","Biodiversity proected",
                                       "Development of decision-support tools"  ) ~ "Other",
           TRUE ~ conservation_problem))%>%
  group_by(conservation_problem_std)%>%
  summarise(perc_studies=(n()/no_studies)*100)%>%
  ungroup()%>%
  arrange(desc(perc_studies))

out <- sys_map_data %>%
  separate_rows(conservation_problem, sep = ",\\s*") %>%
  filter(!is.na(conservation_problem), conservation_problem != "") %>%
  mutate(conservation_problem = str_trim(conservation_problem)) %>%
  mutate(
    conservation_problem_std = case_when(
      str_detect(conservation_problem, regex("land[- ]?use change", ignore_case = TRUE)) ~ "Land use change",
      str_to_lower(conservation_problem) %in% str_to_lower(c(
        "habitat degradation","Forest restoration","Agriculture impacts","Marine spatial planning",
        "Urbanisation","Restoration","Ecosystem restoration"
      )) ~ "Land use change",
      str_to_lower(conservation_problem) %in% str_to_lower(c(
        "Sustainable shark fisheries","Overfishing"
      )) ~ "Overexploitation",
      
      str_to_lower(conservation_problem) %in% str_to_lower(c("climate change mitigation","climate change impacts","Climate Change")) ~ "Climate change",
      str_to_lower(conservation_problem) %in% str_to_lower(c("Protected area managment","wetland conservation",
                                                             "Marie Protected Area management","Multiple: forest management and governdance",
                                                             "Not mentioned","managment of protected forests",
                                                             "Assessment of conservation status","Plant reintroduction",
                                                             "Forest managment","Protected area designation","Protected areas management",
                                                             "Natural resource management","Endangered species managemnt",
                                                             "Biodiversity proected","Development of decision-support tools",
                                                             "Protected area management"
      )) ~ "Not reported",
      str_to_lower(conservation_problem) %in% str_to_lower(c("Eutrophication")) ~ "Pollution",
      
      str_to_lower(conservation_problem) %in% str_to_lower(c(
        "Water use","Human-wildlife conflict","Fire",
        "Identification of vulnerable ecosystems","Predation of birds","Pest control","GMOs","Species conservation prioritisation",
        "Various: Grazing impacts of reindeer"
      )) ~ "Other",
      
      TRUE ~ conservation_problem
    )
  ) %>%
  group_by(conservation_problem_std) %>%
  summarise(perc_studies = (n() / no_studies) * 100, .groups = "drop") %>%
  arrange(desc(perc_studies))

#plot conservation problem data
conservation_problem_plot<-ggplot(out,aes(x=reorder(conservation_problem_std,perc_studies),y=perc_studies))+
  geom_bar(stat="identity")+
  theme_cowplot()+
  coord_flip()+
  labs(x="Threat",y="Percentage of studies (%)")+
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

#combine all of these into figure 1

#first combine plots b and c
biome_threat<-plot_grid(biome_plot,conservation_problem_plot,ncol=2,labels=c("(b)","(c)"),
                        label_size = 12,rel_widths=c(1,1))

map_biome_threat<-plot_grid(location_plot,biome_threat,ncol=1,labels=c("(a)",""),
                            label_size = 12,rel_heights = c(1,0.7))

#save figure 1
ggsave("figures/figure_1_study_context.png",
       plot=map_biome_threat,
       width=20,
       height=15,
       units="cm",
       dpi=300)



#################################################################
#Figure 2 - Actors and organisations#############################
#################################################################

#figure 2a - bar chart with types of actors that are the focus of studies

# Build a single, readable regex for things you want to collapse to "Other"
other_pat <- regex(
  # use groups, plurals and small spelling variants
  "\\b(Indigenous (groups|representatives)|Knowledge[- ]?brokers?|Advisors?|Recrational fishers: coded as practitioners|Mining and agriculture representatives?|Policy analysts?|Science communicators?|Journalists?|Funders?|Grant funders?)\\b",
  ignore_case = TRUE
)

sys_actor_subset <- sys_map_data %>%
  select(rayyan_key, stakeholder_population) %>%
  # 1) keep rows with at least some content
  filter(!is.na(stakeholder_population), stakeholder_population != "") %>%
  # 2) split on commas into long form (one actor per row)
  separate_longer_delim(stakeholder_population, delim = ",") %>%
  # 3) trim/squish whitespace so matching works
  mutate(stakeholder_population = str_squish(stakeholder_population)) %>%
  # 4) collapse many specific labels into "Other"
  mutate(
    stakeholder_population = case_when(
      str_detect(stakeholder_population, other_pat) ~ "Other",
      TRUE ~ stakeholder_population
    )
  ) %>%
  # 5) avoid double‐counting a study that lists the same actor multiple times
  distinct(rayyan_key, stakeholder_population) %>%
  # 6) mark presence and pivot to wide 0/1 incidence matrix
  mutate(mentioned = 1L) %>%
  pivot_wider(
    names_from  = stakeholder_population,
    values_from = mentioned,
    values_fill = 0,
    # if duplicates slipped through, ensure the result is still 0/1
    values_fn   = ~ as.integer(any(. == 1))
  )


#Create an upset plot


listInput <- list(Practitioners = which(sys_actor_subset$Practitioners == 1),
                  Researchers = which(sys_actor_subset$Researchers == 1),
                  Policymakers = which(sys_actor_subset$Policymakers == 1),
                  Other = which(sys_actor_subset$Other == 1))

upset(fromList(listInput),order.by="freq",mainbar.y.label = "Number of studies",
      sets.x.label = "No. of studies per group",show.numbers = FALSE)

#same figure but removing "Other" category

listInput2 <- list(Practitioners = which(sys_actor_subset$Practitioners == 1),
                   Researchers = which(sys_actor_subset$Researchers == 1),
                   Policymakers = which(sys_actor_subset$Policymakers == 1))
actor_plot<-upset(fromList(listInput2),order.by="freq",mainbar.y.label = "Number of studies",
                  sets.x.label = "No. of studies per group",show.numbers = FALSE)



#figure 2b - bar chart with types of organisations that are the focus of studies

#list unique values for organisation types

names(sys_map_data)

Organisation_matrix <- sys_map_data %>%
  select(rayyan_key, type_of_organisation_studied) %>%
  filter(!is.na(type_of_organisation_studied), type_of_organisation_studied != "") %>%
  separate_rows(type_of_organisation_studied, sep = ",\\s*") %>%
  mutate(
    type_of_organisation_studied = str_squish(type_of_organisation_studied),
    .type_lower = str_to_lower(type_of_organisation_studied),
    org_type_std = case_when(
      .type_lower %in% c(
        "first nation fisheries",
        "first nations group",
        "indigenous group",
        "local recreational fishers",
        "recreational fisheries"
      ) ~ "Community/local organisation",
      TRUE ~ type_of_organisation_studied
    )
  ) %>%
  distinct(rayyan_key, org_type_std) %>%
  mutate(mentioned = 1L) %>%
  pivot_wider(
    names_from  = org_type_std,           # <- pivot on the recoded label
    values_from = mentioned,
    values_fill = 0,
    values_fn   = ~ as.integer(any(. == 1))
  )

Organisation_matrix <- sys_map_data %>%
  select(rayyan_key, type_of_organisation_studied) %>%
  filter(!is.na(type_of_organisation_studied), type_of_organisation_studied != "") %>%
  separate_rows(type_of_organisation_studied, sep = ",\\s*") %>%
  mutate(
    type_of_organisation_studied = str_squish(type_of_organisation_studied),
    .type_lower = str_to_lower(type_of_organisation_studied),
    org_type_std = case_when(
      .type_lower %in% c(
        "first nation fisheries",
        "first nations group",
        "indigenous group",
        "local recreational fishers",
        "recreational fisheries"
      ) ~ "Community/local organisation",
      TRUE ~ type_of_organisation_studied
    )
  ) %>%
  distinct(rayyan_key, org_type_std) %>%
  mutate(mentioned = 1L) %>%
  pivot_wider(
    names_from  = org_type_std,           # <- pivot on the recoded label
    values_from = mentioned,
    values_fill = 0,
    values_fn   = ~ as.integer(any(. == 1))
  )%>%
  #Trim whitespace from column names
  rename_with(~ str_trim(.x)) %>%
  #Replace spaces and slashes with underscores
  rename_with(~ str_replace_all(.x, "[ /-]+", "_"))

names(Organisation_matrix)

#input for upset plot
listInput_orgs <- list("Government/statutory body" = which(Organisation_matrix$Government_statutory_body== 1),
                       "NGO/non_profit" = which(Organisation_matrix$NGO_non_profit == 1),
                       "Academic institution" = which(Organisation_matrix$Academic_institution== 1),
                       "Community/local organisation" = which(Organisation_matrix$Community_local_organisation== 1))

organisation_plot<-upset(fromList(listInput_orgs),order.by="freq",mainbar.y.label = "Number of studies",
                         sets.x.label = "No. of studies per group",show.numbers = FALSE)


#combine the two upset plots into figure 2

#try with complexupset package
load("data/movies.rda")

#set genres
genres<-colnames(movies)[18:24]
#convert to boolean values

movies[genres] = movies[genres] == 1
t(head(movies[genres], 3))
movies[movies$mpaa == '', 'mpaa'] = NA
movies = na.omit(movies)

#plot this
set_size(8, 3)
upset(movies, genres, name='genre', width_ratio=0.1)

#try to reproduce this with our data
#first for actors
#remove the 'other' category
actors<-colnames(sys_actor_subset)[c(-1,-which(colnames(sys_actor_subset)=="Other"))]
sys_actor_subset_bool <- sys_actor_subset %>%
  mutate(across(-rayyan_key, ~ .x == 1L))

actors_upset<-upset(sys_actor_subset_bool, actors, name='Actor type', width_ratio=0.1,
                    # with manual aes specification:
                    base_annotations=list('Number of studies'=(intersection_size(counts=FALSE))),
                    set_sizes=FALSE)+theme(text=element_text(size=10))

#then for organisations
colnames(Organisation_matrix)
#remove the categories "Research funders", "Not_detailed" and "Intergovernmental_organisation"
organisations<-colnames(Organisation_matrix)[c(-1,-7,-8,-9)]
Organisation_matrix_bool <- Organisation_matrix %>%
  mutate(across(-rayyan_key, ~ .x == 1L))

#plot this
organisation_upset<-upset(Organisation_matrix_bool, organisations, name='Organisation type', width_ratio=0.1,
                          base_annotations=list('Number of studies'=(intersection_size(counts=FALSE))),
                          set_sizes=FALSE,
                          labeller=ggplot2::as_labeller(c(
                            'Government_statutory_body'='Government/statutory body',
                            'NGO_non_profit'='NGO/non profit',
                            'Academic_institution'='Academic institution',
                            'Community_local_organisation'='Community/local organisation',
                            "Private_sector"='Private sector'
                          )))+theme(text=element_text(size=10))
organisation_upset

#join these plots together for figure 2
plot_grid(actors_upset,organisation_upset,ncol=2,labels=c("(a)","(b)"),
          label_size = 12,rel_widths = c(1,2),align = "h")
#save plot
ggsave("figures/figure_2_actors_organisations.png",
       width=20,
       height=12,
       units="cm",
       dpi=300)

########################################################################
##Figure 3 - Factors influencing evidence use###########################
########################################################################

############################################
#alternative version just using percentages#
############################################

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
                                             "Accessibility",
                                             "Format & language",
                                             "Research skills",
                                             "Rigour",
                                             "Uncertainty",
                                             "Attitude to evidence use",
                                             "Existence",
                                             "Timeliness",
                                             "Between colleagues",
                                             "Source",
                                             "Practitioner-stakeholder",
                                             "Time lag",
                                             "Management",
                                             "Spatial/temporal scale",
                                             "Quantity of information",
                                             "Language barrier",
                                             "Other stakeholder values",
                                             "Personal characteristics",
                                             "Skills & awareness",
                                             "Nature of decision",
                                             "Decision process",
                                             "Awareness of evidence",
                                             "Implementation capacity",
                                             "Academic demands",
                                             "Culture",
                                             "Culture",
                                             "Inconclusive",
                                             "Attitude to evidence use")

#add alternative high-level categories
mentions_factors_categories <- mentions_factors_categories %>%
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
    ))%>%
  #arrange in descending order of percentage mentions
  arrange(desc(perc_mentions))

#plot this
mentions_factors_categories%>%
  dplyr::filter(category!="Characteristics of other stakeholders")%>%
  ggplot(aes(y = reorder(factors_label, perc_mentions), x = perc_mentions)) +
  geom_bar(stat="identity") +
  labs(y = "Factor impacting evidence use", x = "Pecentage of studies mentioning factor") +
  theme_bw()+
  facet_wrap(~category, scales = "free_y",ncol=1)+
  force_panelsizes(rows = c(0.3,1,1,0.3))+
  theme(axis.text = element_text(size=10))

ggsave("figures/figure_4_percentage_factors.png",
       width=15,
       height=20,
       units="cm",
       dpi=300)


########################################################
#figure 4 - heatmaps of factors by biomes and continent#
########################################################

#first by biome
#join factor mentions to biome data
sys_biome_factors <- sys_map_data %>%
  select(rayyan_key, biome, factors_influencing_evidence_use) %>%
  filter(!is.na(biome), !is.na(factors_influencing_evidence_use)) %>%
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
  filter(!is.na(biome), biome != "") %>%
  separate_rows(biome, sep = ",\\s*") %>%
  mutate(
    biome = str_trim(biome),
    biome_std = case_when(
      str_detect(biome, regex("Agricultur|Mangrove|Mountains?|Floodplains?|Terrestrial ecosystems|Grassland|
                              Shrubland|Desert|Coastal|Shrubland",
                              ignore_case = TRUE)) ~ "Other",
      str_detect(biome,regex("Not mentioned",ignore_case = TRUE)) ~ "Not reported",
      TRUE ~ biome
    )
  ) %>%
  group_by(biome_std, category) %>%
  summarise(n_mentions = n(), .groups = "drop") %>%
  arrange(biome_std, category)%>%
  #filter out biomes "Not reported" and "Other"
  filter(biome_std!="Not reported",
         biome_std!="Other")%>%
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

unique(sys_biome_factors$biome_std)

#now plot this as a heatmap
ggplot(sys_biome_factors,aes(x=category,y=biome_std,fill=n_mentions))+
  geom_tile(colour="white")+
  scale_fill_carto_c(palette = "Purp",name="No. of studies")+
  theme_cowplot()+
  labs(x="Factor category",y="Biome")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

#this is not interesting! Biomes have similar patterns of factors mentioned

#try by continent instead



##################################################
#Supplementary figures############################
##################################################

#figure S1 - bar chart with types of scientific evidence considered
#calculate percentage of studies per type of scientific evidence considered
sys_evidence_subset<-sys_map_data%>%
  select(type_of_scientific_evidence)%>%
  filter(!is.na(type_of_scientific_evidence))

#calculate percentage of studies per type of scientific evidence considered
sys_evidence_summary<-sys_evidence_subset%>%
  separate_longer_delim(type_of_scientific_evidence,delim=", ")%>%
  group_by(type_of_scientific_evidence)%>%
  summarise(perc_studies=(n()/nrow(sys_map_data))*100)%>%
  ungroup()%>%
  arrange(desc(perc_studies))

#need to standardise names of evidence types or find a way to group the wide variety of evidence types

#plot evidence data
evidence_plot<-ggplot(sys_evidence_summary,aes(x=reorder(type_of_scientific_evidence,perc_studies),y=perc_studies))+
  geom_bar(stat="identity")+
  theme_cowplot()+
  coord_flip()+
  labs(x="Type of scientific evidence",y="Percentage of studies (%)")+
  scale_fill_carto_d(palette = "Vivid")+
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

#figure S2 - bar chart with the different types of discipline considered

sys_discipline_subset<-sys_map_data%>%
  select(evidence_discipline)%>%
  filter(!is.na(evidence_discipline))

#calculate percentage of studies per type of discipline considered
sys_discipline_summary<-sys_discipline_subset%>%
  separate_longer_delim(evidence_discipline,delim=", ")%>%
  group_by(evidence_discipline)%>%
  summarise(perc_studies=(n()/nrow(sys_map_data))*100)%>%
  ungroup()%>%
  arrange(desc(perc_studies))

#plot discipline data
discipline_plot<-ggplot(sys_discipline_summary,aes(x=reorder(evidence_discipline,perc_studies),y=perc_studies))+
  geom_bar(stat="identity")+
  theme_cowplot()+
  coord_flip()+
  labs(x="Discipline of scientific evidence",y="Percentage of studies (%)")+
  scale_fill_carto_d(palette = "Vivid")+
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

#figure S3

#count number of factors identified per study
factors_per_study <- sys_map_data %>%
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
                      "Social, political, and economic context")
  ) %>%
  filter(!is.na(factors_influencing_evidence_use)) %>%
  group_by(rayyan_key) %>%
  summarise(n_factors = n(), .groups = "drop")

#plot this in histogram
ggplot(factors_per_study,aes(x=n_factors))+
  geom_histogram(binwidth=1,fill="steelblue",colour="black")+
  theme_cowplot()+
  labs(x="Number of factors identified per study",
       y="Number of studies")+
  scale_x_continuous(breaks=seq(0,15,1))

