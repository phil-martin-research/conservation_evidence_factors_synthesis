# this script produces all the figures and analyses for the 
# systematic map manuscript on evidence use in conservation

#first load packages
pacman::p_load(tidyverse,ggtext,ggspatial,cowplot,lemon,ggmap,scales,sf,
               countrycode,rnaturalearth,rnaturalearthdata,rcartocolor,
               tidyr,UpSetR,ComplexUpset,ggplot2,BaseSet,ggh4x,
               tidytext,forcats,grid,gridExtra,grateful)

#load data
sys_map_data<-read.csv("data/extracted_data_2026_01_20.csv")
factor_categories<-read.csv("data/evidence_factor_categories.csv")
articles<-read.csv("data/articles_2025_11_12.csv")

#########################################################
# 1- Tidy and clean data#################################
#########################################################

#convert all column names to lower case, replace spaces with underscores, and dashes with underscores
colnames(sys_map_data) <- colnames(sys_map_data) %>%str_trim() %>%tolower() %>% str_replace_all("[ .\\/-]+", "_") %>%       
  str_replace_all("_+", "_") %>%              
  str_replace_all("^_|_$", "") 

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
         location!="South-East Asia")%>%
  #remove any white space from location names
  mutate(location=str_trim(location))


#count number of studies per location
sys_location_subset<-sys_location_subset%>%
  group_by(location)%>%
  summarise(n_studies=n())%>%
  ungroup()

sys_location_subset%>%
  #order by number of studies
  arrange(desc(n_studies))%>%
  #calculate percentage of studies per location
  mutate(perc_studies=(n_studies/nrow(sys_map_data))*100)%>%
  print(n=120)

#identify the continent each country is in - need to fix this so that 
#study identity is taken into account
sys_location_subset %>%
  mutate(
    continent = countrycode(location, "country.name", "continent",
                            warn = TRUE) %>%
      dplyr::coalesce(countrycode(location, "country.name.en", "continent", warn = TRUE))
  ) %>%
  arrange(desc(n_studies)) %>%
  mutate(perc_studies = 100 * n_studies / nrow(sys_map_data)) %>%
  group_by(continent) %>%
  summarise(n_studies = sum(n_studies),
            perc_studies = sum(perc_studies))

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
  scale_fill_carto_c(palette = "Purp",name="No. of studies",breaks=c(0,5,10,15,20,25,30,35,40))

#figure 1b - bar chart of biomes in which studies were done
#count percentage of studies per biome

#calculate number of studies with biome data
no_studies<-nrow(sys_map_data%>%filter(!is.na(biome)))

#calculate percentage of studies per biome
sys_biome_subset <- sys_map_data %>%
  filter(!is.na(biome), biome != "") %>%
  separate_rows(biome, sep = ",\\s*") %>%
  mutate(biome = str_trim(biome),biome_std = case_when(
    #Agroecosystems
    str_detect(biome,regex("agricultur|agro ?ecosystems?", ignore_case = TRUE)) ~ "Agroecosystem",
    # Not reported
    str_detect(biome,regex("not mentioned|not reported", ignore_case = TRUE)) ~ "Not reported",
    #Other
    str_detect(biome,regex("mangroves?|mountains?|floodplains?|terrestrial ecosystems|grasslands?|shrublands?|deserts?|coastal|polar regions?|peatlands?",
                           ignore_case = TRUE)) ~ "Other",
    TRUE ~ biome
  )
  ) %>%
  group_by(biome_std) %>%
  summarise(
    perc_studies = (n() / no_studies) * 100,
    .groups = "drop"
  ) %>%
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

sys_map_data%>%
  filter(!is.na(conservation_problem))%>%
  separate_rows(conservation_problem, sep = ",\\s*")%>%
  select(conservation_problem)%>%
  distinct()%>%
  print(n=100)


#reclassify categories
out <- sys_map_data %>%
  separate_rows(conservation_problem, sep = ",\\s*") %>%
  filter(!is.na(conservation_problem), conservation_problem != "") %>%
  mutate(conservation_problem = str_trim(conservation_problem)) %>%
  mutate(
    conservation_problem_std = case_when(
      str_detect(conservation_problem, regex("land[- ]?use change", ignore_case = TRUE)) ~ "Land use change",
      str_to_lower(conservation_problem) %in% str_to_lower(c(
        "habitat degradation","Forest restoration","Agriculture impacts","Marine spatial planning",
        "Urbanisation","Restoration","Ecosystem restoration","Infrastructure"
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
                                                             "Protected area management","River management","River regulation"
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

#first organise data for upset plots

#first data for actors

# Build a single, readable regex for things I want to collapse to "Other"
other_pat <- regex(
  # use groups, plurals and small spelling variants
  "\\b(Indigenous (groups|representatives)|Knowledge[- ]?brokers?|Advisors?|Recrational fishers: coded as practitioners|Mining and agriculture representatives?|Policy analysts?|Science communicators?|Journalists?|Funders?|Grant funders?|Law enforcement)\\b",
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
      str_detect(stakeholder_population, other_pat) ~ "Other actors",
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


#second data for organisations

dput(sys_map_data)

write_csv(sys_map_data,"data/sys_map_data_dput.csv")

#list unique values for organisation types
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
        "recreational fisheries",
        "Indigenous_groups",
        "First_Nations_Communities"
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

#potentially fixed version

Organisation_matrix <- sys_map_data %>%
  select(rayyan_key, type_of_organisation_studied) %>%
  filter(!is.na(type_of_organisation_studied), type_of_organisation_studied != "") %>%
  separate_rows(type_of_organisation_studied, sep = ",\\s*") %>%
  
  # 1) Clean the raw text and create a normalised key for matching
  mutate(
    type_of_organisation_studied = str_squish(type_of_organisation_studied),
    
    # normalised key: lower-case and treat space/_/-// as separators
    org_key = type_of_organisation_studied %>%
      str_to_lower() %>%
      str_replace_all("[_\\-/]+", " ") %>%
      str_squish()
  ) %>%
  
  # 2) Recode to standardised groups
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
  
  # 3) Avoid double-counting: one row per study × org type
  distinct(rayyan_key, org_type_std) %>%
  
  # 4) Pivot to 0/1 incidence matrix
  mutate(mentioned = 1L) %>%
  pivot_wider(
    names_from  = org_type_std,
    values_from = mentioned,
    values_fill = 0,
    values_fn   = ~ as.integer(any(. == 1))
  ) %>%
  
  # 5) Clean column names: trim + spaces/slashes/dashes/dots -> underscores
  rename_with(~ str_trim(.x)) %>%
  rename_with(~ str_replace_all(.x, "[ .\\/-]+", "_")) %>%
  rename_with(~ str_replace_all(.x, "_+", "_")) %>%
  rename_with(~ str_replace_all(.x, "^_|_$", "")) %>%
  rename_with(~ str_to_lower(.x))

names(Organisation_matrix)


#now produce figures for (a) actors focused on in studies and (b) the related organisations

#first for actors
#remove the 'rayyan key' category
actors<-colnames(sys_actor_subset)[c(-1)]
sys_actor_subset_bool <- sys_actor_subset %>%
  mutate(across(-rayyan_key, ~ .x == 1L))%>%
  select(!rayyan_key)

actors_upset<-upset(sys_actor_subset_bool, actors, name='Actor type', width_ratio=0.1,
                    # with manual aes specification:
                    base_annotations=list('Number of studies'=(intersection_size(counts=FALSE))),
                    set_sizes=FALSE)+theme(text=element_text(size=10))

#then for organisations
colnames(Organisation_matrix)
#remove the category "Not_detailed" and the study ID
organisations<-colnames(Organisation_matrix)[c(-1,-5)]
Organisation_matrix_bool <- Organisation_matrix %>%
  mutate(across(-rayyan_key, ~ .x == 1L))

#plot this
organisation_upset<-upset(Organisation_matrix_bool, organisations, name='Organisation type', width_ratio=0.1,
                          base_annotations=list('Number of studies'=(intersection_size(counts=FALSE))),
                          set_sizes=FALSE,
                          labeller=ggplot2::as_labeller(c(
                            'government_statutory_body'='Government/statutory body',
                            'ngo_non_profit'='NGO/non profit',
                            'academic_institution'='Academic institution',
                            "other"="Other"
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


#save plots for BES presentation

#first actors
actors_upset_presentation<-upset(sys_actor_subset_bool, actors, name='Actor type', width_ratio=0.1,
                                 # with manual aes specification:
                                 base_annotations=list('Number of studies'=(intersection_size(counts=FALSE))),
                                 set_sizes=FALSE)+
  theme(text=element_text(size=20),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20))

actors_upset_presentation

ggsave("figures/for_talk/study_actors.png",
       actors_upset_presentation,
       width=20,
       height=15,
       units="cm",
       dpi=300)


#then organisations
organisation_upset_presentation<-upset(Organisation_matrix_bool, organisations, name='Organisation type', width_ratio=0.1,
                                       base_annotations=list('Number of studies'=(intersection_size(counts=FALSE))),
                                       set_sizes=FALSE,
                                       labeller=ggplot2::as_labeller(c(
                                         'Government_statutory_body'='Government/statutory body',
                                         'NGO_non_profit'='NGO/non profit',
                                         'Academic_institution'='Academic institution',
                                         'Community_local_organisation'='Community/local organisation',
                                         "Private_sector"='Private sector'
                                       )))+theme(text=element_text(size=20),
                                                 axis.text=element_text(size=20),
                                                 axis.title=element_text(size=20),
                                                 axis.title.y=element_text(size=20))

organisation_upset_presentation

ggsave("figures/for_talk/study_organisations.png",
       organisation_upset_presentation,
       width=30,
       height=15,
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


mentions_factors_categories%>%
  print(n=100)

#plot this
mentions_factors_categories%>%
  filter(category!="Characteristics of other stakeholders")%>%
  mutate(factors_label = reorder_within(factors_label, perc_mentions, category)) %>%
  ggplot(aes(y = factors_label, perc_mentions), x = perc_mentions) +
  geom_bar(stat="identity") +
  labs(y = "Factor impacting evidence use", x = "Pecentage of studies mentioning factor") +
  theme_bw()+
  facet_wrap(~category, scales = "free_y",ncol=1)+
  force_panelsizes(rows = c(0.3,1,1,0.3))+
  theme(axis.text = element_text(size=10))+
  scale_y_reordered()

#just plot relationships category
relationship_plot<-mentions_factors_categories%>%
  filter(category!="Characteristics of other stakeholders")%>%
  filter(category=="Relationships")%>%
  mutate(factors_label = reorder_within(factors_label, perc_mentions, category)) %>%
  ggplot(aes(y = factors_label, perc_mentions), x = perc_mentions) +
  geom_bar(stat="identity",fill="#FA7876",alpha=0.8) +
  labs(y = "Factor impacting evidence use", x = "Pecentage of studies mentioning factor") +
  theme_cowplot()+
  theme(axis.text = element_text(size=10),
        axis.title=element_blank())+
  scale_y_reordered()+
  scale_x_continuous(limits=c(0,50))

#just plot evidence category
evidence_plot<-mentions_factors_categories%>%
  filter(category!="Characteristics of other stakeholders")%>%
  filter(category=="Characteristics of evidence")%>%
  mutate(factors_label = reorder_within(factors_label, perc_mentions, category)) %>%
  ggplot(aes(y = factors_label, perc_mentions), x = perc_mentions) +
  geom_bar(stat="identity",fill="#4B2991",alpha=0.8) +
  labs(y = "Factor impacting evidence use", x = "Pecentage of studies mentioning factor") +
  theme_cowplot()+
  theme(axis.text = element_text(size=10),
        axis.title=element_blank())+
  scale_y_reordered()+
  scale_x_continuous(limits=c(0,50))

#just plot decision-makers category
actor_plot<-mentions_factors_categories%>%
  filter(category!="Characteristics of other stakeholders")%>%
  filter(category=="Practitioner/policymaker characteristics,\norganisation, and decisions")%>%
  mutate(factors_label = reorder_within(factors_label, perc_mentions, category)) %>%
  ggplot(aes(y = factors_label, perc_mentions), x = perc_mentions) +
  geom_bar(stat="identity",fill="#C0369D",alpha=0.8) +
  labs(y = "Factor impacting evidence use", x = "Pecentage of studies mentioning factor") +
  theme_cowplot()+
  theme(axis.text = element_text(size=10),
        axis.title=element_blank())+
  scale_y_reordered()+
  scale_x_continuous(limits=c(0,50))

#just plot researcher category
researcher_plot<-mentions_factors_categories%>%
  filter(category!="Characteristics of other stakeholders")%>%
  filter(category=="Researcher &\nresearch organisations")%>%
  mutate(factors_label = reorder_within(factors_label, perc_mentions, category)) %>%
  ggplot(aes(y = factors_label, perc_mentions), x = perc_mentions) +
  geom_bar(stat="identity",fill="#EDD9A3",alpha=0.8)+
  labs(y = "Factor impacting evidence use", x = "Pecentage of studies mentioning factor") +
  theme_cowplot()+
  theme(axis.text = element_text(size=10),
        axis.title=element_blank())+
  scale_y_reordered()+
  scale_x_continuous(limits=c(0,50))

#combine these plots into figure 3
combined_factor_plot<-plot_grid(actor_plot,evidence_plot,researcher_plot, relationship_plot,
                                ncol=2,labels=c("a)","b)","c)","d)"),label_size = 10,align = "v",
                                rel_heights = c(1,0.4))

#create common x and y labels

y.grob <- textGrob("Factor impacting evidence use", 
                   gp=gpar(fontface="bold",fontsize=12), rot=90)

x.grob <- textGrob("Pecentage of studies mentioning factor", 
                   gp=gpar(fontface="bold", fontsize=12))

#add to plot

grid.arrange(arrangeGrob(combined_factor_plot, 
                         left = y.grob, bottom = x.grob))

?arrangeGrob

ggsave("figures/figure_3_factors_coloured.png",
       width=20,
       height=18,
       units="cm",
       dpi=300)

#plot figures for BES presentation

#first relationships
relationship_plot_presentation<-relationship_plot+
  facet_wrap(~category)+
  theme(text=element_text(size=20),
        axis.text = element_text(size=18),
        axis.title = element_text(size=22,face="bold"),
        strip.text = element_text(size=28,face="bold"),
        strip.background = element_rect(fill="lightgrey"))+
  scale_x_continuous(limits=c(0,50))

ggsave("figures/for_talk/relationships.png",
       relationship_plot_presentation,
       width=30,
       height=15,
       units="cm",
       dpi=300)

#now evidence characteristics
evidence_plot_presentation<-evidence_plot+
  facet_wrap(~category)+
  theme(text=element_text(size=20),
        axis.text = element_text(size=18),
        axis.title = element_text(size=22,face="bold"),
        strip.text = element_text(size=28,face="bold"),
        strip.background = element_rect(fill="lightgrey"))+
  scale_x_continuous(limits=c(0,50))

ggsave("figures/for_talk/evidence_characteristics.png",
       evidence_plot_presentation,
       width=30,
       height=15,
       units="cm",
       dpi=300)

#now practitioners and organisations
actor_plot_presentation<-actor_plot+
  facet_wrap(~category)+
  theme(text=element_text(size=20),
        axis.text = element_text(size=18),
        axis.title = element_text(size=22,face="bold"),
        strip.text = element_text(size=28,face="bold"),
        strip.background = element_rect(fill="lightgrey"))+
  scale_x_continuous(limits=c(0,50))

ggsave("figures/for_talk/actor_characteristics.png",
       actor_plot_presentation,
       width=31,
       height=16,
       units="cm",
       dpi=300)


#now researcher and research organisations
researcher_plot_presentation<-researcher_plot+
  facet_wrap(~category)+
  theme(text=element_text(size=20),
        axis.text = element_text(size=18),
        axis.title = element_text(size=22,face="bold"),
        strip.text = element_text(size=28,face="bold"),
        strip.background = element_rect(fill="lightgrey"))+
  scale_x_continuous(limits=c(0,50))


ggsave("figures/for_talk/researcher_characteristics.png",
       researcher_plot_presentation,
       width=30,
       height=16,
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


###################################################
#figure 5 - Changes in factors studied over time###
###################################################

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

#now plot this as line plot
ggplot(factors_over_time,aes(x=year,y=n_studies,colour=category))+
  geom_line(size=1,alpha=0.5)+
  geom_point(size=3,alpha=0.5)+
  theme_cowplot()+
  labs(x="Publication year",y="Number of studies")+
  scale_colour_carto_d(palette = "ag_Sunset",name="Factor category")+
  scale_x_continuous(limits = c(min(factors_over_time$year),
                                max(factors_over_time$year)+1))+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12),
        legend.position = "bottom")+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))

#save figure
ggsave("figures/figure_4_factors_over_time.png",
       width=16,
       height=12,
       units="cm",
       dpi=300)

#do the same figure but with cumulative number of studies over years
factors_over_time_cum <- factors_over_time %>%
  group_by(category) %>%
  arrange(year) %>%
  mutate(cum_studies = cumsum(n_studies)) %>%
  ungroup()
#plot this
ggplot(factors_over_time_cum,aes(x=year,y=cum_studies,colour=category))+
  geom_line(size=1,alpha=0.5)+
  geom_point(size=3,alpha=0.5)+
  theme_cowplot()+
  labs(x="Publication year",y="Cumulative number of studies")+
  scale_colour_carto_d(palette = "ag_Sunset",name="Factor category")+
  scale_x_continuous(limits = c(min(factors_over_time_cum$year),
                                max(factors_over_time_cum$year)+1))+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12),
        legend.position = "bottom")+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))

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

##############################################
#extra figures for bes talk###################
##############################################


#load data from bluesky polls
aim_poll<-read.csv("data/bluesky_poll_1.csv")
practice_poll<-read.csv("data/bluesky_poll_2.csv")
head(practice_poll)

#plot this data as barplots
#keeping order of responses the same as in dataframe


ggplot(aim_poll,aes(y = fct_rev(Response), x = Perc)) +
  geom_bar(stat="identity",fill="#1184fd",alpha=0.8) +
  labs(y = "Response", x = "Pecentage of respondants") +
  theme_cowplot()+
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=22,face="bold"))

ggsave("figures/for_talk/bluesky_1.png",
       width=30,
       height=16,
       units="cm",
       dpi=300)

#do the same for the data on evidence use
practice_poll%>%
  mutate(Response=as.factor(Response),
         Response=fct_relevel(Response,"Yes","No","Maybe"))%>%
  ggplot(aes(y = fct_rev(Response), x = Perc)) +
  geom_bar(stat="identity",fill="#1184fd",alpha=0.8) +
  labs(y = "Response", x = "Pecentage of respondants") +
  theme_cowplot()+
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=22,face="bold"))

ggsave("figures/for_talk/bluesky_2.png",
       width=30,
       height=16,
       units="cm",
       dpi=300)
