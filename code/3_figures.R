# this script produces all the figures and analyses for the 
# systematic map manuscript on evidence use in conservation

rm(list=ls())

#first load packages
pacman::p_load(tidyverse,ggtext,ggspatial,cowplot,lemon,ggmap,scales,sf,
               countrycode,rnaturalearth,rnaturalearthdata,rcartocolor,
               tidyr,UpSetR,ComplexUpset,ggplot2,BaseSet,ggh4x,
               tidytext,forcats,grid,gridExtra,grateful,vegan)

#load data
study_loc_counts<-read_csv("data/processed/study_location_counts.csv")
biome_counts<-read_csv("data/processed/study_biome_counts.csv")
con_prob_counts<-read_csv("data/processed/study_con_prob_counts.csv")

########################################################
#1 - Figure 1 - Study context###########################
########################################################

#Figure 1a - map of study locations

#make global map
world<-ne_countries(scale="medium",returnclass = "sf")

#join geocoded data to shapefile
world_map_join<-world%>%
  left_join(study_loc_counts,by = "iso_a3_eh",keep=FALSE)

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
biome_plot<-ggplot(biome_counts,aes(x=reorder(biome_std,perc_studies),y=perc_studies))+
  geom_bar(stat="identity")+
  theme_cowplot()+
  coord_flip()+
  labs(x="Ecosystem type",y="Percentage of studies (%)")+
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

#figure 1c - bar chart with conservation problems addressed in studies
conservation_problem_plot<-ggplot(con_prob_counts,aes(x=reorder(conservation_problem_std,perc_studies),y=perc_studies))+
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
      str_detect(stakeholder_population, other_pat) ~ "Other_actors",
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

#calculate number and percentage of studies that focused on practitioners
sys_actor_subset%>%
  summarise(n_practitioners=sum(Practitioners),
            perc_practitioners=(n_practitioners/167)*100)

#calculate number and percentage of studies that only focused on practitioners
sys_actor_subset%>%
  filter(Practitioners==1)%>%
  mutate(only_practitioners=if_else(rowSums(across(-rayyan_key))==1,1,0))%>%
  summarise(n_only_practitioners=sum(only_practitioners),
            perc_only_practitioners=(n_only_practitioners/167)*100)

#calculate number and percentage of studies that focussed on researchers
sys_actor_subset%>%
  summarise(n_researchers=sum(Researchers),
            perc_Researchers=(n_researchers/167)*100)

#calculate number and percentage of studies that focussed on policymakers
sys_actor_subset%>%
  summarise(n_policymakers=sum(Policymakers),
            perc_policymakers=(n_policymakers/167)*100)

#calculate number and percentage of studies that focussed on policymakers
sys_actor_subset%>%
  summarise(n_other=sum(Other_actors),
            perc_other=(n_other/167)*100)

#count number and percentage of studies with more than one actor type
sys_actor_subset%>%
  mutate(n_actor_types=rowSums(across(-rayyan_key)))%>%
  filter(n_actor_types>1)%>%
  summarise(n_multi_actor_studies=n(),
            perc_multi_actor_studies=(n_multi_actor_studies/167)*100)

#count number and percentage of studies with only practitioners and researchers as actors
sys_actor_subset%>%
  filter(Practitioners==1,Researchers==1)%>%
  mutate(only_prac_research=if_else(rowSums(across(-rayyan_key))==2,1,0))%>%
  summarise(n_pract_researchers=sum(only_prac_research),
            perc_pract_researchers=(n_pract_researchers/167)*100)

#count number and percentage of studies with only practitioners, researchers, and policymakers as actors
sys_actor_subset%>%
  filter(Practitioners==1,Researchers==1,Policymakers==1)%>%
  mutate(only_prac_research_policy=if_else(rowSums(across(-rayyan_key))==3,1,0))%>%
  summarise(n_pract_researchers=sum(only_prac_research_policy),
            perc_pract_researchers=(n_pract_researchers/167)*100)

#count number and percentage of studies with only researchers and policymakers as actors
sys_actor_subset%>%
  filter(Researchers==1,Policymakers==1)%>%
  mutate(only_prac_research_policy=if_else(rowSums(across(-rayyan_key))==2,1,0))%>%
  summarise(n_pract_researchers=sum(only_prac_research_policy),
            perc_pract_researchers=(n_pract_researchers/167)*100)

#second data for organisations

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
inte<-colnames(sys_actor_subset)[c(-1)]
head(sys_actor_subset_bool)
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
                          )),
                          themes=upset_modify_themes(
                            list(
                              "default"=theme(
                                text=element_text(size=10),
                                axis.line = element_line(colour = "black"),
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.border = element_blank(),
                                panel.background = element_blank(),
                                plot.margin = margin(0, 0, 0, 0, "cm"),
                                
                            )))
                          )
organisation_upset

names(upset_themes)

#join these plots together for figure 2
plot_grid(actors_upset,organisation_upset,ncol=2,labels=c("(a)","(b)"),
          label_size = 12,rel_widths = c(1.3,2),align = "h")
#save plot
ggsave("figures/figure_2_actors_organisations.png",
       width=20,
       height=12,
       units="cm",
       dpi=300)


########################################################################
##Figure 3 - Factors influencing evidence use ##########################
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
                   gp=gpar(fontface="bold",fontsize=14), rot=90)

x.grob <- textGrob("                             Pecentage of studies mentioning factor", 
                   gp=gpar(fontface="bold", fontsize=14))

#add to plot

combined_factor_plot_with_axes<-grid.arrange(arrangeGrob(combined_factor_plot, 
                                                         left = y.grob, bottom = x.grob))


ggsave("figures/figure_3_factors_coloured.png",
       combined_factor_plot_with_axes,
       width=20,
       height=14,
       units="cm",
       dpi=300)


###################################################
#figure 4 - Changes in factors studied over time###
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

#cumulative number of studies over years
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
  scale_x_continuous(limits = c(min(factors_over_time_cum$year)-1,
                                max(factors_over_time_cum$year)+1),
                     breaks=c(2000,2005,2010,2015,2020,2025))+
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

#calculate the number of studies per year for each of the four main categories
factors_over_time_summary <- clean_factors_data_studies %>%
  filter(category != "Characteristics of other stakeholders") %>%
  mutate(category = fct_drop(category)) %>%
  group_by(year, category) %>%
  summarise(n_studies = n_distinct(rayyan_key), .groups = "drop") %>%
  complete(
    year = full_seq(range(year, na.rm = TRUE), 1),
    category = unique(category),
    fill = list(n_studies = 0)
  ) %>%
  arrange(year, category)%>%
  group_by(category)%>%
  summarise(total_studies=sum(n_studies/21))%>%
  arrange(desc(total_studies))


##################################################
#Supplementary figures and summary stats##########
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
  summarise(perc_studies=(n()/nrow(sys_map_data))*100,
            no_studies=n())%>%
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
  summarise(perc_studies=(n()/nrow(sys_map_data))*100,
            n_studies=n())%>%
  ungroup()%>%
  arrange(desc(perc_studies))

#plot discipline data
discipline_plot<-ggplot(sys_discipline_summary,aes(x=reorder(evidence_discipline,perc_studies),y=perc_studies))+
  geom_bar(stat="identity")+
  theme_cowplot()+
  coord_flip()+
  labs(x="Discipline of scientific evidence",y="Percentage of studies (%)")+
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

#save plot
ggsave("figures/figure_S1.png",plot=discipline_plot,width=20,height=12,units="cm",dpi=300)

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

#calculate mean and median number of factors per study using tidyverse
#and standard deviation
factors_per_study%>%
  summarise(mean_factors=mean(n_factors),
            median_factors=median(n_factors),
            sd_factors=sd(n_factors))

#calculate the percentage of studies that identified more than 10 factors
factors_per_study%>%
  filter(n_factors>=10)%>%
  summarise(perc_studies=(n()/nrow(sys_map_data))*100)

#calculate the percentage of studies that identified fewer than 3 factors
factors_per_study%>%
  filter(n_factors<3)%>%
  summarise(perc_studies=(n()/nrow(sys_map_data))*100)

#now focus on the scale of studies

#calculate the percentage of studies for each category of spatial scale
#first remove any blank rows
sys_scale_subset<-sys_map_data%>%
  select(scale_of_study)%>%
  filter(!is.na(scale_of_study))

#calculate percentage of studies for each category of spatial scale
sys_scale_subset%>%
  group_by(scale_of_study)%>%
  summarise(perc_studies=(n()/nrow(sys_map_data))*100)
  
#now focus on the scale of decision-making
#calculate the percentage of studies for each category of decision-making scale
#first remove any blank rows
sys_map_data%>%
  group_by(scale_of_decision_making)%>%
  summarise(perc_studies=(n()/nrow(sys_map_data))*100)

  