#this script calculates descriptive statistics for the systematic map

rm(list=ls())

#first load packages
pacman::p_load(tidyverse)

#load data
cleaned_data<-read_csv("data/processed/cleaned_data.csv")
location_count<-read_csv("data/processed/study_location_counts.csv")
actor_data<-read_csv("data/processed/study_actor_counts.csv")
factors_over_time<-read_csv("data/processed/factors_over_time_cumulative.csv")
organisation_data<-read_csv("data/processed/study_organisation_counts.csv")
sys_map_data<-read.csv("data/raw/extracted_data.csv")

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
  mutate(perc_studies=(n_studies/474)*100)|>
  print(n=10)

#calculate the mean number of biomes per study
cleaned_data|>
  filter(!is.na(biome), biome != "") |>
  separate_rows(biome, sep = ",\\s*") |>
  filter(biome!="Not reported",biome!="not reported",biome!="Not mentioned")|>
  group_by(rayyan_key)|>
  summarise(n_biomes=n())|>
  ungroup()|>
  summarise(mean_biomes=mean(n_biomes),
            sd_biomes=sd(n_biomes))

#calculate number and percentage of studies that focused on practitioners
actor_data|>
  summarise(n_practitioners=sum(Practitioners),
            perc_practitioners=(n_practitioners/169)*100)

#calculate number and percentage of studies that only focused on practitioners
actor_data|>
  filter(Practitioners==1)|>
  mutate(only_practitioners=if_else(rowSums(across(-rayyan_key))==1,1,0))|>
  summarise(n_only_practitioners=sum(only_practitioners),
            perc_only_practitioners=(n_only_practitioners/167)*100)

#calculate number and percentage of studies that focussed on researchers
actor_data|>
  summarise(n_researchers=sum(Researchers),
            perc_Researchers=(n_researchers/169)*100)

#calculate number and percentage of studies that focussed on policymakers
actor_data|>
  summarise(n_policymakers=sum(Policymakers),
            perc_policymakers=(n_policymakers/169)*100)

#calculate number and percentage of studies that focussed on other actors
actor_data|>
  summarise(n_other=sum(Other_actors),
            perc_other=(n_other/169)*100)

#count number and percentage of studies with more than one actor type
actor_data|>
  mutate(n_actor_types=rowSums(across(-rayyan_key)))|>
  filter(n_actor_types>1)|>
  summarise(n_multi_actor_studies=n(),
            perc_multi_actor_studies=(n_multi_actor_studies/169)*100)

#count number and percentage of studies with only practitioners and researchers as actors
actor_data|>
  filter(Practitioners==1,Researchers==1)|>
  mutate(only_prac_research=if_else(rowSums(across(-rayyan_key))==2,1,0))|>
  summarise(n_pract_researchers=sum(only_prac_research),
            perc_pract_researchers=(n_pract_researchers/169)*100)

#count number and percentage of studies with only practitioners, researchers, and policymakers as actors
actor_data|>
  filter(Practitioners==1,Researchers==1,Policymakers==1)|>
  mutate(only_prac_research_policy=if_else(rowSums(across(-rayyan_key))==3,1,0))|>
  summarise(n_pract_researchers=sum(only_prac_research_policy),
            perc_pract_researchers=(n_pract_researchers/169)*100)

#count number and percentage of studies with only researchers and policymakers as actors
actor_data|>
  filter(Researchers==1,Policymakers==1)|>
  mutate(only_research_policy=if_else(rowSums(across(-rayyan_key))==2,1,0))|>
  summarise(n_policy_researchers=sum(only_research_policy),
            perc_policy_researchers=(n_policy_researchers/169)*100)


#calculate the number and percentage of studies that focus on the different organisation types
organisation_data|>
  summarise(across(government_statutory_body:ngo_non_profit,sum))|>
  pivot_longer(government_statutory_body:ngo_non_profit)|>
  mutate(perc=(value/169)*100)

#count number of factors identified per study
factors_per_study <- cleaned_data %>%
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
  #remove duplicate factors
  distinct(factors_influencing_evidence_use, .keep_all = TRUE) %>%
  summarise(n_factors = n(), .groups = "drop")

#calculate mean and median number of factors per study using tidyverse
#and standard deviation and standard error
factors_per_study%>%
  summarise(mean_factors=mean(n_factors),
            median_factors=median(n_factors),
            sd_factors=sd(n_factors),
            se_factors=sd(n_factors)/sqrt(n()))

#calculate the percentage of studies that identified more than 10 factors
factors_per_study%>%
  filter(n_factors>=10)%>%
  summarise(perc_studies=((n()/169))*100)

#calculate the percentage of studies that identified fewer than 3 factors
factors_per_study%>%
  filter(n_factors<3)%>%
  summarise(perc_studies=((n()/169))*100)


#now focus on the scale of studies

#calculate the percentage of studies for each category of spatial scale
#first remove any blank rows

sys_scale_subset<-sys_map_data%>%
  select(Scale.of.study)

#calculate percentage of studies for each category of spatial scale
sys_scale_subset%>%
  group_by(Scale.of.study)%>%
  summarise(perc_studies=(n()/nrow(sys_scale_subset))*100,
            n_studies=n())


#now focus on the scale of decision-making
#calculate the percentage of studies for each category of decision-making scale
#first remove any blank rows
sys_map_data%>%
  group_by(Scale.of.decision.making)%>%
  summarise(perc_studies=(n()/nrow(sys_map_data))*100,
            n_studies=n())
