# this script produces all the figures and analyses for the 
# systematic map manuscript on evidence use in conservation

rm(list=ls())

#first load packages
pacman::p_load(tidyverse,ggtext,ggspatial,cowplot,lemon,ggmap,scales,sf,
               countrycode,rnaturalearth,rnaturalearthdata,rcartocolor,
               tidyr,ComplexUpset,BaseSet,ggh4x,
               tidytext,forcats,grid,gridExtra,grateful,vegan,patchwork)

#load data
sys_map_data<-read.csv("data/raw/extracted_data.csv")
study_loc_counts<-read_csv("data/processed/study_location_counts.csv")
biome_counts<-read_csv("data/processed/study_biome_counts.csv")
con_prob_counts<-read_csv("data/processed/study_con_prob_counts.csv")
actor_data<-read_csv("data/processed/study_actor_counts.csv")
organisation_data<-read_csv("data/processed/study_organisation_counts.csv")
mentions_factors_categories<-read_csv("data/processed/factors_influencing_evidence_use.csv")
factors_over_time<-read_csv("data/processed/factors_over_time_cumulative.csv")

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
  scale_fill_carto_c(palette = "Purp",name="No. of studies",breaks=c(0,10,20,30,40,50))

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

#now produce figures for (a) actors focused on in studies and (b) the related organisations

#first for actors
#remove the 'rayyan key' category
inte<-colnames(actor_data)[c(-1)]
sys_actor_subset_bool <- actor_data %>%
  mutate(across(-rayyan_key, ~ .x == 1L))%>%
  select(!rayyan_key)
actors<-colnames(sys_actor_subset_bool)


actors_upset<-upset(sys_actor_subset_bool, actors, name='Actor type', width_ratio=0.1,
                    # with manual aes specification:
                    base_annotations = list("Number of studies" =
                      intersection_size(counts = FALSE) +
                      scale_y_continuous(expand = expansion(mult = c(0, 0.05)))),
                    set_sizes=FALSE,
                    themes=upset_modify_themes(
                      list(
                        "default"=theme(
                          text=element_text(size=10),
                          axis.line = element_line(colour = "black"),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank(),
                          plot.margin = margin(0, 0, 0, 0, "cm")
                        ))))

#then for organisations

#remove the category "Not_detailed" and the study ID
organisations<-colnames(organisation_data)[c(-1,-5)]
Organisation_matrix_bool <- organisation_data %>%
  mutate(across(-rayyan_key, ~ .x == 1L))

#plot this
organisation_upset<-upset(Organisation_matrix_bool, organisations, 
                          name='Organisation type', width_ratio=0.1,
                          base_annotations = list("Number of studies" =
                              intersection_size(counts = FALSE) +
                              scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
                                theme(axis.title.y = element_text(margin = margin(r = -20, unit = "pt")))),
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
                                plot.margin = margin(0, 0, 0, 0, "cm")
                            )))
                          )

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

#plot data for factors influencing evidence use

mentions_factors_categories

#just plot relationships category
relationship_plot<-mentions_factors_categories%>%
  filter(category!="Characteristics of other stakeholders")%>%
  filter(category=="Relationships")%>%
  mutate(factors_label = reorder_within(factors_label, perc_mentions, category)) %>%
  ggplot(aes(y = factors_label, x = perc_mentions)) +
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
  ggplot(aes(y = factors_label, x = perc_mentions)) +
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
  filter(category=="Practitioner/policymaker characteristics,\r\norganisation, and decisions")%>%
  mutate(factors_label = reorder_within(factors_label, perc_mentions, category)) %>%
  ggplot(aes(y = factors_label, x = perc_mentions)) +
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
  filter(category=="Researcher &\r\nresearch organisations")%>%
  mutate(factors_label = reorder_within(factors_label, perc_mentions, category)) %>%
  ggplot(aes(y = factors_label, x = perc_mentions)) +
  geom_bar(stat="identity",fill="#EDD9A3",alpha=0.8)+
  labs(y = "Factor impacting evidence use", x = "Pecentage of studies mentioning factor") +
  theme_cowplot()+
  theme(axis.text = element_text(size=10),
        axis.title=element_blank())+
  scale_y_reordered()+
  scale_x_continuous(limits=c(0,50))

#combine these plots into figure 3
combined_factor_plot<-plot_grid(actor_plot,evidence_plot,researcher_plot, relationship_plot,
                                ncol=2,labels=c("(a)","(b)","(c)","(d)"),label_size = 10,align = "v",
                                rel_heights = c(1,0.4))

#create common x and y labels

y.grob <- textGrob("Factor impacting evidence use", 
                   gp=gpar(fontface="bold",fontsize=14), rot=90)

x.grob <- textGrob("Pecentage of studies mentioning factor", 
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

#plot this
ggplot(data=factors_over_time,aes(x=year,y=cum_studies,colour=category))+
  geom_line(linewidth=1,alpha=0.5)+
  geom_point(size=3,alpha=0.5)+
  theme_cowplot()+
  labs(x="Publication year",y="Cumulative number of studies")+
  scale_colour_carto_d(palette = "ag_Sunset",name="Factor category")+
  scale_x_continuous(limits = c(min(factors_over_time$year)-1,
                                max(factors_over_time$year)+1),
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



##################################################
#Supplementary figures and summary stats##########
##################################################

#figure S2 - bar chart with the different types of discipline considered

sys_discipline_subset<-sys_map_data%>%
  select(Evidence.discipline)%>%
  filter(!is.na(Evidence.discipline))

#calculate percentage of studies per type of discipline considered
sys_discipline_summary<-sys_discipline_subset%>%
  separate_longer_delim(Evidence.discipline,delim=", ")%>%
  group_by(Evidence.discipline)%>%
  summarise(perc_studies=(n()/nrow(sys_map_data))*100,
            n_studies=n())%>%
  ungroup()%>%
  arrange(desc(perc_studies))

#plot discipline data
discipline_plot<-ggplot(sys_discipline_summary,aes(x=reorder(Evidence.discipline,perc_studies),y=perc_studies))+
  geom_bar(stat="identity")+
  theme_cowplot()+
  coord_flip()+
  labs(x="Discipline of scientific evidence",y="Percentage of studies (%)")+
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))

#save plot
ggsave("figures/figure_S2_discipline_type.png",plot=discipline_plot,width=20,height=12,units="cm",dpi=300)


  