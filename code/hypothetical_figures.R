#this script is for producing dummy plots to show potential figures for the manuscript

#load packages
pacman::p_load(tidyverse,cowplot,ggbeeswarm)


#create data showing showing the factors that influence evidence use for 
#a number of different studies

#set seed
set.seed(123)

#generate data
evidence_data<-data.frame(factor=c("Existence","Accessibility","Relevance","Quality/credibility",
                     "Relationship between\nscientists and practitioners",
                     "Relationships between colleagues",
                     "Type of decision maker","Nature of decision","Capacity/resources","Personal characteristics")) %>%
  mutate(evidence_use=rnorm(10,0.5,0.2),
         error=rnorm(10,0.1,0.05)) 


#produce error bar figure of this data
  ggplot(evidence_data,aes(x=factor,y=evidence_use,colour=factor))+
  geom_errorbar(aes(ymin=evidence_use-error,
                    ymax=evidence_use+error),
                width=0.2)+
  geom_point(size=3)+
  theme_cowplot()+
  labs(x="Factor impacting evidence use",
         y="Probability of factor\n being mentioned")+
  scale_colour_viridis_d(begin = 0.1,end = 0.9,option="C")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

#save plot
ggsave("figures/hypothetical_evidence_use_factors.png",width=25,height=15,dpi=300,units="cm")

#produce data to show differences between NGOs and government agencies

#set seed
set.seed(123)

#generate data
context_data<-expand.grid(organisation=c("NGO","Government agency"),factor=c("Accessibility","Capacity/resources")) %>%
  mutate(evidence_use=c(0.82,0.6,0.9,0.6),
         error=rnorm(4,0.1,0.05))

#plot this
ggplot(context_data,aes(x=organisation,y=evidence_use,colour=factor))+
  geom_errorbar(aes(ymin=evidence_use-error,
                    ymax=evidence_use+error),
                width=0.2)+
  geom_point(size=3)+
  theme_cowplot()+
  labs(x="Organisation type",
         y="Probability of factor\n being mentioned")+
  scale_colour_viridis_d(begin = 0.1,end = 0.5,option="C")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")+
  facet_wrap(~factor)

#save plot
ggsave("figures/hypothetical_evidence_context.png",width=25,height=15,dpi=300,units="cm")

