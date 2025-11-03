#this script counts the number of articles screened by each of the different users

#load packages
pacman::p_load(tidyverse,lubridate,psych,cowplot)

#load data
screening<-read.csv("data/articles_screened_2025_08_23.csv")

#clean up screening data ready for analysis
screening_clean<-screening%>%
  filter(key=="included")%>%
  #change user emails to names
  mutate(user_name=fct_recode(user_email,
                              "Phil"="phil.martin.research@gmail.com",
                              "Prishnee"="prishnee.bissessur1@gmail.com",
                              "Alice"="alice.m.oswald@durham.ac.uk",
                              "Tiff"="tltk2@cam.ac.uk",
                              "Tabitha"="tabitha.taberer@jesus.ox.ac.uk",
                              "Carlos"="carlos.barreto@algomau.ca",
                              "Elina"="elina.takola@ufz.de",
                              "Catia"="c.matos@hull.ac.uk",
                              "Aidan"="ake@ceh.ac.uk",
                              "Matt"="matthew.grainger@nina.no",
                              "Fereshteh"="amirmohammadif@gmail.com",
                              "Iris"="ib451@cam.ac.uk",
                              "Jamie"="jhartup45@gmail.com",
                              "Santiago"="santip1320@gmail.com",
                              "Valentin"="valentin.moser@wsl.ch",
                              "Ian"="ian.thornhill@manchester.ac.uk",
                              "Sarah"="sarah.luke@nottingham.ac.uk",
                              "Isa"="isa.donoso@bc3research.org",
                              "Nibu"="nibedita.41282@gmail.com",
                              "Isobel"="i.ollard@jbs.cam.ac.uk",
                              "Alec"="achristi@ic.ac.uk",
                              "Nick"="nick.littlewood@sruc.ac.uk",
                              "Alvaro"="alvaro.moreno@bc3research.org",
                              "Eñaut"="enaut.martinezdebirgara@bc3research.org"),
         #convert string to date and time
         date_time=ymd_hms(created_at, tz = "UTC"))%>%
  #select row with latest date for each article for each reviewer
  group_by(article_id,user_name)%>%
  filter(date_time==max(date_time))%>%
  summarise(included=value)%>%
  #change maybes to inclusions
  mutate(included=if_else(included=="0","1",included))%>%
  #ungroup the data
  ungroup()

#count number of articles screened by each user
total_screened<-screening_clean%>%
  group_by(user_name)%>%
  summarise(n_articles=n())%>%
  mutate(n_articles=if_else(user_name=="Phil",n_articles-13852,n_articles))%>%
  arrange(desc(n_articles))%>%
  print(n=30)
#for the user Phil, adjust the number of articles he screened to account for automated screening


#plot this
ggplot(total_screened,aes(x=reorder(user_name,n_articles),y=n_articles))+
  geom_bar(stat="identity",fill="steelblue")+
  theme_cowplot()+
  labs(x="User",
       y="Number of articles screened")+
  coord_flip()
