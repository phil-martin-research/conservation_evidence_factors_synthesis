#this script is used to scope searches for study on the factors
#impacting evidence use in conservation decision-making

#first load packages
pacman::p_load(revtools, tidyverse, synthesisr)

#import data from scopus searches

#import all studies
scopus_search<- read_bibliography("data/scoping_searches/scopus_1_zotero.ris")
#randomly select 500 publications
rand_pubs<-scopus_search[sample(nrow(scopus_search), 500), ]
#save randomly selected papers
write_bibliography(rand_pubs,
                   filename = "data/scoping_searches/scoping_search_2/random_selection.ris",
                   format = "ris")

#load in random publications
library("revtools")
rand_papers<-read_bibliography(c("data/scoping_searches/scoping_search_2/random_selection.ris"))

screen_titles(rand_papers)

#load in papers after title screening
post_title<-read.csv("post_title_screening.csv")
included_titles<-post_title%>%filter(screened_titles!="excluded")
#screen abstracts
screen_abstracts(included_titles)
