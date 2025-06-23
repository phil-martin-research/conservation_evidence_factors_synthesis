#this script is for calculating inter-reviewer agreement for screening of titles and abstracts

#load packages
pacman::p_load(tidyverse,lubridate,psych,cowplot)

#load data
screening<-read.csv("data/customizations_log_25_04_15.csv")

#filter just to include the articles I screened

unique(screening$user_email)

screening_agreement<-screening%>%
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
  #spread to wide format
  spread(user_name,included)%>%
  ungroup()


#calculate inter-reviewer agreement  

# Define reviewers
reviewers <- c("Aidan","Alec" ,"Alice", "Carlos", "Catia", "Elina", "Fereshteh", "Ian", "Isa", "Isobel", "Iris", 
               "Jamie", "Matt", "Nibu","Prishnee", "Santiago", "Sarah", "Tabitha", "Tiff", "Valentin","Nick",
               "Alvaro","Eñaut")

# Compute kappa statistics
kappa_df <- tibble(reviewer = reviewers) %>%
  mutate(
    kappa_results = map(reviewer, ~ cohen.kappa(cbind(screening_agreement$Phil, screening_agreement[[.x]]))),
    kappa = map_dbl(kappa_results, "kappa"),
    n_comparisons = map_dbl(kappa_results, "n.obs")
  ) %>%
  select(-kappa_results)%>%# Remove the list column
  print(n=Inf)

kappa_df%>%
  filter(n_comparisons>1)%>%
  ggplot(aes(x=kappa))+
  geom_histogram(binwidth = 0.025,fill="skyblue",color="black")+
  geom_vline(xintercept = 0.6, linetype = "dashed",size=1)+
  scale_y_continuous(expand=c(0,0,0,0),"No. of reviewers")+
  xlab("Cohen's Kappa")+
  theme_cowplot()

ggsave("figures/inter_reviewer_agreement.png",width=18,height=10,dpi=300,units="cm")

kappa_df%>%
  print(n=Inf)


#calculate mean Kappa value
kappa_df%>%
  filter(n_comparisons>1)%>%
  summarise(mean_kappa=mean(kappa))


##########################################################
#calculate agreement for first screening with Fereshteh###
##########################################################

#load data
screening_fereshteh<-read.csv("data/Fereshteh_agreement_first_round.csv")

#filter just to include the articles I screened

screening_agreement_fereshteh<-screening_fereshteh%>%
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
                              "Isa"="isa.donoso@bc3research.org"),
         #convert string to date and time
         date_time=ymd_hms(created_at, tz = "UTC"))%>%
  #select row with latest date for each article for each reviewer
  group_by(article_id,user_name)%>%
  filter(date_time==max(date_time))%>%
  summarise(included=value)%>%
  #change maybes to inclusions
  mutate(included=if_else(included=="0","1",included))%>%
  #spread to wide format
  spread(user_name,included)%>%
  ungroup()%>%
  select(article_id,Phil,Fereshteh)%>%
  filter(!is.na(Fereshteh))%>%
  filter(!is.na(Phil))


#calculate inter-reviewer agreement  

# Define reviewers
reviewers <- c("Fereshteh")

# Compute kappa statistics
kappa_df <- tibble(reviewer = reviewers) %>%
  mutate(
    kappa_results = map(reviewer, ~ cohen.kappa(cbind(screening_agreement_fereshteh$Phil, screening_agreement_fereshteh[[.x]]))),
    kappa = map_dbl(kappa_results, "kappa"),
    n_comparisons = map_dbl(kappa_results, "n.obs")
  ) %>%
  select(-kappa_results)  # Remove the list column

##########################################################
#calculate agreement for first screening with Santiago###
##########################################################

#load data
screening_santiago<-read.csv("data/Santiago_agreement_first_round.csv")

#filter just to include the articles I screened

screening_agreement_santiago<-screening_santiago%>%
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
                              "Isa"="isa.donoso@bc3research.org"),
         #convert string to date and time
         date_time=ymd_hms(created_at, tz = "UTC"))%>%
  #select row with latest date for each article for each reviewer
  group_by(article_id,user_name)%>%
  filter(date_time==max(date_time))%>%
  summarise(included=value)%>%
  #change maybes to inclusions
  mutate(included=if_else(included=="0","1",included))%>%
  #spread to wide format
  spread(user_name,included)%>%
  ungroup()%>%
  select(article_id,Phil,Santiago)%>%
  filter(!is.na(Santiago))%>%
  filter(!is.na(Phil))


#calculate inter-reviewer agreement  

# Define reviewers
reviewers <- c("Santiago")

# Compute kappa statistics
kappa_df <- tibble(reviewer = reviewers) %>%
  mutate(
    kappa_results = map(reviewer, ~ cohen.kappa(cbind(screening_agreement_santiago$Phil, screening_agreement_santiago[[.x]]))),
    kappa = map_dbl(kappa_results, "kappa"),
    n_comparisons = map_dbl(kappa_results, "n.obs")
  ) %>%
  select(-kappa_results)  # Remove the list column


######################################################
#calculate agreement for second screening with Tiff###
######################################################

#load data
screening_tiff<-read.csv("data/Tiff_agreement_second_round.csv")

#filter just to include the articles I screened

screening_agreement_tiff<-screening_tiff%>%
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
                              "Isa"="isa.donoso@bc3research.org"),
         #convert string to date and time
         date_time=ymd_hms(created_at, tz = "UTC"))%>%
  #select row with latest date for each article for each reviewer
  group_by(article_id,user_name)%>%
  filter(date_time==max(date_time))%>%
  summarise(included=value)%>%
  #change maybes to inclusions
  mutate(included=if_else(included=="0","1",included))%>%
  #spread to wide format
  spread(user_name,included)%>%
  ungroup()%>%
  select(article_id,Phil,Tiff)


#calculate inter-reviewer agreement  

# Define reviewers
reviewers <- c("Tiff")

# Compute kappa statistics
kappa_df <- tibble(reviewer = reviewers) %>%
  mutate(
    kappa_results = map(reviewer, ~ cohen.kappa(cbind(screening_agreement_tiff$Phil, screening_agreement_tiff[[.x]]))),
    kappa = map_dbl(kappa_results, "kappa"),
    n_comparisons = map_dbl(kappa_results, "n.obs")
  ) %>%
  select(-kappa_results)  # Remove the list column

####################################################
#check agreement at second round####################
####################################################

# A named vector to recode all user emails at once
email_lookup <- c(
  "phil.martin.research@gmail.com" = "Phil",
  "prishnee.bissessur1@gmail.com" = "Prishnee",
  "alice.m.oswald@durham.ac.uk"    = "Alice",
  "tltk2@cam.ac.uk"                = "Tiff",
  "tabitha.taberer@jesus.ox.ac.uk" = "Tabitha",
  "carlos.barreto@algomau.ca"      = "Carlos",
  "elina.takola@ufz.de"            = "Elina",
  "c.matos@hull.ac.uk"             = "Catia",
  "ake@ceh.ac.uk"                  = "Aidan",
  "matthew.grainger@nina.no"       = "Matt",
  "amirmohammadif@gmail.com"       = "Fereshteh",
  "ib451@cam.ac.uk"                = "Iris",
  "jhartup45@gmail.com"            = "Jamie",
  "santip1320@gmail.com"           = "Santiago",
  "valentin.moser@wsl.ch"          = "Valentin",
  "ian.thornhill@manchester.ac.uk" = "Ian",
  "sarah.luke@nottingham.ac.uk"    = "Sarah",
  "isa.donoso@bc3research.org"     = "Isa",
  "nick.littlewood@sruc.ac.uk"     = "Nick",
  "alvaro.moreno@bc3research.org"  = "Alvaro",
  "enaut.martinezdebirgara@bc3research.org" = "Eñaut",
  "nibedita.41282@gmail.com"       = "Nibu",
  "i.ollard@jbs.cam.ac.uk"         = "Isobel",
  "achristi@ic.ac.uk"              = "Alec"
)


calc_agreement <- function(csv_file, reviewer_name) {
  # Read in the file
  df <- read_csv(csv_file, show_col_types = FALSE) %>%
    filter(key == "included") %>%
    # Recode emails to names, convert timestamps, and keep only the last decision
    mutate(
      user = recode(user_email, !!!email_lookup),
      date_time = ymd_hms(created_at, tz = "UTC")
    ) %>%
    group_by(article_id, user) %>%
    slice_max(date_time, n = 1) %>%  # Keep only latest decision per article/user
    ungroup() %>%
    mutate(
      included = if_else(value == "0", "1", value)  # Reverse 0 to 1 for agreement
    ) %>%
    select(article_id, user, included) %>%
    pivot_wider(names_from = user, values_from = included)
  
  # Ensure the necessary columns are present
  required_cols <- c("Phil", reviewer_name)
  if (!all(required_cols %in% names(df))) {
    stop(glue::glue(
      "One or both required columns ('Phil' and '{reviewer_name}') not found in the data frame.\n",
      "Check if recoding worked and both reviewers have included values in this file."
    ))
  }
  
  # Filter to rows where both Phil and reviewer made a decision
  df <- df %>% filter(!is.na(.data$Phil), !is.na(.data[[reviewer_name]]))
  
  # Convert included/excluded to numeric: 1 = included, -1 = excluded
  df <- df %>%
    mutate(
      Phil = if_else(Phil == "1", 1, -1),
      !!reviewer_name := if_else(.data[[reviewer_name]] == "1", 1, -1)
    )
  
  # Calculate Cohen's kappa
  kappa_data <- df %>% select(Phil, all_of(reviewer_name))
  ck <- cohen.kappa(as.matrix(df %>% select(Phil, !!sym(reviewer_name))))
  
  # Summary stats
  tibble(
    reviewer       = reviewer_name,
    kappa          = ck$kappa,
    percent_agreement = 100 * mean(df$Phil == df[[reviewer_name]]),
    n_comparisons  = ck$n.obs,
    both_included  = sum(df$Phil == 1 & df[[reviewer_name]] == 1),
    only_phil      = sum(df$Phil == 1 & df[[reviewer_name]] == -1),
    only_other     = sum(df$Phil == -1 & df[[reviewer_name]] == 1),
    both_excluded  = sum(df$Phil == -1 & df[[reviewer_name]] == -1)
  )
}

# List of reviewers and their corresponding files
reviewers <- c(
  Tiff      = "data/Tiff_agreement_second_round.csv",
  Prishnee  = "data/Prishnee_agreement_second_round.csv",
  Iris      = "data/Iris_agreement_second_round.csv",
  Sarah     = "data/Sarah_agreement_second_round.csv",
  Carlos    = "data/Carlos_agreement_second_round.csv",
  Nick      = "data/Nick_agreement_third_round.csv",
  Alvaro    = "data/Alvaro_agreement_second_round.csv",
  Nibu      = "data/Nibu_agreement_second_round.csv",
  Isa       = "data/Isa_agreement_third_round.csv",
  Eñaut     = "data/Enaut_agreement_second_round.csv",
  Ian       = "data/Ian_agreement_second_round.csv",
  Jamie     = "data/Jamie_agreement_second_round.csv",
  Santiago  = "data/Santiago_agreement_second_round.csv",
  Fereshteh = "data/Fereshteh_agreement_second_round.csv",
  Aidan     = "data/Aidan_agreement_second_round.csv"
)

agreement_results <- map2(
  .x = reviewers,
  .y = names(reviewers),
  .f = ~calc_agreement(csv_file = .x, reviewer_name = .y)
) |> list_rbind()

print(agreement_results)


