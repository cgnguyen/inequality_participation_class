####Setup####

  #Install Packages 
  install.packages("tidyverse")
  install.packtes("essurvey")
  install.packages("car")
  install.packages("haven")
  
  #Activate Library 
  library(tidyverse)
  library(essurvey)
  library(car)
  library(haven)

  library(ViewPipeSteps)

  
####Data into R####
   #Set your own email with set_email("")
  source("my_email.R")

  
  DATA<-import_rounds(
    rounds=7,
    format='stata')  
  
  
    
####Exploring Data####
  #External Efficacy
  DATA$psppipl
  
  #Internal Efficacy
  DATA$cptppol
  
  
  DATA %>%
    select(psppipl,cptppol) %>%
    summary(.)
  
####Rename Data####
  DATA$eff_ext<-DATA$psppipl
  
  DATA$eff_int<-DATA$cptppol
  
  DATA %>%
    select(eff_int,eff_ext) %>%
    summary(.)
  
####Clean Data####
  DATA$country<-as_factor(DATA$cntry)
  summary(DATA$country)
  
  DATA$gndr
  DATA$gender<-as_factor(DATA$gndr)
  
####Descriptive Statistics by groups####
  
  #Do men and women have different average levels of internal and external efficacy 
  
  DATA %>%
    group_by(gender) %>%
    summarize(eff_ext_mean = mean(eff_ext, na.rm = T),
              eff_int_mean =mean(eff_int, na.rm=T))
  
  DATA %>% 
    group_by(country) %>%
    summarize(eff_ext_mean = mean(eff_ext, na.rm = T),
              eff_int_mean =mean(eff_int, na.rm=T)) %>%
    arrange(-eff_ext_mean)%>%
    print(n=50)
  
  
  DATA %>% 
    group_by(country,gender) %>%
    summarize(eff_ext_mean = mean(eff_ext, na.rm = T),
              eff_int_mean =mean(eff_int, na.rm=T)) %>%
    print(n=50)
  
  
  DATA %>% 
    group_by(country,gender) %>%
    summarize(eff_ext_mean = mean(eff_ext, na.rm = T),
              eff_int_mean =mean(eff_int, na.rm=T))  %>%
    pivot_wider(names_from = gender, values_from = c(eff_ext_mean, eff_int_mean)) %>%
    summarize(int_gap = eff_int_mean_Male - eff_int_mean_Female) %>%
    arrange(-int_gap) %>%
    print(n=50)
  
  
####Data Cleaning - Recoding####
  DATA$hinctnta
  
  DATA$income<-as_factor(DATA$hinctnta)
  
  summary(DATA$income)
  
  #Simplify Income Variables
  DATA$income_rec <- dplyr::recode(DATA$income,
                                   "J - 1st decile" = "Low",
                                   "R - 2nd decile" = "Low",
                                   "C - 3rd decile" = "Low",
                                   "M - 4th decile" = "Middle",
                                   "F - 5th decile" = "Middle",
                                   "S - 6th decile" = "Middle",
                                   "K - 7th decile" = "Middle",
                                   "P - 8th decile" = "High",
                                   "D - 9th decile" = "High",
                                   "H - 10th decile" = "High",
                                   "Refusal" = NA_character_,
                                   "Don't know" = NA_character_,
                                   "No answer" = NA_character_)

  summary(DATA$income_rec)
  
  DATA$income_rec<-relevel(DATA$income_rec,ref="Middle")

 #Voting
  
  DATA$vote_new<-as_factor(DATA$vote)

  
  DATA$vote_new_rec <- dplyr::recode(DATA$vote_new,
                              "Don't know" = "Maybe",
                              "Refusal" = "No",
                              "No answer" = "Maybe")
  DATA$vote_new_rec <- DATA$vote_new_rec  
  

  
  
