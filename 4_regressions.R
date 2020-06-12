####Setup####
  ####*Packages and Getting Data####
  #Activate Library 
    library(tidyverse)
    library(essurvey)
    library(car)
    library(haven)
   

  ####*Data into R####
    #Set your own email with set_email("")
    source("my_email.R")

    DATA<-import_rounds(
      rounds=7,
      format='stata')  

####Clean Data####
  ####*Simple Clean- rename and turn into factors where necessary####
  #Outcome Variables
  DATA$eff_int<-DATA$cptppol
  
  #Voting - already exclude people who are not eligible as NA
  DATA$vote<-as_factor(DATA$vote)
  DATA$vote <- fct_recode(DATA$vote,
    NULL = "Not eligible to vote",
    NULL = "Don't know",
    NULL = "Refusal",
    NULL = "No answer")
  

  #Main variable  
  DATA$gender<-as_factor(DATA$gndr)
  DATA$gender <- fct_recode(DATA$gender,
    NULL = "No answer")
  
  #Control Variables
  DATA$education<-as_factor(DATA$eisced)

  DATA$age<-DATA$agea
  DATA$income<-as_factor(DATA$hinctnta)
  
  
  
  DATA$activity<-
    
  DATA$country<-as_factor(DATA$cntry)
  
  
  ####*Simplify activity variable####
  
  

####Bivariate Regression: Internal Efficacy and Gender####
  
  
####Bivariate Logistic Regression####
  
  
  
####Multivariate Regression 1: Internal Efficacy and Socio-Demographic controls####
  
  
####Multivariate Regression 2: Country Fixed Effects####
  
  
  
####Presenting Results####

  ####*Table of Results####
  ####*Visual Representations####

  