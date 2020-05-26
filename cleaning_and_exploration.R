####Setup####
  
  #Install Packages (only need to do this once)
  install.packages("tidyverse")  #Tidyverse helps us with many helper functions + ggplot 2
  install.packages("essurvey")  #To download ESS directly
  install.packages("car")    #For data cleaning. A bit old school - note conflict with dplyr in recode
  install.packages('haven') #Better handle for stata files

  #Activate Packages 
  library(tidyverse)
  library(essurvey)
  library(car)
  library(haven)

  #Just for teaching - show pipes
  #devtools::install_github("daranzolin/ViewPipeSteps")
  library(ViewPipeSteps)
  
####Download the data###
  set_email("cgnguyen@gmail.com") #Add your own email here that you used to register at the ESS
  
  
  ##Using essurvey package to directly download an ESS survey. We will use Wave 7 only for now to keep things simple 
  
  DATA<-import_rounds(
    rounds=7,
    format='stata')
    

####Exploring the Data 1: Looking at the data####
  #External Efficacy 
  DATA$psppipl
  
  #Internal Efficacy
  DATA$cptppol
  
  #Summary statistics of variables
  
  DATA %>%
    select(psppipl,cptppol) %>%
    summary(.)
  
  
####Cleaning Data 1: Renaming Variables####
  
  DATA$eff_ext<-DATA$psppipl
  DATA$eff_int<-DATA$cptppol
  
  
####Data Cleaning 1: Turning Variables into factors####
  #Country
  summary(DATA$cntry)
  DATA$country<-as_factor(DATA$cntry)

  summary(DATA$country)  
  
  #Gender
  summary(DATA$gndr)
  
  DATA$gender<-as_factor(DATA$gndr)
  
  
####Descriptive Statistics by groups####
  
  #Do men and women have different average levels of internal and external efficacy 
  
  DATA %>%
    group_by(gender)%>%
    summarize(eff_ext_mean= mean(eff_ext, na.rm=T),
              eff_int_mean= mean(eff_int, na.rm=T),
              n=n())%>%
    print_pipe_steps()
  
  
  #Are there differences in efficacy by country ? 
  DATA %>% 
    group_by(country)%>%
    summarize(eff_ext_mean= mean(eff_ext, na.rm=T),
              eff_int_mean= mean(eff_int, na.rm=T),
              n=n()) %>%
    arrange(-eff_ext_mean)%>%
    print(n=21)%>%
    print_pipe_steps()

  
  #Are there differences by country and gender? 
  DATA %>% 
    group_by(country,gender)%>%##
    summarize(eff_ext_mean= mean(eff_ext, na.rm=T),
              eff_int_mean= mean(eff_int, na.rm=T),
              n=n()) %>%
    print(n=50)
    
  
  #Are there difference by country and gender -looking at the gender gap 
  DATA %>% 
    group_by(country,gender)%>%
    summarize(eff_ext_mean= mean(eff_ext, na.rm=T),
              eff_int_mean= mean(eff_int, na.rm=T))%>%
    pivot_wider(names_from=gender, values_from=c(eff_ext_mean,eff_int_mean))%>%
    summarize(ext_gap=eff_ext_mean_Male - eff_ext_mean_Female,
              int_gap=eff_int_mean_Male - eff_int_mean_Female)%>%
    arrange(ext_gap)%>%
    print(n=21)
  
####More Data Cleaning: Recoding####
  
  #Income
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
  
  #Change the Baseline Category
  DATA$income_rec<-relevel(DATA$income_rec,ref="Middle")
  
  
  #Recode Age Variables - turn numeric to reduce labels
  DATA$age<-as.numeric(DATA$agea)
  
  #Recode using car to create bins 
  DATA$age_factor<-car::recode(DATA$age,"
                               14:34='Young';
                               35:64='Middle Age';
                               64:114='Old'")
  #Turn the agefactor into a factor variable - it was a character variable before
  DATA$age_factor<-as.factor(DATA$age_factor)
  
  

  
  
  
  