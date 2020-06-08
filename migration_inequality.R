####Setup####
  library(tidyverse)
  library(essurvey)
  library(car)
  library(haven)
  library(ggthemes)
  library(plotrix)
  library(broom)
  library(magrittr) # for specific extraction - careful about conflict with dplyr

####Download Data####
  #Set your own email with set_email("")
  source("my_email.R")
  
  #Download Data 
  DATA<-import_all_rounds(
    format = 'stata')
  
         
  
  
  
  
  
  
####Select and clean variables of interest####
  #Interested in migration background, 
  #experience of discrimination and political participation (voting) and interest
  DATA.simple<-map_dfr(
    DATA, magrittr::extract, c("essround","cntry",
                               "polintr","vote",
                               "brncntr","facntr","mocntr",
                               "hincfel","eduyrs",
                               "dscrgrp","dscrrce"))
  
  ####*Generate migration background variable 
  ####*#- either own background or parents - limited coverage here for longer duration####
  DATA.simple<-
    DATA.simple %>% 
    mutate(
      mig = as.factor(case_when(
        brncntr == 2 ~ "Yes",
        facntr == 2 | mocntr == 2 ~ "Yes",
        is.na(brncntr)  ~ NA_character_,
        TRUE ~ "No")))
  
 ####*Voting####
  DATA.simple%>%
    group_by(mig,vote)%>%
    drop_na()%>%
    mutate(vote=as_factor(vote))%>%
    summarize(n=n())%>%
    mutate(freq = n / sum(n))%>%
    ggplot()+
      aes(x=vote, y= freq,fill=mig, group=mig)+
      geom_col(position="dodge")+
      theme_bw()+
      labs(x="Voting",
         y ="Frequency",
         title="Electoral Participation by Background",
         fill="Migration \nBackground")
 
  
  
                             
  ####*Feeling discriminated####
  DATA.simple%>%
    group_by(mig,dscrgrp)%>%
    drop_na()%>%
    mutate(dscrgrp=as_factor(dscrgrp))%>%
    summarize(n=n())%>%
    mutate(freq = n / sum(n))%>%
    filter(dscrgrp=="Yes")%>%
    ggplot()+
      aes(x=mig, y= freq, fill=mig)+
      geom_col()+
    theme_bw()+
    labs(x="Migration Background",
         y ="Frequency",
         title="Feeling of discrimnation \nby Background")
  
  ####*Feeling discriminated by country####
  DATA.simple%>%
    group_by(mig,dscrgrp,cntry)%>%
    drop_na()%>%
    mutate(dscrgrp=as_factor(dscrgrp))%>%
    summarize(n=n())%>%
    mutate(freq = n / sum(n))%>%
    filter(dscrgrp=="Yes")%>%
    ggplot()+
      aes(x=cntry, y=freq, fill=mig)+
      geom_col(position="dodge")+
    theme_bw()+
    labs(x="Country",
         y ="Frequency",
         fill="Immigrant\n Background",
         title="Feeling of discrimnation \nby Background")+
     theme(axis.text.x = element_text(angle = 90))

  
  
  
  

  
  