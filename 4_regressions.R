####Setup####
  ####*Packages and Getting Data####
  #Activate Library 
    library(tidyverse)
    library(essurvey)
    library(car)
    library(haven)
    library(stargazer)
    library(texreg)
    library(sjPlot)

  ####*Data into R####
    #Set your own email with set_email("")
    source("my_email.R")

    DATA<-import_rounds(
      rounds=7,
      format='stata')  
    
  ####*fix random seed for reprod####
  set.seed(11)

####Clean Data####
  ####*Simple Clean- rename and turn into factors where necessary####
  #Outcome Variables
  DATA$eff_int<-as.numeric(DATA$cptppol)
  
  #Voting - already exclude people who are not eligible as NA
  DATA$vote<-as_factor(DATA$vote)
  DATA$vote <- fct_recode(DATA$vote,
    NULL = "Not eligible to vote",
    NULL = "Don't know",
    NULL = "Refusal",
    NULL = "No answer")
  

  #Gender
  DATA$gender<-as_factor(DATA$gndr)
  DATA$gender <- fct_recode(DATA$gender,
    NULL = "No answer")
  
  #Education
  DATA$education<-as.numeric(DATA$eduyrs)


  #Age
  DATA$age<-as.numeric(DATA$agea)
  
  #Income
  DATA$income<-as_factor(DATA$hinctnta)
  DATA$income <- fct_recode(DATA$income,
    NULL = "Refusal",
    NULL = "Don't know",
    NULL = "No answer")
  
  #Main Activity
  DATA$activity<-as_factor(DATA$mnactic)
  DATA$activity <- fct_recode(DATA$activity,
    "Other" = "Community or military service",
    "Unemployed" ="Unemployed, not looking for job",
    "Unemployed" ="Unemployed, looking for job",
    NULL = "Don't know",
    NULL = "Refusal",
    NULL = "Not applicable",
    NULL = "No answer")
    
  DATA$country<-as_factor(DATA$cntry)
  
  
  ####*Simplify variables####
  #Income
    DATA$income_simple <- fct_recode(DATA$income,
      "Low" = "J - 1st decile",
      "Low" = "R - 2nd decile",
      "Low" = "C - 3rd decile",
      "Middle" = "M - 4th decile",
      "Middle" = "F - 5th decile",
      "Middle" = "S - 6th decile",
      "Middle" = "K - 7th decile",
      "High" = "P - 8th decile",
      "High" = "D - 9th decile",
      "High" = "H - 10th decile",
      NULL = "Refusal",
      NULL = "Don't know",
      NULL = "No answer"
    )
    
  ####*Set new baselines for factor variables####
  DATA$income_simple<-relevel(DATA$income_simple, ref="Middle")
  
  DATA$vote<-relevel(DATA$vote, ref="No")
  
  
####What is Regression?####
  ####*Generate a sample dataset####
  #x =1000 random numbers between -10 and plus 10
  x<-runif(1000, min=-10, max=10)
  #Y= x * 2 
  y_real<-(2*x)
  
  #Intercept
  intercept<-3
  
  #Some randomly distributed measurement error with mean 0
  error<- rnorm(n=1000,mean=0, sd=10)
  
  #Generate observed variables
  y<-y_real+error+intercept
  
  #Simple scatter plot
  ggplot()+
    aes(x=x, y=y)+
    geom_point()
  
  #Fit a straight line through this cloud that minimizes distance
  ggplot()+
    aes(x=x, y=y)+
    geom_point()+
    geom_smooth(method="lm")+
    geom_vline(xintercept=0, linetype="dashed")+
    geom_hline(yintercept=0, linetype="dashed")+
    theme_bw()

  #Regression Model
  summary(lm(y~x))

####Bivariate Regression: Internal Efficacy and Gender####
  mod_1<-lm(eff_int~education, data=DATA)
  
  summary(mod_1)

  ##Visual Example##
  DATA %>%
    filter(!is.na(eff_int))%>%
    filter(!is.na(education))%>%
    sample_frac(0.1)%>%
    ggplot()+
      aes(x=education, y=eff_int)+
      geom_point(alpha=0.5, size=1, position="jitter")+
      geom_smooth(method="lm")+
    theme_bw()
  
####Multivariate Regression 1: Internal Efficacy, Age, Gender####
  mod_2<-lm(eff_int ~ gender + education, data= DATA)
  
  summary(mod_2)
  
  
  #Visual Example 1
    DATA %>%
      select(gender,eff_int,education)%>%
      drop_na()%>%
      sample_frac(0.2)%>%
      ggplot()+
        aes(x=education, y=eff_int, color=gender)+
        geom_point(alpha=0.25, size=1, position="jitter")+
      theme_bw()
    
  #Visual Example 2 - two regression lines with different intercepts for gender
    #predicted values
      DATA%>%
        select(gender,education,eff_int)%>%
        drop_na()%>%
        cbind(., pred=predict(mod_3))%>%
        sample_frac(0.2) %>%
        ggplot()+
          aes(x=education, y=eff_int, color=gender)+
          geom_point(alpha=0.25, size=1, position="jitter")+
          geom_line(mapping = aes(y=pred), size=1.5)+
        theme_bw()
  
####Multivariate Regression 2: Socio-demographic Controls####
  #Version 1
  mod_3<-lm(eff_int ~ education + gender+ activity + age+ education + income, data= DATA)
  
  summary(mod_3)
      
  #Version 2 - with simplified income
  mod_4<-lm(eff_int ~ education+ gender+ activity + age + income_simple, data= DATA)
  
  summary(mod_4)  
  
####Presenting Results####

  ####*Table of Results-Texreg####

  screenreg(list(mod_1,mod_2,mod_4))
  
  screenreg(list(mod_1,mod_2,mod_4),
            custom.coef.names = 
              c("Intercept","Years of Education","Age","Female",
                "Student","Unemployed","Sick","Retired","Other Activity","Housework etc.",
                "High Income","Low Income"))
  #Export to html 
  htmlreg(list(mod_1,mod_2,mod_4),
            custom.coef.names = 
              c("Intercept","Years of Education","Age","Female",
                "Student","Unemployed","Sick","Retired","Other Activity","Housework etc.",
                "High Income","Low Income"),
          file="results.html")
                                  
  
  ####*Table of Results-Stargazer####
  stargazer(mod_1, type="text")
  
  stargazer(mod_1,mod_2,mod_4, type="text")
  
  stargazer(mod_1,mod_2,mod_4, type="text",
            covariate.labels = c("Gender", "Student","Unemployed","Sick","Retired","Other Activity","Housework etc.",
                                 "Age", "High Income","Low Income","Years of Education","Intercept"))
  
  stargazer(mod_1,mod_2,mod_4, type="html",
            covariate.labels = c("Gender", "Student","Unemployed","Sick","Retired","Other Activity","Housework etc.",
                                 "Age", "High Income","Low Income","Years of Education","Intercept"),
            out= "results_stargazer.html")
  
  
  ####*Visual Representations####
  plot_models(mod_4)
  
  plot_models(mod_4,
              axis.labels = c("High Income","Low Income",
                              "Age","Housework","Other Activity","Retired","Sick or Disabled",
                              "Unemplyed","Student","Female","Years of Education"))+
    theme_bw()+
    geom_hline(mapping=aes(yintercept=0), color="black", linetype="dashed")+
    labs(x="Variables", title="Model Estimates")
                 


    
  plot_models(mod_4,mod_2,
              axis.labels = c("High Income","Low Income",
                              "Age","Housework","Other Activity","Retired","Sick or Disabled",
                              "Unemplyed","Student","Female","Years of Education"))+
    theme_bw()+
    geom_hline(mapping=aes(yintercept=0), color="black", linetype="dashed")+
    labs(x="Variables", title="Model Estimates")

  
  
####Expansion 1: Country Fixed Effects####
  mod_5<-lm(eff_int ~ + education+ gender+ activity + age + income_simple+country, data= DATA)

  summary(mod_5)
  
  
####Expansion 2: Interaction Effects - Careful with interpretation!####
  #Education and gender
  mod_6<-lm(eff_int ~ + education * gender+ activity + age + income_simple+country, data= DATA)
  
  #Education and the country
  mod_7<-lm(eff_int ~ + education * country + gender+ activity + age + income_simple, data= DATA)

  
####Expansion 3: Logit Models - binary dep. variables####
  mod_8<-glm(vote ~ + eff_int,
             data= DATA,
             family=binomial(link = "logit"))
  summary(mod_8)
  
  mod_9<-glm(vote ~ + eff_int+education+ gender+ activity + age + income_simple+country, 
             data= DATA,
             family=binomial(link = "logit"))
  summary(mod_9)
  summary(mod_9)


  