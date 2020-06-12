####Setup####

#Activate Library 
  library(tidyverse)
  library(essurvey)
  library(car)
  library(haven)
  library(ggthemes)
  library(plotrix)
  library(broom)
  library(magrittr) # for specific extraction - careful about conflict with dplyr


 
####Data into R####
  #Set your own email with set_email("")
  source("my_email.R")

  DATA<-import_rounds(
    rounds=7,
    format='stata')  

  
####Clean Data 1####
  DATA$eff_ext<-DATA$psppipl
  DATA$eff_int<-DATA$cptppol
  DATA$country<-as_factor(DATA$cntry)
  DATA$gender<-as_factor(DATA$gndr)

####Display Descriptive Statistics by groups####
  ####*Simple Bar Plot - Do men and women have different average levels of internal efficacy####
  DATA %>%
    group_by(gender) %>%
    summarize(eff_int_mean =mean(eff_int, na.rm=T))%>%
    filter(gender!="No answer") %>% 
    ggplot()+
      aes(x=gender, y=eff_int_mean)+
      geom_col()
  
  ####*Simple Bar Plot - Do men and women have different average levels of internal efficacy -include Standard errors####
  DATA %>%
    group_by(gender) %>%
    summarize(eff_int_mean =mean(eff_int, na.rm=T),
              se = std.error(eff_int, na.rm=T))%>%
    filter(gender!="No answer") %>% 
    ggplot()+
      aes(x=gender, y=eff_int_mean, ymin=eff_int_mean-1.96*se, ymax=eff_int_mean+1.96*se)+
      geom_col()+
      geom_errorbar(width=0.4)
  
  
  #Make this plot a bit nicer to look at 
  DATA %>%
    group_by(gender) %>%
    summarize(eff_int_mean =mean(eff_int, na.rm=T),
              se = std.error(eff_int, na.rm=T))%>%
    filter(gender!="No answer") %>% 
    ggplot()+
      aes(x=gender, y=eff_int_mean, ymin=eff_int_mean-1.96*se, ymax=eff_int_mean+1.96*se)+
      geom_col(fill=c("blue","red"))+
      geom_errorbar(width=0.4)+
      theme_bw()+
      labs(x="Gender", y="Average Internal Efficacy", 
           title="Average Internal Efficacy by Gender",
           subtitle="Based on European Social Survey Wave 7")
  
    #Make this plot a bit nicer to look at? - I wish this was excel 
    last_plot()+
      theme_excel()

    #Make this plot a bit nicer to look at - Economist Style
    last_plot()+
      theme_economist()

    
  ####*More complex Barplot - Internal Efficacy by Country####  
  DATA %>%
   group_by(gender,country) %>%
    summarize(eff_int_mean =mean(eff_int, na.rm=T),
              se = std.error(eff_int, na.rm=T))%>%
    filter(gender!="No answer")%>% 
    ggplot()+
      aes(x=gender, y=eff_int_mean, ymin=eff_int_mean-1.96*se, ymax=eff_int_mean+1.96*se)+
      geom_col()+
      geom_errorbar(width=0.2)+
      facet_grid(.~country)+
      theme_bw()+
      labs(x="Gender", y="Average Internal Efficacy", 
           title="Average Internal Efficacy by Gender",
           subtitle="Based on European Social Survey Wave 7")+
      theme(axis.text.x = element_text(angle = 90))
    
 
    
  ####*More complex Barplot - Intenral Efficacy gap by country
  
  DATA %>% 
    group_by(country,gender) %>%
    summarize(eff_int_mean =mean(eff_int, na.rm=T))%>%
    pivot_wider(names_from = gender, values_from = c(eff_int_mean))%>%
    summarize(int_gap = Male - Female) %>%
    arrange(-int_gap) %>%
    ggplot()+
      aes(x=reorder(country, -int_gap),y=int_gap)+
      geom_col()+
      theme_bw()+
      labs(x="Country", y="Internal Efficacy Gap", 
           title="Average Internal Efficacy Gap by Country",
           subtitle="Based on European Social Survey Wave 7.\nHigher Values indicate greater efficacy for men")+
      theme(axis.text.x = element_text(angle = 90))
  
  ####*Optional - Including Standard errors for this gap####

  #Calculate general difference in means test using the t.test command and tidy from broom for a nice data frame
  DATA %>%
    filter(gender!="No answer")%>%
    select(gender,eff_int)%>%
    do(tidy(t.test(eff_int ~ gender, data=.)))
  
  #Do this again for each country
  DATA %>%
    group_by(country)%>%
    filter(gender!="No answer")%>%
    select(gender,eff_int) %>%
    do(tidy(t.test(eff_int ~ gender, data=.)))  
  
  #Create a graph from it 
    DATA %>%
    group_by(country)%>%
    filter(gender!="No answer")%>%
    select(gender,eff_int) %>%
    do(tidy(t.test(eff_int ~ gender, data=.)))%>%
    ggplot()+
      aes(x=reorder(country,-estimate), y=estimate, ymin=conf.low, ymax=conf.high)+
      geom_col()+
      geom_errorbar(width=0.4)+
      theme_bw()+
      labs(x="Country", y="Internal Efficacy Gap", 
           title="Average Internal Efficacy Gap by Country",
           subtitle="Based on European Social Survey Wave 7.\nHigher Values indicate greater efficacy for men",
           capiton="Errorbars represent 95% confidence intervals around differences in means")+
      theme(axis.text.x = element_text(angle = 90))

    
####Scatterplot - income and Efficacy in Germany ####
    ####*Simple Scatterplot - this does not work properly####
    DATA%>%
      filter(cntry=="DE")%>%
      mutate(income=as_factor(hinctnta))%>%
      ggplot()+
      aes(x=income, y=eff_int)+
      geom_point()
    
    ####*Adjust figure - remove missing and jitter####
    DATA%>%
      filter(cntry=="DE")%>%
      mutate(income=as_factor(hinctnta),
             eff_int=unclass(eff_int))%>%
      filter(income!="Refusal" & income!="Don't know")%>%
      ggplot()+
        aes(x=income, y=eff_int)+
        geom_point(position="jitter", alpha= 0.4)+
        theme_bw()+
        labs(x="Income",y="Internal Efficacy")+
        theme(axis.text.x=element_text(angle=-90))
    
    

    
    
    
    
    
    

####Line graphs - gender gap,income and interest in Germany over the years####
  ####*Setup####
  #Download data 
  DATA.germany<-import_country(
    country="Germany",
    rounds=1:8,
    format='stata')  
    
  #Turn list into data frames with the variables we need - requires 
  DATA.simple<-map_dfr(DATA.germany, magrittr::extract, c("gndr", "polintr","essround","hincfel"))
  
  #Check out the data
  DATA.simple
  
  ####*Simple Line graph - mean political interest over the waves
  DATA.simple %>%
    group_by(essround)%>%
    summarize(interest=mean(polintr, na.rm=T)) %>%
    ggplot()+
      aes(x=essround, y=interest)+
      geom_line()
  
  #Adjust scales and add themes. Otherwise it becomes very misleading 
  DATA.simple %>%
    group_by(essround)%>%
    summarize(interest=mean(polintr, na.rm=T)-1) %>%
    ggplot()+
      aes(x=essround, y=interest)+
      geom_line()+
      ylim(0,3)+
      theme_bw()+
      labs(x="Ess Round",y="Political Interest",title="Political Interest in Germany")
  
  
  ####*More Complex Graph - Political interest by gender over time in Germany####
   DATA.simple %>%
    mutate(gender=as_factor(gndr))%>%
    group_by(essround,gender)%>%
    summarize(interest=mean(polintr, na.rm=T)-1)%>%
    ggplot()+
      aes(x=essround, y=interest, color=gender)+
      geom_line()+
      ylim(0,3)+
      theme_bw()+
      labs(x="Ess Round",y="Political Interest",title="Political Interest in Germany")
  
  

  ####*More Complex Graph - Political interest by income over time in Germany####
   DATA.simple %>%
    mutate(hincfel=as_factor(hincfel))%>%
    group_by(essround,hincfel)%>%
    summarize(interest=mean(polintr, na.rm=T)-1) %>%
    filter(!is.na(hincfel))%>%
    ggplot()+
      aes(x=essround, y=interest, group=hincfel, color=hincfel)+
      geom_line()+
      ylim(0,3)+
      theme_bw()+
      labs(x="Ess Round",y="Political Interest",title="Political Interest in Germany")
  
  
  
  

    

    
    
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
####*Bonus - Make a highlightable map####
  
  ##Calculate average eficacy by country####
    data.temp<-
        DATA %>%
        group_by(cntry)%>%
        summarise(eff_int=mean(eff_int, na.rm=T)) %>%
        filter(!is.na(cntry))


  ##Load additional packages for maps and highlights
    library(maps) #maps
    library(viridis)    #For theme
    library(plotly) #highlights 
    library(crosstalk) #highlights
    
    #Get Map data
    world_map <- map_data("world")

    #Read in the country code conversion 
    countrycodes<-read.csv("countrycodes.csv")
    names(countrycodes)<-c("region","cntry")
    
    #Merge with data frame for easier integration into map
    data.temp<-inner_join(data.temp,countrycodes)
    
    #Merge data into map
    eu_map_eff_int <- right_join(data.temp, world_map, by = "region")
    
    eu_map_eff_int <- SharedData$new(eu_map_eff_int, ~cntry)

    
    #Generate Map
    figure_map<-ggplot(eu_map_eff_int, aes(long, lat, group = group))+
      geom_polygon(aes(fill = eff_int ), color = "white")+
       coord_fixed(xlim = c(-10, 30),  ylim = c(36, 72), ratio = 1.3)+
      theme(legend.position = "none")+
      scale_fill_continuous(high = "#132B43", low = "#56B1F7")+
      theme_bw() +
      xlab("")+ylab("")

  
    #Show map
    figure_map
    
    #Create interactive map using plotly
    ggplotly(figure_map,tooltip = "fill", height = 700, width= 700) %>% 
       layout(xaxis = list(automargin=TRUE, showgrid = F,  showticklabels=F , ticks=""), 
              yaxis = list(automargin=TRUE, showgrid = F,  showticklabels=F , ticks="")) %>%
      config(displayModeBar = F) 
  

  
  
  
  
    

  