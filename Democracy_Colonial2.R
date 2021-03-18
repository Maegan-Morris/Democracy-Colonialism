##Preliminary
--------------------------------------------------------------------------
  setwd("C:/Users/lenor/Documents/POS401-Laboratories")

install.packages("table1")
install.packages("dslabs")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages('DescTools')

library("table1","tidyverse,","ggplot2", "dslabs", "DescTools")
library(tidyverse)
library(table1)
library(DescTools)

## PART 2

##Filtering####---------------------------------------------------------------------------------

Qualgov<- read.csv("QualityOfGovernment_TimeSeries(1990-2015).csv")

colgov <-Qualgov %>%
  filter( p_durable|ht_colonial ==1 | ht_colonial ==2 | ht_colonial==3 | ht_colonial ==4 |ht_colonial== 5 | ht_colonial == 6 | ht_colonial == 7 | ht_colonial == 8| ht_colonial == 9| ht_colonial == 10)

Demo <- filter(colgov, chga_demo == "1")
Demo <- filter(Demo,ht_colonial == "1" | ht_colonial == "2" | ht_colonial=="3"|ht_colonial == "4"|ht_colonial== "5" |ht_colonial =="6" |ht_colonial == "7"| ht_colonial == "8"| ht_colonial == "9"|ht_colonial == "10")

##Colonized Democratic Countries--------------------------------------------------------------------------------------------------------------

Demo <- Demo%>%
  select("chga_demo", "ht_colonial","cname","year","p_durable")

##Colonized Undemocratic countries-----------------------------------------------------------------------------------------------------------
UN_Demo <- Qualgov
 
UN_Demo <- UN_Demo %>%
  filter(chga_demo == "0")

UN_Demo <- filter(UN_Demo,ht_colonial == "1" | ht_colonial == "2" | ht_colonial=="3"|ht_colonial == "4"|ht_colonial== "5" |ht_colonial =="6" |ht_colonial == "7"| ht_colonial == "8"| ht_colonial == "9"|ht_colonial == "10")%>%
  select("chga_demo", "ht_colonial","cname","year","p_durable")

##Un-colonized Undemocratic Countries--------------------------------------------------------------------------------------------------------------------

UN_col_UNDEM <-colgov %>%
  select("chga_demo", "ht_colonial","cname","year","p_durable")
UN_col_UNDEM <- filter(UN_col_UNDEM, ht_colonial == "0", chga_demo == "0")

##Un-colonized Democratic Countries-----------------------------------------------------------------------------------------------------------------------
UN_ColDem <-colgov %>%
  select("chga_demo", "ht_colonial","cname","year","p_durable")
UN_ColDem <-filter(UN_ColDem, ht_colonial == "0", chga_demo == "1")

## Summary---------------------------------------------------------------------------------------------------------
summary(UN_Demo)
summary(Demo)
summary(UN_ColDem)
summary(UN_col_UNDEM)

##Cleaning up data##------------------------------------------------------------------------------------------------
UN_Demo <- na.omit(UN_Demo)
Demo <- na.omit(Demo)
UN_col_UNDEM <-na.omit(UN_col_UNDEM)
UN_ColDem <- na.omit(UN_ColDem)

##Figures Preparation##------------------------------------------------------------------------------------------------
Demo <-Demo %>%
  mutate(Regime = case_when(
    chga_demo == 1 ~ "Democratic"))
UN_Demo <- UN_Demo %>%
  mutate(Regime = case_when(
    chga_demo == 0 ~ "Undemocratic"))

UN_ColDem <-UN_ColDem%>%
  mutate(Regime = case_when(
    chga_demo == 1 ~ "Democratic"))

UN_col_UNDEM <- UN_col_UNDEM%>%
  mutate(Regime= case_when(
    chga_demo == 0 ~ "Undemocratic"))

Demofull<- rbind.data.frame(Demo,UN_ColDem)

Un_demofull<-rbind.data.frame(UN_col_UNDEM,UN_Demo)

summary(Demofull)
summary(Un_demofull)

##Identify by stability ranges---------------------------------------------------------------------

Demofull<- Demofull %>%
  mutate(stability= case_when( 
    p_durable >-1 & p_durable < 2 ~ "0-1",
    p_durable > 1 & p_durable < 25 ~ "2-25",
    p_durable > 24 & p_durable < 50 ~ "26-50",
    p_durable > 49 & p_durable < 75 ~ "51-75",
    p_durable > 74 & p_durable< 100 ~ "76-100",
    p_durable > 99 & p_durable < 200 ~ "100-200")) 
      

Un_demofull <- Un_demofull %>%
  mutate(stability= case_when(
    p_durable >-1 & p_durable < 2 ~ "0-1",
    p_durable > 1 & p_durable < 25 ~ "2-25",
    p_durable > 24 & p_durable < 50 ~ "26-50",
    p_durable > 49 & p_durable < 75 ~ "51-75",
    p_durable > 74 & p_durable< 100 ~ "76-100",
    p_durable > 99 & p_durable < 200 ~ "100-200"))
     

##I had to select a year to eliminate multiple entries of one country. I went 
##with 1999 as it was the mean year for both datasets
##---------------++++++++++++++++++++++++++++++++++++++++
  Demofull1999<-Demofull%>%
    filter(year == "1999")
  Un_demofull1999 <- Un_demofull %>%
    filter(year == "1999")

#2.1.1- Bivariate table to represent X-Y relationship (X = colonial history, Y =
# country stability)
  
##Mean Comparison Table------------------------------------------------------------------------------------------------------------------------------------  

  Year1999<- rbind.data.frame(Demofull1999,Un_demofull1999)
  
  Year1999 <- Year1999 %>%
    mutate(rating= case_when(
      p_durable >-1 & p_durable < 2 ~ "Unstable",
      p_durable > 1 & p_durable < 25 ~ "Low Stability",
      p_durable > 24 & p_durable < 75 ~ "Stable ",
      p_durable > 74 & p_durable < 200 ~ "High Stability",))

  Year1999 <- Year1999 %>%
    mutate(History = case_when(
      ht_colonial > -1 & ht_colonial < 1 ~ "Never Colonized",
      ht_colonial > 0 & ht_colonial < 10 ~ "Colonized"
    ))

Year1999<- na.omit(Year1999)

label(Year1999$stability)<-"Regime Stability (Years)"
label(Year1999$History)<- "Colonial History" 
label(Year1999$p_durable)<- "Years since last regime change"

table1(~ p_durable| factor(History), data=Year1999, overall = "Total", 
       transpose = TRUE, render="Mean (CV%)")


label(Year1999$stability)<-"Regime Stability (Years)"
label(Year1999$History)<- "Colonial History"
table1(~ History | stability, data=Year1999, overall = "Total")


#2.1.2 Prepare a bivariate figure to represent the X-Y relationship of interest


Year1999 %>%
  group_by(History) %>%
  summarize(ht_colonial <-mean(p_durable))

df2 <- data.frame(
  ht_colonial = factor(c("Colonized", "Never Colonized"), 
                       levels = c("Colonized", "Never Colonized")),
  mean_Years =   c(16.5, 32.5))
 
ggplot(data=df2, aes(x= ht_colonial, y= mean_Years)) +
  geom_bar(colour="blue", stat = "identity") +
  xlab("Colonial History") + 
  ylab("Years Since Last Regime Change") +
  theme_grey()

 ggplot(data = df2, aes(x=ht_colonial, y= mean_Years, group= 1))+
  geom_line( linetype= "solid", color= "green")+
  geom_point(color="red")+
  expand_limits(y=0)+
   xlab("Colonial History")+
  ylab("Mean Years Since Last Regime Change")+
  theme_gray()
  
#2.3.1 Prepare a control table to represent the X-Y-Z relationship of interest
# (Z = Regime type [democratic or undemocratic])
 
 summary(Year1999$p_durable)
 
 Control <- Year1999 %>%
   group_by(History,Regime) %>%
   count(p_durable)


Control <- data.frame(
   Regime  = factor(c("Democratic", "Undemocratic", "Democratic", "Undemocratic")), 
   History = factor(c("Colonized", "Colonized", "Uncolonized", "Uncolonized"),
                    levels = c("Colonized", "Uncolonized")),
   count =   c(38,61, 43,17 ))

table1(~ rating | Regime + History, data=Year1999, overall=FALSE)

#2.3.2 Prepare a multivariate figure to represent the X-Y-Z relationship of interest

ggplot(data = Control, aes(x=History, y=p_durable,fill= Regime))+
  geom_bar( stat="identity", position=position_dodge())+ 
  xlab("Colonial History")+
  ylab("Years Since Regime Change")+
  theme_gray()

#3.1 Conduct a significance test of the X-Y relationship. 
    #Chi-square
chisq.test(Year1999$History, Year1999$stability)

#3.2 Estimate a measure of association of the X-Y relationship

  #Somers
som_XY <- table(Year1999$stability,Year1999$History)
SomersDelta(som_XY,direct=c("row"),conf.level=0.95,verbose=NULL)

#3.3 Conduct a significance test of the Z-Y relationship.

    #Chi-square
chisq.test(Year1999$Regime, Year1999$stability)

 
#3.4 Estimate a measure of association of the Z-Y relationship.
som_ZY <- table(Year1999$stability,Year1999$Regime )
SomersDelta(som_ZY,direct=c("row"),conf.level=0.95,verbose=NULL)
