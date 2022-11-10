#DATA SCIENCE COURSEWORK
#LOADING THE REQUIRED LIBRARIES
library(scales)
library(Hmisc)
library(corrplot)
library(tidyverse)
library(lubridate)
library(forecast)

#GETTING THE COVID DATASET
covid <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
view(covid)

#PRE-PROCESSING AND CLEANING
#FILTERING REQUIRED COLUMNS
covid <-covid %>%select(c("iso_code","continent","location", "date","new_cases","new_deaths","icu_patients","hosp_patients","people_vaccinated")) 

#EXPLORING DATA THROUGH SUMMARY
covid %>% group_by(iso_code) %>% summarise(Mean= mean(new_cases,na.rm=TRUE))
usa <-covid%>%filter(iso_code %in% c("USA"))  
View(usa)

#REPLACING N.A. VALUES WITH 0
usa[is.na(usa)] <- 0    

#cHECKING FOR ANY NEGATIVE VALUES
nrow(usa[usa$new_cases<0,])
nrow(usa[usa$new_deaths<0,])
nrow(usa[usa$icu_patients<0,])
nrow(usa[usa$hosp_patients<0,])
nrow(usa[usa$people_vaccinated<0,])        

#CHECKING CLASS OF COLUMN DATE
class(usa$date) 

#CONVERTING DATA TYPE FROM CHAR TO DATE
usa$date<-as.Date(usa$date,'%Y-%m-%d')  

#PLOTTING VISUALISATIONS
#TREND OF COVID-19 CASES
ggplot()+geom_line(usa,mapping = aes(date,new_cases),color = "dark green")+
  scale_x_date(breaks="8 weeks",date_labels="%b %y") +
  labs(x="Timeline",y="Cases",title = "Everyday trend of new COVID-19 cases") + 
  scale_y_continuous(labels = label_number(),breaks =seq(from =0,to= 1500000,by=150000))             

#GIVING NEW NAMES TO VARIABLES
newcases <- usa$new_cases
newdeaths <- usa$new_deaths
icu <- usa$icu_patients
hospital <- usa%>%select(hosp_patients)

#CHECKING CORRELATION BETWEEN ICU PATIENTS AND DEATHS
cor.test(icu,newdeaths)            

#SCATTERPLOT BETWEEN ICU PATIENTS AND DEATHS
ggplot(usa,mapping=aes(icu_patients,new_deaths)) +geom_point()+
  labs(x="ICU patients",y="New deaths",title = "Correlation between Deaths and ICU patients")  

#CORRELATION MATRIX
corrmatrix <- usa[,c(5,6,7,8)]
usacor<-cor(corrmatrix)                              
usacor

#P-VALUE MATRIX
corr <-rcorr(as.matrix(corrmatrix))                     
corr


#TIME SERIES ANALYSIS

#CREATING MULTIVARIATE TIME SERIES OBJECT
mts2 <- ts(cbind(icu,hosp,newdeaths,newcases),
           start = decimal_date(ymd("2020-01-22")),
           frequency = 365.25)

#PLOTTING MULTIVARIATE TIME SERIES
plot(mts2, xlab ="Timeline",
     main ="COVID-19 Pandemic",
     col.main ="darkgreen") 

#UNIVARIATE TIME SERIES FORECASTING USING ARIMA MODEL
#EXPLORING IF DATA IS STATIONARY OR NOT
plot(newcases)                                              #No constant mean/variance
diffnewcases<-diff(newcases)                                #Making data stationary
plot(diffnewcases)

#DICKEY-FULLER TEST
adf.test(newcases,alternative = "stationary")               #DATA IS NOT STATIONARY p>0.05(Dickey-fuller test)
adf.test(diffnewcases,alternative = "stationary")           #DATA IS STATIONARY p<0.05

#MAKING ARIMA MODEL USING auto.arima()
auto.arima(newcases)                                        #ARIMA(2,1,1) WITH DRIFT
auto.arima(diffnewcases)                                    #ARIMA(2,0,1) (ALREADY DIFFERENCIATED)

#CREATING TIME SERIES OBJECT(UNIVARIATE)
mts1 <- ts(newcases, 
           start = decimal_date(ymd("2020-01-22")),
           frequency = 365.25)

#FORECASTING MODEL USING ARIMA
fit1<- auto.arima(mts1)
fit1

#PLOTTING FORECAST FOR NEXT 120 DAYS
plot(forecast(fit1, 120), 
     xlab ="Timeline",
     ylab ="New Cases",
     main ="COVID-19 Pandemic", 
     col.main ="darkgreen")
