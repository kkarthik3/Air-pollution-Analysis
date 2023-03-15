library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)


air <- read_csv("data.csv")
colSums(is.na(air))

#dropping unwanted columns
air <- air[, -c(1,2,5,11,12)]

air$date <-as.Date(air$date,'%Y-%m-%d')
summary(air)

#replace null value by mean
#air["so2"][is.na(air['so2'])] = mean(air$so2, na.rm = TRUE)
#air["no2"][is.na(air['no2'])] = mean(air$no2, na.rm = TRUE)
#air["rspm"][is.na(air['rspm'])] = mean(air$rspm, na.rm = TRUE)
#air["spm"][is.na(air['spm'])] = mean(air$spm, na.rm = TRUE)

#some Data cleanup
air$type[air$type=="Sensitive Areas"] <-"Sensitive Area"
air$type[air$type %in% c("Industrial","Industrial Areas")] <-"Industrial Area"
air$type[air$type %in% c("Residential")] <-"Residential and others"


#Due to High Pollution in Delhi we have to analyze them
by_state_wise <-air%>%group_by(state)%>%summarise(Avg_So2=mean(so2,na.rm=TRUE),
                                                  Avg_No2=mean(no2,na.rm=TRUE),
                                                  Avg_Rspm=mean(rspm,na.rm=TRUE),
                                                  Avg_Spm= mean(spm,na.rm=TRUE))


ggplot(by_state_wise,aes(x=state,y=Avg_So2,fill=Avg_So2)) +
  geom_bar(stat="identity") +
  theme(axis.text.x =element_text(angle=45)) +
  ggtitle("Average Sulphor DiOxide Content-State Wise") +
  xlab(label="State") +
  ylab(label="Average SO2 Content")



ggplot(by_state_wise,aes(x=state,y=Avg_No2,fill=Avg_No2)) +
  geom_bar(stat="identity") +
  theme(axis.text.x =element_text(angle=45)) +
  ggtitle("Average Nitrous DiOxide Content-State Wise") +
  xlab(label="State") +
  ylab(label="Average NO2 Content")
 


ggplot(by_state_wise,aes(x=state,y=Avg_Rspm,fill=Avg_Rspm)) +
  geom_bar(stat="identity") +
  theme(axis.text.x =element_text(angle=45)) +
  ggtitle("Average Respirable Suspended Particulate Matter(RSPM) Content-State Wise") +
  xlab(label="State") +
  ylab(label="Average RSPM Content")


ggplot(by_state_wise,aes(x=state,y=Avg_Spm,fill=Avg_Spm)) +
  geom_bar(stat="identity") +
  theme(axis.text.x =element_text(angle=45)) +
  ggtitle("Average Suspended Particulate Matter(SPM) Content-State Wise") +
  xlab(label="State") +
  ylab(label="Average SPM Content")
 


#Lets investigate more on Delhi Trend w.r.t pollution
air$date <-as.POSIXct(air$date)
air$year <-year(air$date)

Delhi <-air%>%filter(state=="Delhi")%>%group_by(year,type)%>%summarise(Avg_So2=mean(so2,na.rm=TRUE),
                                                                       Avg_No2=mean(no2,na.rm=TRUE),
                                                                       Avg_Rspm=mean(rspm,na.rm=TRUE),
                                                                       Avg_Spm =mean(spm,na.rm=TRUE))
ggplot(Delhi,aes(x=year,y=Avg_So2)) +
  geom_line(size=1,color="darkred") +
  geom_point()+
  facet_wrap(~type) +
  ggtitle("Delhi SO2 Content-Year Wise")+
  xlab("Year") +
  ylab("Average SO2")

ggplot(Delhi,aes(x=year,y=Avg_No2)) +
  geom_line(size=1,color="darkred") +
  geom_point()+
  facet_wrap(~type) +
  ggtitle("Delhi NO2 Content-Year Wise")+
  xlab("Year") +
  ylab("Average NO2")

ggplot(Delhi,aes(x=year,y=Avg_Rspm)) +
  geom_line(size=1,color="darkred") +
  geom_point()+
  facet_wrap(~type) +
  ggtitle("Delhi RSPM Content-Year Wise")+
  xlab("Year") +
  ylab("Average RSPM")

ggplot(Delhi,aes(x=year,y=Avg_Spm)) +
  geom_line(size=1,color="darkred") +
  geom_point()+
  facet_wrap(~type) +
  ggtitle("Delhi SPM Content-Year Wise")+
  xlab("Year") +
  ylab("Average SPM")
