library(tidyverse) #for data wrangling
library(reshape2) #for melting data
library(readxl) #for reading xlsx files
library(plotly) #for interactive maps
library(lmtest) #for robust standard errors
library(sandwich) #for robust standard errors

source.homicide.data<-read_excel("C:/Users/axvmo/Desktop/114 Final Paper/homicide_country.xlsx")
rate.homicide.data<-source.homicide.data%>%
  select(Unit, Gender, Year, country, iso3_code, Value)%>%
  filter(Unit=="Rate per  100,000 population",
         Gender=="Total (all ages)",
         Year>=1999 & Year<=2014,
         iso3_code=="SLV"|iso3_code=="GTM"|iso3_code=="HND"|iso3_code=="NIC")%>%
  mutate(NT.indicator=ifelse(iso3_code=="SLV"|iso3_code=="GTM"|iso3_code=="HND",1,0),
         name=ifelse(NT.indicator==1,"Northern Triangle","Nicaragua"),
         Value=as.numeric(Value))%>%
  group_by(name,Year,NT.indicator)%>%
  summarise(rate=mean(Value))%>%
  mutate(post.policy.indicator=ifelse(Year>2006,1,0))

ggplot(data=rate.homicide.data,mapping=aes(Year,rate))+
  geom_line(mapping=aes(linetype=name,color=name))+
  geom_vline(xintercept=2007,linetype="dotted",alpha=0.5)+
  geom_point(mapping=aes(shape=name,color=name))+
  labs(x="Year",y="Rate",color="Region",linetype="Region",shape="Region")+
  scale_color_brewer(palette="Paired")+
  theme_classic()

reps<-length(unique(rate.homicide.data$Year))
country.names<-c(rep("Nicaragua",reps),
                 rep("El Salvador",reps),
                 rep("Honduras",reps),
                 rep("Guatemala",reps))
NT.rate.data<-data.frame(subset(rate.homicide.data,name=="Northern Triangle"))
interactive.plot.data<-rbind(data.frame(subset(rate.homicide.data,name=="Nicaragua")),
                             NT.rate.data,NT.rate.data,NT.rate.data)
interactive.plot.data<-interactive.plot.data%>%
  mutate(plot.names=country.names,
         hover=paste0(name,"\n",round(rate,2)))
fontStyle<-list(family = "DM Sans",size = 15,color = "black")
fontStyle1<-list(family = "DM Sans",size = 17,color = "white")
label<-list(bgcolor = "#EEEEEE",bordercolor = "transparent",font = fontStyle)

plot_geo(interactive.plot.data,locationmode="country names",frame=~Year)%>%
  add_trace(locations=~plot.names,z=~rate,color=~rate,
            zmin=min(interactive.plot.data$rate),
            zmax=max(interactive.plot.data$rate),
            colorscale = "RdBu",
            text = ~hover,
            hoverinfo = "text")%>%
  layout(geo=list(fitbounds="locations",font = list(type = "DM Sans"),
                  bgcolor = "rgba(0,0,0,0)"))%>%
  style(hoverlabel = label)%>%
  config(displayModeBar = TRUE)%>%
  colorbar(y=0.75)%>%
  animation_opts(easing = "elastic")

model.data<-subset(rate.homicide.data,Year>1998&Year<2012)

model<-lm(rate~NT.indicator+post.policy.indicator+NT.indicator*post.policy.indicator,data=model.data)
robust.model<-coeftest(model, vcov=vcovHC(model,type="HC0"))
robust.model

balance.test<-function(data.path,measure,specification){
  source.data<-read_excel(data.path)
  ts.data<-source.data[-1]%>%
    filter(Country=="Nicaragua"|Country=="El Salvador"|Country=="Honduras"|Country=="Guatemala")%>%
    melt(id.vars="Country",variable.name="year",na.rm=TRUE)%>%
    mutate(NT.indicator=ifelse(Country=="El Salvador"|Country=="Honduras"|Country=="Guatemala",1,0),
           name=ifelse(NT.indicator==1,"Northern Triangle","Nicaragua"),
           value=as.numeric(value),year=as.integer(as.character(year)))%>%
    group_by(name,year,NT.indicator)%>%
    summarise(measure=mean(value))%>%
    mutate(post.policy.indicator=ifelse(year>2006,1,0))
  if(specification=="plot"){
    plot<-ggplot(data=ts.data,mapping=aes(year,measure))+
      geom_line(mapping=aes(linetype=name,color=name))+
      geom_vline(xintercept=2007,linetype="dotted",alpha=0.5)+
      geom_point(mapping=aes(shape=name,color=name))+
      labs(x="Year",y=measure,color="Region",linetype="Region",shape="Region")+
      scale_color_brewer(palette="Paired")+
      theme_classic()
    return(plot)
  }
  if(specification=="model"){
    model.data<-subset(ts.data,year>1998&year<2012)
    model<-lm(measure~NT.indicator+post.policy.indicator+NT.indicator*post.policy.indicator,data=model.data)
    robust.model<-coeftest(model, vcov=vcovHC(model,type="HC0"))
    return(robust.model)
  }
}
balance.test("C:/Users/axvmo/Desktop/Apllying/example code/data/Gross national income (GNI) per capita (constant 2017 PPP$).xlsx","GNI per Capita","plot")
balance.test("C:/Users/axvmo/Desktop/114 Final Paper/Mean years of schooling (years).xlsx","Mean Years of Schooling","plot")
balance.test("C:/Users/axvmo/Desktop/114 Final Paper/Life expectancy at birth (years).xlsx","Life Expectancy at Birth (Years)","plot")
balance.test("C:/Users/axvmo/Desktop/114 Final Paper/Human Development Index (HDI).xlsx","HDI","plot")