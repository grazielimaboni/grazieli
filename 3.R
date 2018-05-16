library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)
library(GGally)
library(maptools)
library(ggmap)
library(maps)
ld<- read_csv("C:/Users/workshop/Desktop/lyme.csv")
prism<- read_csv("C:/Users/workshop/Desktop/climate.csv")
pop<- read_csv("C:/Users/workshop/Desktop/pop.csv")
lyme
climate
pop

str_replace_all(x,y,z)
#This comand selects for fips starting with 2
pop %<>% select(fips,starts_with("pop2"))
pop
#This comand gathers the data in two columns stating with pop2, selecting for size and eliminating the NA values
pop %<>% gather(starts_with("pop2"),key="str_year",value="size") %>% na.omit
pop
#This comand mutate the data adding a year column
pop %<>% mutate(year=str_replace_all(str_year,"pop",""))
pop
#This comand changes a character to an integer for th entire year column
pop %<>% mutate(year=as.integer(year))
pop
#This comand removes the 0 from fips
pop %<>% mutate(fips=str_replace_all(fips,"^0",""))
pop
#This comand changes a character to an integer 
pop %<>% mutate(fips=as.integer(fips))
pop

str(pop)

#Comand to remove state-wide summaries, remove all fips with 000
1000 %% 1000
1002 %% 1000
1000 %% 100
5 %% 2

ld %<>% gather(starts_with("Cases"),key="str_year",value="cases")
ld %<>% mutate(year=str_replace_all(str_year,"Cases",""))
ld %<>% mutate(year=as.integer(year))
ld %<>% rename(state=STNAME,county=CTYNAME)
fips.builder<-function(st,ct){
  if (str_length(ct)==3){
    fips<-paste(as.character(st),as.character(ct),sep="") %>% as.integer
  }
  else if (str_length(ct)==2){
    fips<-paste(as.character(st),"0",as.character(ct),sep="") %>% as.integer
  }
  else {
    
    fips<-paste(as.character(st),"00",as.character(ct),sep="") %>% as.integer
  }
  return(fips)
}
ld %<>% rowwise() %>% mutate(fips=fips.builder(STCODE,CTYCODE)) # takes about 10 seconds
ld %<>% select(-c(STCODE,CTYCODE,str_year))
ld

#Comand to merge or join two datasets
ld.prism <- inner_join(ld,prism)
ld.prism.pop <- inner_join(ld.prism, pop)

cases_by_year <- ld %>% ungroup %>% group_by(year) %>%
  summarize(total=sum(cases)) %>% arrange(desc(total))
                 
cases_by_year
ld

#Comand to summarize information in table
state_by_county_year <- ld %>% ungroup %>% group_by(state, county, year) %>%
  summarize(mean=mean(cases)) %>% arrange(desc(mean))

state_by_county_year
#2002 was the worst year and New York, Connecticut and Massachusetts were the most affected states)

#Comand to save file
save(ld.prism.pop, file="pop.ld.prism.rda")

write_csv(ld.prism.pop, "pop.ld.prism.csv")

county_map <- map_data("county")
state_map <- map_data("state")

#Comand to organize the population data according to fips
ag.fips <- group_by(ld.prism.pop,fips)
#Comand summarizes the ag.fips by the sum cases for the fip code
ld.16y<-summarize(ag.fips,all.cases=sum(cases))
ld.16y

#Comand adds columns for state and count to he fips in th data frame ld.16y
ld.16y<-left_join(select(ld.prism.pop,c(state,county,fips)),ld.16y)
ld.16y
#Comand returns distinct values 
ld.16y<-distinct(ld.16y)
ld.16y
#Comand renames the columns state with region and county with subregion 
ld.16y %<>% rename(region=state,subregion=county)
ld.16y
#Comand to remove county from subregion
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","")
ld.16y
#Comand 
ld.16y$region<-tolower(ld.16y$region)

ld.16y$subregion<-tolower(ld.16y$subregion)

ld.16y %<>% mutate(log10cases=log10(1+all.cases))

map.ld.16y<-left_join(county_map,ld.16y)

ggplot(map.ld.16y)+geom_point(aes(long,lat,color=log10cases),size=0.1) +
  scale_colour_gradientn(colours=rev(rainbow(4)))






