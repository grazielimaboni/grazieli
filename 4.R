library(tidyverse)
library(magrittr)
library(GGally)
#How to import the data that were merged before and saved as cvs
pop.ld.prism <- read_csv("C:/Users/workshop/Desktop/pop.ld.prism.csv")

#Comand to creat the plots
ggpairs(ld.prism.pop,columns=c("prcp","avtemp","size", "cases"))

#Comand to log the data and creat two new columns of log10size data and log10 cases data
ld.prism.pop %<>% mutate(log10size=log10(size))
ld.prism.pop
range(ld.prism.pop$size)
range(ld.prism.pop$cases)
log10(1)

ld.prism.pop %<>% mutate(log10cases=log10(cases+1))

ggpairs(ld.prism.pop,columns=c("prcp","avtemp","log10size", "log10cases"))

#Comand to creat a new data frame with 100 rows
set.seed(222)
smalldata<-ld.prism.pop %>% sample_n(100)

#Comand to plot the data precipitation and average temperature in a linear model
myPlot <- ggplot(data=smalldata,mapping=aes(x=prcp, y=avtemp)) +
  geom_point()
myPlot  
myPlot+geom_smooth(method="lm")

#Comand to create a linear model (lm) in order to obtain residuals and coefficients including estimate, standard error and significant codes etc
myModel <- lm(avtemp ~ prcp, data = smalldata)
summary(myModel)

lm(formula=avtemp ~ prcp, data = smalldata)

#Comand to generate a ggplot of total population size by year
ld.prism.pop %>% group_by(year) %>% summarize(total=sum(size)) %>%
  ggplot(.)+geom_point(aes(x=year,y=total))

#Comand to create a new data frame that groups the data by state
by_state <-ld.prism.pop %>% group_by(state)
by_state

#Comand to nest the data frame created above
by_state %<>% nest
by_state

#Comand to save the data in rds format 
saveRDS(by_state, "by_state.rds")
test <- readRDS("by_state.rds")

linGrowth_model <- function(df){
  lm(size ~ year, data = df)
}
models <- purrr::map(by_state$data, linGrowth_model)
models
by_state %<>% mutate(model = map(data, linGrowth_model))
by_state
library(modelr)
by_state %<>% mutate(resids = map2(data, model, add_residuals))
