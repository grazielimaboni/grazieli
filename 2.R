wnv<- read_csv("C:/Users/workshop/Desktop/wnv.csv")
ggplot(data=wnv) +
  geom_histogram(aes(x=log(Total))) +
  labs(x='Total', y='Frequency')+
  scale_x_log10()
ggplot(data=wnv) +
  geom_bar(mapping=aes(x=Year, y=Total, fill=State), stat="identity")+
  scale_x_continuous(breaks=seq(1999,2007,1))
  
  wnv$CFR<-(wnv$Fatal/wnv$Total)

ggplot(data=wnv) +
  geom_histogram(aes(x=CFR))
  
  
mean<-function(x){
  s <- sum(x)
  n <- length(x)
  m <- s/n
  return(m)
  }

mean(wnv$EncephMen)

for(y in 1999:2007){
  States <- lenght(wnv$Total[wnv$Year==y])
  
  #Total number of states reporting cases
  states <- lenght(wnv$Total[wnv$Year == y])
  
  #total number of reported cases
  
  #total number of fatalities
  #and case fatality rate
  print(States)
}

