#How to save csv files into Rproj
library(ggplot2)
cases <- read_csv("cases.csv")

ggplot(data=cases) +
  geom_bar(mapping=aes(x=country))

