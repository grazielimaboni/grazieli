#How to save csv files into Rproj
library(ggplot2)
#Comand to load csv data from the working directory, no need to include everything because the file was already inside the Rproj
cases <- read_csv("cases.csv")

#Comand to make a graph using the file that was loaded and saved above
ggplot(data=cases) +
  geom_bar(mapping=aes(x=country))

#In order to commit the files and send to the online Git, use Commit, click on the files, write a name for the commit and push changes


