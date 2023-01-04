rm(list=ls())
library(dplyr)
library(ggplot2)

#url <- 'https://covid.ourworldindata.org/data/owid-covid-data.csv'
#destfile <- 'C:\\Users\\User\\Documents\\Fakultet\\Usluge zasnovane na lokaciji\\owid-covid-data.csv'
#download.file(url,destfile)
data <- as.data.frame(read.csv('C:\\Users\\User\\Documents\\Fakultet\\Usluge zasnovane na lokaciji\\owid-covid-data.csv', header=TRUE, sep = ','))
chosenDay<- filter(data, date == "2020-10-21")
dayBefore <- filter(data, date=="2020-10-20")

chosenDay[is.na(chosenDay)] = 0
chosenDay$location[chosenDay$location=="United States"] = "USA"
chosenDay$location[chosenDay$location=="United Kingdom"] = "UK"

dayBefore[is.na(dayBefore)] = 0
dayBefore$location[dayBefore$location=="United States"] = "USA"
dayBefore$location[dayBefore$location=="United Kingdom"] = "UK"

casesDifference =(chosenDay$new_cases - dayBefore$new_cases) 

world <- map_data("world")

world$variable <- casesDifference[match(world$region,chosenDay$location)]
breaks = c(0)

#newWorld <- world[order(world[['variable']]), ] 
#print(newWorld)
#print(world)

world_map <- ggplot(world, aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=variable), col="white",lwd=0.1)
#world_map + scale_fill_gradient(low='darkblue', high='red')

#Moguće rješenje za problem sa bojama
# colors prima niz boja 
# scales::rescale prima brojeve koji označavaju intervale za pojedinu boju
world_map + scale_fill_gradientn(colors = c("#0093ff","#c0e4ff", "#ffa6a6", "#ff0000"),
      values =scales::rescale(c(-2000,-0.1, 0.1,6000)))

