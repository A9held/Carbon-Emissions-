#install.packages(c("dplyr","lubridate", "ggplot2"))
library(dplyr)
library(lubridate)
library(ggplot2)

dotCO2 = read.csv("/cloud/project/annual-co-emissions-by-region.csv")
dotCO2_CCtemp = read.csv("/cloud/project/climate-change.csv")
int = read.csv("/cloud/project/share-of-individuals-using-the-internet.csv")
colnames(int) [4] = "IUI"
colnames(int)

colnames(dotCO2) [4] = "CO2"
colnames(dotCO2)

colnames(dotCO2_CCtemp)[4] = "TA"
colnames(dotCO2_CCtemp)

dotCO2$Entity = as.factor(dotCO2$Entity)
name.Ent = levels(dotCO2$Entity)
name.Ent

dotCO2_CCtemp$Entity = as.factor(dotCO2_CCtemp$Entity)
class.ent = levels(dotCO2_CCtemp$Entity)
class.ent

plot(dotCO2$Year, dotCO2$CO2)
plot(dotCO2$Year, dotCO2$CO2, type = "b", 
     xlab = "Year", ylab= "CO2 emissions (tons)")

US = dotCO2[dotCO2$Entity == "United States",]
ME = dotCO2[dotCO2$Entity == "Mexico",]

# Plot of US CO2
plot(US$Year, #x axis data
     US$CO2, #y axis data
     type = "b", #b = points and lines
     pch = 19, #symbol shape
     ylab = "Annual fossil fuel emissions (tons CO2)", #y axis label
     xlab = "Year", #x axis label 
     yaxt = "n") #turn off y axis

#add y axis arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
#las = 2 changes the labels to be read in horizontal direction
axis(2, seq(0,6000000000, by=2000000000), #location of ticks 
     seq(0,6, by = 2), #label for ticks 
     las=2)

NA_CO = dotCO2 %>%
  filter(Entity == "United States"|
         Entity =="Mexico"| Entity == "Canada")

ggplot(dotCO2,
       aes(x =Year, y=CO2, color=Entity))+
  geom_line()+
  labs(x= "Year", y="CO2 emissions")+
  theme_classic()

ggplot(dotCO2,
       aes(x =Year, y=CO2, color=Entity))+
  geom_line()+
  labs(x= "Year",
       y=expression(paste("CO"[2], "emissions(tons)")))+
  theme_classic()



NorthA = dotCO2[dotCO2$Entity == "United States"|
                dotCO2$Entity == "Canada"|
                dotCO2$Entity == "Mexico",]
ggplot(data = NorthA, #data for plot 
       aes(x = Year, y=CO2, color=Entity)) + #aes, x and y 
  geom_point()+ #make points at data point
  geom_line()+ #use lines to connect data points
  labs(x = "year", y = "US fossil fuel emissions (tons CO2)")+ 
  theme_classic()

#subset CO2 to meet conditions
compCO2 = dotCO2[dotCO2$Year>= 1950 & dotCO2$Entity =="France"|
                   dotCO2$Year>= 1950 & dotCO2$Entity =="India"|
                   dotCO2$Year>= 1950 & dotCO2$Entity =="Russia",]

#Area plot 
ggplot(data=compCO2, aes(x=Year, y=CO2, fill =Entity))+ #data
  geom_area() #Geometry

ggplot(data=compCO2,
       aes(x=Year, ymin=0, ymax=CO2, fill =Entity))+ #fill works for polygons/shaded areas
  geom_ribbon(alpha=0.5)+ #fill with 50% transparency
  labs(x="Year", y="Annual emissions (tons CO2)")


b = ggplot(data=compCO2,
       aes(x=Year, ymin=0, ymax=CO2, fill =Entity))+ #fill works for polygons/shaded areas
  geom_ribbon(alpha=0.5)+ #fill with 50% transparency
  labs(x="Year", y="Annual emissions (tons CO2)")
b + annotate("segment", #line label
             x = 1991, #startx coordinate
             y= 2450000000, #start y coordinate
             xend=1991,
             yend= 2600000000)+

  annotate("text", #add text label
         x= 1991, #center of lanel x coordinate
         y = 2700000000, #center of label y coordinate
         label = "End of USSR")

#Question 1: Comparing US emissions to Germany

USGERMANYCO2 = dotCO2[dotCO2$Year>= 1950 & dotCO2$Entity =="United States"|
                   dotCO2$Year>= 1950 & dotCO2$Entity =="Germany",]


ggplot(data = USGERMANYCO2, #data for plot 
       aes(x = Year, y=CO2, color=Entity)) + #aes, x and y 
  geom_point()+ #make points at data point
  geom_line()+ #use lines to connect data points
  labs(x = "year", y = "Fossil fuel emissions (tons CO2)")+ 
  theme_classic()

# Question 2
#world temperature anomaly 1950- 2021-12-15
World = dotCO2_CCtemp[dotCO2_CCtemp$Day>= 1950-01-15 & dotCO2_CCtemp$Entity == "World",]

ggplot(World,
       aes(x =Day, y=TA, group=1))+
  geom_line()+
  labs(x= "Day", y="Temperature Anomaly")+
  theme_classic()

#World CO2 
WorldCO2 = dotCO2[dotCO2$Year>= 1950 & dotCO2$Entity == "World",]

ggplot(WorldCO2,
       aes(x =Year, y=CO2))+
  geom_line()+
  labs(x= "Year", y="Fossil Fuel Emissions (tons CO2)")+
  theme_classic()

#Question 3
#Individuals using the internet

ggplot(WorldIUI,
       aes(x =Year, y=IUI))+
  geom_line()+
  labs(x= "Year", y="Percent of Individuals Using the Internet (Globally)")+
  theme_classic()
