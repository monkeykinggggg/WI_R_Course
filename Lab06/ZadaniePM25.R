getwd()
unzip("data_NEI_data.zip")
file1="data_NEI_data/summarySCC_PM25.rds"
file2="data_NEI_data/Source_Classification_Code.rds"
NEI <- readRDS(file1)
SCC <- readRDS(file2) 
str(SCC) # dataframe

#######################

library(dplyr)
total_emissions = NEI %>%group_by(year) %>%summarise(Total_Emissions = sum(Emissions))
png("plot1.png")
ggplot(data=total_emissions, aes(x=factor(year), y=Total_Emissions))+
  geom_col(fill="steelblue")+labs(title = "Całkowita emisja PM2.5",x="Rok",y="Emisja [t]")+
  geom_hline(yintercept = total_emissions$Total_Emissions[4], linetype="dashed", col="red")
dev.off()

####

library(ggplot2)
baltimore_data = subset(NEI, fips=="24510")
total_emissions_for_baltimore <- baltimore_data %>%
  group_by(year) %>%
  summarise(Total_Emissions = sum(Emissions))
png("plot2.png")
ggplot(data=total_emissions_for_baltimore, aes(x=factor(year), y=Total_Emissions))+
  geom_col(fill="steelblue")+
  labs(title = "Całkowita emisja PM2.5 w Baltimore",x="Rok",y="Emisja [t]")
dev.off()

####

baltimore_data = subset(NEI, fips=="24510")
baltimore_by_type_emissions = baltimore_data %>%
  summarise(Total_Emissions = sum(Emissions),.by = c(year, type))
png("plot3.png")
ggplot(baltimore_by_type_emissions, aes(x=factor(year), y=Total_Emissions, col=type)) +
  geom_col() +
  facet_grid(. ~ type) +
  labs(title="Emisja w Baltimore wg typu źródła",x="Rok", y="Emisja [t]")+
  theme(legend.position="none")
dev.off()

####
View(SCC)
whether_coal_word_exists  <- grepl("coal", SCC$EI.Sector, ignore.case=TRUE)
coal_scc <- SCC[whether_coal_word_exists, ]$SCC
nei_with_coal <- NEI[NEI$SCC %in% coal_scc, ]
total_coal_emissions = nei_with_coal %>%
  group_by(year) %>%
  summarise(Total_Emissions = sum(Emissions))

png("plot4.png")
ggplot(data=total_coal_emissions, aes(x=factor(year), y=Total_Emissions))+
  geom_col(fill="steelblue")+labs(title = "Emisja PM2.5 ze zrodel zwiazanych z Coal",x="Rok",y="Emisja [t]")
dev.off()

####

# mozna brac po wszystkich kolumnach
whether_vehicle_word_exists  <- grepl("vehicle", SCC$EI.Sector, ignore.case=TRUE)
vehicle_scc <- SCC[whether_vehicle_word_exists, ]$SCC
nei_with_vehicle <- NEI[NEI$SCC %in% vehicle_scc, ]
baltimore_vehicle_data = subset(nei_with_vehicle, fips=="24510")
vehicle_emissions_for_baltimore <- baltimore_vehicle_data %>%
  group_by(year) %>%
  summarise(Total_Emissions = sum(Emissions))
png("plot5.png")
ggplot(data=vehicle_emissions_for_baltimore, aes(x=factor(year), y=Total_Emissions))+
  geom_col(fill="steelblue")+
  labs(title = "Całkowita emisja PM2.5 w Baltimore z pojazdow samochodowych",x="Rok",y="Emisja [t]")
dev.off()

####
compare_nei <- nei_with_vehicle[(nei_with_vehicle$fips == "24510" | nei_with_vehicle$fips == "06037"), ]
vehicles_between_two_cities = compare_nei %>%
  summarise(Total_Emissions = sum(Emissions),.by = c(year, fips))
png("plot6.png")
ggplot(vehicles_between_two_cities, aes(x=factor(year), y=Total_Emissions, col=fips)) +
  geom_col() +
  facet_grid(. ~ fips, labeller=label_bquote(fips)) +
  labs(title="Porownanie emisji z motoryzacji miedzy miastami",x="Rok", y="Emisja z pojazdow [t]")
dev.off()