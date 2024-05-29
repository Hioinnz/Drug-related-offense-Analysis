crime_data <- read.csv("DrugCrime_data.csv", fileEncoding = "UTF-16LE", sep = "\t")

library(ggplot2)
library(dplyr)

# Rename the "Year.Month" variable to "Date"
names(crime_data)[names(crime_data) == "Anzsoc.Division"] <- "Division"
names(crime_data)[names(crime_data) == "Police.District"] <- "District"

# Convert the "Year.Month" column to a Date type and extract the year
crime_data$Date <- as.Date(crime_data$Year.Month, format = "%Y.%m.%d")


# Create a table of crime counts per year
crime_per_year <- table(crime_data$Date)
crime_per_year <- as.data.frame(crime_per_year)

# Rename the variables in crime_per_year
colnames(crime_per_year) <- c('Year', 'Count')

# Extract the year from the Date column
crime_per_year$Year <- substring(crime_per_year$Year, 1, 7)

# Extract the year from the Date column
crime_data$Year <- format(crime_data$Date, "%Y")

# Create a table of crime counts per year
crime_per_year <- crime_data %>%
  group_by(Year) %>%
  summarise(Count = n())

# Calculate the total offenses for each year
crime_per_year <- crime_per_year %>%
  mutate(Total = sum(Count))

# Order the data by year
crime_per_year <- crime_per_year[order(crime_per_year$Year),]

print(crime_per_year)

# Plot the data
ggplot(data = crime_per_year, aes(x = as.numeric(Year), y = Count, group = 1)) +
  geom_line() +
  labs(title = "Trend of Drug-Related Offenses", x = "Year", y = "Total Offenses") +
  scale_x_continuous(breaks = seq(min(as.numeric(crime_per_year$Year)), max(as.numeric(crime_per_year$Year)), by = 2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



drugtype_data <- read.csv("DrugType_data.csv", fileEncoding = "UTF-16LE", sep = "\t")


library(ggplot2)
library(dplyr)

# Rename variables
names(drugtype_data)[names(drugtype_data) == "Drug.Type"] <- "Type"
names(drugtype_data)[names(drugtype_data) == "Month.Year"] <- "Date"

# Extract the year from the Date column
drugtype_data$Year <- substring(drugtype_data$Date, 7, 10)

# Group the data by year and drug type, and calculate the total crimes
yearly_totals <- drugtype_data %>%
  group_by(Year, Type) %>%
  summarize(Total_Crimes = n(), .groups = 'drop')

# Convert Total_Crimes to numeric type
yearly_totals$Total_Crimes <- as.numeric(yearly_totals$Total_Crimes)

# Plot the line graph using ggplot
ggplot(data = yearly_totals, aes(x = Year, y = Total_Crimes, group = Type, color = Type)) +
  geom_line() +
  labs(title = "Trend of Drug-related offenses by drug type", x = "Year", y = "Total offenses") +
  scale_color_discrete(name = "Drug Type")


drugtype_data <- read.csv("DrugType_data.csv", fileEncoding = "UTF-16LE", sep = "\t")

library(ggplot2)
library(dplyr)


#Convert to a data frame
crime_by_district = table(drugtype_data$District)
crime_by_district = as.data.frame(crime_by_district)

colnames(crime_by_district) <- c("District", "Count")

# Plot the total number of crime by district using ggplot
ggplot(crime_by_district, aes(x = District, y = Count)) +
  geom_bar(stat = "identity", fill = "orange", width = 0.8) +
  labs(title = "Drug-related offenses by District (2013-2021)", x = "District", y = "Total number of drug-realted offenses")





