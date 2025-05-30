# Load required libraries
library(readr)       # For reading CSV files efficiently
library(dplyr)       # For data manipulation (filtering, selecting, summarizing, etc.)
library(ggplot2)     # For data visualization (creating plots)
library(tidyverse)   # A collection of R packages for data science (includes ggplot2, dplyr, readr, etc.)

# Load dataset (update path if necessary)
# Reads the CSV file from the specified path and stores it in the variable 'data'
data <- read_csv("C:/Users/hp/OneDrive/Desktop/R-programming/city_temperature.csv")  

# Displays the entire dataset in the console
data


# Returns the dimensions of the dataset (number of rows and columns)
dim(data)

# Provides summary statistics for each column in the dataset
summary(data)

# Shows the structure of the dataset, including data types for each column
str(data)

# Displays the first 6 rows of the dataset
head(data)

# Displays the last 6 rows of the dataset
tail(data)

# Counts the total number of missing (NA) values in the dataset
sum(is.na(data))

# Counts the number of missing values in each column
colSums(is.na(data))

# Counts how many duplicate rows exist in the dataset
sum(duplicated(data))

# Removes all rows with missing values (NAs) and updates the dataset
data <- na.omit(data)

# Displays the cleaned dataset (without missing values)
data

# Selects only the columns from 'clean' where less than 50% of the values are missing (NA), keeping columns with more data
#clean <- clean %>% select(where(~mean(is.na(.)) < 0.5))

# Displays the resulting cleaned dataset after filtering out columns with too many missing values
#clean 

# Removes duplicate rows from the dataset and updates 'data' with only unique entries
data <- unique(data)

# Counts how many duplicate rows exist in the cleaned dataset
sum(duplicated(data))


# 13. Histogram: Distribution of temperatures
ggplot(data, aes(x = AvgTemperature)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  labs(title = "Distribution of Average Temperatures", x = "Temperature (°F)", y = "Frequency")

# 14. Boxplot: Temperature by Country
ggplot(data, aes(x = Country, y = AvgTemperature)) +
  geom_boxplot(fill = "lightgreen") +
  coord_flip() +
  labs(title = "Average Temperature by Country", x = "Country", y = "Temperature (°F)")

# 15. Scatter Plot: Temperature vs Year
ggplot(data, aes(x = Year, y = AvgTemperature)) +
  geom_point(alpha = 0.3, color = "orange") +
  labs(title = "Temperature Trends Over Time", x = "Year", y = "Temperature (°F)")

# 16. Bar Chart: Number of Records by Country
ggplot(data, aes(x = Month)) +
  geom_bar(fill = "orange") +
  labs(title = "Number of Records by Month", x = "Month", y = "Count") +
  theme_minimal()

 # 17. Correlation Matrix for numeric columns
numeric_cols <- temp_data_clean %>% select(where(is.numeric))
cor_matrix <- cor(numeric_cols, use = "pairwise.complete.obs")
print(cor_matrix)

# 18. (Optional) Export cleaned dataset
write_csv(temp_data_clean, "C:/Users/hp/OneDrive/Desktop/R-programming/city_temperature_cleaned.csv")

# 19. (Optional) Save a plot to file
ggplot(temp_data_clean, aes(x = AvgTemperature)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  labs(title = "Distribution of Average Temperatures", x = "Temperature (°F)", y = "Frequency")



#ass 2
# Let's say we're choosing temperature as the target variable
target <- "AvgTemperature"
colnames(clean2)
cor.test(clean2$AvgTemperature, clean2$Day, method = "pearson")
cor.test(clean2$AvgTemperature, clean2$Year, method = "pearson")

clean2 <- data
#NOVE
# Convert to factor if not already
clean2$Region <- as.factor(clean2$Region)
clean2$Region

clean2$Country <- as.factor(clean2$Country)
clean2$Country

clean2$State <- as.factor(clean2$State)
clean2$State

clean2$City <- as.factor(clean2$City)
clean2$City

clean2$Month <- as.factor(clean2$Month)
clean2$Month

# Run ANOVA
summary(aov(AvgTemperature ~ Region, data = clean2))
summary(aov(AvgTemperature ~ Country, data = clean2))
summary(aov(AvgTemperature ~ State, data = clean2))
summary(aov(AvgTemperature ~ City, data = clean2))
summary(aov(AvgTemperature ~ Month, data = clean2))