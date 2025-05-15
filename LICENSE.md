# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(haven)

 # 1. Load dataset (update path if necessary)
temp_data <- read_csv("C:/Users/hp/OneDrive/Desktop/R-programming/city_temperature.csv")
temp_data
# 2. View the entire dataset in a scrollable viewer (safe for large data)
View(temp_data)

# 3. Show first and last 6 rows
head(temp_data)
tail(temp_data)

# 4. Show number of rows and columns
dim(temp_data)

# 5. Show structure of each column
str(temp_data)

# 6. Show summary statistics
summary(temp_data)

# 7. Count missing values in each column
colSums(is.na(temp_data))

# 8. Count total missing values
sum(is.na(temp_data))

# 9. Count duplicate rows
sum(duplicated(temp_data))
temp_data_clean <- na.omit(temp_data)
temp_data_clean

# 10. Clean the dataset: remove rows with missing AvgTemperature and duplicates
temp_data_clean <- temp_data %>%
  filter(!is.na(AvgTemperature)) %>%
  distinct()

# 11. Create date column and extract Year again (if needed)
#temp_data_clean$Date <- as.Date(paste(temp_data_clean$Year, temp_data_clean$Month, temp_data_clean$Day, sep = "-"))
#temp_data_clean$Year <- as.integer(format(temp_data_clean$Date, "%Y"))

# 12. Show sample of cleaned data
head(temp_data_clean)

# 13. Histogram: Distribution of temperatures
ggplot(temp_data_clean, aes(x = AvgTemperature)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  labs(title = "Distribution of Average Temperatures", x = "Temperature (°F)", y = "Frequency")

# 14. Boxplot: Temperature by Country
ggplot(temp_data_clean, aes(x = Country, y = AvgTemperature)) +
  geom_boxplot(fill = "lightgreen") +
  coord_flip() +
  labs(title = "Average Temperature by Country", x = "Country", y = "Temperature (°F)")

# 15. Scatter Plot: Temperature vs Year
ggplot(temp_data_clean, aes(x = Year, y = AvgTemperature)) +
  geom_point(alpha = 0.3, color = "orange") +
  labs(title = "Temperature Trends Over Time", x = "Year", y = "Temperature (°F)")

# 16. Bar Chart: Number of Records by Country
ggplot(temp_data_clean, aes(x = Country)) +
  geom_bar(fill = "skyblue") +
  coord_flip() +
  labs(title = "Number of Records by Country", x = "Country", y = "Count")

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

ggsave("C:/Users/hp/OneDrive/Desktop/R-programming/temperature_histogram.png")

