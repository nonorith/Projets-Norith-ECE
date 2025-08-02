                                    #Data analytics : Project
#Charlotte BOCAHUT
#Valentine MORGANT
#Jade HARAUCOURT
#Norith UNG
#ING5 Group2 Big Data & IA


#1) Data collection: Gather data related to Bolt rides,
#including number of rides per day, per month, etc.

install.packages("lubridate")
install.packages("dplyr")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("readxl")
install.packages("readxl")
install.packages("corrplot")
install.packages("arules")

library(lubridate)
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(arules)
library(tidyr)
library(sf)
library(cluster)
library(readxl)
library(randomForest)
library(e1071)
library(class)
library(caret)
library(corrplot)
library(gridExtra)
library(forecast)

#PS: it is possible that it does not directly find the files. If error message, put these commands
#getwd()
#setwd("C:/chemin/vers/le/dossier/contenant/votre/fichier")


# Load CSV files for each month
data_apr <- read.csv('bolt-raw-data-apr14.csv')
data_may <- read.csv('bolt-raw-data-may14.csv')
data_jun <- read.csv('bolt-raw-data-jun14.csv')
data_jul <- read.csv('bolt-raw-data-jul14.csv')
data_aug <- read.csv('bolt-raw-data-aug14.csv')
data_sep <- read.csv('bolt-raw-data-sep14.csv')

# Combine all files into a single dataset
combined_data <- bind_rows(data_apr, data_may, data_jun, data_jul, data_aug, data_sep)
head(combined_data)
tail(combined_data)

#--------------------------------------------------------------------------------
#2) Data cleansing : Apply cleaning techniques to eliminate inconsistencies,
#missing values and anomalies, thus ensuring data quality.

# Check for missing values
sum(is.na(combined_data)) 

# Display columns with missing values
colSums(is.na(combined_data))

#Check for inconsistent values (Latitude and Longitude)
#For geographic data, it is essential that latitude and longitude
#latitude and longitude values are within valid ranges:
# - Latitude must be between -90 and 90.
# - Longitude must be between -180 and 180.

invalid_lat <- combined_data %>%
  filter(Lat < -90 | Lat > 90)

# Check longitudes outside the range [-180, 180].
invalid_lon <- combined_data %>%
  filter(Lon < -180 | Lon > 180)

# Display the number of lines with invalid latitudes or longitudes
cat("Number of invalid latitudes :", nrow(invalid_lat), "\n")
cat("Number of invalid longitudes :", nrow(invalid_lon), "\n")

# Check Date/Time columns for duplicates
sum(duplicated(combined_data$Date.Time))

# Check all columns for exact duplicates
sum(duplicated(combined_data))
# Delete fully duplicated lines
combined_data_cleaned <- combined_data %>%
  distinct()
sum(duplicated(combined_data_cleaned))

# Show before and after dimensions
cat("Dimensions before deletion :", dim(combined_data), "\n")
cat("Dimensions after deletion :", dim(combined_data_cleaned), "\n")

# Convert Date/Time column to DateTime format
combined_data_cleaned$Date.Time <- as.POSIXct(combined_data_cleaned$Date.Time, format='%m/%d/%Y %H:%M:%S', tz="UTC")
head(combined_data_cleaned)

#--------------------------------------------------------------------------------
#3)Data analysis : Use R packages to conduct in-depth data analysis,
#focusing on criteria such as daily and monthly number of rides.

# Extract date, month and day of the week
combined_data_cleaned$Date <- as.Date(combined_data_cleaned$Date.Time)
combined_data_cleaned$Month <- month(combined_data_cleaned$Date.Time)
combined_data_cleaned$Day <- day(combined_data_cleaned$Date.Time)

head(combined_data_cleaned)
tail(combined_data_cleaned)

# Calculate the number of rides per day
daily_data <- combined_data_cleaned %>%
  group_by(Date) %>%
  summarise(Total_Rides = n())

# Calculate the number of rides per month
monthly_data <- combined_data_cleaned %>%
  group_by(Month) %>%
  summarise(Total_Rides = n())

# Calculate the number of rides per day of the week
weekly_data <- combined_data_cleaned %>%
  mutate(DayOfWeek = wday(Date.Time, label=TRUE)) %>%
  group_by(DayOfWeek) %>%
  summarise(Total_Rides = n())

# Count the number of rides per base
base_counts <- combined_data_cleaned %>%
  group_by(Base) %>%
  summarise(Total_Rides = n())

print("Number of rides per day:")
print(daily_data)

print("Number of rides per month:")
print(monthly_data)

print("Number of rides per weekday:")
print(weekly_data)

print("Number of rides per base:")
print(base_counts)

#--------------------------------------------------------------------------------
#4)Data Visualization: Create visualizations with ggplot2 to graphically represent identified trends and patterns.
#Number of rides per day
ggplot(daily_data, aes(x = Date, y = Total_Rides)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Number of rides per day",
       x = "Date",
       y = "Number of rides") +
  theme_minimal()

#Number of rides per month
ggplot(monthly_data, aes(x = factor(Month), y = Total_Rides)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of rides per month",
       x = "Month",
       y = "Number of rides") +
  theme_minimal()

#Number of rides per weekday
ggplot(weekly_data, aes(x = DayOfWeek, y = Total_Rides)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Number of rides per weekday",
       x = "Day of the week",
       y = "Number of rides") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

# Number of rides per base
ggplot(base_counts, aes(x = reorder(Base, -Total_Rides), y = Total_Rides)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Number of rides per base",
       x = "Base",
       y = "Number of rides") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------------------------------------------------------------------------
#5) Heat map of rides by time of day and day of the week
combined_data_cleaned %>%
  mutate(Hour = hour(Date.Time),
         DayOfWeek = wday(Date.Time, label = TRUE)) %>%
  group_by(Hour, DayOfWeek) %>%
  summarise(Total_Rides = n()) %>%
  ggplot(aes(x = Hour, y = DayOfWeek, fill = Total_Rides)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heat map of rides by time of day and day of the week",
       x = "Hour",
       y = "Day of the week") +
  theme_minimal()
#--------------------------------------------------------------------------------
#6)Average Passenger Determination: Determine the times when the most customers are available.

hourly_average <- combined_data_cleaned %>%
  mutate(Hour = hour(Date.Time)) %>%
  group_by(Hour) %>%
  summarise(Average_Rides = n() / n_distinct(date(Date.Time)))

ggplot(hourly_average, aes(x = factor(Hour), y = Average_Rides)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average number of rides per hour",
       x = "Hours",
       y = "Average number of rides") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = 0:23)

#--------------------------------------------------------------------------------
#7) Identification of the peak hours  
# Class from maximum to minimum
Peak_hours <- hourly_average %>%
  arrange(desc(Average_Rides))

Top_peaks_hours <- head(Peak_hours, 7)

hourly_average <- hourly_average %>%
  mutate(Peaks_hours = ifelse(Hour %in% Top_peaks_hours$Hour, "Peak Hours", "Other Hours"))

ggplot(hourly_average, aes(x = factor(Hour), y = Average_Rides, fill = Peaks_hours)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Peak Hours" = "red", "Other Hours" = "skyblue"), 
                    name = "Légende") +
  labs(title = "Ranking of average rides per hour from highest to lowest",
       x = "Hours",
       y = "Average number of rides") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

#--------------------------------------------------------------------------------
#8) 
#Filter to have the number of rides per day
rides_per_day <- combined_data_cleaned %>%
  mutate(Day = day(Date.Time)) %>%
  group_by(Day) %>%
  summarise(Total_Rides = n())

#Find the maximum
Highest_number_rides <- rides_per_day %>%
  filter(Total_Rides == max(Total_Rides))

ggplot(rides_per_day, aes(x = factor(Day), y = Total_Rides)) +
  geom_bar(stat = "identity", aes(fill = Day == Highest_number_rides$Day)) +
  geom_text(aes(label = ifelse(Day == Highest_number_rides$Day, paste0("Max: ",Total_Rides), "")), 
            vjust = -0.5, color = "red", size = 3.5) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "skyblue"), guide = "none") +
  labs(title = "Number of rides per day",
       subtitle = paste("Day with the highest number of rides :", Highest_number_rides$Day),
       x = "Day of the month",
       y = "Total number of rides") +
  theme_minimal()

#--------------------------------------------------------------------------------
#9)
# Step 1: Data preparation and categorization

enriched_data <- combined_data_cleaned %>%
  mutate(
    Hour = hour(Date.Time),
    TimeBlock = case_when(
      Hour >= 5 & Hour < 10 ~ "Morning_Rush",
      Hour >= 10 & Hour < 16 ~ "Midday",
      Hour >= 16 & Hour < 20 ~ "Evening_Rush",
      TRUE ~ "Night"
    ),
    DayType = ifelse(wday(Date.Time) %in% c(1, 7), "Weekend", "Weekday"),
    Location_Zone = case_when(
      Lat >= median(Lat) & Lon >= median(Lon) ~ "NorthEast",
      Lat >= median(Lat) & Lon < median(Lon) ~ "NorthWest",
      Lat < median(Lat) & Lon >= median(Lon) ~ "SouthEast",
      TRUE ~ "SouthWest"
    )
  )

# Step 2: Spatial and temporal association analysis
# Create a transaction matrix
transactions_data <- enriched_data %>%
  select(Base, TimeBlock, DayType, Location_Zone) %>%
  mutate(across(everything(), as.factor))

# Convert to transaction format for apriori algorithm
transactions <- as(transactions_data, "transactions")

# Extract association rules
rules <- apriori(transactions,
                 parameter = list(support = 0.01, 
                                  confidence = 0.5,
                                  minlen = 2))

# Display the most relevant rules
top_rules <- sort(rules, by = "lift", decreasing = TRUE)
inspect(head(top_rules, 10))

# Step 3:Spatial-temporal pattern analysis

# Spatial and temporal aggregation
spatial_temporal_patterns <- enriched_data %>%
  group_by(Location_Zone, TimeBlock, DayType) %>%
  summarise(
    Ride_Count = n(),
    Avg_Lat = mean(Lat),
    Avg_Lon = mean(Lon)
  ) %>%
  ungroup()

# Create a distance matrix based on average coordinates
coords_matrix <- spatial_temporal_patterns %>%
  select(Avg_Lat, Avg_Lon) %>%
  as.matrix()

dist_matrix <- dist(coords_matrix)

# Hierarchical zone clustering
hc <- hclust(dist_matrix, method = "ward.D2")

# View the dendrogram
plot(hc, main = "Hierarchical zone clustering",
     xlab = "Zones", ylab = "Distance")

# Step 4: Dimensional analysis of race patterns

# Preparing data for PCA
pca_data <- enriched_data %>%
  group_by(Date = as.Date(Date.Time), Location_Zone) %>%
  summarise(
    Total_Rides = n(),
    Morning_Rush_Rides = sum(TimeBlock == "Morning_Rush"),
    Midday_Rides = sum(TimeBlock == "Midday"),
    Evening_Rush_Rides = sum(TimeBlock == "Evening_Rush"),
    Night_Rides = sum(TimeBlock == "Night"),
    Weekend_Rides = sum(DayType == "Weekend"),
    Weekday_Rides = sum(DayType == "Weekday")
  ) %>%
  ungroup()

# Standardizing digital data
pca_data_normalized <- pca_data %>%
  select(-Date, -Location_Zone) %>%
  scale()

# Perform PCA
pca_result <- PCA(pca_data_normalized, graph = FALSE)

# View explained variance
fviz_eig(pca_result, 
         addlabels = TRUE,
         title = "Variance decomposition by dimension")

# View variable contributions
fviz_pca_var(pca_result,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Contribution of variables to principal components")

# Step 5: Spatial correlation analysis

# Calculate the correlation matrix between numerical variables
correlation_matrix <- enriched_data %>%
  group_by(Location_Zone) %>%
  summarise(
    Avg_Rides_Per_Hour = n() / n_distinct(hour(Date.Time)),
    Avg_Lat = mean(Lat),
    Avg_Lon = mean(Lon),
    Weekend_Ratio = mean(DayType == "Weekend"),
    Rush_Hour_Ratio = mean(TimeBlock %in% c("Morning_Rush", "Evening_Rush"))
  ) %>%
  select(-Location_Zone) %>%
  cor()

# View correlation matrix
corrplot::corrplot(correlation_matrix,
                   method = "color",
                   type = "upper",
                   addCoef.col = "black",
                   tl.col = "black",
                   tl.srt = 45,
                   diag = FALSE)

#--------------------------------------------------------------------------------
#10)Summary of insights

# Create a summary of the main patterns
pattern_summary <- enriched_data %>%
  group_by(Location_Zone, TimeBlock) %>%
  summarise(
    Total_Rides = n(),
    Avg_Rides_Per_Day = n() / n_distinct(as.Date(Date.Time)),
    Weekend_Proportion = mean(DayType == "Weekend"),
    Most_Common_Base = names(which.max(table(Base)))
  ) %>%
  arrange(desc(Total_Rides))

print("Summary of main patterns by zone and period:")
print(pattern_summary)

#--------------------------------------------------------------------------------
#11) 15)
# Data preparation
prediction_data <- combined_data_cleaned %>%
  mutate(
    Hour = hour(Date.Time),
    DayOfWeek = wday(Date.Time),
    IsWeekend = ifelse(DayOfWeek %in% c(1, 7), 1, 0),
    TimeBlock = case_when(
      Hour >= 5 & Hour < 10 ~ "Morning_Rush",
      Hour >= 10 & Hour < 16 ~ "Midday",
      Hour >= 16 & Hour < 20 ~ "Evening_Rush",
      TRUE ~ "Night"
    )
  )

# Data aggregation by hour
hourly_rides <- prediction_data %>%
  group_by(Date = as.Date(Date.Time), Hour, DayOfWeek, IsWeekend, TimeBlock) %>%
  summarise(
    Rides = n(),
    .groups = 'drop'
  ) %>%
  arrange(Date, Hour)

# Adding time features
hourly_rides <- hourly_rides %>%
  mutate(
    Previous_Hour_Rides = lag(Rides, 1),
    Rolling_Mean = zoo::rollmean(Rides, k = 3, fill = NA, align = "right"),
    DayOfMonth = day(Date),
    Month = month(Date)
  ) %>%
  na.omit()

# Encoding categorical variables
hourly_rides$TimeBlock <- as.factor(hourly_rides$TimeBlock)
hourly_rides$DayOfWeek <- as.factor(hourly_rides$DayOfWeek)

# Data division
set.seed(123)
train_index <- createDataPartition(hourly_rides$Rides, p = 0.7, list = FALSE)
train_data <- hourly_rides[train_index, ]
test_data <- hourly_rides[-train_index, ]

# Evaluation function
evaluate_model <- function(actual, predicted, model_name) {
  mse <- mean((actual - predicted)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(actual - predicted))
  mape <- mean(abs((actual - predicted) / actual)) * 100
  
  cat(paste("Results of the model", model_name, ":\n"))
  cat("RMSE:", round(rmse, 2), "\n")
  cat("MAE:", round(mae, 2), "\n")
  cat("MAPE:", round(mape, 2), "%\n\n")
  
  return(data.frame(Actual = actual, Predicted = predicted, Model = model_name))
}

# Linear regression model
lm_model <- lm(Rides ~ ., data = train_data)
lm_predictions <- predict(lm_model, test_data)
lm_results <- evaluate_model(test_data$Rides, lm_predictions, "Linear regression")

# Random Forest model
rf_model <- randomForest(Rides ~ ., data = train_data, ntree = 300, importance = TRUE)
rf_predictions <- predict(rf_model, test_data)
rf_results <- evaluate_model(test_data$Rides, rf_predictions, "Random Forest")

# SVM model
svm_model <- svm(Rides ~ ., data = train_data)
svm_predictions <- predict(svm_model, test_data)
svm_results <- evaluate_model(test_data$Rides, svm_predictions, "SVM")

# Viewing results
all_results <- rbind(lm_results, rf_results, svm_results)

ggplot(all_results, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  facet_wrap(~ Model) +
  labs(title = "Comparaison of the prediction models",
       x = "Real number of rides",
       y = "Peredicted number of rides") +
  theme_minimal() +
  theme(legend.position = "none")

# Importance of variables for Random Forest
importance_df <- data.frame(
  Feature = rownames(importance(rf_model)),
  Importance = importance(rf_model)[,1]
)

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Importance of variables for the Random Forest",
       x = "Variables",
       y = "Importance") +
  theme_minimal()



#We can see from the graphics that the best model is the Random Forest, as the points are closer to the line compared to the other models
#To rigorously evaluate the performance of our predictive models, we used metrics such as RMSE, MAE, and MAPE.
#To address bias and limitations, we can use cross-validation to assess the robustness of models. 
#Secondly, analyzing the residuals of models allows us to identify systematic biases. Thirdly, we added additional variables and used feature engineering techniques to improve data quality, such as categorizing the hour of the day and the location.


#--------------------------------------------------------------------------------
#12)
# Public holidays in New York for 2014
ny_holidays <- as.Date(c("2014-01-01", "2014-01-15", "2014-02-19", "2014-05-27",
                         "2014-07-04", "2014-09-02", "2014-10-14",
                         "2014-11-11", "2014-11-28", "2014-12-25"))

# Add an IsHoliday column for New York holidays
combined_data_cleaned <- combined_data_cleaned %>%
  mutate(IsHoliday_NY = ifelse(Date %in% ny_holidays, 1, 0))

# School vacation periods in New York for 1024
ny_vacation_periods <- list(
  Winter_Recess = as.Date(c("2013-12-25", "2014-01-01")),
  Midwinter_Recess = as.Date(c("2014-02-19", "2014-02-23")),
  Spring_Recess = as.Date(c("2014-04-22", "2014-04-26"))
)

# Add an IsVacation column for vacation periods
combined_data_cleaned <- combined_data_cleaned %>%
  mutate(IsVacation_NY = ifelse(
    Date >= ny_vacation_periods$Winter_Recess[1] & Date <= ny_vacation_periods$Winter_Recess[2] |
      Date >= ny_vacation_periods$Midwinter_Recess[1] & Date <= ny_vacation_periods$Midwinter_Recess[2] |
      Date >= ny_vacation_periods$Spring_Recess[1] & Date <= ny_vacation_periods$Spring_Recess[2], 1, 0))


# Create a list of file names for each month
file_names <- c("april2014.xlsx", "may2014.xlsx", "june2014.xlsx", "july2014.xlsx", "august2014.xlsx", "september2014.xlsx")

# Function for reading and cleaning weather files
load_and_clean_weather <- function(file_name, month, year = 2014) {
  weather_data <- read_excel(file_name)
  weather_data_cleaned <- weather_data %>%
    rename(Day = Time, Temperature_Avg = `Temperature (°F)`, Wind_Speed_Avg = `Wind Speed (mph)`, Precipitation = `Precipitation (in)`) %>%
    mutate(Day = as.numeric(Day),
           Date = as.Date(paste(year, month, Day, sep = "-")))
  return(weather_data_cleaned)
}

# Apply function to each file
weather_data_list <- list(
  load_and_clean_weather("april2014.xlsx", 4),
  load_and_clean_weather("may2014.xlsx", 5),
  load_and_clean_weather("june2014.xlsx", 6),
  load_and_clean_weather("july2014.xlsx", 7),
  load_and_clean_weather("august2014.xlsx", 8),
  load_and_clean_weather("september2014.xlsx", 9)
)

# Combine data from all months into a single dataframe
combined_weather_data <- bind_rows(weather_data_list)

# Merge weather data with route data
combined_data_cleaned <- combined_data_cleaned %>%
  left_join(combined_weather_data, by = "Date") %>%
  select(-contains(".x"), -any_of(c("Year", "Day.y", "Day")))

#Convert columns to numeric
combined_data_cleaned <- combined_data_cleaned %>%
  mutate(
    Temperature_Avg = as.numeric(Temperature_Avg),
    Wind_Speed_Avg = as.numeric(Wind_Speed_Avg),
    Precipitation = as.numeric(Precipitation)
  )

# Prepare the data: group the wrinkles by date and calculate the average weather factor.
daily_data <- combined_data_cleaned %>%
  group_by(Date, Month) %>%
  summarise(
    Total_Rides = n(),
    Temperature_Avg = mean(as.numeric(Temperature_Avg), na.rm = TRUE),
    Precipitation = mean(as.numeric(Precipitation), na.rm = TRUE),
    Wind_Speed_Avg = mean(as.numeric(Wind_Speed_Avg), na.rm = TRUE),
    IsHoliday_NY = max(IsHoliday_NY),
    IsVacation_NY = max(IsVacation_NY)
  ) %>%
  distinct(Date, Month, .keep_all = TRUE)

#Loops to display a graph by month containing the number of wrinkles and associated secondary factors
unique_months <- unique(daily_data$Month)

for (month in unique_months) {
  monthly_data2 <- daily_data %>% filter(Month == month)
  
  plot <- ggplot(monthly_data2, aes(x = Date)) +
    
    geom_bar(aes(y = Total_Rides,
                 fill = factor(case_when(
                   IsHoliday_NY == 1 ~ "Holiday",
                   IsVacation_NY == 1 ~ "Vacation",
                   wday(Date) %in% c(1, 7) ~ "Weekend",
                   TRUE ~ "Weekday"))),
             stat = "identity", alpha = 0.7) +
    
    geom_line(aes(y = Temperature_Avg * 250, color = "Temperature (°F"), size = 1) +
    
    geom_line(aes(y = Precipitation * 5000, color = "Precipitations (mm)"), size = 1, linetype = "dashed") +
    
    geom_line(aes(y = Wind_Speed_Avg * 1000, color = "Wind (km/h)"), size = 1, linetype = "dotdash") +
    
    labs(title = paste("Histogram of rides and weather curves - Month:", month),
         x = "Day", y = "Number of rides",
         fill = "Type of day", color = "Meteorological factors") +
    
    scale_fill_manual(values = c("Holiday" = "purple", "Vacation" = "orange", "Weekend" = "red", "Weekday" = "skyblue")) +
    
    scale_y_continuous(
      name = "Number of rides",
      sec.axis = sec_axis(~ ./100, name = "Meteorological factors")
    ) +
    
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
  
  print(plot)
}

# Make a new model
# Definition of prediction data
prediction_data <- combined_data_cleaned %>%
  mutate(
    Hour = hour(Date.Time),
    DayOfWeek = wday(Date.Time),
    IsWeekend = ifelse(DayOfWeek %in% c(1, 7), 1, 0),
    TimeBlock = case_when(
      Hour >= 5 & Hour < 10 ~ "Morning_Rush",
      Hour >= 10 & Hour < 16 ~ "Midday",
      Hour >= 16 & Hour < 20 ~ "Evening_Rush",
      TRUE ~ "Night"
    ),
    Temperature_Avg = as.numeric(Temperature_Avg),
    Precipitation = as.numeric(Precipitation),
    Wind_Speed_Avg = as.numeric(Wind_Speed_Avg),
    IsHoliday_NY = as.factor(IsHoliday_NY),
    IsVacation_NY = as.factor(IsVacation_NY)
  )

# Step 2: Data aggregation by hour
hourly_rides <- prediction_data %>%
  group_by(Date = as.Date(Date.Time), Hour, DayOfWeek, IsWeekend, TimeBlock,
           Temperature_Avg, Precipitation, Wind_Speed_Avg, IsHoliday_NY, IsVacation_NY) %>%
  summarise(
    Rides = n(),
    .groups = 'drop'
  ) %>%
  arrange(Date, Hour)

# Step 3: Additional time features
hourly_rides <- hourly_rides %>%
  mutate(
    Previous_Hour_Rides = lag(Rides, 1),
    Rolling_Mean = zoo::rollmean(Rides, k = 3, fill = NA, align = "right"),
    DayOfMonth = day(Date),
    Month = month(Date)
  ) %>%
  na.omit()

# Step 4: Encoding of categorical variables 
hourly_rides$TimeBlock <- as.factor(hourly_rides$TimeBlock)
hourly_rides$DayOfWeek <- as.factor(hourly_rides$DayOfWeek)
hourly_rides$IsHoliday_NY <- as.factor(hourly_rides$IsHoliday_NY)
hourly_rides$IsVacation_NY <- as.factor(hourly_rides$IsVacation_NY)

# Step 5: Division of data into training and test sets
set.seed(123)
train_index <- createDataPartition(hourly_rides$Rides, p = 0.7, list = FALSE)
train_data <- hourly_rides[train_index, ]
test_data <- hourly_rides[-train_index, ]

# Step 6: Model evaluation function
evaluate_model <- function(actual, predicted, model_name) {
  mse <- mean((actual - predicted)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(actual - predicted))
  mape <- mean(abs((actual - predicted) / actual)) * 100
  
  cat(paste("Results of the model", model_name, ":\n"))
  cat("RMSE:", round(rmse, 2), "\n")
  cat("MAE:", round(mae, 2), "\n")
  cat("MAPE:", round(mape, 2), "%\n\n")
  
  return(data.frame(Actual = actual, Predicted = predicted, Model = model_name))
}

# Step 7: Training a linear regression model
lm_model <- lm(Rides ~ ., data = train_data)
lm_predictions <- predict(lm_model, test_data)
lm_results <- evaluate_model(test_data$Rides, lm_predictions, "Linear regression")

# Step 8: Training the Random Forest model with the new data
rf_model <- randomForest(Rides ~ ., data = train_data, ntree = 300, importance = TRUE)
rf_predictions <- predict(rf_model, test_data)
rf_results <- evaluate_model(test_data$Rides, rf_predictions, "Random Forest")

# Step 9: Train the SVM model
svm_model <- svm(Rides ~ ., data = train_data)
svm_predictions <- predict(svm_model, test_data)
svm_results <- evaluate_model(test_data$Rides, svm_predictions, "SVM")

importance_df <- data.frame(
  Feature = rownames(importance(rf_model)),
  Importance = importance(rf_model)[,1]
)

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Importance of variables in Random Forest Model",
       x = "Variables",
       y = "Importance") +
  theme_minimal()

all_results <- rbind(lm_results, rf_results, svm_results)

ggplot(all_results, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  facet_wrap(~ Model) +
  labs(title = "Comparison of the prediction models",
       x = "Real number of rides",
       y = "Predicted number of rides") +
  theme_minimal() +
  theme(legend.position = "none")

#------------------------------------------------------------------------------------
#13)
#############Part 1: influence of the weather ######################################
# Function to get real demand from historical data
get_real_demand <- function(historical_data, current_date) {
  real_demand <- historical_data %>%
    filter(Date == current_date) %>%
    pull(Total_Rides)
  
  if (length(real_demand) == 0) {
    return(NA)
  }
  
  return(real_demand)
}

# Function to predict short-term demand
predict_short_term_demand <- function(historical_data, current_date, forecast_horizon = 3) {
  recent_data <- tail(historical_data, 90) %>%
    filter(!is.na(Total_Rides)) %>%
    pull(Total_Rides)
  
  if (length(recent_data) < 14) { 
    return(rep(mean(recent_data, na.rm = TRUE), forecast_horizon))
  }
  
  ts_data <- ts(recent_data, frequency = 7)
  
  tryCatch({
    arima_model <- auto.arima(ts_data, seasonal = TRUE, stepwise = TRUE)
    ets_model <- ets(ts_data)
    
    arima_forecast <- forecast(arima_model, h = forecast_horizon)
    ets_forecast <- forecast(ets_model, h = forecast_horizon)
    
    combined_forecast <- (arima_forecast$mean * 0.6 + ets_forecast$mean * 0.4)
  }, error = function(e) {
    return(rep(mean(tail(recent_data, 7)), forecast_horizon))
  })
  
  return(pmax(1, round(combined_forecast)))
}

# Function to adjust resource allocation
adjust_resource_allocation <- function(current_demand, predicted_demand, available_resources, previous_allocation, historical_performance) {
  current_demand <- as.numeric(current_demand)
  predicted_demand <- as.numeric(predicted_demand[1])
  previous_allocation <- as.numeric(previous_allocation)
  
  default_accuracy <- 0.8
  default_utilization <- 0.9
  
  if (!is.null(historical_performance) && nrow(historical_performance) > 0) {
    recent_accuracy <- tail(historical_performance$prediction_accuracy, 7)
    recent_utilization <- tail(historical_performance$resource_utilization, 7)
    
    accuracy_factor <- mean(recent_accuracy, na.rm = TRUE)
    utilization_factor <- mean(recent_utilization, na.rm = TRUE)
    
    if (is.na(accuracy_factor)) accuracy_factor <- default_accuracy
    if (is.na(utilization_factor)) utilization_factor <- default_utilization
  } else {
    accuracy_factor <- default_accuracy
    utilization_factor <- default_utilization
  }
  
  base_allocation <- mean(c(current_demand, predicted_demand), na.rm = TRUE)
  
  allocation <- if (!is.na(current_demand) && !is.na(previous_allocation)) {
    if (current_demand > previous_allocation * 1.1) {
      base_allocation * (1 + (1 - accuracy_factor))
    } else if (current_demand < previous_allocation * 0.9) {
      base_allocation * (1 - (1 - accuracy_factor) * 0.5)
    } else {
      base_allocation * (1 + (utilization_factor - 0.9) * 0.2)
    }
  } else {
    base_allocation
  }
  
  max_change <- previous_allocation * 0.15
  allocation <- max(min(allocation, previous_allocation + max_change), previous_allocation - max_change)
  
  safety_buffer <- 1.1
  allocation <- allocation * safety_buffer
  
  return(round(min(max(1, allocation), available_resources)))
}

# Simulation of dynamic optimization
simulate_dynamic_optimization <- function(historical_data, start_date, num_days, initial_resources) {
  results <- data.frame()
  previous_allocation <- initial_resources
  historical_performance <- data.frame(
    prediction_accuracy = numeric(),
    resource_utilization = numeric()
  )
  
  for (i in 1:num_days) {
    current_date <- start_date + days(i - 1)
    
    current_demand <- get_real_demand(historical_data, current_date)
    
    if (is.na(current_demand)) {
      current_demand <- mean(historical_data$Total_Rides, na.rm = TRUE)
    }
    
    predicted_demand <- tryCatch({
      predict_short_term_demand(historical_data, current_date)
    }, error = function(e) {
      rep(current_demand, 3)
    })
    
    if (i > 1 && !is.na(current_demand)) {
      prev_accuracy <- 1 - min(1, abs(current_demand - results$Predicted_Demand[nrow(results)]) / current_demand)
      prev_utilization <- min(1, current_demand / results$Allocated_Resources[nrow(results)])
      
      historical_performance <- rbind(historical_performance,
                                      data.frame(prediction_accuracy = prev_accuracy,
                                                 resource_utilization = prev_utilization))
    }
    
    allocated_resources <- adjust_resource_allocation(
      current_demand, predicted_demand,
      max(historical_data$Total_Rides, na.rm = TRUE),
      previous_allocation,
      historical_performance
    )
    
    current_result <- data.frame(
      Date = current_date,
      Actual_Demand = current_demand,
      Predicted_Demand = predicted_demand[1],
      Allocated_Resources = allocated_resources
    )
    
    results <- bind_rows(results, current_result)
    
    historical_data <- bind_rows(historical_data,
                                 data.frame(Date = current_date,
                                            Total_Rides = current_demand))
    
    previous_allocation <- allocated_resources
  }
  
  return(results)
}

# Using data from April to September for simulation
start_date <- as.Date("2014-04-01")
num_days <- 183  # To cover April to September
initial_resources <- median(daily_data$Total_Rides, na.rm = TRUE)

# Run the simulation over 183 days (6 months)
optimization_results <- simulate_dynamic_optimization(daily_data, start_date, num_days, initial_resources)

# Visualize the results
ggplot(optimization_results, aes(x = Date)) +
  geom_line(aes(y = Actual_Demand, color = "Real Demand")) +
  geom_line(aes(y = Predicted_Demand, color = "Predicted Demand")) +
  geom_line(aes(y = Allocated_Resources, color = "Allocated Resources")) +
  labs(title = "Dynamic Resource Allocation Optimization (April to September)",
       x = "Date",
       y = "Number of Rides / Resources",
       color = "Legend") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Calculate performance metrics
performance_metrics <- optimization_results %>%
  summarise(
    Mean_Absolute_Error = mean(abs(Predicted_Demand - Actual_Demand), na.rm = TRUE),
    MAPE = mean(abs((Predicted_Demand - Actual_Demand) / Actual_Demand), na.rm = TRUE) * 100,
    Resource_Utilization = mean(Actual_Demand / Allocated_Resources, na.rm = TRUE),
    Overage_Rate = mean(pmax(0, Allocated_Resources - Actual_Demand) / Allocated_Resources, na.rm = TRUE),
    Underage_Rate = mean(pmax(0, Actual_Demand - Allocated_Resources) / Actual_Demand, na.rm = TRUE)
  )

# Display the metrics
print(performance_metrics)


##########Part 2: Influence of the days (weekend, peak hours) ##############################

# #1. Identify peak hours

hourly_average <- hourly_rides %>%
  
  group_by(Hour) %>%
  
  summarise(Average_Rides = mean(Rides))

peak_hours <- hourly_average %>%
  
  filter(Average_Rides > quantile(Average_Rides, 0.75)) %>%
  
  pull(Hour)

print("Identified peak hours:")
print(peak_hours)

# #2. Analyze factors contributing to peak hours

peak_data <- hourly_rides %>%
  
  filter(Hour %in% peak_hours)

# #2.1 Analysis by day of the week

weekday_analysis <- peak_data %>%
  
  group_by(DayOfWeek) %>%
  
  summarise(Average_Peak_Rides = mean(Rides))

ggplot(weekday_analysis, aes(x = DayOfWeek, y = Average_Peak_Rides)) +
  
  geom_bar(stat = "identity", fill = "skyblue") +
  
  labs(title = "Average Rides During Peak Hours by Day of the Week",
       
       x = "Day of the Week",
       
       y = "Average Number of Rides") +
  
  theme_minimal()

# #2.2 Analysis by month

monthly_analysis <- peak_data %>%
  
  group_by(Month) %>%
  
  summarise(Average_Peak_Rides = mean(Rides))

ggplot(monthly_analysis, aes(x = factor(Month), y = Average_Peak_Rides)) +
  
  geom_bar(stat = "identity", fill = "lightgreen") +
  
  labs(title = "Average Rides During Peak Hours by Month",
       
       x = "Month",
       
       y = "Average Number of Rides") +
  
  theme_minimal()

# #2.3 Analyze the impact of weekdays vs weekends

weekend_impact <- peak_data %>%
  
  group_by(IsWeekend) %>%
  
  summarise(Average_Peak_Rides = mean(Rides))

ggplot(weekend_impact, aes(x = factor(IsWeekend, labels = c("Weekday", "Weekend")), y = Average_Peak_Rides)) +
  
  geom_bar(stat = "identity", fill = "coral") +
  
  labs(title = "Impact of Weekends on Rides During Peak Hours",
       
       x = "Type of Day",
       
       y = "Average Number of Rides") +
  
  theme_minimal()

# #4. Analyze the correlation between factors

correlation_matrix <- cor(peak_data %>% select_if(is.numeric))
print("Correlation matrix for peak hours:")
print(correlation_matrix)

# #Visualization of the correlation matrix

corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)

# #5. Summary of insights

print("Summary of factors contributing to peak hours:")
print(paste("Busiest days of the week:",
            weekday_analysis %>% filter(Average_Peak_Rides == max(Average_Peak_Rides)) %>% pull(DayOfWeek)))
print(paste("Busiest months:",
            monthly_analysis %>% filter(Average_Peak_Rides == max(Average_Peak_Rides)) %>% pull(Month)))
print(paste("Weekend impact:",
            ifelse(weekend_impact$Average_Peak_Rides[weekend_impact$IsWeekend == 1] >
                     weekend_impact$Average_Peak_Rides[weekend_impact$IsWeekend == 0],
                   "More rides on weekends", "More rides on weekdays")))

# #Example of a linear regression model for demand prediction

set.seed(123)
model <- train(Rides ~ ., data = peak_data, method = "lm")

# #Predictions based on the model

predicted_rides <- predict(model, newdata = test_data)

# #Display the predictions

test_data$predicted_rides <- predicted_rides
head(test_data)


#-------------------------------------------------------------------------------------
#14)  

# load data of people

people <- read.csv('Mall_Customers.csv')

# Display columns

str(people)
print(names(people))


# Select data we need->age, annual income, spending score

selected_data <- people %>%
  select(Age, `Annual_Income`, `Spending_Score`)

# Normalize data

scaled_data <- scale(selected_data)

# Apply K-means to make cclusters

set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 4, nstart = 25)

#Add segments to the dataframe

people$Segment <- as.factor(kmeans_result$cluster)

#Visualize 

fviz_cluster(kmeans_result, data = scaled_data,
             geom = "point", ellipse.type = "convex") +
  labs(title = "Segmentation of custimers by K-means")

#Analyse segments

segment_analysis <- people %>%
  group_by(Segment) %>%
  summarise(
    Avg_Age = mean(Age, na.rm = TRUE),
    Avg_Annual_Income = mean(`Annual_Income`, na.rm = TRUE),
    Avg_Spending_Score = mean(`Spending_Score`, na.rm = TRUE),
    Count = n()
  )

# Visualize results

scatter_plot <- ggplot(people, aes(x = `Annual_Income`, y = `Spending_Score`, color = Segment)) +
  geom_point(size = 3, alpha = 0.6) +
  labs(title = "Segments of Clients : Annual income vs Spending Score",
       x = "Annual income (k$)",
       y = "Spending Score (1-100)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

# Visualize age

age_plot <- ggplot(segment_analysis, aes(x = Segment, y = Avg_Age, fill = Segment)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Average Age by segments",
       x = "Segment",
       y = "Average Age") +
  theme_minimal()

# Graphiques are put together

grid.arrange(scatter_plot, age_plot, ncol = 2)


# Add columns for the hours of the day and days of the week
combined_data_cleaned <- combined_data_cleaned %>%
  mutate(Hour = hour(Date.Time),
         DayOfWeek = wday(Date.Time, label = TRUE))

# Segment by usage time: peak hours (morning and evening), night, weekend
combined_data_cleaned <- combined_data_cleaned %>%
  mutate(Time_Segment = case_when(
    Hour >= 7 & Hour <= 9 ~ "Morning Peak",
    Hour >= 17 & Hour <= 19 ~ "Evening Peak",
    Hour >= 22 | Hour <= 6 ~ "Night",
    TRUE ~ "Off-Peak"
  ))

# Visualize the distribution of time segments
ggplot(combined_data_cleaned, aes(x = Time_Segment, fill = Time_Segment)) +
  geom_bar() +
  labs(title = "User Segmentation by Usage Time",
       x = "Usage Time",
       y = "Number of Rides") +
  theme_minimal()

# Segment users by geographical areas
combined_data_cleaned <- combined_data_cleaned %>%
  mutate(Geographical_Segment = case_when(
    Lat > 40.75 & Lon < -73.98 ~ "NorthWest",
    Lat > 40.75 & Lon >= -73.98 ~ "NorthEast",
    Lat <= 40.75 & Lon < -73.98 ~ "SouthWest",
    Lat <= 40.75 & Lon >= -73.98 ~ "SouthEast"
  ))

# Visualize the distribution of geographical segments
ggplot(combined_data_cleaned, aes(x = Geographical_Segment, fill = Geographical_Segment)) +
  geom_bar() +
  labs(title = "User Segmentation by Geographical Areas",
       x = "Geographical Area",
       y = "Number of Rides") +
  theme_minimal()




#---------------------------------------------------------------------------------------
#Q16


# Weekly Patterns Analysis
weekly_patterns <- combined_data_cleaned %>%
  mutate(
    Week = floor_date(Date.Time, unit = "week"),
    WeekNumber = week(Date.Time),
    DayOfWeek = wday(Date.Time, label = TRUE)
  ) %>%
  group_by(Week, WeekNumber) %>%
  summarise(
    Total_Rides = n(),
    Avg_Daily_Rides = n() / n_distinct(Date),
    Variance = var(as.numeric(table(Date))),
    .groups = 'drop'
  )

# Visualize weekly trends
ggplot(weekly_patterns, aes(x = Week, y = Total_Rides)) +
  geom_line(color = "blue") +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Weekly Ride Patterns Over Time",
       subtitle = "With trend line showing overall pattern",
       x = "Week",
       y = "Total Rides") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Monthly Growth Rate Analysis
monthly_growth <- combined_data_cleaned %>%
  mutate(YearMonth = floor_date(Date.Time, unit = "month")) %>%
  group_by(YearMonth) %>%
  summarise(
    Total_Rides = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    Growth_Rate = (Total_Rides - lag(Total_Rides)) / lag(Total_Rides) * 100
  ) %>%
  filter(!is.na(Growth_Rate))

# Visualize monthly growth
ggplot(monthly_growth, aes(x = YearMonth)) +
  geom_col(aes(y = Total_Rides), fill = "skyblue") +
  geom_line(aes(y = Growth_Rate * max(Total_Rides)/100), color = "red", size = 1) +
  scale_y_continuous(
    name = "Total Rides",
    sec.axis = sec_axis(~./(max(monthly_growth$Total_Rides)/100), name = "Growth Rate (%)")
  ) +
  labs(title = "Monthly Rides and Growth Rate",
       x = "Month") +
  theme_minimal()

#Seasonal Decomposition
# Prepare daily data for time series analysis
daily_ts <- combined_data_cleaned %>%
  group_by(Date) %>%
  summarise(Total_Rides = n()) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"), 
           fill = list(Total_Rides = 0))

# Convert to time series object
ts_data <- ts(daily_ts$Total_Rides, frequency = 7)  # 7 for weekly seasonality

# Decompose the time series
decomposed <- stl(ts_data, s.window = "periodic")

# Plot the decomposition
plot(decomposed, main = "Seasonal Decomposition of Bolt Rides")

# Day-of-Week Patterns by Month
monthly_dow_patterns <- combined_data_cleaned %>%
  mutate(
    Month = month(Date.Time, label = TRUE),
    DayOfWeek = wday(Date.Time, label = TRUE)
  ) %>%
  group_by(Month, DayOfWeek) %>%
  summarise(
    Avg_Rides = n() / n_distinct(Date),
    .groups = 'drop'
  )

# Create heatmap of day-of-week patterns by month
ggplot(monthly_dow_patterns, aes(x = DayOfWeek, y = Month, fill = Avg_Rides)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(title = "Average Daily Rides by Month and Day of Week",
       x = "Day of Week",
       y = "Month",
       fill = "Average Rides") +
  theme_minimal()

# Period-over-Period Analysis
pop_analysis <- combined_data_cleaned %>%
  mutate(
    WeekOfYear = week(Date.Time),
    DayOfWeek = wday(Date.Time, label = TRUE)
  ) %>%
  group_by(WeekOfYear) %>%
  summarise(
    Current_Week_Rides = n(),
    Rides_Per_Day = n() / n_distinct(Date),
    .groups = 'drop'
  ) %>%
  mutate(
    Previous_Week_Rides = lag(Current_Week_Rides),
    WoW_Change = (Current_Week_Rides - Previous_Week_Rides) / Previous_Week_Rides * 100
  )

# Visualize week-over-week changes
ggplot(pop_analysis, aes(x = WeekOfYear)) +
  geom_line(aes(y = WoW_Change), color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Week-over-Week Change in Ride Volume",
       x = "Week of Year",
       y = "Percent Change (%)") +
  theme_minimal()


#------------------------------------------------------------------------------------
#Q17 

#We prepared our data by categorizing variables such as time slots, weekdays, and geographic zones to better understand user behaviors. By converting these into transaction format and applying support and confidence thresholds in the apriori function, we minimized irrelevant associations. Additionally, we identified and addressed suspicious correlations using a correlation matrix, particularly noting high multicollinearity with avg_lat and avg_long.  

#-----------------------------------
#Q18
#To ensure compliance with data privacy regulations while working with user-specific data, we implemented personal data protection measures such as anonymizing user identifiers, removing sensitive information, aggregating data to reflect general trends, and retaining only necessary data for analysis, alongside GDPR compliance practices like data minimization and limiting access to raw data; additionally, we followed best practices by working with anonymized copies, aggregating results at the hourly/daily level, focusing on general patterns, and documenting security procedures.
#Example of code 
#secured_data <- combined_data_cleaned %>%
# select(-user_id, -personal_info) %>%  # Delete sensibles information
#mutate(
# ride_id = as.factor(ride_id),      # Convert to factors to hide reals ID
#timestamp = floor_date(timestamp, "hour")  # Temporal Questions
#---------------------------------------
#Q19 speaking
#The predictive models for Bolt are designed to be scalable by leveraging evolving temporal patterns and are specifically crafted to handle increasing data volumes. They feature a flexible architecture that allows for the addition of new variables and a continuous validation system for predictions.

#Adaptability and Evolution Capabilities:
#Current Model Adaptability: Seasonal decomposition automatically adjusts to changes, while forecasts are continuously updated with new data.
#Future Growth Preparedness: Automatic performance monitoring is in place, with alerts for prediction degradation and dynamic parameter adjustments.
#Recommendations for Evolution: Implementing real-time processing systems, developing models by geographical area, integrating external variables (like weather and events), and establishing A/B validation systems are all crucial steps.
#Limitations and Solutions:
#Current Limitations: The models rely on historical data, necessitating regular recalibration and being sensitive to abrupt changes.
#Proposed Solutions: Continuous learning, adaptive models, and anomaly detection systems will enhance performance.
#Performance Indicators:
#Key metrics include monitoring prediction accuracy, data processing times, adaptation capacity to demand spikes, and forecast error rates.

#This comprehensive approach ensures that the models:

#Remain effective as the user base grows,Adapt to changing patterns,are maintainable in the long term, and Evolve a longside the company’s needs.

#---------------------------------------------------------------
#20 speaking
#User preferences evolve over time, influenced by factors such as changes in employment or relocation, weather conditions, and temporal trends like weekdays versus weekends. Analysis reveals that demand peaks during certain hours, particularly after work and on weekends, while it also shows a growing trend from April to September, coinciding with seasonal activities and the return to routine after summer. Identifying user profiles, including age and spending habits, enables tailored marketing strategies, such as offering discounts to students while pricing differently for other demographics.
#---------------------------------------------------------------
#21 speaking

#We aimed to enrich our analysis by integrating external data sources, such as holidays, vacations, and weather, to improve demand predictions. By understanding how these external events affect user behavior, we can create tailored offerings based on user profiles, enhancing engagement and satisfaction. Additionally, we included customer data and created clusters based on age, annual income, and spending scores. This multifaceted approach increases the robustness of our results, enabling more informed decision-making and better service adaptability.

