
##Required Libraries
install.packages("tidyverse")
install.packages("datarium")
install.packages("qqplotr")
install.packages("ggplot2")
install.packages("car")
install.packages("corrplot")
install.packages("caret") 
install.packages("TTR")
install.packages("forecast")
install.packages("maps")
install.packages("rnaturalearth")

#Loading Required Libraries
library(ggplot2)
library(datarium)
library(qqplotr)
library(tidyverse)
library(dplyr)
library(readr)
library(RVAideMemoire)
library(car)
library(corrplot)
library(caret)
library(TTR)
library(forecast)
library(maps)
library(rnaturalearth)



# Load the data from the CSV file
sustainable_data <- read.csv("sustainable_data.csv" ,header = TRUE)




# Display the structure of the dataset
str(sustainable_data)

# Display the first few rows of the dataset
head(sustainable_data)

# Summary statistics of numerical variables
summary(sustainable_data)

# Check for missing values
missing_values <- colSums(is.na(sustainable_data))
print(missing_values)

#Identifying outliers
# Display the first few rows of the dataset
head(sustainable_data)



##Rename Columns
sustainable_data <- sustainable_data %>%
  rename(
    Country_Name = 1,
    Country_Code = 2,
    Year = 3,
    Year_Code = 4,
    Clean_Fuels_Access_For_Cooking = 5,
    Energy_Intensity_Primary_Energy = 6,
    Renewable_Electricity_Output = 7,
    Renewable_Electricity_Share_of_Total_Output = 8,
    Renewable_Energy_Consumption = 9,
    Renewable_Energy_Share_TFEC = 10,
    Total_Electricity_Output = 11,
    Total_Final_Energy_Consumption = 12
  )
head(sustainable_data)







####Exploratory Data Analysis

# Function to plot boxplot for a specified column
plot_boxplot <- function(data, column_name) {
  ggplot(data, aes(y = .data[[column_name]])) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", column_name))
}



plot_boxplot(sustainable_data, 'Energy_Intensity_Primary_Energy')
plot_boxplot(sustainable_data, 'Renewable_Electricity_Output')
plot_boxplot(sustainable_data, 'Renewable_Electricity_Share_of_Total_Output')
plot_boxplot(sustainable_data, 'Renewable_Energy_Consumption')
plot_boxplot(sustainable_data, 'Renewable_Energy_Share_TFEC')
plot_boxplot(sustainable_data, 'Total_Electricity_Output')
plot_boxplot(sustainable_data, 'Total_Final_Energy_Consumption')


# Line Plots Function for different Indicators
Indicator_Distribution_Over_Time <- function(data, indicator) {
  ggplot(data, aes(x = Year, y = .data[[indicator]], color = Country_Name, group = Country_Name)) +
    geom_line() +
    ggtitle(paste("Line Plot of", indicator, "distribution over the years by Country")) +
    xlab("Year") +
    ylab(indicator)
}


Indicator_Distribution_Over_Time(sustainable_data, 'Energy_Intensity_Primary_Energy')
Indicator_Distribution_Over_Time(sustainable_data, 'Renewable_Electricity_Output')
Indicator_Distribution_Over_Time(sustainable_data, 'Renewable_Electricity_Share_of_Total_Output')
Indicator_Distribution_Over_Time(sustainable_data, 'Renewable_Energy_Consumption')
Indicator_Distribution_Over_Time(sustainable_data, 'Renewable_Energy_Share_TFEC')
Indicator_Distribution_Over_Time(sustainable_data, 'Total_Electricity_Output')
Indicator_Distribution_Over_Time(sustainable_data, 'Total_Final_Energy_Consumption')
Indicator_Distribution_Over_Time(sustainable_data, 'Clean_Fuels_Access_For_Cooking')


##Average Indicator By Country

# Function for average  Indicator distribution across Countries
Average_Indicator_Distribution_By_Country <- function(data, indicator) {
  # Calculate average for the specified indicator for each country
  average_by_country <- data %>%
    group_by(Country_Name) %>%
    summarize(Average_Indicator = mean(.data[[indicator]], na.rm = TRUE))
  
  # Plot the average values
  ggplot(average_by_country, aes(x = Country_Name, y = Average_Indicator, fill = Country_Name)) +
    geom_bar(stat = "identity") +
    ggtitle(paste("Average", indicator, "by Country")) +
    xlab("Country") +
    ylab(paste("Average", indicator)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
}

Average_Indicator_Distribution_By_Country(sustainable_data, 'Energy_Intensity_Primary_Energy')
Average_Indicator_Distribution_By_Country(sustainable_data, 'Renewable_Electricity_Output')
Average_Indicator_Distribution_By_Country(sustainable_data, 'Renewable_Electricity_Share_of_Total_Output')
Average_Indicator_Distribution_By_Country(sustainable_data, 'Renewable_Energy_Consumption')
Average_Indicator_Distribution_By_Country(sustainable_data, 'Renewable_Energy_Share_TFEC')
Average_Indicator_Distribution_By_Country(sustainable_data, 'Total_Electricity_Output')
Average_Indicator_Distribution_By_Country(sustainable_data, 'Total_Final_Energy_Consumption')
Average_Indicator_Distribution_By_Country(sustainable_data, 'Clean_Fuels_Access_For_Cooking')


##Scatter Plot
install.packages("GGally")
library(GGally)
ggpairs(sustainable_data, columns = c("Total_Electricity_Output", "Renewable_Energy_Consumption", "Energy_Intensity_Primary_Energy"))





###map
install.packages("maps")
install.packages("rnaturalearth")
install.packages("ggplot2")

# Load world map data
world_map <- map_data("world")
map_data <- merge(world_map, sustainable_data, by.x = "region", by.y = "Country_Name", all.x = TRUE)

# Plot the map
ggplot(map_data, aes(x = long, y = lat, group = group, fill = Renewable_Energy_Consumption)) +
  geom_polygon(color = "white") +
  scale_fill_viridis_c(name = "Renewable Energy Consumption") +  # You can use a different color scale
  labs(title = "Distribution of Renewable Energy Consumption by Country") +
  theme_minimal()  # Adjust the theme as needed

## Statistical Analysis


install.packages("e1071")
library(e1071) 
# Columns for analysis
columns <- c(
  "Energy_Intensity_Primary_Energy", 
  "Renewable_Electricity_Output", 
  "Renewable_Electricity_Share_of_Total_Output", 
  "Total_Final_Energy_Consumption", 
  "Renewable_Energy_Consumption", 
  "Renewable_Energy_Share_TFEC", 
  "Total_Electricity_Output" )

# Create a data frame to store the results
descriptive_stats <- data.frame(Column_Name = character(), Mean = numeric(), Median = numeric(), SD = numeric(), Skewness = numeric(), Kurtosis = numeric(), stringsAsFactors = FALSE)

# Loop through columns and calculate descriptive statistics
for (column in columns) {
  values <- sustainable_data[[column]]
  descriptive_stats <- rbind(descriptive_stats, c(column, mean(values), median(values), sd(values), skewness(values), kurtosis(values)))
}

# Rename the columns of the result data frame
colnames(descriptive_stats) <- c("Column_Name", "Mean", "Median", "SD", "Skewness", "Kurtosis")

# Print the results
print(descriptive_stats)

barplot(descriptive_stats$Mean, names.arg = descriptive_stats$Column_Name, col = "lightblue", main = "Mean Values of Variables", ylab = "Mean", xlab = "Variable")

plot_histogram_with_mean <- function(data, column_index) {
  column_name <- columns[column_index]
  hist_values <- data[[column_name]]
  
  # Plot histogram
  hist(hist_values, main = paste("Distribution of", column_name), 
       col = "lightblue", xlim = c(min(hist_values), max(hist_values)))
  
  # Add mean indicator line
  abline(v = mean(hist_values), col = "red", lwd = 2)
  
}

# Example usage for the first column
plot_histogram_with_mean(sustainable_data,1)
plot_histogram_with_mean(sustainable_data,2)
plot_histogram_with_mean(sustainable_data,3)
plot_histogram_with_mean(sustainable_data,4)
plot_histogram_with_mean(sustainable_data,5)
plot_histogram_with_mean(sustainable_data,7)

pairs(sustainable_data[, columns])

# Function to create box plots with rotated column names
create_box_plots <- function(data) {
  # Set up the layout
  par(mfrow = c(3, 3))
  
  # Loop through columns and create box plots
  for (i in 1:length(columns)) {
    boxplot(data[[columns[i]]], main = paste("Box Plot of", columns[i]), col = "lightblue", las = 2)
  }
  
  # Reset layout
  par(mfrow = c(1, 1))
}


create_box_plots(sustainable_data)


# Function to create density plots with skewness and kurtosis indicators
create_density_plots <- function(data) {
  # Set up the layout
  par(mfrow = c(3, 3))
  
  # Loop through columns and create density plots
  for (i in 1:length(columns)) {
    column_name <- columns[i]
    hist_values <- data[[column_name]]
    
    # Create density plot
    plot(density(hist_values), main = paste("Density Plot of", column_name), col = "lightblue", xlim = c(min(hist_values), max(hist_values)))
    
    # Add skewness and kurtosis indicators
    text(quantile(hist_values, 0.95), 0.9 * max(density(hist_values)$y), paste("Skewness: ", round(skewness(hist_values), 2)), col = "red")
    text(quantile(hist_values, 0.95), 0.8 * max(density(hist_values)$y), paste("Kurtosis: ", round(kurtosis(hist_values), 2)), col = "blue")
  }
  
  par(mfrow = c(1, 1))
}
create_density_plots(sustainable_data)


##########################
#Correlation Analysis



# Select numerical columns
numerical_sustainable_data <- sustainable_data %>%
  select_if(is.numeric)

#view the correlation
corr_matrix <- cor(numerical_sustainable_data, method = "spearman")
corr_matrix

# Create a correlation plot
dev.off()
corrplot(corr_matrix, method = "number", type = "upper", tl.cex = 0.45)


######################################################################
###Hypothesis
##Testing Normality

# Function to retrieve Renewable_Electricity_Share_of_tota_Output for a specific country
retrieve_Renewable_Electricity_Share_of_tota_Output <- function(data, country_name) {
  # Filter data for the specified country
  country_data <- data[data$Country_Name == country_name, ]
  
  
  sample_data <- country_data[, c("Renewable_Electricity_Share_of_Total_Output")]
  
  return(sample_data)
}

sample_data <- retrieve_Renewable_Electricity_Share_of_Total_Output(sustainable_data, "Belgium")



# Create a QQ plot
qqnorm(sample_data)
qqline(sample_data, col = 2)

# Shapiro-Wilk normality test
shapiro.test(sample_data)

##Hypothesis One 

# Set the threshold
threshold <- 15

# Create a new categorical column based on 'RenewableEnergyShare'
sustainable_data$Renewable_Electricity_Share_Group <- 
  ifelse(sustainable_data$Renewable_Electricity_Share_of_Total_Output
         < threshold, 'group_low', 'group_high')

# Check the new 'Category' column
print(head(sustainable_data$Renewable_Electricity_Share_Group))

df_Renewable_Electricity_Share_Group <- sustainable_data


# convert Renewable_Electricity_Share_Group into factor
df_Renewable_Electricity_Share_Group$Renewable_Electricity_Share_Group<- 
  as.factor(df_Renewable_Electricity_Share_Group$Renewable_Electricity_Share_Group)

# Use box plot to compare Developed or Developing Countries
boxplot(`Renewable_Electricity_Share_of_Total_Output` ~ Renewable_Electricity_Share_Group,
        data=df_Renewable_Electricity_Share_Group,
        names=c("group_high", "group_low"),
        xlab="Total Renewable Electricity Output",
        ylab="Renewable_Electricity_Share_of_Total_Output",
        main="Renewable_Electricity_Share_of_Total_Output for 
        Countries with different renewable energy percentage")



t.test(`Renewable_Electricity_Share_of_Total_Output` ~ 
         Renewable_Electricity_Share_Group, df_Renewable_Electricity_Share_Group)

##Hypothesis Two
# Function to retrieve Energy_Intensity_Primary_energy for a specific country
retrieve_Energy_Intensity_Primary_energy <- function(data, country_name) {
  # Filter data for the specified country
  country_data <- data[data$Country_Name == country_name, ]
  
  
  sample_data <- country_data[, c("Renewable_Electricity_Share_of_Total_Output")]
  
  return(sample_data)
}

sample_data <- retrieve_Energy_Intensity_Primary_energy (sustainable_data, "Belgium")

#TTest
# Set the threshold for Energy Intensity Level
threshold_energy_intensity <- 2.3

# Create a new categorical column based on 'Energy_Intensity_Level'
sustainable_data$Energy_Intensity_Level_Group <- 
  ifelse(sustainable_data$Energy_Intensity_Primary_Energy < 
           threshold_energy_intensity, 'low_intensity', 'high_intensity')

# Check the new 'Energy_Intensity_Level_Group' column
print(head(sustainable_data$Energy_Intensity_Level_Group))

df_Energy_Intensity_Level_Group <- sustainable_data

# Convert 'Energy_Intensity_Level_Group' into a factor
df_Energy_Intensity_Level_Group$Energy_Intensity_Level_Group <- 
  as.factor(df_Energy_Intensity_Level_Group$Energy_Intensity_Level_Group)

# Use box plot to compare energy intensity levels
boxplot(`Renewable_Electricity_Output` ~ Energy_Intensity_Level_Group,
        data=df_Energy_Intensity_Level_Group,
        names=c("high_intensity", "low_intensity"),
        xlab="Energy Intensity Level",
        ylab="Energy_Intensity_Level",
        main="Renewable_Electricity_Output for Countries with different energy intensity")

# Perform a t-test for Energy Intensity Level
t_test_energy_intensity <- t.test(`Renewable_Electricity_Output` ~ 
                                    Energy_Intensity_Level_Group, df_Energy_Intensity_Level_Group)

# Print the results
print(t_test_energy_intensity)


#############################################################
##Regression Analysis
#sample data
year_of_interest <- 2014

# Create a new dataframe for the specific year
df <- sustainable_data[sustainable_data$Year == year_of_interest, ]

print(df)




str(sustainable_data)

sustainable_reduced <- df[ ,c( 'Clean_Fuels_Access_For_Cooking', 'Energy_Intensity_Primary_Energy',
                               'Renewable_Electricity_Output',
                               'Renewable_Electricity_Share_of_Total_Output',
                               'Renewable_Energy_Consumption','Renewable_Energy_Share_TFEC',
                               'Total_Electricity_Output','Total_Final_Energy_Consumption')]

cor(sustainable_reduced)

cor_matrix <- cor(sustainable_reduced)
corrplot(cor_matrix, tl.cex = 0.45)




#Reg One
model_1 <-lm(Renewable_Electricity_Output ~ Renewable_Energy_Consumption +  Total_Electricity_Output ,sustainable_reduced)
summary.lm(model_1)

model_2 <-lm(Renewable_Electricity_Output ~ Renewable_Energy_Consumption + Total_Electricity_Output + Total_Final_Energy_Consumption,sustainable_reduced)
summary.lm(model_2)




#Check Linearity

colnames(sustainable_reduced)
pairs(sustainable_reduced[,c(4,6,3,9)], lower.panel = NULL, pch = 19,cex = 0.2)

#Residual Independence
plot(model_1, 1)

#Normality of Residuals
plot(model_1, 2)

#Equal variances of the residuals (Homoscedasticity)

plot(model_1, 3)

vif(model_1)

#Reg Two
model_1 <-lm(Total_Electricity_Output ~ Energy_Intensity_Primary_Energy  + Renewable_Electricity_Output + Renewable_Energy_Consumption + Total_Final_Energy_Consumption ,sustainable_reduced)
summary.lm(model_1)

model_2 <-lm(Total_Electricity_Output ~ Energy_Intensity_Primary_Energy  + Renewable_Electricity_Output + Renewable_Energy_Consumption ,sustainable_reduced)
summary.lm(model_2)

model_3 <-lm(Total_Electricity_Output ~ Energy_Intensity_Primary_Energy  + Renewable_Electricity_Output ,sustainable_reduced)
summary.lm(model_3)



#Check Linearity

colnames(sustainable_reduced)
pairs(sustainable_reduced[,c( 5,3,7)], lower.panel = NULL, pch = 19,cex = 0.2)

#Residual Independence
plot(model_3, 1)

#Normality of Residuals
plot(model_3, 2)

#Equal variances of the residuals (Homoscedasticity)

plot(model_3, 3)


vif(model_3)


#coefficients
intercept <- -295700
coefficient_energy_intensity <- 84470
coefficient_renewable_output <- 3.671

# values for predictors
energy_intensity <- 10
renewable_output <- 1500

# Calculate Total_Electricity_Output
predicted_output <- intercept + 
  coefficient_energy_intensity * energy_intensity +
  coefficient_renewable_output * renewable_output
print(predicted_output)

###############################
##Time Series



# Filter data for UK
United_Kingdom_data <- subset(sustainable_data, `Country_Name` == "United Kingdom")

United_Kingdom_data <- United_Kingdom_data[order(United_Kingdom_data$Year), ]

United_Kingdom_ts <- ts(United_Kingdom_data$`Renewable_Energy_Consumption`, start = c(2004))

# Plot the time series for UK
plot.ts(United_Kingdom_ts, main = "Renewable_Energy_Consumption  UK in over 10 years", col = "blue", lwd = 2, xlab = "Year", ylab = "Renewable_Energy_Consumption")


##forecast
United_Kingdom_tsforecasts <- HoltWinters(United_Kingdom_ts, gamma=FALSE)
United_Kingdom_tsforecasts


United_Kingdom_tsforecasts$fitted
plot(United_Kingdom_tsforecasts)


#forecast for the next 5 years
United_Kingdom_tsforecasts1 <- forecast(United_Kingdom_tsforecasts, h=5)
United_Kingdom_tsforecasts1

plot(United_Kingdom_tsforecasts1)
United_Kingdom_tsforecasts1$residuals <-United_Kingdom_tsforecasts1$residuals[!is.na(United_Kingdom_tsforecasts1$residuals)]

##Ljung-Box test
United_Kingdom_tsforecasts1$residuals <-United_Kingdom_tsforecasts1$residuals[!is.na(United_Kingdom_tsforecasts1$residuals)]
acf(United_Kingdom_tsforecasts1$residuals, lag.max = 7)


# Plot forecast errors
# Create a histogram with a density plot
hist(United_Kingdom_tsforecasts1$residuals, col = "lightblue", main = "Histogram with Density Plot", xlab = "Residuals", ylab = "Frequency", probability = TRUE)
lines(density(United_Kingdom_tsforecasts1$residuals), col = "blue", lwd = 2)

###Time Series Two

# Filter data for Denmark
Denmark_data <- subset(sustainable_data, `Country_Name` == "Denmark")

Denmark_data <- Denmark_data[order(Denmark_data$Year), ]

Denmark_ts <- ts(Denmark_data$`Renewable_Energy_Consumption`, start = c(2004))

# Plot the time series for Denmark
plot.ts(Denmark_ts, main = "Renewable_Energy_Consumption Denmark in over 10 years", 
        col = "blue", 
        lwd = 2, xlab = "Year", ylab = "Renewable_Energy_Consumption")


##Denmark Forecasting
Denmark_tsforecasts <- HoltWinters(Denmark_ts, gamma=FALSE)
Denmark_tsforecasts


Denmark_tsforecasts$fitted
plot(Denmark_tsforecasts)


#make forecast
HoltWinters(Denmark_ts, gamma=FALSE)


#forecast for the next 5 years
Denmark_tsforecasts_1 <- forecast(Denmark_tsforecasts, h=5)
Denmark_tsforecasts_1

plot(Denmark_tsforecasts_1)
Denmark_tsforecasts_1$residuals <-Denmark_tsforecasts_1$residuals[!is.na(Denmark_tsforecasts_1$residuals)]

##Ljung-Box test
Denmark_tsforecasts_1$residuals <
  -Denmark_tsforecasts_1$residuals[!is.na(Denmark_tsforecasts_1$residuals)]
acf(Denmark_tsforecasts_1$residuals, lag.max = 7)


# Plot forecast errors
# Create a histogram with a density plot
hist(Denmark_tsforecasts_1$residuals, col = "lightblue", main = "Histogram with Density Plot",
     xlab = "Residuals", ylab = "Frequency", probability = TRUE)
lines(density(United_Kingdom_tsforecasts1$residuals), col = "blue", lwd = 2)