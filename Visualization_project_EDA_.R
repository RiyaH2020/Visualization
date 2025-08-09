data <- read.csv("E:\\CMI\\Visualization_project\\MiningProcess_Flotation_Plant_Database.csv")
str(data)
head(data)
# Convert all character columns to numeric
# Convert all columns except the first one to numeric
data[, 2:ncol(data)] <- lapply(data[, 2:ncol(data)], function(x) {
  as.numeric(gsub(",", ".", gsub("[^0-9,]", "", x)))
})

head(data)
colnames(data)
library(ggplot2)
hist(data$X..Silica.Concentrate)
#Plot Silica concentrate vs Iron Concentrate
ggplot(data, aes(x = `X..Iron.Concentrate`, y = `X..Silica.Concentrate`)) +
  geom_point(color ="navy") +  # Set point color to blue
  labs(x = "% Iron Concentrate", y = "% Silica Concentrate", title = "Iron vs Silica Concentrate")

# Load necessary library
library(ggplot2)

# Create the scatter plot
ggplot(data, aes(x = `Starch.Flow`, y = `X..Silica.Concentrate`)) +
  geom_point(color = "blue", size = 2, alpha = 0.6) +  # Adjust point aesthetics
  labs(title = "Scatter Plot of Starch Flow vs. Silica Concentrate",
       x = "Starch Flow",
       y = "Silica Concentrate") +
  theme_minimal() +  # Use a minimal theme
  theme(plot.title = element_text(hjust = 0.5))  # Center the title



# Exclude the first column (and keep only numeric columns if needed)
df_subset <- data[, -c(1)]

# 1. Ensure rows with NA values are removed
df_subset <- df_subset[complete.cases(df_subset), ]

# Load necessary libraries
library(ggplot2)
library(reshape2)
library(RColorBrewer)

# Step 1: Calculate the correlation matrix
cor_matrix <- cor(df_subset, use = "complete.obs")

# Step 2: Melt the correlation matrix into long format for ggplot
cor_data <- melt(cor_matrix)
cor_data$Var2 <- factor(cor_data$Var2, levels = rev(unique(cor_data$Var2)))


# Step 3: Plot heatmap using ggplot2 
ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlGn"),  # Choose your preferred palette
                       values = scales::rescale(c(-1, 0, 1)), 
                       name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Matrix",
       x = "Variables",
       y = "Variables") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = round(value, 3)), color = "black", size = 3) 

display.brewer.all()


# Convert date column to Date type
data$date <- as.Date(data$date)

# Plot trends over time
ggplot(data, aes(x = date)) +
  geom_line(aes(y = X..Iron.Feed, color = "% Iron Feed")) +
  geom_line(aes(y = X..Silica.Concentrate, color = "% Silica Concentrate")) +
  labs(x = "Date", y = "Percentage", title = "Trends Over Time") +
  theme_minimal() +
  scale_color_manual(values = c("% Iron Feed" = "blue", "% Iron Concentrate" = "orange"))


library(lubridate)  # Load this library for date manipulation if needed

ggplot(data, aes(x = date)) +
  geom_line(aes(y = X..Iron.Feed, color = "% Iron Feed")) +
  geom_line(aes(y = X..Iron.Concentrate, color = "% Iron Concentrate")) +
  geom_line(aes(y = X..Silica.Feed, color = "% Silica Feed")) +
  geom_line(aes(y = X..Silica.Concentrate, color = "% Silica Concentrate")) +
  labs(x = "Date", y = "Percentage", title = "Trends Over Time") +
  theme_minimal() +
  scale_color_manual(values = c("% Iron Feed" = "blue", "% Silica Concentrate" = "orange","% Silica Feed" = "red", "% Iron Concentrate" = "purple")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")  # Customize the date breaks


library(GGally)
ggpairs(df_subset[, c("X..Iron.Concentrate", "X..Silica.Concentrate", "Ore.Pulp.pH", "Starch.Flow", "Amina.Flow")])

library(GGally)
library(ggplot2)

# Subset the columns: 'X..Silica.Concentrate' on y-axis and other selected columns on x-axis

# Custom function to create a plot matrix with 'X..Silica.Concentrate' on y-axis
ggpairs(df_subset, 
        columns = 2:ncol(df_subset),   # X-axis variables (every other column)
        mapping = ggplot2::aes_string(y = "X..Silica.Concentrate"))  # Y-axis fixed

