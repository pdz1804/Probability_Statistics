# ----------------------------------------------------------------------------------
#                               INSTALLING PACKAGES                                #
# ----------------------------------------------------------------------------------
library(car)
library(caret)
library(dplyr)
library(drc)
library(ggplot2)
library(lessR)
library(randomForest)
library(readr)
library(nlme)
library(nls.multstart)
library(pacman)
library(readxl)
library(tidyverse)
library(zoo)     # for year-quarter formats
library(rio)     # for imports & exports
library(grid)
library(gridExtra)
library(magick)
library(tree)
library(caret)
library(ggplot2)
library(xgboost)
library(randomForest)
library(gbm)

pacman::p_load(
    rio,            # for imports & exports
    ggplot2,        # for plots
    zoo             # for year-quarter formats
)

#-----------------------------------------------------------------------------------#
#                                DATA CLEANING                                      #
#-----------------------------------------------------------------------------------#

# Import data
data <- import("Dataset/cpu_raw.csv")       # rio::import
data1 <- data[, c("Vertical_Segment", "Status", "Launch_Date", "Lithography",
                  "Recommended_Customer_Price", "nb_of_Cores",
                  "Processor_Base_Frequency", "TDP","T")] 

names(data1) <- c("market", "status", "ldate", "litho", "rprice", "ncore", "bfreq", "tdp", "temp")
export(data1, "Dataset/cpu_choose_cols.csv")

# LAUNCHED DATE
# ----------------------------------------------------------------------------------
names(data1)
unique(data1$ldate) 

# Transform some wrong format (ex: "Q1 '15")
data1$ldate <- gsub("\\s*'(\\d{2})", "'\\1", data1$ldate)
# Convert to year quarters
data1[,"ldate"] <- (as.yearqtr(data1[,"ldate"], format = "Q%q'%y"))
unique(data1$ldate)


# LITHOGRAPHY
# ----------------------------------------------------------------------------------
names(data1)
unique(data1$litho)

data1[,"litho"] <- as.numeric(gsub(" nm", "", data1[,"litho"]))

# RECOMENDED PRICE
# ----------------------------------------------------------------------------------
# Here we clean the data for the RECOMMENDED PRICE
# for those price in the range of a and b, we take the Max of them
# for those not, only convert the value
# for those "N/A", convert to NA
unique(data1$rprice)
data1$rprice <- ifelse(data1$rprice == "N/A", NA, data1$rprice)
# Function to convert price string to numeric
convert_price <- function(price_str) {
    # Check if the string is NA
    if (is.na(price_str)) {
        return(NA)
    }
    # Remove "$" sign and commas ","
    price_str <- gsub("\\$", "", price_str)
    price_str <- gsub(",", "", price_str)
    # Check if the string contains a range
    if (grepl("-", price_str)) {
        # Extract numeric parts from the range
        price_range <- 
            as.numeric(unlist(strsplit(price_str, " - ")))
        # Calculate the average of the range
        price_numeric <- max(price_range, na.rm = TRUE)
    } else {
        # Extract numeric part from the string
        price_numeric <- as.numeric(price_str)
    }
    return(price_numeric)
}
# Apply the function to the entire column
data1$rprice <- sapply(data1$rprice, convert_price)


# PROCESSOR BASE FREQUENCY
# ----------------------------------------------------------------------------------
names(data1)
unique(data1$bfreq)

# Extract numeric part properly and convert to GHz (by multiply 0.001) for non-NA values
non_na_indices <- !is.na(data1$bfreq)
data1$bfreq[non_na_indices] <- 
    ifelse(grepl("GHz", data1$bfreq[non_na_indices]),
           as.numeric(sub("\\s*GHz", "", data1$bfreq[non_na_indices])),
           as.numeric(sub("\\s*MHz", "", data1$bfreq[non_na_indices]))*0.001)

# Unit GHz
data1$bfreq <- as.numeric(data1$bfreq)
data1 <- data1[!is.na(data1$bfreq), ]
unique(data1$bfreq)

# THERMAL DESIGN POWER
# ----------------------------------------------------------------------------------
unique(data1$tdp)

data1[,"tdp"] <- as.numeric(gsub(" W", "", data1[,"tdp"]))

unique(data1$tdp)

# TEMPERATURE
# ----------------------------------------------------------------------------------
# Take the Largest value
names(data1)

unique(data1$temp)

# Function to extract numeric values and find the maximum value
get_max_numeric <- function(row) {
    # Extract numeric values from the row and remove non-numeric characters
    numeric_values <- str_extract_all(row, "-?\\d*\\.?\\d+")
    
    # Convert the extracted numeric values to numeric data type
    numeric_values <- lapply(numeric_values, function(x) as.numeric(x))
    
    # Filter out NA values
    numeric_values <- lapply(numeric_values, function(x) x[!is.na(x)])
    
    # If there are no numeric values, return NA
    if (length(unlist(numeric_values)) == 0) {
        return(NA)
    }
    
    # Find the largest numeric value
    max_value <- max(unlist(numeric_values))
    
    return(max_value)
}

# Apply the function to each row of the dataframe data3$T
max_values <- sapply(data1$temp, get_max_numeric)

# Replace the "T" column with the maximum values
data1$temp <- max_values

#PRINT DATASET AFTER DROPPING NAs
# ----------------------------------------------------------------------------------

names(data1)

# Select columns from 3rd column to the last column of the data1 dataframe and store it in df
df <- data1[, 3:ncol(data1)]

df <- summary(df)

# Convert the summary dataframe to a matrix
summary_df <- as.matrix(df)

# Open a PNG device to save the plot
png("Data_Cleaning/before_drop_NAs.png", width = 11, height = 4, unit = "in", res = 300)

# Add a title to the plot
grid.text("Dataset after cleaning", x = unit(0.5, "npc"), y = unit(0.9, "npc"), just = "center", gp = gpar(fontsize = 18, fontface = "bold"))

# Create a grid table from the summary matrix with text centered
grid_summary <- tableGrob(summary_df, theme = ttheme_default(
    core = list(fg_params = list(hjust = 0.5))  # Center text horizontally
))

# Draw the table onto the PNG
pushViewport(viewport(x = 0.5, y = 0.5, width = 1, height = 1))
grid.draw(grid_summary)
popViewport()

# Close the PNG device
dev.off()

# ----------------------------------------------------------------------------------
# Summary for each Variables
xtabs(~status,data=data1)
xtabs(~ldate,data=data1)
xtabs(~litho,data=data1)
xtabs(~rprice,data=data1)
xtabs(~ncore,data=data1)
xtabs(~bfreq,data=data1)
xtabs(~tdp,data=data1)
xtabs(~temp,data=data1)

# Drop NA values of these columns as they are not much
data1 <- data1[!is.na(data1$tdp), ]
data1 <- data1[!is.na(data1$litho), ]
data1 <- data1[!is.na(data1$temp), ]

summary(data1)


export(data1, "Dataset/cpu_clean.csv")
# ----------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------#
#                              DESCRIPTIVE STATISTICS                               #
#-----------------------------------------------------------------------------------#

#  IMPORT THE DATA
data <- import("Dataset/cpu_clean.csv")        # rio::import

# ----------------------------------------------------------------------------------
# lets do some summary of the data first
df <- data[,3:ncol(data)]
# Generate summary statistics for the data frame
summary_df <- summary(df)
# Convert summary to character matrix
summary_df <- as.matrix(summary_df)


# DESCRIPTIVE STATISTICS TABLE
# ----------------------------------------------------------------------------------
png("Descriptive_statistics/summary_data_frame.png", width = 11, height = 4, unit = "in", res = 300)
grid.text("Descriptive Statistics of Data", x = unit(0.5, "npc"), y = unit(0.9, "npc"), just = "center", gp = gpar(fontsize = 18, fontface = "bold"))

# Create a grid table from the summary matrix with text centered
grid_summary <- tableGrob(summary_df, theme = ttheme_default(
    core = list(fg_params = list(hjust = 0.5))  # Center text horizontally
))

# Draw the table onto the PDF
pushViewport(viewport(x = 0.5, y = 0.5, width = 1, height = 1))
grid.draw(grid_summary)
popViewport()

# Close PNG device
dev.off()


# PLOT LITHO & LDATE
# ----------------------------------------------------------------------------------
# Box-plotting
# ------------
data <- data[!is.na(data$litho), ]
data$litho <- as.factor(data$litho)

p1 <- ggplot(data, aes(x = ldate, y = litho)) +
    geom_boxplot(fill = "deepskyblue", color = "black", outlier.shape = 8, outlier.size = 2) +
    labs(title = "Boxplot of Lithography over Time", 
         subtitle = "Each box represents the distribution of lithography for a given time period",
         x = "Launch Date", 
         y = "Lithography (nm)") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face="bold", size=14),
          plot.subtitle = element_text(hjust = 0.5, face="italic", size=12),
          plot.caption = element_text(hjust = 1, face="italic", size=10),
          axis.title.x = element_text(face="bold", size=12),
          axis.title.y = element_text(face="bold", size=12))

# -----------------
# Scatter plotting
# -----------------
p2 <- ggplot(data, aes(x = ldate, y = litho)) +
    geom_area(alpha = 0.5, fill = "deepskyblue") +
    #geom_smooth(method = lm, se = F, color = "red") +
    geom_point(color = "red", alpha = 0.5, size = 1) + 
    labs(title = "Lithography over time", 
         subtitle = "An area plot showing the change in lithography over time",
         x = "Launch Date", 
         y = "Lithography (nm)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face="bold", size=14),
          plot.subtitle = element_text(hjust = 0.5, face="italic", size=12),
          plot.caption = element_text(hjust = 1, face="italic", size=10),
          axis.title.x = element_text(face="bold", size=12),
          axis.title.y = element_text(face="bold", size=12))

png("Descriptive_statistics/lito_and_ldate.png", width = 15, height = 6, units = "in", res = 300)
grid.arrange(p1,p2,ncol = 2, widths = c(1,1))
dev.off()

# HISTOGRAM of TDP
#---------------------------------------------------------------------------------
p <- ggplot(data, aes(x = tdp)) +
    geom_histogram(binwidth = 5, fill = "orange") +
    xlab("TDP (W)") +
    ylab("Number of CPUs") +
    geom_segment(aes(x = -Inf, xend = Inf, y = 0, yend = 0), 
                 arrow = arrow(length = unit(0.02, "npc")), color = "black") +
    geom_segment(aes(x = 0, xend = 0, y = -Inf, yend = Inf), 
                 arrow = arrow(length = unit(0.02, "npc")), color = "black") +
    annotate("text", x = 0, y = 0, label = "W, cpus", hjust = 1.5) + 
    scale_y_continuous(breaks = seq(0, max(hist(data$tdp, plot=FALSE)$counts), by = 50))

# Create the summary table
mode_calc <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

summary_data <- c(summary(data$tdp), Mode = mode_calc(data$tdp))
summary_df <- as.matrix(summary_data)
summary_table <- tableGrob(summary_df)

png("Descriptive_statistics/TDP_summary.png", width = 10, height = 7, unit = "in", res = 300)
grid.arrange(p, summary_table, ncol = 2, widths = c(3, 2))
dev.off()

# PLOTTING NCORE AND TDP
#----------------------------------------------------------------------------------
# BOXPLOT
# -------------------------------
plot1 <- ggplot(plot_data, aes(x = ncore, y = tdp)) +
    geom_boxplot(fill = "deepskyblue", outlier.shape = 8, outlier.size =2) +
    labs(title = "Boxplot of TDP for each Ncore",
         x = "Ncore",
         y = "TDP (Watts)") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

# GROUPED BAR PLOT
# -----------------------------------------
# Calculate the first and third quartiles (Q1 and Q3) for each ncore
filtered_data <- plot_data
filtered_data <- filtered_data %>%
    group_by(ncore) %>%
    mutate(Q1 = quantile(tdp, 0.25),
           Q2 = quantile(tdp, 0.5),
           Q3 = quantile(tdp, 0.75))

# Filter the tdp for each ncore to its Q1 value
filtered_data <- filtered_data %>%
    group_by(ncore) %>%
    filter(tdp <= Q1)

# plotting code with the new filtered data set
plot2 <- ggplot(filtered_data, aes(x=ncore, y=tdp, fill=ncore)) +
    geom_bar(position="dodge", stat="identity") +
    labs(title = "Grouped Bar Plot of TDP for each Ncore",
         x = "Ncore",
         y = "TDP (Watts)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank())

png("Descriptive_statistics/TDP_Ncore.png", width = 14, height = 7, res = 300, unit = "in")
grid.arrange(plot1, plot2, ncol = 2)
dev.off()

# LITHO AND TDP
#----------------------------------------------------------------------------------
# Convert litho to a factor
plot_data$litho <- as.factor(plot_data$litho)
# Create the base plot
p <- ggplot(plot_data, aes(x = litho, y = tdp)) + 
    geom_boxplot(aes(fill = litho), alpha = 0.7, outlier.shape = 8, outlier.size = 3)

p <- p + labs(title = "Boxplot of TDP by Litho",
              x = "Litho",
              y = "TDP")
p <- p + theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold"),
          legend.position = "none")

# Save the plot
png("Descriptive_statistics/TDP_litho.png", width = 14, height = 7, unit = "in", res = 300)
print(p)
dev.off()

# PLOTTING LITHO AND PROCESSOR BASE FREQUENCY
#-----------------------------------------------------------------------
p <- ggplot(plot_data, aes(x = bfreq, y = tdp)) +
    geom_point(color = "firebrick", size = 1, alpha = 0.7) +
    geom_smooth(method = "lm") +
    labs(title = "TDP vs. Bfreq",
         x = "Bfreq (GHz)",
         y = "TDP (Watts") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
png("Descriptive_statistics/tdp_breq.png", width = 7, height = 6, unit = "in", res = 300)
print(p)
dev.off()

# TEMP VS TDP
#----------------------------------------------------------------------------
p <- ggplot(plot_data, aes(x = temp, y = tdp)) +
    geom_point(color = "blue", size = 0.4, alpha = 0.7) +
    geom_abline(intercept= -50, slope = 1.2, color = "red") +
    labs(title = "TDP vs. Temperature",
         x = "Temperature (°C)",
         y = "TDP (Watts)") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

# Create a grob (graphical object) for the plot
plot_grob <- ggplotGrob(p)

png("Descriptive_statistics/TDP_temp.png", width=8, height=4, unit = "in", res = 300)
grid.arrange(plot_grob, ncol = 1)
dev.off()


# TDP v.s. TEMP AND TDP v.s. MARKET
#----------------------------------------------------------------------------

market_plot <- ggplot(data, aes(x = tdp, fill = market)) +
    #geom_bar(position="dodge", stat="identity") +
    geom_density(alpha = 0.6) + 
    labs(title = "Bar plot for each market",
         x = "Market",
         y = "TDP (Watts)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank())
png("Descriptive_statistics/tdp_market.png", width = 5, height = 7, unit = "in", res = 300)
print(market_plot)
dev.off()

# Counting occurrences of each market type
market_counts <- table(data$market)
# Displaying the counts
print(market_counts)
data1 <- data
data1$type <- ifelse(data1$market == 'Server' | data1$market == 'Desktop', "Computers", "Devices")
# This line creates a new column type in the data1 dataframe. The ifelse() function is used to assign values to this new column based on the values in the market column. If the market value is ‘Server’ or ‘Desktop’, ‘Computers’ is assigned to type. Otherwise, ‘Devices’ is assigned.

data1$type <- as.factor(data1$type)


# Create the DENSITY PLOT of TDP for EACH TYPE
#----------------------------------------------------------------------------------
p <- ggplot(data1, aes(x = tdp, fill = type)) + 
    geom_density(alpha = 0.6) +
    scale_fill_manual(values = c("Computers" = "deepskyblue", "Devices" = "firebrick3")) +
    labs(title = "Density Plot of TDP for Each Type",
         x = "TDP",
         y = "Density",
         fill = "Type") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold"))


# PLOT SEPERATE TYPES OF CPU
#-------------------------------------------------------------------------
p1 <- ggplot(data1, aes(x = temp, y = tdp)) + 
    geom_point(aes(color = type), alpha = 0.6, size = 1) +
    scale_color_manual(values = c("Computers" = "deepskyblue", "Devices" = "firebrick3")) +
    facet_wrap(~type, scales = "free") 
# Add title and labels
p1 <- p1 + labs(title = "Scatterplot of TDP by Temp for Each Type",
                x = "Temp",
                y = "TDP",
                color = "Type")

p1 <- p1 + theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold"))

png("Descriptive_statistics/type.png", width = 15, height = 5, unit = "in", res = 300)
grid.arrange(p, p1, ncol = 2)
dev.off()

#------------------------------------------------------------------------------------#
#                                   DATA ANALYSIS                                    #
#------------------------------------------------------------------------------------#

data <- import("Dataset/cpu_clean.csv") # rio::impor

# SUMMARY OF DATA AND LITHO
# ----------------------------------------------------------------------------------
summary(data)
unique(data$litho)
names(data)

# Data summary by xtabs
# Occurrences of TDP >= 150 is rare --> may delete
xtabs(~tdp,data=data)
xtabs(~litho,data=data)

# Remove data with few count group ldate and remove NA
data <- data[data$litho != 28, ]
data <- data[data$litho != 250, ]
xtabs(~litho,data=data)

summary(data)

# Remove the NA values
data <- data[!is.na(data$tdp), ]
data <- data[!is.na(data$litho), ]
summary(data)
unique(data$litho)

# Occurrences of TDP >= 150 is rare --> may delete
data <- data[data$tdp < 150, ]
data <- data[!is.na(data$tdp), ]
data <- data[!is.na(data$bfreq), ]
data <- data[!is.na(data$litho), ]
data <- data[!is.na(data$ncore), ]
data <- data[!is.na(data$temp), ]
summary(data)

# Store dataframe data into data2
data2 <- data
xtabs(~tdp,data=data)

# BUILD MULTI LINEAR REGRESSION MODEL
model.lr <- lm(tdp ~ ncore + bfreq + temp , data = train)

# Second Assumption: Homoscedasticity - Residual Errors have Constant Variance
#---------------------------------------------------------------------------
# Residual Errors have Constant Variance by using the Scale-Location plot.
# We can check assumption:
png("MLR/scale_location.png")
# Generate the plot
plot(model.lr, which = 3,
     col = "blue", pch = 20)
# Add a horizontal line at y = 0
abline(h = 0, col = "deepskyblue", lwd = 2)
dev.off()

# Third Assumption: Multivariate - The Errors are normally distributed
#---------------------------------------------------------------------------
# HISTOGRAM RESIDUALS
#----------------------------------------------------------------------------
png("MLR/hist_residuals.png", width = 5, height = 7, unit = "in", res = 300)
p <- ggplot(model.lr, aes(x = resid(model.lr))) + 
    geom_histogram(binwidth = 2, fill = "darkorange") +
    labs(
        title = "Histogram of Residual Errors",
        x = "Residual Errors",
        y = "Frequency"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, color = "darkblue", size = 14, face = "bold.italic"),
        axis.title.x = element_text(color = "blue", size = 12, face = "bold"),
        axis.title.y = element_text(color = "blue", size = 12, face = "bold")
    )
print(p)
dev.off()

# QQ-PLOT
#-----------------------------------------------------------------------------
#Testing for normality of the the errors:
    # test if the residuals is normally distributed, 
    # we plot the Q-Q plot using the command `plot()`.
    png("MLR/qq_plot.png")
# Generate the plot
plot(model.lr, which = 2,
     col = "firebrick", pch = 20)
dev.off()

# Fourth Assumption: Independence of Observations
#------------------------------------------------------------------
#perform Durbin-Watson test
durbinWatsonTest(model)


# Fifth Assumption: Multicollinearity
# ------------------------------------------------------------------------------------
model <- lm(tdp ~ ncore + bfreq + temp , data = train)

# Calculating VIF
vif_values <- vif(model)
vif_values
# Visualizing VIF
barplot(vif_values, col = "skyblue", main = "Variance Inflation Factor (VIF)")


# PREDICTED TDP VS ACTUAL TDP
#-----------------------------------------------------------------------------------
# Summary of the model
#---------------------------
ss <- summary(model.lr)
print(ss)

# Create data frame for actual tdp value and predicted tdp value
png("MLR/tdp_predicted.png", width = 5, height = 7, unit = "in", res = 300)
comtab.lr <- test['tdp']
comtab.lr['tdp_predicted'] <- as.data.frame(predict(model.lr, newdata = test))

p <- ggplot(comtab.lr, aes(x = tdp, y = tdp_predicted)) + 
    geom_point(color="firebrick", alpha = 0.6) + 
    geom_abline(mapping=aes(intercept= 0, slope=1), color="darkblue") + 
    labs(
        title = "TDP vs Predicted TDP",
        x = "TDP",
        y = "TDP Predicted"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, color = "darkblue", size = 14, face = "bold.italic"),
        axis.title.x = element_text(color = "blue", size = 12, face = "bold"),
        axis.title.y = element_text(color = "blue", size = 12, face = "bold")
    )
print(p)
dev.off()

# Calculating MAE, MSE, RMSE
# ---------------------------------------------------------------------------
comtab.lr <- test['tdp']
comtab.lr['tdp_predicted'] <- as.data.frame(predict(model.lr, newdata = test), row.names = NULL)
print(paste("MSE: ", mean((comtab.lr$tdp - comtab.lr$tdp_predicted)^2)))
print(paste("MAE: ", caret::MAE(comtab.lr$tdp, comtab.lr$tdp_predicted)))
print(paste("RMSE: ", caret::RMSE(comtab.lr$tdp, comtab.lr$tdp_predicted)))



#--------------------------------------------------------------------------------#
#                                   XGBOOST                                      #
#--------------------------------------------------------------------------------#

# Read data from a file
data1 <- read.table(file.choose(), header = T, sep = ",")

# Set seed for reproducibility
set.seed(123)

# Split data into training and testing sets
parts <- sample(1:nrow(data1), nrow(data1) * 0.8)
train <- data1[parts,]
test <- data1[-parts,]

# Extract predictors (features) and target variable from training and testing sets
train_x <- data.matrix(train[,c("ncore","bfreq","temp")])
train_y <- train[,"tdp"]
test_x <- data.matrix(test[, c("ncore","bfreq","temp")])
test_y <- test[, "tdp"]

# Convert data to xgboost matrix format
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

# Define watchlist for tracking performance during training
watchlist = list(train = xgb_train, test = xgb_test)

# Define parameters for the xgboost model
param_list <- list(booster = "gbtree", eta = 0.1, max_depth = 6)

# Train the xgboost model
final = xgboost(params = param_list, data = xgb_train, nrounds = 200, verbose = 0)

# Print final model details
print(final)

# Make predictions on test data
pred_tdp <- predict(final, xgb_test)

# Create dataframe for test_y and predicted values
accuracyTest <- as.data.frame(test_y, row.names = NULL)
accuracyTest["Predicted_tdp"] <- as.data.frame(pred_tdp, row.names = NULL)

# Measure prediction accuracy
print(paste("MSE: ", mean((test_y - pred_tdp)^2))) # Mean Squared Error (MSE)
print(paste("MAE: ", caret::MAE(test_y,pred_tdp))) # Mean Absolute Error (MAE)
print(paste("RMSE: ", caret::RMSE(test_y,pred_tdp))) # Root Mean Squared Error (RMSE)

# Calculate R squared on testing data
r2_check <- cor(accuracyTest$test_y, accuracyTest$Predicted_tdp)^2
print(r2_check)

# Calculate the residuals
residualsGB <- accuracyTest$test_y - accuracyTest$Predicted_tdp

# Create a normal probability plot of the residuals
qqnorm(residualsGB)
qqline(residualsGB)

# Plot predicted vs actual
ggplot(accuracyTest, aes(x = test_y, y = accuracyTest$Predicted_tdp)) + 
    geom_point(shape=1, color="darkblue") +
    geom_abline(mapping=aes(intercept= 0, slope=1),color="red") + 
    labs(x = "TDP", y = "TDP Predicted")

