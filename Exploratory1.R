
# Import and intial peaks at data 
track_data_raw <- read.csv("directathletics (9).csv")
data_clean1 <- track_data_raw
colnames(track_data_raw)
summary(data_clean1)

# Remove rows with missing names 
data_clean1$Name <- as.character(data_clean1$Name)
length(which(
  data_clean1$Name == ""
))


data_clean1$Time <- as.character(data_clean1$Time)
data_clean1$Distance <- as.character(data_clean1$Distance)

# Combined number of "good" observations 
length(which(
  data_clean1$Distance != "" | data_clean1$Time != ""
))

# Blanks 
length(which(
  data_clean1$Distance == "" & data_clean1$Time == ""
))

# Number of jump observations 
length(which(
  data_clean1$Distance != ""
))

# Number of track observations 
length(which(data_clean1$Time == ""
))


# Number of both 
length(which(
  data_clean1$Distance != "" & data_clean1$Time != ""
))


# Change Data Types 
data_clean1$compiled.href <- as.character(data_clean1$compiled.href)
data_clean1$Gender <- substring(data_clean1$compiled.href, 53, 53)
# Recode Gender 1 if male, 0 if female 
data_clean1$Gender <- ifelse(data_clean1$Gender == 'm', 1, 0)
summary(data_clean1$Gender)



############################################
########### Time Results ##################
############################################

time_rows <- which(data_clean1$Time != "")
time_data <- data_clean1[time_rows, ]

time_cols <- c("ï..meet", "Gender", "Overall", "Name", "Year", "Team", "Time")
time_data1 <- time_data[ , time_cols, drop = FALSE]

# Remove funnny null and DNS 
length(which(data_clean1$Time == "DNS"))
length(which(data_clean1$Time == "null"))

remove_time_rows <- which(data_clean1$Time == "DNS" | data_clean1$Time == "null")
time_data2 <- time_data1[-remove_time_rows, ]

length(as.double(time_data2$Time) == NA)

# Can begin analysis 
time_data2$Time <- as.numeric(time_data2$Time)
hist(time_data2$Time, breaks = 40, probability = T, ylim = c(0, 0.65),
     main = "Distribution of Times ")

# Looks like 2 peaks , possible break around 10 

(low_mean_time <- mean(time_data2$Time[time_data2$Time < 10], na.rm = TRUE))
(low_sd_time <- sd(time_data2$Time[time_data2$Time < 10], na.rm = TRUE))
(high_mean_time <- mean(time_data2$Time[time_data2$Time > 10], na.rm = TRUE))
(high_sd_time <- sd(time_data2$Time[time_data2$Time > 10], na.rm = TRUE))


# The plotting experience 

abline(v = c(low_mean_time, high_mean_time), col = c("red", "blue"), lwd = 3)

low_norm_den <- dnorm(seq(0,15,length = 1000), mean = low_mean_time, sd = low_sd_time)

lines(seq(0,15,length = 1000), low_norm_den, lwd = 2, lty = 4, col = "orange")


high_norm_den <- dnorm(seq(5,20,length = 1000), mean = high_mean_time, sd = high_sd_time)

lines(seq(5,20,length = 1000), high_norm_den, lwd = 2, lty = 4, col = "cyan")

legend("topright", c("Low Mean", "High Mean") ,col = c("red", "blue"), lwd = 3)


