library(tidyverse)
attach(Weather_Data_DATA_SET_44) # to attach the imported dataset

summary(Weather_Data_DATA_SET_44)

# summary of all the variables in the Weather Dataset
table(Weather_Data_DATA_SET_44$Summary)
  p <- ggplot(data = summ_df, aes(x=Category, y=Freq))+
    geom_bar(stat = "identity", fill="cadet blue") +
    geom_text(aes(label=Freq), vjust=-0.3, color="black", size=3.5)+
    theme_minimal()
  p + coord_flip()

table(Weather_Data_DATA_SET_44$`Precipitation Type`)
  pie(summary(factor(`Precipitation Type`)))
  table(Weather_Data_DATA_SET_44_Modified$`Precipitation Type`)
  
  df <- data.frame(
    Group = c("Rain", "Snow"),
    Value = c(171, 28)
  )
  head(df)
  bp<- ggplot(df, aes(x="", y=Value, fill=Group))+
    geom_bar(width = 1, stat = "identity")
  bp
  
  pie <- bp + coord_polar("y", start=0)
  pie
  pie + scale_fill_manual(values = c("cadet blue", "sky blue"))+
    theme_minimal()
  
  
summary(Weather_Data_DATA_SET_44$`Actual Temperature (C)`)
  sd(Weather_Data_DATA_SET_44$`Actual Temperature (C)`)
  
summary(Weather_Data_DATA_SET_44$`Apparent Temperature (C)`)
  sd(`Apparent Temperature (C)`)
  
summary(Weather_Data_DATA_SET_44$Humidity)
  sd(Humidity)

summary(Weather_Data_DATA_SET_44$`Wind Speed (km/h)`)
  sd(`Wind Speed (km/h)`)
  
summary(Weather_Data_DATA_SET_44$`Wind Bearing (degrees)`)
  sd(`Wind Bearing (degrees)`)
  
summary(Weather_Data_DATA_SET_44$`Visibility (km)`)
  sd(`Visibility (km)`)
  
summary(Weather_Data_DATA_SET_44$`Pressure (millibars)`)
  sd(`Pressure (millibars)`)

# to plot a histogram of the Actual Temperature variable
hist(`Actual Temperature (C)`,
     main = "Histogram Showing the Distribution of Actual Temperature",
     xlab = "Actual Temperature (in degree celsius)",
     col = "cadet blue",
     ylim = c(0,40),
     las=1)

# to plot a histogram of the Apparent Temperature variable
hist(`Apparent Temperature (C)`,
     main = "Histogram Showing the Distribution of Apparent Temperature",
     xlab = "Apparent Temperature (in degree celsius)",
     col = "cadet blue",
     ylim = c(0,40),
     las=1)

#to plot a histogram of the Humidity variable
hist(Humidity,
     main = "Histogram Showing the Distribution of Humidity",
     xlab = "Humidity",
     col = "cadet blue",
     ylim = c(0,40),
     las=1)

# to plot a histogram of the Wind Speed variable
hist(`Wind Speed (km/h)`,
     main = "Histogram Showing the Distribution of Wind Speed",
     xlab = "Wind Speed (km/h)",
     col = "cadet blue",
     ylim = c(0,70),
     las=1)

# to plot a histogram of the Visibility variable
hist(`Visibility (km)`,
     main = "Histogram Showing the Distribution of Visibility",
     xlab = "Visibility (km)",
     col = "cadet blue",
     ylim = c(0,70),
     xlim = c(0,20),
     las=1)

# to plot a histogram of the wind bearing variable
hist(Weather_Data_DATA_SET_44$`Wind Bearing (degrees)`,
     main = "Histogram Showing the Distribution of Wind Bearing",
     xlab = "Wind Bearing (degrees)",
     col = "cadet blue",
     xlim = c(0,360),
     las=1)

# to plot a histogram of the Pressure variable
hist(Weather_Data_DATA_SET_44$`Pressure (millibars)`,
     main = "Histogram Showing the Distribution of Pressure",
     xlab = "Pressure (millibars)",
     col = "cadet blue",
     las=1)

par(mfrow=c(1,2))
hist(Weather_Data_DATA_SET_44_Modified$`Pressure (millibars)`,
     main = "Histogram Showing the 
     Distribution of Modified 
     Pressure",
     xlab = "Pressure (millibars)",
     col = "cadet blue",
     las=1)
boxplot(Weather_Data_DATA_SET_44_Modified$`Pressure (millibars)`, col = "cadet blue", 
        main = "Chart Showing a Boxplot 
        of the Modified Pressure",
        ylab = "(millibars)",
        xlab = "Pressure"
)

# boxplot of the Actual Temperature variable
boxplot(`Actual Temperature (C)`, col = "cadet blue", 
        main = "Chart Showing a Boxplot of Actual Temperature",
        ylab = "degrees(C)",
        xlab = "Actual Temperature"
)

# boxplot of the Apparent Temperature variable
boxplot(`Apparent Temperature (C)`, col = "cadet blue", 
        main = "Chart Showing a Boxplot of Apparent Temperature",
        ylab = "degrees(C)",
        xlab = "Actual Temperature")

# boxplot of the Humidity variable
boxplot(Humidity, col = "cadet blue", 
        main = "Chart Showing a Boxplot of the Humidity",
        ylab = "Proportion",
        xlab = "Humidity"
        )

# boxplot of the Wind Speed variable
boxplot(`Wind Speed (km/h)`, col = "cadet blue", 
        main = "Chart Showing a Boxplot of the Wind Speed",
        ylab = "Speed (km/h)",
        xlab = "Wind Speed", 
        ylim = c(0,35),
        frame = FALSE)

# boxplot of the Wind Bearing variable
boxplot(`Wind Bearing (degrees)`, col = "cadet blue", 
        main = "Chart Showing a Boxplot of the Wind Bearing",
        ylab = "(degrees)",
        xlab = "Wind Bearing",
        frame = FALSE,
        horizontal = TRUE)

# boxplot of the visibility variable
boxplot(`Visibility (km)`, col = "cadet blue", 
        main = "Chart Showing a Boxplot of the Visibility",
        ylab = "(km/h)",
        xlab = "Visibility"
)

# boxplot of the Modified Pressure variable
boxplot(Weather_Data_DATA_SET_44_Modified$`Pressure (millibars)`, col = "cadet blue", 
        main = "Chart Showing a Boxplot of the Pressure",
        ylab = "(millibars)",
        xlab = "Pressure"
)

# scatter plot for actual temperature and apparent temperature
plot(`Actual Temperature (C)`,`Apparent Temperature (C)`,
     main = "Scatterplot Showing Relationship between Actual Temperature 
     and Apparent Temperature",
     ylab = "Apparent Temperature (c)",
     xlab = "Actual Temperature (c)",
     pch = 19, frame = FALSE)
abline(lm(`Apparent Temperature (C)`~`Actual Temperature (C)`, 
          data = Weather_Data_DATA_SET_44_Modified), col = "cadet blue")

# scatter plot for actual temperature and humidity
plot(`Actual Temperature (C)`,Humidity,
     main = "Scatterplot Showing Relationship between Actual Temperature
     and Humidity",
     ylab = "Humidity",
     xlab = "Actual Temperature",
     pch = 19, frame = FALSE)
abline(lm(Humidity~`Actual Temperature (C)`, 
          data = Weather_Data_DATA_SET_44_Modified), col = "cadet blue")

# scatter plot for actual temperature and wind speed
plot(`Actual Temperature (C)`,`Wind Speed (km/h)`,
     main = "Scatterplot Showing Relationship between Actual Temperature
     and Wind Speed",
     ylab = "Wind Speed (km/h)",
     xlab = "Actual Temperature (c)",
     pch = 19, frame = FALSE)
abline(lm(`Wind Speed (km/h)`~`Actual Temperature (C)`, 
          data = Weather_Data_DATA_SET_44_Modified), col = "cadet blue")

# scatter plot for actual temperature and wind bearing
plot(`Actual Temperature (C)`,`Wind Bearing (degrees)`,
     main = "Scatterplot Showing Relationship between Actual Temperature
     and Wind Bearing",
     ylab = "Wind Speed (degrees)",
     xlab = "Actual Temperature (c)",
     pch = 19, frame = FALSE)
abline(lm(Weather_Data_DATA_SET_44_Modified$`Wind Bearing (degrees)`
          ~Weather_Data_DATA_SET_44_Modified$`Actual Temperature (C)`, 
          data = Weather_Data_DATA_SET_44_Modified), col = "cadet blue")

# scatter plot for actual temperature and visibility
plot(Weather_Data_DATA_SET_44_Modified$`Actual Temperature (C)`,
     Weather_Data_DATA_SET_44_Modified$`Visibility (km)`,
     main = "Scatterplot Showing Relationship between Actual Temperature
     and Visibility",
     ylab = "Visibility (km)",
     xlab = "Actual Temperature (c)",
     pch = 19, frame = FALSE)
abline(lm(Weather_Data_DATA_SET_44_Modified$`Visibility (km)`
          ~Weather_Data_DATA_SET_44_Modified$`Actual Temperature (C)`, 
          data = Weather_Data_DATA_SET_44_Modified), col = "cadet blue")

# scatter plot for actual temperature and modified pressure
plot(Weather_Data_DATA_SET_44_Modified$`Actual Temperature (C)`,
     Weather_Data_DATA_SET_44_Modified$`Pressure (millibars)`,
     main = "Scatterplot Showing Relationship between Actual Temperature
     and Pressure",
     ylab = "Pressure (millibars)",
     xlab = "Actual Temperature (c)",
     pch = 19, frame = FALSE)
abline(lm(Weather_Data_DATA_SET_44_Modified$`Pressure (millibars)`
          ~Weather_Data_DATA_SET_44_Modified$`Actual Temperature (C)`,
          data = Weather_Data_DATA_SET_44_Modified), col = "cadet blue")

# scatter plot for apparent temperature and actual temperature
plot(`Apparent Temperature (C)`,`Actual Temperature (C)`,
     main = "Scatterplot Showing Relationship between Apparent Temperature
     and Actual Temperature",
     ylab = "Actual Temperature",
     xlab = "Apparent Temperature (c)",
     pch = 19, frame = FALSE)
abline(lm(Weather_Data_DATA_SET_44_Modified$`Actual Temperature (C)`
          ~Weather_Data_DATA_SET_44_Modified$`Apparent Temperature (C)`,
          data = Weather_Data_DATA_SET_44_Modified), col = "cadet blue")

# scatter plot for apparent temperature and humidity
plot(`Apparent Temperature (C)`,Humidity,
     main = "Scatterplot Showing Relationship between Apparent Temperature
     and Humidity",
     ylab = "Humidity",
     xlab = "Apparent Temperature (c)",
     pch = 19, frame = FALSE)
abline(lm(Humidity~`Apparent Temperature (C)`,
          data = Weather_Data_DATA_SET_44_Modified), col = "cadet blue")

# scatter plot for apparent temperature and wind speed 
plot(`Apparent Temperature (C)`,`Wind Speed (km/h)`,
     main = "Scatterplot Showing Relationship between Apparent Temperature
     and Wind Speed",
     ylab = "Wind Speed (km/h)",
     xlab = "Apparent Temperature (c)",
     pch = 19, frame = FALSE)
abline(lm(`Wind Speed (km/h)`~`Apparent Temperature (C)`,
          data = Weather_Data_DATA_SET_44_Modified), col = "cadet blue")

# scatter plot for apparent temperature and wind bearing 
plot(`Apparent Temperature (C)`,`Wind Bearing (degrees)`,
     main = "Scatterplot Showing Relationship between Apparent Temperature
     and Wind Bearing",
     ylab = "Wind Bearing (degrees)",
     xlab = "Apparent Temperature (c)",
     pch = 19, frame = FALSE)
abline(lm(`Wind Bearing (degrees)`~`Apparent Temperature (C)`,
          data = Weather_Data_DATA_SET_44_Modified), col = "cadet blue")

# scatter plot for apparent temperature and visibility 
plot(`Apparent Temperature (C)`,`Visibility (km)`,
     main = "Scatterplot Showing Relationship between Apparent Temperature
     and Visibility",
     ylab = "Visibility (km)",
     xlab = "Apparent Temperature (c)",
     pch = 19, frame = FALSE)
abline(lm(`Visibility (km)`~`Apparent Temperature (C)`,
          data = Weather_Data_DATA_SET_44_Modified), col = "cadet blue")

# scatter plot for apparent temperature and pressure 
plot(Weather_Data_DATA_SET_44_Modified$`Apparent Temperature (C)`,
     Weather_Data_DATA_SET_44_Modified$`Pressure (millibars)`,
     main = "Scatterplot Showing Relationship between Apparent Temperature
     and Pressure",
     ylab = "Pressure (millibars)",
     xlab = "Apparent Temperature (c)",
     pch = 19, frame = FALSE)
abline(lm(Weather_Data_DATA_SET_44_Modified$`Pressure (millibars)`
          ~Weather_Data_DATA_SET_44_Modified$`Apparent Temperature (C)`,
          data = Weather_Data_DATA_SET_44_Modified), col = "cadet blue")

#barplot of modified summary
barplot(modsummvar.data, main = "Barchart Showing the Modified Summary", 
        xlab = "Modified Summary", 
        ylim = c(0,120),
        col = "cadet blue",
        las = 1)

# checking the normality assumption in the Actual Temperature variable
plot(density(`Actual Temperature (C)`))
shapiro.test(Weather_Data_DATA_SET_44_Modified$`Actual Temperature (C)`)

# one sample hypothesis test for the actual temperature
t.test(Weather_Data_DATA_SET_44_Modified$`Actual Temperature (C)`,
       mu=14, alternative = "greater")

#checking the normality assumption in the Apparent Temperature
plot(density(Weather_Data_DATA_SET_44_Modified$`Apparent Temperature (C)`),
     frame = FALSE)

#dataframe for all variables with Modified summary as Cloudy
cloudy_df <- Weather_Data_DATA_SET_44_Modified[`Modified Summary` == 'Cloudy',]

#dataframe for all variables with Modified summary as Overcast
overcast_df <- Weather_Data_DATA_SET_44_Modified[`Modified Summary` == 'Overcast',]

#test of homogeneity of variances
bartlett.test(Weather_Data_DATA_SET_44_Modified$`Apparent Temperature (C)`~
                Weather_Data_DATA_SET_44_Modified$`Modified Summary`)

#equal variance not assummed
t.test(cloudy_df$`Apparent Temperature (C)`,overcast_df$`Apparent Temperature (C)`)

#equal variance assumed 
t.test(cloudy_df$`Apparent Temperature (C)`,overcast_df$`Apparent Temperature (C)`,
       var.equal = T)
