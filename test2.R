# -------------Q1-------------
# load csv into dataframe
london_crime <- read.csv("london-crime-data.csv", 
                         na = "", stringsAsFactors = FALSE)
# show structure
str(london_crime)
# generate Date variable from year and month column
convert_date <- paste('01',
                      london_crime$month,london_crime$year,sep='/')
london_crime$Date <- convert_date
# -------------Q2-------------
# keep only required variables
london_crime <-london_crime[ , 
                             c("borough","major_category","minor_category", "value", "Date")]
# change column names
names(london_crime) <- c("Borough", "MajorCategory", "SubCategory", "Value", "CrimeDate")
# -------------Q3-------------
# change CrimeDate type to Date
london_crime$CrimeDate <- as.Date(london_crime$CrimeDate,"%d/%m/%Y")
# show structure and content after conversion
# we can see CrimeDate has been changed to Date type from chr
str(london_crime)
# -------------Q4-------------
# convert Borough data type from chr to factor
london_crime$Borough <- factor(london_crime$Borough, 
                               levels = london_crime$Borough[order(london_crime$value)])
summary(london_crime$Borough , maxsum = 3)
# from the below graph, we can see 
# Croydon is the borough with highest crimes
# City of London is the borough with lowest crimes
plot(london_crime$Borough, 
     main = "Crimes By Borough",
     xlab = 'Borough',
     ylab = 'number of crimes')

# -------------Q5-------------
# convert MajorCategory into factor type
london_crime$MajorCategory <- as.factor(london_crime$MajorCategory)
# we can see from the below pie chart that :
# a. Theft and Handling category has the highest level of crime
# b. Sexual Offences category is the lowest level of crime 
summary(london_crime$MajorCategory)
# pie chart by MajorCategory
pie(summary(london_crime$MajorCategory)
    , main ="")
# -------------Q6-------------
# solution:
# 1. read the dictionary into a new dataframe
# 2. use london_crime left join the dictionary dataframe
# 3. filter null region and group summary
# 4. replace null
# create borough column
borough_col <- c("Barking and Dagenham",
                 "Barnet",
                 "Bexley",
                 "Brent",
                 "Bromley",
                 "Camden",
                 "Croydon",
                 "Ealing",
                 "Enfield",
                 "Greenwich",
                 "Hackney",
                 "Hammersmith and Fulham",
                 "Haringey",
                 "Harrow",
                 "Havering",
                 "Hillingdon",
                 "Hounslow",
                 "Islington",
                 "Kensington and Chelsea",
                 "Kingston upon Thames",
                 "Lambeth",
                 "Lewisham",
                 "Merton",
                 "Newham",
                 "Redbridge",
                 "Richmond upon Thames",
                 "Southwark",
                 "Sutton",
                 "Tower Hamlets",
                 "Waltham Forest",
                 "Wandsworth",
                 "Westminster")
# create region column
region_col <-c("East",
               "North",
               "East",
               "West",
               "South",
               "North",
               "South",
               "West",
               "North",
               "East",
               "North",
               "West",
               "North",
               "West",
               "East",
               "West",
               "West",
               "Central",
               "Central",
               "East",
               "Central",
               "Central",
               "South",
               "East",
               "East",
               "West",
               "Central",
               "South",
               "Central",
               "Central",
               "East",
               "Central")
# create a dataframe from the above columns as a dictionary dataframe
dict_data <- data.frame(borough_col, region_col)
# change column names 
names(dict_data) <- c("Borough", "Region")
# use london_crime join dict_data by Borough
london_crime_modified <- merge(x = london_crime, y = dict_data, 
      by = "Borough", all.x = TRUE)

# data with null region
uncompleted_region <- london_crime_modified[!complete.cases(london_crime_modified$Region), ]
uncompleted_region$Borough <- factor(uncompleted_region$Borough)
summary(uncompleted_region$Borough)
# replace region = Central where region = NA
london_crime_modified$Region[london_crime_modified$Region == NA] <- 'Central'
# -------------Q7-------------
summary(london_crime_modified$Region)
plot(london_crime_modified$Region, main='Crimes By Region',
     xlab = 'Region',
     ylab = 'number of crimes')
# Central region had the highest number of crimes 
# South region had the lowest number of crimes 
# -------------Q8-------------
# Critique and discuss the major crime category of both regions
highest_crime_dataset <- subset(london_crime_modified, Region == "Central" )
# highest crime region Theft and Handling is the highest catetory of crime and 
# Sexual Offences is the lowest catetory of crime 
summary(highest_crime_dataset$MajorCategory)
# lowest crime region
lowest_crime_dataset <- subset(london_crime_modified, Region == "South" )
# lowest crime region Theft and Handling is the highest catetory of crime 
# Sexual Offences is the lowest catetory of crime as crime
# highest crime region has almost doubled crimes in both categories of that the lowest region
summary(lowest_crime_dataset$MajorCategory)
# -------------Q9-------------
# I don't understand the meaning of this question
# Using information from the summary() function, 
# plot the content of both of your data frames side by side. 
# highest crim summary data as a dataframe
highest_data <- as.data.frame(summary(highest_crime_dataset$MajorCategory))
# add region column
highest_data$Region <- rep('Highest Crim Region - Central')
# lowest crim summary data as a dataframe
lowest_data <- as.data.frame(summary(lowest_crime_dataset$MajorCategory))
lowest_data$Region <- rep('Lowest Crim Region - South')


# -------------Q10-------------
# write london_crime to csv file
write.csv(london_crime_modified, "london-crime-modified.csv")

