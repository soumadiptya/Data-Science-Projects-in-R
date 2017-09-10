#----------------- SETUP --------------#
## Package installation check ##
### Doing check for required packages ##
required_packages <- c("dplyr", "ggplot2", "tidyr", "lubridate", "ggthemes", "gridExtra")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

#Load required libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(tidyr)
#-------------- SETUP - END --------------#

#-------- Data Sourcing ---------#
# Set working directory and read the Uber data set to memory #

setwd("H:/Career Development/Analytics Diploma/UBER case study")
uber_data <- read.csv("Uber Request Data.csv", stringsAsFactors = F)
#---------------- Data Sourcing Complete -------------------#

#------- Data Cleaning -------#
# Let us take a look at the data #
str(uber_data)

# It can be seen that Request.Timestamp and Drop.Timestamp variables are currently character type
# We need to convert them to date type variables but before that lets take a look at first 10 values
head(uber_data, 10)

# Clearly dates are in two different formats with hyphen(-) and backslash(/). 
# Let's convert them all to one format
uber_data <- uber_data %>% mutate(Request.timestamp = gsub("-", "/", Request.timestamp),
                                  Drop.timestamp = gsub("-", "/", Drop.timestamp))
head(uber_data, 10)

# We can see that the command was successfull
# Now let us convert the Request and drop timestamps to proper date time format
# There are two formats here one with seconds and one without seconds. 
# We can use parse_date_time to convert them properly
uber_data$Request.timestamp <-  parse_date_time(uber_data$Request.timestamp,orders = c("dmy_HMS", "dmy_HM"), 
                                                locale = "eng")

uber_data$Drop.timestamp <- parse_date_time(uber_data$Drop.timestamp,orders = c("dmy_HMS", "dmy_HM"), 
                                            locale = "eng" )
str(uber_data)

# All Date time conversions have been done properly as evident above

# Check for duplicates
sum(as.numeric(duplicated(uber_data)))
# There are no duplicate entries in this data set. No other columns need cleaning

# However before begining analysis we need to extract the Request Date, Request time(Hour),
# Drop Date and Drop times(Hour) and Time_taken(per trip) separately. Creating columns for the same
uber_data<-uber_data %>% mutate(Request_Date = date(uber_data$Request.timestamp),
                                Drop_Date = date(uber_data$Drop.timestamp),
                                Request_Hour = hour(uber_data$Request.timestamp),
                                Drop_Hour = hour(uber_data$Drop.timestamp),
                                Time_taken = uber_data$Drop.timestamp-uber_data$Request.timestamp,
                                Time_taken = round(Time_taken,2))

# Note:- Minutes and Seconds will not be very important for analysis as granularity upto hour level,
# would be enough to view and act upon trends. So no separate columns are needed for them. 
#------- Data Cleaning complete -------#

#------- Data Analysis Part 1- Univariate and Segmented Univariate Analysis ------#

# Plot 1- Trips Completed by Pickup Point
ggplot(uber_data, aes(x = Pickup.point, fill = Pickup.point)) +geom_bar() + 
  geom_text(stat = "count",aes(label = ..count..),vjust = -0.5) + 
  scale_fill_pander() + theme_excel() + 
  ggtitle("Total Trips by Pickup Points") + xlab("Pickup Point") + ylab("Count") + 
  theme(plot.title = element_text(hjust = 0.5))

cat("The number of Trips originating from City and the Airport are roughly the same 
    at 3507 and 3238 respectively\n")

#Plot 2- Status of Trips- Shown with a Pie chart
ggplot(uber_data, aes(x = "", fill = Status)) + geom_bar() + coord_polar(theta = "y") + 
  ylab("Count") +   ggtitle("Trip Status") + xlab("Status") + ylab("Count") + 
  theme(plot.title = element_text(hjust = 0.5))

cat("The pie chart provides an interesting insight that the No. of Completed Trips is almost equal to No Cars available.\n ", 
    "The exact Numbers are:-\n",
    "Trips Completed = ", table(uber_data$Status)[3],"\n",
    "Cancelled = ", table(uber_data$Status)[1],"\n",
    "No Cars available = ", table(uber_data$Status)[2],"\n")

# Plot 3- Trip times by Pickup Point- Let us see if there is any significant variation
# in Trip times depending on the pickup point.
ggplot(uber_data, aes(x=Pickup.point, y=as.numeric(Time_taken), fill = Pickup.point)) +
  geom_boxplot(na.rm = T) + ylab("Time Taken (minutes)")+scale_fill_tableau()+
  ggtitle("Trip times by Pickup Point")+
  theme(plot.title = element_text(hjust = 0.5))

cat("There is no significant variation in Trip times from Airport-City or City-Airport\n",
    "They range roughly between 40 minutes to 1 hour\n")

#------ Univariate Analysis Complete------#

#------ Data Analysis Part 2- Bivariate Analysis------#
# Plot 4- Trip Status by Days
# Let us try to see if there is any significant variation in the Trip Status by Days
# Please run below commands together
x11()
ggplot(uber_data, aes(x = Status, fill = Status)) + geom_bar() +
  facet_wrap(~Request_Date) +
  geom_text(stat = "count",aes(label = ..count..), position = position_dodge(width = 0.1),vjust = -0.5) +
  theme_solarized() + ylab("Count") +
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank())
#Note- Removed the ticks and text on x axis to reduce clutter

cat("Clearly there is no significant variation in Trip Status over the 5 days for which Data is given\n",
    "Cancellations, Trips Completed and No Cars Available remain roughly the same,
    as can be seen from the Bar Plots above \n")

# Plot 5- Trip Status by Pickup Point
# Let us try to see how Trip Status vaires by Pickup Point
# Please run below commands together
x11()
ggplot(uber_data, aes(x = Status, fill = Status)) + geom_bar() + facet_wrap(~Pickup.point) +
  geom_text(stat = "count", aes(label = ..count..),position = position_dodge(width = 0.1),vjust = -0.5) +
  theme_dark() + ylab("Count") +
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank())

cat("This graph clearly shows two main Problems for Uber:-\n", 
    "1) 1713", "instances of No cars available from the airport\n",
    "2) 1066", "Cancellations for trips originating from the city\n")

# Plot 6- Trend in cancellations and No cars available over the course of the day
# Let us see if there is a trend in cancellations or No Cars available over the course of the day
# Here we are taking a subset of the uber_data to see counts of
# Cancelled and No Cars Available over time of day

uber_subset <- uber_data %>%filter(Status != "Trip Completed") %>%  group_by(Request_Hour, Status) %>% 
  summarise(No.of.trips = n()) %>% as.data.frame()

# Please run below commands together
x11()
ggplot(uber_subset, aes(x = Request_Hour, y=No.of.trips, col = Status)) + geom_line() +
  ylab("Number of Trips") +ggtitle("Trips lost over Time of day") +
  theme_economist()+scale_x_continuous(breaks = seq(0,24, by=2))+
  theme(plot.title = element_text(hjust = 0.5))

cat("Clearly there is a problem of:- \n", 
    "1) Large spike in cancellations between 5 and 10 Hours\n",
    "2) Even larger spike in No Cars Available between 17 and 22 hours\n")

# Let's further drill down to see if the trend is similar or different for airport and City trips
# Plot 7- Trips lost over Course of Day
uber_drilldown <- uber_data %>%filter(Status != "Trip Completed") %>% 
  group_by(Request_Hour, Status,Pickup.point) %>% 
  summarise(No.of.trips = n()) %>% as.data.frame()

# Please run below commands together
x11()
ggplot(uber_drilldown, aes(x = Request_Hour, y=No.of.trips, col = Status)) + 
  geom_line() +facet_wrap(~Pickup.point) + ylab("Number of Trips") +
  ggtitle("Trips lost drilldown") +
  theme_economist()+scale_x_continuous(breaks = seq(0,24, by=4))+
  theme(plot.title = element_text(hjust = 0.5))

cat("Our drill down graphs by Pickup Location confirm what was learned in the earlier step. 
    However there are some additional insights :-\n", 
    "1) Most instances of unavailability of cars originates from the airport\n",
    "2) Most instances of Cars cancellation arises from the City \n")

# ------ Conclusions Part 1------#
cat("We can conclude that the most problematic requests are (in the order of losses):-\n",
    "1) Trips originating from the airport in the late evening time (between 5 and 10 PM)\n",
    "2) Trips originating from the city during early morning (between 5 and 10 AM)\n",
    "The reason for the problems are Unavailability of cars from Airport and Trip cancellations 
    from City respectively\n")

#------ Part 2 Supply Demand Gap Study -------#
# The various Trip cancellations and No cars available are leading to a gap between Supply and Demand.
# Let us visualise it's magnitude and breakdown over the day for both Airport- City and City- Airport Trips
# To find out the supply-demand gap we have to first define what is supply and demand
# Demand = Total No. of requests = No. of Trips completed + No. of trips cancelled + No cars available
# Supply = No. of Trips completed
# To calculate supply and demand we have to group by Request_Hour and Status and summarise

uber_supply_demand <- uber_data %>% group_by(Request_Hour, Status) %>%
  summarise(No.of.Requests = n()) %>% 
  as.data.frame()

uber_demand <- uber_supply_demand %>% 
  group_by(Request_Hour) %>% 
  summarise(demand = sum(No.of.Requests))

uber_supply <- uber_supply_demand %>% 
  filter(Status == "Trip Completed") %>% 
  group_by(Request_Hour) %>%
  summarise(Supply = sum(No.of.Requests))

#Join the supply and demand dataframes and remove temporary Dataframes
uber_supply_demand <- inner_join(uber_demand, uber_supply, by = "Request_Hour")
rm(uber_demand, uber_supply)

# Let us print out the actual values of supply-demand gap
cat("Supply_demand gap values across the day are:\n", uber_supply_demand$Supply - uber_supply_demand$demand,
    "\n Since the values are negative it indicates a higher demand as compared to the supply")

# Convert Supply and Demand to Long format for ease of Plotting
uber_supply_demand <-uber_supply_demand %>%  gather(Type_of_Request,Counts,demand:Supply)

# Plot 8- Supply Demand gap through the day
ggplot(uber_supply_demand, aes(x=Request_Hour, y=Counts, fill=Type_of_Request)) + 
  geom_col() +geom_line(col="red")+scale_x_continuous(breaks=seq(0,24,by=2))+
  scale_fill_tableau()+theme_economist()+ggtitle("Supply-Demand Gap over Day")+
  theme(plot.title = element_text(hjust = 0.5))

cat("Clearly some supply-demand gap exists throughout the day.\n",
    "However there are peaks between 5 and 10 AM and 5 and 10 PM.\n",
    "At other times the supply-demand gap is negligible.\n")

# Let us drill down at Pickup Point level 
# to see if there is a trend in the Supply demand gap with respect to the Pickup Point
# To do this we will need to introduce an additional grouping by Pickup.point in addition to Request_Hour
uber_supply_demand_route <- uber_data %>%
  group_by(Request_Hour, Status,Pickup.point) %>%
  summarise(No.of.Requests = n()) %>% as.data.frame()

uber_demand_route <- uber_supply_demand_route %>% 
  group_by(Request_Hour,Pickup.point) %>%
  summarise(demand = sum(No.of.Requests))

uber_supply_route <- uber_supply_demand_route %>% 
  filter(Status == "Trip Completed") %>%  
  group_by(Request_Hour,Pickup.point) %>%
  summarise(Supply = sum(No.of.Requests))

#Join the supply and demand dataframes and remove temporary Dataframes
uber_supply_demand_route <- inner_join(uber_demand_route, uber_supply_route, 
                                       by = c("Request_Hour","Pickup.point"))
rm(uber_demand_route, uber_supply_route)

# Convert Supply and Demand to Long format for ease of Plotting
uber_supply_demand_route <-uber_supply_demand_route %>%  
  gather(Type_of_Request,Counts,demand:Supply)

# Plot 9- Supply Demand gap by Pickup Points
ggplot(uber_supply_demand_route, aes(x=Request_Hour, y=Counts, fill=Type_of_Request)) + 
  geom_col() +geom_line(col="red")+scale_x_continuous(breaks=seq(0,24,by=2))+
  scale_fill_tableau()+theme_economist()+ggtitle("Supply-Demand Gap by Point of Pickup")+
  theme(plot.title = element_text(hjust = 0.5))+facet_wrap(~Pickup.point)

# Let us combine Plot 8 and 9 for a very interesting insight
p8 <- ggplot(uber_supply_demand, aes(x=Request_Hour, y=Counts, fill=Type_of_Request)) + 
  geom_col() +geom_line(col="red")+scale_x_continuous(breaks=seq(0,24,by=2))+
  scale_fill_tableau()+theme_economist()+ggtitle("Supply-Demand Gap over Day")+
  theme(plot.title = element_text(hjust = 0.5))

p9 <- ggplot(uber_supply_demand_route, aes(x=Request_Hour, y=Counts, fill=Type_of_Request)) + 
  geom_col() +geom_line(col="red")+scale_x_continuous(breaks=seq(0,24,by=2))+
  scale_fill_tableau()+theme_economist()+ggtitle("Supply-Demand Gap by Point of Pickup")+
  theme(plot.title = element_text(hjust = 0.5))+facet_wrap(~Pickup.point)

# Please run below COmmands together
x11()
grid.arrange(p8, p9, nrow = 2)

cat("Insights from the Supply Demand gap Analysis:-\n",
    "1) The early morning spike in supply demand gap is contributed mainly by cancellations from the city\n ",
    "2) The Late night spike is contributed mainly due to unavailability of cars at the Airport\n")

#------Part 3- Root Cause Analysis of Supply Demand Gap -------#    

#------Late night Supply Demand Gap-------#
# 1) Late Night Supply Demand Gap is caused due to a huge spike in demand at the Airport
# between 5 PM and 10 PM. There is not Enough organic Supply from the city to cope up with
# this spike in Demand. This may be caused because there are many more Flights landing during
# this time than FLights leaving from the City. However we do not have enough Data to prove this Hypothesis.
# The complete time table of flights to and from the city by day of week can supplement this study.

#-------Early Morning Supply Demand Gap------#
# 2) Early Morning Supply Demand Gap is caused due to a huge spike in demand at the City
# between 5 AM and 10 AM. However during this time there are a large no. of cancellations done by drivers
# which leads to a supply demand gap along with a smaller no. of incidences of "No Cars Available"
# One possible reason for cancellations might be that drivers face large wait times on completing a Trip to the Airport
# due to which they refuse to take up City- Airport trips during Early Morning
# This graph compliments the idea-
p9
# It can be seen that the Demand is quite low for Early Morning trips from the Airport to City, which might lead to long wait times
# for Drivers who arrive at the Airport during this time.
# Let us see if this is true by calculating Wait times for drivers  
uber_data <- uber_data %>% arrange(Driver.id,Request.timestamp)
head(uber_data,10)
# We can see that there are succesive trips from Drivers with Pickup Point as Airport or City
# instead of alternate Airport and City pairs. This might occur because drivers take other trips between
# different points in the city for which data is not available or they might work in shifts 
# and elect to go home after completing a trip rather than do another Trip.
# So calculating Idle/Wait times is not possible with the current dataset 
# without supplying additional information.
# In this scenario we can only hypothesize that the cause for cancellations done by drivers
# is long wait times faced at the Airport.

#------ Part 4- Recommendations to resolve the supply-demand gap -------#
# 1) Providing a small but dedicated fleet of cabs at the airport between evening and late night hours. 
# Since there seems to be a huge demand spike in the No. of Requests at this time,
# having a dedicated fleet of cabs will reduce instances of "No Cars available". 
# In order to divert less cars to the Airport UBER might also look at implementing a share cab feature
# such that travelers don't have to book the entire cab by themselves if they don't want to.
##---------------------------------------------------------------##
# 2) Providing a lump sum compensation to drivers for waiting at the airport during early morning hours
# or providing a portion of the fuel costs. This will encourage drivers not to cancel rides 
# between the city and Airport during the morning time slot (5 to 10 AM)
# during which there are many flights leaving from the city.
##---------------------------------------------------------------##
# There is also a problem of unavailability of cars from the city during early morning (to a lesser degree). 
# It can be alleviated by encouraging more drivers to take up the early morning shift, 
# perhaps by giving them slightly higher compensation. 




