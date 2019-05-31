
##There are six attributes associated with each request made by a customer:
  #1.	Request id: A unique identifier of the request
  #2.	Time of request: The date and time at which the customer made the trip request
  #3.	Drop-off time: The drop-off date and time, in case the trip was completed 
  #4.	Pick-up point: The point from which the request was made
  #5.	Driver id: The unique identification number of the driver
  #6.	Status of the request: The final status of the trip, that can be either completed, cancelled by the driver or no cars available
  #Note: For this assignment, only the trips to and from the airport are being considered.

#loading of the packages required
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

#Loading the Data Set
setwd("C:/Users/Ranjita/Desktop/Data Analytics_IITM/Uber Case Study")
#Retaining one set of actual data
Uber_Master_data<-read.csv("Uber request data.csv",stringsAsFactors = FALSE,na.strings = TRUE)
# Making dataset for tabular use
dataset <- Uber_Master_data

#1.	Identify the data quality issues and clean the data so that you can use it for analysis.
# converting to Request timestamp column to date and time format
dataset$Request.timestamp <- str_replace_all(dataset$Request.timestamp,"[/]", "-")
dataset$Request.timestamp<- as.POSIXlt(dataset$Request.timestamp,format="%d-%m-%Y %H:%M")
# Extract Date and hour of the day for 11-15 Jul 16(05 Days)
dataset$Req_hour <- format(dataset$Request.timestamp, "%H")
# Converting Hour to numeric values
dataset$Req_hour <- as.numeric(dataset$Req_hour)
# Extracting Date form the Request timestamp
dataset$Req_date <- as.Date(dataset$Request.timestamp)
# Extracting date and converting it to numeric value
dataset$Req_day <- format(dataset$Req_date, "%d")
dataset$Req_day <- as.numeric(dataset$Req_day)

summary(dataset)
#2.	Ensure that the dates and time are in the proper format. Derive new variables which will be useful for analysis.
#make separate Time slots(05 slots)- Early_Morning(0001-0559 Hrs), Morning_Time(0600- 1059 Hrs), Day_Time( 1100- 1759 Hrs), Evening_Time(1800-2259),Night_Time(2300-0000 Hrs)
dataset$Time_slot = ifelse(dataset$Req_hour < 5, "Early_Morning", ifelse(dataset$Req_hour < 10,"Morning_Time",ifelse(dataset$Req_hour < 17,"Day_Time",ifelse(dataset$Req_hour< 22,"Evening_Time","Night_Time"))))
#dataset$time_slot = if(dataset$req_hour < 5){dataset$time_slot="Pre_Morning"} else if (dataset$req_hour < 10 {dataset$time_slot="Morning_Rush"}



#Visually identify the most pressing problems for Uber. 
##Hint: Create plots to visualise the frequency of requests that get cancelled or show 'no cars available'; identify the most problematic types of requests (city to airport / airport to city etc.) and the time slots (early mornings, late evenings etc.) using plots

# Plot the number of cabs requested in a particular hour for all 05 days.Pickup points(Airport/City) wwoud be  displayed in two colors
hourwise_request_count <- ggplot(dataset,aes(x=factor(Req_hour),fill=factor(Pickup.point)))
#Add title and lables to the plot and save it as a object
plot1 <- hourwise_request_count+geom_bar(stat='count',position = "dodge")+
  ggtitle("Demand(Hourly) for Uber Cabs")+labs(x="Time(Hrs)", y="No of Cabs Requested")+labs(fill="Pickup Point")
### Plotting a Bar Chart depicting the hour-wise trip request made at city and airport respectively. It has been aggregated for  all 5 days on x-axis for 24 Hrs. Each bar should correspond to an hour and pick-up point (city / airport) should be displayed in two colors.( 
#view the plot
plot1

#Find out the gap between supply and demand and show the same using plots.
#Find the time slots when the highest gap exists
# Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slotsif else
#What do you think is the reason for this issue for the supply-demand gap? Write the answer in less than 100 words. You may accompany the write-up with plot(s).
#Recommend some ways to resolve the supply-demand gap.

#1.	Visually identify the most pressing problems for Uber. 
#Hint: Create plots to visualise the frequency of requests that get cancelled or show 'no cars available'; identify the most problematic types of requests (city to airport / airport to city etc.) and the time slots (early mornings, late evenings etc.) using plots

#Subset the master dataframe for Trips completed.
trips_completed <- subset(dataset,dataset$Status=="Trip Completed")

#Ploting a bar chart with time-slots on x axis and trips completed on Y-axis.Time Slots- Early_Morning(0001-0559 Hrs), Morning_Time(0600- 1059 Hrs), Day_Time( 1100- 1759 Hrs), Evening_Time(1800-2259),Night_Time(2300-0000 Hrs)
# The "Time_Slot" takes these 5 categorical values depending on the request time.??? It has been aggregated for  all 5 days on x-axis.
Bar_graph_Trips_Completed <- ggplot(trips_completed,aes(x=factor(Time_slot)))

plot_Trips_Completed <- Bar_graph_Trips_Completed+geom_bar(stat="count",col="black",fill="orange")+ggtitle("Trips completed during different Time Slots")+labs(x="Time Slots",y="Trips Completed")+
  geom_text(stat='count',aes(label=..count..),vjust=-1)+guides(fill=FALSE)+scale_x_discrete(limits=c("Early_Morning","Morning_Time","Day_Time", "Evening_Time","Night_Time"))
#view the plot
plot_Trips_Completed

#Subset the master dataframe for No Cars Available.
No_Cars_Available <- subset(dataset,dataset$Status=="No Cars Available")

#Ploting a bar chart with time-slots on x axis and No Cars Available on Y-axis.Time Slots- Early_Morning(0001-0559 Hrs), Morning_Time(0600- 1059 Hrs), Day_Time( 1100- 1759 Hrs), Evening_Time(1800-2259),Night_Time(2300-0000 Hrs)
# The "Time_Slot" takes these 5 categorical values depending on the request time.??? It has been aggregated for  all 5 days on x-axis.
Bar_graph_Trips_Completed <- ggplot(No_Cars_Available,aes(x=factor(Time_slot)))

plot_No_Cars_Available <- Bar_graph_Trips_Completed+geom_bar(stat="count",col="black",fill="brown")+ggtitle("No Cars Available during different Time Slots")+labs(x="Time Slots",y="Trips Completed")+
  geom_text(stat='count',aes(label=..count..),vjust=-1)+guides(fill=FALSE)+scale_x_discrete(limits=c("Early_Morning","Morning_Time","Day_Time", "Evening_Time","Night_Time"))
#view the plot
plot_No_Cars_Available

#Subset the master dataframe for No Cars Available.
Cancelled <- subset(dataset,dataset$Status=="Cancelled")

#Ploting a bar chart with time-slots on x axis and Cancelled trips on Y-axis.Time Slots- Early_Morning(0001-0559 Hrs), Morning_Time(0600- 1059 Hrs), Day_Time( 1100- 1759 Hrs), Evening_Time(1800-2259),Night_Time(2300-0000 Hrs)
# The "Time_Slot" takes these 5 categorical values depending on the request time.??? It has been aggregated for  all 5 days on x-axis.
Bar_graph_Cancelled_Trip <- ggplot(Cancelled,aes(x=factor(Time_slot)))

plot_Cancelled_Trip <- Bar_graph_Cancelled_Trip+geom_bar(stat="count",col="black",fill="pink")+ggtitle("Cancelled Trips during different Time Slots")+labs(x="Time Slots",y="Trips Completed")+
  geom_text(stat='count',aes(label=..count..),vjust=-1)+guides(fill=FALSE)+scale_x_discrete(limits=c("Early_Morning","Morning_Time","Day_Time", "Evening_Time","Night_Time"))
#view the plot
plot_Cancelled_Trip

#2.	Find out the gap between supply and demand and show the same using plots.
#	Find the time slots when the highest gap exists
#	Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots

#plotting a bar chart with time slots on x-axis and request frequency on y-axis. It includes Trip Completed/No Cars Available/ Cancelled (in different Colours)
Request_count <- ggplot(dataset,aes(x=factor(Time_slot),fill=factor(Status)))
Plot_Request_Frequency <- Request_count+geom_bar(stat="count",position = "stack",col="black")+ggtitle("Trips/ Different Time Slots")+
scale_x_discrete(limits=c("Early_Morning","Morning_Time","Day_Time", "Evening_Time","Night_Time"))+
labs(x="Time Slots",y="Number of Requests")+labs(fill="Trip Status")+
scale_fill_discrete(limits=c("Trip Completed","No Cars Available","Cancelled"))

#view the plot
Plot_Request_Frequency

## Issue I:  It has emerged that a Large number of service requests are getting cancelled during the Morning_Time(0600- 1059 Hrs) slot.
#Subset the Morning Rush time slot data for analysis
Issue_Morning_Time <- subset(dataset,dataset$Time_slot=="Morning_Time")
#Plot the bargraph with status of request in x-axis and count in y-axis for Morning rush time slot
#Show the request from different pickup points in different colors
Issue_Morning_Time_count <- ggplot(Issue_Morning_Time,aes(x=factor(Status),fill=factor(Pickup.point)))
plot_Morning_Time <- Issue_Morning_Time_count+geom_bar(stat="count",position = "stack")+
  ggtitle("Morning Time (0600-1059 Hrs) Cab Status")+
  labs(x="Status",y="Total count")+
  labs(fill="Pickup Point")+scale_x_discrete(limits=c("Trip Completed","Cancelled","No Cars Available"))
#view the plot
plot_Morning_Time
# Conclusion from Plot: It has emerged that the maximum trips has been cancelled from City to Airport in the Morning Hours.


#Number of trips cancelled for the Morning rush time slot
Trip_Cancel_Total <- length(which(Issue_Morning_Time$Status=="Cancelled"))
#Number of trips cancelled from airport for Morning rush
Trip_Cancel_Airport <- length(which((Issue_Morning_Time$Pickup.point=="Airport") & (Issue_Morning_Time$Status == "Cancelled")))
# Number of trips cancelled from city for Morning rush
Trip_Cancel_City <- length(which((Issue_Morning_Time$Pickup.point=="City") & (Issue_Morning_Time$Status == "Cancelled")))
# Percentage of trips cancelled from city out of total trips cancelled during morning rush
Percent_Trip_Cancel_City <- (Trip_Cancel_City/Trip_Cancel_Total*100)
# Percentage of trips cancelled from airport out of total trips cancelled during Morning rush
Percent_Trip_Cancel_Airport <- (Trip_Cancel_Airport/Trip_Cancel_Total*100)
# Number of trips requested from city to airport during morning rush
Demand_Trip_Request_City <- length(which(Issue_Morning_Time$Pickup.point=="City"))
#Number of trips completed from city to airport during morning rush
Demand_Trip_City_Completed <- length(which((Issue_Morning_Time$Pickup.point=="City")& (Issue_Morning_Time$Status=="Trip Completed")))
plot(Demand_Trip_Request_City ,Demand_Trip_City_Completed, col = "red", lwd = 10,title("Demand Supply Correlation Morning_Time(0600- 1059 Hrs)"))

##Issue 2: Cabs were not available for a large number of requests during the Evening_Time(1800-2259) also. 
#subset the data for Evening rush from dataframe for analysis
Issue_Evening_Time <- subset(subset(dataset,dataset$Time_slot=="Evening_Time"))
#plot the bar graph with status of requests on x-axis and count in y-axis for evening rush time slot
# Show the request from different pickup points in different colors
Issue_Evening_Time_count <- ggplot(Issue_Evening_Time,aes(x=factor(Status),fill=factor(Pickup.point)))
plot_Issue_Evening_Time<- Issue_Evening_Time_count +geom_bar(stat="count",position = "stack")+
  ggtitle("Evening_Time Cabs Status")+
  labs(x="Status",y="Total count")+
  labs(fill="Pickup Point")+scale_x_discrete(limits=c("No Cars Available","Trip Completed","Cancelled"))
#view the plot
plot_Issue_Evening_Time
# Conclusion: It has emerged that Cabs were not available for booking from Airport in the Evening Hours




# No of service requests with no cars available for evening_time slot
Nocar_Available_Total <- length(which(Issue_Evening_Time$Status=="No Cars Available"))
# No of  service requests with no cars available from airport during evening_hour
Nocar_Available_Airport <- length(which((Issue_Evening_Time$Pickup.point=="Airport") & (Issue_Evening_Time$Status == "No Cars Available")))
# No of service requests with no cars available from city during evening rush
Nocar_Available_City <- length(which((Issue_Evening_Time$Pickup.point=="City") & (Issue_Evening_Time$Status == "No Cars Available")))
# Percentage of no cars available status from city out of total no cars available during evening rush
Percent_Nocar_City <- (Nocar_Available_City/Nocar_Available_Total*100)
# Percentage of no cars available status from airport out of total no cars available during evening rush
Percent_Nocar_Airport <- (Nocar_Available_Airport/Nocar_Available_Total *100)
#No of service requests from airport to city during evening rush
Demand_Nocar_Request_Airport <- length(which(Issue_Evening_Time$Pickup.point=="Airport"))
#No of trips completed from airport to city during evening rush
Demand_Nocar_Request_Airport_completed <- length(which((Issue_Evening_Time$Pickup.point=="Airport") & (Issue_Evening_Time$Status=="Trip Completed")))
pie(c("% No Available Car  in City"=Percent_Nocar_City,"% No Available Car in Airport"=Percent_Nocar_Airport),col=c("blue", "red"), main = " % No Available Cab City/Airport ")
plot(Demand_Nocar_Request_Airport,Demand_Nocar_Request_Airport_completed , col = "red", lwd = 10, title("Demand Supply Correlation Evening_Time(1800-2259)"))

write.table(dataset, "C:/Users/Ranjita/Desktop/Data Analytics_IITM/dataset", sep="\t")


