##Find out variables that are significant in predicting the price of a car
#How well those variables describe the price of a car

# ----Setting Up required packages-----#
install.packages(c("dplyr","ggplot2","gridExtra","car","MASS","tidyr","corrplot"))
library(dplyr)
library(ggplot2)
library(gridExtra)
library(car)
library(MASS)
library(tidyr)
library(corrplot)
# ---- Load Data ----- #
# Set up the working directory
setwd("C:/Users/Ranjita/Desktop/Data Analytics_IITM/Section IV/4.2 Assignment Linear Regression")
car_data <- read.delim("CarPrice_Assignment.csv",sep=",",stringsAsFactors = FALSE)

# Keep a copy of the data frame
car<- car_data

## Data Understanding
# ---- What do we understand from the data ---- #
# Structure  of the car data #
str(car)

# RESULT:  Summary #
#'data.frame':	205 obs. of  23 variables:
# $ car_ID          : int  1 2 3 4 5 6 7 8 9 10 ...
# $ symboling       : num  5 5 3 4 4 4 3 3 3 2 ...
# $ car_manufacturer: chr  "alfa-romero" "alfa-romero" "alfa-romero" "audi" ...
# $ fueltype        : chr  "gas" "gas" "gas" "gas" ...
# $ aspiration      : chr  "std" "std" "std" "std" ...
# $ doornumber      : chr  "two" "two" "two" "four" ...
# $ carbody         : chr  "convertible" "convertible" "hatchback" "sedan" ...
# $ drivewheel      : chr  "rwd" "rwd" "rwd" "fwd" ...
# $ enginelocation  : chr  "front" "front" "front" "front" ...
# $ wheelbase       : num  88.6 88.6 94.5 99.8 99.4 ...
# $ carwidth        : num  64.1 64.1 65.5 66.2 66.4 66.3 71.4 71.4 71.4 67.9 ...
# $ carheight       : num  48.8 48.8 52.4 54.3 54.3 53.1 55.7 55.7 55.9 52 ...
# $ enginetype      : chr  "dohc" "dohc" "ohc" "ohc" ...
# $ cylindernumber  : chr  "four" "four" "six" "four" ...
# $ enginesize      : num  130 130 152 109 136 136 136 136 131 131 ...
# $ fuelsystem      : chr  "mpfi" "mpfi" "mpfi" "mpfi" ...
# $ boreratio       : num  3.47 3.47 2.68 3.19 3.19 3.19 3.19 3.19 3.13 3.13 ...
# $ stroke          : num  2.68 2.68 3.47 3.4 3.4 3.4 3.4 3.4 3.4 3.4 ...
# $ compressionratio: num  9 9 9 10 8 8.5 8.5 8.5 8.3 7 ...
# $ horsepower      : num  111 111 154 102 115 110 110 110 140 160 ...
# $ peakrpm         : int  5000 5000 5000 5500 5500 5500 5500 5500 5500 5500 ...
# $ citympg         : int  21 21 19 24 18 19 19 19 17 16 ...
# $ price           : num  13495 16500 16500 13950 17450 ...

#  There are Total of 205 observations of  23 variables
#  There are 3 int, 10 num and 10 chr based columns
# Car names are a composite string of Manufacturer's Name and the model
# We need to check the requirement of the  categorical variables, 
# Car_ID is of no use, as its just an index number. The same can be remeoved

## - Data Preparation 

###   Data cleaning
# Removing the car_ID column 
car<- car%>% dplyr::select(-car_ID)

# Checking for NAs 
sum(is.na(car))
# RESULT: 0 <- No NAs in the data, this is nice! 

# Checking for duplicate rows, if we have any car/model repeated
which(duplicated(car))
# RESULT: 0 <- no duplicates

#  Univariate analysis 
# Symboling variable
# Data dictionary says that it ranges from (-3)-to-(3), +3 being highest on risk and -3 being lowest. Its assigned insurance risk rating, A value of +3 indicates that the auto is risky, -3 that it is probably pretty safe.(Categorical)
#   Check the distribution 
Plot01 <- ggplot(car) + geom_bar(aes(x=symboling),fill="red",col="black") +
  scale_x_continuous(breaks = seq(-3,3,1)) +
  scale_y_continuous(breaks = seq(0,80,5)) +
  labs(title="Plot 01 - symboling distribution",x="Symboling",y="Count")
grid.arrange(Plot01)
#   Converting  the range to categorical and then change the levels 1-6. 0 as a lower value will be equal to -3 of risk  and 6 as a higher value which will be equal to +3 of risk
car $symboling <- as.factor(car$symboling)
levels(car$symboling) <- c(0:6)
car$symboling <- as.numeric(levels(car$symboling))[car$symboling]
str(car$symboling)

#  CarName
#      Lets divide carName into 2, as car manufacturer and another as car model
car<- car%>% 
  tidyr::separate(CarName,c("car_manufacturer","car_model"),sep=" ") %>% dplyr::select(-car_model)
#      Lets check the levels of these car manufacturers
levels(as.factor(car$car_manufacturer))
#      Some issues here
#      nissan & Nissan are same-> nissan
car[which(car $car_manufacturer == "Nissan"),'car_manufacturer'] <- "nissan"
#      porcshce & porsche are same-> porsche
car[which(car$car_manufacturer == "porcshce"),'car_manufacturer'] <- "porsche"
#      toyouta & toyota are same-> toyota
car[which(car$car_manufacturer == "toyouta"),'car_manufacturer'] <- "toyota"
#      vokswagen, volkswagen & vw are same-> volkswagen
car[which(car$car_manufacturer == "vokswagen"),'car_manufacturer'] <- "volkswagen"
car[which(car$car_manufacturer == "vw"),'car_manufacturer'] <- "volkswagen"
# maxda & mazda are same, we will go with mazda
car[which(car$car_manufacturer == "maxda"),'car_manufacturer'] <- "mazda"

#      check levels again
levels(as.factor(car $car_manufacturer))

#      Checking  the distribution for cars manufacturers
Plot02 <- ggplot(car) + geom_bar(aes(x=car_manufacturer),fill="brown",col="black") +
  scale_y_continuous(breaks = seq(0,40,5)) +
  labs(title="Plot 02 - Car Manufacturers",x="Car Manufacturer",y="Number of models")
grid.arrange(Plot02)

#  fuel-type
#  check the levels
levels(as.factor(car$fueltype))

#   Checking  the Fuel Type distribution
Plot03 <- ggplot(car) + geom_bar(aes(x=fueltype),fill="blue",col="black") +
  scale_y_continuous(breaks = seq(0,190,10)) +
  labs(title="Plot 03 - Fueltype distribution",x="Fuel Type",y="Number of models")
grid.arrange(Plot03)

#     Checking  Aspiration Distribution 
Plot04 <- ggplot(car) + geom_bar(aes(x=aspiration),fill="blue",col="black") +
  scale_y_continuous(breaks = seq(0,200,10)) +
  labs(title="Plot 04 - Aspiration distribution",x="Aspiration",y="Number of models")
grid.arrange(Plot04)

#      Checking doornumber
Plot05 <- ggplot(car) + geom_bar(aes(x=doornumber),fill="blue",col="black") +
  scale_y_continuous(breaks = seq(0,200,10)) +
  labs(title=" Plot 05 - Doors Distribution",x="Number of doors",y="Number of models")
grid.arrange(Plot05)

#      Checking carbody           
Plot06 <- ggplot(car) + geom_bar(aes(x=carbody),fill="blue",col="black") +
  scale_y_continuous(breaks = seq(0,200,10)) +
  labs(title="Plot 06 - Type of body\nDistribution",x="Type of body",y="Number of models")
grid.arrange(Plot06)

#      Checking drivewheel
Plot07 <- ggplot(car) + geom_bar(aes(x=drivewheel),fill="blue",col="black") +
  scale_y_continuous(breaks = seq(0,200,10)) +
  labs(title="Plot 07 - Front Wheel/Read Wheel Drive\nDistribution",x="Drive Wheel",y="Number of models")
grid.arrange(Plot07)

#      Checking enginelocation
Plot08 <- ggplot(car) + geom_bar(aes(x=enginelocation),fill="blue",col="black") +
  scale_y_continuous(breaks = seq(0,200,10)) +
  labs(title="Plot 08 - Engine Location\nDistribution",x="Engine Location",y="Number of models")
grid.arrange(Plot08)

#      Checking enginetype
Plot09 <- ggplot(car) + geom_bar(aes(x=enginetype),fill="blue",col="black") +
  scale_y_continuous(breaks = seq(0,200,10)) +
  labs(title="Plot 09 - Engine Type\nDistribution",x="Engine Type",y="Number of models")
grid.arrange(Plot09)

#      Checking cylindernumber
Plot10 <- ggplot(car) + geom_bar(aes(x=cylindernumber),fill="blue",col="black") +
  scale_y_continuous(breaks = seq(0,200,10)) +
  labs(title="Plot 10 - Number of Cylinders\nDistribution",x="Number of cylinders",y="Number of models")
grid.arrange(Plot10)

#      Checking fuelsystem
Plot11 <- ggplot(car) + geom_bar(aes(x=fuelsystem),fill="blue",col="black") +
  scale_y_continuous(breaks = seq(0,200,10)) +
  labs(title="Plot 11 - Fuel System\nDistribution",x="Fuel System",y="Number of models")
grid.arrange(Plot11)
# Checking all together
grid.arrange(Plot01, Plot02, Plot03,Plot04,Plot05,Plot06,Plot07,Plot08,Plot09,Plot10,Plot11,nrow=4,ncol=3)

# Examining  box plots of different numeric fields to understand the outliers
Plot11 <- ggplot(car) + geom_boxplot(aes(x=1,y=wheelbase),fill="light blue") +
  labs(title="Plot 11 - WheelBase distribution",x="",y="WheelBase")
Plot12 <- ggplot(car) + geom_boxplot(aes(x=1,y=carlength),fill="light blue") +
  labs(title="Plot 12 - CarLength distribution",x="",y="Car Length")
Plot13 <- ggplot(car) + geom_boxplot(aes(x=1,y=carwidth),fill="light blue") +
  labs(title="Plot 13 - Car Width distribution",x="",y="Car Width")
Plot14 <- ggplot(car) + geom_boxplot(aes(x=1,y=carheight),fill="light blue") +
  labs(title="Plot 14 - Car Height distribution",x="",y="Car Height")
Plot15 <- ggplot(car) + geom_boxplot(aes(x=1,y=curbweight),fill="light blue") +
  labs(title="Plot 15 - Curb Weight distribution",x="",y="Curb Weight")
Plot16 <- ggplot(car) + geom_boxplot(aes(x=1,y=enginesize),fill="light blue") +
  labs(title="Plot 16 - Engine Size distribution",x="",y="Engine Size")
Plot17 <- ggplot(car) + geom_boxplot(aes(x=1,y=boreratio),fill="light blue") +
  labs(title="Plot 17 - Bore Ratio distribution",x="",y="Bore Ratio")
Plot18 <- ggplot(car) + geom_boxplot(aes(x=1,y=stroke),fill="light blue") +
  labs(title="Plot 18 - Stroke distribution",x="",y="Stroke")
Plot19 <- ggplot(car) + geom_boxplot(aes(x=1,y=compressionratio),fill="light blue") +
  labs(title="Plot 19 - Compression Ratio\ndistribution",x="",y="Compression Ratio")
Plot20 <- ggplot(car) + geom_boxplot(aes(x=1,y=horsepower),fill="light blue") +
  labs(title="Plot 20 - Hoursepower\ndistribution",x="",y="horsepower")
Plot21 <- ggplot(car) + geom_boxplot(aes(x=1,y=peakrpm),fill="light blue") +
  labs(title="Plot 21 - Peak RPM\ndistribution",x="",y="Peak RPM")
Plot22 <- ggplot(car) + geom_boxplot(aes(x=1,y=citympg),fill="light blue") +
  labs(title="Plot 22 - City Mileage\ndistribution",x="",y="City Mileage")
Plot23 <- ggplot(car) + geom_boxplot(aes(x=1,y=highwaympg),fill="light blue") +
  labs(title="Plot 23 - Highway Mileage\ndistribution",x="",y="Highway Mileage")

grid.arrange(Plot11,Plot12,Plot13,Plot14,Plot15,Plot16)

grid.arrange(Plot17,Plot18,Plot19,Plot20,Plot21,Plot22,Plot23)

# Dealing with the  Outlier
# engine size 
quantile(car$enginesize,seq(0,1,0.01))
#      The distrbution jumps after 96 percentile, and goes up to 236.
#      For all values above 209, we assign it with the mean of the distribution
car$enginesize[(which(car$enginesize > 209))] <- mean(car$enginesize)

# Horsepower has some outliers
quantile(car$horsepower,seq(0,1,0.01))
#      The distribution jumps after 97 percentile, and goes up from 184 to 288.
#      For values above 184, , we assign it with the mean of the distribution
car$horsepower[(which(car$horsepower > 184))] <- mean(car$horsepower)

# compression ratio has outliers
quantile(car$compressionratio,seq(0,1,0.01))
#      The distribution jumps after 90 percentile, and goes up from 10.94 to 23.0
#      For values above 10.9400, , we assign it with the mean of the distribution
car$compressionratio[(which(car$compressionratio > 10.94))] <- mean(car$compressionratio)

# It emerges that there are 3 kinds of engines:(a)SOHC - Single overhead camshaft (it includes ohcf, ohcv), (b)DOHC - Dual overhead camshaft(it includes dohcv), (c) l type -  nissan makes them, (d) Rotary - Rotary engines
# We assign accordingly
car$enginetype[(which(car$enginetype  %in% c("ohcf","ohcv")))] <- "ohc"
car$enginetype[(which(car$enginetype  %in% c("dohcv")))] <- "dohc"

# Bivariate analysis
Plot24 <- ggplot(car) + geom_boxplot(aes(x=fueltype,y=curbweight,fill=fueltype)) +
  labs(title="Plot 24 - FuelType Vs CurbWeight",x="FuelType",y="Curb Weight",fill="Fuel Type")

Plot25 <- ggplot(car) + geom_boxplot(aes(x=1,y=compressionratio),fill="light blue") + 
  facet_wrap(~fueltype,scales = "free") + labs(title=" Plot 25 - FuelType Vs Compression Ratio",x="Fuel Type",y="COmpression Ratio")

Plot26 <- ggplot(car) + geom_point(aes(x=enginesize,y=horsepower),size=3,col="blue") +
  scale_y_continuous(breaks = seq(0,250,20)) +
  scale_x_continuous(breaks = seq(0,300,10)) +
  labs(title="Plot 26 - Engine Size Vs Horsepower",x="Engine Size",y="Horsepower")

grid.arrange(Plot24,Plot25,Plot26,nrow=2,ncol=2)

# Checking out the corelation between multiple variables, through corelation matrix for numerical variables
car_corelation_matrix <- cor(car%>% dplyr::select(peakrpm,wheelbase,carlength,carwidth,carheight,curbweight,enginesize,boreratio,stroke,compressionratio,horsepower,citympg,highwaympg,price,symboling))
corrplot(car_corelation_matrix, method = "number", title = "Correlation Matrix", type = "lower", order = "FPC", number.cex = 0.8,  tl.cex = 0.7,bg="yellow")

# citympg and highwaympg are corelation upto 97%. 
# We will keep only citympg and remove highwaympg
car<- car%>% dplyr::select(-highwaympg)

#curbweight is corelated to a lot of variables, We remove it
car<- car%>% dplyr::select(-curbweight)

# Car length and wheelbase are correlation. Take wheelbase
car<- car%>% dplyr::select(-carlength)

# We follow stepAIC, for step wise elimination of maximum variables
# Convert all columns to numeric for mathematical analysis.

# Retaining the copy of Car data frame
car_copy <- car
# car_manufacturer - use model.matrix
dummy_car_manufacturer <- model.matrix(~car_manufacturer,data=car_copy)
car_copy <- cbind(car_copy %>% dplyr::select(-car_manufacturer),dummy_car_manufacturer[,-1])
# fueltype - change to gas = 0 and diesel = 1
car_copy$fueltype <- ifelse(car_copy$fueltype == "gas",0,1)
# aspiration - simply change std = 0 and turbo = 1
car_copy$aspiration <- ifelse(car_copy$aspiration == "std",0,1)
# doornumber - simply change two door = 0 and 4 door = 1      
car_copy$doornumber <- ifelse(car_copy$doornumber == "two",0,1)
# car body - use model.matrix          
dummy_car_body <- model.matrix(~carbody,data=car_copy)
car_copy <- cbind(car_copy%>% dplyr::select(-carbody),dummy_car_body[,-1])
# drivewheel - use model.matrix         
dummy_drivewheel <- model.matrix(~drivewheel,data=car_copy)
car_copy <- cbind(car_copy %>% dplyr::select(-drivewheel),dummy_drivewheel[,-1])
# enginelocation - simply change rear = 0 and front = 1     
car_copy$enginelocation <- ifelse(car_copy$enginelocation == "rear",0,1)
# enginetype - use model.matrix
dummy_enginetype <- model.matrix(~enginetype,data=car_copy)
car_copy <- cbind(car_copy %>% dplyr::select(-enginetype),dummy_enginetype[,-1])
# cylindernumber - use model.matrix
dummy_cylindernumber <- model.matrix(~cylindernumber,data=car_copy)
car_copy <- cbind(car_copy %>% dplyr::select(-cylindernumber),dummy_cylindernumber[,-1])
# fuelsystem - use model.matrix
dummy_fuelsystem <- model.matrix(~fuelsystem,data=car_copy)
car_copy <- cbind(car_copy%>% dplyr::select(-fuelsystem),dummy_fuelsystem[,-1])



## Data Modeling 
# We would remove the high-corelation variables first using stepAIC
cols_tobe_removed_for_analysis <- c("")

# Creating a global function 
car_lm <- function(cols_to_remove=c(""),executeSummaryForModel=0,executeVIF=0,executeStepAIC=0) {
  # Assuming that the cols_to_remove does not give "price" as a column to be removed
  if ( length(cols_tobe_removed_for_analysis) == 0 ) {
    cols_tobe_removed_for_analysis <- cols_to_remove
  } else if ( ! length(cols_to_remove) == 0) {
    cols_tobe_removed_for_analysis <- c(cols_tobe_removed_for_analysis,cols_to_remove)
  } else {
    # Do nothing #
  }
  colnums_to_consider <- which(! colnames(car_copy) %in% cols_tobe_removed_for_analysis)
  analysis.df <- car_copy %>% dplyr::select(colnums_to_consider)
  model <- lm(price~.,data=analysis.df)
  if (executeSummaryForModel == 1) {
    print(summary(model))
  }
  if (executeVIF == 1) {
    print(vif(model))
  }
  if (executeStepAIC == 1) {
    step <- stepAIC(model,direction="both")
    print(step)
  }
  return(model)
}


# Model - 1, all columns

model_1 <- car_lm(cols_to_remove=c(""),executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
# Lets execute stepAIC
model_1 <- car_lm(cols_to_remove=c(""),executeSummaryForModel=0,executeVIF=0,executeStepAIC=1)

# Now we have 38 significant variables from earlier  60 variables
# "fueltype", "enginelocation", "wheelbase", "carwidth", "compressionratio", "horsepower",
# "peakrpm", "citympg", "car_manufacturerbmw", "car_manufacturerbuick", 
# "car_manufacturerchevrolet", "car_manufacturerdodge", "car_manufacturerhonda", 
# "car_manufacturerisuzu", "car_manufacturerjaguar", "car_manufacturermazda", 
# "car_manufacturermercury", "car_manufacturermitsubishi", "car_manufacturernissan", 
# "car_manufacturerpeugeot", "car_manufacturerplymouth", "car_manufacturerporsche", 
# "car_manufacturerrenault", "car_manufacturersubaru", "car_manufacturertoyota", 
# "car_manufacturervolkswagen", "car_manufacturervolvo", "carbodyhardtop", "carbodyhatchback", 
# "carbodysedan", "carbodywagon", "drivewheelfwd", "enginetypeohc", "cylindernumberfive", 
# "cylindernumberfour", "cylindernumbersix", "fuelsystem2bbl"
#
# Modify cols_tobe_removed_for_analysis variable so that we can analyze things better

aic_preferred_collist <- c("fueltype", "enginelocation", "wheelbase", "carwidth", "compressionratio", "horsepower",
                           "peakrpm", "citympg", "car_manufacturerbmw", "car_manufacturerbuick", 
                           "car_manufacturerchevrolet", "car_manufacturerdodge", "car_manufacturerhonda", 
                           "car_manufacturerisuzu", "car_manufacturerjaguar", "car_manufacturermazda", 
                           "car_manufacturermercury", "car_manufacturermitsubishi", "car_manufacturernissan", 
                           "car_manufacturerpeugeot", "car_manufacturerplymouth", "car_manufacturerporsche", 
                           "car_manufacturerrenault", "car_manufacturersubaru", "car_manufacturertoyota", 
                           "car_manufacturervolkswagen", "car_manufacturervolvo", "carbodyhardtop", "carbodyhatchback", 
                           "carbodysedan", "carbodywagon", "drivewheelfwd", "enginetypeohc", "cylindernumberfive", 
                           "cylindernumberfour", "cylindernumbersix", "fuelsystem2bbl","price")
cols_tobe_removed_for_analysis <- colnames(car_copy)[which(! colnames(car_copy) %in% aic_preferred_collist)]

# Model - 2, with column suggested by AIC
# The argument cols_to_remove will get added up into cols_tobe_removed_for_analysis.Therefore, when we call cal_lm custom function, we will send extra columns that need to be removed based on previous model analysis.
# For example : in model_3, we sent "citympg", this gets added to the list of columns which AIC rejected
#               (cols_tobe_removed_for_analysis). Further on, "citympg" removal for model_3 is based on
#               analysis done on model_2.

model_2 <- car_lm(cols_to_remove=c(""),executeSummaryForModel=1,executeVIF=1,executeStepAIC=0)
model_3 <- car_lm(cols_to_remove=c("citympg"),executeSummaryForModel=1,executeVIF=1,executeStepAIC=0)
model_4 <- car_lm(cols_to_remove=c("citympg","peakrpm"),executeSummaryForModel=1,executeVIF=1,executeStepAIC=0)
model_5 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc"),executeSummaryForModel=1,executeVIF=1,executeStepAIC=0)
model_6 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd"),executeSummaryForModel=1,executeVIF=1,executeStepAIC=0)
model_7 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl"),
                  executeSummaryForModel=1,executeVIF=1,executeStepAIC=0)
model_8 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                   "wheelbase"), 
                  executeSummaryForModel=1,executeVIF=1,executeStepAIC=0)
model_9 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                   "wheelbase","cylindernumbersix"), 
                  executeSummaryForModel=1,executeVIF=1,executeStepAIC=0)
model_10 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio"), 
                   executeSummaryForModel=1,executeVIF=1,executeStepAIC=0)

# Let us deal with  p-value tests from here

model_11 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_12 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_13 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_14 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_15 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                    "car_manufacturermercury"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_16 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                    "car_manufacturermercury","car_manufacturermazda"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_17 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                    "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_18 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                    "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                    "car_manufacturerplymouth"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_19 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                    "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                    "car_manufacturerplymouth","car_manufacturerdodge"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_20 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                    "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                    "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_21 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                    "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                    "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                    "car_manufacturervolkswagen"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_22 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                    "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                    "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                    "car_manufacturervolkswagen","car_manufacturersubaru"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_23 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                    "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                    "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                    "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_24 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                    "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                    "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                    "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota",
                                    "car_manufacturernissan"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_25 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                    "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                    "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                    "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota",
                                    "car_manufacturernissan","fueltype"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_26 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                    "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                    "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                    "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota",
                                    "car_manufacturernissan","fueltype","car_manufacturermitsubishi"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_27 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                    "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                    "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                    "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota",
                                    "car_manufacturernissan","fueltype","car_manufacturermitsubishi","cylindernumberfive"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_28 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                    "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                    "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                    "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota",
                                    "car_manufacturernissan","fueltype","car_manufacturermitsubishi","cylindernumberfive",
                                    "cylindernumberfour"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_29 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                    "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                    "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                    "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota",
                                    "car_manufacturernissan","fueltype","car_manufacturermitsubishi","cylindernumberfive",
                                    "cylindernumberfour","car_manufacturerporsche"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_30 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                    "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                    "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                    "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota",
                                    "car_manufacturernissan","fueltype","car_manufacturermitsubishi","cylindernumberfive",
                                    "cylindernumberfour","car_manufacturerporsche","carbodyhardtop"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_31 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                    "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                    "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                    "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota",
                                    "car_manufacturernissan","fueltype","car_manufacturermitsubishi","cylindernumberfive",
                                    "cylindernumberfour","car_manufacturerporsche","carbodyhardtop","carbodysedan"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_32 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                    "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                    "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                    "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota",
                                    "car_manufacturernissan","fueltype","car_manufacturermitsubishi","cylindernumberfive",
                                    "cylindernumberfour","car_manufacturerporsche","carbodyhardtop","carbodysedan",
                                    "carbodywagon"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)
model_33 <- car_lm(cols_to_remove=c("citympg","peakrpm","enginetypeohc","drivewheelfwd","fuelsystem2bbl",
                                    "wheelbase","cylindernumbersix","compressionratio","car_manufacturervolvo",
                                    "car_manufacturerisuzu","car_manufacturerchevrolet","car_manufacturerpeugeot",
                                    "car_manufacturermercury","car_manufacturermazda","car_manufacturerhonda",
                                    "car_manufacturerplymouth","car_manufacturerdodge","car_manufacturerrenault",
                                    "car_manufacturervolkswagen","car_manufacturersubaru","car_manufacturertoyota",
                                    "car_manufacturernissan","fueltype","car_manufacturermitsubishi","cylindernumberfive",
                                    "cylindernumberfour","car_manufacturerporsche","carbodyhardtop","carbodysedan",
                                    "carbodywagon","carbodyhatchback"), 
                   executeSummaryForModel=1,executeVIF=0,executeStepAIC=0)



# Final model = model_33, which gives the following
# Number of features chosen : 6 + 1(intercept)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)            -74488.912   7423.401 -10.034  < 2e-16 ***
#  enginelocation         -23829.927   1432.702 -16.633  < 2e-16 ***
#  carwidth                 1574.680    118.543  13.284  < 2e-16 ***
#  horsepower                 63.993      7.233   8.847 5.01e-16 ***
#  car_manufacturerbmw     10873.772    924.490  11.762  < 2e-16 ***
#  car_manufacturerbuick   10706.233   1028.842  10.406  < 2e-16 ***
#  car_manufacturerjaguar  13066.854   1490.450   8.767 8.36e-16 ***
# List of final features    : enginelocation,carwidth,horsepower,car_manufacturerbmw,car_manufacturerbuick
#                             car_manufacturerjaguar
# R-square                  : 0.9085
# Adjusted R-square         : 0.9057
# The R-square and adjuster R-square are pretty good. Lets evaluate our model.

#  Model Evaluation ---- #
#  check the noise in model_33

car_predict.df <- car_copy
car_predict.df$price_predicted <- predict(model_33,(car_predict.df %>% dplyr::select(-price)))
cor(car_predict.df$price,car_predict.df$price_predicted)
r_square_prediction <- (cor(car_predict.df$price,car_predict.df$price_predicted))^2
# 0.9531419

car_predict.df$error <- (car_predict.df$price - car_predict.df$price_predicted)

# Noise plot #
Plot27 <- ggplot(car_predict.df) + geom_line(aes(x=c(1:nrow(car_predict.df)),y=0),col="red",size=1) +
  geom_point(aes(x=c(1:nrow(car_predict.df)),y=error)) +
  scale_x_continuous(breaks = seq(0,250,5)) +
  scale_y_continuous(breaks = seq(-6000,12000,1000)) +
  labs(title="G27 - Noise in prediction",x="Number of observations",y="Error")
grid.arrange(Plot27)

# The noise in the prediction is indeed random and does not follow a pattern. Hence our analysis
# seems to be working properly.

# Lets plot the prediction, and see how accurately we are able to predict#
Plot28 <- ggplot(car_predict.df) + geom_line(aes(x=c(1:nrow(car_predict.df)),y=price,col="blue"),size=1) +
  geom_line(aes(x=c(1:nrow(car_predict.df)),y=price_predicted,col="red"),size=1) +scale_color_discrete(name = "Price", labels = c("Actual Price","Predicted Price")) +
  scale_x_continuous(breaks = seq(0,210,5)) +  scale_y_continuous(breaks = seq(0,50000,5000)) +
  labs(title="G28 - Actual Vs Predicted Price",x="Number of observations",y="Price")
grid.arrange(Plot28)

# The prediction graph, seems to follow a good pattern. It is missing some very high peaks but other wise it is able to trace the patterns to a large extent.

# -- Perform bootstrapping 
num_iter <- 1000
ratio <- 0.8     
cols_to_consider <- c("enginelocation","carwidth","horsepower","car_manufacturerbmw",
                      "car_manufacturerbuick","car_manufacturerjaguar","price")
bootstrap_result.df <- data.frame(iteration=vector(mode="numeric"),train_r_square=vector(mode="numeric"),
                                  train_adjusted_r_square=vector("numeric"),predict_r_square=vector("numeric"))
set.seed(100)
pb <- txtProgressBar(min=1,max=num_iter,style = 3)
for (itr in c(1:num_iter)) {
  setTxtProgressBar(pb,itr)
  random_train_rows_idx <- sample(c(1:nrow(car_copy)),ratio*nrow(car_copy),replace=FALSE)
  test_rows_idx <- c(1:nrow(car_copy))[which(! c(1:nrow(car_copy)) %in% random_train_rows_idx)]
  train.df <- car_copy[random_train_rows_idx,cols_to_consider]
  test.df <- car_copy[test_rows_idx,cols_to_consider]
  temp_model <- lm(price~.,data=train.df)
  if (! nrow(summary(temp_model)[[4]]) == 7) {
    #cat("\nDid a continue here : ",itr,"\n")
    next
  }
  print(nrow(summary(temp_model)[[4]]))
  
  test.df$price_predicted <- predict(temp_model,(test.df %>% dplyr::select(-price)))
  prediction_r_square <- (cor(test.df$price,test.df$price_predicted))^2
  bootstrap_result.df[itr,] <- c(itr,summary(temp_model)[[8]],summary(temp_model)[[9]],prediction_r_square)
}

# Mean value of the predict_r_square
mean(bootstrap_result.df$predict_r_square,na.rm=TRUE)
# 0.8933339:  Ok Value

bootstrap_nona_rows <- nrow(bootstrap_result.df %>% filter(!is.na(iteration)))
Plot29 <- ggplot(bootstrap_result.df %>% filter(!is.na(iteration))) + 
  geom_line(aes(x=c(1:bootstrap_nona_rows),y=train_r_square,col="blue")) +
  geom_line(aes(x=c(1:bootstrap_nona_rows),y=train_adjusted_r_square,col="green")) +
  geom_line(aes(x=c(1:bootstrap_nona_rows),y=predict_r_square,col="red")) +
  geom_line(aes(x=c(1:bootstrap_nona_rows),y=min(predict_r_square)),col="black",size=1) +
  geom_line(aes(x=c(1:bootstrap_nona_rows),y=max(predict_r_square)),col="black",size=1) +
  scale_color_discrete(name = "Graphs",labels = c("Train R-Square","Train Adjusted R-Square","Predict R-Square")) +
  scale_x_continuous(breaks = seq(0,500,20)) +
  scale_y_continuous(breaks = seq(0.7,1.0,0.01)) +
  labs(title="G29 - Bootstrap Result plot",x="Number of iterations",y="r-squares")
grid.arrange(Plot29)

#Results         

# The  predictors are  enginelocation,carwidth,horsepower,car_manufacturerbmw,car_manufacturerbuick, car_manufacturerjaguar
# The  model is able to predict the car prices to a very strong level
# based on the above predictors.

# Remarks
# The US automobile marketdominated by 3 brands, BMW, BUICK & JAGUAR
# Ones pricing is considered car-width, horsepower, and engine location are important factors






