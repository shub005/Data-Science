library(tidyr);
library(dplyr);

##Checkpoint 1 : Part 1-Data Cleaning
# Load the companies and rounds data into two data frames and name them companies and rounds2 respectively

companies <- read.delim("companies.txt", header = TRUE,stringsAsFactors = F)
rounds2 <- read.csv("rounds2.csv", header = TRUE,sep=",")

#Lowering the cases of both the data sets - merge them with commom primary key

companies$permalink <- tolower(companies$permalink)
rounds2$company_permalink <- tolower(rounds2$company_permalink)

# Finding Unique companies present in rounds2 dataset
count(distinct(rounds2,company_permalink))

# Finding Unique companies present in companies dataset
count(distinct(companies,permalink))

#Naming merged frame as master_frame
colnames(companies)[colnames(companies)=="permalink"] <- "company_permalink"

master_frame  <- merge(rounds2,companies,by="company_permalink")

#Removing all the NA values from the data frame

master_frame <- na.omit(master_frame)

##Checkpoint 2: Funding Type Analysis
# Spark Funds wants to choose one of these four investment types for each potential 
#investment they will make.
#Calculate the average investment amount for each of the four funding types (venture, angel,
#seed,and private equity)
#Based on the average investment amount calculated above, which investment type do you think is 
#the most suitable for Spark Funds?

average_funding_by_funding_type <- master_frame %>%
  group_by(funding_round_type)%>%
  summarise(avg_funding=mean(raised_amount_usd,na.rm = T))

##Checkpoint 3: Country Analysis
#Spark Funds wants to see the top nine countries which have received the highest total 
#funding (across ALL sectors for the chosen investment type)

#Slicing down the best_funding type as per the constraints of Sparks fund

best_fund_type<-average_funding_by_funding_type %>%
  filter(avg_funding>=5000000, avg_funding<=15000000)%>%
  arrange(desc(avg_funding))%>%
  head(.,n=1)


#Slicing down the data for the best_fund_type from the master data frame
master_frame_best_fund <- subset(master_frame,master_frame$funding_round_type==best_fund_type$funding_round_type)


# Obtaining the funding country wise, only the English speaking countries
# Obtainiong the top9 countries to invest on as per the Sparks fund contraints 

funding_sum_country_wise <- master_frame_best_fund %>%
  group_by(country_code) %>%
  summarise(sum(raised_amount_usd))

colnames(funding_sum_country_wise) <- c("country_code", "Raised")

funding_sum_country_wise <- arrange(funding_sum_country_wise,desc(Raised))

# For the chosen investment type, make a data frame named top9 with the top nine 
# countries (based on the total investment amount each country has received)

top9 <- head(funding_sum_country_wise, n = 9)


##Checkpoint 4: Sector Analysis
# Extract the primary sector of each category list from the category_list column

#	Use the mapping file 'mapping.csv' to map each primary sector to one
#of the eight main sectors

#Reading the mapping.csv  data set
mappings <- read.csv("mapping.csv",header = TRUE,sep = ",")


#Converting the wide data format to long data format.
mappings <- gather(mappings,sector,isPresent,Automotive...Sports:Social..Finance..Analytics..Advertising)

companies <- read.delim("companies.txt", header = TRUE,stringsAsFactors = F)

#Extracting only the positive scenarios and removing the unwanted rows from the long data
mappings <- mappings[!(mappings$isPresent==0),]
mappings$isPresent <- NULL


colnames(mappings) <- c("Primary_Sector","Main_Sector")

#Extracting the primary sector from the category_list column
Primary_Sector <- sapply(strsplit(master_frame$category_list,"\\|"), function(x) x[1])
master_frame_sectored <- cbind(master_frame,Primary_Sector)

master_frame_sectored$Primary_Sector <- tolower(master_frame_sectored$Primary_Sector)
mappings$Primary_Sector <- tolower(mappings$Primary_Sector)

# Merging the mapping data with the master data to obtain the Main Sector of the corresponding
# Primary sector.
master_frame_sectored <- merge(master_frame_sectored,mappings,by="Primary_Sector", all.x=T)
master_frame_sectored <- na.omit(master_frame_sectored)

##Checkpoint 5: Sector Analysis 2

master_frame_Venture <- subset(master_frame_sectored,master_frame_sectored$funding_round_type==
                                 best_fund_type$funding_round_type)

#Creating the 3 data frames for each of the 3 best english speaking countries to invest in

USA_data <- subset(master_frame_Venture,master_frame_Venture$country_code=="USA" &
                     master_frame_Venture$raised_amount_usd>=5000000
                   & master_frame_Venture$raised_amount_usd<=15000000)

GBR_data <- subset(master_frame_Venture,master_frame_Venture$country_code=="GBR" &
                     master_frame_Venture$raised_amount_usd>=5000000
                   & master_frame_Venture$raised_amount_usd<=15000000)

IND_data <- subset(master_frame_Venture,master_frame_Venture$country_code=="IND" &
                     master_frame_Venture$raised_amount_usd>=5000000
                   & master_frame_Venture$raised_amount_usd<=15000000)



# Obtaining the Sector wise investment count and total investment for each of the three countries
USA_Sector_Investment <- USA_data %>% 
  group_by(Main_Sector) %>%
  summarise(Investment_Count=n(),Investment_Total=sum(raised_amount_usd))
USA_data <- merge(USA_data,USA_Sector_Investment,by="Main_Sector")


GBR_Sector_Investment <- GBR_data %>%
  group_by(Main_Sector) %>%
  summarise(Investment_Count=n(),Investment_Total=sum(raised_amount_usd))
GBR_data <- merge(GBR_data,GBR_Sector_Investment,by="Main_Sector")

IND_Sector_Investment <- IND_data %>%
  group_by(Main_Sector) %>%
  summarise(Investment_Count=n(),Investment_Total=sum(raised_amount_usd))
IND_data <- merge(IND_data,IND_Sector_Investment,by="Main_Sector")


#Total investment made in each of the 3 country
USA_Total_Investment <- sum(USA_data$raised_amount_usd)
IND_Total_Investment <- sum(IND_data$raised_amount_usd)
GBR_Total_Investment <- sum(GBR_data$raised_amount_usd)


#Extracting the top 3 sector wise data for each of the country as per the invesment counts

USA_TOP3_Sectors <- head(arrange(USA_Sector_Investment,desc(Investment_Count)),n=3)
GBR_TOP3_Sectors <- head(arrange(GBR_Sector_Investment,desc(Investment_Count)),n=3)
IND_TOP3_Sectors <- head(arrange(IND_Sector_Investment,desc(Investment_Count)),n=3)


#Top company in USA 
USA_Top_1_Company_wise <- subset(USA_data,USA_data$Main_Sector==USA_TOP3_Sectors$Main_Sector[1]) %>%
  group_by(company_permalink) %>%
  summarise(total=sum(raised_amount_usd))%>%
  arrange(desc(total))
#Top company in GBR         
GBR_Top_1_Company_wise <- subset(GBR_data,GBR_data$Main_Sector==GBR_TOP3_Sectors$Main_Sector[1]) %>%
  group_by(company_permalink) %>%
  summarise(total=sum(raised_amount_usd))%>%
  arrange(desc(total))
#Top company in IND
IND_Top_1_Company_wise <- subset(IND_data,IND_data$Main_Sector==IND_TOP3_Sectors$Main_Sector[1]) %>%
  group_by(company_permalink) %>%
  summarise(total=sum(raised_amount_usd))%>%
  arrange(desc(total))

#Second best in USA
USA_Top_2_Company_wise <- subset(USA_data,USA_data$Main_Sector==USA_TOP3_Sectors$Main_Sector[2]) %>%
  group_by(company_permalink) %>%
  summarise(total=sum(raised_amount_usd))%>%
  arrange(desc(total))

#Second best in GBR
GBR_Top_2_Company_wise <- subset(GBR_data,GBR_data$Main_Sector==GBR_TOP3_Sectors$Main_Sector[2]) %>%
  group_by(company_permalink) %>%
  summarise(total=sum(raised_amount_usd))%>%
  arrange(desc(total))

#Second best in IND
IND_Top_2_Company_wise <- subset(IND_data,IND_data$Main_Sector==IND_TOP3_Sectors$Main_Sector[2]) %>%
  group_by(company_permalink) %>%
  summarise(total=sum(raised_amount_usd))%>%
  arrange(desc(total))

