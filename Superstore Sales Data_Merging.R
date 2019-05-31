setwd("C:\\Users\\Ranjita\\Desktop\\Data Analytics_IITM\\How Data Exists Within Enterprises\\Superstore Sales Data")
# Loaded as data frame
market_fact<-read.csv("market_fact.csv",stringsAsFactors = FALSE )
cust_dimen<-read.csv("cust_dimen.csv",stringsAsFactors = FALSE )
orders_dimen<-read.csv("orders_dimen.csv",stringsAsFactors = FALSE )
prod_dimen<-read.csv("prod_dimen.csv",stringsAsFactors = FALSE )
shipping_dimen<-read.csv("shipping_dimen.csv",stringsAsFactors = FALSE )
merge1<-merge(market_fact, cust_dimen,by="Cust_id")
# by default merge is always inner merge
merge2<-merge(merge1,orders_dimen,by="Ord_id")
merge3<-merge(merge2,prod_dimen,by="Prod_id")
merge4<-merge(merge3,shipping_dimen,by="Ship_id")
# Enhancing the fact table with additional metadata usful for analysis
View(merge4)
slice1<-subset(merge4,merge4$Customer_Name=="FRANK PRICE")
slice2<-subset(merge4,merge4$Ship_Mode=="DELIVERY TRUCK")
dice1<- subset(merge4,merge4$Region=="ATLANTIC" & merge4$Order_Priority=="MEDIUM")
rollup1<-aggregate(merge4$Sales,by=list(merge4$Province),FUN=sum)
rollup2<-aggregate(merge4$Sales,by=list(merge4$Ship_Mode),FUN=sum)
rollup3<-aggregate(merge4$Profit,by=list(merge4$Product_Sub_Category),FUN=sum)

Rollup4<-aggregate(merge4$Profit,by=list(merge4$Customer_Segment),FUN=sum)
slice1=subset(merge4,merge4$Profit==max(merge4$Profit));
write.table(merge4,"merge.txt",row.names=FALSE, sep="\t")
