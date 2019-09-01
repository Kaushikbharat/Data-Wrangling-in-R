#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# Banking Case Study or Jim's Evaluation for Fraud and Bankruptcy
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$



#Setting the directory
setwd("D://Bharat/Case Studies/Banking")

# Reading a CSV file Spend
Spend <- read.csv("spend.csv",stringsAsFactors = FALSE)

# Reading a CSV file Repayment
Repayment <- read.csv("Repayment.csv",stringsAsFactors = FALSE)

# Reading a CSV file Customer_Acqusition
Customer_Acqusition <- read.csv("Customer Acqusition.csv",stringsAsFactors = FALSE)

#Choosing only starting 1500 Rows as Rest of them are NA's
Repayment <- Repayment[1:1500,]

require(dplyr)

#Using dplyr::group_by() function for taking out a subset of Customer and sum(Amount)
Repayment_1 <- Repayment %>% group_by(Customer) %>% dplyr::summarise(SumofRepaymentAmount=sum(Amount))

#Merging the file Customer Acqusition with Repayment_1
C_A_R <- merge(x=Customer_Acqusition,y=Repayment_1,by.x="Customer",by.y="Customer",all=TRUE)

#Arranging the Merged file
C_A_R <- C_A_R %>% arrange(No)

#Finding the mean of the Ages from Merged files
mean(C_A_R$Age)




#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

                #Qns 1  

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


#Note - In the Below file C_A_R is a merged file of Customer Acqusition and Repayment
#Note - In the Below file C_A_R_S is a merged file of C_A_R and Spend

#Qns1.1  In Case Age is less than 18 replace it with mean of age values
C_A_R$Age<- ifelse(C_A_R$Age<18,mean(C_A_R$Age),C_A_R$Age)


#Qns1.2 In Case the if spend Amount is more than limit replace it with 50% of the limit


#Using dplyr::group_by() function for taking out a subset of Customer and sum(Amount)
Spend_1 <- Spend %>% group_by(Customer) %>% dplyr::summarise(SumofSpendAmount=sum(Amount))

#Merging Merge_1 with Spend_1
C_A_R_S <- merge(x=C_A_R,y=Spend_1,by.x="Customer",by.y="Customer",all=TRUE)

#Arranging the Merged file
C_A_R_S <- C_A_R_S %>% arrange(No)

#In Case the if spend Amount is more than limit replace it with 50% of the limit
C_A_R_S$Replace50limit <- ifelse(C_A_R_S$SumofSpendAmount>C_A_R_S$Limit,C_A_R_S$Limit/2,C_A_R_S$SumofSpendAmount)

#Qns1.3 In Case the if Repayment Amount is more than limit replace it with of the limit
C_A_R_S$ReplaceLimit <- ifelse(C_A_R_S$SumofRepaymentAmount > C_A_R_S$Limit,C_A_R_S$Limit,C_A_R_S$SumofRepaymentAmount)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

              #Qns 2  

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$





#Qns 2 From the Above DataSets Create the following Reports

#Qns2.a How many distinct customer exist
sum(!duplicated(C_A_R_S$Customer))


#Qns2.b How many distinct categories of purchases by these customer
Unique_Categories <- unique(Spend$Type)
length(Unique_Categories)

# OR

sum(!duplicated(Spend$Type))

#Qns2.c What is average monthly spends by customer
class(Spend$Month)      #checking the data type of Month in Spend
Spend$Month <- as.Date(Spend$Month,format="%d-%b-%y")     #Changing the format to date format
#install.packages("lubridate")                         
require(lubridate)
Spend$Month_Yr <- format(as.Date(Spend$Month),"%Y-%b")
Spend$Month_Name <- lubridate::month(Spend$Month,label=T)    #Extracting Month_Name from Month
Spend$Year_Name <- lubridate::year(Spend$Month)              #Extracting Year_Name from Month

# Calculating average monthly spends by customer
Spend_Monthly_Average <- Spend %>% dplyr::group_by(Customer,Month_Yr) %>% dplyr::summarise(SpendMonthly_Average=mean(Amount))


#Qns2.d What is average monthly Repayment by Customers      
class(Repayment$Month)
Repayment$Month <- as.Date(Repayment$Month,format="%d-%b-%y")    #Changing the format to date format
Repayment$Month_Name <- lubridate::month(Repayment$Month,label=T)         #Extracting Month_Name from Month
Repayment$Year_Name <- lubridate::year(Repayment$Month)                   #Extracting Year_Name from Month
Repayment$Month_Yr <- paste(Repayment$Year_Name,Repayment$Month_Name, sep = "-")      ##Combining the Columns Month_Name and Year_Name


# Calculating average monthly Repayment by customer
Repayment_Monthly_Average <- Repayment %>% dplyr::group_by(Customer,Month_Yr) %>% dplyr::summarise(RepaymentMonthly_Average=mean(Amount))


#Qns 2.e If the Monthly Rate of Interest is 2.9%, What is the Profit for the Bank for each month

# Calculating Profit
Spend$Profit <- Spend$
# Calculaing the Monthly Profit by each Customer
Spend_4 <- C_A_R_S %>% dplyr::group_by(Customer,Month_Yr) %>% dplyr::summarise(Total_Profit=sum(Profit))


#Qns 2.f What are the top categories in which the customers ae spending more money

# Extracting Top Categories as the customer spended money
Top_Categories <- Spend %>% dplyr::group_by(Type) %>% dplyr::summarise(SumofAmount=sum(Amount))

# Arranging them in Descending Order
Top_Categories <- dplyr::arrange(Top_Categories,desc(SumofAmount))

# Extracting Top 6 Categories in which the customers ae spending more money
head(Top_Categories)


#Qns 2.g Which city is making maximum Profit.

Max_Profit_City <- data.frame(C_A_R_S$City,C_A_R_S$Profit)
Max_Profit_City <- dplyr::arrange(Max_Profit_City,desc(C_A_R_S.Profit))
head(Max_Profit_City,n=1)
# change


# Qns 2.h Which Age group is spending more money

C_A_R_S$Age     # Exracting all the Ages

max(C_A_R_S$Age)  # Extractly Maximum Age 
min(C_A_R_S$Age)  # Extractly Minimum Age

# Bin the Age wise category
C_A_R_S$Age_Category <- ifelse(C_A_R_S$Age<=30,"Young",ifelse(C_A_R_S$Age>30&C_A_R_S$Age<60,"Mid-Age","Old"))

# Arrange the Column
C_A_R_S <- C_A_R_S[,c(1:3,14,4:13)]

# Extracting the Age category wise and the amount spend by that category
C_A_R_S_Age_Category <- C_A_R_S %>% dplyr::group_by(Age_Category) %>% dplyr::summarise(Age_Wise_Spend_Amount=sum(SumofSpendAmount))

# Arranging the file in desending order
C_A_R_S_Age_Category <- dplyr::arrange(C_A_R_S_Age_Category,desc(Age_Wise_Spend_Amount))

#Extracting the category which spends the most
Age_Category_Spending_Maximum <- C_A_R_S_Age_Category$Age_Category[1]
Age_Category_Spending_Maximum


# Qns 2.i Which Segment of People are Spending more Money

# Extracting the segment with that of the total spend amount as per the segment
TSA_Segment <- C_A_R_S %>% dplyr::group_by(Segment) %>% dplyr::summarise(Total_Spend_Amount=sum(SumofSpendAmount))

# Arranging the data frame in descending order to that of Spend_Amount
TSA_Segment <- dplyr::arrange(TSA_Segment,desc(Total_Spend_Amount))

# Extracting the Segment which spends the most
Most_Spending_Segment <- TSA_Segment$Segment[1]
Most_Spending_Segment


# Qns 2.j Which is the Most Profitable Segment

#Extracting the Segments which makes the most profit
Profitable_segment <- C_A_R_S %>% dplyr::group_by(Segment) %>% dplyr::summarise(Profit_Per_Segment=sum(Profit))

#Arranging the Segments as per the Profit in Descending Order
Profitable_segment <- dplyr::arrange(Profitable_segment,desc(Profit_Per_Segment))

# Extracting the Segment which provides Maximum Profit
Most_Profitable_Segment <- Profitable_segment$Segment[1]
Most_Profitable_Segment


# Qns 2.k Who are the Top 10 customers in form of Repayment

# Extracting Customer in terms of Repayment
Repayment_Top10 <- C_A_R_S %>% dplyr::group_by(Customer) %>% dplyr::summarise(Repayment_Amount=SumofRepaymentAmount)

# Arranging it in descending orderrs in terms of Repayment
Repayment_Top10 <- dplyr::arrange(Repayment_Top10,desc(Repayment_Amount))

#Extracting Top 10 Customers in terms of Repayment
Top_10 <- head(Repayment_Top10,n=10)
Top_10





#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

                  #Qns 3  

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


# Qns 3  Create a report with citywise spend on each Category on Yearly basis, Also include Graphical Representation for the same

Spend_2 <- Spend %>% dplyr::group_by(Customer,Year_Name,Type) %>% dplyr::summarise(SUMAmount=sum(Amount))
CS <- merge(Spend_2,Customer_Acqusition,by.x = "Customer",by.y = "Customer",all=T)
require(ggplot2)
ggplot2::ggplot(data = CS) + aes(x=Year_Name,y=SUMAmount,fill=Type) + geom_bar(stat = "identity",position = "dodge")+facet_grid(.~City)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

              #Qns 4  

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#Qns 4.1 Yearly/Monthly Comparison of Total Spends on Different Categories

install.packages("ggplot2")
library(ggplot2)

# Extracting Categories with respect to year and Sum amount
Spend_Year_Graph <- Spend %>% group_by(Type,Year_Name) %>% dplyr::summarise(SumofSpendAmount=sum(Amount))

# Plotting the graph on the basis of Year and Total Spend Amount and Category
Spend_Yearly <- ggplot(data = Spend_Year_Graph)+ aes(x=Year_Name,y=SumofSpendAmount,fill=Type)  +geom_bar(stat = "identity",color="black")
Spend_Yearly

# Extracting Categories with respect to month and Sum amount
require(ggplot2)

#Spend_Graph_1 <- Spend %>% group_by(Type,Month_Name) %>% dplyr::summarise(SumofSpendAmount=sum(Amount))
Spend_Month_Graph <- Spend %>% group_by(Type,Month_Yr) %>% dplyr::summarise(SumofSpendAmount=sum(Amount))

# Plotting the graph on the basis of month and Total Spend Amount and Category
Spend_Monthly <- ggplot(data = Spend_Month_Graph)+ aes(x=Month_Yr,y=SumofSpendAmount,fill=Type)  +geom_bar(stat = "identity",color="black")+ theme(text = element_text(size=20),axis.text.x = element_text(angle=90, hjust=1))
                                                                                                                   
Spend_Monthly

#Qns 4.2 Yearly/Monthly Comparison of Total Spends, CityWise
Spend %>% dplyr::group_by(city,year)%>% summarise(sumofamount = sum(Amount))
spendbycat_citygraph<- ggplot(data = spendbycat_city) + aes(x= Year, y= sumofamount, fill= City) + geom_bar(stat = "Identity", color = "black")
spendbycat_citygraph


#Qns 4.3 Comparison of Yearly Trends of Expenditure on Air Ticket

# Extracting Air Ticket wrt to Spend Amount and Month
Spend_Air_Ticket <- Spend %>% group_by(Type,Year_Name) %>% dplyr::summarise(SumofSpendAmount=sum(Amount))
Spend_Air_Ticket <- Spend_Air_Ticket[Spend_Air_Ticket$Type=="AIR TICKET",]

# Plotting the Graph on the basis of Air Category, Month and Total Spend Amount
Spend_Yearly_Air <- ggplot(data = Spend_Air_Ticket)+ aes(x=Year_Name,y=SumofSpendAmount)  +geom_bar(stat = "identity",color="black",fill="cyan")
Spend_Yearly_Air


# Qns 4.4 
Spend_Pattern <- Spend %>% group_by(Month_Name,Year_Name) %>% dplyr::summarise(SumofSpendAmount=sum(Amount))
Spend_Pattern_Graph <- ggplot(data = Spend_Pattern)+ aes(x=Month_Name,y=SumofSpendAmount,fill=Month_Name)  +geom_bar(stat = "identity")+facet_grid(.~Year_Name)
Spend_Pattern_Graph






  