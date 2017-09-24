
#remove all current objects to clear workspace
rm(list = ls()) 


#structure:
#Part 1: General Data Cleaning line 11 - 1004
#Part 2: Data Cleaning on Four Date Variables line 1005 - 1381
#Part 3: Impute Missing Value for Weight 1382



##############################################################################
#                                                                            #
#                        Part 1 --- General Data Cleaning                    #
#                                                                            #
##############################################################################

###################################################
#                                                 #
#               1. Import Data                    #
#                                                 #
###################################################

#load data
#space '' is not recongized as NA(not available data)
#reload the data, recongnize all space as missing values
train <- read.csv("E:/Assignment_BADS_WS1617/assignment_BADS_WS1617_known.csv",na.strings=c("NA","NaN",""," "))
test <- read.csv("E:/Assignment_BADS_WS1617/assignment_BADS_WS1617_class.csv",na.strings=c("NA","NaN",""," "))

#re-examine the missing values in two dataset
sapply(train, function(x) {sum(is.na(x))}) 
sapply(test, function(x) {sum(is.na(x))}) 

###################################################
#               1.1 Missing Values                #
###################################################
# form_of_address missing 6866(train) 1650(test)--------------13% 13%
# account_creation_date missing 3412(train) 867(test)--------- 7% 7%
# weight missing 3947(train) 985(test)------------------------ 8% 8%
# postcode_delivery 49608(train) 12412(test)------------------96% 96%
# advertising_code  41593(train) 10458(test)------------------80% 80%

#calulate ratio of missing values
round(41593/51884,2)
round(10458/12971,2)
test$return_customer = NA
data = rbind(train,test)
#visualize the missing values, the more pink the more missing values(reduce from left to right)
#install.packages("Amelia")
library(Amelia)
missmap(train, main = "Missing values vs observed Training")
missmap(test, main = "Missing values vs observed Test")
#conclusion
# 5 of 37 variables contain missing values
#two variables have NAs massively
#it seems that missing values are identically distributted

#count the missing values per row
train$na_count <- apply(train, 1, function(x) sum(is.na(x)))
table(train$na_count)

test$return_customer = 1
test$na_count <- apply(test, 1, function(x) sum(is.na(x)))
table(test$na_count)

library(ggplot2)
repurchase = as.factor(train$return_customer)
qplot(ID, na_count, data = train, col=repurchase)+labs(title="Train: Missing Value Per Order against ID",
                                                            x ="ID", y = "NA Count")

qplot(ID, na_count, data = test,col=I("lightblue"))+labs(title="Test: Missing Value Per Order against ID",
                                                            x ="ID", y = "NA Count")

repurchase = as.factor(train$return_customer)
qplot(order_date, na_count, data = train, colour = repurchase)+labs(title="Train: Missing Value Per Order against Order Date",
                                                            x ="Order Date", y = "NA Count")
qplot(order_date, na_count, data = train, colour = repurchase)+labs(title="Test: Missing Value Per Order against Order Date",
                                                          x ="Order Date", y = "NA Count")
###################################################
#                                                 #
#     2. Explantory Data Analysis-Part A          #
#                                                 #
###################################################

class(train)
class(test)

library(dplyr)
glimpse(train)
glimpse(test)

#following variables are not in correct type

#order_date --- date/factor
#account_creation_date --- date/factor
table(train$title) #title --- factor
table(train$newsletter) #newsletter --- factor
table(train$model) #model --- factor
table(train$delivery) #delivery --- factor
table(train$coupon) #coupon --- factor
table(train$goods_value) #goods_value --- factor
table(train$giftwrapping) #giftwrapping --- factor
table(train$points_redeemed) #points_redeemed --- factor  this column contains no info
table(train$cost_shipping) #cost_shipping --- factor
table(train$return_customer) #return_customer --- factor

str(train, list.len=ncol(train)) # structure of the dataset
str(test, list.len=ncol(test))

51884/(51884+12971)
#according to Customer_ID, train is first 80% observations and test is last 20% obervations
#what is the order of the originial 64855 x 38 dataset would be key to predict return

#distribution of dependent variable:
prop.table(table(train$return_customer)) 
#only 18.8% are return customers
Sur = table(train$return_customer)
barplot(Sur,main = 'Churn vs. Repurchase',names.arg = c('Churn','Repurchase'),col=c('red','green'))

#examine the relationships btw target variable(return_customer) and other variales
#to do these a technically prepared dataset is in need
#do data cleaning to process the raw dataset


###################################################
#                                                 #
#                 3. Data Cleaning                #
#                                                 #
###################################################

#preparation for data cleaning:

#assign NA(not available) to return customer of test, so as the new column return_customer is created
test$return_customer = NA 
#combine two dataset as a new one
data = rbind(train,test) 
dim(data)

str(data)
# check up for missing values
sapply(data, function(x) {sum(is.na(x))})

names(data)

###############################
#          No.1 --- ID        #
###############################
#Summary and Max is not consistant
summary(data$ID)
max(data$ID)


###############################
#     No.2 --- order_date     #
###############################
#factor as character to extract info like year, month, day, weekdays
order_date_as_char = as.character(data$order_date)
data$order_date_as_date = as.Date(order_date_as_char,"%Y/%m/%d")
data$order_date_year = substr(order_date_as_char,1,4) 
data$order_date_year = as.factor(data$order_date_year)
data$order_date_month = substr(order_date_as_char,6,7)
data$order_date_month = as.factor(data$order_date_month)
data$order_date_day = substr(order_date_as_char,9,10)
data$order_date_day = as.factor(data$order_date_day)
data$order_date_weekday = weekdays(data$order_date_as_date)
data$order_date_weekday = as.factor( data$order_date_weekday)

#generate more variables
library(data.table)
dt = data.table(data)
dt[,"monthly_order_frequency" := .N, by= order_date_month]
dt[,"daily_order_frequency" := .N, by= order_date]
dt[,"yearly_order_frequency" := .N, by= order_date_year]

data = dt

# a time series of daily/weekly/monthly order number/ratio change should be conducted
#important

#plot the trend

library(ggplot2)

ggplot(data=train[train$return_customer==1,],aes(order_date_as_date,daily_order_frequency))+geom_line(colour="green")+xlab("Date")+ylab("Count of Order")+ggtitle("Count of Order Per Day")
ggplot(data=train[train$return_customer==0,],aes(order_date_as_date,daily_order_frequency))+geom_line(colour="blue")+xlab("Date")+ylab("Count of Order")+ggtitle("Count of Order Per Day")

# a comparsion of daily return-non return order count across time series should be counted
#important


###############################
#   No.3 --- form_of_address  #
###############################
#Missing Value
#Appropriate Class: factor
data$form_of_address = as.character(data$form_of_address)
data$form_of_address[is.na(data$form_of_address)] = "Unknown"
data$form_of_address = as.factor(data$form_of_address)
table(data$form_of_address)


###############################
#       No.4 --- title        #
###############################
data$title = as.factor(data$title)
class(data$title)
m = table(data$return_customer,data$title) 
m[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m  
#remember 18.8%(rate of overall repurchase) is the benchmark
#Title seems be useful

###############################
#   No.5 --- email_domain     #
###############################
data$email_domain = as.factor(data$email_domain)
table(data$email_domain) #13 levels
m = table(data$return_customer,data$email_domain) 
m[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m  
#remember 18.8%(rate of overall repurchase) is the benchmark
#email_domain seems be useful

data$email_domain_other = 0
data$email_domain_other[levels(data$email_domain)=="other"] = 1
data$email_domain_other = as.factor(data$email_domain_other)
class(data$email_domain_other)
prop.table(table(data$return_customer,data$email_domain_other),2)

data$email_domain_german = 1
data$email_domain_german[levels(data$email_domain)=="aol.com"|levels(data$email_domain)=="gmail.com"|levels(data$email_domain)=="other"|levels(data$email_domain)=="yahoo.com"] = 0
data$email_domain_german = as.factor(data$email_domain_german)
class(data$email_domain_german)
prop.table(table(data$return_customer,data$email_domain_german),2)

data$email_domain_german_no_other = 1
data$email_domain_german_no_other[levels(data$email_domain)=="aol.com"|levels(data$email_domain)=="gmail.com"|levels(data$email_domain)=="yahoo.com"] = 0
data$email_domain_german_no_other = as.factor(data$email_domain_german_no_other)
class(data$email_domain_german_no_other)
prop.table(table(data$return_customer,data$email_domain_german_no_other),2)

#######################################
#   No.6 --- account_creation_date    #
#######################################
#Missing Value
#Strang Value
acc_cre_date_as_char = as.character(data$account_creation_date)
data$acc_cre_date_as_date = as.Date(acc_cre_date_as_char,"%Y/%m/%d")
data$acc_cre_date_year = substr(acc_cre_date_as_char,1,4) 
data$acc_cre_date_year = as.factor(data$acc_cre_date_year)
data$acc_cre_date_month = substr(acc_cre_date_as_char,6,7)
data$acc_cre_date_month = as.factor(data$acc_cre_date_month)
data$acc_cre_date_day = substr(acc_cre_date_as_char,9,10)
data$acc_cre_date_day = as.factor(data$acc_cre_date_day)
data$acc_cre_date_weekday = weekdays(data$acc_cre_date_as_date)
data$acc_cre_date_weekday = as.factor(data$acc_cre_date_weekday)
str(data)
dim(data)


#######################################
#         No.7 --- newsletter         #
#######################################
table(data$newsletter) #2 levels
data$newsletter = as.factor(data$newsletter)
table(data$newsletter)
m = table(data$return_customer,data$newsletter) 
m[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m  
#18.8% as benchmark
#newsletter is one of the most critical predictor

#######################################
#           No.8 --- model            #
#######################################
class(data$model) #3 levels
data$model = as.factor(data$model)
m = table(data$return_customer,data$model) 
m[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m  
#18.8% as benchmark
#website stlye seems a good predictor

#########################################
#           No.9 --- Payment            #
#########################################
data$payment = as.factor(data$payment)
table(data$payment) #4levels
m = table(data$return_customer,data$payment) 
m[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m  

m = table(data$model,data$payment)
m[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m  
chisq.test(data$model,data$payment)
#X-squared = 9080.8, df = 6, p-value < 2.2e-16



#########################################
#           No.10 --- delivery          #
#########################################
table(data$delivery) #2 levels Delivery type. 0 = Door delivery, 1 = Collection at post office
data$delivery = as.factor(data$delivery)
m = table(data$return_customer,data$delivery) 
m[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m  
#18.8% as benchmark
#delivery seems a good predictor


#################################################
#           No.11 --- postcode_invoice          #
#################################################
table(data$postcode_invoice)
data$postcode_invoice = as.numeric(data$postcode_invoice)
sort(table(data$postcode_invoice))
hist(data$postcode_invoice)
table(sapply(data$postcode_invoice, function(x) {sum(is.na(x))}))
data$postcode_invoice[is.na(data$postcode_invoice)] = 44 #44 is the mode
library(ggplot2)
ggplot(data[1:51884,],aes(x=postcode_invoice,fill=return_customer))+geom_histogram(bins = 100)
cor.test(data[1:51884,]$return_customer,data[1:51884,]$postcode_invoice)
#t = 0.98074, df = 51882, p-value = 0.3267
#insignificant generate some more info

data$postcode_invoice_rank = as.numeric(factor(data$postcode_invoice,levels=unique(data$postcode_invoice)))
cor.test(data[1:51884,]$return_customer,data[1:51884,]$postcode_invoice_rank)
#t = -2.5672, df = 51882, p-value = 0.01026 cor -0.01126999
table(data$postcode_invoice_rank)
library(smbinning)
result=smbinning(df=data[1:51884,],y="return_customer",x="postcode_invoice_rank",p=0.05) 
result$ivtable
result# IV is still low

##################################################
#           No.12 --- postcode_delivery          #
##################################################
#Missing Value
#Appropriate Class: factor

data$postcode_delivery =as.character(data$postcode_delivery) 
data$postcode_delivery[is.na(data$postcode_delivery)] = "Unknown"
table(data$postcode_delivery)
data$postcode_delivery = as.factor(data$postcode_delivery )

data$postcode_delivery_missing = 0
data[data$postcode_delivery == "Unknown",]$postcode_delivery_missing = 1
table(data$postcode_delivery_missing)

m = table(data$return_customer,data$postcode_delivery_missing)
m[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m  

data$equal_postcode = 0
data[data$postcode_delivery==data$postcode_invoice,]$equal_postcode = 1
m = table(data$return_customer,data$equal_postcode)
m[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m#indifferent

##################################################
#                No.13 --- coupon                #
##################################################
table(data$coupon) #2 levels
data$coupon = as.factor(data$coupon)
m = table(data$return_customer,data$coupon) 
m[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m  
#18.8% as benchmark
#coupon seems a good predictor

##################################################
#           No.14 --- advertising_code           #
##################################################
#Missing Value
#Appropriate Class: factor
data$advertising_code = as.character(data$advertising_code)
data$advertising_code[is.na(data$advertising_code)] = "Unknown"
table(data$advertising_code)
data$advertising_code = as.factor(data$advertising_code )

data$advertising_code_missing = 0
data[data$advertising_code == "Unknown",]$advertising_code_missing = 1
table(data$advertising_code_missing)

chisq.test(data[1:51884,]$return_customer,data[1:51884,]$advertising_code_missing)

#group by A B C and others
data$advertising_code = as.character(data$advertising_code)
data$advertising_code_group = substr(data$advertising_code,1,1) 
table(data$advertising_code_group)
data$advertising_code = as.factor(data$advertising_code)
data$advertising_code_group = as.factor(data$advertising_code_group)
chisq.test(data[1:51884,]$return_customer,data[1:51884,]$advertising_code_group)
#X-squared = 7.8595, df = 3, p-value = 0.04901

##################################################
#              No.15 --- goods_value             #
##################################################
#goods_value should be ordinal
data$goods_value = ordered(data$goods_value )
class(data$goods_value)

m = table(train$return_customer,train$goods_value)
m[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m

#calculate average price of each item in an order
  data$avg_goods_value = as.numeric(data$goods_value)/data$item_count
class(data$avg_goods_value)
#goods_value seems a critical predictor, non liner realationship

#plot regarding goods_value

#plot1 imported and goods_value

repurchase = factor(train$return_customer)
k = qplot(x =factor(train$goods_value),
          y=train$imported_count, 
          data=train,
          geom=c("jitter"),
          color=repurchase)
k+xlab("Goods Value")+ylab("Imported Count")
#most imported goods are expensive


##################################################
#               No.16 --- item_count             #
##################################################

#question: what is the composition of item_count?


#the experiment shows the third edition of calculating item_count is mose correct
#so the code of edition 1/2/4/5 will not be ran the columns will be ignored too

cor(data$remitted_items,data$item_count)#15%
sum(data[,16])#130671
sum(as.vector(colSums(data[,27:37])))#138518 
sum(as.vector(colSums(data[,27:37]))) - sum(data[,16]) #7847 is the difference

sum(as.vector(colSums(data[,26])))#4428
sum(as.vector(colSums(data[,27:37]))) - sum(data[,16])-sum(as.vector(colSums(data[,26]))) #3419 is the difference

sum(as.vector(colSums(data[,25])))#3999
sum(as.vector(colSums(data[,27:37]))) - sum(data[,16])-sum(as.vector(colSums(data[,c(25,26)]))) #-580 is the difference

sum(as.vector(colSums(data[,24])))#3913
sum(as.vector(colSums(data[,27:37]))) - sum(data[,16])-sum(as.vector(colSums(data[,c(24,26)]))) #-494 is the difference


sum(as.vector(colSums(data[,24]))) +sum(as.vector(colSums(data[,27:37]))) - sum(data[,16])-sum(as.vector(colSums(data[,c(24,26)]))) #-494 is the difference

nrow(data[data$remitted_items>=data$used_items,])/length(data$used_items)#0.9609128
nrow(data[data$remitted_items>data$used_items,])/length(data$used_items)#0.037792
nrow(data[data$remitted_items==data$used_items,])/length(data$used_items)#0.9231208
nrow(data[data$remitted_items<data$used_items,])/length(data$used_items)#0.03908719

#there is 96% probability that remitted_items are more or equal to the number of used_items in the transcation

#enumerate all the possibilities
#1 edition

# item_count = count(canceled_items+used_items+all_products),remitted_items are not included

agg_1 = vector(mode ="numeric",length=length(data$item_count))

for (i in 1:length(data$item_count)){
  agg_1[i] = sum( data$canceled_items[i]
                               ,data$used_items[i],data$book_count[i]
                               ,data$paperback_count[i],data$schoolbook_count[i]
                               ,data$ebook_count[i],data$audiobook_count[i]
                               ,data$audiobook_download_count[i],data$film_count[i]
                               ,data$musical_count[i],data$hardware_count[i]
                               ,data$imported_count[i],data$other_count[i])
}

data$agg_1 = agg_1
nrow(data[data$agg_1==data$item_count,])/length(data$item_count)#91.3% accuracy

agg_1_equal = vector(mode="logical",length = length(data$item_count))

for(i in 1:length(data$item_count)){
  agg_1_equal[i] = (data$item_count[i] == data$agg_1[i])
}

table(agg_1_equal)

data$agg_1_equal = agg_1_equal
table(data$agg_1_equal )

#2nd edition
agg_2 = vector(mode ="numeric",length=length(data$item_count))

for (i in 1:length(data$item_count)){
  agg_2[i] = sum( -data$canceled_items[i],-data$remitted_items[i]
                  ,-data$used_items[i],data$book_count[i]
                  ,data$paperback_count[i],data$schoolbook_count[i]
                  ,data$ebook_count[i],data$audiobook_count[i]
                  ,data$audiobook_download_count[i],data$film_count[i]
                  ,data$musical_count[i],data$hardware_count[i]
                  ,data$imported_count[i],data$other_count[i])
}

data$agg_2 = agg_2
nrow(data[data$agg_2==data$item_count,])/length(data$item_count)#0.833598 accuracy

#3rd edition
#calculate agg without used_items
#remitted_items and used_items are not included
agg_3 = vector(mode ="numeric",length=length(data$item_count))

for (i in 1:length(data$item_count)){
  agg_3[i] = sum( data$canceled_items[i]
                              ,data$book_count[i]
                              ,data$paperback_count[i],data$schoolbook_count[i]
                              ,data$ebook_count[i],data$audiobook_count[i]
                              ,data$audiobook_download_count[i],data$film_count[i]
                              ,data$musical_count[i],data$hardware_count[i]
                              ,data$imported_count[i],data$other_count[i])
}

data$agg_3 = agg_3
nrow(data[data$agg_3==data$item_count,])/length(data$item_count)#95.2% right now

agg_3_equal = vector(mode="logical",length = length(data$item_count))

for(i in 1:length(data$item_count)){
  agg_3_equal[i] = (data$item_count[i] == data$agg_3[i])
}

table(agg_3_equal)

data$agg_3_equal = agg_3_equal
table(data$agg_3_equal )
data$agg_3_equal = as.character(data$agg_3_equal)
data$agg_3_equal[data$agg_3_equal=="TRUE"] = 1
data$agg_3_equal[data$agg_3_equal=="FALSE"] = 0
data$agg_3_equal = as.factor(data$agg_3_equal)


#fourth edition
agg_4 = vector(mode ="numeric",length=length(data$item_count))

for (i in 1:length(data$item_count)){
  agg_4[i] = sum( data$used_items[i],-data$canceled_items[i],-data$remitted_items[i]
                       ,data$book_count[i]
                       ,data$paperback_count[i],data$schoolbook_count[i]
                       ,data$ebook_count[i],data$audiobook_count[i]
                       ,data$audiobook_download_count[i],data$film_count[i]
                       ,data$musical_count[i],data$hardware_count[i]
                       ,data$imported_count[i],data$other_count[i]
                       )
  #remitted_items are not included
}

data$agg_4 = agg_4
nrow(data[data$agg_no_can==data$item_count,])/length(data$item_count)#83%

#fifth edition
agg_5 = vector(mode ="numeric",length=length(data$item_count))

for (i in 1:length(data$item_count)){
  agg_5[i] = sum(       data$book_count[i]
                       ,data$paperback_count[i],data$schoolbook_count[i]
                       ,data$ebook_count[i],data$audiobook_count[i]
                       ,data$audiobook_download_count[i],data$film_count[i]
                       ,data$musical_count[i],data$hardware_count[i]
                       ,data$imported_count[i],data$other_count[i]
  )
}

data$agg_5 = agg_5
nrow(data[data$agg_5==data$item_count,])/length(data$item_count)#90.4%

#fifth edition
agg_5 = vector(mode ="numeric",length=length(data$item_count))

for (i in 1:length(data$item_count)){
  agg_5[i] = sum( data$used_items[i],-data$canceled_items[i]
                       ,data$book_count[i]
                       ,data$paperback_count[i],data$schoolbook_count[i]
                       ,data$ebook_count[i],data$audiobook_count[i]
                       ,data$audiobook_download_count[i],data$film_count[i]
                       ,data$musical_count[i],data$hardware_count[i]
                       ,data$imported_count[i],data$other_count[i]
  )
  #remitted_items are not included
}

data$agg_5 = agg_5
nrow(data[data$agg_5==data$item_count,])/length(data$item_count)#86%

#the third edition is the best (similarity rate 95%)

FS_GBM = data[1:51884,]
result=smbinning(df=FS_GBM,y="return_customer",x="item_count",p=0.05) 
result$ivtable #total IV 0.0348

result=smbinning(df=FS_GBM,y="return_customer",x="agg_3",p=0.05) 
result$ivtable #total IV 0.0340 dropped

cor.test(FS_GBM$return_customer,FS_GBM$item_count)
#t = 14.772, df = 51882, p-value < 0.00000000000000022,   cor  0.06471598 

cor.test(FS_GBM$return_customer,FS_GBM$agg_no_used_items)
#t = 10.708, df = 51882, p-value < 0.00000000000000022,   cor  0.04695858 

#so do not replace ? 

#do ranking
data$item_count_rank = as.numeric(factor(data$item_count,levels=unique(data$item_count)))
cor.test(data[1:51884,]$return_customer,data[1:51884,]$item_count_rank)
#t = 11.465, df = 51882, p-value < 0.00000000000000022, cor -cor 0.05027194

library(smbinning)
result=smbinning(df=data[1:51884,],y="return_customer",x="item_count_rank",p=0.05) 
result$ivtable
result# 0.0348

library(smbinning)
result=smbinning(df=data[1:51884,],y="return_customer",x="item_count",p=0.05) 
result$ivtable
result# 0.0348

#do not replace right now !
nrow(data[data$item_count==data$all_product_count,])/nrow(data)#90%
train = data[!is.na(data$return_customer),]
test = data[is.na(data$return_customer),]
nrow(train[train$all_product_count==0,])/nrow(train)#90%
nrow(test[test$all_product_count==0,])/nrow(test)#90%
names(data)
##################################################
#             No.17 --- giftwrapping             #
##################################################
table(data$giftwrapping)
data$giftwrapping = as.factor(data$giftwrapping)
m = table(train$return_customer,train$giftwrapping)
m[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m

#18.8% as benchmark
#giftwrapping reduces prob of repurchasing

chisq.test(data[1:51884,]$giftwrapping,data[1:51884,]$equal_postcode)
#X-squared = 75.504, df = 1, p-value < 0.00000000000000022
x = data[data$giftwrapping==1,c("postcode_invoice","postcode_delivery")]
nrow(data[(data$giftwrapping==1)&(data$postcode_invoice==data$postcode_delivery),])
g = data[(data$giftwrapping==1)&(data$postcode_invoice==data$postcode_delivery),]

#giftwrapping cannot help impute missing postcode_delivery very well

##################################################
#                No.18 --- referrer              #
##################################################
table(data$referrer)
data$referrer = as.factor(data$referrer)
m = table(train$return_customer,train$referrer)
m2 <- m
m2[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m2


##################################################
#             No.19 --- points_redeemed          #
##################################################
table(data$points_redeemed)
data$points_redeemed = NULL
#variance is zero

##################################################
#              No.20 --- cost_shipping           #
##################################################
table(data$cost_shipping)
data$cost_shipping = as.factor(data$cost_shipping)
m = table(data$return_customer,data$cost_shipping) 
m[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m  
#cost_shipping is a critical predictor

#######################################################
#           No.21 --- deliverydate_estimated          #
#######################################################
#one suppose: deliver time may be affected by how many kinds of product in one order
#one suppose: deliver time is affected by which category of product in the order

#Strang Value
head(data$deliverydate_estimated)
de_est_date_as_char = as.character(data$deliverydate_estimated)
data$de_est_date_as_date = as.Date(de_est_date_as_char,"%Y/%m/%d")
data$de_est_date_year = substr(de_est_date_as_char,1,4) 
data$de_est_date_year = as.factor(data$de_est_date_year)
data$de_est_date_month = substr(de_est_date_as_char,6,7)
data$de_est_date_month = as.factor(data$de_est_date_month)
data$de_est_date_day = substr(de_est_date_as_char,9,10)
data$de_est_date_day = as.factor(data$de_est_date_day)
data$de_est_date_weekday = weekdays(data$de_est_date_as_date)
data$de_est_date_weekday = as.factor(data$de_est_date_weekday)

#compute estimated deliver time
#estimated_deliver_time = de_est_date_as_date - order_date_as_date
order_date_as_char = as.character(data$order_date)
de_est_date_as_char = as.character(data$deliverydate_estimated)
date1 <- strptime(de_est_date_as_char, format="%Y/%m/%d")
date2 <- strptime(order_date_as_char, format="%Y/%m/%d")
data$days_est_deliver = floor(difftime(date1, date2, units = "days"))
head(data$days_est_deliver)
class(data$days_est_deliver)
data$days_est_deliver = as.numeric(data$days_est_deliver)
class(data$days_est_deliver) 
table(data$days_est_deliver)
#write a loop to detect the type of errors in deliveded_estimate and count frequency
#generate a new column to notice the error type on deliveded_estimate

x = data[data$days_est_deliver==0,c("order_date","deliverydate_estimated")]#this number is wrong
data[data$days_est_deliver==0]$days_est_deliver = 1

x = data[data$days_est_deliver==2,c("order_date","deliverydate_estimated")]

names(data)#col no.70
data$days_est_deliver_category = NA
class(data$days_est_deliver_category)
data$days_est_deliver_category = as.character(data$days_est_deliver_category)
#important info to write loop in r
#http://www.statmethods.net/management/operators.html

for (i in 1:nrow(data)) {
  if (data[i,70] > 730){ data[i,71] = "More Than Two Years"}
  else if ((data[i,70] > 365)& (data[i,70] <=730)){ data[i,71] = "Between one and Two Years"}
  else if ((data[i,70] > 60)& (data[i,70] < 365)){ data[i,71] = "2 Months to One Year"}
  else if ((data[i,70] > 0)& (data[i,70] <= 60)){ data[i,71] = "Inner 2 Month"}
  else if ( data[i,70] == 0){ data[i,71] = "Same Day"}
  else if ( data[i,70] < 0){ data[i,71] = "Negative"}
}
data$days_est_deliver_category = as.factor(data$days_est_deliver_category)
names(data)
table(data$days_est_deliver_category)
save(data,file="BADS_DataCleaning_2")

#there is no 0 day exist!!!!!!!!!!!!!!!!!!!!!
#there must be sth. wrong
#are the extremely large date actually 0????
table(data$deliverydate_actual)
nrow(data[as.character(data$deliverydate_actual)==as.character(data$order_date),])/length(data$days_est_deliver_category)

#15.83 of products are deliveried actually at the day the ordered was placed

#what are these kind of products???
#prodcut classification

#what are digital products actually???

#digital products
xx = data[(data$musical_count>0)&(data$book_count==0)&(data$schoolbook_count==0)&(data$hardware_count==0)&(data$other_count==0)&(data$hardware_count==0)&(data$imported_count==0)&(data$paperback_count==0)&(data$audiobook_count==0)&(data$musical_count==0)&(data$film_count==0),]
xx = data[(data$cost_shipping==1)&(data$musical_count>0),]
#musical is not digital products
xx = data[(data$cost_shipping==1)&(data$film_count>0),]
#film is not digital products since orders with only films are also shipped (cost_shipping = 1)
xx = data[(data$cost_shipping==1)&(data$audiobook_count>0),]
#audiobook is not digital products since orders with only films are also shipped (cost_shipping = 1)
xx = data[(data$cost_shipping==1)&(data$audiobook_download_count>0),]
#audiobook_download is digital product
xx = data[(data$cost_shipping==1)&(data$ebook_count>0),]
#ebook is digital product

names(data)
data$digital_count = NULL

#count of digital products in one order
digital_product_count = vector(mode ="numeric",length=length(data$item_count))

for (i in 1:length(data$item_count)){
  digital_product_count[i] = sum( data$ebook_count[i]
                             ,data$audiobook_download_count[i])
}

data$digital_product_count = digital_product_count
table(data$digital_product_count)

names(data)
#count of physical products in one order
physical_product_count = vector(mode ="numeric",length=length(data$item_count))

for (i in 1:length(data$item_count)){
  physical_product_count[i] = sum( data$book_count[i]
                                  ,data$paperback_count[i]
                                  ,data$schoolbook_count[i]
                                  ,data$audiobook_count[i]
                                  ,data$film_count[i]
                                  ,data$musical_count[i]
                                  ,data$hardware_count[i]
                                  ,data$imported_count[i]
                                  ,data$other_count[i]
                                  )
}

data$physical_product_count = physical_product_count
table(data$physical_product_count)

data$all_product_count = NA
data$all_product_count = as.numeric(data$all_product_count)
data$all_product_count = as.numeric(data$physical_product_count)+as.numeric(data$digital_product_count)
max(data$all_product_count)


nrow(data[data$all_product_count>0,])
table(data$all_product_count)

data$order_canceled = 0
data[data$all_product_count==0,]$order_canceled = 1
table(data$order_canceled)
data$order_canceled = as.factor(data$order_canceled)

data$basket_diversity_coarse_grained = "Empty Basket" 
data$basket_diversity_coarse_grained[(data$physical_product_count==0)&(!data$digital_product_count==0)] = "Pure Digital Goods"
data$basket_diversity_coarse_grained[(!data$physical_product_count==0)&(data$digital_product_count==0)] = "Pure Physical Goods"
data$basket_diversity_coarse_grained[(!data$all_product_count==0)&(!data$physical_product_count==0)&(!data$digital_product_count==0)] = "Mixed Basket"

table(data$basket_diversity_coarse_grained)
prop.table(table(data$return_customer,data$basket_diversity_coarse_grained),2)


# data$basket_diversity_fine_grained, to think about if this makes sense later on
#important

#use the information on goods to impute the missing or incorrect info on date

xx = data[data$basket_diversity_coarse_grained=="Pure Digital Goods",c(2,6,20,21)]
#impute 0000/00/00 by order_date
data[data$basket_diversity_coarse_grained=="Pure Digital Goods"]$deliverydate_actual = data[data$basket_diversity_coarse_grained=="Pure Digital Goods"]$order_date
#9114 observations with 0000/00/00 is now replaced

##################################################
#           No.22 --- deliverydate_actual        #
##################################################
#impute missing value of actual_delivery by order_date
data$deliverydate_actual[is.na(data$deliverydate_actual)] = data$order_date[is.na(data$deliverydate_actual)]
#impute 0000/00/00 in deliver_date_actual by order_date since they are canceled orders
data$deliverydate_actual[data$deliverydate_actual=="0000/00/00"] = data$order_date[data$deliverydate_actual=="0000/00/00"]

sapply(data, function(x) {sum(is.na(x))})
names(data)
de_act_date_as_char = as.character(data$deliverydate_actual)
data$de_act_date_as_date = as.Date(de_act_date_as_char,"%Y/%m/%d")
data$de_act_date_year = substr(de_act_date_as_char,1,4) 
data$de_act_date_year = as.factor(data$de_act_date_year)
data$de_act_date_month = substr(de_act_date_as_char,6,7)
data$de_act_date_month = as.factor(data$de_act_date_month)
data$de_act_date_day = substr(de_act_date_as_char,9,10)
data$de_act_date_day = as.factor(data$de_act_date_day)
data$de_act_date_weekday = weekdays(data$de_act_date_as_date)
data$de_act_date_weekday = as.factor(data$de_act_date_weekday)

#compute actual deliver time
#days_deliver_actual = de_act_date_as_date - order_date_as_date
date1 <- strptime(de_act_date_as_char, format="%Y/%m/%d")
date2 <- strptime(order_date_as_char, format="%Y/%m/%d")
data$days_deliver_actual = floor(difftime(date1, date2, units = "days"))
table(data$days_deliver_actual)
data$days_deliver_actual = as.numeric(data$days_deliver_actual)
class(data$days_deliver_actual)
View(data[,c(2,6,20,21,82,37)])
##################################################
#                 No.23 --- weight               #
##################################################
#weight analysis see later part
table(sapply(data$weight,function(x){sum(is.na(x))}))
xx = data[(is.na(data$weight))&(data$basket_diversity_coarse_grained=="Pure Digital Goods"),]
data[(is.na(data$weight))&(data$basket_diversity_coarse_grained=="Pure Digital Goods"),]$weight = 0

##################################################
#           No.24 --- remitted_items             #
##################################################
table(data$remitted_items)
#there is no liner relationship btw remitted_items and return_customer, since the effect of buying more (like this business)
#is not seperated from hate this business (remitte more)
m = table(data$return_customer,data$remitted_items) 
m[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m  


names(data)#col no.23,74,83
data$remitted_items_level = NA
data$remitted_items_level = as.character(data$remitted_items_level)

for (i in 1:nrow(data)) {
  if ((data[i,23] > 0) & (data[i,23] == data[i,74])){ data[i,83] = "Return All"}
   else if ((data[i,23] > 0)& (data[i,23] > (data[i,74])/2)){ data[i,83] = "Return More than Half"}
   else if ((data[i,23] > 0)& (data[i,23] ==(data[i,74])/2)){ data[i,83] = "Return Half"}
   else if ((data[i,23] > 0)& (data[i,23] < (data[i,74])/2)){ data[i,83] = "Return Less than Half"} 
   else if (data[i,23] == 0){ data[i,83] = "Return Nothing"}
}

data$remitted_items_level  = as.factor(data$remitted_items_level )
prop.table(table(data$return_customer,data$remitted_items_level),2)#the result is counter-inuative
data$half = NULL

##################################################
#           No.25 --- canceled_items             #
##################################################
ggplot(data=data,aes(x=1,y=canceled_items))+geom_boxplot()
ggplot(data=data,aes(x=canceled_items))+geom_histogram()
table(data$canceled_items)
m = table(data$return_customer,data$canceled_items) 
m[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m  
#take log to make the distribution normal-like??



##################################################
#              No.26 --- used_items              #
##################################################
ggplot(data=data,aes(x=1,y=used_items))+geom_boxplot()
ggplot(data=data,aes(x=used_items))+geom_histogram()
table(data$used_items)
m = table(data$return_customer,data$used_items) 
m[] <- sprintf("%.0f%%",round(prop.table(m,2)*100, 3))
m  
#take log to make the distribution normal-like??

##################################################
#              No.27 --- book_count              #
##################################################
summary(data$book_count)
ggplot(data=data,aes(x=book_count))+geom_histogram()

##################################################
#           No.28 --- paperback_count            #
##################################################
summary(data$paperback_count)
ggplot(data=data,aes(x=paperback_count))+geom_histogram()

##################################################
#           No.29 --- schoolbook_count           #
##################################################
summary(data$schoolbook_count)
ggplot(data=data,aes(x=schoolbook_count))+geom_histogram()

##################################################
#           No.12 --- postcode_delivery          #
##################################################
summary(data$postcode_delivery)
ggplot(data=data,aes(x=postcode_delivery))+geom_histogram()

##################################################
#           No.30 --- ebook_count                #
##################################################
summary(data$ebook_count)
ggplot(data=data,aes(x=ebook_count))+geom_histogram()

##################################################
#           No.31 --- audiobook_count            #
##################################################
summary(data$audiobook_count)
ggplot(data=data,aes(x=audiobook_count))+geom_histogram()

##################################################
#     No.32 --- audiobook_download_count         #
##################################################
summary(data$audiobook_download_count)
ggplot(data=data,aes(x=audiobook_download_count))+geom_histogram()

##################################################
#           No.33 --- film_count                 #
##################################################
summary(data$film_count)
ggplot(data=data,aes(x=film_count))+geom_histogram()

##################################################
#            No.34 --- musical_count             #
##################################################
summary(data$musical_count)
ggplot(data=data,aes(x=musical_count))+geom_histogram()

##################################################
#              No.35 --- hardware_count          #
##################################################
summary(data$hardware_count)
ggplot(data=data,aes(x=hardware_count))+geom_histogram()

##################################################
#           No.36 --- imported_count             #
##################################################
summary(data$imported_count)
ggplot(data=data,aes(x=imported_count))+geom_histogram()

##################################################
#               No.37 --- other_count            #
##################################################
summary(data$other_count)
ggplot(data=data,aes(x=other_count))+geom_histogram()

##################################################
#             No.38 --- return_customer          #
##################################################
table(data$return_customer) #2levels
data$return_customer = factor(data$return_customer,labels=c("No Repurcahase","Repurchase"))
table(data$return_customer)

save(data, file = "BADS_DATACLEANING_Nov_16.Rda")

##############################################################################
#                                                                            #
#                   Part 2 --- Data Cleaning on Four Date Variables          #
#                                                                            #
##############################################################################

dim(data)
sapply(data, function(x) {sum(is.na(x))})
names(data)
###################################################
#                                                 #
#                 1. The Task                     #
#                                                 #
###################################################


#re-process the four date variables from the perspective of business
#http://www.ez2world.com/6207.html


#understand the problems to be solved:
#three of four date-related columns are problematic

#define days_delivery_delay = days_deliver_actual - days_est_deliver
data$days_delivery_delay = data$days_deliver_actual - data$days_est_deliver
table(data$days_delivery_delay) 

#Missing Vlaues
#account_creation_date missing 3412(train) 867(test)--------- 7% 7%
dd = data[is.na(data$account_creation_date),]
dc = data[!is.na(data$account_creation_date),]
prop.table(table(dd$return_customer))#0.1822978
prop.table(table(dc$return_customer))#0.1887894
dc$order_date= as.character(dc$order_date)
dc$account_creation_date= as.character(dc$account_creation_date)
nrow(dc[dc$order_date == dc$account_creation_date,])/nrow(dc)#0.9562368
prop.table(table(dc[dc$order_date == dc$account_creation_date,]$return_customer))
prop.table(table(dd$return_customer))

View(train[is.na(train$account_creation_date),c(2,6,21,22)])
#Error
View(train[train$deliverydate_actual=="0000/00/00",c(2,6,21,22)])
nrow(train[train$deliverydate_actual=="0000/00/00",c(2,6,21,22)])#8708 rows,16.8% of observations
nrow(test[test$deliverydate_actual=="0000/00/00",c(2,6,21,22)]) #2119 rows,16.3% of observations
#Error
View(data[data$de_est_date_year=="2010",c(2,6,21,22)])
nrow(data[data$de_est_date_year=="2010",c(2,6,21,22)]) #73 rows in the year of 2010
View(data[data$de_est_date_year=="4746",c(2,6,21,22)])
nrow(data[data$de_est_date_year=="4746",c(2,6,21,22)]) #14 rows in the year of 4746
#Error
View(data[(data$days_est_deliver_category=="Between one and Two Years"),c(2,6,21,22,67,68,24:38)]) 

names(data)#col no.82
class(data$days_deliver_actual)
summary(data$days_deliver_actual)
data$days_act_deliver_category = as.character(data$days_act_deliver_category)
table(data$days_deliver_actual)

for (i in 1:nrow(data)) {
  if (data[i,82] > 365){ data[i,85] = "More Than One Year"}
  else if ((data[i,82] > 0) & (data[i,82] <= 60)){ data[i,85] = "Inner 2 Month"}
  else if ( data[i,82] == 0){ data[i,85] = "Same Day"}
  else if ( data[i,82] < 0){ data[i,85] = "Negative"}
}

data$days_act_deliver_category = as.factor(data$days_act_deliver_category)
table(data$days_act_deliver_category)
save(data, file = "BADS_DATACLEANING_Nov_16.Rda")
dim(data)

###################################################
#                                                 #
#                3. Methodology                   #
#                                                 #
###################################################
names(data)
#set one: days_est_deliver_category (bins of how long it is estimated to deliver)
#         days_act_deliver_category (bins of how long it is actually to deliver)
View(data[,c(2,6,71,85)])
#days_est_deliver_category = estimated_delivery_date - order_date
#days_act_deliver_category = actual_delivery_date - order_date

table(data$days_est_deliver_category)
#2 Months to One Year       Between one and Two Years             Inner 2 Month 
#            62                       3530                            61176 
#More Than Two Years(4746)          Negative(2010) 
#          14                           73 

table(data$days_act_deliver_category)
#Inner 2 Month     Same Day          More Than One Year 
#42518              21101                   110 

###################################################
#                                                 #
#                  4. Analysis                    #
#                                                 #
###################################################

####################################################################
#                                                                  #
#            Question of Missing Account Creation Date             #
#                                                                  #
####################################################################
#a column labels the missing value in data$account_creation_date
data$account_creation_date_missing = 0
data$account_creation_date_missing[is.na(data$account_creation_date)] = 1
table(data$account_creation_date_missing)
data$account_creation_date_missing = as.factor(data$account_creation_date_missing)

#there is a difference
prop.table(table(data[is.na(data$account_creation_date)]$return_customer))
prop.table(table(data[!is.na(data$account_creation_date)]$return_customer))

View(data[is.na(data$account_creation_date)])
names(data)
#compute how long it takes to generate first order
#days_first_order_occurs = order_date_as_date - acc_cre_date_as_date 
names(data)
order_date_as_char   = as.character(data$order_date)
acc_cre_date_as_char = as.character(data$account_creation_date)
date1 <- strptime(order_date_as_char, format="%Y/%m/%d")
date2 <- strptime(acc_cre_date_as_char, format="%Y/%m/%d")
data$days_first_order_occurs = floor(difftime(date1, date2, units = "days"))
summary(data$days_first_order_occurs)
data$days_first_order_occurs = as.numeric(data$days_first_order_occurs)
class(data$days_first_order_occurs)
summary(data$days_first_order_occurs)
table(data$days_first_order_occurs)
57925/64855
#89% of first orders are placed by the same day the account is created
#important rule found!!!
#replace odd account_creation_date by order_date

names(data)

data$instant_purchase[data$days_first_order_occurs==0] <- 1
data$instant_purchase[data$days_first_order_occurs!=0] <- 0
table(data$instant_purchase)
x = table(data[!is.na(data$return_customer),]$return_customer,data[!is.na(data$return_customer),]$instant_purchase)
prop.table(x,2)
class(data$days_first_order_occurs)
train = data[1:51884,]
cor.test(train[!is.na(train$days_first_order_occurs),]$return_customer,train[!is.na(train$days_first_order_occurs),]$days_first_order_occurs)
#the customers donot purchase as soon as they creat their account,
#are more likely to repurchase(26%) than those purchase instantly(19%)
#p-value = 0.007193 significant
chisq.test(as.factor(train[!is.na(train$days_first_order_occurs),]$return_customer),train[!is.na(train$days_first_order_occurs),]$instant_purchase)
#X-squared = 69.436, df = 1, p-value < 2.2e-16
data$account_creation_date = as.character(data$account_creation_date)
data$account_creation_date[is.na(data$account_creation_date)] = data$order_date[is.na(data$account_creation_date)]
data$account_creation_date = as.factor(data$account_creation_date)
table(sapply(data$account_creation_date, function(x) {sum(is.na(x))}))


###################################################
#                                                 #
#            Question of "0000/00/00"             #
#                                                 #
###################################################

#replace by order_date, stated before already

###################################################
#                                                 #
#            Question of "2010/01/01"             #
#                                                 #
###################################################
#the time interval of transactions is 2013/12/29-2014/03/06
#estimated delivery in the year of 2010 is deffinately not acceptable
names(data)
nrow( data[data$de_est_date_year=="2010",c(2,6,20,21,22,38)])
tem = data[data$de_est_date_year=="2010",c(2,6,21,22,37)]
prop.table(table(tem$return_customer)) #15.0 which is lower than 18.8%!!!
#Nuggets!!!

#why ???
#it takes averagely 8 days to delviery to the customers
#the customers are definately bortherd by a delayed delivery

#73 observation is estimated to be delivered in 2010
#but in reality should be in 2014
#so change all 2010 to 2014

data$de_est_date_year[data$de_est_date_year==2010] = 2014
unique(data$de_est_date_year)
data$x =paste(data$de_est_date_year,"/",data$de_est_date_month,"/",data$de_est_date_day,sep = "")
data$x_as_date = as.Date(data$x,"%Y/%m/%d")
data$x_as_date = as.character(data$x_as_date)
data$x_as_date = gsub("-", "/", data$x_as_date)
head(data$order_date)
head(data$x_as_date)
data$deliverydate_estimated  = as.factor(data$x_as_date)
head(data$deliverydate_estimated)
save(data, file = "BADS_DATACLEANING_Nov_16.Rda")
load("BADS_DATACLEANING_Nov_16.Rda")

###################################################
#                                                 #
#            Question of "4646/01/01"             #
#                                                 #
###################################################
#the time interval of transactions is 2013/12/29-2014/03/06
#estimated delivery in the year of 2010 is deffinately not acceptable
View( data[data$de_est_date_year=="4746",c(2,6,20,21,22,38)])
tem = data[data$de_est_date_year=="4746",c(2,6,21,22,37)]
prop.table(table(tem$return_customer))#33.3% higher than 18.8%

#generate a column to capture the error
#a column labels the missing value in data$account_creation_date
data$de_est_date_year_4746 = 0
data$de_est_date_year_4746[data$de_est_date_year=="4746"] = 1
table(data$de_est_date_year_4746)

#replace all estimated_delivery_date which is in the year of 4746 by order_date
data$deliverydate_estimated = as.character(data$deliverydate_estimated)
data$order_date = as.character(data$order_date)
data$deliverydate_estimated[data$de_est_date_year=="4746"] = data$order_date[data$de_est_date_year=="4746"]
data$deliverydate_estimated = as.factor(data$deliverydate_estimated)
data$order_date = as.factor(data$order_date)
names(data)

#this is actually crude, or not correct
###################################################
#                                                 #
#          shipping more than one year            #
#                                                 #
###################################################
View(data[(data$days_est_deliver_category=="Between one and Two Years"),c(2,6,20,21,22,38)]) 
#911 observations 
#replace 2014 by 2013

data$de_est_date_year[data$days_est_deliver_category=="Between one and Two Years"]=2013
unique(data$de_est_date_year)
data$x =paste(data$de_est_date_year,"/",data$de_est_date_month,"/",data$de_est_date_day,sep = "")

data$x_as_date = as.Date(data$x,"%Y/%m/%d")
data$x_as_date = as.character(data$x_as_date)
data$x_as_date = gsub("-", "/", data$x_as_date)
head(data$order_date)
head(data$x_as_date)
data$deliverydate_estimated  = as.factor(data$x_as_date)
head(data$deliverydate_estimated)

###################################################
#                                                 #
#                5. Conclusion                    #
#                                                 #
###################################################
#The actual delivery date of transactions contains only digitial goods is disordered,
#this happens to the canceled orders as well, in such circumstance the actual delivery date is
#replaced by order_date 

#Missing Account Creation Date --- Replace by order_date (96% the case)
#account_creation_date missing 3412(train) 867(test)--------- 7% 7%

#"4646/05/02"
#replace by the corresponing deliverydate_actual + 2 days

#"2010/01/06"
#replace 2010 by 2014

#"0000/00/00"
#replace by order_date
#rerun the year, month and day after replaceing
#factor as character to extract info like year, month, day, weekdays
order_date_as_char = as.character(data$order_date)
data$order_date_as_date = as.Date(order_date_as_char,"%Y/%m/%d")
data$order_date_year = substr(order_date_as_char,1,4) 
data$order_date_year = as.factor(data$order_date_year)
data$order_date_month = substr(order_date_as_char,6,7)
data$order_date_month = as.factor(data$order_date_month)
data$order_date_day = substr(order_date_as_char,9,10)
data$order_date_day = as.factor(data$order_date_day)
data$order_date_weekday = weekdays(data$order_date_as_date)
data$order_date_weekday = as.factor( data$order_date_weekday)


acc_cre_date_as_char = as.character(data$account_creation_date)
data$acc_cre_date_as_date = as.Date(acc_cre_date_as_char,"%Y/%m/%d")
data$acc_cre_date_year = substr(acc_cre_date_as_char,1,4) 
data$acc_cre_date_year = as.factor(data$acc_cre_date_year)
data$acc_cre_date_month = substr(acc_cre_date_as_char,6,7)
data$acc_cre_date_month = as.factor(data$acc_cre_date_month)
data$acc_cre_date_day = substr(acc_cre_date_as_char,9,10)
data$acc_cre_date_day = as.factor(data$acc_cre_date_day)
data$acc_cre_date_weekday = weekdays(data$acc_cre_date_as_date)
data$acc_cre_date_weekday = as.factor(data$acc_cre_date_weekday)


head(data$deliverydate_estimated)
de_est_date_as_char = as.character(data$deliverydate_estimated)
data$de_est_date_as_date = as.Date(de_est_date_as_char,"%Y/%m/%d")
data$de_est_date_year = substr(de_est_date_as_char,1,4) 
data$de_est_date_year = as.factor(data$de_est_date_year)
data$de_est_date_month = substr(de_est_date_as_char,6,7)
data$de_est_date_month = as.factor(data$de_est_date_month)
data$de_est_date_day = substr(de_est_date_as_char,9,10)
data$de_est_date_day = as.factor(data$de_est_date_day)
data$de_est_date_weekday = weekdays(data$de_est_date_as_date)
data$de_est_date_weekday = as.factor(data$de_est_date_weekday)


de_act_date_as_char = as.character(data$deliverydate_actual)
data$de_act_date_as_date = as.Date(de_act_date_as_char,"%Y/%m/%d")
data$de_act_date_year = substr(de_act_date_as_char,1,4) 
data$de_act_date_year = as.factor(data$de_act_date_year)
data$de_act_date_month = substr(de_act_date_as_char,6,7)
data$de_act_date_month = as.factor(data$de_act_date_month)
data$de_act_date_day = substr(de_act_date_as_char,9,10)
data$de_act_date_day = as.factor(data$de_act_date_day)
data$de_act_date_weekday = weekdays(data$de_act_date_as_date)
data$de_act_date_weekday = as.factor(data$de_act_date_weekday)

#impute missing (b/c account_creation_date is missing) by -1 so as to maintain a numeric format
data$days_first_order_occurs = as.numeric(data$days_first_order_occurs)
data$days_first_order_occurs[is.na(data$days_first_order_occurs)] = -1
table(data$days_first_order_occurs)

data$instant_purchase = as.factor(data$instant_purchase)
data$instant_purchase[is.na(data$instant_purchase)] = "Unknown"
table(data$instant_purchase)

#compute actual deliver time
#days_deliver_actual = de_act_date_as_date - order_date_as_date
de_act_date_as_char = as.character(data$deliverydate_actual)
order_date_as_char = as.character(data$order_date)

date1 <- strptime(de_act_date_as_char, format="%Y/%m/%d")
date2 <- strptime(order_date_as_char, format="%Y/%m/%d")
data$days_deliver_actual = floor(difftime(date1, date2, units = "days"))
table(data$days_deliver_actual)
data$days_deliver_actual = as.numeric(data$days_deliver_actual)
class(data$days_deliver_actual)

de_est_date_as_char = as.character(data$deliverydate_estimated)
date1 <- strptime(de_est_date_as_char, format="%Y/%m/%d")
date2 <- strptime(order_date_as_char, format="%Y/%m/%d")
data$days_est_deliver = floor(difftime(date1, date2, units = "days"))
head(data$days_est_deliver)
class(data$days_est_deliver)
data$days_est_deliver = as.numeric(data$days_est_deliver)
class(data$days_est_deliver) 

#define days_delivery_delay = days_deliver_actual - days_est_deliver
data$days_delivery_delay = data$days_deliver_actual - data$days_est_deliver
table(data$days_delivery_delay)

data$days_act_deliver_category = as.character(data$days_act_deliver_category)
data$days_act_deliver_category[is.na(data$days_act_deliver_category)] = "Unknown"
data$days_act_deliver_category = as.factor(data$days_act_deliver_category)
table(data$days_act_deliver_category)


###excursion to fix the wired value in account_creation_date after imputing by order_date

train <- read.csv("E:/Assignment_BADS_WS1617/assignment_BADS_WS1617_known.csv",na.strings=c("NA","NaN",""," "))
test <- read.csv("E:/Assignment_BADS_WS1617/assignment_BADS_WS1617_class.csv",na.strings=c("NA","NaN",""," "))
test$return_customer = NA
xx = rbind(train,test)
data$account_creation_date = xx$account_creation_date
table(sapply(data$account_creation_date, function(x) {sum(is.na(x))}) )
data$account_creation_date = as.character(data$account_creation_date)
data$order_date = as.character(data$order_date)

data$account_creation_date[is.na(data$account_creation_date)] = data$order_date[is.na(data$account_creation_date)]
data$account_creation_date = as.factor(data$account_creation_date)
data$order_date = as.factor(data$order_date)

table(data$account_creation_date)

#####################   end of excursion

save(data, file = "BADS_DATACLEANING_Nov_16.Rda")
load("BADS_DATACLEANING_Nov_16.Rda")
dim(data)
sapply(data, function(x) {sum(is.na(x))})

##############################################################################
#                                                                            #
#                       Part 3 --- Missing Value in Weight                   #
#                                                                            #
##############################################################################

################# canceled_items studies
test$return_customer = NA
xx = rbind(train,test)

data$canceled_items = xx$canceled_items

summary(data$canceled_items)

table(data$canceled_items)
table(data$all_product_count)

nrow(data[(data$canceled_items>data$all_product_count),])#1989
  nrow(data[(data$canceled_items>data$all_product_count)&(data$all_product_count==0),])#1932
  nrow(data[(data$canceled_items>data$all_product_count)&(data$all_product_count>0),])#57
  
nrow(data[(data$canceled_items==data$all_product_count),])#613
  nrow(data[(data$canceled_items==data$all_product_count)&(data$all_product_count==0),])#64
  nrow(data[(data$canceled_items==data$all_product_count)&(data$all_product_count>0),])#549

nrow(data[(data$canceled_items<data$all_product_count),])#62253
  nrow(data[(data$canceled_items<(data$all_product_count)/2),])#61894
  nrow(data[(data$canceled_items==(data$all_product_count)/2)&(data$all_product_count>0),])#329
  nrow(data[(data$canceled_items>(data$all_product_count)/2)&(data$all_product_count>0)&(data$all_product_count>data$canceled_items),])#30
  

names(data)#24,74,91
data$canceled_items_level = NA
data$canceled_items_level = as.character(data$canceled_items_level)

for (i in 1:nrow(data)) {
  if ((data[i,24] > data[i,74]) & (data[i,74] == 0)){ data[i,91] = "Cancel All And No Basket Records"}
  else if ((data[i,24] > data[i,74]) & (data[i,74] > 0)){ data[i,91] = "Cancel More Than Records"}
 
  else if ((data[i,24] == data[i,74]) & (data[i,74] == 0)){ data[i,91] = "Zero Cancel Zero Records"}
  else if ((data[i,24] == data[i,74]) & (data[i,74] > 0)){ data[i,91] = "Cancel All Some Basket Records Left"}
  
  else if (data[i,24] < (data[i,74]/2)){ data[i,91] = "Cancel Less Than Half"}
  else if ((data[i,24] == (data[i,74]/2)) & (data[i,74] > 0)){ data[i,91] = "Cancel Half"}
  else if ((data[i,74] >data[i,24])&(data[i,24] > (data[i,74]/2)) & (data[i,74] > 0)){ data[i,91] = "Cancel More Than Half"}

}

data$canceled_items_level = as.factor(data$canceled_items_level)
table(data$canceled_items_level)

prop.table(table(data$return_customer,data$canceled_items_level),2)

################## weight analysis
train <- read.csv("E:/Assignment_BADS_WS1617/assignment_BADS_WS1617_known.csv",na.strings=c("NA","NaN",""," "))
test <- read.csv("E:/Assignment_BADS_WS1617/assignment_BADS_WS1617_class.csv",na.strings=c("NA","NaN",""," "))

test$return_customer = NA
xx = rbind(train,test)

data$weight = xx$weight

nrow(data[is.na(data$weight),] )#4932

data$weight_missing = NULL
data$account_creation_date_missing = NULL
# outlier in weight
nrow(data[data$weight==0,])#14306
jj = data[(data$weight==0)&(!data$basket_diversity_coarse_grained=="puredigitalgoods"),]#4892
table(data$basket_diversity_coarse_grained)
nrow(data[(data$weight==0)&(data$basket_diversity_coarse_grained=="emptybasket"),])#364
View(data[(data$weight==0)&(data$basket_diversity_coarse_grained=="emptybasket")&(data$canceled_items==0),])#27
nrow(data[(data$weight==0)&(data$basket_diversity_coarse_grained=="purephysicalgoods")&(!data$other_count==0),])#4528
View(data[(data$all_product_count == data$other_count)&(!data$all_product_count == 0),])#4340

#For Pure Digital Goods
#weight of basket with pure digital prodtucts are replaced by 0
table(data$basket_diversity_coarse_grained)
nrow(data[(is.na(data$weight))&(data$basket_diversity_coarse_grained=="puredigitalgoods"),])
data[(is.na(data$weight))&(data$basket_diversity_coarse_grained=="puredigitalgoods"),]$weight = 0
#707 cases are fixed
nrow(data[is.na(data$weight),])
nrow(data[(is.na(data$weight))&(data$basket_diversity_coarse_grained=="puredigitalgoods"),])

#For Empty Basket:

nrow(data[(is.na(data$weight))&(data$basket_diversity_coarse_grained=="emptybasket"),])#149
nrow(data[(is.na(data$weight))&(data$basket_diversity_coarse_grained=="emptybasket")&(data$cost_shipping==1),])#47

x = median(data[(!is.na(data$weight))&(data$basket_diversity_coarse_grained=="emptybasket")&(data$cost_shipping==0),]$weight)
y = median(data[(!is.na(data$weight))&(data$basket_diversity_coarse_grained=="emptybasket")&(data$cost_shipping==1),]$weight)

data[(is.na(data$weight))&(data$basket_diversity_coarse_grained=="emptybasket")&(data$cost_shipping==0),]$weight = x
data[(is.na(data$weight))&(data$basket_diversity_coarse_grained=="emptybasket")&(data$cost_shipping==1),]$weight = y
#149 cases are fixed, 4076 left
nrow(data[is.na(data$weight),])

#For Pure Physcal Goods:
table(data$basket_diversity_fine_grained)

nrow(data[(is.na(data$weight))&(data$basket_diversity_coarse_grained=="purephysicalgoods"),])#4076
#audiobookonly
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="audiobookonly")&(data$cost_shipping==1),])#7
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="audiobookonly")&(data$cost_shipping==0),])# 60

x = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="audiobookonly")&(data$cost_shipping==1),]$weight)
y = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="audiobookonly")&(data$cost_shipping==0),]$weight)
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="audiobookonly")&(data$cost_shipping==1),]$weight = x
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="audiobookonly")&(data$cost_shipping==0),]$weight = y
#67 cases are fixed, 4009 left
4076-4009
nrow(data[is.na(data$weight),])
table(data$basket_diversity_fine_grained)
#bookaudiobook
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookaudiobook")&(data$cost_shipping==1),])#0
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookaudiobook")&(data$cost_shipping==0),])# 42

x = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="bookaudiobook")&(data$cost_shipping==1),]$weight)
y = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="bookaudiobook")&(data$cost_shipping==0),]$weight)
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookaudiobook")&(data$cost_shipping==1),]$weight = x
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookaudiobook")&(data$cost_shipping==0),]$weight = y

nrow(data[is.na(data$weight),])

#bookimport
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookimport")&(data$cost_shipping==1),])#3
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookimport")&(data$cost_shipping==0),])# 81

x = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="bookimport")&(data$cost_shipping==1),]$weight)
y = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="bookimport")&(data$cost_shipping==0),]$weight)
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookimport")&(data$cost_shipping==1),]$weight = x
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookimport")&(data$cost_shipping==0),]$weight = y

nrow(data[is.na(data$weight),])
#3883
#bookonly
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookonly")&(data$cost_shipping==1),])#289
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookonly")&(data$cost_shipping==0),])#1435

x = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="bookonly")&(data$cost_shipping==1),]$weight)
y = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="bookonly")&(data$cost_shipping==0),]$weight)
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookonly")&(data$cost_shipping==1),]$weight = x
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookonly")&(data$cost_shipping==0),]$weight = y

nrow(data[is.na(data$weight),])

#bookother
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookother")&(data$cost_shipping==1),])
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookother")&(data$cost_shipping==0),])

x = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="bookother")&(data$cost_shipping==1),]$weight)
y = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="bookother")&(data$cost_shipping==0),]$weight)
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookother")&(data$cost_shipping==1),]$weight = x
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookother")&(data$cost_shipping==0),]$weight = y

nrow(data[is.na(data$weight),])

#bookpaperback
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookpaperback")&(data$cost_shipping==1),])
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookpaperback")&(data$cost_shipping==0),])

x = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="bookpaperback")&(data$cost_shipping==1),]$weight)
y = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="bookpaperback")&(data$cost_shipping==0),]$weight)
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookpaperback")&(data$cost_shipping==1),]$weight = x
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookpaperback")&(data$cost_shipping==0),]$weight = y

nrow(data[is.na(data$weight),])

#bookschoolbook
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookschoolbook")&(data$cost_shipping==1),])
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookschoolbook")&(data$cost_shipping==0),])

x = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="bookschoolbook")&(data$cost_shipping==1),]$weight)
y = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="bookschoolbook")&(data$cost_shipping==0),]$weight)
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookschoolbook")&(data$cost_shipping==1),]$weight = x
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="bookschoolbook")&(data$cost_shipping==0),]$weight = y

nrow(data[is.na(data$weight),])

#filmonly
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="filmonly")&(data$cost_shipping==1),])
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="filmonly")&(data$cost_shipping==0),])

x = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="filmonly")&(data$cost_shipping==1),]$weight)
y = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="filmonly")&(data$cost_shipping==0),]$weight)
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="filmonly")&(data$cost_shipping==1),]$weight = x
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="filmonly")&(data$cost_shipping==0),]$weight = y

nrow(data[is.na(data$weight),])
table(data$basket_diversity_fine_grained)
#importonly
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="importonly")&(data$cost_shipping==1),])
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="importonly")&(data$cost_shipping==0),])

x = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="importonly")&(data$cost_shipping==1),]$weight)
y = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="importonly")&(data$cost_shipping==0),]$weight)
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="importonly")&(data$cost_shipping==1),]$weight = x
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="importonly")&(data$cost_shipping==0),]$weight = y

nrow(data[is.na(data$weight),])

#musicalonly
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="musicalonly")&(data$cost_shipping==1),])
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="musicalonly")&(data$cost_shipping==0),])

x = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="musicalonly")&(data$cost_shipping==1),]$weight)
y = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="musicalonly")&(data$cost_shipping==0),]$weight)
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="musicalonly")&(data$cost_shipping==1),]$weight = x
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="musicalonly")&(data$cost_shipping==0),]$weight = y

nrow(data[is.na(data$weight),])
class(data$weight) = as.numeric(data$weight)

#musicalonly
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="musicalonly")&(data$cost_shipping==1),])
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="musicalonly")&(data$cost_shipping==0),])

x = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="musicalonly")&(data$cost_shipping==1),]$weight)
y = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="musicalonly")&(data$cost_shipping==0),]$weight)
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="musicalonly")&(data$cost_shipping==1),]$weight = x
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="musicalonly")&(data$cost_shipping==0),]$weight = y

nrow(data[is.na(data$weight),])

#otheronly
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="otheronly")&(data$cost_shipping==1),])
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="otheronly")&(data$cost_shipping==0),])

x = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="otheronly")&(data$cost_shipping==1),]$weight)
y = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="otheronly")&(data$cost_shipping==0),]$weight)
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="otheronly")&(data$cost_shipping==1),]$weight = x
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="otheronly")&(data$cost_shipping==0),]$weight = y

nrow(data[is.na(data$weight),])

#paperbackonly
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="paperbackonly")&(data$cost_shipping==1),])
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="paperbackonly")&(data$cost_shipping==0),])

x = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="paperbackonly")&(data$cost_shipping==1),]$weight)
y = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="paperbackonly")&(data$cost_shipping==0),]$weight)
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="paperbackonly")&(data$cost_shipping==1),]$weight = x
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="paperbackonly")&(data$cost_shipping==0),]$weight = y

nrow(data[is.na(data$weight),])

#physicalmixed
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="physicalmixed")&(data$cost_shipping==1),])
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="physicalmixed")&(data$cost_shipping==0),])

x = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="physicalmixed")&(data$cost_shipping==1),]$weight)
y = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="physicalmixed")&(data$cost_shipping==0),]$weight)
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="physicalmixed")&(data$cost_shipping==1),]$weight = x
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="physicalmixed")&(data$cost_shipping==0),]$weight = y

nrow(data[is.na(data$weight),])

#schoolbookonly
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="schoolbookonly")&(data$cost_shipping==1),])
nrow(data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="schoolbookonly")&(data$cost_shipping==0),])

x = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="schoolbookonly")&(data$cost_shipping==1),]$weight)
y = median(data[(!is.na(data$weight))&(data$basket_diversity_fine_grained=="schoolbookonly")&(data$cost_shipping==0),]$weight)
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="schoolbookonly")&(data$cost_shipping==1),]$weight = x
data[(is.na(data$weight))&(data$basket_diversity_fine_grained=="schoolbookonly")&(data$cost_shipping==0),]$weight = y

nrow(data[is.na(data$weight),])
table(data$weight)
save(data, file="BADS_DATACLEANING_Nov_30.Rda")
load("BADS_DATACLEANING_Nov_30.Rda")
