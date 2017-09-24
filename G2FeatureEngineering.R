#remove all current objects to clear workspace
#structure 
#   Feature Construction --- customer repurchase study based  20
#   Feature Construction --- ranking of nominal variable 160
#   Feature Processing   --- discretization of nominal variable 171
#   Feature Construction --- counting of discretization 234
#   Feature Processing --- dummy (one hot encoding) 259
#   Feature Construction --- interaction 291
#   Feature Construction --- nosiy_observation 505

#   Feature Selection --- drop Date type varaibles 526
#   Feature Selection --- Filter  545
#   Feature Selection --- Wrapper  706
#   Feature Selection --- Embedded  871

###################################################
#                                                 #
#                Feature Construction             #
#                                                 #
###################################################

##################  based on customer repurchase study    ################ 
load("BADS_DATACLEANING_Nov_21.Rda")
prop.table(table(data$goods_value,data$order_date_month),2)
prop.table(table(data$return_customer,data$order_date_month),2)
names(data)
summary(data$avg_goods_value)

x = known[known$all_product_count==0,]
table(x$canceled_items)
View(x[x$canceled_items==1,])
prop.table(table(x$return_customer))
#cancel more than products in record is a trace for customers' unsatisification
x = unknown[unknown$all_product_count==0,]
table(x$canceled_items)
View(x[x$canceled_items==0,])

z = known[known$canceled_items>=known$all_product_count,]
prop.table(table(z$return_customer))

#create varaible that captures canceled items are more than products count in one order
data$cancel_more_than_records = 0
data[data$canceled_items>=data$all_product_count,]$cancel_more_than_records = 1
data$cancel_more_than_records = as.factor(data$cancel_more_than_records)
table(data$cancel_more_than_records)
table(data$canceled_items_level)

table(data$basket_diversity_coarse_grained)

##basket diversity fine grained
names(data)#physical:26:28,30,32:36,digital:29,31,76,94
data$basket_diversity_fine_grained = NA
data$basket_diversity_fine_grained = as.character(data$basket_diversity_fine_grained)

for (i in 1:nrow(data)) {
  if (( data[i,76] == "Pure Physical Goods") &(!data[i,26] == 0) & (sum(data[i,c(27,28,30,32:36)]) == 0)){ data[i,94] = "bookonly"}
  else if (( data[i,76] == "Pure Physical Goods") &(!data[i,27] == 0) & (sum(data[i,c(26,28,30,32:36)]) == 0)){ data[i,94] = "paperbackonly"}
  else if (( data[i,76] == "Pure Physical Goods") & (!data[i,28] == 0) &(sum(data[i,c(26,27,30,32:36)]) == 0)){ data[i,94] = "schoolbookonly"}
  else if (( data[i,76] == "Pure Physical Goods") & (!data[i,30] == 0) &(sum(data[i,c(26:28,32:36)]) == 0)){ data[i,94] = "audiobookonly"}
  else if (( data[i,76] == "Pure Physical Goods") & (!data[i,32] == 0) &(sum(data[i,c(26:28,30,33:36)]) == 0)){ data[i,94] = "filmonly"}
  else if (( data[i,76] == "Pure Physical Goods") & (!data[i,33] == 0) &(sum(data[i,c(26:28,30,32,34:36)]) == 0)){ data[i,94] = "musicalonly"}
  else if (( data[i,76] == "Pure Physical Goods") & (!data[i,34] == 0) &(sum(data[i,c(26:28,30,32,33,34,36)]) == 0)){ data[i,94] = "hardwareonly"}
  else if (( data[i,76] == "Pure Physical Goods") & (!data[i,35] == 0) &(sum(data[i,c(26:28,30,32,33,34,36)]) == 0)){ data[i,94] = "importonly"}
  else if (( data[i,76] == "Pure Physical Goods") & (!data[i,36] == 0) &(sum(data[i,c(26:28,30,32,33,34,35)]) == 0)){ data[i,94] = "otheronly"}
  else if (( data[i,76] == "Pure Digital Goods") & (data[i,c(31)] == 0)& (!data[i,29] == 0)){ data[i,94] = "ebookonly"}
  else if (( data[i,76] == "Pure Digital Goods") & (data[i,c(29)] == 0)& (!data[i,31] == 0)){ data[i,94] = "audiodownloadonly"}
  else if (( data[i,76] == "Pure Digital Goods") & (!data[i,c(29)] == 0)& (!data[i,c(31)] == 0)){ data[i,94] = "digitalmixed"}
}

data[(data$basket_diversity_coarse_grained=="Pure Digital Goods")&(!data$audiobook_download_count==0)&(data$ebook_count==0),]$basket_diversity_fine_grained = "audiodownloadonly"
data[(data$basket_diversity_coarse_grained=="Pure Digital Goods")&(!data$audiobook_download_count==0)&(!data$ebook_count==0),]$basket_diversity_fine_grained = "ebookaudiodowload"
data[(data$basket_diversity_coarse_grained=="Pure Physical Goods")&(!data$book_count==0)&(!data$paperback_count==0)&(sum(data[,c(30,32:36)])),]$basket_diversity_fine_grained = "bookpaperback"
data[(data$basket_diversity_coarse_grained=="Pure Physical Goods")&(!data$book_count==0)&(!data$paperback_count==0)&(sum(data[,c(28,30,32:36)])),]$basket_diversity_fine_grained = "bookpaperback"
data[(data$basket_diversity_coarse_grained=="Pure Physical Goods")&(!data$book_count==0)&(!data$schoolbook_count==0)&(sum(data[,c(27,30,32:36)])),]$basket_diversity_fine_grained = "bookschoolbook"
data[(data$basket_diversity_coarse_grained=="Pure Physical Goods")&(!data$book_count==0)&(!data$audiobook_count==0)&(sum(data[,c(27,28,32:36)])),]$basket_diversity_fine_grained = "bookaudiobook"
data[(data$basket_diversity_coarse_grained=="Pure Physical Goods")&(!data$book_count==0)&(!data$imported_count==0)&(sum(data[,c(27,28,30,32:34,36)])),]$basket_diversity_fine_grained = "bookimport"
data[(data$basket_diversity_coarse_grained=="Pure Physical Goods")&(!data$book_count==0)&(!data$other_count==0)&(sum(data[,c(27,28,30,32:35)])),]$basket_diversity_fine_grained = "bookother"
data[is.na(data$basket_diversity_fine_grained),]$basket_diversity_fine_grained = "physicalmixed"
View(data[is.na(data$basket_diversity_fine_grained),])
data$basket_diversity_fine_grained = as.factor(data$basket_diversity_fine_grained)
table(data$basket_diversity_fine_grained)

prop.table(table(data$return_customer,data$basket_diversity_fine_grained),2)
z = (data[(data$basket_diversity_coarse_grained=="Pure Digital Goods"),])

table(sapply(data$basket_diversity_fine_grained, function(x) {sum(is.na(x))}))

levels(data$basket_diversity_fine_grained) <- c("audiodownloadonly","bookonly","ebookonly",
                                                "emptybasket","mixedbasket","mixedphysical","paperbackonly")
prop.table(table(data$return_customer,data$basket_diversity_fine_grained),2)
nrow(data[!data$book_count==0,])


chisq.test(known$return_customer,known$basket_diversity_fine_grained)
#X-squared = 267.09, df = 16, p-value < 2.2e-16

# calculate IV and WOE

# Factor Variable Application -----------------------------------------------
library(InformationValue)
known= data[1:51884,]
WOETable(X=known$basket_diversity_fine_grained, Y=known$return_customer)

#basket diversity coarse grained
data$basket_diversity_coarse_grained = as.factor(data$basket_diversity_coarse_grained)
levels(data$basket_diversity_coarse_grained) = c("emptybasket"   ,     "mixedbasket"  ,      "puredigitalgoods" , "purephysicalgoods")
table(data$basket_diversity_coarse_grained)


save(data, file="BADS_DATACLEANING_Jan_25.Rda")

load("BADS_DATACLEANING_jan_25.Rda")
table(data$basket_diversity_fine_grained)
#calculate price change by product and by date
with(data,tapply(avg_goods_value,list(basket_diversity_fine_grained,order_date_month),diff))
names(data)
z = data[order(data[,94],data[,2]),]#sort by basket category and date
z$diff <- ave(z$avg_goods_value, factor(z$basket_diversity_fine_grained), FUN=function(x) c(NA,diff(x)))
z[is.na(z$diff)]$diff=0
View(z[,c(2,94,95)])
names(z)
data = merge(data,z[,c(1,95)],by="ID")
colnames(data)[95] = "product_val_diff_by_day"
names(data)
# statisitcs for product frequency --- mainstrem customers
library(data.table)

data[,"basket_frequency" := .N, by= basket_diversity_fine_grained]

table(data$basket_frequency)

# product_val_diff_by_day monthly differnece
z = data[order(data[,94],data[,2]),]#sort by basket category and date
z = data[order(data[,94],data[,40],data[,41])]#sort by basket category and year and month
z$pro_val_diff_by_day_mon_ave <- ave(z$avg_goods_value, factor(z$order_date_month), FUN=function(x) c(NA,cumsum(x)))

View(z[,c(2,94,95)])
names(z)
View(z[,c(2,94,95,97)])
z[is.na(z$pro_val_diff_by_day_mon_ave)]$pro_val_diff_by_day_mon_ave=0
data = merge(data,z[,c(1,97)],by="ID")

save(data, file="BADS_DATACLEANING_Jan_26.Rda")
load("BADS_DATACLEANING_jan_26.Rda")
names(data)

#rate of remitting and canceling
data$remitted_items_rate = data$remitted_items/data$all_product_count
data$canceled_items_rate =  data$canceled_items/data$all_product_count
data[data$all_product_count==0,]$canceled_items_rate = 0
data[is.na(data$remitted_items_rate),]$remitted_items_rate = 0
data[is.na(data$canceled_items_rate),]$canceled_items_rate = 0

#used_items_rate
save(data, file="BADS_DATACLEANING_Jan_30.Rda")
data$used_items_rate = data$used_items/data$all_product_count
data[is.infinite(data$used_items_rate),]$used_items_rate = 0
data_FS_FEB_01$used_items_rate = data$used_items_rate

##################    ranking of nominal variable   #####################
load("BADS_DATACLEANING_jan_26.Rda")
data.factor = Filter(is.factor,data)
data.num = Filter(is.numeric,data)
str(data.num)

numrank = lapply(data.num[-1],rank,ties.method='min')
numrank = as.data.frame(numrank)
colnames(numrank) = paste("Rank",colnames(numrank),sep="_")
save(numrank, file="numrank.Rda")

##################    discretization of continuous variables   #####################    
#equal width
binning = function(num.vector,NO_BINS=5,DO_EQUAL_WIDTH=T){
  #cut is used to seperate each numeric variable into variables
  #first decide which type of binning
  if (DO_EQUAL_WIDTH==T) {
    #Equal Width Binning, the cut function takes the bumber of bins
    output = cut(num.vector,NO_BINS,include.lowest = T,labels=paste0("EWidth_Level",1:NO_BINS))
}
  else {
    #Equal Frequency Binning
    #the argument break takes specific break points, so quantile is used to find out where to cut the variable
    #the n quantile returns the value below which n% of the data are located
    #thus equal spaced quantiles gives equal percentage of observations in each bin
    #the difficulty lies in automatically adjusting the number of quantiles
    breaks = quantile(num.vector,0:NO_BINS/NO_BINS)
    output = cut(num.vector,breaks,include.lowest = T,right=F,labels=paste0("EFreq_Level",1:NO_BINS))
    }
}
d_EW = sapply(data.num,binning)
d_EW = as.data.frame(d_EW)
save(d_EW, file="datanum_equal_width.Rda")

#optimal binning
library(smbinning)
load("BADS_DATACLEANING_Jan_30.Rda")
data.factor = Filter(is.factor,data)
data.num = Filter(is.numeric,data)
names(data)
result=smbinning(df=data,y="return_customer",x="canceled_items_rate",p=0.05) 
data = smbinning.gen(data, result, chrname = "EO_remitted_items_rate")

#equal frequency
Pct20.Breaks=as.vector(quantile(data.num$remitted_items, probs=seq(0,1,0.5), na.rm=T))
Cuts.Pct20=Pct20.Breaks[2:(length(Pct20.Breaks)-1)]
result= smbinning.custom(df=data,y="return_customer",x="hardware_count",cuts=Cuts.Pct20) # Run and save
data = smbinning.gen(data, result, chrname = "EF_hardware_count")

#delete useless variables
data$yearly_order_frequency = NULL
data$postcode_invoice_rank = NULL
data$agg_3 = NULL

#save file
data.EOEQ = data[,97:127]
save(data.EOEQ, file="data_opt_equal_frequency.Rda")

#optimal binning graph
result=smbinning(df=data,y="return_customer",x="weight",p=0.05) 
result$ivtable
result
par(mfrow=c(2,2)) 
boxplot(data$weight~data$return_customer, 
        horizontal=T, frame=F, col="lightblue",main="Weight Distribution")#weight is not a good predictor
smbinning.plot(result,option="dist") 
smbinning.plot(result,option="badrate") 
smbinning.plot(result,option="WoE")

result=smbinning(df=train,y="return_customer",x="form_of_address",p=0.05) 
names(data)
table(data$EO_monthly_order_frequency)
levels(data$EO_monthly_order_frequency)

##################    counting features of categorical variables   ##################### 
#extract binning group
extract_bin = function(num.vector){
 as.numeric(substr(as.character(num.vector),1,2))
}
unique(dd[-32])
dd = sapply(data.EOEQ,extract_bin)
dd = as.data.frame(dd)
max(dd,na.rm = T)# group 1 to group 6
dd$one_occurance <- apply(dd, 1, function(x) sum(x==1))
dd$two_occurance <- apply(dd, 1, function(x) sum(x==2))
dd$three_occurance <- apply(dd, 1, function(x) sum(x==3))
dd$four_occurance <- apply(dd, 1, function(x) sum(x==4))
dd$five_occurance <- apply(dd, 1, function(x) sum(x==5))
dd$six_occurance <- apply(dd, 1, function(x) sum(x==6))

xx = cbind(data,dd[32:37])
data = xx
save(data, file="BADS_DATACLEANING_jan_30.Rda")

#merge 
load("numrank.Rda")
load("BADS_DATACLEANING_Jan_30.Rda")
data = cbind(data,numrank)

##################             one hot encoding       ##################### 
data =data_maxmin_inter
data.factor = Filter(is.factor,data)
data.num = Filter(is.numeric,data)
data.jj = lapply(data.num, as.numeric)
data.num = as.data.frame(data.jj)
names(data.num)
names(data.factor)
library(dummies)
data_dummy = dummy.data.frame(data.factor)
dim(data_dummy)
dd = cbind(data.num,data_dummy)
save(dd, file="badsindummy.Rda")
names(dd)
dd$return_customer

################################# scaling ##########################
#scaling by max-min
minmaxscaler = function(x){ return((x - min(x))/(max(x) - min(x)))}
d.scaling.maxmin = as.data.frame(lapply(data.num[-c(1,19)],minmaxscaler))
data.maxmin = cbind(data$return_customer,d.scaling.maxmin,data.factor)
colnames(data.maxmin)[1] = "return_customer"
save(data.maxmin,file="BADS_data.maxmin_Jan_30.Rda")

#scaling by nominalzation
library(caret)
normalization <- preProcess(data.num[-c(1,19)], method=c("center", "scale")) 
d.normalization <- predict(normalization, data.num[-c(1,19)])
data.nor = cbind(data$return_customer,d.normalization,data.factor)
colnames(data.nor)[1] = "return_customer"
save(data.nor,file="BADS_data.nor_Jan_30.Rda")

########################## interactions ###########################################

load("BADS_data.maxmin_Jan_30.Rda")
data.maxmin = data.maxmin[, sapply(data.maxmin, nlevels) < 30]
save(data.maxmin, file="BADS_data.maxmin_Jan_30.Rda")
str(data.maxmin)
names(data.maxmin)
data = data.maxmin
rm(data.maxmin)
names(data)
data.factor = Filter(is.factor,data)
xx = data[,c("EO_daily_order_frequency","EO_weight",
             "form_of_address","cost_shipping","EO_avg_goods_value",
             "newsletter","EO_product_val_diff_by_day","EO_monthly_order_frequency",
             "delivery","EO_days_est_deliver","EO_basket_frequency",
             "coupon","model","referrer")]
BADS_Interaction_1 = as.data.frame(model.matrix(~.^2,data=xx))
BADS_Interaction_1$`(Intercept)`= NULL
BADS_Interaction_1 = lapply(BADS_Interaction_1, as.factor)
data.maxmin = as.data.frame(BADS_Interaction_1) 
data.maxmin = data.maxmin[, sapply(data.maxmin, nlevels) > 1]
data.maxmin = data.maxmin[, sapply(data.maxmin, nlevels) < 10]

colnames(data.maxmin) <- sub(" > ","lt",colnames(data.maxmin))
colnames(data.maxmin) <- sub(" <= ","se",colnames(data.maxmin))
colnames(data.maxmin) <- sub(" > ","lt",colnames(data.maxmin))
colnames(data.maxmin) <- sub(":","",colnames(data.maxmin))
names(data.maxmin)

data_maxmin_inter = cbind(data,data.maxmin)
save(data_maxmin_inter,file="data_maxmin_inter.Rda")
##return.costs.test.mean=1.89,f1.test.mean=0.0466
#interaction terms are not incorporated


###########cleaning interaction terms

#chi square test
load("BADS_data.maxmin_Jan_30.Rda")
data.maxmin = data.maxmin[, sapply(data.maxmin, nlevels) < 30]
save(data.maxmin, file="BADS_data.maxmin_Jan_30.Rda")
str(data.maxmin)
names(data.maxmin)
data = data.maxmin
rm(data.maxmin)
names(data)
data.factor = Filter(is.factor,data)
xx = data[,c("EO_daily_order_frequency","EO_weight",
             "form_of_address","cost_shipping","EO_avg_goods_value",
             "newsletter","EO_product_val_diff_by_day","EO_monthly_order_frequency",
             "delivery","EO_days_est_deliver","EO_basket_frequency",
             "coupon","model","referrer")]
BADS_Interaction_1 = as.data.frame(model.matrix(~.^2,data=xx))
BADS_Interaction_1$`(Intercept)`= NULL
BADS_Interaction_1 = lapply(BADS_Interaction_1, as.factor)
data.maxmin = as.data.frame(BADS_Interaction_1) 
data.maxmin = data.maxmin[, sapply(data.maxmin, nlevels) > 1]
data.maxmin = data.maxmin[, sapply(data.maxmin, nlevels) < 10]
data.maxmin = dd
colnames(data.maxmin) <- sub(" > ","lt",colnames(data.maxmin))
colnames(data.maxmin) <- sub(" <= ","se",colnames(data.maxmin))
colnames(data.maxmin) <- sub(" > ","lt",colnames(data.maxmin))
colnames(data.maxmin) <- sub(":","",colnames(data.maxmin))
colnames(data.maxmin) <- sub("product_val_diff_by_day","pvdbd",colnames(data.maxmin))
colnames(data.maxmin) <- sub("monthly_order_frequency","mof",colnames(data.maxmin))
colnames(data.maxmin) <- sub("basket_frequency","bf",colnames(data.maxmin))
colnames(data.maxmin) <- sub("avg_goods_value","agv",colnames(data.maxmin))
colnames(data.maxmin) <- sub(" Year","Year",colnames(data.maxmin))
colnames(data.maxmin) <- sub("_","",colnames(data.maxmin))
colnames(data.maxmin) <- gsub(" ","_",colnames(data.maxmin))
colnames(data.maxmin) <- gsub("-","_",colnames(data.maxmin))

colnames(data.maxmin)[46] <- "sixoccreal"
colnames(data.maxmin)[14] <- "film_count"
table(duplicated(colnames(data.maxmin)))
names(data.maxmin)
head(names(data.maxmin),50)

data_maxmin_inter = cbind(data,data.maxmin)
save(data.maxmin,file="dummy_cleaned")
load("dummy_cleaned")



###########################
#optimal case 2

dd = cbind(data_maxmin_inter,numrank)
dim(dd)#64855  1050
numrank$Rank_return_customer = NULL
save(numrank,file="numrank.Rda")
data_maxmin_inter = dd
data_maxmin_inter$Rank_return_customer = NULL
known = data_maxmin_inter[!is.na(data_maxmin_inter$return_customer),]
unknown = data_maxmin_inter[is.na(data_maxmin_inter$return_customer),]
set.seed(1008)
inTrain = createDataPartition(known$return_customer,p=0.85,list = F)
train = known[inTrain,]
validation = known[-inTrain,]
dim(validation_sparse_matrix)
train_output_vector = as.numeric(train$return_customer)
train_sparse_matrix = sparse.model.matrix(return_customer~.-1,data=train)

validation_output_vector = as.numeric(validation$return_customer)
validation_sparse_matrix = sparse.model.matrix(return_customer~.-1,data=validation)
#strategy 1: scale_pos_weight + auc
params <- list(
  max.depth = 10,
  eta = 0.1,
  subsample = 0.5,
  colsample_bytree = 0.7,
  #  gamma = 0.1,
  scale_pos_weight = 50,
  lambda = 5000,
  alpha = 300,
  min_child_weight = 2000,
  max_delta_step = 50,
  eval_metric = "auc",
  objective = 'binary:logistic'
)
dataTrain <- xgb.DMatrix(train_sparse_matrix, label = train_output_vector)
model.dt <- xgb.train(dataTrain,params = params, nrounds = 7000)
pred_validation =  predict(model.dt,validation_sparse_matrix)
#predict validation
auc(validation$return_customer,pred_validation)#0.5851 auc is too low, weight is 6, 0.5862 7. 0.5857 8.
length(pred_validation)
length(validation$return_customer)

#50 weight,50 max_delta_step, subsample = 0.5,colsam 0.7, minschild 2000,max.de12,max_delt50,lama5000,alpha300
#Area under the curve: 0.6449,objective = 'rank:pairwise'
pred_validation_class = as.numeric(pred_validation>0.9)
table(pred_validation_class)
y = factor(validation$return_customer)
table(y,pred_validation_class)
confusionMatrix(data=pred_validation_class,y)
#predict and save unknown
# xgb_dtree <- xgb.model.dt.tree(feature_names = dataTrain_names, model = model1)
xgb_importance <- xgb.importance(feature_names = colnames(train_sparse_matrix), model = model.dt)
head(xgb_importance,50)
xgb.plot.importance(xgb_importance)
xgb.
x = xgb_importance$Feature[[2]]
x
xd = data_maxmin_inter[]
d = gsub(" ",",", x)
class(d)
j =as.vector(xgb_importance$Feature)
xgb.importance(train_sparse_matrix@Dimnames[[2]])
print(j)
unknown$return_customer = NULL
test_sparse_matrix = sparse.model.matrix(~.,data=unknown)
dim(validation_sparse_matrix)

### interaction terms based on business study #################
#12 possibilities --- No.1,35 cases

data$cou_mo_ref[(data$referrer==1)&(data$model==1)&(data$coupon==0)] = "referrer_1_NoCou"
table(data$cou_mo_ref)


#12 possibilities --- No.2,6 cases

data$cou_mo_ref[(data$referrer==1)&(data$model==1)&(data$coupon==1)] = "referrer_1_Cou"
table(data$cou_mo_ref)

#12 possibilities --- No.3, 4891 cases

data$cou_mo_ref[(data$referrer==1)&(data$model==2)&(data$coupon==1)] = "referrer_2_Cou"
table(data$cou_mo_ref)

#12 possibilities --- No.4, 7693 cases
data$cou_mo_ref[(data$referrer==1)&(data$model==2)&(data$coupon==0)] = "referrer_2_NoCou"
table(data$cou_mo_ref)
#12 possibilities --- No.5, 13787 cases
data$cou_mo_ref[(data$referrer==1)&(data$model==3)&(data$coupon==0)] = "referrer_3_NoCou"
table(data$cou_mo_ref)

#12 possibilities --- No.6, 525 cases
data$cou_mo_ref[(data$referrer==1)&(data$model==3)&(data$coupon==1)] = "referrer_3_Cou"
table(data$cou_mo_ref)

#12 possibilities --- No.7, 3 cases
data$cou_mo_ref[(data$referrer==0)&(data$model==3)&(data$coupon==1)] = "Noreferrer_3_Cou"
table(data$cou_mo_ref)

#12 possibilities --- No.8, 373  cases
data$cou_mo_ref[(data$referrer==0)&(data$model==3)&(data$coupon==0)] = "Noreferrer_3_NoCou"
table(data$cou_mo_ref)

#12 possibilities --- No.9, 8  cases
data$cou_mo_ref[(data$referrer==0)&(data$model==2)&(data$coupon==0)] = "Noreferrer_2_NoCou"
table(data$cou_mo_ref)

#12 possibilities --- No.10, 6  cases
data$cou_mo_ref[(data$referrer==0)&(data$model==2)&(data$coupon==1)] = "Noreferrer_2_Cou"
table(data$cou_mo_ref)

#12 possibilities --- No.11, 5113  cases
data$cou_mo_ref[(data$referrer==0)&(data$model==1)&(data$coupon==1)] = "Noreferrer_1_Cou"
table(data$cou_mo_ref)

#12 possibilities --- No.12, 32413  cases (main strem)
data$cou_mo_ref[(data$referrer==0)&(data$model==1)&(data$coupon==0)] = "Noreferrer_1_NoCou"
data$cou_mo_ref = as.factor(data$cou_mo_ref)

data[(data$cou_mo_ref=="referrer_1_NoCou"),]$cou_mo_ref = "minority"

library(smbinning)
result=smbinning.factor(df=data,y="return_customer",x="cou_mo_ref") # Run and save
result
data=smbinning.factor.gen(data, result, chrname = "EO_cou_mo_ref")
names(data)
data$cou_mo_ref = NULL

################## nosiy_observation ###############
load("BADS_DATACLEANING_Jan_30.Rda")
data$nosiy_observation = 0
data[data$all_product_count ==0,]$nosiy_observation = 1
data[data$account_creation_date_missing==1,]$nosiy_observation = 1
data[data$de_est_date_year_4746==1,]$nosiy_observation = 1
data$noisy_observation = data$nosiy_observation
data$nosiy_observation = NULL
data$weight_missing = NULL
data$de_est_date_year_4746 = NULL
table(data$nosiy_observation)
names(data.maxmin)
save(data,file="BADS_Feb_01.Rda")


###################################################
#                                                 #
#                  Feature Selection              #
#                                                 #
###################################################

########################## drop Date type varaibles    ########################
#delete date variable
load("BADS_DATACLEANING_Jan_30.Rda")
str(data)
data$de_est_date_as_date = NULL
data$order_date_as_date = NULL
data$acc_cre_date_as_date = NULL
data$de_act_date_as_date = NULL

data$order_date = NULL
data$account_creation_date = NULL
data$deliverydate_actual = NULL
data$deliverydate_estimated = NULL
#drop weight counting
data$weight_missing=NULL
data$account_creation_date_missing=NULL
save(data, file = "BADS_Feb_01.Rda")
nrow(data[(0<data$weight)&(50>data$weight),])
names(data)
##################################  Filter #################################

##############################      Information Value    ###########################
#http://www.scoringmodeling.com/rpackage/smbinning/
#http://www.scoringmodeling.com/rpackage/smbinning/smbinningv03.R
# Information Value for all variables in one step ---------------------------
library(smbinning)
IV_featureconstruction= smbinning.sumiv(df=data.maxmin,y="return_customer") # IV for eache variable
save(IV_no_featureconstruction,file="Table of IV Jan_30.Rda")
save(IV_featureconstruction,file="Table of IV Jan_30_2.Rda")
load("Table of IV Jan_30.Rda")
load("Table of IV Jan_30_2.Rda")

# Plot IV for all variables 
sumivt=z
sumivt # Display table with IV by characteristic
par(mfrow=c(1,1))
smbinning.sumiv.plot(sumivt,cex=0.3) # Plot IV summary table

#generate IV table with penalty
library(Information)
library(gridExtra)
library(caret)
load("BADS_data.maxmin_Jan_30.Rda")
data = data.maxmin
data.train = data[!is.na(data$return_customer),]
sapply(data.train, function(x) {sum(is.na(x))})

inTrain = createDataPartition(data.train$form_of_address,p=0.75,list = F)
data.train.train = data.train[inTrain,]
data.train.valid = data.train[-inTrain,]
IV = create_infotables(data= data.train.train,
                       valid= data.train.valid,
                       y="return_customer")
IV_grid = grid.table(head(IV$Summary,20),rows=NULL)

############################## relative importance ######################

library(relaimpo)
lmMod <- lm(return_customer ~ . , data = train)  # fit lm() model
relImportance <- calc.relimp(lmMod, rela = TRUE)  # calculate relative importance scaled to 100
#Error: cannot allocate vector of size 775.5 Mb
sort(relImportance$lmg, decreasing=TRUE)  # relative importance


############# Person Correlation 
library(mlbench)
library(caret)
train = data.maxmin[!is.na(data.maxmin$return_customer),]
class(data.maxmin$return_customer) = as.numeric(data.maxmin$return_customer)
data.maxmin.n = Filter(is.numeric,train)
correlationMatrix = cor(data.maxmin.n)
#delete highlycorrelated, tau is 75%
highlycorrelated = findCorrelation(correlationMatrix,cutoff = 0.75)
print(highlycorrelated)
class(highlycorrelated)
data.maxmin$avg_goods_value=NULL
plot(correlationMatrix)

######## chi square and information gain
save(data.maxmin,file="BADS_data.maxmin_FS_Feb_02.Rda")
data.maxmin$Rank_return_customer = NULL
data=data.maxmin
dim(data.maxmin) 
known = data.maxmin[!is.na(data.maxmin$return_customer),]
unknown = data.maxmin[is.na(data.maxmin$return_customer),]
unknown$return_customer = NULL
set.seed(6)
library(caret)
inTrain = createDataPartition(known$return_customer,p = 0.8,list=F)
train = known[inTrain,]
validation = known[-inTrain,]
prop.table(table(validation$return_customer))
train$return_customer = factor(train$return_customer,labels = c("Not Return","Return"))
table(train$return_customer)
train = as.data.frame(train)

library(mlr)
return.task = makeClassifTask(data = train, target = "return_customer",positive = "Return")
fv = generateFilterValuesData(return.task, method = "information.gain")
fv$data
plotFilterValues(fv2)
fv2 = generateFilterValuesData(return.task, method = c("information.gain", "chi.squared"))
class(fv2$data)
dd =fv$data[fv$data$information.gain<=0.0005,]
data.maxmin$digital_product_count = NULL

plotFilterValues(fv)

install.packages("randomForestSRC")
fv3 = generateFilterValuesData(return.task, method = "mrmr")
fv$data
??generateFilterValuesData
fv3 = as.data.frame(fv2$data)

## Selecting a feature subset #
#Keep a certain absolute number (abs) of features with highest importance.
#Keep a certain percentage (perc) of features with highest importance.
#Keep all features whose importance exceeds a certain threshold value (threshold).
filtered.task = filterFeatures(return.task, fval = fv, perc = 0.75)

#experiment in LR
known = data.maxmin[!is.na(data.maxmin$return_customer),]
library(caret)
inTrain = createDataPartition(known$form_of_address,p=0.75,list = F)
train = known[inTrain,]
val = known[-inTrain,]
set.seed(1)
th = 3/13
table(train$return_customer)
train$return_customer = factor(train$return_customer,labels = c("Not Return","Return"))
table(train$return_customer)
names(train)
train = as.data.frame(train)
library(mlr)
return.task = makeClassifTask(data = train, target = "return_customer",positive = "Return")
#return.task = removeConstantFeatures(return.task)
class(return.task)
costs = matrix(c(0,3,10,0),2)
colnames(costs) = rownames(costs) = c("Return","Not Return")
costs
th = 3/(10+3)
#average cost dropped 
return.cost = makeCostMeasure(id = "return.costs", name = "Return Costs", costs = costs,
                              best = 0, worst = 10)
rin = makeResampleInstance("CV", iters = 3, task = return.task)
lrn = makeLearner("classif.multinom", predict.type = "prob", predict.threshold = th, trace = FALSE)
auc = resample(lrn, filtered.task, resampling = rin, measures = list(return.cost, auc), show.info = FALSE)
auc#1.65,0.65  vs. benchmark 1.68 0.33

#gbm
lrn = makeLearner("classif.gbm")
lrn.gbm.w = makeWeightedClassesWrapper(lrn, wcw.weight = 4)
r = resample(lrn.gbm.w, return.task, rin, measures = list(return.cost, f1), show.info = FALSE)
r
#
lrn = makeFilterWrapper(learner = "classif.gbm", fw.method = "information.gain", fw.abs = 20)
rdesc = makeResampleDesc("CV", iters = 5)
r = resample(learner = lrn, task = return.task, resampling = rdesc, show.info = FALSE, models = TRUE)
r$aggr
sfeats = sapply(r$models, getFilteredFeatures)
table(sfeats)
#to see if the selection of features seems to be very stable??

######## Tuning the size of the feature subset
#tuning optimal percentage value for feature selection


#### feature selection edition 1 ##############
xgb_importance$Feature
data_FS_FEB_01 = data_maxmin_inter[,c("form_of_address","newsletter","EF_remitted_items_rate",
                                      "EO_real_item_in_basket",  "physical_product_count", "EO_all_product_count",
                                      "cost_shipping","EO_daily_order_frequency02....310.newsletter1","newsletter1.EO_product_val_diff_by_day02....0.9167",
                                      "basket_diversity_fine_grained","weight","monthly_order_frequency",
                                      "delivery","pro_val_diff_by_day_mon_ave","avg_goods_value","referrer","daily_order_frequency",
                                      "one_occurance","na_count","basket_frequency","three_occurance","five_occurance","four_occurance","six_occurance","two_occurance")]
data_FS_FEB_01$return_customer = data_maxmin_inter$return_customer
names(data_FS_FEB_01)

load("BADS_data.maxmin_Jan_30.Rda")

########################         Wrapper methods          ##################
#Select a feature subset

#############wrapper 1: exhaustive search 
#Learning method: LR
#The performance is assessed by the holdout estimate of the concordance index 
ctrl = makeFeatSelControlRandom(maxit = 20L)
ctrl
## Resample description
rdesc = makeResampleDesc("Holdout")

#assign class weights to a Learner 
#a suitable weight vector is then generated internally during training or resampling
#using the wrapper 
#specify the weight w for the positive class, negative class will autically receive 1

## Weighted learner for LR
load("BADS_data.maxmin_Jan_30.Rda")
load("data_FS_FEB_01.Rda")
known = data.maxmin[!is.na(data.maxmin$return_customer),]
unknown = data.maxmin[is.na(data.maxmin$return_customer),]
train = known
table(train$return_customer)
train$return_customer = factor(train$return_customer,labels = c("Not Return","Return"))
table(train$return_customer)
train = as.data.frame(train)
return.task = makeClassifTask(data = train, target = "return_customer",positive = "Return")
return.task = removeConstantFeatures(return.task)
costs = matrix(c(0,3,10,0),2)
colnames(costs)=rownames(costs)  = c("Return","Not Return")
return.cost = makeCostMeasure(id = "return.costs", name = "Return Costs", costs = costs,
                              best = 0, worst = 10)
lrn = makeLearner("classif.multinom", predict.type = "prob",predict.threshold = 0.23,trace = FALSE)
lrn2 = makeWeightedClassesWrapper(lrn, wcw.weight = 3.5)

sfeats = selectFeatures(learner = lrn2, task = return.task, resampling = rdesc,
                        control = ctrl, measures = list(return.cost, f1) ,show.info = FALSE)
??selectFeatures
sfeats$x
sfeats$y #return.costs.test.mean=1.64,f1.test.mean=0.363

######################### wrapper 2: sequential forward search 
#where features are added to the model until the performance cannot be improved anymore
## Specify the search strategy

#learning method: LR
ctrl = makeFeatSelControlSequential(method = "sfs", alpha = 0.02)
## Select features
rdesc = makeResampleDesc("CV", iters = 3)
lrn = makeLearner("classif.multinom", predict.type = "prob",predict.threshold = 0.23,trace = FALSE)
lrn2 = makeWeightedClassesWrapper(lrn, wcw.weight = 3.5)

sfeats = selectFeatures(learner = lrn2, task = return.task, resampling = rdesc,
                        control = ctrl, measures = list(return.cost, f1) ,show.info = FALSE)
#return.costs.test.mean=1.71,f1.test.mean=0.327

#learning method: classif.blackboost
lrn = makeLearner("classif.blackboost", predict.type = "prob")
lrn2 = makeWeightedClassesWrapper(lrn, wcw.weight = 3.5)
sfeats = selectFeatures(lrn2, task = return.task, resampling = rdesc, control = ctrl,
                         measures = list(return.cost, f1),show.info = FALSE)
sfeats#return.costs.test.mean= 1.7,f1.test.mean=0.33,remitted_items, form_of_address, newsletter, cost_shipping
analyzeFeatSelResult(sfeats)

######################### wrapper 3: sequential backward search 
#where features are added to the model until the performance cannot be improved anymore
## Specify the search strategy
#learning method: glmboost
ctrl = makeFeatSelControlSequential(method = "sbs", alpha = 0.02)
lrn = makeLearner("classif.glmboost")
lrn2 = makeWeightedClassesWrapper(lrn, wcw.weight = 4)
sfeats = selectFeatures(lrn2, task = return.task, resampling = rdesc, control = ctrl,
                        measures = list(return.cost, f1),show.info = FALSE)
sfeats#return.costs.test.mean= 1.7,f1.test.mean=0.33,remitted_items, form_of_address, newsletter, cost_shipping
analyzeFeatSelResult(sfeats)
#return.costs.test.mean=1.89,f1.test.mean=0.0466


### Fuse a learner with feature selection ###
## Specify the search strategy
ctrl = makeFeatSelControlSequential(method = "sfs", alpha = 0.02)

## Select features
rdesc = makeResampleDesc("CV", iters = 10)
sfeats = selectFeatures(learner = "regr.lm", task = bh.task, resampling = rdesc, control = ctrl,
                        show.info = FALSE)
sfeats


names(data)


#################### feature selction edition 2 ###################
save(data,file = "BADS_Feb_01.Rda")
data_FS_FEB_01 = data
data_FS_FEB_01$item_count = NULL


data_FS_FEB_01 = data_FS_FEB_01[-c(25:35)]
data_FS_FEB_01$ID = NULL

names(data_FS_FEB_01)
data$acc_cre_date_day = as.numeric(data$acc_cre_date_day)
result=smbinning(df=data,y="return_customer",x="acc_cre_date_day",p=0.05) # Run and save
result
data_FS_FEB_01=smbinning.gen(data_FS_FEB_01, result, chrname = "EO_account_creation_date")
data_FS_FEB_01$equal_postcode = NULL
fa = Filter(is.factor,data_FS_FEB_01)
library(smbinning)
#### numeric EO
names(data_FS_FEB_01)
data$order_date_day = as.numeric(data$order_date_day)
result=smbinning(df=data,y="return_customer",x="canceled_items_rate",p=0.1) # Run and save
result
data=smbinning.gen(data, result, chrname = "EO_order_date_month")
names(data_FS_FEB_01)
data_FS_FEB_01$EO_order_date_month = data$EO_order_date_month
data_FS_FEB_01$order_date_day = NULL

table(data_FS_FEB_01$used_items_rate_rank)
names(data_FS_FEB_01)
table(data_FS_FEB_01$remitted_items)
unique(data_FS_FEB_01$used_items_rate)
#equal frequency
Pct20.Breaks=as.vector(quantile(data_FS_FEB_01$postcode_delivery, probs=seq(0,1,0.1), na.rm=T))
Cuts.Pct20=Pct20.Breaks[2:(length(Pct20.Breaks)-1)]
result= smbinning.custom(df=data_FS_FEB_01,y="return_customer",x="postcode_delivery",cuts=Cuts.Pct20) # Run and save
data_FS_FEB_01 = smbinning.gen(data_FS_FEB_01, result, chrname = "EF_postcode_invoice")

######## factor EO
data_FS_FEB_01 = as.data.frame(data_FS_FEB_01)
data = data_FS_FEB_01
data_FS_FEB_01$advertising_code_group = NULL
class(data_FS_FEB_01$postcode_invoice)
data_FS_FEB_01$order_date_month = as.factor(data_FS_FEB_01$order_date_month)
result=smbinning.factor(df=data,y="return_customer",x="basket_diversity_fine_grained") # Run and save
result
data=smbinning.gen(data, result, chrname = "EO_order_date_weekday")
names(data_FS_FEB_01)
data_FS_FEB_01.num = data.factor = Filter(is.numeric,data_FS_FEB_01)
data_FS_FEB_01$acc_cre_date_weekday = NULL
data_FS_FEB_01$cancel_more_than_records = NULL
data_FS_FEB_01$x = NULL

names(data)
names(data_FS_FEB_01)
save(data_FS_FEB_01, file = "data_FS_FEB_01.Rda")
data_FS_FEB_01_rank = cbind(data_FS_FEB_01,numrank)
save(data, file = "data_FEB_01.Rda")
data_FS_FEB_01_rank.num = Filter(is.numeric,data_FS_FEB_01_rank)
data_FS_FEB_01_rank.fac = Filter(is.factor,data_FS_FEB_01_rank)

names(data_FS_FEB_01_rank.num)
#scaling by max-min
minmaxscaler = function(x){ return((x - min(x))/(max(x) - min(x)))}
d.scaling.maxmin = as.data.frame(lapply(data_FS_FEB_01_rank.num[-1],minmaxscaler))
data.maxmin = cbind(data_FS_FEB_01_rank$return_customer,d.scaling.maxmin,data_FS_FEB_01_rank.fac)
colnames(data.maxmin)[1] = "return_customer"
save(data.maxmin,file="BADS_data.maxmin_Feb_01.Rda")

colnames(data.maxmin) <- sub(" > ","lt",colnames(data.maxmin))
colnames(data.maxmin) <- sub(" <= ","se",colnames(data.maxmin))
colnames(data.maxmin) <- sub(" > ","lt",colnames(data.maxmin))
colnames(data.maxmin) <- sub(":","",colnames(data.maxmin))
str(data.maxmin)
################################### XGBOOST for variable selection #################
require(caret)
require(xgboost)
require(Matrix)
require(data.table)
require(vcd)
load("BADS_data.maxmin_FS_Feb_02.Rda")
known = data.maxmin[!is.na(data.maxmin$return_customer),]
unknown = data.maxmin[is.na(data.maxmin$return_customer),]
unknown$return_customer = NULL
train = known
#create sparse matrix
train_output_vector = as.numeric(train$return_customer)
train_sparse_matrix = sparse.model.matrix(return_customer~.-1,data=train)

#strategy 1: original data.maxmin (3 kinds included)

params <- list(
  max.depth = 6,
  eta = 0.8,
  subsample = 0.7,
  colsample_bytree = 0.6,
  gamma = 0.1,
  scale_pos_weight = 700,
  lambda = 50000,
  #  alpha = 100,
  min_child_weight = 0,
  max_delta_step = 1000,
  objective = 'binary:logistic',
  eval_metric = "auc",
  nthread = 7
)
xgb_cv_1 = xgb.cv(params = params,
                  data = train_sparse_matrix,
                  label = train_output_vector,
                  nrounds = 3000, 
                  nfold = 5,                                                   # number of folds in K-fold
                  prediction = TRUE,                                           # return the prediction using the final model 
                  showsd = TRUE,                                               # standard deviation of loss across folds
                  stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                  verbose = TRUE,
                  print.every.n = 1, 
                  early.stop.round = 200)
xgb_cv_1#0.6585164 in no rank #0.6567108 #0.6558058


#strategy 2: factor and rank from orignial data.maxmin
data.maxmin.factor = Filter(is.factor,data.maxmin)
data.maxmin.factor$return_customer = data.maxmin$return_customer
data.maxmin.rank = data.maxmin[,c("Rank_remitted_items","Rank_weight","Rank_paperback_count"
                                  ,"Rank_ebook_count","Rank_paperback_count","Rank_ebook_count"
                                  ,"Rank_audiobook_download_count","Rank_na_count","Rank_days_first_order_occurs",
                                  "Rank_avg_goods_value","Rank_digital_product_count","Rank_real_item_in_basket")]

data.combine = cbind(data.maxmin.factor,data.maxmin.rank)
names(data.combine)
data.maxmin = data.combine
known = data.maxmin[!is.na(data.maxmin$return_customer),]
unknown = data.maxmin[is.na(data.maxmin$return_customer),]
unknown$return_customer = NULL
train = known
#create sparse matrix
train_output_vector = as.numeric(train$return_customer)
train_sparse_matrix = sparse.model.matrix(return_customer~.-1,data=train)
params <- list(
  max.depth = 6,
  eta = 0.8,
  subsample = 0.7,
  colsample_bytree = 0.6,
  gamma = 0.1,
  scale_pos_weight = 7,
  lambda = 50000,
  #  alpha = 100,
  min_child_weight = 0,
  max_delta_step = 100,
  objective = 'binary:logistic',
  eval_metric = "auc",
  nthread = 7
)
xgb_cv_1 = xgb.cv(params = params,
                  data = train_sparse_matrix,
                  label = train_output_vector,
                  nrounds = 3000, 
                  nfold = 5,                                                   # number of folds in K-fold
                  prediction = TRUE,                                           # return the prediction using the final model 
                  showsd = TRUE,                                               # standard deviation of loss across folds
                  stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                  verbose = TRUE,
                  print.every.n = 1, 
                  early.stop.round = 200)
xgb_cv_1#0.657285
dataTrain <- xgb.DMatrix(train_sparse_matrix, label = train_output_vector)
model.dt <- xgb.train(dataTrain, params = params, nrounds = 7000)
# xgb_dtree <- xgb.model.dt.tree(feature_names = dataTrain_names, model = model1)
xgb_importance <- xgb.importance(feature_names = colnames(train_sparse_matrix), model = model.dt)
head(xgb_importance,20)
xgb.plot.importance(xgb_importance[1:50,])

#strategy 3: factor from orignial data.maxmin only
data.maxmin.factor = Filter(is.factor,data.maxmin)
data.maxmin.factor$return_customer = data.maxmin$return_customer
data.maxmin = data.maxmin.factor
known = data.maxmin[!is.na(data.maxmin$return_customer),]
unknown = data.maxmin[is.na(data.maxmin$return_customer),]
unknown$return_customer = NULL
train = known
#create sparse matrix
train_output_vector = as.numeric(train$return_customer)
train_sparse_matrix = sparse.model.matrix(return_customer~.-1,data=train)
params <- list(
  max.depth = 8,
  eta = 0.8,
  subsample = 0.7,
  colsample_bytree = 0.9,
  gamma = 0.1,
  scale_pos_weight = 80,
  lambda = 50000,
  #  alpha = 100,
  min_child_weight = 1000,
  max_delta_step = 2000,
  objective = 'binary:logistic',
  eval_metric = "auc",
  nthread = 7
)
xgb_cv_1 = xgb.cv(params = params,
                  data = train_sparse_matrix,
                  label = train_output_vector,
                  nrounds = 3000, 
                  nfold = 5,                                                   # number of folds in K-fold
                  prediction = TRUE,                                           # return the prediction using the final model 
                  showsd = TRUE,                                               # standard deviation of loss across folds
                  stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                  verbose = TRUE,
                  print.every.n = 1, 
                  early.stop.round = 200)
xgb_cv_1#0.6536394
dataTrain <- xgb.DMatrix(train_sparse_matrix, label = train_output_vector)
model.dt <- xgb.train(dataTrain, params = params, nrounds = 7000)
# xgb_dtree <- xgb.model.dt.tree(feature_names = dataTrain_names, model = model1)
xgb_importance <- xgb.importance(feature_names = colnames(train_sparse_matrix), model = model.dt)
head(xgb_importance,20)
xgb.plot.importance(xgb_importance[1:50,])

#strategy 4: factor and rank and categorical variable counts
data.maxmin.factor = Filter(is.factor,data.maxmin)
data.maxmin.factor$return_customer = data.maxmin$return_customer
data.maxmin.count = data[,c("one_occurance","two_occurance","three_occurance"
                                   ,"four_occurance","five_occurance","six_occurance")]

data.combine = cbind(data.combine,data.maxmin.count)
names(data.combine)
data.maxmin = data.combine
known = data.maxmin[!is.na(data.maxmin$return_customer),]
unknown = data.maxmin[is.na(data.maxmin$return_customer),]
unknown$return_customer = NULL
train = known
#create sparse matrix
train_output_vector = as.numeric(train$return_customer)
train_sparse_matrix = sparse.model.matrix(return_customer~.-1,data=train)
params <- list(
  max.depth = 6,
  eta = 0.8,
  subsample = 0.7,
  colsample_bytree = 0.6,
  gamma = 0.1,
  scale_pos_weight = 7,
  lambda = 50000,
  #  alpha = 100,
  min_child_weight = 0,
  max_delta_step = 100,
  objective = 'binary:logistic',
  eval_metric = "auc",
  nthread = 7
)
xgb_cv_1 = xgb.cv(params = params,
                  data = train_sparse_matrix,
                  label = train_output_vector,
                  nrounds = 3000, 
                  nfold = 5,                                                   # number of folds in K-fold
                  prediction = TRUE,                                           # return the prediction using the final model 
                  showsd = TRUE,                                               # standard deviation of loss across folds
                  stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                  verbose = TRUE,
                  print.every.n = 1, 
                  early.stop.round = 200)
xgb_cv_1#0.6590412

#use only very important variables from XGBoost Strategy 2
data.maxmin.factor = Filter(is.factor,data.maxmin)
data.maxmin.factor$return_customer = data.maxmin$return_customer
data.optimal= data.combine[,c("Rank_weight","Rank_avg_goods_value","one_occurance"
                                  ,"two_occurance","three_occurance","Rank_days_first_order_occurs"
                                  ,"Rank_remitted_items","cost_shipping","five_occurance",
                                  "newsletter","four_occurance","Rank_paperback_count",
                                  "six_occurance","delivery","remitted_items_level","return_customer")]

data.maxmin = data.optimal
known = data.maxmin[!is.na(data.maxmin$return_customer),]
unknown = data.maxmin[is.na(data.maxmin$return_customer),]
unknown$return_customer = NULL
train = known
#create sparse matrix
train_output_vector = as.numeric(train$return_customer)
train_sparse_matrix = sparse.model.matrix(return_customer~.-1,data=train)
params <- list(
  max.depth = 10,
  eta = 0.8,
  subsample = 0.7,
  colsample_bytree = 0.8,
  gamma = 0.1,
  scale_pos_weight = 7,
  lambda = 5,
  #  alpha = 100,
  min_child_weight = 0,
  max_delta_step = 100,
  objective = 'binary:logistic',
  eval_metric = "auc",
  nthread = 7
)
xgb_cv_1 = xgb.cv(params = params,
                  data = train_sparse_matrix,
                  label = train_output_vector,
                  nrounds = 3000, 
                  nfold = 5,                                                   # number of folds in K-fold
                  prediction = TRUE,                                           # return the prediction using the final model 
                  showsd = TRUE,                                               # standard deviation of loss across folds
                  stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                  verbose = TRUE,
                  print.every.n = 1, 
                  early.stop.round = 200)
xgb_cv_1#0.586954+
dataTrain <- xgb.DMatrix(train_sparse_matrix, label = train_output_vector)
model.dt <- xgb.train(dataTrain, params = params, nrounds = 7000)
# xgb_dtree <- xgb.model.dt.tree(feature_names = dataTrain_names, model = model1)
xgb_importance <- xgb.importance(feature_names = colnames(train_sparse_matrix), model = model.dt)
head(xgb_importance,20)
xgb.plot.importance(xgb_importance[1:20,])
xgb_importance$Feature
