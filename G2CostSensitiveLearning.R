
#topic1: Cost Sensitive Learning --- thresholding, weighting, resampling
#structure of Logistic Regression: 
#original data for LR Model line  8 - 89
#optimal threshold line 90 - 250
#weighting line 251 - 313
#under/oversampling line 314 - 391

#topic2: Cost Sensitive Learning Classifers---  389 - 451

#topic3: Cost Sensitive Learning --- XGBOOST 453


###################################### Original Data For LR
load("BADS_DATACLEANING_Jan_1.Rda")

names(data)
levels(data$payment) <- c("cash", "invoice","creditcard","currentcount")
levels(data$email_domain)[levels(data$email_domain)=="t-online.de"] <- "t_online.de"
data$goods_value = factor(data$goods_value,ordered = F)
class(data$goods_value)
table(data$email_domain)
save(data, file="BADS_DATACLEANING_Jan_21.Rda")

load("BADS_DATACLEANING_Jan_21.Rda")
data = data[,1:37]
data.numeric = Filter(is.numeric,data)
data.factor = Filter(is.factor,data)
dim(data.factor)
#factor exclude date
data.factor.nonedate = data.factor[,-c(1,5,17,18)]

#date to numeric
date = as.data.frame(lapply(data.factor[,c(1,5,17,18)], as.numeric))

#numeric combine
data.numeric = cbind(data.numeric,date)

#clean numeric
data.numeric$ID = NULL
data.numeric$return_customer = NULL

#scaling numeric variables
#scaling by max-min
minmaxscaler = function(x){ return((x - min(x))/(max(x) - min(x)))}
d.scaling = as.data.frame(lapply(data.numeric,minmaxscaler))
#scaling by nominalzation
library(caret)
normalization <- preProcess(data.numeric, method=c("center", "scale")) 
d.normalization <- predict(normalization, data.numeric)

#dummry varaiables for categorical variables
library(dummies)
data_dummy = dummy.data.frame(data.factor.nonedate)
dim(data_dummy)#194 columns

#combine numeric and factor and target variables
data.combine.LR.maxmin = cbind(d.scaling,data_dummy,data$return_customer)
names(data.combine.LR.maxmin)
colnames(data.combine.LR.maxmin)[220] = "return_customer"
save(data.combine.LR.maxmin,file="data.origin.LR.maxmin.Rda")


data.combine.LR.normal = cbind(d.normalization,data_dummy,data$return_customer)
names(data.combine.LR.normal)
colnames(data.combine.LR.normal)[220] = "return_customer"
save(data.combine.LR.normal,file="data.origin.LR.normal.Rda")
names(data.combine.LR.normal)
#seperate for maxmin
set.seed(6)
known = data.combine.LR.maxmin[1:51884,]
unknown = data.combine.LR.maxmin[51885:64855,]
unknown$return_customer = NULL
library(caret)
inTrain = createDataPartition(known$weight,p = 0.8,list=F)
train = known[inTrain,]
validation = known[-inTrain,]
prop.table(table(validation$return_customer))

save(train,file="train.LR.maxmin.Rda")
save(validation,file="validation.LR.maxmin.Rda")
save(unknown,file="uknown.LR.maxmin.Rda")
names(train)
#seperate for normilnation 
set.seed(7)
known = data.combine.LR.normal[1:51884,]
unknown = data.combine.LR.normal[51885:64855,]
unknown$return_customer = NULL
library(caret)
inTrain = createDataPartition(known$weight,p = 0.8,list=F)
train = known[inTrain,]
validation = known[-inTrain,]
prop.table(table(validation$return_customer))

save(train,file="train.LR.normal.Rda")
save(validation,file="validation.LR.normal.Rda")
save(unknown,file="uknown.LR.normal.Rda")


#################################
#https://mlr-org.github.io/mlr-tutorial/devel/html/cost_sensitive_classif/index.html
#https://webdocs.cs.ualberta.ca/~zaiane/postscript/BiologicallyInspired13.pdf
#https://pdfs.semanticscholar.org/9908/404807bf6b63e05e5345f02bcb23cc739ebd.pdf


############################## train original data on cost sensitive LR

################# tunning optimal threshold

#Class-dependent misclassification costs
#MLR takes missclassification cost into consideration by thresholding and rebalancing

#thresholding: minimize costs by choosing thresholds which turn posterior probabilities into labels
#thresholding requires a classifier that predict postierior probabilities
#during training the cost is not taken into consideration

#rebalancing: accounting costs by changing the proportion of the classes in the trainning set
#two kinds of rebalancing: weighting
#two kinds of rebalancing: over/undersampling

#Binary classification problems

#We start by fitting a logistic regression model to the German credit data set and predict posterior probabilities.
require(mlr)
load("train.LR.normal.Rda")


load("validation.LR.normal.Rda")
load("uknown.LR.normal.Rda")

table(train$return_customer)
train$return_customer = factor(train$return_customer,labels = c("Not Return","Return"))
table(train$return_customer)
names(train)
train = as.data.frame(train)
return.task = makeClassifTask(data = train, target = "return_customer",positive = "Return")
#return.task = removeConstantFeatures(return.task)
class(return.task)
costs = matrix(c(0,3,10,0),2)
colnames(costs) = rownames(costs) = c("Return","Not Return")
costs
th = 3/(10+3)
#average cost
return.cost = makeCostMeasure(id = "return.costs", name = "Return Costs", costs = costs,
                              best = 0, worst = 10)
rin = makeResampleInstance("CV", iters = 5, task = return.task)

#1. Thresholding

#threshold: Not Return=0.50,Return=0.50
names(train)
class(train$return_customer)
lrn = makeLearner("classif.multinom", predict.type = "prob", trace = FALSE)
mod = train(lrn, return.task)

#predict on train dataset
pred = predict(mod, task = return.task)
pred

#theoretical thresholding

pred.th = setThreshold(pred, th)
pred.th

## Performance with default thresholds 0.5
#mmce: the error rate 
#fnr: false negative rate
#https://mlr-org.github.io/mlr-tutorial/devel/html/measures/index.html
performance(pred, measures = list(return.cost, fnr))#0.9881423
performance(pred, measures = list(return.cost, tnr))#0.9968514
performance(pred, measures = list(return.cost, tpr))#0.01185771
performance(pred, measures = list(return.cost, fpr))#0.003148577
performance(pred, measures = list(return.cost, f1))#0.02312857
performance(pred, measures = list(return.cost, mmce))#1.8747260    0.1892602

## Performance with theoretical thresholds
performance(pred.th, measures = list(return.cost, fnr))#0.6162183
performance(pred.th, measures = list(return.cost, tnr))#0.7973623
performance(pred.th, measures = list(return.cost, tpr))#0.3837817
performance(pred.th, measures = list(return.cost, fpr))#0.2026377
performance(pred.th, measures = list(return.cost, f1))#0.3405941
performance(pred.th, measures = list(return.cost, mmce))#1.6573755    0.2807825 

## Cross-validated performance with theoretical thresholds
set.seed(1)
rin = makeResampleInstance("CV", iters = 5, task = return.task)
lrn = makeLearner("classif.multinom", predict.type = "prob", predict.threshold = th, trace = FALSE)
f1 = resample(lrn, return.task, resampling = rin, measures = list(return.cost, f1), show.info = FALSE)
f1#1.68 0.33

fnr = resample(lrn, return.task, resampling = rin, measures = list(return.cost, fnr), show.info = T)
fnr#1.68 0.63

tpr = resample(lrn, return.task, resampling = rin, measures = list(return.cost, tpr), show.info = T)
tpr#1.68 0.375 (precision)

fpr = resample(lrn, return.task, resampling = rin, measures = list(return.cost, fpr), show.info = T)
fpr#1.68 0.206

#draw plot for performace measure, return cost is the expected misclassification cost
d = generateThreshVsPerfData(f1, measures = list(return.cost, f1))
plotThreshVsPerf(d, mark.th = th)


## Cross-validated performance with default thresholds
set.seed(1)
lrn = makeLearner("classif.multinom", predict.type = "prob", predict.threshold = 0.5, trace = FALSE)
f1 = resample(lrn, return.task, resampling = rin, measures = list(return.cost, f1), show.info = FALSE)
f1#1.88 0.02

fnr = resample(lrn, return.task, resampling = rin, measures = list(return.cost, fnr), show.info = FALSE)
fnr#1.88 0.99

tpr = resample(lrn, return.task, resampling = rin, measures = list(return.cost, tpr), show.info = T)
tpr#1.88 0.00407

fpr = resample(lrn, return.task, resampling = rin, measures = list(return.cost, fpr), show.info = T)
fpr#1.88 

#draw plot for performace measure, return cost is the expected misclassification cost
d = generateThreshVsPerfData(f1, measures = list(return.cost, mmce))
plotThreshVsPerf(d, mark.th = th)


#ii. Empirical thresholding
#select cost-optimal threshold values for a given learning method based on the training data
lrn = makeLearner("classif.multinom", predict.type = "prob", trace = FALSE)

## 5-fold cross-validation
f1 = resample(lrn, return.task, resampling = rin, measures = list(return.cost,f1), show.info = FALSE)
f1#

tune.res = tuneThreshold(pred = f1$pred, measure = return.cost)
tune.res# th:0.2192419 #return.costs:1.680286

## Cross-validated performance with empirical optimal thresholds
set.seed(1)
rin = makeResampleInstance("CV", iters = 5, task = return.task)
lrn = makeLearner("classif.multinom", predict.type = "prob", predict.threshold = 0.2192419, trace = FALSE)
f1 = resample(lrn, return.task, resampling = rin, measures = list(return.cost, f1), show.info = FALSE)
f1#1.69 0.34

fnr = resample(lrn, return.task, resampling = rin, measures = list(return.cost, fnr), show.info = FALSE)
fnr#1.69 0.58

tpr = resample(lrn, return.task, resampling = rin, measures = list(return.cost, tpr), show.info = T)
tpr#1.88 0.42

fpr = resample(lrn, return.task, resampling = rin, measures = list(return.cost, fpr), show.info = T)
fpr#1.69 0.25


################# rebalancing (weighting)
#observations from the less costly class should be given higher importance during training
#so as to minimize the average costs

## Learners that accept observation weights
listLearners("classif", properties = "weights")[c("class", "package")]

## Learners that can deal with class weights
listLearners("classif", properties = "class.weights")[c("class", "package")]

#theoritical weighting
#the positive class the proportion of observations in the positive class has to be multiplied by

w = (1-th)/th
w2 = (1-0.2192419)/0.2192419
#assign class weights to a Learner 
#a suitable weight vector is then generated internally during training or resampling
#using the wrapper 
#specify the weight w for the positive class, negative class will autically receive 1

## Weighted learner for LR
lrn = makeLearner("classif.multinom", predict.type = "prob",trace = FALSE)
lrn = makeWeightedClassesWrapper(lrn, wcw.weight = w)
lrn

lrn2 = makeLearner("classif.multinom",predict.type = "prob", trace = FALSE)
lrn2 = makeWeightedClassesWrapper(lrn2, wcw.weight = w2)
lrn2

lrn3 = makeLearner("classif.multinom",predict.type = "prob", trace = FALSE)
lrn3 = makeWeightedClassesWrapper(lrn3, wcw.weight = 3.5)
lrn3

## 5-fold cross-validation

#w = 3.33, measured by return.cost
r = resample(lrn, return.task, resampling = rin, measures = list(return.cost,f1), show.info = FALSE)
r#return.costs.mean: 1.68 f1.mean: 0.33

#w = 3.56, measured by return.cost and f1
r = resample(lrn2, return.task, resampling = rin, measures = list(return.cost, f1), show.info = FALSE)
r#return.costs.mean: 1.68 f1.mean: 0.34

#w = 3.5, measured by return.cost and f1
r = resample(lrn3, return.task, resampling = rin, measures = list(return.cost, f1), show.info = FALSE)
r#return.costs.mean: 1.68 f1.mean: 0.34

######################## tune weight
lrn = makeLearner("classif.multinom", trace = FALSE)
lrn = makeWeightedClassesWrapper(lrn)
ps = makeParamSet(makeDiscreteParam("wcw.weight", seq(3,8, 0.5)))
ctrl = makeTuneControlGrid()
tune.res = tuneParams(lrn, return.task, resampling = rin, par.set = ps,
                      measures = list(return.cost, mmce), control = ctrl, show.info = FALSE)
tune.res
#wcw.weight=3.5
#return.costs.test.mean=1.68,mmce.test.mean=0.302
#predict validation by tunned optimal weight

#conclusion weighthing performs sligtly worse than optimal threshold

###############################   Over- and undersampling
#If the Learner supports neither observation nor class weights the 
#proportions of the classes in the training data can be changed by over- or undersampling.

# oversampling class Bad with a rate of 5

return.task.over = oversample(return.task, rate = 3.5, cl = "Return")
lrn = makeLearner("classif.multinom", trace = FALSE)
mod = train(lrn, return.task.over)
pred = predict(mod, task = return.task)
#oversample 
performance(pred, measures = list(return.cost, fnr))#1.6479800    0.5760551
performance(pred, measures = list(return.cost, tnr))#1.6479800    0.7700351 
performance(pred, measures = list(return.cost, f1))#1.6479800    0.3516658 
performance(pred, measures = list(return.cost, tpr))#1.6479800    0.4239449

# undersampling class Bad with a rate of 1/3.5
table(train$return_customer)
return.task.over = undersample(return.task, rate = 1/3.5, cl = "Not Return")
lrn = makeLearner("classif.multinom", trace = FALSE)
mod = train(lrn, return.task.over)
pred = predict(mod, task = return.task)
performance(pred, measures = list(return.cost, fnr))#-1.6479800    0.5760551
performance(pred, measures = list(return.cost, tnr))#-1.6479800    0.7655498
performance(pred, measures = list(return.cost, f1))#1.6479800    0.3516658 
performance(pred, measures = list(return.cost, tpr))#1.6479800    0.4239449

#resample+oversample
#realize by makeOversampleWrapper
lrn = makeLearner("classif.multinom", trace = FALSE)
lrn = makeOversampleWrapper(lrn, osw.rate = 3.5, osw.cl = "Return")
lrn
r = resample(lrn, return.task, rin, measures = list(return.cost, f1), show.info = FALSE)
r#1.6479800    0.3516658 

# tune the oversampling rate
lrn = makeLearner("classif.multinom", trace = FALSE)
lrn = makeOversampleWrapper(lrn, osw.cl = "Return")
ps = makeParamSet(makeDiscreteParam("osw.rate", seq(2.5, 5, 0.5)))
ctrl = makeTuneControlGrid()
tune.res = tuneParams(lrn, return.task, rin, par.set = ps, measures = list(return.cost, f1),
                      control = ctrl, show.info = FALSE)
tune.res

#resample+optimal oversample rate
#realize by makeOversampleWrapper
lrn = makeLearner("classif.multinom", trace = FALSE)
lrn = makeOversampleWrapper(lrn, osw.rate = 3.25, osw.cl = "Return")
lrn
r = resample(lrn, return.task, rin, measures = list(return.cost, f1), show.info = FALSE)
r#the result is not so impressive return.costs.aggr: 1.68,: 0.33

#smote
library(DMwR)
known$return_customer = as.factor(known$return_customer)
data.maxmin.smote = SMOTE(return_customer~.,data.maxmin,perc.over = 100,perc.under = 200)
warning()
################### undersampling
# tune the undersampling rate
lrn = makeLearner("classif.multinom", trace = FALSE)
lrn = makeUndersampleWrapper(lrn, usw.cl = "Not Return")
ps = makeParamSet(makeDiscreteParam("usw.rate", seq(0.2, 0.4 , 0.1)))
ctrl = makeTuneControlGrid()
tune.res = tuneParams(lrn, return.task, rin, par.set = ps, measures = list(return.cost, mmce),
                      control = ctrl, show.info = FALSE)
tune.res#0.3 return.costs.test.mean=1.69,mmce.test.mean=0.29

#resample+optimal undersample rate
#realize by makeOversampleWrapper
lrn = makeLearner("classif.multinom", trace = FALSE)
lrn = makeUndersampleWrapper(lrn, usw.rate = 0.3, usw.cl = "Not Return")
lrn
r = resample(lrn, return.task, rin, measures = list(return.cost, f1), show.info = FALSE)
r#1.6479800    0.3516658

###############    other cost sensitive classifiers
library(mlr)
load("optimal.Rda")
data.combine = dd
data.combine = Filter(is.numeric,data.combine)
known = data.combine[!is.na(data.combine$return_customer),]
unknown = data.combine[is.na(data.combine$return_customer),]
unknown$return_customer = NULL
library(caret)
inTrain = createDataPartition(known$return_customer,p = 0.8,list=F)
train = known[inTrain,]
validation = known[-inTrain,]
train = as.data.frame(train)
train$Rank_return_customer = NULL
train$return_customer = factor(train$return_customer,labels = c("Return","NotReturn"))
return.task = makeClassifTask(data = train, target = "return_customer",positive = "Return")
return.task = removeConstantFeatures(return.task)
costs = matrix(c(0,3,10,0),2)
colnames(costs)=rownames(costs)  = c("Return","NotReturn")
return.cost = makeCostMeasure(id = "return.costs", name = "Return Costs", costs = costs,
                              best = 0, worst = 10)

table(train$return_customer)
rin = makeResampleInstance("CV", iters = 5, task = return.task)


############### cost sensitive Neural Network
library(mlr)
getParamSet("classif.nnet")

ps = makeParamSet(
  makeDiscreteParam("size", values = seq(2,10,1)),
  makeDiscreteParam("maxit", values =seq(50,500,50))
)
ctrl = makeTuneControlGrid()
rdesc = makeResampleDesc("CV", iters = 3L)
lrn = makeLearner("classif.nnet", predict.type = "prob")
lrn = makeWeightedClassesWrapper(lrn, wcw.weight = 4)
res = tuneParams(lrn, task = return.task, resampling = rdesc, par.set = ps,
                 control = ctrl, measures = list(return.cost,auc))
lrn = setHyperPars(makeLearner("classif.nnet"), par.vals = res$x)
r = resample(lrn, return.task, rin, measures = list(return.cost, auc), show.info = FALSE)
data = generateHyperParsEffectData(res)
plotHyperParsEffect(data, x = "iteration", y = "averge cost",
                    plot.type = "line")

#average cost 0.37, auc 0.84 at the weght of 3
#average cost 0.47, auc 0.71 at the weght of 3.3
#average cost 0.37, auc 0.88 at the weght of 4
#average cost 0.42, auc 0.67 at the weght of 4.5
#average cost 0.49, auc 0.58 at the weght of 5

############### cost sensitive LR with L2
lrn = makeLearner("classif.LiblineaRL1L2SVC")
lrn = setHyperPars(lrn,)
lrn = makeWeightedClassesWrapper("classif.LiblineaRL1L2SVC", wcw.weight = 4)
r = resample(lrn, return.task, rin, measures = list(return.cost, f1), show.info = FALSE)

??setHyperPars

############### cost sensitive KSVM
ps = makeParamSet(
  makeDiscreteParam("C", values = 2^(-2:2)),
  makeDiscreteParam("sigma", values = 2^(-2:2))
)
ctrl = makeTuneControlGrid()
rdesc = makeResampleDesc("CV", iters = 3L)
lrn = makeWeightedClassesWrapper("classif.ksvm", wcw.weight = 4)
res = tuneParams(lrn, task = return.task, resampling = rdesc, par.set = ps,
                 control = ctrl, measures = list(return.cost,auc))
lrn = setHyperPars(makeLearner("classif.ksvm"), par.vals = res$x)
r = resample(lrn, return.task, rin, measures = list(return.cost, f1), show.info = FALSE)
data = generateHyperParsEffectData(res)
plotHyperParsEffect(data, x = "iteration", y = "averge cost",
                    plot.type = "line")
############### cost sensitive Gradient Boosting With Regression Trees

lrn = makeLearner("classif.blackboost", predict.type = "prob")
lrn2 = makeWeightedClassesWrapper(lrn, wcw.weight = 4)
r = resample(lrn2, return.task, rin, measures = list(return.cost), show.info = FALSE)
r#average cost 0.00 auc.mean
############### cost sensitive random Forest
library(mlr)
table(train$return_customer)
train$return_customer = factor(train$return_customer,labels = c("Not Return","Return"))
table(train$return_customer)
train = as.data.frame(train)
return.task = makeClassifTask(data = train, target = "return_customer",positive = "Return")
return.task = removeConstantFeatures(return.task)
costs = matrix(c(0,3,10,0),2)
return.cost = makeCostMeasure(id = "return.costs", name = "Return Costs", costs = costs,
                              best = 0, worst = 10)

colnames(costs)=rownames(costs)  = c("Return","Not Return")
rin = makeResampleInstance("CV", iters = 5, task = return.task)
lrn = makeWeightedClassesWrapper("classif.randomForest", wcw.weight = 4)
r = resample(lrn, return.task, rin, measures = list(return.cost), show.info = FALSE)
r


#all perform worse than XGBoost, no much clue on how to improve

################################### XGBOOST for original data
require(caret)
require(xgboost)
require(Matrix)
require(data.table)
require(vcd)

load("data_maxmin_inter.Rda")
#create sparse matrix
known = data_maxmin_inter[!is.na(data_maxmin_inter$return_customer),]
unknown = data_maxmin_inter[is.na(data_maxmin_inter$return_customer),]
inTrain = createDataPartition(known$return_customer,p=0.75,list = F)
train = known[inTrain,]
validation = known[-inTrain,]
save(train,file="data_maxmin_inter_train.Rda")
save(validation,file="data_maxmin_inter_val.Rda")

train_output_vector = as.numeric(train$return_customer)
train_sparse_matrix = sparse.model.matrix(return_customer~.-1,data=train)

validation_output_vector = as.numeric(validation$return_customer)
validation_sparse_matrix = sparse.model.matrix(return_customer~.-1,data=validation)

unknown$return_customer = NULL
test_sparse_matrix = sparse.model.matrix(~.,data=unknown)
dim(validation_sparse_matrix)

#strategy 1: scale_pos_weight + auc
params <- list(
  max.depth = 7,
  eta = 0.1,
  subsample = 0.5,
  colsample_bytree = 0.7,
  #  gamma = 0.1,
  scale_pos_weight = 50,
  lambda = 5000,
  alpha = 300,
  min_child_weight = 3000,
  max_delta_step = 0,
  objective = 'binary:logistic',
  eval_metric = "auc"
)
dataTrain <- xgb.DMatrix(train_sparse_matrix, label = train_output_vector)
model.dt <- xgb.train(dataTrain, params = params, nrounds = 7000)
#predict validation
pred_validation =  predict(model.dt,validation_sparse_matrix)
auc(validation$return_customer,pred_validation)#0.5851 auc is too low, weight is 6, 0.5862 7. 0.5857 8.
#50 weight,50 max_delta_step, 0.6436, subsample = 0.5,colsam 0.7, minschild 2000,max.de7,max_delt50,lama5000,alpha300
#0.6262
pred_validation_class = as.numeric(pred_validation>0.219)
library(pROC)
plot.roc(roc(validation$return_customer,pred_validation))
plot(roc(validation$return_customer,pred_validation),col="green")

y = factor(validation$return_customer)
table(y,pred_validation_class)

confusionMatrix(data=pred_validation_class,y)
#predict and save unknown

# xgb_dtree <- xgb.model.dt.tree(feature_names = dataTrain_names, model = model1)
xgb_importance <- xgb.importance(feature_names = colnames(train_sparse_matrix), model = model.dt)
head(xgb_importance,20)
xgb.plot.importance(xgb_importance[1:50,])
save(xgb_importance, file="xgb_importance_maxmini_inter.Rda")
x = xgb_importance$Feature

#confusion matrix
pred_validation = predict(model.dt,validation_sparse_matrix)
pred_validation = 1- pred_validation
y_predict.class = factor(ifelse(y_predict.class>0.8,1,0))
length(y_predict.class)

table(y_predict.class,y)



pred_test = predict(model.dt,test_sparse_matrix)
pred_test.class = as.factor(ifelse(pred_test>tau,1,0))
table(pred_test.class)
write.csv(data.frame('ID'=data[51885:64855,]$ID,'return_customer'=pred_test.class),file='NoTunning_19_Jan.csv',row.names=F)


model = xgb.dump(model.dt,with_stats = T)
model[1:10]#this statement prints the top 10 nodes

xgb.plot.importance(xgb_importance[1:20,])

print(chisq.test(train$item_count,train$remitted_items))

################################### XGBOOST train by auc
require(caret)
require(xgboost)
require(Matrix)
require(data.table)
require(vcd)
load("original.train.Rda")
load("original.validation.Rda")
load("original.uuknown.Rda")

#optimal
load("BADS_data.nor_Jan_30.Rda")
load("numrank.Rda")
dd = cbind(data.nor,numrank)
names(dd)
save(dd,file="optimal.Rda")
xgb.save(model.dt,fname = "optimal")
#dummy
data_maxmin_inter = data.maxmin[1:100]
known = data_maxmin_inter[!is.na(data_maxmin_inter$return_customer),]
unknown = data_maxmin_inter[is.na(data_maxmin_inter$return_customer),]
inTrain = createDataPartition(known$return_customer,p=0.8,list = F)
train = known[inTrain,]
validation = known[-inTrain,]
train_output_vector = as.numeric(train$return_customer)
train_sparse_matrix = sparse.model.matrix(return_customer~.-1,data=train)


validation_output_vector = as.numeric(validation$return_customer)
validation_sparse_matrix = sparse.model.matrix(return_customer~.-1,data=validation)

unknown$return_customer = NULL
test_sparse_matrix = sparse.model.matrix(~.,data=unknown)
dim(validation_sparse_matrix)

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
  eval_metric = "auc"
)
dataTrain <- xgb.DMatrix(train_sparse_matrix, label = train_output_vector)
model.dt <- xgb.train(dataTrain,  objective = 'rank:pairwise',params = params, nrounds = 7000)
pred_validation =  predict(model.dt,validation_sparse_matrix)
#predict validation
auc(validation$return_customer,pred_validation)#0.5851 auc is too low, weight is 6, 0.5862 7. 0.5857 8.
#50 weight,50 max_delta_step, subsample = 0.5,colsam 0.7, minschild 2000,max.de12,max_delt50,lama5000,alpha300
#Area under the curve: 0.6449,objective = 'rank:pairwise'
pred_validation_class = as.numeric(pred_validation>0.9)
table(pred_validation_class)
library(ROCR)
performance(pred_validation,measure = )

library(pROC)
plot.roc(roc(validation$return_customer,pred_validation))
plot(roc(validation$return_customer,pred_validation),col="green")

y = factor(validation$return_customer)
table(y,pred_validation_class)

confusionMatrix(data=pred_validation_class,y)
#predict and save unknown

# xgb_dtree <- xgb.model.dt.tree(feature_names = dataTrain_names, model = model1)
xgb_importance <- xgb.importance(feature_names = colnames(train_sparse_matrix), model = model.dt)
head(xgb_importance,20)
plot(model.dt)
library(mlr)


#confusion matrix
pred_validation = predict(model.dt,validation_sparse_matrix)
pred_validation = 1- pred_validation
y_predict.class = factor(ifelse(y_predict.class>0.8,1,0))
length(y_predict.class)

table(y_predict.class,y)



pred_test = predict(model.dt,test_sparse_matrix)
pred_test.class = as.factor(ifelse(pred_test>tau,1,0))
table(pred_test.class)
write.csv(data.frame('ID'=data[51885:64855,]$ID,'return_customer'=pred_test.class),file='NoTunning_19_Jan.csv',row.names=F)


model = xgb.dump(model.dt,with_stats = T)
model[1:10]#this statement prints the top 10 nodes

xgb.plot.importance(xgb_importance[1:20,])

print(chisq.test(train$item_count,train$remitted_items))


#################### Overall Data For XGBoost ###################

load("BADS_DATACLEANING_Jan_16.Rda")
ID = data[51885:64855,"ID"]

data.numeric = Filter(is.numeric,data)
data.factor = Filter(is.factor,data)
dim(data.factor)
#factor exclude date
data.factor.nonedate = data.factor[,-c(1,5,17,18)]

#date to numeric
date = as.data.frame(lapply(data.factor[,c(1,5,17,18)], as.numeric))

#numeric combine
data.numeric = cbind(data.numeric,date)

#clean numeric
data.numeric$ID = NULL
data.numeric$return_customer = NULL
#scaling numeric variables

minmaxscaler = function(x){ return((x - min(x))/(max(x) - min(x)))}

d = as.data.frame(lapply(data.numeric,minmaxscaler))

#combine numeric and factor
data.combine = cbind(d,data.factor.nonedate,data$return_customer)
names(data.combine)
colnames(data.combine)[83] = "return_customer"
data.combine  = data.table(data.combine) 
class(data.combine)

save(data.combine,file="data.combine.Rda")

load("data.combine.Rda")
require(caret)
require(xgboost)
require(Matrix)
require(data.table)
require(vcd)

data.combine = data.combine
names(data.maxmin)
#seperate
set.seed(6)
known = data.combine[!is.na(data.combine$return_customer),]
unknown = data.combine[is.na(data.combine$return_customer),]
unknown$return_customer = NULL
library(caret)
inTrain = createDataPartition(known$return_customer,p = 0.8,list=F)
train = known[inTrain,]
validation = known[-inTrain,]
prop.table(table(validation$return_customer))

#create sparse matrix
train_output_vector = as.numeric(train$return_customer)
train_sparse_matrix = sparse.model.matrix(return_customer~.-1,data=train)

validation_output_vector = as.numeric(validation$return_customer)
validation_sparse_matrix = sparse.model.matrix(return_customer~.-1,data=validation)

test_sparse_matrix = sparse.model.matrix(~.,data=unknown)
dim(test_sparse_matrix)


################################ scale_pos_weight + auc ########################

params <- list(
  max.depth = 6,
  eta = 0.1,
  subsample = 0.5,
  colsample_bytree = 0.3,
  gamma = 0.1,
  scale_pos_weight = 70,
  lambda = 50000,
  #  alpha = 100,
  min_child_weight = 20,
  max_delta_step = 5000,
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
                  early.stop.round = 200
)

xgb_cv_1$
  dataTrain <- xgb.DMatrix(train_sparse_matrix, label = train_output_vector)
model.dt <- xgb.train(dataTrain, params = params, nrounds = 7000)
# xgb_dtree <- xgb.model.dt.tree(feature_names = dataTrain_names, model = model1)
xgb_importance <- xgb.importance(feature_names = colnames(train_sparse_matrix), model = xgb_cv_1)
head(xgb_importance,20)

library(caret)
confusionMatrix(data=y_predict.class,y)
#predict and save unknown

pred_test = predict(model.dt,test_sparse_matrix)
pred_test.class = as.factor(ifelse(pred_test>tau,1,0))
table(pred_test.class)
write.csv(data.frame('ID'=data[51885:64855,]$ID,'return_customer'=pred_test.class),file='NoTunning_19_Jan.csv',row.names=F)


model = xgb.dump(model.dt,with_stats = T)
model[1:10]#this statement prints the top 10 nodes

xgb.plot.importance(xgb_importance[1:20,])

print(chisq.test(train$item_count,train$remitted_items))

################################ scale_pos_weight + auc ########################

params <- list(
  max.depth = 8,
  eta = 0.01,
  subsample = 0.7,
  colsample_bytree = 0.3,
  gamma = 0.1,
  #scale_pos_weight = 5,
  lambda = 50000,
  alpha = 10,
  
  min_child_weight = 20,
  max_delta_step = 0,
  objective = 'binary:logistic',
  eval_metric = "auc",
  nthread = 7
)

####################### tune parameters

#2. Hyperparameter search using train
load("train.LR.normal.Rda")
load("validation.LR.normal.Rda")
load("uknown.LR.normal.Rda")
#alternative
load("original.train.Rda")

str(train)
names(train)
# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
  eta = c(0.05,0.1),
  max_depth = c(5,7,8),
  delta = c(4000,50000),
  subsample = 0.7,
  colsample_bytree = 0.8,
  min_child_weight = 30,
  nrounds=10000
)
# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)
# train the model for each parameter combination in the grid, 
xgb_train_1 = train(return_customer~.,data=train,
                    trControl = xgb_trcontrol_1,
                    tuneGrid = xgb_grid_1,
                    method = "xgbTree",
                    max_delta_step = 0,
                    scale_pos_weight = 7,
                    objective = 'binary:logistic',
                    eval_metric = "auc")

xgb_train_1$results

#   using CV to evaluate

table(train$return_customer)
train$return_customer[train$return_customer==0] = "No"
train$return_customer[train$return_customer==1] = "Yes"
table(train$return_customer)

xgb_train_2 = train(return_customer~.,data=train,
                    trControl = xgb_trcontrol_1,
                    tuneGrid = xgb_grid_1,
                    method = "xgbTree",
                    max_delta_step = 0,
                    scale_pos_weight = 5)
plot(xgb_train_2)
xgb_train_3 = train(return_customer~.,data=train,
                    trControl = xgb_trcontrol_1,
                    tuneGrid = xgb_grid_1,
                    method = "xgbTree",
                    max_delta_step = 0,
                    scale_pos_weight = 5,
                    eval_metric = "logloss")
plot(xgb_train_3)
xgb_train_4 = train(return_customer~.,data=train,
                    trControl = xgb_trcontrol_1,
                    tuneGrid = xgb_grid_1,
                    method = "xgbTree",
                    max_delta_step = 0,
                    scale_pos_weight = 7,
                    eval_metric = "auc")
plot(xgb_train_4)
# scatter plot of the AUC against max_depth and eta
ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")





