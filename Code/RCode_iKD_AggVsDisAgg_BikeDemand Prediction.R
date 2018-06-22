#install.packages("glmnet")
#install.packages("randomForest")
#install.packages("e1071")
#install.packages("car")
#install.packages("MASS")
library(car)
library(MASS)
library(glmnet)
library(randomForest)
library(e1071)


#Load data
dat = read.csv("C:/MSTM/Course/Study Material/Supply Chain Analytics/Class 3_Demand Forecasting/BikeDemandDaily.csv", header = T)

#Visualise the Demand data
#Total
plot(dat$Total,main = "Aggregate Demand", xlab = "Day", ylab = "Total Customer Demand")
abline(lm( dat$Total ~ dat$Index ), col = 4)
#Casual
plot(dat$Casual, main = "Casual Demand", xlab = "Day", ylab = "Demand")
abline(lm( dat$Casual ~ dat$Index ), col = 4)
#Regitered
plot(dat$Registered, main = "Registered Demand",xlab = "Day", ylab = "Demand")
abline(lm( dat$Registered ~ dat$Index ), col = 4)

#Formula
f1 = as.formula("dat$Total ~ dat$Index + dat$year + dat$month + dat$day + dat$season + dat$holiday + dat$workingday + dat$meanatemp + dat$maxatemp + dat$minatemp + dat$sdatemp + dat$meanhumidity + dat$maxhumidity + dat$minhumidity + dat$sdhumidity + dat$meanwindspeed + dat$maxwindspeed + dat$minwindspeed + dat$sdwindspeed")
f2 = as.formula("dat$Casual ~ dat$Index + dat$year + dat$month + dat$day + dat$season + dat$holiday + dat$workingday + dat$meanatemp + dat$maxatemp + dat$minatemp + dat$sdatemp + dat$meanhumidity + dat$maxhumidity + dat$minhumidity + dat$sdhumidity + dat$meanwindspeed + dat$maxwindspeed + dat$minwindspeed + dat$sdwindspeed")
f3 = as.formula("dat$Registered ~ dat$Index + dat$year + dat$month + dat$day + dat$season + dat$holiday + dat$workingday + dat$meanatemp + dat$maxatemp + dat$minatemp + dat$sdatemp + dat$meanhumidity + dat$maxhumidity + dat$minhumidity + dat$sdhumidity + dat$meanwindspeed + dat$maxwindspeed + dat$minwindspeed + dat$sdwindspeed")

#Linear Trend Model
#Total
linmod1 = lm(f1, data = dat)
summary(linmod1)
#Casual
linmod2 = lm(f2, data = dat)
summary(linmod2)
#Registered
linmod3 = lm(f3, data = dat)
summary(linmod3)

#Train and Test a sample of data for Visualisaton
p = dim(dat)[1]
q = floor(0.2*n)
ind = sample.int(p, size=q, replace = F)
dtrain = dat[-ind,]
dtest = dat[ind,]

#Visaulise if the split of the data into train and test samples has been random
#Total
lot(dtrain$Index, dtrain$Total, xlim= c(1,460),xlab="Day",ylab="Total Demand", col=2)
abline(lm(dtrain$Total ~ dtrain$Index ), lty =2, col=1)
points(dtest$Index , dtest$Total , col=4, pch="");
abline(lm(dtest$Total ~ dtest$Index), lty =3, col =3);
legend("topleft", legend=c("Train Sample Trend Line ","Test Sample Trend Line", "Train Data", "Test Data"),lty=c(2,3,NA,NA), col=c(1,3,2,4),pch=c(NA,NA,"o",""))
#Casual
plot(dtrain$Index, dtrain$Casual, xlim= c(1,460),xlab="Day",ylab="Casual Demand", col=2)
abline(lm(dtrain$Casual ~ dtrain$Index ), lty =2, col=1)
points(dtest$Index , dtest$Casual , col=4, pch="");
abline(lm(dtest$Casual ~ dtest$Index), lty =3, col =3);
legend("topleft", legend=c("Train Sample Trend Line ","Test Sample Trend Line", "Train Data", "Test Data"),lty=c(2,3,NA,NA), col=c(1,3,2,4),pch=c(NA,NA,"o",""))
#Registered
plot(dtrain$Index, dtrain$Registered, xlim= c(1,460),xlab="Day",ylab="Registered Demand", col=2)
abline(lm(dtrain$Registered ~ dtrain$Index ), lty =2, col=1)
points(dtest$Index , dtest$Registered , col=4, pch="");
abline(lm(dtest$Registered ~ dtest$Index), lty =3, col =3);
legend("topleft", legend=c("Train Sample Trend Line ","Test Sample Trend Line", "Train Data", "Test Data"),lty=c(2,3,NA,NA), col=c(1,3,2,4),pch=c(NA,NA,"o",""))

#Predict the trend from the linear model
#Total
pred1 = predict(linmod1)
lines(dat$Index, pred1)
plot(pred1)
#Casual
pred2 = predict(linmod2)
lines(dat$Index, pred2)
plot(pred2)
#Registered
pred3 = predict(linmod3)
lines(dat$Index, pred3)
plot(pred3)

#Detrend and visualise the data
#Total
plot(dat$Index,dat$Total - pred1,xlab="Day",ylab="Detrended Total Demand",col=2, pch="")
lines(c(0,460),c(0,0))
#Casual
plot(dat$Index,dat$Total - pred2,xlab="Day",ylab="Detrended Casual Demand",col=2, pch="")
lines(c(0,460),c(0,0))
#Registered
plot(dat$Index,dat$Total - pred3,xlab="Day",ylab="Detrended Registered Demand",col=2, pch="")
lines(c(0,460),c(0,0))

#Visualise Box plots for season
#Total
boxplot (dat$Total ~ dat$season, xlab="Seasons", ylab="Aggregate Demand",col=c(2,3,4,5))
#Casual
boxplot (dat$Casual ~ dat$season, xlab="Seasons", ylab="Casual Demand",col=c(2,3,4,5))
#Registered
boxplot (dat$Registered ~ dat$season, xlab="Seasons", ylab="Registered Demand",col=c(2,3,4,5))

#Visualise by scatterplots for meanatemp, meanhumidity, and maxwindspeed
#Total
scatterplotMatrix(~Total + meanatemp + meanhumidity + maxwindspeed, data=dat,pch=".")
#Casual
scatterplotMatrix(~Casual + meanatemp + meanhumidity + maxwindspeed, data=dat,pch=".")
#Registered
scatterplotMatrix(~Registered + meanatemp + meanhumidity + maxwindspeed, data=dat,pch=".")


#Run Lasso for Total
y1 = dat$Total
x1 = model.matrix(linmod1)
lasso1 <- cv.glmnet(x1,y1,family = "gaussian")
summary(lasso1)

#Lasso Stats
lasso1$lambda.min #Penalising Factor Optinal Lambda
min(lasso1$cvm) #Gives the min mean squared error
plot(lasso1)
log(lasso1$lambda)

lasso_T = glmnet(x1,y1,family = "gaussian", lambda = lasso1$lambda.min)
lasso_T$beta

#Run Lasso for Casual
y2 = dat$Casual
x2 = model.matrix(linmod2)
lasso2 <- cv.glmnet(x2,y2,family = "gaussian")
summary(lasso2)

#Lasso Stats
lasso2$lambda.min #Penalising Factor Optinal Lambda
min(lasso2$cvm) #Gives the min mean squared error
plot(lasso2)
log(lasso2$lambda)

lasso_C = glmnet(x2,y2,family = "gaussian", lambda = lasso2$lambda.min)
lasso_C$beta

#Run Lasso for Registered
y3 = dat$Registered
x3 = model.matrix(linmod3)
lasso3 <- cv.glmnet(x3,y3,family = "gaussian")
summary(lasso3)

#Lasso Stats
lasso3$lambda.min #Penalising Factor Optinal Lambda
min(lasso3$cvm) #Gives the min mean squared error
plot(lasso3)
log(lasso3$lambda)

lasso_R = glmnet(x3,y3,family = "gaussian", lambda = lasso3$lambda.min)
lasso_R$beta

#Incorporate the variables selected by LASSO
fT = as.formula("Total ~ Index + year + day + season + holiday + meanatemp + maxatemp + minatemp + sdatemp + meanhumidity + maxhumidity + minhumidity + sdhumidity + meanwindspeed + maxwindspeed + minwindspeed + sdwindspeed")
fTi = as.formula("Total ~ 1")
fC = as.formula("Casual ~ Index + year + day + holiday + workingday + meanatemp + minatemp + sdatemp + meanhumidity + sdhumidity + meanwindspeed + maxwindspeed")
fCi = as.formula("Casual ~ 1")
fR = as.formula("Registered ~ Index + month + day + season + holiday + workingday + meanatemp + maxatemp + minatemp + sdatemp + meanhumidity + sdhumidity + meanwindspeed + maxwindspeed + sdwindspeed")
fRi = as.formula("Registered ~ 1")

#Linear Trend Model after LASSO
#Total
lassomod1 = lm(fT, data = dat)
summary(lassomod1)
#Casual
lassomod2 = lm(fC, data = dat)
summary(lassomod2)
#Registered
lassomod3 = lm(fR, data = dat)
summary(lassomod3)

#Train and Test a sample of data after LASSO
p1 = dim(dat)[1]
q1 = floor(0.2*n)
ind1 = sample.int(p1, size=q1, replace = F)
dtrain1 = dat[-ind1,]
dtest1 = dat[ind1,]

#Visualise actual versus Predicted Demand Plot for Complete Model after LASSO
#Total
pred4 = predict(lassomod1, newdata = dtest1)
plot(dtest1$Index,dtest1$Total,pch="", col=2, xlab="Day", ylab="Aggregate Demand")
points(dtest1$Index,pred4,pch="o",col=4)
legend("topleft",legend=c("Observed Total Demand"," Predicted Total Demand"),pch=c("","o"),col=c(2,4))
#Casual
pred5 = predict(lassomod2, newdata = dtest1)
plot(dtest1$Index,dtest1$Casual,pch="", col=2, xlab="Day", ylab="Casual Demand")
points(dtest1$Index,pred5,pch="o",col=4)
legend("topleft",legend=c("Observed Casual Demand"," Predicted Casual Demand"),pch=c("","o"),col=c(2,4))
#Registered
pred6 = predict(lassomod3, newdata = dtest1)
plot(dtest1$Index,dtest1$Registered,pch="", col=2, xlab="Day", ylab="Registered Demand")
points(dtest1$Index,pred6,pch="o",col=4)
legend("topleft",legend=c("Observed Registered Demand"," Predicted Registered Demand"),pch=c("","o"),col=c(2,4))


#MACHINE LEARNING ALGORITHMS DEFINITION
#GeneralizedLinearModel_GaussianDistribution
Run_GLM_GaussianDist <- function(f,glm_gaus_train,glm_gaus_test){
  glm_gaus_train_det <- glm(f, data = glm_gaus_train, family = "gaussian")
  glm_gaus_resp <- predict(glm_gaus_train_det, newdata = glm_gaus_test, type = "response")
  return(glm_gaus_resp)
}


#GeneralizedLinearModel_PoissonDistribution
Run_GLM_PoissonDist <- function(f,glm_pois_train,glm_pois_test){
  glm_pois_train_det <- glm(f, data = glm_pois_train, family = "poisson")
  glm_pois_resp <- predict(glm_pois_train_det, newdata = glm_pois_test, type = "response")
  return(glm_pois_resp)
}

#GeneralizedLinearModel_NegativeBionomialDistribution
Run_GLM_NegativeBionomialDist <- function(f,glm_nb_train,glm_nb_test){
  glm_nb_train_det <- glm.nb(f, data = glm_nb_train)
  glm_nb_resp <- predict(glm_nb_train_det, newdata = glm_nb_test, type = "response")
  return(glm_nb_resp)
}

#Stepwise Regression_AIC_Total
Run_StepWiseRegression_T <- function(f,stepreg_train,stepreg_test){
  stepreg_train_det1 <- glm(Total ~ 1, data = stepreg_train, family = "gaussian")
  stepreg_train_det2 <- glm(f, data = stepreg_train,family = "gaussian")
  step1 <- step(stepreg_train_det1, scope = list(lower = stepreg_train_det1, upper = stepreg_train_det2), direction = "forward")
  #summary(step1)
  stepreg_resp <- predict(step1, newdata = stepreg_test, type = "response")
  return(stepreg_resp)
}

#Stepwise Regression_AIC_Casual
Run_StepWiseRegression_C <- function(f,stepreg_train,stepreg_test){
  stepreg_train_det1 <- glm(Casual ~ 1, data = stepreg_train, family = "gaussian")
  stepreg_train_det2 <- glm(f, data = stepreg_train,family = "gaussian")
  step1 <- step(stepreg_train_det1, scope = list(lower = stepreg_train_det1, upper = stepreg_train_det2), direction = "forward")
  #summary(step1)
  stepreg_resp <- predict(step1, newdata = stepreg_test, type = "response")
  return(stepreg_resp)
}

#Stepwise Regression_AIC_Registered
Run_StepWiseRegression_R <- function(f,stepreg_train,stepreg_test){
  stepreg_train_det1 <- glm(Registered ~ 1, data = stepreg_train, family = "gaussian")
  stepreg_train_det2 <- glm(f, data = stepreg_train,family = "gaussian")
  step1 <- step(stepreg_train_det1, scope = list(lower = stepreg_train_det1, upper = stepreg_train_det2), direction = "forward")
  #summary(step1)
  stepreg_resp <- predict(step1, newdata = stepreg_test, type = "response")
  return(stepreg_resp)
}

#RandomForest
Run_RandomForest <- function(f,rf_train,rf_test){
  rf_train_det <- randomForest(f, data = rf_train, ntree = 500, importance = T, proximity = T, na.action = na.omit)
  rf_resp <- predict(rf_train_det, newdata = rf_test, type = "response")
  return(rf_resp)
}

#SupportVectorModel(SVM)
Run_SVM <- function(f,svm_train,svm_test){
  svm_train_det <- svm(f, data = svm_train,kernel="radial")
  svm_resp <- predict(svm_train_det, newdata = svm_test, type = "response")
  return(svm_resp)
}

#CalculateRMSPE
RMSPE <- function(actual,predicted){
  temp1 <- mean((actual-predicted)^2)
  temp2 <- sqrt(temp1)
  return(temp2)
}

#MAIN PROGRAM
#Aggregate_RMSPE Declaration
glm_gaus_rmspe_agg = numeric()
glm_pois_rmspe_agg = numeric()
glm_nb_rmspe_agg = numeric()
stepreg_rmspe_agg = numeric()
rf_rmspe_agg = numeric()
svm_rmspe_agg = numeric()

#Casual_RMSPE Declaration
glm_gaus_rmspe_cas = numeric()
glm_pois_rmspe_cas = numeric()
glm_nb_rmspe_cas = numeric()
stepreg_rmspe_cas = numeric()
rf_rmspe_cas = numeric()
svm_rmspe_cas = numeric() 

#Registered_RMSPE Declaration
glm_gaus_rmspe_regd = numeric()
glm_pois_rmspe_regd = numeric()
glm_nb_rmspe_regd = numeric()
stepreg_rmspe_regd = numeric()
rf_rmspe_regd = numeric()
svm_rmspe_regd = numeric()

dat1 <- dat

for(i in 1:10){
  index = sample.int(n, size=nt, replace = F)
  train = dat1[-index,]
  test = dat1[index,]

#AGGREGATED DEMAND FORECAST METHOD
  #Call Generalized Linear Models - Gaussian
  glm_gaus_response_total = Run_GLM_GaussianDist(fT,train,test)
  glm_gaus_rmspe_agg[i] = RMSPE(test$Total,glm_gaus_response_total)
  
  #Call Generalized Linear Models - Possion
  glm_pois_response_total = Run_GLM_PoissonDist(fT,train,test)
  glm_pois_rmspe_agg[i] = RMSPE(test$Total,glm_pois_response_total)
  
  #Call Generalized Linear Models - NegativeBionomialDistribution
  glm_nb_response_total = Run_GLM_NegativeBionomialDist(fT,train,test)
  glm_nb_rmspe_agg[i] = RMSPE(test$Total,glm_nb_response_total)
  
  #Call Stepwise Regression_AIC
  stepreg_response_total = Run_StepWiseRegression_T(fT,train,test)
  stepreg_rmspe_agg[i] = RMSPE(test$Total,stepreg_response_total)
  
  #Call RandomForest
  rf_response_total = Run_RandomForest(fT,train,test)
  rf_rmspe_agg[i] = RMSPE(test$Total,rf_response_total)
  
  #Call SVM
  svm_response_total = Run_SVM(fT,train,test)
  svm_rmspe_agg[i] = RMSPE(test$Total,svm_response_total)

#DIS-AGGREGATED DEMAND FORECAST METHOD
  #Casual and Registered
  #Call Generalized Linear Models - Gaussian
  glm_gaus_response_cas = Run_GLM_GaussianDist(fC,train,test)
  glm_gaus_rmspe_cas[i] = RMSPE(test$Casual,glm_gaus_response_cas)
  glm_gaus_response_regd = Run_GLM_GaussianDist(fR,train,test)
  glm_gaus_rmspe_regd[i] = RMSPE(test$Registered,glm_gaus_response_regd)
  
  #Call Generalized Linear Models - Possion
  glm_pois_response_cas = Run_GLM_PoissonDist(fC,train,test)
  glm_pois_rmspe_cas[i] = RMSPE(test$Casual,glm_pois_response_cas)
  glm_pois_response_regd = Run_GLM_PoissonDist(fR,train,test)
  glm_pois_rmspe_regd[i] = RMSPE(test$Registered,glm_pois_response_regd)
  
  #Call Generalized Linear Models - NegativeBionomialDistribution
  glm_nb_response_cas = Run_GLM_NegativeBionomialDist(fC,train,test)
  glm_nb_rmspe_cas[i] = RMSPE(test$Casual,glm_nb_response_cas)
  glm_nb_response_regd = Run_GLM_NegativeBionomialDist(fR,train,test)
  glm_nb_rmspe_regd[i] = RMSPE(test$Registered,glm_nb_response_regd)
  
  #Call Stepwise Regression_AIC
  stepreg_response_cas = Run_StepWiseRegression_C(fC,train,test)
  stepreg_rmspe_cas[i] = RMSPE(test$Casual,stepreg_response_cas)
  stepreg_response_regd = Run_StepWiseRegression_R(fR,train,test)
  stepreg_rmspe_regd[i] = RMSPE(test$Registered,stepreg_response_regd)
  
  #Call RandomForest
  rf_response_cas = Run_RandomForest(fC,train,test)
  rf_rmspe_cas[i] = RMSPE(test$Casual,rf_response_cas)
  rf_response_regd = Run_RandomForest(fR,train,test)
  rf_rmspe_regd[i] = RMSPE(test$Registered,rf_response_regd)
  
  #Call SVM
  svm_response_cas = Run_SVM(fC,train,test)
  svm_rmspe_cas[i] = RMSPE(test$Casual,svm_response_cas)
  svm_response_regd = Run_SVM(fR,train,test)
  svm_rmspe_regd[i] = RMSPE(test$Registered,svm_response_regd)
}

#SUMMARIZE STATISTICS FOR ALL Aggregate ALGORITHMS
cat("RMSPE's Aggregate")
cat("Generalised Linear Model with Gaussian Distribution: ",mean(glm_gaus_rmspe_agg))
cat("Generalised Linear Model with Poisson Distribution: ",mean(glm_pois_rmspe_agg))
cat("Generalised Linear Model with Negative Bionomial Distribution: ",mean(glm_nb_rmspe_agg))
cat("Stepwise Regression - Akaike's Information Criterion: ",mean(stepreg_rmspe_agg))
cat("Random Forest: ",mean(rf_rmspe_agg))
cat("Support Vector Machine(SVM): ",mean(svm_rmspe_agg))

AGG <- min(mean(glm_gaus_rmspe_agg),mean(glm_pois_rmspe_agg),mean(glm_nb_rmspe_agg),mean(stepreg_rmspe_agg),mean(rf_rmspe_agg),mean(svm_rmspe_agg))
cat("Accurate Model Aggregate: ", AGG)

#SUMMARIZE STATISTICS FOR ALL Casual ALGORITHMS
cat("RMSPE's Casual")
cat("Generalised Linear Model with Gaussian Distribution: ",mean(glm_gaus_rmspe_cas))
cat("Generalised Linear Model with Poisson Distribution: ",mean(glm_pois_rmspe_cas))
cat("Generalised Linear Model with Negative Bionomial Distribution: ",mean(glm_nb_rmspe_cas))
cat("Stepwise Regression - Akaike's Information Criterion: ",mean(stepreg_rmspe_cas))
cat("Random Forest: ",mean(rf_rmspe_cas))
cat("Support Vector Machine(SVM): ",mean(svm_rmspe_cas))

CAS = min(mean(glm_gaus_rmspe_cas),mean(glm_pois_rmspe_cas),mean(glm_nb_rmspe_cas),mean(stepreg_rmspe_cas),mean(rf_rmspe_cas),mean(svm_rmspe_cas))
cat("Accurate Model Casual:", CAS)

#SUMMARIZE STATISTICS FOR ALL Registered ALGORITHMS
cat("RMSPE's Registered")
cat("Generalised Linear Model with Gaussian Distribution: ",mean(glm_gaus_rmspe_regd))
cat("Generalised Linear Model with Poisson Distribution: ",mean(glm_pois_rmspe_regd))
cat("Generalised Linear Model with Negative Bionomial Distribution: ",mean(glm_nb_rmspe_regd))
cat("Stepwise Regression - Akaike's Information Criterion: ",mean(stepreg_rmspe_regd))
cat("Random Forest: ",mean(rf_rmspe_regd))
cat("Support Vector Machine(SVM): ",mean(svm_rmspe_regd))

REGD = min(mean(glm_gaus_rmspe_regd),mean(glm_pois_rmspe_regd),mean(glm_nb_rmspe_regd),mean(stepreg_rmspe_regd),mean(rf_rmspe_regd),mean(svm_rmspe_regd))
cat("Accurate Model Registered:", REGD)

glm_gaus_rmspe_disagg = sum(mean(glm_gaus_rmspe_cas),mean(glm_gaus_rmspe_regd))
glm_pois_rmspe_disagg = sum(mean(glm_pois_rmspe_cas),mean(glm_pois_rmspe_regd))
glm_nb_rmspe_disagg = sum(mean(glm_nb_rmspe_cas),mean(glm_nb_rmspe_regd))
stepreg_rmspe_disagg = sum(mean(stepreg_rmspe_cas),mean(stepreg_rmspe_regd))
rf_rmspe_disagg = sum(mean(rf_rmspe_cas),mean(rf_rmspe_regd))
svm_rmspe_disagg = sum(mean(svm_rmspe_cas),mean(svm_rmspe_regd))

#SUMMARIZE STATISTICS FOR ALL Dis-Aggregated ALGORITHMS
cat("RMSPE's Dis-Aggregated")
cat("Generalised Linear Model with Gaussian Distribution: ",glm_gaus_rmspe_disagg)
cat("Generalised Linear Model with Poisson Distribution: ",glm_pois_rmspe_disagg)
cat("Generalised Linear Model with Negative Bionomial Distribution: ",glm_nb_rmspe_disagg)
cat("Stepwise Regression - Akaike's Information Criterion: ",stepreg_rmspe_disagg)
cat("Random Forest: ",rf_rmspe_disagg)
cat("Support Vector Machine(SVM): ",svm_rmspe_disagg)

DISAGG <- min(glm_gaus_rmspe_disagg,glm_pois_rmspe_disagg,glm_nb_rmspe_disagg,stepreg_rmspe_disagg,rf_rmspe_disagg,svm_rmspe_disagg)
cat("Accurate Model Dis-Aggregated: ", DISAGG)

#Compare the model's accuracy with RandomForest and SVM, select the model with relatively smaller mean RMSPE 
if(AGG < DISAGG){
  cat("AGGREGATE APPROACH delivers relatively accurate model with mean RMSPE of: ",AGG)
}else if(AGG == DISAGG){
    cat("AGGREGATE and DIS-AGGREGATE APPROACH deliver equally accurate model with mean RMSPE of: ",AGG)
}else if (AGG > DISAGG){
  cat("DIS-AGGREGATE APPROACH delivers relatively accurate model with mean RMSPE of: ",DISAGG)
}

