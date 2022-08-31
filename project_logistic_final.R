library(haven)
library(ggplot2)
library(car)
library(dplyr)
library(skimr)
library(DataExplorer)
library(factoextra)#for PCA
library(psych)

getwd()
setwd("C:/Users/William/OneDrive/Desktop/jo/nci/Statistics for Data analysis/stats_project")
house_categories <- read_sav("C:/Users/William/OneDrive/Desktop/jo/nci/Statistics for Data analysis/stats_project/House Categories.SAV")
nrow(house_categories)

#setting up the digits display ,this helps to view p value in decimal
options(digits=3,scipen = 999)

str(house_categories)

table(house_categories$fuel)
table(house_categories$waterfront)
table(house_categories$newConstruction)
table(house_categories$PriceCat)

#Data Cleaning
house_categories$PriceCat <- ifelse(house_categories$PriceCat == 1, "Budget", "Expensive")

house_categories$fuel<-as.factor(as.character(house_categories$fuel))
house_categories$waterfront<-as.factor(house_categories$waterfront)
house_categories$newConstruction<-as.factor(as.character(house_categories$newConstruction))
house_categories$PriceCat<-as.factor(as.character(house_categories$PriceCat))



head(house_categories$PriceCat)

sapply(house_categories,function(x) sum(is.na(x)))

library(Amelia)
missmap(house_categories, main = "Missing values vs observed")


summary(house_categories)
glimpse(house_categories)
skim(house_categories)
plot_intro(house_categories)
#continuous variables:
plot_density(house_categories)
plot_histogram(house_categories)



#categorical features
plot_bar(house_categories)

plot_correlation(house_test)

head(house_test)
house_test<-house_categories[,1:9]
#Fitting the model
fit1<-glm(PriceCat~.,data = house_categories, family=binomial)
summary(fit1)




fit2<-glm(PriceCat~.-bedrooms,data = house_categories, family=binomial)
summary(fit2)

fit3<-glm(PriceCat~.-fireplaces,data = house_categories, family=binomial)
summary(fit3)

fit4<-glm(PriceCat~.-fireplaces-rooms,data = house_categories, family=binomial)
summary(fit4)

fit5<-glm(PriceCat~.-fireplaces-rooms-fuel,data = house_categories, family=binomial)
summary(fit5)

fit6<-glm(PriceCat~.-fireplaces-rooms-fuel-newConstruction,data = house_categories, family=binomial)
summary(fit6)

fit7<-glm(PriceCat~.-fireplaces-rooms-fuel-newConstruction-bedrooms,data = house_categories, family=binomial)
summary(fit7)

fit8<-glm(PriceCat~.-fireplaces-rooms-fuel-newConstruction-bedrooms-pctCollege,data = house_categories, family=binomial)
summary(fit8)

fit9<-glm(PriceCat~.-fireplaces-rooms-fuel-newConstruction-bedrooms-pctCollege-age,data = house_categories, family=binomial)
summary(fit9)
#Fit 7 is the model with all the relevant predictors
fit7<-glm(PriceCat~lotSize+age+landValue+livingArea+pctCollege+bathrooms+waterfront,data = house_categories, family=binomial)
summary(fit7)
fit7


contrasts(house_categories$PriceCat)

#Examine predicted probabilities and classifications
pred_probs<-predict(fit7, type="response")
pred_probs
pred_class<-ifelse(pred_probs<0.5, "Budget","Expensive")
pred_class

#confusionMatrix function in caret package
library(caret)
library(e1071)

pred_classf<-as.factor(pred_class)
class(pred_classf)
class(house_categories$PriceCat)
table(house_categories$PriceCat,pred_classf)
confusionMatrix(house_categories$PriceCat,pred_classf,positive = "Expensive")


#Examine predicted probabilities and classifications
pred_probs<-predict(fit8, type="response")
pred_probs
pred_class<-ifelse(pred_probs<0.5, "Budget","Expensive")
pred_class

#confusionMatrix function in caret package
library(caret)
library(e1071)

pred_classf<-as.factor(pred_class)
class(pred_classf)
class(house_categories$PriceCat)
table(house_categories$PriceCat,pred_classf)
confusionMatrix(house_categories$PriceCat,pred_classf,positive = "Expensive")

vif(fit7)

house_subset=house_categories %>% dplyr::select(-rooms,-bedrooms,-fireplaces,-waterfront,-newConstruction,
                                                - PriceCat,-fuel)
house_subset
plot_correlation(house_subset)




#Check Linearity of the Logit

LoLTestModel<-glm(PriceCat~Age+Age:log(Age)+Price+Price:log(Price),data = X65d_WARRANTY_P, family=binomial)
summary(LoLTestModel)

#identify the outliers
nrow(house_categories)
boxplot(house_categories$lotSize, horizontal = TRUE) #Outliers >165000
boxplot(loan_sanction$Credit_Score, horizontal = TRUE) #no outliers
boxplot(loan_sanction$Loan_Amount_Request, horizontal = TRUE)



house_filtered=house_categories[,1:9]
house_filtered <- house_filtered %>% dplyr::select(-rooms,-bedrooms,-fireplaces,-pctCollege,-lotSize)
head(house_filtered)


#check for number of componennts
fa.parallel(house_filtered,fa="pc",n.iter=100)

#extract components
#pca_category<-principal(house_filtered,nfactors=3,rotate="none")
#pca_category

#Rotate components
pca_rotate_category<-principal(house_filtered,nfactors=2,rotate="varimax",scores=TRUE)
pca_rotate_category
pca_rotate_category$scores

#intrepret rotated components
fa.diagram(pca_rotate_category)
fa.diagram(pca_category)

house_final<-cbind(house_categories,(pca_rotate_category$scores))
house_final

fit10<-glm(PriceCat~RC1+RC2+lotSize+waterfront,data = house_final, family=binomial)
summary(fit10)
vif(fit10)
fit10

pred_prob10<-predict(fit10, type="response")
pred_prob10
pred_class10<-ifelse(pred_prob10<0.5, "Budget","Expensive")
pred_class10

pred_class10<-as.factor(pred_class10)
class(pred_class10)
class(house_final$PriceCat)
table(house_final$PriceCat,pred_class10)
confusionMatrix(house_final$PriceCat,pred_class10,positive = "Expensive")

house_final
LoLTestModel<-glm(PriceCat~RC1+RC1:log(RC1)+RC2+RC2:log(RC2)+lotSize+lotSize:log(lotSize),data = house_final, family=binomial)
summary(LoLTestModel)
#Total rows of house categories dataset
total_Count=nrow(house_final)
sprintf("Total cases  = %d",total_Count)
# Standardized residulas with absolute value above 2
k<-rstandard(fit10)
k<-as.data.frame(k)
res_2=nrow(filter(k,k<(-2) | k>2))
res_2_percent=(res_2/total_Count)*100
res_2
# Standardized residulas with absolute value above 2.5
res_2.5=nrow(filter(k,k<(-2.5) | k>2.5))
res_2.5
res_2.5_percent=(res_2.5/total_Count)*100
sprintf("Total cases with standaridized residuals greater than 2 = %f",res_2_percent)
sprintf("Total cases with standaridized residuals greater than 2.5 = %f",res_2.5_percent)

#Standard residuals with absolute value above 3--outliers
filter(k,k<(-3) | k>3)


library(dplyr)
house_final<-house_final %>%
  slice(-c(122))
house_final<-house_final %>%
  slice(-c(1506))


#--------------------------------------------
6/1688

plot_histogram(rstandard(fit10))
house_final

library(dplyr)
house_filtered_final<-house_final %>%
  slice(-c(122,759,773,1507,1351))

length(house_filtered_final$PriceCat)

fit10<-glm(PriceCat~RC1+RC2+RC3+waterfront,data = house_filtered_final, family=binomial)

filtered<-rstandard(fit10)
plot_histogram(rstandard(fit10))
cooks.distance(fit10)

influencePlot(model=fit10,scale=3,main="influence plot")

k_new<-rstandard(fit10)
k_new<-as.data.frame(k_new)
nrow(filter(k_new,k_new<(-2.5) | k_new>2.5))
nrow(filter(k_new,k_new<(-2) | k_new>2))
#outliers
filter(k_new,k_new<=(-3) | k_new>=3)

nrow(house_filtered_final)
house_filtered_final<-(house_filtered_final %>%
                         slice(-c(1351)))

fit11<-glm(PriceCat~RC1+RC2+RC3+waterfront,data = house_filtered_final, family=binomial)
summary(fit11)


pred_prob11<-predict(fit11, type="response")
length(pred_prob11)
length(house_filtered_final)
pred_class11<-ifelse(pred_prob11<0.5, "Budget","Expensive")
pred_class11
class(house_filtered_final$PriceCat)
table(house_filtered_final$PriceCat)
pred_class11<-as.factor(pred_class11)
table(house_filtered_final$PriceCat)
table(pred_class11)
class(house_filtered_final$PriceCat)
table(house_filtered_final$PriceCat,pred_class11)
confusionMatrix(house_filtered_final$PriceCat,pred_class11,positive = "Expensive")



k_new<-rstandard(fit11)
k_new<-as.data.frame(k_new)
nrow(filter(k_new,k_new<(-2.5) | k_new>2.5))
nrow(filter(k_new,k_new<(-2) | k_new>2))
#outliers
filter(k_new,k_new<=(-3) | k_new>=3)

plot_histogram(rstandard(fit11))
plot(rstandard(fit10))



cooks<-cooks.distance(fit11)
cooks<-as.data.frame(cooks)
nrow(filter(cooks,cooks<(cooks>1)))

influencePlot(model=fit10,scale=3,main="influence plot")


vif(fit11)

house_final[122,]
#PseudoRSquared Statistics
library(DescTools)
PseudoR2(fit10, which = "all")


library(generalhoslem)
logitgof(house_final$PriceCat,fitted(fit10))
h1<-hoslem.test(fit10$fitted.values,fitted(fit10),g=10)
h1


library(ResourceSelection)
h1<-hoslem.test(fit10$y,fitted(fit10),g=10)
h1
  