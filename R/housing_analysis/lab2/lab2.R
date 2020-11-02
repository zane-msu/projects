library(ggplot2)
library(rowr)

ameslist <- as.data.frame(read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",header = TRUE,sep = ","))

#Run the above, but instead specifying header = FALSE. 
#ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",header = FALSE, sep = ",")

#What data type are the various columns? 
#typeof(ameslist$v1)
#this reurns "NULL" which makes sense as the column contains mixed types of data

#Now try ommitting the line altogether. 
#ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv", sep = ",")
#What is the default behavior of the read.table function?1
#default behavior is same as header=FALSE

names(ameslist)

#Go through the variables in the dataset and make a note about your 
#interpretation for each. 
#Many will be obvious, but some require additional thought.
#Interpretations for confusing variables are:
#LotShape describes what the lot looks like, reg likely means rectangular and IR1, IR2, etc. are for irregular shapes
#MSSubClass is the type of dwelling in the sale according to googling it.
#The conditions do not provide enough information to say. It may be useful to check if they cause differnces in the sale price to see if they can be ignored. 

typeof(ameslist)

GarageTemp <- model.matrix( ~ GarageType - 1, data=ameslist )
ameslist <- cbind.fill(ameslist, GarageTemp)

ameslist$GarageOutside <- ifelse(ameslist$GarageTypeDetchd == 1 | ameslist$GarageTypeCarPort == 1, 1, 0)
unique(ameslist$GarageOutside)

#Utilizing a similar approach to what you did above, fix this so that the only outputs are zero and one.
#our code does not output NA values, merging instead of cbinding may be the reason

Ames<-ameslist[which(sapply(ameslist,is.numeric)==TRUE)]

names(Ames)
pairs(~LotArea+YearBuilt+OverallQual+OverallCond+BsmtFinSF1+TotRmsAbvGrd+MoSold+FullBath+
        HalfBath+GarageCars+BedroomAbvGr+YrSold,data=Ames)

Ames_small <- Ames[,c(4,5,6,7,10,20,21,22,24,27,36,37,38)]

cor(Ames_small)
#most matched prior beliefs, overall condition and sale price were negatively correlated, which was surprising
reg<-lm(SalePrice~GrLivArea,data=ameslist)
ggplot(ameslist,aes(x=GrLivArea,y=SalePrice))+
  geom_point()+
  geom_abline(intercept=coef(reg)["(Intercept)"],slope=coef(reg)["GrLivArea"])

#the following line gives information abou the largest outlier above the line
ameslist[which.max(ameslist$SalePrice-(ameslist$GrLivArea*coef(reg)["GrLivArea"]+coef(reg)["(Intercept)"])),]

#/////////////////exercise 2///////////////////////////
#lm(SalePrice~lstat,data=ameslist) what is lstat?

attach(Ames)
lm.fit <- lm(SalePrice ~ GrLivArea)

#What is GrLivArea? 
#It is the square footage of the ground level

#If you did not include this variable above, check its relationship to other variables in 
#the dataset to get a better idea what it is.
cor(ameslist$GrLivArea,Ames_small)
lm.fit
summary(lm.fit)

#Use plot() to explore the model above. Do you suspect that some outliers have a large influence on the data?
plot(lm.fit)
#there are some outliers, however most of the data seems to be reasonably grouped.

lm.fit = lm(SalePrice ~ GrLivArea + LotArea)

#Does controlling for LotArea change the qualitative conclusions from the previous regression? 
#not very much, a greater ground living area still means greater sale price and is still very significant.
#What about the quantitative results? 
#the ceof went from 107 to 104 on ground living area
#Does the direction of the change in the quantitative results make sense to you?
#yes, adding lot area will reduce the coefficient since lotarea and grlivarea are related.

summary(lm(SalePrice ~ GarageOutside,data=ameslist))
#an indoor garage is worth $2849, however the p value does not indicate significance

all<-lm(SalePrice~.,data=Ames)
summary(all)
#Is there a relationship between the predictors and the response?
#yes there is a relationship 
  
#Which predictors appear to have a statistically significant relationship to the response?
#MSSubClass,LotArea, OverallQual, OverallCond, YearBuilt,MasVnrArea,BsmtFinSF1, X1stFlrSF, X2ndFlrSF,
#BsmtFullBath, BedroomAbvGr, KitchenAbvGr, TotRmsAbvGrd, Fireplaces, GarageCars,ScreenPorch,

#What does the coefficient for the year variable suggest?
#the trend of housing prices was downward for the duration of the data collection.

plot(all)
#yes, there are several large outliers 
# one observation has very high leverage, some others have moderately high leverage.

#Recall that the operator : designates the interaction between two variables. 
#The operator * designates the interaction between the two variables, plus the main effects.

#find interactions that are statistically significant
inter1<-lm(SalePrice~GrLivArea*FullBath,data=ameslist)
summary(inter1)

inter2<-lm(SalePrice~LotFrontage:LotArea,data=ameslist)
summary(inter2)


baseline <- lm(SalePrice ~ LotArea,data=ameslist)
trans_ln <- lm(SalePrice ~ log(LotArea),data=ameslist)
summary(trans_ln)
summary(baseline)
#the t value improved from 10 to 16 when using an ln function 
#r^2 also got much better, going from 0.07 to 0.15

trans_x2 <- lm(SalePrice ~ (LotArea*LotArea),data=ameslist)
summary(trans_x2)
summary(baseline)
#There is no difference in result when the value is squared

trans_sqrt <- lm(SalePrice ~ sqrt(LotArea),data=ameslist)
summary(trans_sqrt)
summary(baseline)
#Using the squareroot, the r^2 value got better, going from .07 to .14
#The t value also improved from 10 to 15


