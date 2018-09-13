# Linear_Regression
#Loading the library files
library(MASS)
library(car)
library(stringr)
library(plyr)
library(ggplot2)



car_details_df<-read.csv("CarPrice_Assignment.csv")
str(car_details_df)
nrow(car_details_df)

#Removing unwanted coulumns i.e Car_ID
car_details_df<-car_details_df[,-1]

#chacking for duplicates in data set
Car.details.dup<-unique(car_details_df)
nrow(car_details_df)
# -- There are no duplicates in the data set

#Verifying for missing values in data set
car.missing.values <-sum(is.na(car_details_df))
View(car.missing.values)

###########################################################################

#wheelbase
quantile(car_details_df$wheelbase,seq(0,1,0.01))

#car Length
quantile(car_details_df$carlength,seq(0,1,0.01))

#car width
quantile(car_details_df$carwidth,seq(0,1,0.01))

#car height
quantile(car_details_df$carheight,seq(0,1,0.01))

#curbweight
quantile(car_details_df$curbweight,seq(0,1,0.01))

#curbweight
quantile(car_details_df$curbweight,seq(0,1,0.01))

#enginesize
quantile(car_details_df$enginesize,seq(0,1,0.01))
car_details_df$enginesize[which(car_details_df$enginesize>209)]<-209

#boreratio
quantile(car_details_df$boreratio,seq(0,1,0.01))

#stroke
quantile(car_details_df$stroke,seq(0,1,0.01))

#compressionratio
quantile(car_details_df$compressionratio,seq(0,1,0.01))
car_details_df$compressionratio[which(car_details_df$compressionratio>10.9400)]<-10.9400

#horsepower
quantile(car_details_df$horsepower,seq(0,1,0.01))

#peakrpm
quantile(car_details_df$peakrpm,seq(0,1,0.01))

#citympg
quantile(car_details_df$citympg,seq(0,1,0.01))

#highwaympg
quantile(car_details_df$highwaympg,seq(0,1,0.01))

#######################################################################################

#######################################################################################

car_details_df$CarName<-as.character(car_details_df$CarName)
temp.list <- strsplit(car_details_df$CarName, " ")
names.df <- ldply(temp.list, function (x) data.frame(companyName = x[1], Model = x[2]))

#Removing the carName column and adding the car name and model as separate columns
car_details_df<-cbind(car_details_df[,-2],names.df)

# Cleaning the incorrect names there in car companyName
levels(car_details_df$companyName)[levels(car_details_df$companyName)=="porcshce"] <- "porsche"
levels(car_details_df$companyName)[levels(car_details_df$companyName)=="Nissan"]   <- "nissan"
levels(car_details_df$companyName)[levels(car_details_df$companyName)=="toyouta"]  <- "toyota"
levels(car_details_df$companyName)[levels(car_details_df$companyName)=="vokswagen"]<- "volkswagen"
levels(car_details_df$companyName)[levels(car_details_df$companyName)=="maxda"]    <- "mazda"
levels(car_details_df$companyName)[levels(car_details_df$companyName)=="vw"]    <- "volkswagen"

#Handalling the catagorical variables

#fueltype
levels(car_details_df$fueltype)<-c(1,0)
car_details_df$fueltype<-as.numeric(levels(car_details_df$fueltype))[car_details_df$fueltype]

#Aspiration
levels(car_details_df$aspiration)<-c(1,0)
car_details_df$aspiration<-as.numeric(levels(car_details_df$aspiration))[car_details_df$aspiration]

#doornumber
levels(car_details_df$doornumber)<-c(1,0)
car_details_df$doornumber<-as.numeric(levels(car_details_df$doornumber))[car_details_df$doornumber]

# Create the dummy variable for carbody variable
dummy_1 <- data.frame(model.matrix( ~carbody, data = car_details_df))
dummy_1 <- dummy_1[,-1]

# Combine the dummy variables and the numeric columns of car_details_df dataset,
car_details_df <- cbind(car_details_df[,-5], dummy_1)

# Create the dummy variable for drivewheel variable
dummy_2 <- data.frame(model.matrix( ~drivewheel, data = car_details_df))
dummy_2 <- dummy_2[,-1]

# Combine the dummy variables and the numeric columns of car_details_df dataset,
car_details_df <- cbind(car_details_df[,-5], dummy_2)

#enginelocation
levels(car_details_df$enginelocation)<-c(1,0)
car_details_df$enginelocation<-as.numeric(levels(car_details_df$enginelocation))[car_details_df$enginelocation]

# Create the dummy variable for enginetype variable
dummy_3 <- data.frame(model.matrix( ~enginetype, data = car_details_df))
dummy_3 <- dummy_3[,-1]

# Combine the dummy variables and the numeric columns of car_details_df dataset,
car_details_df <- cbind(car_details_df[,-11], dummy_3)


# Create the dummy variable for cylindernumber variable
dummy_4 <- data.frame(model.matrix( ~cylindernumber, data = car_details_df))
dummy_4 <- dummy_4[,-1]

# Combine the dummy variables and the numeric columns of car_details_df dataset,
car_details_df <- cbind(car_details_df[,-11], dummy_4)


# Create the dummy variable for fuelsystem variable
dummy_5 <- data.frame(model.matrix( ~fuelsystem, data = car_details_df))
dummy_5 <- dummy_5[,-1]

# Combine the dummy variables and the numeric columns of car_details_df dataset,
car_details_df <- cbind(car_details_df[,-12], dummy_5)


# Create the dummy variable for comapany Name variable
dummy_6 <- data.frame(model.matrix( ~companyName, data = car_details_df))
dummy_6 <- dummy_6[,-1]

# Combine the dummy variables and the numeric columns of car_details_df dataset,
car_details_df <- cbind(car_details_df[,-20], dummy_6)

#Removing the Model data
car_details_df<-car_details_df[,-20]

#######################################################################################

#######################################################################################

# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(car_details_df), 0.7*nrow(car_details_df))
train = car_details_df[trainindices,]
test = car_details_df[-trainindices,]

# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)

step <- stepAIC(model_1, direction="both")
step

model_2<-lm(formula = price ~ aspiration + enginelocation + wheelbase +
              carwidth + curbweight + enginesize + stroke + peakrpm + citympg +
              carbodywagon + drivewheelrwd + enginetypedohcv + enginetypel +
              enginetypeohcf + cylindernumberfive + cylindernumberfour +
              cylindernumbersix + cylindernumberthree + fuelsystem2bbl +
              fuelsystemmpfi + companyNameaudi + companyNamebmw + companyNamedodge +
              companyNamehonda + companyNameisuzu + companyNamejaguar +
              companyNamemazda + companyNamebuick + companyNamemercury +
              companyNamemitsubishi + companyNamenissan + companyNameplymouth +
              companyNamerenault + companyNamesaab + companyNametoyota +
              companyNamevolkswagen + companyNamevolvo, data = train)
summary(model_2)

vif(model_2)

# cylindernumberthree   -3.558e+03  2.818e+03  -1.263 0.209469 hence removing

model_3<-update(model_2,~.-cylindernumberthree)
summary(model_3)

vif(model_3)

# citympg       5.268e+01  4.780e+01   1.102 0.272923 ence removing


model_4<-update(model_3,~.-citympg)
summary(model_4)

vif(model_4)

# companyNameaudi      -1.751e+03  1.699e+03  -1.030 0.305181 hence removing

model_5<-update(model_4,~.-companyNameaudi)
summary(model_5)

vif(model_5)

# fuelsystemmpfi         7.272e+02  5.509e+02   1.320 0.189628  hence removing

model_6<-update(model_5,~.-fuelsystemmpfi)
summary(model_6)

vif(model_6)

# fuelsystem2bbl      6.016e+02  4.787e+02   1.257 0.211522 hence removing

model_7<-update(model_6,~.-fuelsystem2bbl)
summary(model_7)

vif(model_7)

# carbodywagon     -692.011    481.793  -1.436 0.153748 hence removing

model_8<-update(model_7,~.-carbodywagon)
summary(model_8)

vif(model_8)

# wheelbase     8.298e+01  5.548e+01   1.496 0.137570 hence removing

model_9<-update(model_8,~.-wheelbase)
summary(model_9)

vif(model_9)

# companyNameisuzu      -2.806e+03  1.812e+03  -1.548 0.124333 hence removing

model_10<-update(model_9,~.-companyNameisuzu)
summary(model_10)

vif(model_10)

# enginetypedohcv   -3.869e+03  2.052e+03  -1.885 0.061946 .hence removing

model_11<-update(model_10,~.-enginetypedohcv)
summary(model_11)

vif(model_11)

# companyNamevolvo  -1.503e+03  9.460e+02  -1.589 0.114838  hence removing

model_12<-update(model_11,~.-companyNamevolvo)
summary(model_12)

vif(model_12)

# companyNamemercury    -2.295e+03  1.622e+03  -1.415 0.159819  hence removing

model_13<-update(model_12,~.-companyNamemercury)
summary(model_13)

vif(model_13)

# companyNamesaab    -1.772e+03  9.225e+02  -1.920 0.057271 hence removing

model_14<-update(model_13,~.-companyNamesaab)
summary(model_14)

vif(model_14)

# stroke   -1.750e+03  8.091e+02  -2.163 0.032577 hence removing

model_15<-update(model_14,~.-stroke)
summary(model_15)

vif(model_15)

# peakrpm     1.335e+00  4.512e-01   2.957 0.003749 hence removing

model_16<-update(model_15,~.-peakrpm)
summary(model_16)

vif(model_16)

# drivewheelrwd   -1201.290    477.672  -2.515 0.013241 hence removing

model_17<-update(model_16,~.-drivewheelrwd)
summary(model_17)

vif(model_17)

# aspiration  -1.089e+03  4.494e+02  -2.424  0.01685 hence removing

model_18<-update(model_17,~.-aspiration)
summary(model_18)

vif(model_18)

# enginesize   3.412e+01  1.255e+01   2.718 0.007527 hence removing

model_19<-update(model_18,~.-enginesize)
summary(model_19)

vif(model_19)

# companyNamehonda  -2.438e+03  7.431e+02  -3.280 0.001352 hence removing

model_20<-update(model_19,~.-companyNamehonda)
summary(model_20)

vif(model_20)

# companyNamenissan -1.337e+03  6.329e+02  -2.112 0.036716 hence removing

model_21<-update(model_20,~.-companyNamenissan)
summary(model_21)

vif(model_21)

# companyNamedodge  -1.750e+03  9.637e+02  -1.816 0.071745 hence removing

model_22<-update(model_21,~.-companyNamedodge)
summary(model_22)

vif(model_22)

# companyNameplymouth   -1490.802    808.696  -1.843  0.06763 hence removing

model_23<-update(model_22,~.-companyNameplymouth)
summary(model_23)

vif(model_23)

# companyNamevolkswagen -1.311e+03  6.763e+02  -1.939 0.054783 hence removing

model_24<-update(model_23,~.-companyNamevolkswagen)
summary(model_24)

vif(model_24)

# enginetypeohcf     -1.756e+03  8.090e+02  -2.171 0.031819 hence removing
model_25<-update(model_24,~.-enginetypeohcf)
summary(model_25)

vif(model_25)

# companyNamemitsubishi  -1777.401    716.308  -2.481 0.014386  hence removing

model_26<-update(model_25,~.-companyNamemitsubishi)
summary(model_26)

vif(model_26)
# companyNamerenault -3.234e+03  1.404e+03  -2.303 0.022887 hence removing

model_27<-update(model_26,~.-companyNamerenault)
summary(model_27)

vif(model_27)

# companyNametoyota  -1.128e+03  4.574e+02  -2.466  0.01498  hence removing
model_28<-update(model_27,~.-companyNametoyota)
summary(model_28)

vif(model_28)

# companyNamemazda   -1.956e+03  6.681e+02  -2.929 0.004018  hence removing

model_29<-update(model_28,~.-companyNamemazda)
summary(model_29)

vif(model_29)

# corelation between car width and curb weight is high

c1<-data.frame(car_details_df$carwidth,car_details_df$curbweight)
cor(c1)

# Hence removing curbweight with Vif :  6.694484

model_30<-update(model_29,~.-curbweight)
summary(model_30)

vif(model_30)
# cylindernumbersix   -1926.2     1267.1  -1.520    0.131 due to high vif 3.854312
model_31<-update(model_30,~.-cylindernumbersix)
summary(model_31)

vif(model_31)

# enginetypel   -1108.1      981.3  -1.129    0.261  hence removing

model_32<-update(model_31,~.-enginetypel)
summary(model_32)

vif(model_32)
##########################################################################################
View(test)
Predict_price<-predict(model_32,test[,-19])
test$test_price<-Predict_price
test$carId <-seq(1:62)



ggplot(test, aes(carId,price)) + geom_line(aes(colour = "blue" )) +
  scale_x_continuous(name = "CarId", breaks = seq(0,65,3), limits = c(0,65)) +
  scale_y_continuous(name = "Price", breaks = seq(0,50000,2000), limits = c(0,50000)) + geom_line(aes(x=carId, y=test_price, colour="red"))


#correlation

cor_out<-cor(test$price,test$test_price)
View(cor_out)

r_squ<-cor_out^2
View(r_squ)
