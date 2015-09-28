
###Step-1  ---- reading the file

ETC_Modelling <- read.csv(file="D:/R Input files/input.csv")

###Step-2  ---- split the data into 2 sets(training & testing)

training = (ETC_Modelling$Created_year == 2014)
testing = !training

training_data = ETC_Modelling[training, ]
testing_data = ETC_Modelling[testing, ]

###step-3  ---- Fit a logistic reg model by using training data

mylogit <- glm(Days_Taken ~ KEYWORDVAR_O1 + Group_O1 + Department_O1, data = training_data, family = "binomial")

summary(mylogit)

##step-4 ---- Use the fitted model to do predictions for the testing data

testing_data$model_predict_probs = predict(mylogit, testing_data, type="response")

testing_data$model_predict_daystaken = rep("less", 2117)
testing_data$model_predict_daystaken[testing_data$model_predict_probs> 0.5] = "more"

##step-5  ---- compute the misclassification rate

table(testing_data$model_predict_daystaken, testing_data$Days_Taken)

mean(testing_data$model_predict_daystaken != testing_data$Days_Taken)

###################################################################################################

###Step-1  ---- reading the file

ETC_Modelling <- read.csv(file="D:/R Input files/input.csv")

###Step-2  ---- split the data into 2 sets(training & testing)

training = (ETC_Modelling$Created_year == 2014)
testing = !training

training_data = ETC_Modelling[training, ]
testing_data = ETC_Modelling[testing, ]

###step-3  ---- Fit a logistic reg model by using training data

mylogit <- glm(Days_Taken ~ RequestType_O1 + CreatedDayofWeek_O1, data = training_data, family = "binomial")

summary(mylogit)

##step-4 ---- Use the fitted model to do predictions for the testing data

testing_data$model_predict_probs = predict(mylogit, testing_data, type="response")

testing_data$model_predict_daystaken = rep("less", 2117)
testing_data$model_predict_daystaken[testing_data$model_predict_probs> 0.5] = "more"

##step-5  ---- compute the misclassification rate

table(testing_data$model_predict_daystaken, testing_data$Days_Taken)

mean(testing_data$model_predict_daystaken != testing_data$Days_Taken)

###################################################################################################

###Step-1  ---- reading the file

ETC_Modelling <- read.csv(file="D:/R Input files/input.csv")

###Step-2  ---- split the data into 2 sets(training & testing)

training = (ETC_Modelling$Created_year == 2014)
testing = !training

training_data = ETC_Modelling[training, ]
testing_data = ETC_Modelling[testing, ]

###step-3  ---- Fit a logistic reg model by using training data

mylogit <- glm(Days_Taken ~ Subcategory_O1 + Item_O1, data = training_data, family = "binomial")

summary(mylogit)

##step-4 ---- Use the fitted model to do predictions for the testing data

testing_data$model_predict_probs = predict(mylogit, testing_data, type="response")

testing_data$model_predict_daystaken = rep("less", 2117)
testing_data$model_predict_daystaken[testing_data$model_predict_probs> 0.5] = "more"

##step-5  ---- compute the misclassification rate

table(testing_data$model_predict_daystaken, testing_data$Days_Taken)

mean(testing_data$model_predict_daystaken != testing_data$Days_Taken)

##_________________________________________________________________##

setwd("D:/R Output files")

write.csv(testing_data,file="testing_data.csv")
write.csv(model_predict_daystaken,file="predicted_testing.csv")