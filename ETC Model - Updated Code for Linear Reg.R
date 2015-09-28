
ETC_Modelling <- read.csv(file="D:/R Input files/testing_data.csv")

ETC_Modelling_More  <- subset(ETC_Modelling,model_predict_daystaken %in% c("more")) 
ETC_Modelling_less  <- subset(ETC_Modelling,model_predict_daystaken %in% c("less"))

## LE 1440

y <- cbind(ETC_Modelling_less$Duration)
x <- cbind(ETC_Modelling_less$KEYWORDVAR_O2, ETC_Modelling_less$KEYWORDVAR_R2, ETC_Modelling_less$Group_O2, ETC_Modelling_less$Department_O2, ETC_Modelling_less$Region_O2, ETC_Modelling_less$RequestType_O2, ETC_Modelling_less$CreatedDayofWeek_O2, ETC_Modelling_less$Subcategory_O2, ETC_Modelling_less$Item_O2)
x1 <- cbind(ETC_Modelling_less$KEYWORDVAR_O2)

cor(y,x)

plot(y~x1, data = ETC_Modelling_less)

olsreg1 <- lm (y ~ x)
summary(olsreg1)
confint(olsreg1, level = 0.95)


## GT 1440

y <- cbind(ETC_Modelling_More$Duration)
x <- cbind(ETC_Modelling_More$KEYWORDVAR_O3, ETC_Modelling_More$KEYWORDVAR_R3, ETC_Modelling_More$Group_O3, ETC_Modelling_More$Department_O3, ETC_Modelling_More$Region_O3, ETC_Modelling_More$RequestType_O3, ETC_Modelling_More$CreatedDayofWeek_O3, ETC_Modelling_More$Subcategory_O3, ETC_Modelling_More$Item_O3, ETC_Modelling_More$Item_R3)
x1 <- cbind(ETC_Modelling_More$KEYWORDVAR_O3)

cor(y,x)

plot(y~x1, data = ETC_Modelling_More)

olsreg1 <- lm (y ~ x)
summary(olsreg1)
confint(olsreg1, level = 0.95)



head(ETC_Modelling,30)

setwd("D:/R Output files")
write.csv(ETC_Modelling,file="output.csv")
