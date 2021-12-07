Emissions <-read.csv("F:\\SCHOOL\\CHANG SCHOOL\\CIND820\\CO2\\CO2_Emissions_Canada.csv",header = TRUE)
#SUMMARY
summary(Emissions)

#TYPE OF ATTRIBUTES
str(Emissions)
##Renaming columns
names(Emissions)[names(Emissions) == "Engine.Size.L."] <- "Engine_Size_l"
names(Emissions)[names(Emissions) == "Fuel.Type"] <- "Fuel_Type"
names(Emissions)[names(Emissions) == "Fuel.Consumption.City..L.100.km."] <- "FC_City_l"
names(Emissions)[names(Emissions) == "Fuel.Consumption.Hwy..L.100.km."] <- "FC_Highway_l"
names(Emissions)[names(Emissions) == "Fuel.Consumption.Comb..L.100.km."] <- "FC_Combined_l"
names(Emissions)[names(Emissions) == "CO2.Emissions.g.km."] <- "CO2_Emissions_g/km"

#Transmission
#Automatic
Emissions$Transmission[Emissions$Transmission == "A4" ]<- "Automatic"
Emissions$Transmission[Emissions$Transmission == "A5" ]<- "Automatic"
Emissions$Transmission[Emissions$Transmission == "A6" ]<- "Automatic"
Emissions$Transmission[Emissions$Transmission == "A7" ]<- "Automatic"
Emissions$Transmission[Emissions$Transmission == "A8" ]<- "Automatic"
Emissions$Transmission[Emissions$Transmission == "A9" ]<- "Automatic"
Emissions$Transmission[Emissions$Transmission == "A10" ]<- "Automatic"
#Automated Manual
Emissions$Transmission[Emissions$Transmission == "AM5" ]<- "Automated Manual"
Emissions$Transmission[Emissions$Transmission == "AM6" ]<- "Automated Manual"
Emissions$Transmission[Emissions$Transmission == "AM7" ]<- "Automated Manual"
Emissions$Transmission[Emissions$Transmission == "AM8" ]<- "Automated Manual"
Emissions$Transmission[Emissions$Transmission == "AM9" ]<- "Automated Manual"
#Automatic With Select Shift
Emissions$Transmission[Emissions$Transmission == "AS4" ]<- "Automatic with Select Shift"
Emissions$Transmission[Emissions$Transmission == "AS5" ]<- "Automatic with Select Shift"
Emissions$Transmission[Emissions$Transmission == "AS6" ]<- "Automatic with Select Shift"
Emissions$Transmission[Emissions$Transmission == "AS7" ]<- "Automatic with Select Shift"
Emissions$Transmission[Emissions$Transmission == "AS8" ]<- "Automatic with Select Shift"
Emissions$Transmission[Emissions$Transmission == "AS9" ]<- "Automatic with Select Shift"
Emissions$Transmission[Emissions$Transmission == "AS10" ]<- "Automatic with Select Shift"
#Continuously Variable
Emissions$Transmission[Emissions$Transmission == "AV" ]<- "Continuously Variable"
Emissions$Transmission[Emissions$Transmission == "AV6" ]<- "Continuously Variable"
Emissions$Transmission[Emissions$Transmission == "AV7" ]<- "Continuously Variable"
Emissions$Transmission[Emissions$Transmission == "AV10" ]<- "Continuously Variable"
Emissions$Transmission[Emissions$Transmission == "AV8" ]<- "Continuously Variable"
#Manual
Emissions$Transmission[Emissions$Transmission == "M5" ]<- "Manual"
Emissions$Transmission[Emissions$Transmission == "M6" ]<- "Manual"
Emissions$Transmission[Emissions$Transmission == "M7" ]<- "Manual"

#Fuel Type
Emissions$Fuel_Type[Emissions$Fuel_Type == "Z" ]<- "Premium Gasoline"
Emissions$Fuel_Type[Emissions$Fuel_Type == "D" ]<- "Diesel"
Emissions$Fuel_Type[Emissions$Fuel_Type == "X" ]<- "Regular Gasoline"
Emissions$Fuel_Type[Emissions$Fuel_Type == "E" ]<- "Ethanol (E85)"
Emissions$Fuel_Type[Emissions$Fuel_Type == "N" ]<- "Natural Gas"

##MISSING VALUES
install.packages("DataExplorer") 
library(DataExplorer)
plot_str(Emissions)
plot_missing(Emissions)
plot_summary(Emissions)

na.omit(Emissions)
#Frequency 
plot_histogram(Emissions)

ggplot(Emissions, aes(Make)) +
  geom_bar(fill = "#0073C2FF")
ggplot(Emissions, aes(Model)) +
  geom_bar(fill = "#0073C2FF")
ggplot(Emissions, aes(Cylinders)) +
  geom_bar(fill = "#0073C2FF")
ggplot(Emissions, aes(CO2_Rating)) +
  geom_bar(fill = "#0073C2FF")


##OUTLIERS
#Observing outliers
library(ggplot2)
boxplot(Emissions$Engine_Size_L,
        col= (c("coral2", "darkcyan")),
        main= "Outliers",
        xlab= "Engine Size",
        border = c("darkcyan","coral2"))
boxplot(Emissions$Cylinders,
        col= (c("coral2", "darkcyan")),
        main= "Outliers",
        xlab= "Cylinders",
        border = c("darkcyan","coral2"))
boxplot(Emissions$`FC_City_L/100KM`,
        col= (c("coral2", "darkcyan")),
        main= "Outliers",
        xlab= "Fuel Consumption in City",
        border = c("darkcyan","coral2"))
boxplot(Emissions$`FC_Highway_L/100KM`,
        col= (c("coral2", "darkcyan")),
        main= "Outliers",
        xlab = "Highway Fuel Consumption",
        border = c("darkcyan","coral2"))
boxplot(Emissions$`CO2_Emissions_g/km`,
        col= (c("coral2", "darkcyan")),
        main= "Outliers",
        xlab = "CO2 Emissions",
        border = c("darkcyan","coral2"))



#Cleaning outliers through winsorizing


install.packages("robustHD")
library("robustHD")

Emissions$Engine_Size_l <- winsorize(Emissions$Engine_Size_l, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                     na.rm = FALSE, type = 7)

Emissions$Cylinders <- winsorize(Emissions$Cylinders, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                 na.rm = FALSE, type = 7)

Emissions$`FC_City_l` <- winsorize(Emissions$`FC_City_l` , minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                   na.rm = FALSE, type = 7)

Emissions$`CO2_Emissions_g/km`<- winsorize(Emissions$`CO2_Emissions_g/km` , minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                           na.rm = FALSE, type = 7)

Emissions$`FC_Highway_l`<- winsorize(Emissions$`FC_Highway_l` , minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                     na.rm = FALSE, type = 7)

Emissions$`FC_Combined_l`<- winsorize(Emissions$`FC_Combined_l` , minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                      na.rm = FALSE, type = 7)

Emissions$Fuel.Consumption.Comb..mpg. <- winsorize(Emissions$Fuel.Consumption.Comb..mpg. , minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                                   na.rm = FALSE, type = 7)


#changing character variables into factor
Emissions$Make <- as.factor(Emissions$Make)
Emissions$Model <- as.factor(Emissions$Model)
Emissions$Vehicle.Class <- as.factor(Emissions$Vehicle.Class)
Emissions$Transmission <- as.factor(Emissions$Transmission)
Emissions$Fuel_Type <- as.factor(Emissions$Fuel_Type)
Emissions$Cylinders <- as.factor(Emissions$Cylinders)
table(Emissions$Fuel_Type)

##CORRELATION

#Distribution by CO2 Emissions
install.packages("forcats")
library(ggplot2)
library(forcats)

boxplot(`CO2_Emissions_g/km` ~ fct_reorder(Transmission, `CO2_Emissions_g/km`), data = Emissions,
        main= "Distribution of Transmission by CO2",
        xlab = "Transmission",
        ylab = "CO2 Emissions",
        col= (c("coral2")))
boxplot(`CO2_Emissions_g/km` ~ fct_reorder(Make, `CO2_Emissions_g/km`), data = Emissions,
        main= "Distribution of Vehicle Make by CO2 Emissions",
        xlab = "Vehicle Make",
        ylab = "CO2 Emissions",
        col= (c("coral2")))
boxplot(`CO2_Emissions_g/km` ~ fct_reorder(Model, `CO2_Emissions_g/km`), data = Emissions,
        main= "Distribution of Vehicle Model by CO2 Emissions",
        xlab = "Vehicle Model",
        ylab = "CO2 Emissions",
        col= (c("coral2")))
boxplot(`CO2_Emissions_g/km` ~ fct_reorder(Fuel_Type, `CO2_Emissions_g/km`), data = Emissions,
        main = "Distribution of Fuel Type by CO2 Emissions",
        xlab = "Fuel Type",
        ylab = "CO2 Emissions",
        col= (c("coral2")))
boxplot(`CO2_Emissions_g/km` ~ fct_reorder(Fuel_Type, `CO2_Emissions_g/km`), data = Emissions,
        main = "Distribution of Fuel Type by CO2 Emissions",
        xlab = "Fuel Type",
        ylab = "CO2 Emissions",
        col= (c("coral2")))


install.packages("corrplot")
library("corrplot")
install.packages("DataExplorer") 

install.packages("corrplot")
library(corrplot)

install.packages("car")
library(car)
library(ggpubr)

Emissions$Engine_Size         
install.packages("dplyr")
library(dplyr)
library(ggplot2)
install.packages("corrr")
library(corrr)
install.packages("FSelector")
library(FSelector)
install.packages("caret")



cor(Emissions$Cylinders,Emissions$`CO2_Emissions_g/km`)
cor(Emissions$Engine.Size.L.,Emissions$CO2.Emissions.g.km.)

ggscatter(Emissions, x = "Cylinders_Clean", y = "CO2_Emissions_g/km",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Number of Cylinders", ylab = "CO2 Emissions g/km")

ggscatter(Emissions, x = "Engine_Size", y = "CO2_Emissions_g/km",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Engine Size", ylab = "CO2 Emissions g/km")
ggscatter(Emissions, x = "Transmission", y = "CO2_Emissions_g/km",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Transmission", ylab = "CO2 Emissions g/km")
ggscatter(Emissions, x = "Fuel_Type", y = "CO2_Emissions_g/km",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Transmission", ylab = "CO2 Emissions g/km")


##FACTOR SELECTION 

install.packages("MASS")
library(MASS)
install.packages("FSelector")
library(FSelector)


#CORRELATION BASED

data(Emissions)
Subset <- cfs(Emissions$`CO2_Emissions_g/km` ~.,Emissions)
f <- as.simple.formula(Subset, "`CO2_Emissions_g/km`")
summary(f)


#RECURSIVE FEATURE SELECTION

data(Emissions_scaled)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(Emissions_scaled[,1:6], Emissions_scaled[,7], sizes=c(1:11), rfeControl=control)
print(results)

#STEPWISE REGRESSION BOTH

res.lm <- lm(`CO2_Emissions_g/km` ~.,data = Emissions_scaled)
step <- stepAIC(res.lm, direction = "both", trace = FALSE)
step
summary(step)

##REGRESSION 

library(caret)
library(car)

#scaling data for normality 
Emissions_scaled <- as.data.frame(scale(Emissions[,c(4:5,8:12)]))


set.seed(1234)
reg_index <-createDataPartition(Emissions_scaled$`CO2_Emissions_g/km`,  p=0.8,list = FALSE)
reg_train <- Emissions_scaled[reg_index,]
reg_test <-Emissions_scaled[-reg_index,]
dim(reg_train)

lm_model <-lm(`CO2_Emissions_g/km`~.,data = reg_train)

print(lm_model)
summary(lm_model)


##CLASSIFICATION THROUGH DECISION TREE
install.packages("rpart.plot")
library(rpart.plot)


#CATEGORIZING CO2 INTO A,B,C,D,E rating 
Emissions_scaled$CO2_Rating = cut(Emissions_scaled$`CO2_Emissions_g/km`, breaks= c(0,150,250,350,450,550),
                               labels = c("1", "2","3","4","5"))

set.seed(1234)
ind <- sample(2,nrow(Emissions_scaled), replace = TRUE, prob = c(0.7, 0.3))
train.data <- Emissions_scaled[ind==1,]
test.data <- Emissions_scaled[ind==2,]
myf <- CO2_Rating ~ Engine.Size.L.+Fuel.Consumption.Comb..L.100.km.+Fuel.Consumption.City..L.100.km.+ Fuel.Consumption.Hwy..L.100.km.+Cylinders
Emissions_ctree <- ctree(myf, data = train.data)

# confusion matrix
result <- confusionMatrix(predict(Emissions_ctree), train.data$CO2_Rating)

rpart.plot(Emissions_ctree)
multi.class.model <- rpart(CO2_Rating ~., data = train.data)
rpart.plot(multi.class.model)

 



