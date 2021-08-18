# TO check Working Dir
getwd()
# Set the correct WD
setwd("~/../git/BlackFriday_Sale_Analysis")

## ADDING ALL IMP Packages
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("plyr")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("dslabs")
install.packages("ggpubr")
install.packages("ISLR")
install.packages("yrdpl")
install.packages("rmarkdown")
install.packages("data.table", type="source", dependencies=TRUE)
install.packages("corrplot")
install.packages("caTools")
install.packages("Caret")



# sUing thme 
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(ggpubr)
library(ISLR)
library(rmarkdown)
library(data.table)
library(funModeling) 
library(corrplot)
library(Hmisc)
library(caTools)
library(caret)
library(rpart)
library(glmnet)
library(e1071)



#Importing data set CSV to Data set

OblackFriday <- as.data.frame(read.csv(file = "Data/blackfriday.csv"))
str(OblackFriday)
as.data.frame(names(OblackFriday),str(OblackFriday)[2])

dim(OblackFriday)

str(OblackFriday)

head(OblackFriday)

nrow(OblackFriday)

sum(complete.cases(OblackFriday))



#We find there are below no of rows which aare not NA that is missing data. to furhet analyssis we need to clean the data and proceed.
# We need to drop the columns so to avoid incorrect analysis.


FilterBlackFriday <-  dplyr::select(OblackFriday, 
                                    -Stay_In_Current_City_Years,
                                    -Product_Category_2, 
                                    -Product_Category_3)

head(FilterBlackFriday)

# Factoring each column to have limited number of different values

table(str(FilterBlackFriday))

FilterBlackFriday$Product_ID = factor(FilterBlackFriday$Product_ID)

FilterBlackFriday$Gender = factor(FilterBlackFriday$Gender)

FilterBlackFriday$Age = factor(FilterBlackFriday$Age)

FilterBlackFriday$Occupation = factor(FilterBlackFriday$Occupation)

FilterBlackFriday$City_Category = factor(FilterBlackFriday$City_Category)

FilterBlackFriday$Stay_In_Current_City_Years  = factor(FilterBlackFriday$Stay_In_Current_City_Years )

FilterBlackFriday$Marital_Status = factor(FilterBlackFriday$Marital_Status)

FilterBlackFriday$Product_Category_1 = factor(FilterBlackFriday$Product_Category_1 )

FilterBlackFriday$Purchase = factor(FilterBlackFriday$Purchas)


# Renameing colums for better understaning

#names(FilterBlackFriday)[7] <- 'Years_InCurr_City'
names(FilterBlackFriday)[8] <- 'Product_Category'

head(FilterBlackFriday)
str(FilterBlackFriday)

count(FilterBlackFriday$Age)



class(FilterBlackFriday)

Cleaned_BlackFriday <- FilterBlackFriday

Cleaned_BlackFriday <- subset(Cleaned_BlackFriday, 
                              select = -c(Years_InCurr_City))

Cleaned_BlackFriday$User_ID <- as.numeric(FilterBlackFriday$User_ID)

Cleaned_BlackFriday$Product_ID <- as.numeric(FilterBlackFriday$Product_ID)

Cleaned_BlackFriday$Gender <- as.numeric(ifelse(FilterBlackFriday$Gender=="M", 1, 0))

Cleaned_BlackFriday$Age <- as.numeric(ifelse(FilterBlackFriday$Age=='0-17', 17, 
                                             ifelse(FilterBlackFriday$Age=='18-25', 25, 
                                                    ifelse(FilterBlackFriday$Age=='26-35', 35, 
                                                           ifelse(FilterBlackFriday$Age=='36-45', 45, 
                                                                  ifelse(FilterBlackFriday$Age=='46-50', 48, 
                                                                         ifelse(FilterBlackFriday$Age=='51-55', 55, 62)))))))

Cleaned_BlackFriday$Years_InCurr_City <- as.numeric(FilterBlackFriday$Years_InCurr_City)


Cleaned_BlackFriday$Marital_Status <- as.numeric(FilterBlackFriday$Marital_Status)

Cleaned_BlackFriday$Occupation <- as.numeric(FilterBlackFriday$Occupation)

Cleaned_BlackFriday$City_Category <- as.numeric(ifelse(FilterBlackFriday$City_Category=='A', 1, 
                                                       ifelse(FilterBlackFriday$City_Category=='B', 2, 3)))

Cleaned_BlackFriday$Product_Category <- as.numeric(FilterBlackFriday$Product_Category)

Cleaned_BlackFriday$Purchase <- as.numeric(FilterBlackFriday$Purchase)


head(Cleaned_BlackFriday)

str(Cleaned_BlackFriday)

summary(Cleaned_BlackFriday)
as.data.frame(names(OblackFriday))

as.data.frame(names(Cleaned_BlackFriday))


length(unique(Cleaned_BlackFriday$User_ID))

str(Cleaned_BlackFriday)

count(Cleaned_BlackFriday$Age)
count(Cleaned_BlackFriday$Occupation)
count(Cleaned_BlackFriday$City_Category)
count(Cleaned_BlackFriday$Years_InCurr_City)
count(Cleaned_BlackFriday$Marital_Status)
count(Cleaned_BlackFriday$Product_Category)
count(Cleaned_BlackFriday$Purchase)


nrow(Cleaned_BlackFriday)

is.factor(FilterBlackFriday$User_ID)

length(FilterBlackFriday$User_ID)

# Export the dataset

write.csv(Cleaned_BlackFriday,'Data/Cleaned_BlackFriday.csv')




#userVsAge <- subset(Cleaned_BlackFriday,select= c(Age))

userVsAge <- table(Cleaned_BlackFriday$Age)
userVsAge

barplot(userVsAge,
        main = "User count per Age",
        ylab = "Counts",
        cex.names = 1, 
)

PurchasePerAge <- table(subset(Cleaned_BlackFriday,
                               select = c(Purchase,Age)))


barplot(PurchasePerAge,
        main = "Purchase per Age",
        ylab = "Counts",
        cex.names = 1, 
)

n_distinct(Cleaned_BlackFriday$Occupation)

nrow(Cleaned_BlackFriday)


Sam_Cleaned_BlackFriday <- sample_n(Cleaned_BlackFriday, 
                                    100000, 
                                    replace = TRUE, 
                                    prob = NULL)
nrow(Sam_Cleaned_BlackFriday)

corrplot(cor(Cleaned_BlackFriday), method="circle")
corrplot(cor(Cleaned_BlackFriday), method="color", order = "AOE")


## making sure the varity is not changed drastically

n_distinct(Cleaned_BlackFriday$User_ID) 
n_distinct(Sam_Cleaned_BlackFriday$User_ID)

n_distinct(Cleaned_BlackFriday$Product_ID)
n_distinct(Sam_Cleaned_BlackFriday$Product_ID)

n_distinct(Cleaned_BlackFriday$Gender)
n_distinct(Sam_Cleaned_BlackFriday$Gender)

n_distinct(Cleaned_BlackFriday$Age)
n_distinct(Sam_Cleaned_BlackFriday$Age)

n_distinct(Cleaned_BlackFriday$Occupation)
n_distinct(Sam_Cleaned_BlackFriday$Occupation)

n_distinct(Cleaned_BlackFriday$City_Category)
n_distinct(Sam_Cleaned_BlackFriday$City_Category)

n_distinct(Cleaned_BlackFriday$Marital_Status)
n_distinct(Sam_Cleaned_BlackFriday$Marital_Status)

n_distinct(Cleaned_BlackFriday$Product_Category)
n_distinct(Sam_Cleaned_BlackFriday$Product_Category)

n_distinct(Cleaned_BlackFriday$Purchase)
n_distinct(Sam_Cleaned_BlackFriday$Purchase)


str(Sam_Cleaned_BlackFriday)

write.csv(Sam_Cleaned_BlackFriday,'Data/Sampled_Cleaned_BlackFriday.csv')

