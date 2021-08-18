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
