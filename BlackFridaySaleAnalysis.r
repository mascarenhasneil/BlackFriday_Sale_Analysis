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


# VISUALIZATION

ggplot(Sam_Cleaned_BlackFriday, 
       aes(Gender,
           Product_Category,
           fill=Gender))+geom_col(width=1)+facet_wrap(~Age)+labs(title ="Age Vs Gender Vs Product")


# Summarize by City - To focus which City generates more revenue and also avg purchase per shopper
avg_district = as.data.frame(Cleaned_BlackFriday %>% 
                                 group_by(City_Category) %>% 
                                 dplyr:: summarise(`Product Revenue` = sum(Purchase), 
                                                   `Number of Distinct shoppers` = n_distinct(`User_ID`), 
                                                   `Avg. Purchase per Shopper` = `Product Revenue`/`Number of Distinct shoppers`, 
                                                   `Number of Product` = n(),  
                                                   `Avg. Unit Price per Product` = `Product Revenue`/`Number of Product`,
                                                   `Product per Shopper` = round(`Number of Product`/`Number of Distinct shoppers`,0)))




## I am trying to summarize by the city - To get to know which City is generating more revenues and what is the average purchase per user.






p1 = Sam_Cleaned_BlackFriday %>% filter(City_Category == 'A') %>% group_by(City_Category,Age) %>% summarise(n = n_distinct(User_ID)) %>%mutate(prop = percent(n/1045)) %>% ggplot(aes(x="", y=n, fill=Age)) + geom_bar(width = 1, stat = 'identity', position= "stack") + coord_polar("y") + geom_text(aes(label = prop), position = position_stack(vjust = 0.8), size = 3) + theme_void() + theme(plot.title = element_text(size=14, face="bold"), axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), axis.title=element_blank(), legend.title = NULL, legend.position="bottom") + labs(subtitle = "City A")+ labs(tag = 'Figure 1\n') + labs(title = 'Age Distribution for Each City')

p2 = Sam_Cleaned_BlackFriday %>% filter(City_Category == 'B') %>% group_by(City_Category,Age) %>% summarise(n = n_distinct(User_ID)) %>%mutate(prop = percent(n/1707)) %>% ggplot(aes(x="", y=n, fill=Age)) + geom_bar(width = 1, stat = 'identity', position= "stack") + coord_polar("y") + geom_text(aes(label = prop), position = position_stack(vjust = 0.8), size = 3) + theme_void() + theme(plot.title = element_text(size=14, face="bold"), axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), axis.title=element_blank(), legend.title = NULL, legend.position="bottom") + labs(subtitle = "City B")

p3 = Sam_Cleaned_BlackFriday %>% filter(City_Category == 'C') %>% group_by(City_Category,Age) %>% summarise(n = n_distinct(User_ID)) %>%mutate(prop = percent(n/3139)) %>% ggplot(aes(x="", y=n, fill=Age)) + geom_bar(width = 1, stat = 'identity', position= "stack") + coord_polar("y") + geom_text(aes(label = prop), position = position_stack(vjust = 0.8), size = 3) + theme_void() + theme(plot.title = element_text(size=14, face="bold"), axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), axis.title=element_blank(), legend.title = NULL, legend.position="bottom") + labs(subtitle = "City C")



Pie1 = Sam_Cleaned_BlackFriday %>% filter(City_Category == 'A') %>% group_by(City_Category,Age) %>% summarise(n = n_distinct(User_ID))  %>%                                  ggplot(aes(x="", y=n, fill=Age)) + geom_bar(width = 1, stat = 'identity', position= "stack") + coord_polar("y") +                                                                                  theme_void() + theme(plot.title = element_text(size=14, face="bold"), axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), axis.title=element_blank(), legend.title = NULL, legend.position="bottom") + labs(subtitle = "City A")+ labs(tag = 'Figure 1\n') + labs(title = 'Age Distribution for Each City')

Pie2 = Sam_Cleaned_BlackFriday %>% filter(City_Category == 'B') %>% group_by(City_Category,Age) %>% summarise(n = n_distinct(User_ID))  %>% ggplot(aes(x="", y=n, fill=Age)) + geom_bar(width = 1, stat = 'identity', position= "stack") + coord_polar("y") +  theme_void() + theme(plot.title = element_text(size=14, face="bold"), axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), axis.title=element_blank(), legend.title = NULL, legend.position="bottom") + labs(subtitle = "City B")

Pie3 = Sam_Cleaned_BlackFriday %>% filter(City_Category == 'C') %>% group_by(City_Category,Age) %>% summarise(n = n_distinct(User_ID))  %>% ggplot(aes(x="", y=n, fill=Age)) + geom_bar(width = 1, stat = 'identity', position= "stack") + coord_polar("y") +  theme_void() + theme(plot.title = element_text(size=14, face="bold"), axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), axis.title=element_blank(), legend.title = NULL, legend.position="bottom") + labs(subtitle = "City C")



ggarrange(p1,p2,p3, ncol = 3, nrow = 1, common.legend = TRUE, legend = 'bottom', align = 'v', widths = c(1, 1, 1))
ggarrange(p1, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom', align = 'v', widths = c(5, 5, 5))


Counts <- count(Cleaned_BlackFriday,vars="Age")



class(Cleaned_BlackFriday)

Countshead <- arrange(Counts, desc(Counts$freq))
Countshead <
    class(Countshead)
Countstail <- tail(Counts,30)
Countstail



barplot(Countshead, col = "cyan4", main = "userid top 10 and bottom 10 frequency", las = "2",cex.names=0.6)

arrange(amazon, desc(amazon$User_ID))
user123


df1 = Cleaned_BlackFriday %>% group_by(User_ID) %>% dplyr::summarise(`Shopper Purchase` = sum(Purchase),`Number of Products Sold` = n())
head(df1)
blackfriday_user = select(Cleaned_BlackFriday, User_ID, Gender, Age, City_Category, Marital_Status, Occupation)
blackfriday_user = unique(blackfriday_user)
blackfriday_user = blackfriday_user %>% left_join(df1, by = 'User_ID')
head(blackfriday_user)

nrow(blackfriday_user)


# User_ID by Product Category: to see the rev. for each category
df2 = Cleaned_BlackFriday %>% group_by(User_ID,Product_Category) %>% dplyr::summarise(`User Purchase` = sum(Purchase),`Number of Products Sold` = n())
User_Category = select(Cleaned_BlackFriday, User_ID, Gender, Age, City_Category, Marital_Status, Occupation)
User_Category = unique(User_Category)
User_Category = User_Category %>% left_join(df2, by = 'User_ID')

head(User_Category)
nrow(User_Category)

checkusr<- Cleaned_BlackFriday %>% group_by(City_Category) %>% dplyr::summarise(`Cat count` = Purchase/City_Category)

n_distinct(Cleaned_BlackFriday$Product_ID)

CityCat <- c("City ", "Suburbs", "Countyside")
names(CityCat) <- c("1","2","3")

#0-17  15102
#2 18-25  99660
#3 26-35 219587
#4 36-45 110013
#5 46-50  45701
#6 51-55  38501
#7   55+  21504


# hi i think looking at the Graph we can Hypothesise the purchase by age and city -
blackfriday_user %>% group_by(City_Category, Age, Gender) %>% dplyr::summarise(n=n(), purchase = sum(`Shopper Purchase`) ,avg = sum(`Shopper Purchase`)/n) %>% 
    ggplot(aes(x =Age, y = avg/100000, fill=factor(Gender,labels=c("0"="Male", "1"="Female")))) + 
    geom_col(position="stack") + facet_grid(~City_Category, labeller = labeller(City_Category = CityCat)) + 
    labs(title = "Average Purchase Comparison  \nby Age and City", tag = '', x="Age Groups", y= "Purchase in Million($)", fill="Gender")  + ##scale_x_discrete(label=c(17="0-17",25="18-25","35"="26-35","45"="36-45","50"="46-50","55"="51-55","55+")) +
    theme(plot.title = element_text(size=14, face="bold"), axis.text.x = element_text(angle=60, hjust=1), axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"), legend.position="bottom") + scale_fill_brewer(palette="Dark2")

unique(Sam_Cleaned_BlackFriday$Age)
#####################################

blackfriday_user %>% group_by(City_Category, Age, Gender) %>% dplyr::summarise(n=n(), DistU=n_distinct('User_ID')) %>% 
    ggplot(aes(x =Age, y = n, fill=factor(Gender,labels=c("0"="Male", "1"="Female")))) + 
    geom_col(position="stack") + facet_grid(~City_Category, labeller = labeller(City_Category = CityCat)) + 
    labs(title = "All Users Distribution \nby Age and City", tag = '', x="Age Groups", y= "Distinct Users", fill="Gender")  + ##scale_x_discrete(label=c(17="0-17",25="18-25","35"="26-35","45"="36-45","50"="46-50","55"="51-55","55+")) +
    theme(plot.title = element_text(size=14, face="bold"), axis.text.x = element_text(angle=60, hjust=1), axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"), legend.position="bottom") + scale_fill_brewer(palette="Paired")

table(blackfriday_user$City_Category,blackfriday_user$User_ID)

#================------------------

malesPurchaserData = Sam_Cleaned_BlackFriday[Sam_Cleaned_BlackFriday['Gender'] == 0]
malesPurchaseMean = np.mean(malesPurchaserData['Purchase'])
print("Purchase mean for male purchasers = ",malesPurchaseMean)

femalsPurchaserData = data_train.loc[data_train['Gender'] == 'F']
femalsPurchaseMean = np.mean(femalsPurchaserData['Purchase'])
print("Purchase mean for femal purchasers = ",femalsPurchaseMean)




getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

result <- 
    getmode(Cleaned_BlackFriday$Age)

getmode(Cleaned_BlackFriday$Purchase)
getmode(Cleaned_BlackFriday)




sum(Cleaned_BlackFriday$City_Category)



p1 = Cleaned_BlackFriday %>% filter(City_Category == '1') %>% group_by(City_Category,Age) %>% summarise(n = n_distinct(User_ID)) %>% mutate(prop = percent(n/1045)) %>% 
    ggplot(aes(x="", y=n, fill=NULL),inherit.aes = FALSE) + 
    geom_bar(width = 1, stat = 'identity', position= "stack") + 
    coord_polar("y") +   
    geom_text(aes(label = prop), position = position_stack(vjust = 0.8), size = 3)+theme_void()+ theme(plot.title = element_text(size=14, face="bold"),axis.text = element_blank(),axis.ticks = element_blank(),panel.grid  = element_blank(),axis.title=element_blank(), legend.title = NULL,legend.position="bottom") +labs(tag = 'Figure 1\n') +labs(subtitle = "City A")

p2= blackfriday_clean %>% filter(City_Category == 'B') %>% group_by(City_Category,Age) %>% summarise(n = n_distinct(User_ID)) %>%mutate(prop = percent(n/1707)) %>% ggplot(aes(x="", y=n, fill=Age)) + geom_bar(width = 1, stat = 'identity', position= "stack") + coord_polar("y") +geom_text(aes(label = prop), position = position_stack(vjust = 0.8), size = 3)+theme_void()+ theme(plot.title = element_text(size=14, face="bold"),axis.text = element_blank(),axis.ticks = element_blank(),panel.grid  = element_blank(),axis.title=element_blank(),legend.position="bottom") +labs(title = 'Age Distribution for Each City',subtitle = "City B")

p3=blackfriday_clean %>% filter(City_Category == 'C') %>% group_by(City_Category,Age) %>% summarise(n = n_distinct(User_ID)) %>%mutate(prop = percent(n/3139)) %>% 
    ggplot(aes(x="", y=n, fill=Age)) + 
    geom_bar(width = 1, stat = 'identity', position= "stack") + coord_polar("y") +
    geom_text(aes(label = prop), position = position_stack(vjust = 0.8), size = 3)+ theme_void() +
    theme(plot.title = element_text(size=14, face="bold"),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid  = element_blank(), 
          axis.title=element_blank(), legend.position="bottom")  +
    labs(subtitle = "City C")

ggarrange(p1,p2,p3, ncol = 3, nrow = 1, common.legend = TRUE, legend = 'bottom', align = 'v', widths = c(1, 1, 1))




sd()

age35 <- group_by(City_Category,Age) Cleaned_BlackFriday$Age["26-35"]



gender=as.data.frame(blackfriday_clean %>% group_by(Gender) %>% summarise(`Number of Distinct shoppers` = n_distinct(`User_ID`)))
knitr::kable(as.data.table(gender), caption = "Table 1:  Gender")



# Summarize by City - To focus which City generates more revenue and also avg purchase per shopper
avg_district = as.data.frame(Cleaned_BlackFriday %>% 
                                 group_by(City_Category) %>% 
                                 dplyr:: summarise(`Product Revenue` = sum(Purchase), 
                                                   `Number of Distinct shoppers` = n_distinct(`User_ID`), 
                                                   `Number of Product` = n(),  
                                                   `Avg. Unit Price per Product` = `Product Revenue`/`Number of Product`,
                                                   `Product per Shopper` = round(`Number of Product`/`Number of Distinct shoppers`,0)))
knitr::kable(as.data.table(avg_district), caption = "Summary by Each City")

nrow(Cleaned_BlackFriday$User_ID="1000019")

nrow(count(Cleaned_BlackFriday$User_ID, Cleaned_BlackFriday$City_Category))

nrow(count(Cleaned_BlackFriday$City_Category))

Agecol <- table(Cleaned_BlackFriday$Age)

barplot(Agecol, col = "cyan4",  main = "Age Group Buying Most", xlab = "Age", ylab = "Purchases")

head(Cleaned_BlackFriday)

glimpse(Cleaned_BlackFriday)
