library(ggplot2)
library(lattice)
library(caret)
library(repr)
library(tidyverse)
library(plyr)
library(dplyr)

# ************************************************

#Business_Analytics_Project-Lending_Club_Data_Analysis
#First_Objective - Reading_THe_Dataset
#Target Variable - Loan_status

# ************************************************
#Reading the Dataset
getwd()
dataset<-read.csv("Accepted_Loans_2018_1.csv")
dataset

# ************************************************

#Printing the number of rows and columns

#************************************************

print(paste("The number of rows  in the dataset are",nrow(dataset)))
print(paste("The number of COlumns  in the dataset are",ncol(dataset)))


#************************************************
#Exploring number of values in each category in target variable
library(repr)
options(repr.plot.width=5, repr.plot.height=5)
ggplot(dataset, aes(x =loan_status)) + geom_bar(color='blue',fill = "#FF6666")

#************************************************


# ************************************************

# Getting to know about the feautres(Columns) in  the dataset
# Installed Package - Tidyverse,dplyr
# This Pacakage is used for DataCleaning and Pre-Processing

# ************************************************

library(tidyverse)
categorical_dataset<-select_if(dataset,is.character)
numeric_dataset<-select_if(dataset,is.numeric)
print(paste("The number of categorical feautres  in the dataset are",ncol(categorical_dataset)))
print(paste("The number of numerical  feautres in the dataset are",ncol(numeric_dataset)))

# ************************************************[UPDATED_NEW]

#  we are keeping columns which have null values less than 30%.
null_values <- colMeans(is.na.data.frame(dataset))
null_values<-round(null_values*100,2)
keep_list=sort(null_values[null_values<=30])
keep_names<-names(keep_list)
length(keep_names)

#Remove columns from the datset which have more than 30% of null values
#Below Code contains columns which have less than 30% missing values
dataset_r<-subset(dataset,select = keep_names)
dataset_r<-as.data.frame(dataset_r)
dim(dataset_r)

# ************************************************

#Saving the names of categorical columns in catg_names
categorical_dataset<-select_if(dataset_r,is.character)

catg_names<-data.frame((names(categorical_dataset)))



#Saving the names of Numerical columns in num_names
numeric_dataset<-select_if(dataset_r,is.numeric)
num_names<-data.frame((names(numeric_dataset)))
num_names
# ************************************************
#Percentage-breaKup of null values of columns less than 30%
null_values <- colMeans(is.na.data.frame(dataset_r))
round(null_values*100,2)

# ************************************************
#On going through Data Dictionary,we found these columns to be relevant for data analysis


data<-subset(dataset_r,select=c('addr_state', 'annual_inc', 'application_type', 'dti', 
                              'earliest_cr_line', 'emp_length', 'emp_title', 'fico_range_high', 
                              'fico_range_low', 'grade', 'home_ownership', 'id', 
                              'initial_list_status', 'installment', 'int_rate', 
                              'issue_d', 'loan_amnt', 'loan_status', 'mort_acc', 
                              'open_acc', 'pub_rec', 'pub_rec_bankruptcies', 
                              'purpose', 'revol_bal', 'revol_util', 'sub_grade', 
                              'term', 'title', 'total_acc', 'verification_status', 'zip_code'))


# ************************************************

#Identifying the percentage of null values in each column

null_values <- colMeans(is.na.data.frame(data))
print(round(null_values*100,2))

#We can clearly see the percentage of null values is less  than 5%

# ************************************************

#FICO_Scores can be mathematically computed by appling the formula on fico_range_low and fico_range_high
data$fico_score = 0.5*data$fico_range_low + 0.5*data$fico_range_high

# ************************************************[UPDATED_NEW]
# In above columns,for further preprocessing, we are looking into columns which have more distinct values 
#in both categorical and numerical variables.We are also checking whether two categorical columns have cor-relation
#by using chisquare test.

#1
summary(data$id)

#The ID is not useful for modeling, either as a categorical 
#variable (there are too many distinct values) or as a numerical variable.So we will remove this column

#2
#There are two columns named grade and subgrade.We can remove either one of the column 
#if one has relation  with other column
#Let us look at possible values of subgrade

grade_count<-count(data,vars= "grade")
print(grade_count)

sub_grade_count<-count(data,vars= "sub_grade")
print(sub_grade_count)

library(MASS)       # load the MASS package [UPDATED_NEW_1]
#We load the chi_square test to see the relation between the two columns
#null Hypothesis - There is no relation between subgrade and grade column
#ALternate Hypothesis - There is corelation between subgrade and grade column at 0.95 sigificance level
tbl = table(data$grade, data$sub_grade) 
tbl
chisq_test_G_SG <- chisq.test(tbl) 

Print("We can see the grade and subgrade are clearly are related to each other as the p-value is less than 0.05 siginificance level",
       chisq_test_G_SG$p.value)

#As we can see the grade is implied by sub-grade,so we can drop off grade column

#3
# Now let us look at the similar kind of columns like zip_code,addr_state.
#Zipcode - The first 3 numbers of the zip code provided by the borrower in the loan application
#addr_state-The state provided by the borrower in the loan application

zipcode_count<-count(data,vars= "zip_code")
print(zipcode_count)
print(nrow(zipcode_count))

addr_state_count<-count(data,vars= "addr_state")
print(addr_state_count)
print(nrow(addr_state_count))


#We load the chi_square test to see the relation between the two columns[UPDATED_NEW_1]
#null Hypothesis - There is no relation between zip_code and addr_state column
#ALternate Hypothesis - There is corelation between zip_code and addr_state column at 0.95 sigificance level
tbl = table(data$zip_code, data$addr_state) 
tbl
chisq_test_Z_AD <- chisq.test(tbl) 
chisq_test_Z_AD

print("We can see the zipcode and address state are clearly are related to each other as the p-value is less than 0.05 siginificance level",
      chisq_test_Z_AD$p.value)

#let's just keep the state column.

#4
library(ggplot2)
library(ggpubr)

#Display the number of unique values in each category[UPDATED_NEW_1]
title_count<-count(data,vars= "title")
title_count<-as.data.frame(title_count)
#sort in Descending order
title_count<-title_count[order(-title_count$freq),]
title_count$prop<-round(title_count$freq*100/sum(title_count$freq))
# Visualization purposes we will see the top 5 that the borrower's title using dot charts
title_count_top5<-head(title_count)
ggplot(title_count_top5, aes(prop,title)) +
  geom_linerange(
    aes(y = title, xmin = 0, xmax = prop), 
    color = "lightgray", size = 1
  )+
  geom_point(aes(color = title), size = 2)+
  geom_text(aes(label = prop), vjust = -0.3)+
  ggpubr::color_palette("jco")+
  labs(y = "Percentage of each Borrower's Loan Title",x="Borrower's Loan Title")
  
# we can see the borrowers are puchasing the loan mainly for Debt Consolidation.

#There is relation between purpose column and title column.The categories appears to repeat in both of the columns
sub<-(subset(data,select=c("purpose","title")))
#first 10 records
sub[1:20,]


#We load the chi_square test to see the relation between the two columns
#null Hypothesis - There is no relation between purpose and title column
#ALternate Hypothesis - There is corelation between zip_code and addr_state column at 0.95 sigificance level
tbl = table(data$purpose, data$title) [UPDATED_NEW_1]
tbl
chisq_test_p_t <- chisq.test(tbl) 
chisq_test_p_t

print("We can see the purpose and title are clearly are related to each other as the p-value is less than 0.05 siginificance level",
      chisq_test_p_t$p.value)

# Now we can go ahead above mentioned columns.We derived ficoscore from fico_range_high and fico_range_low
#so we can remove these columns as well
data_new<-select(data,-c('id','grade','title','zip_code','fico_range_high', 'fico_range_low'))

# ************************************************

#Eventhough null values removed,there are some empty spaces in columns which went undetected 
#so we go ahead check these columns and try to remove it

table(data_new$emp_length)
#Removing the null value from emp_length column
data_new<-data_new[!(data_new$emp_length==""),]
sum(data_new$emp_length=="")
#Add YOur REference
na.omit(mutate_all(data_new, ~ifelse(. %in% c("N/A", "null", "",""),  NA, .)))
null_values <- colMeans(is.na.data.frame(data_new))
round(null_values*100,2)
dim(data_new)
summary(data_new)

# ************************************************
#Removing the null value from dti column
sum(is.na(data_new$dti))
data_new<- data_new[!is.na(data_new$dti), ]

#Removing the null value from revol_util
sum(is.na(data_new$revol_util))
data_new<- data_new[!is.na(data_new$revol_util), ]
# ************************************************

#IN emp_length variable,We consider changing the emp_length less than one year as 0 years above 10+ years as 11 years
data_new$emp_length[which(data_new$emp_length=="< 1 year")]<-"0 years"
data_new$emp_length[which(data_new$emp_length=="10+ years")]<-"11 years"
table(data_new$emp_length)

# ************************************************#[UPDATED_NEW]
#Manipulating the values certain columns and remving certain columns as a part of data cleaning

#Now we try to change charcter to numeric datatype in emp_length column by removing
#string "Years"
#1
string=data_new$emp_length
x<-str_extract(string, "\\d+")
table(x)
table(data_new$emp_length)
data_new$emp_length<-x
data_new$emp_length<-as.integer(data_new$emp_length) 

colnames(data_new)
emp_length_count<-count(data_new,vars= "emp_length")
emp_length_count<-as.data.frame(emp_length_count)
#sort in Descending order
emp_length_count[order(-emp_length_count$freq),]
print(emp_length_count)
ggplot(as.data.frame(emp_length_count), aes(freq,factor(emp_length))) +     
  geom_col(position = 'dodge')+
  geom_bar(stat="identity")+
  theme_minimal()

#We can see that people who have more than 9 years of experience tends to take loans more


#2 - same steps followed for earliest_cr_line(the month and year the borrower's earliest 
#reported credit line was opened)

#Here we are going to retain the year only.
table(data_new$earliest_cr_line)
string1=data_new$earliest_cr_line
x1<-str_extract(string1, "\\d+")
x1<-as.integer(x1)
data_new$earliest_cr_line<-x1
table(data_new$earliest_cr_line)

#Display the number of unique values in each category
earliest_cr_line_count<-count(data_new,vars= "earliest_cr_line")
emp_length_count<-as.data.frame(emp_length_count)
#sort in Descenbding order
emp_length_count[order(-emp_length_count$freq),]


#3term - The number of payments on the loan. Values are in months and can be either 36 or 60.
#We going to retain the number and drop of the string "month"
str(data_new)
string2=data_new$term
x2<-str_extract(string2, "\\d+")
x2<-as.integer(x2)
data_new$term<-x2

#Display the number of unique values in each category
term_count<-count(data_new,vars= "term")
term_count<-as.data.frame(term_count)
#sort in Descenbding order
term_count[order(-term_count$freq),]

ggplot(as.data.frame(term_count), aes(freq,factor(term))) +     
  geom_col(position = 'dodge')+
  geom_bar(stat="identity")+
  theme_minimal()

#4
emp_title_count<-count(data_new,vars= "emp_title")
emp_title_count<-as.data.frame(term_count)
emp_title_count[order(-emp_title_count$freq),]
print(nrow(emp_title_count))
#There are more number of unique values in the emp_title column.So  we can drop it

data_new<-subset(data_new,select= -c(emp_title))
dim(data_new)
data_new_charged_off<-data_new[which(data_new$loan_status=="Charged Off"),]
data_new_fully_paid<-data_new[which(data_new$loan_status=="Fully Paid"),]
plot(density(data_new$loan_amnt))
boxplot(data_new_charged_off$loan_amnt, data_new_fully_paid$loan_amnt,
        main = "Boxplots for comparision ",
        at = c(1,2),
        names = c("Charged_Off", "FUlly_Paid"),
        las = 2,
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)

data_new$loan_status[which(data_new$loan_status=="Fully Paid")]<-1
data_new$loan_status[which(data_new$loan_status=="Charged Off")]<-0
data_new$loan_status<-as.integer(data_new$loan_status)
# ************************************************
#Checking the subcategories in the categorical column
categorical_cols<-select_if(data_new,is.character)
f<-function(x){length(unique(x))}
nunique_catg<-sort(sapply(categorical_cols,f))
nunique_catg

# ************************************************
#Checking the correlation among the numerical columns
num<-select_if(data_new,is.numeric)
t(colnames(num))#gives the index values of every column
findCorrelation(cor(num), cutoff = .65, verbose = TRUE)#gives a matrix of correlation among every column
#We can see (loan_amnt,installment), (open_acc,total_acc), (pub_rec,pub_rec_bankruptcies) are the highly correlated pairs in our dataset
cor(num$loan_amnt,num$installment)
cor(num$open_acc,num$total_acc)
cor(num$pub_rec,num$pub_rec_bankruptcies)
#So, inorder to remove the multi collinearity we need to drop a column from every pair
data_new<-select(data_new,-c(loan_amnt,open_acc,pub_rec_bankruptcies))
str(data_new)


# ************************************************

#Removing  outliers from numerical columns.
num<-select_if(data_new,is.numeric)
names(num)

#Number of NUmerical columns(input variables) is reducd to 13.We cheched outliers on every column.
#we are removing the outliers only on   the  certain  columns which have extreme ouliers.
#
#
#Removing the Outliers
#1

#Visualize the numerical dataset variable 
boxplot(data_new$annual_inc)
#Display OUtliers
boxplot.stats(data_new$annual_inc)
#removing only the extreme outliers as it might generate noise in the data
quantiles <- quantile(data_new$annual_inc, c(.99))
data_new_1 <- filter(data_new,(annual_inc<= quantiles[1]))
#visualize the boxplot after outlier removal
boxplot(data_new_1$annual_inc)

#2
#Visualize the numerical dataset variable 
boxplot(data_new_1$pub_rec)
#Display OUtliers
boxplot.stats(data_new_1$pub_rec)

# pub_rec - Public records and collections are derogatory items because they reflect financial obligations 
#that were not paid as agreed.
#Most Borrowers have zero public record or none
#We will remove only outlier who have more than 5 Public records

data_new_1<-filter(data_new_1,(pub_rec<= 5))
#visualize the boxplot after outlier removal

boxplot(data_new_1$pub_rec)

#3
#Visualize the numerical dataset variable 
boxplot(data_new_1$dti)

#removing only the extreme outliers as it might generate noise in the data
quantiles <- quantile(data_new_1$dti, c(.999))
data_new_1 <- filter(data_new_1,(dti<= quantiles[1]))
#visualize the boxplot after outlier removal
boxplot(data_new_1$dti)


#4
boxplot(data_new_1$revol_bal)
boxplot.stats(data_new_1$revol_bal)
#removing only the extreme outliers as it might generate noise in the data
quantiles <- quantile(data_new_1$revol_bal, c(.99))
data_new_1 <- filter(data_new_1,(revol_bal<= quantiles[1]))
#visualize the boxplot after outlier removal
boxplot(data_new_1$revol_bal)

#5
boxplot(data_new_1$mort_acc)

boxplot.stats(data_new_1$mort_acc)
#removing only the extreme outliers as it might generate noise in the data

quantiles <- quantile(data_new_1$mort_acc, c(.999))
data_new_1 <- filter(data_new_1,(mort_acc<= quantiles[1]))
#visualize the boxplot after outlier removal.
boxplot(data_new_1$mort_acc)

print(paste(("Total number of records removed : "), nrow(data_new)-nrow(data_new_1)))
dim(data_new_1)


########<<<<<<------PREPROCESSINGENDSHERE------->>>>>>>>>>>######

DF <- data_new_1
DF$str(DF)
#Train Test Split
DF$loan_status<-as.factor(DF$loan_status)#converting the target variable as factor because most of the models expects target variable as a factor
train <- sample(1:nrow(DF),size = ceiling(0.70*nrow(DF)),replace = FALSE)#splitting the data to 70 and 30 percent
#train set
df_train <- DF[train,]
#test set
df_test <- DF[-train,]
str(df_train)
str(df_test)
########<<<<<<------Model building------->>>>>>>>>>>######
library(randomForest)

set.seed(71)
rf <-randomForest(loan_status~.,data=df_train, ntree=500) #fitting the random forest model to rf
print(rf)
grep("loan_status", colnames(df_test))#to find the index position of df_test 
pred = predict(rf, newdata=df_test[-12])
cm = table(df_test[,12], pred)
cm
#We can see our model is correctly predicting one class and its not predicting the other class as our data set is imbalanced.
#So we need to balance the data by sampling methods

set.seed(71)
rf <-randomForest(loan_status~.,data=df_train,importance=TRUE,ntree=500)
print(rf)
#Evaluate variable importance
importance(rf)
varImpPlot(rf)
#As we can see addr_state is the least important variable in our analysis we can drop it
data_new<-select(data_new,-c(addr_state))

#Encoding the categorical variables
DF_1<-data_new
dmy <- dummyVars(" ~ .", data = DF_1, fullRank = T)#assigns dummy variables to categorical data
data_enc <- data.frame(predict(dmy, newdata = DF_1))#The above dummy variables are assigned to our dataframe
colnames(data_enc)

#Smote Analysis for balancing the target variable
library(DMwR)
data_enc$loan_status<-as.factor(data_enc$loan_status)
data_enc<- SMOTE(loan_status ~ ., data_enc, perc.over = 100, perc.under=200)
table(data_enc$loan_status)# as we can see it is balanced now

# train test split
train <- sample(1:nrow(data_enc),size = ceiling(0.70*nrow(data_enc)),replace = FALSE)
#train set
df_train <- data_enc[train,]
#table(df_train$loan_status)
#test set
df_test <- data_enc[-train,]

########<<<<<<------Random Forest Model------->>>>>>>>>>>######
rf <-randomForest(loan_status~.,data=df_train, ntree=500) 
print(rf)
grep("loan_status", colnames(df_test))
pred = predict(rf, newdata=df_test[-17])
cm = table(df_test[,17], pred)
cm
confusionMatrix(cm)
accuracy<-sum(diag(cm))/sum(cm)
print(paste('Accuracy on test dataset using Random Forest model is ', accuracy*100))

########<<<<<<------SVM Model------->>>>>>>>>>>######

library(e1071) 
classifier = svm(loan_status ~ .,data = df_train, 
                 type = 'C-classification', 
                 kernel = 'polynomial') 

test<-select(df_test,-c(loan_status))
y_pred = predict(classifier, newdata = test) 
cm = table(df_test$loan_status, y_pred) 
cm
confusionMatrix(cm)
accuracy<-sum(diag(cm))/sum(cm)
print(paste('Accuracy on test dataset using SVM model is ', accuracy*100))

cm

########<<<<<<------SVM Model------->>>>>>>>>>>######



library("FactoMineR")
res<-FAMD (data_new_1, ncp = 5, sup.var = NULL, ind.sup = NULL, graph = TRUE)

########<<<<<<------SVM Model------->>>>>>>>>>>######
set.seed(123)
train.control <- trainControl(method = "repeatedcv",number = 10, repeats = 3)

# Train the model
model <- train(loan_status ~., data = DF, method = classifier,
               trControl = train.control)
# Summarize the results
print(model)


#>>>>>>>>>>********************************************<<<<<<<<<<<<<<<<<<<<<<<<
  


