# ---------------------------------------------------#
# Code Description : PGDDA - HR Case Study           #
# Author           : Soumadiptya     (DDA1710089)    #
#                  : Ranjidha Rajan  (DDA1710198)    #
#                  : Nihar Behara    (DDA1710367)    #
#                  : Anugraha Sinha (DDA1710381)     #
#----------------------------------------------------#

# ---- SETUP ---- #
# Library Import Segment
required_packages <- c("dplyr","tidyr","lubridate","stringr","ggplot2","gridExtra","corrplot","caTools","MASS","car",
                       "caret","pROC")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(caTools)
library(MASS)
library(car)
library(caret)
library(pROC)

# Working Directory
# I am assuming that the working directory will be set where unzip would have been done
# Hence removing the below. If required, uncomment below line and modify the argument
# if setwd() function as per need.


# setwd("C:/Upgrad/")

setwd("C:/Users/anugraha.sinha/OneDrive/Documents/Post Graduate Data Sciences IIIT-Bangalore Upgrad/Main Program/3_Predictive_Analytics-1/HR Group Case Study/PA-I_Case_Study_HR_Analytics")


emp_survey <- read.csv("employee_survey_data.csv",stringsAsFactors = FALSE)
man_survey <- read.csv("manager_survey_data.csv",stringsAsFactors = FALSE)
in_time <- read.csv("in_time.csv",stringsAsFactors = FALSE)
out_time <- read.csv("out_time.csv",stringsAsFactors = FALSE)
general_data <- read.csv("general_data.csv",stringsAsFactors = FALSE)

# ---- Setup Complete ---- #


# ---- CRISP-DM Framework - Stage 1 - Business Understanding (through comments) ---- #
# Question/Objective : Analyze the data given (in multiple CSV files) to understand
#                      a) Important factors which effect attrition
#                      b) Probability attrition for employees based on past data
#                         The attrition column in general_data (df) will provide
#                         information whether the employee left or not. Therefore,
#                         when we build our train/test data set, we should make it a point
#                         we use sample.split according to this column.
# Data given :
# 1) Manager Survey Data : By Employee ID
# 2) Employee Survey Data : By Employee ID
# 3) General Data about employee : By Employee ID
# 4) In time and out time : By Employee ID
#
# Primary linkage key between all the data : Employee ID
#
# Information about individual columns are given in Data dictionary, so not including it
# here.

# ---- CRISP-DM Framework - Stage 2 & 3 - Data Understanding & Data Preparation ---- #

# Working on in_time and out_time #
# 1) First correct the column names of in_time and out_time
colnames(in_time)[1] <- "EmployeeID"
colnames(in_time) <- gsub(pattern="X","",colnames(in_time))
      
colnames(out_time)[1] <- "EmployeeID"
colnames(out_time) <- gsub(pattern="X","",colnames(out_time))
      
# 2) Lets check whether all column names match in both in_time and out_time
which(!colnames(in_time) == colnames(out_time))
# 0
      
# Now we have data of dates matched in in_time and out_time data frame
      
# 3) Check uniqueness of employee IDs in both in_time and out_time
nrow(in_time) == length(unique(in_time$EmployeeID))
# TRUE
nrow(out_time) == length(unique(out_time$EmployeeID))
# TRUE
          
# 4) Now lets check for which all days have NAs in in_time and out_time
# in_time data set
colnames(in_time)[which(sapply(in_time,function(x) sum(is.na(x))) == nrow(in_time))]
# [1] "2015.01.01" "2015.01.14" "2015.01.26" "2015.03.05" "2015.05.01" "2015.07.17" "2015.09.17" "2015.10.02" "2015.11.09" "2015.11.10"
# [11] "2015.11.11" "2015.12.25"
# out_time data set
colnames(out_time)[which(sapply(out_time,function(x) sum(is.na(x))) == nrow(out_time))]
# [1] "2015.01.01" "2015.01.14" "2015.01.26" "2015.03.05" "2015.05.01" "2015.07.17" "2015.09.17" "2015.10.02" "2015.11.09" "2015.11.10"
# [11] "2015.11.11" "2015.12.25"
          
# The above can be considered as holidays, and we can straight away remove them from in_time and out_time
in_time <- in_time %>% dplyr::select(-c(which(sapply(in_time,function(x) sum(is.na(x))) == nrow(in_time))))
out_time <- out_time %>% dplyr::select(-c(which(sapply(out_time,function(x) sum(is.na(x))) == nrow(out_time))))
          
# 5) Building up the total time spent by employees on each day in the office
in_time_epoch <- as.data.frame((sapply(c(2:ncol(in_time)),function(x) ymd_hms(in_time[,x]))))
colnames(in_time_epoch) <- colnames(in_time)[-1]
          
out_time_epoch <- as.data.frame((sapply(c(2:ncol(out_time)),function(x) ymd_hms(out_time[,x]))))
colnames(out_time_epoch) <- colnames(out_time)[-1]

# Lets calculate the time in hours for each employee
emp_daily_office_spent_time <- as.data.frame((as.matrix(out_time_epoch) - as.matrix(in_time_epoch))/(60*60))
          
# Number of columns in emp_daily_office_spent_time
ncol(emp_daily_office_spent_time)
# 249    -> This is fine, as in in_time & out_time we had 250 columns with Employee ID also
          
# Lets combine the data with employee ID
emp_daily_office_spent_time <- cbind(in_time$EmployeeID,emp_daily_office_spent_time)
colnames(emp_daily_office_spent_time)[1] <- "EmployeeID"

# Lets gather the data for all the dates so as to easily analyze them.
emp_office_time <- gather(emp_daily_office_spent_time,key=Dates,values=c(2:ncol(emp_daily_office_spent_time)))

# Lets convert the Dates column to PosixCt Date time object, and add a column for months (Jan(1) - Dec(12))
emp_office_time$Dates <- ymd(emp_office_time$Dates)
emp_office_time$Month <- month(emp_office_time$Dates)

# Lets aggregate the data (total number of hours worked by employee per month and the offs taken)
emp_working_hours_stats <- emp_office_time %>% group_by(EmployeeID,Month) %>% summarize(totalMonthlyHours=sum(value,na.rm=TRUE),offs=sum(is.na(value)))

# The number of working days in the month are 
monthly_base <- data.frame(Month=c(1:12),totalWorkDays=as.vector(sapply(c(1:12),function(x) length(which(month(unique(emp_office_time$Dates)) == x)))))

# Join the number of working hours woth emp_working_hours_stats
emp_working_hours_stats <- full_join(emp_working_hours_stats,monthly_base,by="Month")

# Calculate the percentage of working hours utilized by each employee every month.
emp_working_hours_stats <- emp_working_hours_stats %>% mutate(percWorked=totalMonthlyHours/(totalWorkDays*8))

# Lets spread the data month wise for all employees (employeeID and all the months utilization)
emp_working_hours_stats_spread <- spread(emp_working_hours_stats[,c(1,2,6)],key=Month,value=percWorked)

# Add the total offs taken by employee in this data frame
emp_working_hours_stats_spread$totalOffs <- (emp_working_hours_stats %>% group_by(EmployeeID) %>% summarize(totalOffs=sum(offs)))$totalOffs

# Add the percentage number of hours the employee worked over the complete year
# As per number of working days in the year.
# Taking mean may not be ideal in this case and percentage is a better statistics
emp_working_hours_stats_spread$percYearlyWorking  <- (emp_working_hours_stats %>% 
                                                        group_by(EmployeeID) %>% 
                                                        summarize(totalYearlyWorkingHours=sum(totalMonthlyHours),
                                                                  totalYearlyWorkingDays=sum(totalWorkDays)) %>% 
                                                        mutate(percYearlyWorking=totalYearlyWorkingHours/(totalYearlyWorkingDays*8))
                                                      )$percYearlyWorking
# Lets see if there is any co-relation between monthly data
emp_working_hours.cor <- cor(emp_working_hours_stats_spread[,-1])
dimnames(emp_working_hours.cor) <- list(
  c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','off','yearly'),
  c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','off','yearly'))
corrplot(emp_working_hours.cor,method = "number", 
         title = "Correlation Matrix", 
         type = "lower", 
         order = "FPC",
         number.cex = 0.8,
         tl.cex = 0.7,
         bg="light green",number.digits = 2)

# It is clear from the cor plot that, we can just have the yearly column and the offs column
# rest of the columns can be very well described by other yearly column itself.
emp_working_hours <- emp_working_hours_stats_spread[,c(1,14,15)]
# -- end working on in_time & out_time --#

# -- Working on emp_survey data -- #
# 1) Check the uniqueness of employeeIDs
nrow(emp_survey) == length(unique(emp_survey$EmployeeID))
        
# 2) NA Evaluation
sapply(emp_survey, function(x) sum(is.na(x)))
# EmployeeID EnvironmentSatisfaction         JobSatisfaction         WorkLifeBalance 
#          0                      25                      20                      38
# There might be some pattern or relation between these NAs and other variables, lets just keep
# them for now, we will come back to them
# 3) Outlier treatment (not nedded in this - as all categorical in nature)
        
# -- Working on man_survey data -- #
# 1) Check the uniqueness of employeeIDs
nrow(man_survey) == length(unique(man_survey$EmployeeID))
        
# 2) NA Evaluation
sapply(man_survey, function(x) sum(is.na(x)))
# EmployeeID    JobInvolvement PerformanceRating 
#          0                 0                 0
# 3) Outlier treatment (not needed in this, - as all categorical)
        
# -- Working on general data -- #
# 1) Check the uniqueness of employeeIDs
nrow(general_data) == length(unique(general_data$EmployeeID))
        
# 2) NA Evaluation
sapply(general_data, function(x) sum(is.na(x)))
#           Age               Attrition          BusinessTravel              Department        DistanceFromHome               Education 
#             0                       0                       0                       0                       0                       0 
# EducationField           EmployeeCount              EmployeeID                  Gender                JobLevel                 JobRole 
#             0                       0                       0                       0                       0                       0 
# MaritalStatus           MonthlyIncome      NumCompaniesWorked                  Over18       PercentSalaryHike           StandardHours 
#             0                       0                      19                       0                       0                       0 
# StockOptionLevel       TotalWorkingYears   TrainingTimesLastYear          YearsAtCompany YearsSinceLastPromotion    YearsWithCurrManager 
#             0                       9                       0                       0                       0                       0
        
# Some NAs present, lets see further if there is pattern in this.
      
# Lets combine all the data frame i.e merge
# emp_monthly.df, emp_survey, man_survey, general_data
emp_base_data <- full_join(emp_working_hours,general_data,by="EmployeeID")
emp_base_data <- full_join(emp_base_data,emp_survey,by="EmployeeID")
emp_base_data <- full_join(emp_base_data,man_survey,by="EmployeeID")
    
emp_cleaned_data <- emp_base_data    
# Data Cleaning #
# a) Lets see if there are some columns which have the same data all together in each cell
colnames(emp_cleaned_data)[which(sapply(emp_cleaned_data,function(x) length(unique(x)) == 1))]
# [1] "EmployeeCount" "Over18"        "StandardHours"
# Lets remove these columns first
emp_cleaned_data <- emp_cleaned_data %>% dplyr::select(-c(which(sapply(emp_base_data,function(x) length(unique(x)) == 1))))
        
# We know that from column 1 to column 25 there are no NAs present.
# Lets look at other columns and fix that first
colnames(emp_cleaned_data)[which(!(sapply(emp_cleaned_data,function(x) sum(is.na(x))) == 0))]
# [1] "NumCompaniesWorked"      "TotalWorkingYears"       "EnvironmentSatisfaction" "JobSatisfaction"        
# [5] "WorkLifeBalance"
        
# Working on fixing TotalWorkingYears
# Method : 
# There does not seem to be any relationship between the NAs of TotalWorkingYears and other variables.
# Hence we will take the median of the approx age when other employees generally started working
print("Age at which employees of the company generally started working: ")

round(mean((emp_cleaned_data %>% 
  filter(!is.na(TotalWorkingYears)) %>% 
  dplyr::select(EmployeeID,Age,TotalWorkingYears) %>% 
  mutate(started_working_at=(Age-TotalWorkingYears)))$started_working_at),0)

# 26
        
# Based on above value of 26 years, totalWorkingYears for NAs can be calculated based on their age
# Assuming they did not skip years
emp_cleaned_data[which(is.na(emp_cleaned_data$TotalWorkingYears)),'TotalWorkingYears'] <- (emp_cleaned_data %>% filter(is.na(TotalWorkingYears)))$Age - 26
        

# For other columns where NAs are present, there is no obvious relationship may be drawn.
# Also data does not show any specific relationship/pattern for relationship with columns
# "NumCompaniesWorked" "EnvironmentSatisfaction" "JobSatisfaction" "WorkLifeBalance"

# Lets see what happens if we drop these
# Total attrition value in the complete data set
emp_cleaned_data %>% group_by(Attrition) %>% summarize(cnt=length(EmployeeID))
#Attrition   cnt
#    <chr> <int>
#       No  3699
#      Yes   711

# The attrition count if we remove the lines which have NA in the above columns
emp_cleaned_data[(which(is.na(emp_cleaned_data$NumCompaniesWorked) | 
                          is.na(emp_cleaned_data$EnvironmentSatisfaction) | 
                          is.na(emp_cleaned_data$JobSatisfaction) | 
                          is.na(emp_cleaned_data$WorkLifeBalance))),] %>%
  group_by(Attrition) %>%
  summarize(cnt=length(EmployeeID))
#Attrition   cnt
#    <chr> <int>
#       No    88
#      Yes    14

# Based on above analysis, if we remove the the rows from this data frame, then
# Number of : Yes (Attrition) value reduced by = 14/711 = 0.01969058 ~ 2.0%
# Number of :  No  (Attrition) rows reduced by = 88/3699 = 0.02379021 ~ 2.3%
# Therefore the distribution of attrition in base data set will not be disturbed if we remove
# the NAs for these columns

# Removing NA rows for column NumCompaniesWorked, EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance
emp_cleaned_data <- as.data.frame(emp_cleaned_data %>% 
  filter(!is.na(NumCompaniesWorked), 
         !is.na(EnvironmentSatisfaction), 
         !is.na(JobSatisfaction), 
         !is.na(WorkLifeBalance)))

# Any NA value left in dataset after this #
sum(is.na(emp_cleaned_data))
# 0 <- No more NAs in data set

# Final Clearned updata saved as emp_data        
emp_data <- emp_cleaned_data



## Data Format/type conversion ##
        
## For emp_cleaned_data : 
## Column1(EmployeeID) - Column 15 (TotalOffs) - Everything is numeric
## Age - Numeric
## Attrition - Categorical
emp_data$Attrition <- as.factor(emp_data$Attrition)
## BusinessTravel - Categorical ##
emp_data$BusinessTravel <- as.factor(emp_data$BusinessTravel)
## Department - Categorical ##
emp_data$Department <- as.factor(emp_data$Department)
## DistanceFromHome - Numeric ##
## Education - We will consider this a numeric, because 1 = below college and 5 = doctor.
##             Hence, higher the number, better the education
## EducationField - Categorical ##
emp_data$EducationField <- as.factor(emp_data$EducationField)
## Gender - Categorical ##
emp_data$Gender <- as.factor(emp_data$Gender)
## JobLevel - Higher number signifies higher job level, so we will consider this as numeric
## JobRole - Categorical ##
emp_data$JobRole <- as.factor(emp_data$JobRole)
## MaritalStatus - categorical ##
emp_data$MaritalStatus <- as.factor(emp_data$MaritalStatus)
## MonthlyIncome - Numeric ##
## NumOfCompaniesWorked - numeric ##
## PercentSalaryHike - numeric ##
## StockOptionLevel - Categorical ##
emp_data$StockOptionLevel <- as.factor(emp_data$StockOptionLevel)
## TotalWorkingYears - Numeric ##
## TrainingTimesLastYear - Numeric ##
## YearsAtCompany - Numeric ##
## YearsSinceLastPromotion - Numeric ##
## YearsWithCurrManager - Numeric ##
## JobSatisfaction - Numeric as higher number means better Job Satisfaction ##
## WorkLifeBalance, JobInvolvement,PerformanceRating all numeric for same reason ##

numeric_column_list <- c(2:4,8,9,12,15,16,17,19:28)
categorical_column_list <- c(2:ncol(emp_data))[which(!c(2:ncol(emp_data)) %in% numeric_column_list)]
        
# Lets check the co-relation between the numerical column so that we can eliminate them
# if they have high corelation.
        
emp_data.numeric.cor <- cor(scale(emp_data[,numeric_column_list]))
dimnames(emp_data.numeric.cor) <- list(
  c('off','yearWork','age','disthome','edu','jlevel','mIncome','nComWork','percHike','totExp','train','yearCom','promo','currMan','eSatis','jSatis','WorkBal','jInvo','perfRat'),
  c('off','yearWork','age','disthome','edu','jlevel','mIncome','nComWork','percHike','totExp','train','yearCom','promo','currMan','eSatis','jSatis','WorkBal','jInvo','perfRat'))
corrplot(emp_data.numeric.cor,method = "number", 
         title = "Correlation Matrix", 
         type = "lower", 
         order = "FPC",
         number.cex = 0.8,
         tl.cex = 0.7,
         bg="light green",number.digits = 2)
        
# There is no numerical variable that can be eliminiated from here.
        

## Univariate Analysis - With segmentation for Attrition rate ##
        
# Lets do it for categorical variables first #
        
G01 <- ggplot(emp_data) + 
  geom_bar(aes(x=BusinessTravel,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) + 
  scale_y_continuous(breaks=seq(0,3500,200)) +
  labs(title="G01 - BusinessTravel & Attrition",x="Business Travel",y="No of Employees",fill="Attrition")
        
G02 <- ggplot(emp_data) + 
  geom_bar(aes(x=Department,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) + 
  scale_y_continuous(breaks=seq(0,3500,200)) +
  labs(title="G02 - Department & Attrition",x="Department",y="No of Employees",fill="Attrition")

G03 <- ggplot(emp_data) + 
  geom_bar(aes(x=EducationField,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) + 
  scale_y_continuous(breaks=seq(0,3500,200)) +
  labs(title="G03 - Education Field & Attrition",x="Education Field",y="No of Employees",fill="Attrition")

G04 <- ggplot(emp_data) + 
  geom_bar(aes(x=Gender,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) + 
  scale_y_continuous(breaks=seq(0,3500,200)) +
  labs(title="G04 - Gender & Attrition",x="Gender",y="No of Employees",fill="Attrition")

G05 <- ggplot(emp_data) + 
  geom_bar(aes(x=JobRole,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) + 
  scale_y_continuous(breaks=seq(0,3500,200)) +
  labs(title="G05 - JobRole & Attrition",x="JobRole",y="No of Employees",fill="Attrition")

G06 <- ggplot(emp_data) + 
  geom_bar(aes(x=MaritalStatus,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) + 
  scale_y_continuous(breaks=seq(0,3500,200)) +
  labs(title="G06 - MaritalStatus & Attrition",x="Marital Status",y="No of Employees",fill="Attrition")

G07 <- ggplot(emp_data) + 
  geom_bar(aes(x=StockOptionLevel,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) + 
  scale_y_continuous(breaks=seq(0,3500,100)) +
  labs(title="G07 - StockOptionLevel & Attrition",x="StockOption Level",y="No of Employees",fill="Attrition")

grid.arrange(G01,G02,G03,G04,G05,G06,G07)
        

#colnames(emp_data)[numeric_column_list]

## Visualization for numerical data, based on attrition ##
G08 <- ggplot(emp_data) + geom_boxplot(aes(x=Attrition,y=totalOffs,fill=Attrition)) +
  scale_y_continuous(breaks=seq(0,25,2)) +
  labs(title="G08 - Total Offs and Attrition",x="Attrition",y="Total Offs")

G08_hist <- ggplot(emp_data) + geom_histogram(aes(x=totalOffs,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,250,10)) +
  labs(title="G08_dist - Total Offs distribution",x="totalOffs",y="No of Employees")

G09 <- ggplot(emp_data) + geom_boxplot(aes(x=Attrition,y=percYearlyWorking,fill=Attrition)) +
  scale_y_continuous(breaks=seq(0,1.4,0.1)) +
  labs(title="G09 - Yearly Worked Hours(%) and Attrition",x="Attrition",y="Working Hours percent")

G09_hist <- ggplot(emp_data) + geom_histogram(aes(x=percYearlyWorking,fill=Attrition),col="black",binwidth=0.01) +
  scale_y_continuous(breaks=seq(0,150,10)) +
  labs(title="G09_dist - Percentage Working Hours distribution",x="Percentage working hours",y="No of Employees")

G10 <- ggplot(emp_data) + geom_boxplot(aes(x=Attrition,y=Age,fill=Attrition)) +
  scale_y_continuous(breaks=seq(15,65,2)) +
  labs(title="G10 - Age and Attrition",x="Attrition",y="Age")

G10_hist <- ggplot(emp_data) + geom_histogram(aes(x=Age,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,250,10)) +
  scale_x_continuous(breaks=seq(18,60,2)) +
  labs(title="G10_dist - Age distribution",x="Age",y="No of Employees")

G11 <- ggplot(emp_data) + geom_boxplot(aes(x=Attrition,y=DistanceFromHome,fill=Attrition)) +
  scale_y_continuous(breaks=seq(0,30,2)) +
  labs(title="G11 - Distance From Home and Attrition",x="Attrition",y="Distance From Home")

G11_hist <- ggplot(emp_data) + geom_histogram(aes(x=DistanceFromHome,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,600,30)) +
  scale_x_continuous(breaks=seq(0,30,1)) +
  labs(title="G11_dist - Distance From Home Distribution",x="Distance from home",y="No of Employees")

G12 <- ggplot(emp_data) + geom_bar(aes(x=Education,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) +
  scale_y_continuous(breaks=seq(0,1800,100)) +
  labs(title="G12 - Education and Attrition",x="Employee Count",y="Education")

G13 <- ggplot(emp_data) + geom_bar(aes(x=JobLevel,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) +
  scale_y_continuous(breaks=seq(1,1800,100)) +
  labs(title="G13 - JobLevel and Attrition",x="Employee Count",y="JobLevel")

grid.arrange(G08,G09,G10,G11,G12,G13)
grid.arrange(G08_hist,G09_hist,G10_hist,G11_hist)

G14 <- ggplot(emp_data) + geom_boxplot(aes(x=Attrition,y=MonthlyIncome,fill=Attrition)) +
  scale_y_continuous(breaks=seq(5000,200000,15000)) +
  labs(title="G14 - MonthlyIncome and Attrition",x="Attrition",y="MonthlyIncome")

G14_hist <- ggplot(emp_data) + 
  geom_histogram(aes(x=MonthlyIncome,fill=Attrition),binwidth=5000,col="black") +
  scale_y_continuous(breaks=seq(0,600,30)) +
  scale_x_continuous(breaks=seq(0,200000,20000)) +
  labs(title="G14_hist - Monthly Income Distribution",x="Monthly Income",y="No of employees",fill="Attrition")

G15 <- ggplot(emp_data) + geom_boxplot(aes(x=Attrition,y=NumCompaniesWorked,fill=Attrition)) +
  scale_y_continuous(breaks=seq(0,10,1)) +
  labs(title="G15 - NumCompaniesWorked and Attrition",x="Attrition",y="NumCompaniesWorked")

G16 <- ggplot(emp_data) + geom_boxplot(aes(x=Attrition,y=PercentSalaryHike,fill=Attrition)) +
  scale_y_continuous(breaks=seq(0,25,1)) +
  labs(title="G16 - PercentSalaryHike and Attrition",x="Attrition",y="PercentSalaryHike")

G20 <- ggplot(emp_data) + geom_boxplot(aes(x=Attrition,y=TotalWorkingYears,fill=Attrition)) +
  scale_y_continuous(breaks=seq(0,40,4)) +
  labs(title="G20 - TotalWorkingYears and Attrition",x="Attrition",y="TotalWorkingYears")

G20_hist <- ggplot(emp_data) + 
  geom_histogram(aes(x=TotalWorkingYears,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,600,30)) +
  labs(title="G20_hist - TotalWorkingYears",x="TotalWorkingYears",y="No of Employees")

G21 <- ggplot(emp_data) + geom_bar(aes(x=TrainingTimesLastYear,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) +
  scale_y_continuous(breaks=seq(0,1800,100)) +
  labs(title="G21 - TrainingTimesLastYear and Attrition",x="Employee Count",y="TrainingTimesLastYear")

G22 <- ggplot(emp_data) + geom_boxplot(aes(x=Attrition,y=YearsAtCompany,fill=Attrition)) +
  scale_y_continuous(breaks=seq(0,40,4)) +
  labs(title="G22 - YearsAtCompany and Attrition",x="Attrition",y="YearsAtCompany")

G22_hist <- ggplot(emp_data) + 
  geom_histogram(aes(x=YearsAtCompany,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,600,30)) +
  labs(title="G22_hist - YearsAtCompany",x="YearsAtCompany",y="No of Employees")

grid.arrange(G14,G15,G16,G20,G21,G22)
grid.arrange(G14_hist,G20_hist,G22_hist,nrow=2,ncol=2)

### Moving ahead with other numerical variables ###

G23 <- ggplot(emp_data) + geom_boxplot(aes(x=Attrition,y=YearsSinceLastPromotion,fill=Attrition)) +
  scale_y_continuous(breaks=seq(0,40,4)) +
  labs(title="G23 - YearsSinceLastPromotion and Attrition",x="Attrition",y="YearsSinceLastPromotion")

G23_hist <- ggplot(emp_data) + 
  geom_histogram(aes(x=YearsSinceLastPromotion,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,1750,50)) +
  scale_x_continuous(breaks=seq(0,15,1)) +
  labs(title="G23_hist - YearsSinceLastPromotion",x="YearsSinceLastPromotion",y="No of Employees")

G24 <- ggplot(emp_data) + geom_boxplot(aes(x=Attrition,y=YearsWithCurrManager,fill=Attrition)) +
  scale_y_continuous(breaks=seq(0,40,4)) +
  labs(title="G24 - YearsWithCurrManager and Attrition",x="Attrition",y="YearsWithCurrManager")

G24_hist <- ggplot(emp_data) + 
  geom_histogram(aes(x=YearsWithCurrManager,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,1750,50)) +
  scale_x_continuous(breaks=seq(0,17,1)) +
  labs(title="G24_hist - YearsWithCurrManager",x="YearsWithCurrManager",y="No of Employees")

G25 <- ggplot(emp_data) + geom_bar(aes(x=EnvironmentSatisfaction,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) +
  scale_y_continuous(breaks=seq(0,1800,100)) +
  labs(title="G25 - EnvironmentSatisfaction and Attrition",x="Employee Count",y="EnvironmentSatisfaction")

G26 <- ggplot(emp_data) + geom_bar(aes(x=JobSatisfaction,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) +
  scale_y_continuous(breaks=seq(0,1800,100)) +
  labs(title="G26 - JobSatisfaction and Attrition",x="Employee Count",y="JobSatisfaction")

G27 <- ggplot(emp_data) + geom_bar(aes(x=WorkLifeBalance,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) +
  scale_y_continuous(breaks=seq(0,1800,100)) +
  labs(title="G27 - WorkLifeBalance and Attrition",x="Employee Count",y="WorkLifeBalance")

G28 <- ggplot(emp_data) + geom_bar(aes(x=JobInvolvement,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) +
  scale_y_continuous(breaks=seq(0,1800,100)) +
  labs(title="G28 - JobInvolvement and Attrition",x="Employee Count",y="JobInvolvement")

G43 <- ggplot(emp_data) + geom_bar(aes(x=PerformanceRating,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) +
  scale_y_continuous(breaks=seq(0,3500,100)) +
  labs(title="G43 - PerformanceRating and Attrition",x="Employee Count",y="PerformanceRating")

grid.arrange(G23,G24,G25,G26,G27,G43)
grid.arrange(G23_hist,G24_hist,nrow=1,ncol=2)


# ----- MonthlyIncome Outlier Treatment ------#
# From these graphs, there needs to be outlier treatment for Monthly Income #
# Lets see the graph more clearly #
grid.arrange(G14)

# Lets see a histogram for this
G17 <- ggplot(emp_data) + 
  geom_histogram(aes(x=MonthlyIncome,fill=Attrition),binwidth=5000,col="black") +
  labs(title="G17 - Monthly Income & Attrition",x="Monthly Income",y="No of employees",fill="Attrition")
grid.arrange(G17)
# Higher attrition rate in lower income group #
# So even if we fix the outliers we will not be disturbing this #

# Lets see a quantile & boxplot of net monthly income #
G18 <- ggplot(emp_data) +
  geom_boxplot(aes(x=1,y=MonthlyIncome)) + scale_y_continuous(breaks=seq(0,200000,10000)) +
  labs(title="G16 - MonthlyIncome",x="MonthlyIncome",y="No of Employees")
grid.arrange(G18)

quantile(emp_data$MonthlyIncome,seq(0,1,0.01))
G19 <- ggplot() + geom_line(aes(x=c(0:100),y=quantile(emp_data$MonthlyIncome,seq(0,1,0.01)))) + labs(title="G17 - MonthlyIncome Quantile Rise")
grid.arrange(G19)

# Lets cap the monthly income to 170000 as depicted by the boxplot and expresed by quantile also.
# A total of 282 rows have income higher than 170000 which is 282/4308 ~ 6.5% of the data. 
# Seems decent enough
emp_data[which(emp_data$MonthlyIncome > 170000),'MonthlyIncome'] <- 170000

# Based on the graphs, we can see that following columns would require outlier treatment
# TotalWorkingYears, YearsAtCompany, YearsSinceLastPromotion, YearsWithCurrentManager

# --------- TotalWorkingYears - outlier treatment ----------#
# Lets see the graph more clearly
grid.arrange(G20)

# Lets check the histogram distribution #
G29 <- ggplot(emp_data) + 
  geom_histogram(aes(x=TotalWorkingYears,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,600,30)) +
  labs(title="G29 - TotalWorkingYears",x="TotalWorkingYears",y="No of Employees")
grid.arrange(G29)

# Lets check a net boxplot for this
G30 <- ggplot(emp_data) + 
  geom_boxplot(aes(x=1,y=TotalWorkingYears)) +
  scale_y_continuous(breaks=seq(0,40,5)) +
  labs(title="G30 - TotalWorkingYears",x="TotalWorkingYears",y="No of Employees")
grid.arrange(G30)

# Lets see the quantile #
quantile(emp_data$TotalWorkingYears,seq(0,1,0.01))
G31 <- ggplot() + 
  geom_line(aes(x=c(0:100),y=quantile(emp_data$TotalWorkingYears,seq(0,1,0.01)))) +
  scale_x_continuous(breaks=seq(0,100,2)) +
  scale_y_continuous(breaks=seq(0,40,2)) + 
  labs(title="G31 - TotalWorkingYears quantile wise increase")
grid.arrange(G31)

# As shown by quantiles and boxplot lets take 26 as top most value for this #
# A total of 247 rows will be effected by this, which is 247/4308 ~ 5.7% rows
# decent enough
emp_data[which(emp_data$TotalWorkingYears > 26),'TotalWorkingYears'] <- 26

# ---------- Working on YearsAtCompany ------------ #
# Lets see the graph more clearly
grid.arrange(G22)

# Lets check the histogram distribution #
G32 <- ggplot(emp_data) + 
  geom_histogram(aes(x=YearsAtCompany,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,600,30)) +
  labs(title="G32 - YearsAtCompany",x="YearsAtCompany",y="No of Employees")
grid.arrange(G32)

# Lets check a net boxplot for this
G33 <- ggplot(emp_data) + 
  geom_boxplot(aes(x=1,y=YearsAtCompany)) +
  scale_y_continuous(breaks=seq(0,40,5)) +
  labs(title="G33 <- YearsAtCompany",x="YearsAtCompany",y="No of Employees")
grid.arrange(G33)

# Lets see the quantile #
quantile(emp_data$YearsAtCompany,seq(0,1,0.01))
G34 <- ggplot() + 
  geom_line(aes(x=c(0:100),y=quantile(emp_data$YearsAtCompany,seq(0,1,0.01)))) +
  scale_x_continuous(breaks=seq(0,100,2)) +
  scale_y_continuous(breaks=seq(0,40,2)) +
  labs(title="G34 - YearsAtCompany quantile wise increase")
grid.arrange(G34)

# As shown by quantiles and boxplot lets take 22 as top most value for this #
# A total of 247 rows will be effected by this, which is 111/4308 ~ 2.57% rows
# decent enough
emp_data[which(emp_data$YearsAtCompany > 22),'YearsAtCompany'] <- 22

# --------- YearsSinceLastPromotion - outlier treatment ----------#
# Lets see the graph more clearly
grid.arrange(G23)

# Lets check the histogram distribution #
G35 <- ggplot(emp_data) + 
  geom_histogram(aes(x=YearsSinceLastPromotion,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,1750,50)) +
  labs(title="G35 - YearsSinceLastPromotion",x="YearsSinceLastPromotion",y="No of Employees")
grid.arrange(G35)

# Lets check a net boxplot for this
G36 <- ggplot(emp_data) + 
  geom_boxplot(aes(x=1,y=YearsSinceLastPromotion)) +
  scale_y_continuous(breaks=seq(0,15,1)) +
  labs(title="G36 - YearsSinceLastPromotion",x="YearsSinceLastPromotion",y="No of Employees")
grid.arrange(G36)

# Lets see the quantile #
quantile(emp_data$YearsSinceLastPromotion,seq(0,1,0.01))
G37 <- ggplot() + 
  geom_line(aes(x=c(0:100),y=quantile(emp_data$YearsSinceLastPromotion,seq(0,1,0.01)))) +
  scale_x_continuous(breaks=seq(0,100,2)) +
  scale_y_continuous(breaks=seq(0,40,2)) +
  labs(title="G37 - YearsSinceLastPromotion quantile wise increase")
grid.arrange(G37)

# As shown by quantiles and boxplot lets take 11 as top most value for this #
# A total of 247 rows will be effected by this, which is 126/4308 ~ 2.92% rows
# decent enough
emp_data[which(emp_data$YearsSinceLastPromotion > 11),'YearsSinceLastPromotion'] <- 11

# --------- YearsWithCurrManager - outlier treatment ----------#
# Lets see the graph more clearly
grid.arrange(G24)

# Lets check the histogram distribution #
G40 <- ggplot(emp_data) + 
  geom_histogram(aes(x=YearsWithCurrManager,fill=Attrition),col="black",binwidth=1) +
  scale_y_continuous(breaks=seq(0,1750,50)) +
  labs(title="G40 - YearsWithCurrManager",x="YearsWithCurrManager",y="No of Employees")
grid.arrange(G40)

# Lets check a net boxplot for this
G41 <- ggplot(emp_data) + 
  geom_boxplot(aes(x=1,y=YearsWithCurrManager)) +
  scale_y_continuous(breaks=seq(0,20,1)) +
  labs(title="G41 - YearsWithCurrManager",x="YearsWithCurrManager",y="No of Employees")
grid.arrange(G41)

# Lets see the quantile #
quantile(emp_data$YearsWithCurrManager,seq(0,1,0.01))
G42 <- ggplot() + 
  geom_line(aes(x=c(0:100),y=quantile(emp_data$YearsWithCurrManager,seq(0,1,0.01)))) +
  scale_x_continuous(breaks=seq(0,100,2)) +
  scale_y_continuous(breaks=seq(0,40,2)) +
  labs(title="G42 YearsWithCurrentManager quantile wise increase")
grid.arrange(G42)

# As shown by quantiles and boxplot lets take 11 as top most value for this #
# A total of 247 rows will be effected by this, which is 151/4308 ~ 3.50% rows
# decent enough
emp_data[which(emp_data$YearsWithCurrManager > 11),'YearsSinceLastPromotion'] <- 11




# Data Preparation #

emp_data_4_model <- emp_data

# Scaling needs to be done for numerical columns #

emp_data_4_model <- cbind(emp_data_4_model$EmployeeID ,scale(emp_data_4_model %>% dplyr::select(numeric_column_list)), emp_data_4_model %>% dplyr::select(categorical_column_list))
colnames(emp_data_4_model)[1] <- "EmployeeID"

# List of column names which are being considered as categorical #
colnames(emp_data)[categorical_column_list]
# [1] "Attrition"        "BusinessTravel"   "Department"       "EducationField"   "Gender"           "JobRole"         
# [7] "MaritalStatus"    "StockOptionLevel"

# Above columns map to following columns in emp_data_4_model after scale cbind #
which(colnames(emp_data_4_model) %in% colnames(emp_data)[categorical_column_list])
# [1] 21 22 23 24 25 26 27 28

# --- Working on BusinessTravel,Department,EducationField,JobRole,MaritalStatus,StockOptionLevel for conversion to numerical elements --- #
# We will use model.matrix fundamentals here #
dummies <- sapply(emp_data_4_model[,c(22,23,24,26,27,28)],function(x) 
  as.data.frame(model.matrix(~x,data=emp_data_4_model[,c(22,23,24,26,27,28)])))
dummies <- as.data.frame(dummies)
dummies <- dummies %>% dplyr::select(-c(BusinessTravel..Intercept.,
                      Department..Intercept.,
                      EducationField..Intercept.,
                      JobRole..Intercept.,
                      MaritalStatus..Intercept.,
                      StockOptionLevel..Intercept.))

emp_data_4_model <- cbind(emp_data_4_model %>% 
                            dplyr::select(-c(BusinessTravel,Department,EducationField,JobRole,MaritalStatus,StockOptionLevel)),
                          dummies)

# --- Working on Attrition for conversion to numerical elements --- #
# We will consider this as churn analysis, with yes = 1 and no = 0
emp_data_4_model$Attrition <- sapply(emp_data_4_model$Attrition, function(x) ifelse(x == "Yes",1,0))

# --- Working on Gender for conversion to numerical elements --- #
# We will consider here Male = 1 and Female = 0 #
emp_data_4_model$Gender <- sapply(emp_data_4_model$Gender, function(x) ifelse(x == "Male",1,0))

#View(emp_data_4_model)

emp_data_4_model <- emp_data_4_model %>% dplyr::select(-EmployeeID)

set.seed(100)
train_idx <- which(sample.split(emp_data_4_model$Attrition,SplitRatio = 0.7))
test_idx <- which(!c(1:nrow(emp_data_4_model) %in% train_idx))

emp_train <- emp_data_4_model[train_idx,]
emp_test <- emp_data_4_model[test_idx,]

### Data Understanding and Data Preparation Complete ##


# ---- CRISP-DM Framework - Stage 4 - Model Preparation ---- #

## Model Building ##
# Note: An appendix to the results of the models is given at the end of the code for reference
#       Please refer for evaluation.


## Building Model_1 ##
## Eliminated Columns
model_1 <- glm(Attrition~., data=emp_train, family="binomial")
summary(model_1)

step <- stepAIC(model_1,direction="both")

model_2 <- glm(Attrition ~ percYearlyWorking + Age + JobLevel + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + JobInvolvement + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + EducationField.xMarketing + EducationField.xTechnical.Degree + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xSales.Representative + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3,
               data=emp_train,family="binomial")
sort(vif(model_2),decreasing = TRUE)
summary(model_2)
# Department.xSales removed after analysis of above #

model_3 <- glm(Attrition ~ percYearlyWorking + Age + JobLevel + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + JobInvolvement + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 EducationField.xMarketing + EducationField.xTechnical.Degree + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xSales.Representative + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3,
               data=emp_train,family="binomial")
sort(vif(model_3),decreasing = TRUE)
summary(model_3)
# JobInvolvement removed after analysis of above #
# VIF is not helping from here onwards, working with only p value#

model_4 <- glm(Attrition ~ percYearlyWorking + Age + JobLevel + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 EducationField.xMarketing + EducationField.xTechnical.Degree + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xSales.Representative + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3,
               data=emp_train,family="binomial")
summary(model_4)
# Department.xResearch...Development removed after analysis of above #

model_5 <- glm(Attrition ~ percYearlyWorking + Age + JobLevel + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + 
                 EducationField.xMarketing + EducationField.xTechnical.Degree + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xSales.Representative + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3,
               data=emp_train,family="binomial")
summary(model_5)
# EducationField.xTechnical.Degree removed after analysis of above #

model_6 <- glm(Attrition ~ percYearlyWorking + Age + JobLevel + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + 
                 EducationField.xMarketing +
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xSales.Representative + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3,
               data=emp_train,family="binomial")
summary(model_6)
# EducationField.xMarketing removed after analysis of above #

model_7 <- glm(Attrition ~ percYearlyWorking + Age + JobLevel + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xSales.Representative + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3,
               data=emp_train,family="binomial")
summary(model_7)
# JobRole.xSales.Representative removed after analysis of above #

model_8 <- glm(Attrition ~ percYearlyWorking + Age + JobLevel + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + 
                 JobRole.xManager + JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + StockOptionLevel.x3,
               data=emp_train,family="binomial")
summary(model_8)
# StockOptionLevel.x3 removed after analysis of above #

model_9 <- glm(Attrition ~ percYearlyWorking + Age + JobLevel + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + 
                 JobRole.xManager + JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + StockOptionLevel.x1,
               data=emp_train,family="binomial")
summary(model_9)
# StockOptionLevel.x1 removed after analysis of above #

model_10 <- glm(Attrition ~ percYearlyWorking + Age + JobLevel + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                  WorkLifeBalance + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + 
                  JobRole.xManager + JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle,
               data=emp_train,family="binomial")
summary(model_10)
# JobLevel removed after analysis of above #

model_11 <- glm(Attrition ~ percYearlyWorking + Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                  WorkLifeBalance + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + 
                  JobRole.xManager + JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle,
                data=emp_train,family="binomial")
summary(model_11)
# JobRole.xManager removed after analysis of above #

model_12 <- glm(Attrition ~ percYearlyWorking + Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                  WorkLifeBalance + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + 
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle,
                data=emp_train,family="binomial")
summary(model_12)
# BusinessTravel.xTravel_Rarely removed after analysis of above #

model_13 <- glm(Attrition ~ percYearlyWorking + Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                  WorkLifeBalance + BusinessTravel.xTravel_Frequently + 
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle,
                data=emp_train,family="binomial")
summary(model_13)


# All P-values now come out to be significant #
model_final <- model_13

# Significant Variables and their respective p-values are
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                       -2.62190    0.09811 -26.724  < 2e-16 ***
# percYearlyWorking                  0.58747    0.05420  10.839  < 2e-16 ***
# Age                               -0.26054    0.07865  -3.313 0.000924 ***
# NumCompaniesWorked                 0.36017    0.05742   6.272 3.56e-10 ***
# TotalWorkingYears                 -0.64254    0.10449  -6.149 7.79e-10 ***
# TrainingTimesLastYear             -0.20913    0.05761  -3.630 0.000283 ***
# YearsSinceLastPromotion            0.62684    0.08711   7.196 6.19e-13 ***
# YearsWithCurrManager              -0.58090    0.09462  -6.139 8.29e-10 ***
# EnvironmentSatisfaction           -0.38163    0.05548  -6.879 6.03e-12 ***
# JobSatisfaction                   -0.39377    0.05649  -6.971 3.16e-12 ***
# WorkLifeBalance                   -0.24788    0.05461  -4.539 5.64e-06 ***
# BusinessTravel.xTravel_Frequently  0.78395    0.13005   6.028 1.66e-09 ***
# JobRole.xManufacturing.Director   -0.92506    0.22247  -4.158 3.21e-05 ***
# MaritalStatus.xSingle              1.03141    0.11525   8.949  < 2e-16 ***


# ---- CRISP-DM Framework - Stage 5 - Model Evaluation ---- #

# Model evaluation done on TEST data.

# Lets see what are the predictions made
emp_test$AttritionPredictProb <- predict(model_final,type="response",newdata = emp_test %>% dplyr::select(-Attrition))

# Minimum value of Probability predicted #
min(emp_test$AttritionPredictProb)
# 0.0008597726
# Maximum value of Probability predicted #
max(emp_test$AttritionPredictProb)
# 0.8875297

# lets say we want to have a 500 different cut off value for closet prediction result #
AttritionProbCheckRange <- seq(min(emp_test$AttritionPredictProb),max(emp_test$AttritionPredictProb),length.out = 500)
cutOffAnalysis.df <- data.frame(iteration=vector(mode="numeric"),
                                cutoff=vector(mode="numeric"),
                                TP=vector(mode="numeric"),
                                TN=vector(mode="numeric"),
                                FP=vector(mode="numeric"),
                                FN=vector(mode="numeric"))

print("Building Analysis for checking performance of model at different cutOff Levels")
itr <- 1
pb <- txtProgressBar(min=1,max=500,style = 3)
for (cutoff in AttritionProbCheckRange) {
  setTxtProgressBar(pb,itr)
  emp_test_check <- emp_test
  emp_test_check$AttritionPredict <- sapply(emp_test_check$AttritionPredictProb,function(x) ifelse(x>cutoff,1,0))
  TP <- length(which(emp_test_check$Attrition == 1 & emp_test_check$AttritionPredict == 1))
  TN <- length(which(emp_test_check$Attrition == 0 & emp_test_check$AttritionPredict == 0))
  FP <- length(which(emp_test_check$Attrition == 0 & emp_test_check$AttritionPredict == 1))
  FN <- length(which(emp_test_check$Attrition == 1 & emp_test_check$AttritionPredict == 0))
  cutOffAnalysis.df[itr,] <- c(itr,cutoff,TP,TN,FP,FN)
  itr <- itr + 1
}

# Lets calculate various statistics for analysis like accuracy, specificity, sensitivity etc.
cutOffAnalysis.df <- cutOffAnalysis.df %>% mutate(P=FN+TP,N=TN+FP) %>% 
  mutate(Sn=TP/P, Sp=TN/N) %>%
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  mutate(Accuracy=(TP+TN)/(P+N)) %>%
  mutate(Lift=(TP/P)/((TP+FP)/(P+N))) %>% 
  mutate(M=(8*FN+2*FP)/(P+N))
# Here M is a custom parameter defined by us. 
# Since as per our business problem we have to focus more on penalising False Negatives
# (people who will Attrition but are detected as not attritioning) 

# Lets build 4 cutoff Values :
# cutOff1 - data frame = emp_test_1 - When Accuracy/Specificity/Senstivity intersect
# cutOff2 - data frame = emp_test_2 - When Accuracy is maximum
# cutOff3 - data frame = emp_test_3 - When KS statistics is maximum
# cutoff4- data frame = emp_test_4 - When M  is minimum


# Building emp_test_1, based on above parameters
cutOff1 <- cutOffAnalysis.df[(which(abs(cutOffAnalysis.df$Sn - cutOffAnalysis.df$Sp) < 0.01))[2],'cutoff']
emp_test_1 <- emp_test %>% mutate(AttritionPredict=ifelse(AttritionPredictProb>cutOff1,1,0))

# Building emp_test_2, based on above parameters
cutOff2 <- cutOffAnalysis.df[which(cutOffAnalysis.df$Accuracy == max(cutOffAnalysis.df$Accuracy))[1],'cutoff']
emp_test_2 <- emp_test %>% mutate(AttritionPredict=ifelse(AttritionPredictProb>cutOff2,1,0))

# Building emp_test_3, based on above parameters
cutOff3 <- cutOffAnalysis.df[which(cutOffAnalysis.df$KS == max(cutOffAnalysis.df$KS)),'cutoff']
emp_test_3 <- emp_test %>% mutate(AttritionPredict=ifelse(AttritionPredictProb>cutOff3,1,0))

# Building emp_test_4, based on above parameters
cutOff4 <- cutOffAnalysis.df[which(cutOffAnalysis.df$M == min(cutOffAnalysis.df$M)),'cutoff']
emp_test_4 <- emp_test %>% mutate(AttritionPredict=ifelse(AttritionPredictProb>cutOff4,1,0))

# Lets see these values ovar a graph
G44 <- ggplot(cutOffAnalysis.df) +
  geom_line(aes(x=cutoff,y=Accuracy,color="Accuracy"),size=1) + 
  geom_line(aes(x=cutoff,y=Sp,color="Specificity"),size=1) + 
  geom_line(aes(x=cutoff,y=Sn,color="Sensitivity"),size=1) + 
  geom_line(aes(x=cutoff,y=KS,col="KS"),size=1) + 
  geom_vline(xintercept=cutOff1,size=1,col="black") +
  geom_text(aes(x=cutOff1+0.085,y=0.9,label=c("Acc/Sp/Sn CutOff = 0.1608")),inherit.aes = FALSE) +
  geom_vline(xintercept=cutOff2,size=1,col="black") +
  geom_text(aes(x=cutOff2+0.075,y=0.9,label=c("Max Acc CutOff = 0.6192")),inherit.aes = FALSE) +
  geom_vline(xintercept=cutOff3,size=1,col="black") +
  geom_text(aes(x=cutOff3-0.075,y=0.12,label=c("Max KS CutOff = 0.1465")),inherit.aes = FALSE) +
  scale_color_manual("",breaks=c("Accuracy","Specificity","Sensitivity","KS"),
                     values=c("blue","green","red","magenta")) +
  scale_x_continuous(breaks=seq(0,1,0.05)) +
  scale_y_continuous(breaks=seq(0,1,0.05)) +
  labs(title="G44 - Accuracy/Specificity/Sensitivity/KS comparison",x="CutOff",y="Accuracy/Specificity/Sensitivity/KS comparison")
grid.arrange(G44)

confusion1 <- confusionMatrix(emp_test_1$AttritionPredict,emp_test_1$Attrition)
confusion2 <- confusionMatrix(emp_test_2$AttritionPredict,emp_test_2$Attrition)
confusion3 <- confusionMatrix(emp_test_3$AttritionPredict,emp_test_3$Attrition)

# When we do the analysis by Accuracy, specificity, and sensitivity intersection point
# CutOff = 0.1608
confusion1$table
#             Reference
#Prediction   0   1
#         0 766  61
#         1 317 148

# When we do the analysis by Maximum Accuracy
# CutOff = 0.6192
confusion2$table
#             Reference
# Prediction    0    1
#          0 1076  184
#          1    7   25

# When we do the analysis by Maximum KS-Statistics
# CutOff = 0.1465
confusion3$table
#             Reference
# Prediction    0    1
#          0   737  48
#          1   346 161


## Building gainChart building functions here ##

buildOrderedRandomEmpData <- function(emp_ref.df,random) {
  if (random == 0) {
    emp_ref_ordered_prob.df <- emp_ref.df %>% arrange(desc(AttritionPredictProb))
    return(emp_ref_ordered_prob.df)
  } else {
    set.seed(100)
    emp_ref_random_prob.df <- emp_ref.df[sample(c(1:nrow(emp_ref.df)),size=nrow(emp_ref.df)),]
    return(emp_ref_random_prob.df)
  }
}

buildGainChart <- function(df) {
  itr <- 1
  gainChart <- data.frame(numObs = vector(mode="numeric"),
                          attritionYes = vector(mode="numeric"),
                          cumAttritionYes = vector(mode="numeric"),
                          percCumAttritionYes = vector(mode="numeric"),
                          attritionNo = vector(mode="numeric"),
                          cumAttritionNo = vector(mode="numeric"),
                          percCumAttritionNo = vector(mode="numeric"),
                          KS = vector(mode="numeric"))
  startingRowNumber = 1
  groupsOf <- round(nrow(df)/10,0) - 1
  totalAttritionYes <- sum(df$AttritionPredict == 1)
  totalAttritionNo <- sum(df$AttritionPredict == 0)
  while (itr <= 10) {
    endingRowNumber = (startingRowNumber + groupsOf) - 1
    if (endingRowNumber > nrow(df)) {
      endingRowNumber <- nrow(df)
    }
    ref.df <- df[c(startingRowNumber:endingRowNumber),]
    numObs <- nrow(ref.df)
    attritionYes <- sum(ref.df$AttritionPredict == 1)
    attritionNo <- sum(ref.df$AttritionPredict == 0)
    cumAttrition_temp <- cumsum(c(gainChart$attritionYes,attritionYes))
    cumAttritionYes <- cumAttrition_temp[length(cumAttrition_temp)]
    percCumAttritionYes <- cumAttritionYes/totalAttritionYes
    cumAttrition_temp <- cumsum(c(gainChart$attritionNo,attritionNo))
    cumAttritionNo <- cumAttrition_temp[length(cumAttrition_temp)]
    percCumAttritionNo <- cumAttritionNo / totalAttritionNo
    KS <- percCumAttritionYes - percCumAttritionNo
    gainChart[itr,] <- c(numObs,attritionYes,cumAttritionYes,percCumAttritionYes,attritionNo,cumAttritionNo,percCumAttritionNo,KS)
    itr <- itr + 1
    startingRowNumber <- endingRowNumber + 1
  }
  return(gainChart)
}


# Lets build the ROC curve 

# when prediction done through Accuracy/Sensitivity/Specificity intersection point
# DataFrame to consider = emp_test_1 --> This is based on cutOff1 = 0.1608
emp_ref_ordered_prob.df <- buildOrderedRandomEmpData(emp_test_1,random=0)
gainChart_ordered_test_1 <- buildGainChart(emp_ref_ordered_prob.df)


# Building random data frame
emp_ref_random_prob.df <- buildOrderedRandomEmpData(emp_test_1,random=1)
gainChart_random_test_1 <- buildGainChart(emp_ref_random_prob.df)

G45 <- ggplot() + 
  geom_line(data=gainChart_random_test_1,aes(x=percCumAttritionNo,y=percCumAttritionYes,color="Random"),size=1) +
  geom_line(data=gainChart_ordered_test_1,aes(x=percCumAttritionNo,y=percCumAttritionYes,color="Ordered"),size=1) +
  scale_color_manual("",breaks=c("Random","Ordered"),
                     values=c("blue","red")) +
  scale_x_continuous(breaks=seq(0,1,0.1)) +
  scale_y_continuous(breaks=seq(0,1,0.05)) +
  labs(title="G45 - ROC Curve for Prediction based on\nAccuracy,Specificity,Senstivity\nCutOff = 0.1608",
       x="Percentage Attrition = No",
       y="Percentage Attrition = Yes")
grid.arrange(G45)


# Lets build the ROC curve 
# when prediction done through Max Accuracy
# DataFrame to consider = emp_test_2 --> This is based on cutOff2 = 0.6192
emp_ref_ordered_prob.df <- buildOrderedRandomEmpData(emp_test_2,random=0)
gainChart_ordered_test_2 <- buildGainChart(emp_ref_ordered_prob.df)


# Building random data frame
emp_ref_random_prob.df <- buildOrderedRandomEmpData(emp_test_2,random=1)
gainChart_random_test_2 <- buildGainChart(emp_ref_random_prob.df)

G46 <- ggplot() + 
  geom_line(data=gainChart_random_test_2,aes(x=percCumAttritionNo,y=percCumAttritionYes,color="Random"),size=1) +
  geom_line(data=gainChart_ordered_test_2,aes(x=percCumAttritionNo,y=percCumAttritionYes,color="Ordered"),size=1) +
  scale_color_manual("",breaks=c("Random","Ordered"),
                     values=c("blue","red")) +
  scale_x_continuous(breaks=seq(0,1,0.1)) +
  scale_y_continuous(breaks=seq(0,1,0.05)) +
  labs(title="G46 - ROC Curve for Prediction based on\nMax Accuracy\nCutOff = 0.6192",
       x="Percentage Attrition = No",
       y="Percentage Attrition = Yes")
grid.arrange(G46)


# Lets build the ROC curve 
# when prediction done through Max KS Statistics
# DataFrame to consider = emp_test_3 --> This is based on cutOff3 = 0.6192
emp_ref_ordered_prob.df <- buildOrderedRandomEmpData(emp_test_3,random=0)
gainChart_ordered_test_3 <- buildGainChart(emp_ref_ordered_prob.df)


# Building random data frame
emp_ref_random_prob.df <- buildOrderedRandomEmpData(emp_test_3,random=1)
gainChart_random_test_3 <- buildGainChart(emp_ref_random_prob.df)

G47 <- ggplot() + 
  geom_line(data=gainChart_random_test_3,aes(x=percCumAttritionNo,y=percCumAttritionYes,color="Random"),size=1) +
  geom_line(data=gainChart_ordered_test_3,aes(x=percCumAttritionNo,y=percCumAttritionYes,color="Ordered"),size=1) +
  scale_color_manual("",breaks=c("Random","Ordered"),
                     values=c("blue","red")) +
  scale_x_continuous(breaks=seq(0,1,0.1)) +
  scale_y_continuous(breaks=seq(0,1,0.05)) +
  labs(title="G47 - ROC Curve for Prediction based on\nKS-Statistics\nCutOff = 0.1465",
       x="Percentage Attrition = No",
       y="Percentage Attrition = Yes")
grid.arrange(G47)

grid.arrange(G45,G46,G47,nrow=1,ncol=3)


# If we do the same calculations above using pre-defined library #

roccurve1 <- roc(emp_test_1$Attrition,emp_test_1$AttritionPredict)
plot(roccurve1,main="ROC Curve made by pROC library\nPrediction based on Accuracy/Sensitivity/Specificity intersection CutOff = 0.1608")
auc(roccurve1)
# Area under the curve: 0.7077

roccurve2 <- roc(emp_test_2$Attrition,emp_test_2$AttritionPredict)
plot(roccurve2,main="ROC Curve made by pROC library\nPrediction based on Max Accuracy CutOff = 0.6192")
auc(roccurve2)
# Area under the curve: 0.5566

roccurve3 <- roc(emp_test_3$Attrition,emp_test_3$AttritionPredict)
plot(roccurve3,main="ROC Curve made by pROC library\nPrediction based on Max KS Statistics CutOff = 0.1465")
auc(roccurve3)
# Area under the curve: 0.7254

# From above AUC of KS statistics, is better, we can go by that



######## Inference from above analysis #######
# Model tuning : I will add this later, too tired now!!!!!!!!!!!!! :-(


## Appendix A : for p-values and VIF during data modelling ##
#|------------------------------------------------------------------------------|
#|Result For | Variable         |VIF of feature   | p-value of      |Current    |
#|Model      | considered for   |being considered | feature being   |AIC Value  |
#|Number     | removal after    |for removal      | considered for  |           |
#|           | checking results |                 | removal         |           |
#|------------------------------------------------------------------------------|
#|Model-1    | NA               | NA              |                 | 2118.4    |
#|------------------------------------------------------------------------------|
#|StepAIC    | NA               | NA              |                 | 2092.36   |
#|------------------------------------------------------------------------------|
#|Model-2    | Department.xSales| 4.689721        | 0.010109        | 2092.4    |
#|------------------------------------------------------------------------------|
#|Model-3    | JobInvolvement   |NO MORE VIF TEST | 0.135819        | 2096.7    |
#|------------------------------------------------------------------------------|
#|Model-4    | Department.x     |                 |                 |           |
#|           | Research...      |                 |                 |           |
#|           | Development      |NO MORE VIF TEST | 0.117524        | 2096.9    |
#|------------------------------------------------------------------------------|
#|Model-5    | EducationField.x |                 |                 |           |
#|           | Technical.Degree |NO MORE VIF TEST | 0.079557        | 2097.4    |
#|------------------------------------------------------------------------------|
#|Model-6    | EducationField.  |                 |                 |           |
#|           | xMarketing       |NO MORE VIF TEST | 0.109101        | 2098.6    |
#|------------------------------------------------------------------------------|
#|Model-7    | JobRole.xSales.  |                 |                 |           |
#|           | Representative   |NO MORE VIF TEST | 0.066585        | 2099.3    |
#|------------------------------------------------------------------------------|
#|Model-8    | StockOptionLevel.|                 |                 |           |
#|           | x3               |NO MORE VIF TEST | 0.056115        | 2100.9    |
#|------------------------------------------------------------------------------|
#|Model-9    | StockOptionLevel.|                 |                 |           |
#|           | x1               |NO MORE VIF TEST | 0.111252        | 2102.8    |
#|------------------------------------------------------------------------------|
#|Model-10   | JobLevel         |NO MORE VIF TEST | 0.02342         | 2103.3    |
#|------------------------------------------------------------------------------|
#|Model-11   | JobRole.xManager |NO MORE VIF TEST | 0.011651        | 2106.6    |
#|------------------------------------------------------------------------------|
#|Model-12   | BusinessTravel.x |                 |                 |           |
#|           | Travel_Rarely    |NO MORE VIF TEST | 0.007726        | 2111.6    |
#|------------------------------------------------------------------------------|
#|Model-13   |                  |                 |                 | 2117.4    |
#|------------------------------------------------------------------------------|