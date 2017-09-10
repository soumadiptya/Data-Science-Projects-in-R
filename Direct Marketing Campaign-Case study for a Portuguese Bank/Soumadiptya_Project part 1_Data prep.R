# Importing Data and using it for Data Prep

setwd("G:/Career Development/Analytics Course/Datasets/Data/Data")

#Creating a data set bd
bd=read.csv("bank-full.csv",stringsAsFactors = F,sep=";")

#Writing the function to calculate Outliers
calculator=function(x)
{
  m1=mean(x)
  dev=sd(x)
  quartile1=as.numeric(quantile(x)[2])
  quartile2=as.numeric(quantile(x)[4])
  IQR=quartile2-quartile1
  li=list(m1,dev,quartile1,quartile2,IQR)
  names(li)=c("mean","standard_deviation","q1","q2","IQR")
  return(li)
}

values_age=calculator(bd$age)
values_balance=calculator(bd$balance)

#In Project Pt. 1 we had seen that both Age and balance do'nt follow Normal Distribution
#Hence calculating Outliers as

age_lower=values_age$q1-1.5*values_age$IQR
age_upper=values_age$q2+1.5*values_age$IQR

balance_lower=values_balance$q1-1.5*values_balance$IQR
balance_upper=values_balance$q2+1.5*values_balance$IQR

#Now filtering the data set for outliers

bd = bd %>%
  filter(age>age_lower&age<age_upper) %>% 
  filter(balance>balance_lower&balance<balance_upper)

#Combining Job categories admin,self-employed and unknown into 
#cat 1 and combining housemaid and services into cat2

bd_new=bd %>% 
  mutate(job_new=ifelse(job %in% c("admin.","self-employed","unknown"),"cat1",job),
         job_new=ifelse(job_new %in% c("housemaid","services"),"cat2",job_new)) %>% select(-job)

#Combining months jan,jun and nov together to generate month_new

bd_final=bd_new %>%mutate(month_new=ifelse(month %in% c("jan","jun","nov"),"month1",month)) %>% 
  select(-month)

#Removing intermediate data set
rm(bd_new)

#Writing function to only return dummy variables for categorical variables
#here old_data is any dataset and cat_var is name of categorical variable

dummyf=function(old_data,cat_var){
  #Storing unique values in a vector
  df<-unique(old_data[,cat_var])
  #Sorting it
  df=sort(df)
  #Loop to create the dummies
  for(i in 1:(length(df)-1)){
    column_name=paste(cat_var,i,sep="_")
    old_data[,column_name]=ifelse(old_data[,cat_var]==df[i],1,0)
  }
  #Removing the original variable
  x=match(cat_var,names(old_data))
  new_data=old_data[,-x]
  return(new_data)
}

#Creating dummy variables for each of the categorical vars in data set and storing them in a new data set
#We will use data set bd_final created in earlier
#1- For job
bd_final=dummyf(bd_final,"job_new")

#2- For education
bd_final=dummyf(bd_final,"education")

#3- For marital status
bd_final=dummyf(bd_final,"marital")

#4- For default
bd_final=dummyf(bd_final,"default")

#5- For housing
bd_final=dummyf(bd_final,"housing")

#6- For contact
bd_final=dummyf(bd_final,"contact")

#7- For loan
bd_final=dummyf(bd_final,"loan")

#8- For month
bd_final=dummyf(bd_final,"month_new")

#9- For poutcome
bd_final=dummyf(bd_final,"poutcome")

#10- For y value (Variable to model on)
bd_final=bd_final %>% mutate(y_value=ifelse(y=="yes",1,0)) %>% select(-y)

glimpse(bd_final)
#This confirms that there are no more categorical variables in bd_final