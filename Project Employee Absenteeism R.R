#Clean the environment
rm(list = ls())

# Setting the working directory
setwd("E:/data science_edwisor/Project 2 Employee Absenteesim")
getwd()

#Load the librarires
libraries = c("dummies","caret","rpart.plot","plyr","dplyr", "ggplot2","rpart","dplyr","DMwR","randomForest","usdm","corrgram","DataCombine")
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)

#Read the csv file
data1 = read.csv('Absenteeism_at_work_Project.csv', header = TRUE)

########################################EXPLORE THE DATA########################################
#Check number of rows and columns
dim(data1)

#Structure of variables
str(data1)

#Transform data types
data1$ID = as.factor(as.character(data1$ID))

data1$Reason.for.absence[data1$Reason.for.absence %in% 0] = 20
data1$Reason.for.absence = as.factor(as.character(data1$Reason.for.absence))

data1$Month.of.absence[data1$Month.of.absence %in% 0] = NA
data1$Month.of.absence = as.factor(as.character(data1$Month.of.absence))

data1$Day.of.the.week = as.factor(as.character(data1$Day.of.the.week))
data1$Seasons = as.factor(as.character(data1$Seasons))
data1$Disciplinary.failure = as.factor(as.character(data1$Disciplinary.failure))
data1$Education = as.factor(as.character(data1$Education))
data1$Son = as.factor(as.character(data1$Son))
data1$Social.drinker = as.factor(as.character(data1$Social.drinker))
data1$Social.smoker = as.factor(as.character(data1$Social.smoker))
data1$Pet = as.factor(as.character(data1$Pet))

# From the above EDA and problem statement categorising data in 2 category "continuous" and "catagorical"
#continuous_vars = c('Distance.from.Residence.to.Work', 'Service.time', 'Age',
                    'Work.load.Average.day.', 'Transportation.expense',
                    'Hit.target', 'Weight', 'Height', 
                    'Body.mass.index', 'Absenteeism.time.in.hours')

#catagorical_vars = c('ID','Reason.for.absence','Month.of.absence','Day.of.the.week',
                     'Seasons','Disciplinary.failure', 'Education', 'Social.drinker',
                     'Social.smoker', 'Son', 'Pet')

#######################################MISSING VALUE ANALYSIS########################################
#Get number of missing values
sapply(data1,function(x){sum(is.na(x))})
missing_values = data.frame(sapply(data1,function(x){sum(is.na(x))}))

#Get the rownames as new column
missing_values$Variables = row.names(missing_values)

#Reset the row names 
row.names(missing_values) = NULL

#Rename the column
names(missing_values)[1] = "Miss_perc"

#Calculate missing percentage
missing_values$Miss_perc = ((missing_values$Miss_perc/nrow(data1)) *100)

#Reorder the columns
missing_values = missing_values[,c(2,1)]

#Sort the rows according to decreasing missing percentage
missing_values = missing_values[order(-missing_values$Miss_perc),]

#Create a bar plot to visualie top 5 missing values
ggplot(data = missing_values[1:5,], aes(x=reorder(Variables, -Miss_perc),y = Miss_perc))+
  geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
  ggtitle("Missing data percentage") + theme_bw()

#Create missing value and impute using mean, median and knn
#Value = 31
#Mean = 26.67
#Median = 25
#KNN = 31
#data1[["Body.mass.index"]][3]
#data1[["Body.mass.index"]][3] = NA
#data1[["Body.mass.index"]][3] = mean(data1$Body.mass.index, na.rm = T)
#data1[["Body.mass.index"]][3] = median(data1$Body.mass.index, na.rm = T)
data1 = kNN(data = data1, k = 5)
#Check if any missing values
sum(is.na(data1))

########################################EXPLORE DISTRIBUTION USING GRAPHS########################################
#Get numerical data
numeric_index = sapply(data1, is.numeric)
numeric_data = data1[,numeric_index]

#Distribution of factor data using bar plot
bar1 = ggplot(data = data1, aes(x = ID)) + geom_bar() + ggtitle("Count of ID") + theme_bw()
bar2 = ggplot(data = data1, aes(x = Reason.for.absence)) + geom_bar() + 
  ggtitle("Count of Reason for absence") + theme_bw()
bar3 = ggplot(data = data1, aes(x = Month.of.absence)) + geom_bar() + ggtitle("Count of Month") + theme_bw()
bar4 = ggplot(data = data1, aes(x = Disciplinary.failure)) + geom_bar() + 
  ggtitle("Count of Disciplinary failure") + theme_bw()
bar5 = ggplot(data = data1, aes(x = Education)) + geom_bar() + ggtitle("Count of Education") + theme_bw()
bar6 = ggplot(data = data1, aes(x = Son)) + geom_bar() + ggtitle("Count of Son") + theme_bw()
bar7 = ggplot(data = data1, aes(x = Social.smoker)) + geom_bar() + 
  ggtitle("Count of Social smoker") + theme_bw()

gridExtra::grid.arrange(bar1,bar2,bar3,bar4,ncol=2)
gridExtra::grid.arrange(bar5,bar6,bar7,ncol=2)

#Check the distribution of numerical data using histogram
hist1 = ggplot(data = data1, aes(x =Transportation.expense)) + 
  ggtitle("Transportation.expense") + geom_histogram(bins = 25)
hist2 = ggplot(data = data1, aes(x =Height)) + 
  ggtitle("Distribution of Height") + geom_histogram(bins = 25)
hist3 = ggplot(data = data1, aes(x =Body.mass.index)) + 
  ggtitle("Distribution of Body.mass.index") + geom_histogram(bins = 25)
hist4 = ggplot(data = data1, aes(x =Absenteeism.time.in.hours)) + 
  ggtitle("Distribution of Absenteeism.time.in.hours") + geom_histogram(bins = 25)

gridExtra::grid.arrange(hist1,hist2,hist3,hist4,ncol=2)

########################################OUTLIER ANALYSIS########################################


#Get the data with only numeric columns
numeric_index = sapply(data1, is.numeric)
numeric_data = data1[,numeric_index]

#Get the data with only factor columns
factor_data = data1[,!numeric_index]


#Check for outliers using boxplots
for(i in 1:ncol(numeric_data)) {
  assign(paste0("box",i), ggplot(data = data1, aes_string(y = numeric_data[,i])) +
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour = "red", fill = "grey", outlier.size = 1) +
           labs(y = colnames(numeric_data[i])) +
           ggtitle(paste("Boxplot: ",colnames(numeric_data[i]))))
}

#Arrange the plots in grids
gridExtra::grid.arrange(box1,box2,box3,box4,ncol=2)
gridExtra::grid.arrange(box5,box6,box7,box8,ncol=2)
gridExtra::grid.arrange(box9,ncol=2)

#Get the names of numeric columns
numeric_columns = colnames(numeric_data)

#Replace all outlier data with NA
for(i in numeric_columns){
  val = data1[,i][data1[,i] %in% boxplot.stats(data1[,i])$out]
  print(paste(i,length(val)))
  data1[,i][data1[,i] %in% val] = NA
}

#Check number of missing values
sapply(data1,function(x){sum(is.na(x))})

#Get number of missing values after replacing outliers as NA
missing_values_out = data.frame(sapply(data1,function(x){sum(is.na(x))}))
missing_values_out$Columns = row.names(missing_values_out)
row.names(missing_values_out) = NULL
names(missing_values_out)[1] = "Miss_perc"
missing_values_out$Miss_perc = ((missing_values_out$Miss_perc/nrow(data1)) *100)
missing_values_out = missing_values_out[,c(2,1)]
missing_values_out = missing_values_out[order(-missing_values_out$Miss_perc),]
missing_values_out

#Compute the NA values using KNN imputation
data1 = kNN(data1, k = 5)

#Check if any missing values
sum(is.na(data1))

#-----------------------------------Feature Selection------------------------------------------#
## Correlation Plot 
corr = round(cor(numeric_data),2)
ggcorrplot(corr,hc.order = T,
           type = "full",
           lab = T,
           lab_size = 3,
           method = "square",
           colors = c("blue","white","darkgreen"),
           title = "Correlation Plot",
           ggtheme = theme_bw)
## Dimension Reduction
data1 = subset(data1, select = -c(Body.mass.index))

########################################FEATURE SCALING########################################
#Normality check
hist(data1$Absenteeism.time.in.hours)
#Remove dependent variable
numeric_index = sapply(data1,is.numeric)
numeric_data = data1[,numeric_index]
numeric_columns = names(numeric_data)
numeric_columns = numeric_columns[-9]

#Normalization of continuous variables
for(i in numeric_columns){
  print(i)
  data1[,i] = (data1[,i] - min(data1[,i]))/
    (max(data1[,i]) - min(data1[,i]))
}

### Modeling
#Dividing into test and train seta
t_idx = sample(1:nrow(data1), 0.8*nrow(data1))
train = data1[t_idx,]
test = data1[-t_idx,]
# Removing all the custom variables from the memory
rmExcept(c("test","train","data1"))

########################################DECISION TREE########################################

#RMSE: 2.276
#MAE: 1.694
#R squared: 0.44
#Build decsion tree using rpart
dt_model = rpart(Absenteeism.time.in.hours ~ ., data = train, method = "anova")
#Perdict for test cases
dt_predictions = predict(dt_model, test[,-115])
#Create data frame for actual and predicted values
df_pred = data.frame("actual"=test[,115], "dt_pred"=dt_predictions)
head(df_pred)
#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = dt_predictions, obs = test[,115]))

########################################RANDOM FOREST########################################
#RMSE: 2.194
#MAE: 1.61
#R squared: 0.479

##Train the model using training data
rf_model = randomForest(Absenteeism.time.in.hours~., data = train, ntree = 500)

#Predict the test cases
rf_predictions = predict(rf_model, test[,-115])

#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,rf_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = rf_predictions, obs = test[,115]))


########################################LINEAR REGRESSION########################################
#RMSE: 2.559
#MAE: 1.86
#R squared: 0.358

##Train the model using training data
lr_model = lm(formula = Absenteeism.time.in.hours~., data = train)

#Get the summary of the model
summary(lr_model)

#Predict the test cases
lr_predictions = predict(lr_model, test[,-115])

#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,lr_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = lr_predictions, obs = test[,115]))
