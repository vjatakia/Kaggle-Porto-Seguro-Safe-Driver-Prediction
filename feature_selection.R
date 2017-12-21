library(ranger)
library(Boruta)
library(randomForest)
library(data.table)
library(dplyr)
library(plotly)

#Getting the balanced data
train_data = fread("balanced_train.csv")
View(train_data)


###### FEATURE SELECTION USING BORUTA ######


# Training the Boruta Model
boruta_mod= Boruta( target~., data= bal_data, doTrace = 2)

#Displaying a list of variables with the status as reject/accept
boruta_mod$finalDecision



##### FEATURE SELECTION USING RANDOM FOREST #####

train_target = train_data$target

# Removing the unnecessary variables like id
train_data$id = NULL

#Random Forest cannot handle variables with levels more than 53

#Removing the feature with 53+ features 
train_data$ps_car_11_cat = NULL


#Training the Random Forest Model 
reg<-randomForest(x=train_data[,1:ncol(train_data)],y=train_target)
summary(reg)

#Storing the importance values in a dataframe
imp = data.frame(importance(reg, type = 2))
View(imp)

#Saving the importance values in a text file 
write.csv(imp, "importance_values.txt")

#Plotting the values for visualization
plot_ly(
  x = imp_val$colname[2:nrow(imp_val)],
  y = imp_val$MeanDecreaseGini[2:nrow(imp_val)],
  name = "Importance Values",
  type = "bar"
)

#Selecting the features whose importance value is greater than 300
#Threshold can be changed as per requirement
nam=as.vector(imp_val$colname[(imp_val$MeanDecreaseGini)>300])


#Creating a new dataframe with the most important features 
tmp1 = which(names(train_data) %in% nam)
new_train_data = subset( train_data, select = tmp1)
new_train_data = new_train_data[sample(nrow(new_train_data), nrow(new_train_data)) , ]
fwrite(new_train_data, "balanced_data_new.csv")


#### The following section is optional ####

###### DIVIDING THE NEW DATA INTO TRAIN AND TEST  ######

# row_val  = 1:(nrow(new_train_data)/2)
# 
#
#
# #Storing the training set 
# new_train_set = new_train_data[row_val, ]
# sum(new_train_set$target == 1)
# sum(new_train_set$target == 0)
# fwrite(new_train_set, "balanced_train_new.csv")
# 
# 
# #Storing the testing set
# new_test_set = new_train_data[-row_val,]
# sum(new_test_set$target == 1)
# sum(new_test_set$target == 0)
# fwrite(new_test_set, "balanced_test_new.csv")










