library(dplyr)
library(ROSE)
library(data.table)

#Getting the data
insurance= read.csv("train.csv")

######  Handling Missing Values ######

# Handling Column-wise Missing Values
col_miss=colSums(insurance == -1)
plot(col_miss, main= "Missing Data")
lines(col_miss)

#Removing the columns with over 50% missing values
insurance[,which(col_miss >= 260000) ] = NULL

#Handling Row-wise Missing values
c_row=rowSums(insurance == -1)
sum(c_row>0)
c_i= which(c_row>0)

#Removing the rows with missing values
insurance= insurance[-c_i, ]

###### Balancing the Data using ROSE ######

#Converting the _cat and _bin features to factor
insurance1 = insurance
cat_col = grep("_cat", names(insurance1), value = T)
bin_col = grep("_bin", names(insurance1), value = T)
insurance1[cat_col]= lapply(insurance1[cat_col], factor)
insurance1[bin_col]= lapply(insurance1[bin_col], factor)

#Applying the ROSE method
balanced_insurance= ROSE( target~., data = insurance1, seed = 126 )$data
sum(balanced_insurance$target == 0)
sum(balanced_insurance$target == 1)

#Saving the cleaned file
fwrite(balanced_insurance, "balanced_train.csv")








