require(xgboost)


#Implementing the cross validation-procedure, creating the train and test datasets.
cross_validate_data <- function(test_data1) {
  k_fold = 5
  total_records = nrow(test_data1)
  total_columns = ncol(test_data1)
  subsets_of_data = list()
  final_list = list()
  
  print("hello")
  print(dim(test_data1))
  
  class_labels = unique(test_data1[, 3])
  print(class_labels)
  
  for (i in 1:length(class_labels)) {
    temp_data = subset(test_data1, test_data1[, 3] == class_labels[i])
    set.seed(i)
    temp_data$class_tag = sample(1:5, size = nrow(temp_data), replace = TRUE)
    subsets_of_data[[paste("label",class_labels[i])]] = temp_data
  }
  
  for (i in 1:k_fold) {
    temp_frame = data.frame()
    for (j in 1:length(subsets_of_data)) {
      temp_frame = rbind(temp_frame,
                         subset(subsets_of_data[[paste("label",class_labels[j])]], subsets_of_data[[paste("label",class_labels[j])]]$class_tag == i))
    }
    final_list[[i]] <- temp_frame[, -ncol(temp_frame)]
  }
  
  print(length(final_list))
  
  for (test in 1:length(final_list)) {
    print(dim(final_list[[test]]))
  }
  
  
  return(final_list)
}


train_data = read.csv2(
  file = "/Users/schmuck/Documents/Box Sync/AML_Project/Data/clean_unbalanced.csv",
  sep = ",",
  header = FALSE,
  stringsAsFactors = FALSE
)

# test_data1  = train_data[2:100,]
# test_data1 = rbind(test_data1,train_data[443500:443531,])
# test_data1 = as.data.frame(sapply(test_data1),as.numeric())
observe_data = as.data.frame(sapply(train_data[2:nrow(train_data),], as.numeric))
#
list_of_sets = cross_validate_data(observe_data)
set_counter =1
#
while(set_counter<=5){

  aggregate_set = data.frame()
  test_set = as.matrix(list_of_sets[[set_counter]])
  test_target = matrix(test_set[, 3])
  test_set = test_set[,-ncol(test_set)]
  temp_err = 0

  for (k in 1:length(list_of_sets)) {
    if (k != set_counter) {
      aggregate_set = rbind(aggregate_set, list_of_sets[[k]])
    }
  }

  train_target = data.matrix(aggregate_set[, 3])
  aggregate_set = aggregate_set[, -ncol(aggregate_set)]
  aggregate_set = as.matrix(aggregate_set)
  col = ncol(test_set)

  aggregate_set = xgb.DMatrix(aggregate_set,label=train_target)
  test_set = xgb.DMatrix(test_set,label=test_target)

  bstSparse <- xgboost(data = aggregate_set, label = train_target, max_depth = 4, eta = 1, nthread = 2, nrounds = 4, objective = "binary:logistic")
  pred <- predict(bstSparse, test_set)
  temp_err =  mean(as.numeric(pred > 0.5) != test_target)
  print(paste("test-error=", temp_err))
  set_counter = set_counter + 1
}

#bstSparse <- xgboost(data = observe_data, label = target, max_depth = 2, eta = 1, nthread = 2, nrounds = 3, objective = "binary:logistic")


