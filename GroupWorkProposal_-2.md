
1.Explain how you will conduct either your classification analysis, and which variables/columns you will use (describe the data table to us).

2.Describe at least one way that you will visualize the classification results (this is in addition to the visualization created previously for exploratory data analysis)

Firstly We will use the createDataPartition() function from the caret package to divide our data into training/testing set.In our case, we will use 75% of the data for training, and 25% for testing. Then We try out 3 different classification methods and calculate their accuracy. For Method1,We use 6 variables for predictors:P1 rank,P1 seasons,P1 titles,P2 rank,P2 seasons,P2 titles. For Method 2,We use 4 variables for predictors:P1 rank,P1 seasons,P2 rank,P2 seasons. For Method 3,We use 4 variables for predictors:P1 rank,P1 titles,P2 rank,P2 titles. Then we apply 10-fold cross-validation to our training data and deciding the number of K for each case. We will test the following k's: 1, 3, 5, 7, 9, 11. In order to find the k that brings the highest accuracy, we would create a line plot "k versus Accuracy" for each case. Then we can calculate the accuracy for each method with their best k’s. We create a bar chart to visualize the comparison of accuracy among these 3 different classifications. From the chart we would be able to conclude the best classification method.(around 200 words)



```R
# Split data into training and test sets
#Case1:
set.seed(1234)
X_train_all <- training_set %>% 
    select(P1 rank,P1 seasons,P1 titles,P2 rank,P2 seasons,P2 titles) %>% 
    data.frame()

Y_train <- training_set %>% 
    select(winner) %>% 
    unlist()

X_test_all <- test_set %>% 
    select(P1 rank,P1 seasons,P1 titles,P2 rank,P2 seasons,P2 titles) %>% 
    data.frame()

Y_test <- test_set %>% 
    select(winner) %>% 
    unlist()
```


```R
#Deciding the number of K for case1

ks <- tibble(k=c(1,3,5,7,9,11))
k = data.frame(ks)
train_control_a <- trainControl(method="cv", number = 10) 
knn_model_cv_10fold_a <- train(x = X_train_all, y = Y_train, method = "knn", tuneGrid = k,trControl = train_control_a)

accuracy_a <-knn_model_cv_10fold_a$results
accuracy_vs_k_a <- ggplot(accuracy_a, aes(x = k, y = Accuracy)) +
  geom_point() +
  geom_line()

accuracy_vs_k_a
```


```R
# Split data into training and test sets
#Case1:
X_train_b <- training_set %>% 
    select(P1 rank,P1 seasons,P2 rank,P2 seasons) %>% 
    data.frame()

X_test_b <- test_set %>% 
    select(P1 rank,P1 seasons,P2 rank,P2 seasons) %>% 
    data.frame()

```


```R
#Deciding the number of K for case2

ks <- tibble(k=c(1,3,5,7,9,11))
k = data.frame(ks)
train_control_b <- trainControl(method="cv", number = 10) 
knn_model_cv_10fold_b <- train(x = X_train_b, y = Y_train, method = "knn", tuneGrid = k,trControl = train_control_b)

accuracy_a <-knn_model_cv_10fold_a$results
accuracy_vs_k_b <- ggplot(accuracy_a, aes(x = k, y = Accuracy)) +
  geom_point() +
  geom_line()

accuracy_vs_k_b
```


```R
# Split data into training and test sets
#Case3:
X_train_c <- training_set %>% 
    select(P1 rank,P1 titles,P2 rank,P2 titles) %>% 
    data.frame()

X_test_c <- test_set %>% 
    select(P1 rank,P1 titles,P2 rank,P2 titles) %>% 
    data.frame()
```


```R
#Deciding the number of K for case3

ks <- tibble(k=c(1,3,5,7,9,11))
k = data.frame(ks)
train_control_c <- trainControl(method="cv", number = 10) 
knn_model_cv_10fold_c <- train(x = X_train_c, y = Y_train, method = "knn", tuneGrid = k,trControl = train_control_c)

accuracy_c <-knn_model_cv_10fold_c$results
accuracy_vs_k_c <- ggplot(accuracy_c, aes(x = k, y = Accuracy)) +
  geom_point() +
  geom_line()

accuracy_vs_k_c
```


```R
#Training error for case1
error <- train(x = X_train_all, y = Y_train, method = "knn", tuneGrid = data.frame(k=1)) #K's value needs to be changed.
training_pred <- predict(object=error, X_train_all)
training_results <- confusionMatrix(training_pred, Y_train)
training_results$overall[1]
```


```R
#Training error for case2
error <- train(x = X_train_b, y = Y_train, method = "knn", tuneGrid = data.frame(k=1)) #K's value needs to be changed.
training_pred <- predict(object=error, X_train_b)
training_results <- confusionMatrix(training_pred, Y_train)
training_results$overall[1]
```


```R
#Training error for case3
error <- train(x = X_train_c, y = Y_train, method = "knn", tuneGrid = data.frame(k=1)) #K's value needs to be changed.
training_pred <- predict(object=error, X_train_c)
training_results <- confusionMatrix(training_pred, Y_train)
training_results$overall[1]
```


```R
#Result(Test Data) for case1

k = data.frame(k = 1)   #K's value needs to be changed.
model_knn_all <- train(x = X_train_all, y = Y_train, method = "knn", tuneGrid = k)

Y_test_predicted_all<- predict(object = model_knn, X_test_all)

model_quality_all <- confusionMatrix(data = Y_test_predicted_all, reference = Y_test)
model_quality_all$overall[1]
```


```R
#Result(Test Data) for case2

k = data.frame(k = 1)   #K's value needs to be changed.
model_knn <- train(x = X_train_all, y = Y_train, method = "knn", tuneGrid = k)

Y_test_predicted_b<- predict(object = model_knn, X_test_b)

model_quality_b <- confusionMatrix(data = Y_test_predicted_b, reference = Y_test)
model_quality_b$overall[1]
```


```R
#Result(Test Data) for case3

k = data.frame(k = 1)   #K's value needs to be changed.
model_knn <- train(x = X_train, y = Y_train, method = "knn", tuneGrid = k)

Y_test_predicted_c<- predict(object = model_knn, X_test_c)

model_quality_c <- confusionMatrix(data = Y_test_predicted, reference = Y_test)
model_quality_c$overall[1]
```


```R
options(repr.plot.width = 8, repr.plot.height = 8) 
a <- data.frame(classifier="all",accuracy=model_quality_all$overall[1]) 
rownames(a) <- c()
b <- data.frame(classifier="b",accuracy=model_quality_b$overall[1]) 
rownames(b) <- c()
c <- data.frame(classifier="c",accuracy=model_quality_c$overall[1]) 
rownames(c) <- c()


rbind(a,b,c) %>% ggplot(aes(x=classifier,y=accuracy*100))+
    geom_bar(stat = "identity")+
    labs(x="Classifier",y="Accuracy (%)",cex.lab=10)+
    theme(text = element_text(size = 18))
```
