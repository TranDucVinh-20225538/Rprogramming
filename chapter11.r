seg.ex.raw <- read.csv("https://goo.gl/s1KEiF")
seg.ex     <- seg.ex.raw                      
seg.ex$Segment <- NULL
summary(seg.ex.raw)
summary(seg.ex)

#Q1
summary_by_group <- function(data, group_var) {
  group_summary <- data %>%
    group_by(.data[[group_var]]) %>%
    summarise(across(where(is.numeric), list(mean = ~ mean(., na.rm = TRUE), 
                                             sd = ~ sd(., na.rm = TRUE)), .names = "{col}_{fn}"))
  return(group_summary)
}
summary_by_group(seg.ex, "sex")

#Q2
library(dplyr)
library(ggplot2)
seg.ex.num <- seg.ex %>%
  select(where(is.numeric))
seg.ex.scaled <- scale(seg.ex.num)
dist_matrix <- dist(seg.ex.scaled)
hclust_res <- hclust(dist_matrix, method = "ward.D2")
num_segments <- 4
segments <- cutree(hclust_res, k = num_segments)
seg.ex$cluster <- as.factor(segments)
ggplot(seg.ex, aes(x = age, y = householdIncome, color = cluster)) +
  geom_point() +
  labs(title = paste("Hierarchical clustering with", num_segments, "Segments"),
       x = "Age", y = "Household Income") +
  theme_minimal()

#Q3
#red group includes people having higher income and older
#green group includes people at average age and average incom
#blue group represents young people with the lowest income
#purple group falls between other groups in age and income

#Q4
library(ggplot2)
seg.ex$sex <- as.numeric(factor(seg.ex$sex))
seg.ex$subscribeToMusic <- as.numeric(factor(seg.ex$subscribeToMusic))
set.seed(123)  
kmeans_res <- kmeans(seg.ex, centers = 4)
seg.ex$cluster <- as.factor(kmeans_res$cluster)
ggplot(seg.ex, aes(x = age, y = householdIncome, color = cluster)) +
  geom_point() +
  labs(title = "K-means clustering with 4 segments", x = "Age", y = "Household income") +
  theme_minimal()

#Q5
#the segmentation appears logical
# that younger people generally earning less, and older individuals earning more

#Q6
library(mclust)
ex.mc <- Mclust(seg.ex.num)
library(cluster)
clusplot(seg.ex, ex.mc$class, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="Model-based cluster plot")


#Q7
ex.mc2 <- Mclust(seg.ex.num, G = 2)
summary(ex.mc2)
BIC(ex.mc2) #-52418

ex.mc3 <- Mclust(seg.ex.num, G = 3)
summary(ex.mc3)
BIC(ex.mc3) # -48860

ex.mc4 <- Mclust(seg.ex.num, G = 4)
summary(ex.mc4)
BIC(ex.mc4) # -47338
#we see that mc2<mc3<mc4 and which model has higher BIC value is the model that fit better to the data
#so when comparing with G=3, G=4 is better while G=2 is worse 

#Q8
seg.ex.po <- seg.ex
seg.ex.po$age <- factor(ifelse(seg.ex.po$age < 30, "LessThan30", "Over30"))
seg.ex.po$householdIncome <- factor(ifelse(seg.ex.po$householdIncome < 55000, "LessThan55k", "Over55k"))
seg.ex.po$kidsAtHome <- factor(ifelse(seg.ex.po$kidsAtHome > 0, "Kids", "NoKids"))
seg.ex.po$musicEnthuse <- factor(ifelse(seg.ex.po$musicEnthuse > 4, "MusicEnthuse", "NoEnthuse"))
seg.ex.po$commuteCar <- factor(seg.ex.po$commuteCar, labels = c("noCarCommute", "yesCarCommute"))
seg.ex.po$milesDrive <- NULL
seg.ex.po$drivingEnthuse <- NULL
summary(seg.ex.po)

#Q9
library(poLCA)
f <- cbind(age, householdIncome, kidsAtHome, musicEnthuse, commuteCar) ~ 1
set.seed(911) 
lc_3class <- poLCA(f, seg.ex.po, nclass = 3)
set.seed(911)
lc_4class <- poLCA(f, seg.ex.po, nclass = 4)
plot(lc_3class)
plot(lc_4class)
lc_3class$aic #4185
lc_3class$bic #4266
lc_4class$aic #4196
lc_4class$bic #4306

#the model with G=3 has lower aic and bic value so it fits better to the data than G = 4 model

#Q10
set.seed(1011)
n <- nrow(seg.ex.po)
train_indices <- sample(seq_len(n), size = floor(0.65 * n))
train_data <- seg.ex.po[train_indices, ]
test_data <- seg.ex.po[-train_indices, ]
summary(train_data)
summary(test_data)
#they seem to be similar

#Q11
install.packages("e1071")
install.packages("proxy")
library(e1071)
naive_bayes_model <- naiveBayes(cluster ~ ., data = train_data)
predictions <- predict(naive_bayes_model, test_data)
conf_matrix <- table(Predicted = predictions, Actual = test_data$cluster)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(conf_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))
chance_accuracy <- max(table(test_data$cluster)) / nrow(test_data)
print(paste("Chance Accuracy:", round(chance_accuracy * 100, 2), "%"))
# Accuracy: 75.48 %
# Chance Accuracy: 28.66 %
# it is much better than chance

#Q12
install.packages("randomForest")
library(randomForest)
set.seed(1211)  
rf_model <- randomForest(cluster ~ ., data = train_data, importance = TRUE, ntree = 500)
rf_model
oob_error_rate <- rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
print(paste("Out of bag error rate:", round(oob_error_rate * 100, 2), "%"))
# Out of bag error rate: 28.23 %

class_weights <- prop.table(table(train_data$cluster))^-1
names(class_weights) <- levels(train_data$cluster)
rf_model_weighted <- randomForest(cluster ~ ., data = train_data, importance = TRUE, ntree = 500, classwt = class_weights)
oob_error_rate_weighted <- rf_model_weighted$err.rate[nrow(rf_model_weighted$err.rate), "OOB"]
print(paste("Out of bag error rate (weighted):", round(oob_error_rate_weighted * 100, 2), "%"))
# Out of bag error rate(weighted): 24.44 %

# it seems weighted model is more useful in this case

#Q13
library(randomForest)
set.seed(1211)
rf_model <- randomForest(cluster ~ ., data = train_data, importance = TRUE, ntree = 500)
rf_model
oob_error_rate <- rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
cat("Out of bag error rate:", oob_error_rate * 100, "%\n")
#Out of bag error rate: 28.22719 %
test_predictions <- predict(rf_model, newdata = test_data)
confusion_matrix <- table(Predicted = test_predictions, Actual = test_data$cluster)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy * 100, "%\n")
#Accuracy: 73.24841 %
class_distribution <- prop.table(table(test_data$cluster))
chance_accuracy <- max(class_distribution)
cat("Chance accuracy:", chance_accuracy * 100, "%\n")
#Chance accuracy: 28.66242 %

#Q14
varImpPlot(rf_model, main="Variable importance by segment")
library(gplots)
library(RColorBrewer)
heatmap.2(t(importance(rf_model)[ , 1:6]), 
          col=brewer.pal(9, "Blues"), 
          dend="none", trace="none", key=FALSE,
          margins=c(10, 10),
          main="Variable importance by segment"
          )
#age, household income and car comute

#Q15
library(randomForest)
train_data$subscribeToMusic <- as.factor(train_data$subscribeToMusic)
set.seed(1211)
rf_model <- randomForest(subscribeToMusic ~ ., data = train_data, ntree = 500, importance = TRUE)
rf_model
test_predictions <- predict(rf_model, newdata = test_data)
confusion_matrix <- table(Predicted = test_predictions, Actual = test_data$subscribeToMusic)
confusion_matrix
#Actual
#Predicted   1   2
#        1 276  38
#        2   0   0
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", round(accuracy * 100, 2), "%\n")
#Accuracy: 87.9 %
importance_scores <- importance(rf_model)
importance_scores
varImpPlot(rf_model, main = "Variable Importance for subscribeToMusic")

#it predicts quite good
#most important variables are cluster, comute care, age and household income

