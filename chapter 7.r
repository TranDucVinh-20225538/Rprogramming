#Q1
m1 <- lm(satOverall ~ satCleanRoom + satCleanBath + satCleanCommon +satFrontStaff  + satDiningStaff + satHouseStaff +  satValetStaff   +   satPerks   +   satRoomPrice +  satDiningPrice + satWifiPrice +  satParkingPrice +   satCity +satCloseTransp + satCloseEvents+    satPoints   +  satRecognition, data = hotel.df)
library(coefplot)
coefplot(m1, main = "Coefficient Plot of Linear Model")
#we can divide the data into group acording to their meaning
service_vars <- hotel.df[, c("satCleanRoom", "satCleanBath", "satCleanCommon", "satFrontStaff", "satDiningStaff", "satHouseStaff", "satValetStaff", "satPerks", "satOverall")]
cost_vars <- hotel.df[, c("satRoomPrice", "satDiningPrice", "satWifiPrice", "satParkingPrice")]
other_vars <- hotel.df[, c("satCloseTransp", "satCloseEvents", "satPoints", "satRecognition", "distanceTraveled", "nightsStayed", "avgRoomSpendPerNight", "avgFoodSpendPerNight", "avgWifiSpendPerNight")]

#Q2
#i see that in the service variables affects each other like the clean and the overall
#about the cost variables, when customesr pay for 1 variable more thay intend to spend on other more too
#other variables also show strong coorelation for example who come from a long distance will stay more days

#Q3
cleanliness_vars <- hotel.df[, c("satCleanRoom", "satCleanBath", "satCleanCommon")]
cor_cleanliness <- cor(cleanliness_vars)
cor_cleanliness
#we can use lm()
m2 <- lm(satCleanRoom ~ satCleanBath + satCleanCommon, data = hotel.df)
#it can give us more information to evaluate the model's validity by R-squared and F-statistic

#Q4
lm(satOverall ~ satPerks, data = hotel.df)
#p value is quite small (<2.2e-16) show that they have a high statistically meaning

#Q5
lm(formula = satOverall ~ satPerks + satFrontStaff + satCity, data = hotel.df)
#Coefficients:
 # (Intercept)       satPerks  satFrontStaff        satCity  
  #  8.209e-16      1.728e-01      3.436e-01      1.274e-01  
# satPerks increases by an average of approximately 0.1728 units this case while the case before is 0.4029
#show that satPerks influences weaker when comparing with two factor
#it means satPerks is not the primary determining factor to the overall satisfaction

#Q6
lm(satRecognition ~ satFrontStaff + satCleanRoom + satPoints + satPerks, data = hotel.df)
#result is
#Coefficients:
# (Intercept)  satFrontStaff   satCleanRoom      satPoints       satPerks  
  #    1.04212        0.06718        0.03079        0.34752        0.24837  
#the result show that the satPoints has the most influence to the satRecognition and the second is satPerks
#so we should invest in satPoints and satPerks to optimize the satRecognition

#Q7
#because the data is not only from the Gold and Platinum elite member so we can not estimate the coorelation exactly
#and the present data includes only a few variables influencing satisfaction with elite recognition 
# we need more information such as income, lifestyle, preferences,...

#Q8
# i wouldn't because the coefficient for satCleanRoom is 0.03308, it is much smaller compared to the satPoints or satPerks

#Q9
lm(avgFoodSpendPerNight ~ eliteStatus + satDiningPrice, data = hotel.df)

Call:
lm(formula = avgFoodSpendPerNight ~ eliteStatus + satDiningPrice, data = hotel.df)

#Coefficients:
 #       (Intercept)  eliteStatusNoStatus  eliteStatusPlatinum    eliteStatusSilver       satDiningPrice  
  #         -0.06246              0.08230              0.11739              0.06258              0.09195  
#the coefficients don't show eliteStatusGold because it is treated as the reference category for comparison
#the result show that the Platinum customers spend more on dining than no status and the lowest is Gold customer
#while the dining price show that it is positively associated with satisfaction when the price is increasing

#Q10
# as the result in the last question we see that customers feel mor satisfied with the high price of the dining
#so we can expect that the sastifaction high when they pay more for the dinner

#Q11
m4 <- lm(avgFoodSpendPerNight ~ nightsStayed, data = hotel.df)
new_data <- data.frame(nightsStayed = 0:40)
new_data$predictedFoodSpend <- predict(m4, newdata = new_data)
ggplot(new_data, aes(x = nightsStayed, y = predictedFoodSpend)) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = hotel.df, aes(x = nightsStayed, y = avgFoodSpendPerNight), color = "red", alpha = 0.5) +
  labs(title = "Predicted average food spend per night",
       x = "Nights stayed",
       y = "Predicted average food spend per night") +
  theme_minimal()

#Q12
install.packages("dplyr")
library(dplyr)
m5 <- lm(avgFoodSpendPerNight ~ nightsStayed, data = hotel.df %>% filter(eliteStatus == "Platinum"))
data_platinum <- data.frame(nightsStayed = 0:40)
data_platinum$predictedFoodSpend <- predict(m5, newdata =data_platinum)
library(ggplot2)
ggplot(data_platinum, aes(x = nightsStayed, y = predictedFoodSpend)) +
geom_line(color = "blue", size = 1) +
geom_point(data = hotel.df %>% filter(eliteStatus == "Platinum"), 
aes(x = nightsStayed, y = avgFoodSpendPerNight), 
color = "black", alpha = 0.3) +
labs(title = "Predicted average aood spend per night for platinum members",
x = "Nights stayed",
y = "Predicted average food spend per night") +
theme_minimal()
#it shows that the customers who stay less nights spend more on dining
#it is still consistent with the previous strategy 

#Q13
install.packages("MCMCpack")
library(MCMCpack)
bayes_m1 <- MCMCregress(satRecognition ~ satFrontStaff + satCleanRoom + satPoints + satPerks, data = hotel.df)
summary(bayes_model)
#satPoints has the highest mean coeffecient show a strong coorelation with elite recognition

#Q14
bayes_coef <- c(Intercept = 1.04169, satFrontStaff = 0.06735,satCleanRoom = 0.03092,satPoints = 0.34770,satPerks = 0.24803)
linear_coef <- c(Intercept = 1.04169,satFrontStaff = 0.06735,satCleanRoom = 0.03092,satPoints = 0.34770,satPerks = 0.24803)
coef_comparison <- data.frame(Variable = names(bayes_coef),Bayesian = bayes_coef,Linear = linear_coef)
coef_comparison <- data.frame(Variable = names(bayes_coef),Bayesian = bayes_coef, Linear = linear_coef)
library(ggplot2)
ggplot(coef_comparison, aes(x = Linear, y = Bayesian)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") + labs(title = "Comparison of Bayesian and Classical Linear Model Coefficients",
x = "Classical Coefficients", y = "Bayesian Coefficients") + theme_minimal()

#they show the same result

#Q15
#i prefer the bayes more because bayes  provide full probability of distributions
# that give us more understanding of the uncertainty surrounding these estimates.