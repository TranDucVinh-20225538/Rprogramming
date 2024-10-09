sales.data.raw <- read.csv("https://goo.gl/4Akgkt")
#Q1
m1 <- lm(spendMonth ~ acctAge + visitsMonth + spendToDate + satSite + satQuality + satPrice + satOverall + region + coupon + purchase, data = sales.data.raw)
library(car)
vif(m1)
#some variables are > 5 are satprice, satoverall, satsite
# they make the resulti less exactly
# we can omit them
m2 <- lm(spendMonth ~ acctAge + visitsMonth + spendToDate + satQuality + region + coupon + purchase, data = sales.data.raw)
vif(m2)

predictions <- predict(m2, newdata = sales.data.raw)
comparison <- data.frame(Actual = sales.data.raw$spendMonth, Predicted = predictions)
head(comparison)

#Q2
library(forecast)
autoTransform <- function(x) {return(scale(BoxCox(x, BoxCox.lambda(x))))}
sales.data <- sales.data.raw
sales.data[ , -9] <- lapply(sales.data.raw[ , -9], autoTransform)
m3 <- lm(spendMonth ~ . - satSite - satPrice, sales.data)
vif(mod.tr1)
predictions_m2 <- predict(m2, newdata = sales.data.raw)
predictions_m3 <- predict(m3, newdata = sales.data)
comparison <- data.frame(
  Actual = sales.data.raw$spendMonth,
  Predicted_No_Transform = predictions_m2,
  Predicted_With_Transform = predictions_m3)
head(comparison)
#  Actual Predicted_No_Transform Predicted_With_Transform
#1     21               25.99765             -0.304518245
#2     55               21.11343             -0.007315963
#3     17               32.35050              0.015514661
#4      8               24.01842             -0.017946462
#5      9               20.39370             -0.286709865
#6     32               21.38578              0.037336595

#the negative values show that the model doesn't optimize the data.

#Q3
satisfaction_vars <- sales.data.raw[, c("satSite", "satQuality", "satPrice", "satOverall")]
pca_result <- prcomp(satisfaction_vars, scale. = TRUE)
pc_data <- data.frame(pca_result$x)
sales.data$PC1 <- pc_data$PC1
mod_pca <- lm(spendMonth ~ acctAge + visitsMonth + spendToDate + PC1 + region + coupon + purchase, data = sales.data)
summary(mod_pca)

#Q4
#a large effect that is not statistically significant such as in the West region
#it may be the result from a combination of factors: data variability, small sample sizes, multicollinearity, or unmodeled interactions

#Q5
logistic_model <- glm(purchase ~ coupon, data = sales.data.raw, family = binomial)
summary(logistic_model)

#Q6
logistic_model_full <- glm(purchase ~ coupon + region + satQuality + spendToDate, data = sales.data.raw, family = binomial)
summary(logistic_model_full)

#Q7
model_interaction <- glm(purchase ~ coupon * satQuality + region + spendToDate, family = binomial, data = sales.data.raw)
summary(model_interaction)

#Q8
#the coeffecient of coupon in the logistic_model is 1.7234
#so we can calculate the odds ratio = e^1.723 = 5.6

#Q9
# the coefecient of satquality is 0.552
# so the odds ratio = e^0.552 = 1.737
describe(sales.data.raw[ , 5:8])
#describe(sales.data.raw[ , 5:8])
#           vars   n mean   sd median trimmed  mad min max range  skew kurtosis   se
#satSite       1 835 5.71 1.53      6    5.70 1.48   1  10     9  0.02    -0.02 0.05
#satQuality    2 835 6.01 1.61      6    6.03 1.48   1  10     9 -0.13     0.00 0.06
#satPrice      3 835 5.76 1.60      6    5.76 1.48   1  10     9 -0.07     0.01 0.06
#satOverall    4 835 5.70 1.65      6    5.70 1.48   1  10     9 -0.01    -0.13 0.06

#1 unit = 1 point on the 1–10 scale.

#Q10
#question:
#is satisfaction a leading indicator of purchase, or do purchases influence satisfaction levels
#should the product offerings or pricing strategies be adjusted based on satisfaction feedback

#explain
#insights could be gathered from conducting surveys or focus groups that ask customers about their feelings before and after purchase
#by analyzing customer feedback i would expect to gather actionable insights into which aspects of my product offerings or pricing strategies are meeting or failing to meet customer expectations

conjoint.df <- read.csv("https://goo.gl/gEKSQt")

#Q11
model_price <- lm(rating ~ price, data = conjoint.df)

#Q12
bag.hlm1 <- lmer(rating ~ price + color + zipper + finish + 
                        (1 | resp.id), 
                        data = conjoint.df,
                        control=lmerControl(optCtrl=list(maxfun=100000)))
summary(bag.hlm1_simple)
#Q13
# Create a new data frame for the black bag
new_bag <- data.frame(
  price = 15,                  
  color = "black",          
  zipper = "gold",    
  finish = "matte")
predict(bag.hlm1, newdata = new_bag, re.form = NA)
#predict(bag.hlm1, newdata = new_bag, re.form = NA)
#       1 
#11.10213 

#Q14
navy_bags <- subset(conjoint.df, color == "navy")
navy_interest <- aggregate(rating ~ resp.id, data = navy_bags, FUN = mean)
most_interested <- navy_interest[which.max(navy_interest$rating), ]
least_interested <- navy_interest[which.min(navy_interest$rating), ]
most_interested
#most_interested
#    resp.id   rating
#232     232 7.571429
 least_interested
#least_interested
#    resp.id   rating
#209     209 2.714286

#Q15
library(MCMCpack)
bag.mc <- MCMChregress(
  fixed = rating ~ price + color + zipper + finish,   
  random = ~ price + color + zipper + finish,         
  group = "resp.id",                                   
  r = 6,                                               
  R = diag(6))          
summary(bag.mc$mcmc[, 1:6])





