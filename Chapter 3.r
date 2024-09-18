#1.How many observations and variables are in the e-commerce data set?
#we use dim
dim(ecomm.df)
# 1593 observations and 45 variables

# 2.Compute a frequency table for the country of origin for site visits. After the United States, which country had the most visitors?
country_frequency <- table(ecomm.df$country)
sorted_country_frequency <- sort(country_frequency, decreasing = TRUE)
sorted_country_frequency[2]

# 3. Compute a two-way frequency table for the intent to purchase (intentWasPlanningToBuy), broken out by user profile.
intent_to_buy <- table(ecomm.df$intentWasPlanningToBuy, ecomm.df$profile)
intent_to_buy
#4. What are the proportions of parents who intended to purchase? The proportions of teachers who did? Omit observations where intent is unknown (blank).
was_intent <- ecomm.df$intentWasPlanningToBuy != ""
parent_proportion <- prop.table(table(ecomm.df$intentWasPlanningToBuy[was_intent & ecomm.df$profile == "Parent"]))
teacher_proportion <- prop.table(table(ecomm.df$intentWasPlanningToBuy[was_intent & ecomm.df$profile == "Teacher"]))
parent_proportion
teacher_proportion
#5. Among US states (recorded in the variable region), which state had the most visitors and how many?
us_visitors <- ecomm.df$region[ecomm.df$country == "United States"]
state_frequency <- table(us_visitors)
most_visitors_state <- sort(state_frequency, decreasing = TRUE)[1]
most_visitors_state #it is TX
#6. Solve the previous problem for the state with the most visitors, using the which.max() function.
most_visitors_state <- state_frequency[which.max(state_frequency)]
names(most_visitors_state)
#7. Draw a histogram for the number of visits to the site (behavNumVisits). Adjust it for more detail in the lower values. Color the bars and add a density line.
hist(ecomm.df$behavNumVisits,breaks=40,col = "red",
main = "Histogram of Site Visits",
xlab = "Number of Site Visits",ylab = "Density",
freq = FALSE)
lines(density(ecomm.df$behavNumVisits), col = "blue", lwd = 1)
#8. Draw a horizontal boxplot for the number of site visits.
boxplot(ecomm.df$behavNumVisits, horizontal = TRUE,
main = "Number of Site Visits by Profile",
xlab = "Number of Site Visits",
col = "blue")                   
axis(side = 2, at = c(1), labels = c("Site Visits"))

#9. Which chart from the previous two exercises, a histogram or a boxplot, is more useful to you, and why?
#there are some ouliers in the dataset so i prefer the boxplot to the histogram because the data is in a long range that the boxplit is more useful for me to understanding the whole range

#10. Draw a boxplot for site visits broken out with a unique row for each profile type.
boxplot(behavNumVisits ~ profile,
data = ecomm.df, horizontal = TRUE, 
main = "Boxplot of Site Visits by Profile Type", xlab = "Number of Site Visits", col = "lightblue")

#11. Write a function called MeanMedDiff that returns the absolute difference between the mean and the median of a vector.
MeanMedDiff <- function(x){return(abs(mean(x) - median(x)))}
#12. What is the mean-median difference for the number of site visits?
#we use the function
MeanMedDiff(ecomm.df$behavNumVisits)
#the result is [1] 0.7715003
#13. What is the mean-median difference for site visits, after excluding the person who had the most visits?
excluded_visits <- ecomm.df$behavNumVisits[ecomm.df$behavNumVisits != max(ecomm.df$behavNumVisits)]
MeanMedDiff(excluded_visits)
# the result is [1] 0.7091709
#14. Use the apply() function to find the mean-median difference for the 1/0 coded behavioral variables for onsite behaviors.
behavioral <- ecomm.df[, c("behavDetailProdB", "behavDetailProdC", "behavAnySolution",  "behavAnySale ",     " behavCart ",     "behavConversion" )]
apply(behavioral, 2, MeanMedDiff)
#the result is
# behavHomePage behavDetailProdA behavDetailProdB behavDetailProdC behavAnySolution     behavAnySale        behavCart  behavConversion 
#    0.40112994       0.36974262       0.23854363       0.11676083       0.11801632       0.09478970       0.11738858       0.07093534 

#15. Write the previous command using an anonymous function.
apply(behavioral,2,function(x)abs(mean(x)-median(x)))

#16. Do you prefer the named function or an anonymous function? Why? What is a situation for each in which it might be preferable?
# each has the advantages and disadvantages. named function can be reused many times but much complex more than anonynous function which useful when the function is simple and it only used once or twice but if i have to do it multiple times i prefer the named function
ecomm.df <- read.csv ("https: // goo. gl/hzRyFd")
﻿﻿summary (ecomm. df)



>


