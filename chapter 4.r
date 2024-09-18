“1.The e-commerce data set (Sect. 3.​8.​1) includes the number of visits a user made to the site (behavNumVisits). Plot this using a histogram, and then again by plotting a table of frequencies. Which plot is a better starting place for visualization, and why?”

#plot histogram 
hist(ecomm.df$behavNumVisits, 
main = "Histogram of number of visits", 
xlab = "Number of visits", 
ylab = "frequency",
col = "blue",
breaks = 30)
#plot the frequency table 
visit_frequency <- table(ecomm.df$behavNumVisits)
barplot(visit_frequency, 
main = "Frequency table of number of visits", 
xlab = "Number of visits", 
ylab = "Frequency", 
col = "green")
# i prefer the frequency table to the histogram in this case because the frequency table is describe the value in small range better than the histogram

#2.Adjust the table plot from the previous exercise to improve it. Use logarithmic values for the numbers of visits instead of raw counts, and add a chart title and axis labels.”
log_visit_frequency <- log10(visit_frequency + 1)  
barplot(log_visit_frequency, 
main = "Log-transformed frequency of visits", 
xlab = "Number of visits", 
ylab = "Log(frequency)", 
col = "orange")

#“3.The default Y axis on the previous plot is somewhat misleading. Why? Remove the default Y axis, and replace it with better labels. (Note: for logarithmic values, labels that begin with digits 1, 2, and 5—such as 1, 2, 5, 10, 20, 50, etc.—may be useful.) Make the Y axis readable for all labels.”

barplot(log_visit_frequency, 
main = "Log-transformed frequency of visits with log10 y-Axis", 
xlab = "Number of visits", 
ylab = "Log(frequency)", 
col = "purple", 
yaxt = "n")  
axis(2, at = log10(c(1, 2, 5, 10, 20, 50, 100, 200,500,1000)), 
labels = c(1, 2, 5, 10, 20, 50, 100, 200,500,1000), 
las = 1)

#“4.The variable behavPageViews is a factor variable, but we might like to do computations on the number of views. Create a new variable pageViewInt that is an integer estimate of the number of page views for each row, and add it to ecomm.df. Be conservative with the estimates; for example, when the data say “10+” views, code only as many as are indicated with confidence.”
ecomm.df$pageViewInt <- as.numeric(gsub("\\D", "", ecomm.df$behavPageviews)) 
ecomm.df$pageViewInt[is.na(ecomm.df$pageViewInt)] <- 0

#“5.Plot a histogram of the newly added integer estimate of page views (pageView Int).”
hist(ecomm.df$pageViewInt, 
main = "Histogram of page view ", 
xlab = "Page View ", 
ylab = "Frequency", 
col = "blue", 
border = "black")
 
#“6.For a first exploration, make a scatterplot for the integer estimate of page views vs. the number of site visits. Should number of visits be on a log scale? Why or why not?”
#log10
plot(log10(ecomm.df$behavNumVisits + 1), ecomm.df$pageViewInt,
main = "Page Views vs log10 of number of site visits",
xlab = "Log(Number of site visits)",
ylab = "Page views",
col = "blue", pch = 16)
#normal
plot(ecomm.df$behavNumVisits, ecomm.df$pageViewInt,
main = "Page Views vs. Number of Site Visits",
xlab = "Number of Site Visits",
ylab = "Page Views",
col = "blue", pch = 16)
#i like to use the log scale because there some oulier value and when using the log the data is more readable

#“7.There are only a few values of X and Y in the previous plot. Adjust the plot to visualize more clearly the frequencies occurring at each point on the plot.”
plot(log10(ecomm.df$behavNumVisits + 1), ecomm.df$pageViewInt,
main = "Page Views vs log10 of number of site visits",
xlab = "Log(Number of site visits)",
ylab = "Page views",
col = rgb(0, 0, 1, 0.5), 
pch = 16,                
cex = 0.7)              

#8What is the Pearson’s r correlation coefficient between number of visits and the integer estimate of page views? What is the correlation if you use log of visits instead?”
cor_visits_pageviews <- cor(ecomm.df$behavNumVisits, ecomm.df$pageViewInt, use = "complete.obs")
cor_visits_pageviews
#its [1] -0.01512686
cor_log_visits_pageviews <- cor(log10(ecomm.df$behavNumVisits + 1), ecomm.df$pageViewInt, use = "complete.obs")
cor_log_visits_pageviews
#its [1] -0.0595903

#“9 .Is the correlation from the previous exercise statistically significant?”
# yes because with p < 0.05

#“10.Is Pearson’s r a good estimate for the relationship of these two variables? Why or why not?”
# no because it gives weak correlation suggest that there is no meaningful linear relationship between site visits and page views

#11What is the polychoric correlation coefficient between number of visits and integer page views? Is it a better estimate than Pearson’s r in this case?”

library(polycor)
poly_cor <- polychor(ecomm.df$behavNumVisits, ecomm.df$pageViewInt)
print(poly_cor)
#its [1] -0.08173549 and its still < 0.05 so it isnt better than the pearson'r in this case
#12.Overall, what do you conclude about the relationship between the number of times a user has visited the site and the number of page views in a given session?”
# because it gives weak correlation so the number of visits may not significantly affect how many pages a user views in a session.

#“13.How do you load the Salaries data from the car package? (Hint: review the data() function.) Within R itself, how can you find out more detail about the Salaries data set? ”
library(car)
data(Salaries)
?Salaries

#“14.Using the Salaries data, create scatterplot matrix plots using two different plotting functions. Which do you prefer and why?”

pairs(~salary + sex + discipline + rank + yrs.service + yrs.since.phd, data = Salaries)
scatterplotMatrix(~salary + sex + discipline + rank + yrs.service + yrs.since.phd, data = Salaries)
#i prefer the scatterplotmatrix because it gives me more insights like linear regression lines

#“15.Which are the numeric variables in the Salaries data set? Create a correlation plot for them, with correlation coefficients in one area of the plot. Which two variables are most closely related?”

numeric_vars <- sapply(Salaries, is.numeric)
numeric_vars
#the result is 
# rank    discipline      yrs.since.phd   yrs.service           sex        salary 
#   FALSE         FALSE          TRUE          TRUE           FALSE          TRUE 

# Load necessary packages
library(corrplot)

numeric_data <- Salaries[, numeric_vars]
cor_matrix <- cor(numeric_data)
corrplot(cor_matrix, method="number", type="upper", tl.col="black", tl.srt=45)

# yrs.since.phd and yrs.service are most related to each other with the the highest correlation coefficient is 0.91