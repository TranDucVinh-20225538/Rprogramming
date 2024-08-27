Chapter 2
# 1. Create a text vector called Months with names of the 12 months of the year
months <- c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")

# 2. Create a numeric vector Summer, with Calendar month index positions for the summer months
summer <- c(6, 7, 8, 9)

# 3. Use vector indexing to extract the text values of Months, indexed by Summer. 
summer_months <- months[summer]


# 4. Multiply Summer by 3. What are the values of Months, when indexed by Summer multiplied by 3?“Why do you get that answer?”
summer_3 <- summer * 3
months_3 <- months[summer_3]
# i get the answer is NA because there is no value above 12 in months

# 5. “What is the mean (average) summer month, as an integer value? Which value of Months corresponds to it? Why do you get that answer?”

mean_summer <- mean(summer)
mean_summer_int <- as.integer(mean_summer)
mean_summer_month <- months[mean_summer_int]
# the average value is 7.5 and the month value corresponds is augaust becáue 7.5 is rounded up to 8

# 6. “Use the floor() and ceiling() functions to return the upper and lower limits of Months for the average Summer month. (Hint: to find out how a function works, use R help if needed.)”
floor_mean <- floor(mean_summer)
ceiling_mean <- ceiling(mean_summer)
floor_mean_month <- months[floor_mean]
ceiling_mean_month <- months[ceiling_mean]

# 7. Using the store.df data from Sect. 2.5, how many visits did Bert’s store have?
# 245 visits

# 8. Confirm that the previous answer is actually from Bert’s store
print(visit_data[1, , drop = FALSE])

# 9. *Write a function called PieArea that takes the length of a slice of pie and returns the area of the whole pie. (Assume that the pie is cut precisely, and the length of the slice is, in fact, the radius of the pie.) Note that  is the exponentiation operator in R.”

pie_area <- function(slice_length) {pi * slice_length^2}

# 10. What is PieArea for slices with lengths 4.0, 4.5, 5.0, and 6.0?
slice_lengths <- c(4.0, 4.5, 5.0, 6.0)
areas <- pie_area(slice_lengths)

# 11. “Rewrite the previous command as one line of code, without using the PieArea() function. Which of the two solutions do you prefer, and why?”
pie <- pi * (slice_lengths^2)
# i prefer the first one to the second one because it is consistent and can be reused to calculate the similar problem in later