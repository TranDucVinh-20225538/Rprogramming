#“1.Using the integer approximation of page views (see Exercises in Sect. 4.​10), describe page views for parents, teachers, and health professionals. Use a by() or aggregate() function as appropriate.”

filter_data <- ecomm.df[ecomm.df$profile %in% c("Parent", "Teacher", "Health Professional"), ]
filter_data$pageViewInt <- as.numeric(gsub("\\D", "", filter_data$behavPageviews))
aggregate(pageViewInt ~ profile, data = filter_data, FUN = sum)
by(filter_df$pageViewInt, filter_data$profile, sum)


#“2.Repeat the previous task, this time using a for() loop to iterate over the groups.”
groups <- unique(filter_df$profile)
results <- list()
for (group in groups) {
  group_data <- filter_df[filter_df$profile == group, ]
  results[[group]] <- sum(group_data$pageViewInt, na.rm = TRUE) 
}

results
#3“Comparing the previous two approaches—grouping versus a for() loop—which do you prefer, and why? What is a time when the other approach might be preferable?”

#i prefer using by() and aggregate() to for() because they are shorter and easier to read the data for me.
# these 2 functions are better if we have to calculate many groups in a short time
# the for() function is better if we want to draw a chart i think

#“4.What are the proportions of men and women among the various visitor profiles (teacher, parent, relative, etc.)? For this question, don’t count observations where the gender is not specified as male or female.”
filter_df <- filter_data[filter_data$gender %in% c("Male", "Female"), ]
filter_df$profile <- as.factor(filter_df$profile)
filter_df$gender <- as.factor(filter_df$gender)
histogram(~gender | profile, data=filter_df)

#
#visualization
purchase_data <- data.frame(
  profile = c("Health Professional", "Parent", "Teacher"),
  purchasedBefore.V1 = c(120, 445, 172),
  purchasedBefore.No = c(51, 154, 16),
  purchasedBefore.Yes_more_than_once = c(73, 67, 20),
  purchasedBefore.Yes_once = c(33, 110, 15)
)
purchase_data$total_purchase <- purchase_data$purchasedBefore.Yes_more_than_once + purchase_data$purchasedBefore.Yes_once
barchart(total_purchase ~ profile, data= purchase_data)

#descriptive
aggregate( total_purchase ~ profile, data= purchase_data, FUN=sum)

#
#how many purchases that each profile make?
#summary the purchases of each profile?

#
filter_df$profile <- as.factor(filter_df$profile)
filter_df$behavAnySale <- as.factor(filter_df$behavAnySale)
filter_df$gender <- as.factor(filter_df$gender)
histogram(~behavAnySale | gender + profile, data=filter_df)
