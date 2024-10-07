chapter 6.r
#Q1
parent_teacher <- ecomm.df[ecomm.df$profile %in% c("Parent", "Teacher"),]
parent_teacher$productKnewWhatWanted[parent_teacher$productKnewWhatWanted == ""] <- NA
parent_teacher <- na.omit(parent_teacher)
table_stats <- table(parent_teacher$profile, parent_teacher$productKnewWhatWanted)
chisq_test_result <- chisq.test(table_stats)
chisq_test_result
library(ggplot2)

ggplot(parent_teacher, aes(x = profile, fill = productKnewWhatWanted)) +
geom_bar(position = "fill") +
labs(title = "Comparison of product knowledge before visit by teachers and parents",
x = "Profile (Teacher/Parent)", y = "Proportion",
fill = "Knew Product Wanted") +
theme_minimal()

#Q2
#if we limit the observations to just Yes or No, we can concentrate on comparing 2 groups that one has known and one hasn't known it
# but if we omit the observation "Somewhat" we can lose a potential data so that we can not analyze the data generally
#the result will changed so much because there's a huge number of the observation "somewhat"
#Q3
table_counts <- table(ecomm.df$profile, ecomm.df$productKnewWhatWanted)
filtered_table <- table_counts[c("Teacher", "Parent"), c("Yes", "No")]
filtered_table
chisq_test_result <- chisq.test(filtered_table)
chisq_test_result
# the result is:
#Pearson's Chi-squared test
#data:  table_stats
#X-squared = 2.0038, df = 2, p-value = 0.3672
# because the p-value is not smaller than 0.05 so there no statistically difference between parent and teacher

#Q4
aggregate(behavPageviews ~ profile, data = ecomm.df[ecomm.df$profile %in% c("Parent", "Teacher", "Health Professional"), ], FUN = table)

#Q5
yes_parent <- sum(parent_teacher$productKnewWhatWanted[parent_teacher$profile == "Parent"] == "Yes")
no_parent <- sum(parent_teacher$productKnewWhatWanted[parent_teacher$profile == "Parent"] == "No")
n_parent <- sum(yes_parent, no_parent)
prop_parent <- yes_parent / n_parent
#[1] 0.6079137
yes_teacher <- sum(parent_teacher$productKnewWhatWanted[parent_teacher $profile == "Teacher"] == "Yes")
no_teacher <- sum(parent_teacher$productKnewWhatWanted[parent_teacher$profile == "Teacher"] == "No")
n_teacher <- sum(yes_teacher, no_teacher)
prop_teacher <- yes_teacher / n_teacher
[1] 0.5542169

#Q6
binom_result <-prop.test(yes_teacher, n_teacher, p= prop_parent)
#data:  yes_teacher out of n_teacher, null probability prop_parent
#X-squared = 0.7914, df = 1, p-value = 0.3737
#alternative hypothesis: true p is not equal to 0.6079137
#95 percent confidence interval:
# 0.4413679 0.6620051
#sample estimates:
#        p 
#0.5542169

# p-value is 0.55 is not smaller than 0.05 so there's not statistically difference
# so the 95% confidence interval is between 0.44 and 0.66

#Q7
values <- c(0, 1, 10, 2.5, 5, 8)
parent_teacher_count <- c(0, 53, 146, 50, 60,52)
profile_count <- c(5, 251, 581, 256, 301,199)
parent_teacher_view <- rep(values, parent_teacher_count)
profile_view <- rep(values, profile_count)
t.test(parent_teacher_view, profile_view)

#result is t = 1.8047, df = 538.24, p-value = 0.07167
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
# -0.03273834  0.77297132
#sample estimates:
#mean of x mean of y 
# 6.520776  6.150659 

#the parent and teacher mean is higher than the general mean
# the p value is 0.07 which is not smaller than 0.05 so there is no statistically difference
#95% Confidence Interval is from -0.03273834 to 0.77297132.

#Q8
page_views <- data.frame(
    profile =c(rep("Relative", 6), 
                 rep("Friend/family friend", 6), 
                 rep("Health Professional", 6),
                rep("Other", 6), 
                rep("Parent", 6), 
                rep("Person with [condition A]", 6),
                rep("Teacher", 6)),
 view_category = c("0", "1", "10+", "2 to 3", "4 to 6", "7 to 9"),
 counts = c(0,18,     38 ,    16   ,  20  ,   15 ,
               0,5,      9,      4,      2    ,  3,
             0, 56 ,    82 ,    47 ,    57  ,   35 ,
             0, 32 ,    28,     28,     34,     12 ,
            2, 93, 334, 119, 131, 97, 
 1    ,  9,     19 ,     4,     14    ,  5,
 2, 38, 71, 37, 43, 32 )
 )

 page_views$view_int <- with(page_views, 
   ifelse(view_category == "0", counts * 0,
   ifelse(view_category == "1", counts * 1,
   ifelse(view_category == "10+", counts * 10,
   ifelse(view_category == "2 to 3", counts * 2.5,
   ifelse(view_category == "4 to 6", counts * 5,
   ifelse(view_category == "7 to 9", counts * 8, NA)))))))

aggregate(view_int ~ profile, data = page_views, FUN = sum)
anova_result <- aov(view_int ~ profile, data = page_views)
summary(anova_result)
#            Df  Sum Sq Mean Sq F value Pr(>F)  
#profile      6 3009431  501572   1.993 0.0932 .
#Residuals   35 8809633  251704                 
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

library(ggplot2)

ggplot(page_views, aes(x = profile, y = view_int, fill = profile)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Estimated page views by profile Group",
       x = "Profile group",
       y = "Estimated page views") +
  theme_minimal()

#p value is bigger than 0.05 but smaller than 0.1 so we can consider it as a statistically at 0.1 level

#Q9
p_t_views <- subset(page_views, profile %in% c("Parent", "Teacher"))
aggregate(view_int ~ profile, data = limited_page_views, FUN = sum)
anova_result_limited <- aov(view_int ~ profile, data = limited_page_views)
summary(anova_result_limited)

ggplot(limited_page_views, aes(x = profile, y = view_int, fill = profile)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Estimated page views by profile group (Parents and Teachers)",
       x = "Profile group",
       y = "Estimated page piews") +
  theme_minimal()

#Df  Sum Sq Mean Sq F value Pr(>F)
#profile      1 1235208 1235208   1.509  0.247
#Residuals   10 8182965  818297 

#p value is 0.247 showing that theres no statistically difference
# Q8 has 7 groups that much more than 2 in Q9 that influence the result
# and there is less variability between in parent and teacher than when we considering the whole 7 group so p value is higher

