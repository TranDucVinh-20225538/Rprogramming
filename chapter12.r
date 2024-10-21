retail.raw <- readLines("https://goo.gl/wi8KHg") 
retail.margin <- read.csv("https://goo.gl/Pidzpd")
margin.short  <- data.frame(retail.margin$margin)
rownames(margin.short) <- retail.margin$item

#Q1
install.packages("arules")
library(arules)
transactions_list <- strsplit(retail.raw, ",")
trans <- as(transactions_list, "transactions")
n_items <- length(itemLabels(trans))
n_items
item_freq <- itemFrequency(trans, type = "absolute")
top_5_items <- sort(item_freq, decreasing = TRUE)[1:5]
top_5_items
basket_sizes <- size(trans)
min_basket <- min(basket_sizes)
max_basket <- max(basket_sizes)
median_basket <- median(basket_sizes)
list(min_basket = min_basket, max_basket = max_basket, median_basket = median_basket)
#$min_basket
#[1] 1
#$max_basket
#[1] 41
#$median_basket
#[1] 14


#Q2
rules <- apriori(trans, parameter = list(supp = 0.001, conf = 0.8, minlen = 2, maxlen = 5))
length(rules)
inspect(rules[1:5])
install.packages("arulesViz")
library(arulesViz)
plot(rules, measure = c("support", "confidence"), shading = "lift")

#Q3
top_lift_rules <- sort(rules, by = "lift")[1:30]
inspect(top_lift_rules)
plot(top_lift_rules, method = "graph", control = list(type = "items"))
highest_support_item <- sort(itemFrequency(trans, type = "absolute"), decreasing = TRUE)[1]
rules_with_highest_support_item <- subset(rules, items %in% names(highest_support_item))
inspect(rules_with_highest_support_item)# Xác định các luật có nhiều mặt hàng nhất
largest_group_rules <- subset(rules, size(lhs(rules)) == max(size(lhs(rules))))
inspect(largest_group_rules)


#Q4
calc_total_margin <- function(items_in_basket, margin_data) {
  margins <- margin_data[margin_data$item %in% items_in_basket, "margin"]
  total_margin <- sum(margins, na.rm = TRUE)
  return(total_margin)
}
transactions_list <- strsplit(retail.raw, ",")
retail.margin$item <- as.character(retail.margin$item)
total_margins <- sapply(transactions_list, calc_total_margin, margin_data = retail.margin)
top_10_baskets <- order(total_margins, decreasing = TRUE)[1:10]
top_10_margins <- total_margins[top_10_baskets]
top_10_baskets
#[1] 39958 71648 44470 47884 76891 97747 61431 27136  9950 96371
top_10_margins
#[1] 497.124 494.952 494.253 480.479 470.479 465.459 459.787 459.407 458.858 455.608

#Q5
total_margins <- sapply(transactions_list, calc_total_margin, margin_data = retail.margin)
high_margin_baskets <- total_margins >= 200
proportion_high_margin <- sum(high_margin_baskets) / length(total_margins)
proportion_high_margin
#[1] 0.01826993
high_margin_transactions <- transactions_list[high_margin_baskets]
high_margin_trans <- as(high_margin_transactions, "transactions")
item_freq <- itemFrequency(high_margin_trans, type = "relative")
common_items <- item_freq[item_freq >= 0.1]
common_items
barplot(sort(common_items, decreasing = TRUE), 
        main = "Frequency of items in high margin baskets (>= 10%)",
        xlab = "Items", ylab = "Frequency",
        las = 2, col = "skyblue")

#Q6
barplot(sort(common_items, decreasing = TRUE), 
        main = "Frequency of items in high margin baskets (>= 10%)",
        xlab = "Items", ylab = "Frequency",
        las = 2, 
        names.arg = names(sort(common_items, decreasing = TRUE)), 
        cex.names = 0.7, 
        col = "skyblue")

#Q7
retail.margin$proportional_margin <- retail.margin$margin / retail.margin$price
head(retail.margin$proportional_margin)
plot(retail.margin$proportional_margin, 
     main = "Proportional Margin for Each Item",
     xlab = "Items", 
     ylab = "Proportional Margin", 
     pch = 19, col = "blue")

#because proportional margin may have a skewed distribution 
#so i think logarithmic transformation might be appropriate

retail.margin$log_proportional_margin <- log(retail.margin$proportional_margin)
head(retail.margin$log_proportional_margin)
plot(retail.margin$log_proportional_margin, 
     main = "Log of Proportional Margin for Each Item",
     xlab = "Items", 
     ylab = "Log(Proportional Margin)", 
     pch = 19, col = "red")

#Q8
calc_margin_price <- function(items_in_basket, margin_data) {
  margins <- margin_data[margin_data$item %in% items_in_basket, "margin"]
  prices <- margin_data[margin_data$item %in% items_in_basket, "price"]
    total_margin <- sum(margins, na.rm = TRUE)
  total_price <- sum(prices, na.rm = TRUE)
    return(list(total_margin = total_margin, total_price = total_price))
}
calc_margin_price(c("item1", "item10"), retail.margin)
margin_price_data <- sapply(transactions_list, calc_margin_price, margin_data = retail.margin, simplify = FALSE)
total_margins <- sapply(margin_price_data, function(x) x$total_margin)
total_prices <- sapply(margin_price_data, function(x) x$total_price)
margin_to_price_ratio <- total_margins / total_prices
top_10_baskets_ratio <- order(margin_to_price_ratio, decreasing = TRUE)[1:10]
top_10_ratio <- margin_to_price_ratio[top_10_baskets_ratio]
top_10_baskets_ratio
#[1] 28.678
top_10_ratio
#[1] 553.98

