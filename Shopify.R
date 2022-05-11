shopify <- read.csv("2019 Winter Data Science Intern Challenge Data Set - Sheet1.csv"); View(shopify)
summary(shopify)
mean(shopify$order_amount) 

# creating a new column to accumulate AOV of individual shops
shopify$avg_order_value <- shopify$order_amount/shopify$total_items

# Visulaizing the key columns
hist(x = shopify$order_amount)
items_vis <- boxplot(x = shopify$total_items) 
# There is some outlier in the above region (2000). Since the plot is not clearly visible, we enlarge the plog by using log transformation
# Transformed_Boxplot
transformed_items_vis <- boxplot(x = log(shopify$total_items))
transformed_items_vis$out
items_vis$out # So the order with total_item of 8 and 2000 is determined as an outlier by the boxplot. Lets take a closer look on these values

# Exploring the outlier and their counts
View(subset(shopify, total_items > 8)) # There are 17 entries in total having total_items of 2000 which was ordered by the same user in the same shop. 
# So, we will drop these false entries values considering as an outlier. 

# Exploring the order with totalitems of 8
item8 <- subset(new_shopify, total_items == 8); item8 # Considering the order amount of 1064 and the AOV of that shop for this particular purchase, 
# we consider this not as an outlier and proceed with modelling.

# Creating a new table with ordered items less than or equal to 8
new_shopify <- subset(shopify, total_items <= 8)
View(new_shopify)
dim(new_shopify)

# Regression Modelling
regression1 <- lm(order_amount ~ total_items, data = new_shopify)
summary(regression1)
# Interpretation: order_amount = 24.28 + 366.01*total_items
  
regression2 <- lm(avg_order_value ~ order_amount + total_items, data = new_shopify)
summary(regression2)
# Interpretation: avg_order_value = 417.8 + 0.4074*order_amount - 169.1*total_items

# AOV Calculation
total_revenue = sum(new_shopify$order_amount); total_revenue
total_orders = sum(new_shopify$total_items); total_orders
AOV <- total_revenue/total_orders; AOV

