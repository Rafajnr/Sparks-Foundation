# changing sub-Category column name to be able to work with it
colnames(df)[9] <- "Sub_Category"
view(df)

# Grouping by Sub-Category and summarizing totals for each sub-category
df_grp_sub_category_total = df %>% group_by(Category, Sub_Category) %>% 
  summarise(total_sales = sum(Sales), total_profit = sum(Profit),
            total_quantity = sum(Quantity), total_discount = sum(Discount),.groups = "drop")

# Ordering total profit in descending order 
df_grp_sub_category_total_profit <- df_grp_sub_category_total %>% arrange(desc(total_profit))

view(df_grp_sub_category_total_profit)                                                              
                                                              
# Grouping by Sub-Category and summarizing averages for each sub-category                          
df_grp_sub_category_average = df %>% group_by(Category, Sub_Category) %>% 
  summarise(average_sales = mean(Sales), average_profit = mean(Profit),
average_quantity = mean(Quantity), average_discount = mean(Discount,.groups = "drop"))

# Ordering average discount in descending order
df_grp_sub_category_average_discount <- df_grp_sub_category_average %>% arrange(desc(average_discount))

view(df_grp_sub_category_average_discount)

# Ordering total quantity in descending order
df_grp_sub_category_total_quantity <- df_grp_sub_category_total %>% arrange(desc(total_quantity))

view(df_grp_sub_category_total_quantity)

# Barchart showing the average discount per sub category in descending order 
Sub_category_average_discount <- ggplot(df_grp_sub_category_average_discount, aes(x=reorder(Sub_Category,average_discount), y=average_discount, fill=Category)) + 
  geom_bar(stat="identity", width = 0.5, color="black") + scale_fill_discrete(palette("Dark2")) +
  labs(title = "Average Discount per Sub-Category", subtitle = "Average discount given on each transaction per category") + 
  theme_classic() + theme(axis.text.x = element_blank()) + coord_flip()

# Barchart showing the average discount per sub category in descending order
sub_category_total_quantity <- ggplot(df_grp_sub_category_total_quantity, aes(x=reorder(Sub_Category,total_quantity), y=total_quantity, fill=Category)) + 
  geom_bar(stat="identity", width = 0.5, color="black") + scale_fill_discrete(palette("Dark2")) +
  labs(title = "Total Quantity per Sub-Category", subtitle = "total amount of purchases per sub-category") + 
  theme_classic() + theme(axis.text.x = element_blank()) + coord_flip()

# Barchart showing the average discount per sub category in descending order
sub_category_total_profit <- ggplot(df_grp_sub_category_total_quantity, aes(x=reorder(Sub_Category,total_profit), y=total_profit, fill=Category)) + 
  geom_bar(stat="identity", width = 0.5, color="black") + scale_fill_discrete(palette("Dark2")) +
  labs(title = "Total Profit per Sub-Category", subtitle = "Sum of all profits made per sub-category") + 
  theme_classic() + theme(axis.text.x = element_blank()) + coord_flip()

Sub_category_average_discount + sub_category_total_quantity + sub_category_total_profit


df_grp_city_total = df %>% group_by(City) %>% 
  summarise(total_sales = sum(Sales), total_profit = sum(Profit),
            total_quantity = sum(Quantity), total_discount = sum(Discount),.groups = "drop")


view(df_grp_city_total)

df_grp_city_average = df %>% group_by(City) %>% 
  summarise(average_sales = mean(Sales), average_profit = mean(Profit),
            average_quantity = mean(Quantity), average_discount = mean(Discount),.groups = "drop")

view(df_grp_city_average)

# Ordering average discount in descending order
df_grp_city_average_discount <- df_grp_city_average %>% arrange(desc(average_discount))

# Filtering for for top 10 cities
df_grp_city_average_discount_top <- head(df_grp_city_average_discount, 10)

# Filtering for for last 10 cities
df_grp_city_average_discount_last <- tail(df_grp_city_average_discount, 10)

# Ordering total quantity in descending order
df_grp_city_total_quantity <- df_grp_city_total %>% arrange(desc(total_quantity))

# Filtering for for top 10 cities
df_grp_city_total_quantity_top <- head(df_grp_city_total_quantity, 10)

# Filtering for for last 10 cities
df_grp_city_total_quantity_last <- tail(df_grp_city_total_quantity, 10)

# Ordering total profit in descending order
df_grp_city_total_profit <- df_grp_city_total %>% arrange(desc(total_profit))

# Filtering for for top 10 cities
df_grp_city_total_profit_top <- head(df_grp_city_total_profit, 10)

# Filtering for for last 10 cities
df_grp_city_total_profit_last <- tail(df_grp_city_total_profit, 10)




# Barchart showing the average discount per city in descending order 
average_discount_city_top <- ggplot(df_grp_city_average_discount_top, aes(x=reorder(City,average_discount), y=average_discount, fill=City)) + 
  geom_bar(stat="identity", width = 0.5, color="black") + scale_fill_discrete(palette("Dark2")) +
  labs(title = "Average Discount per City (Top 10)", subtitle = "Top 10 cities that had the highest discount on average") + 
  theme_classic() + theme(axis.text.x = element_blank()) + coord_flip()

# Barchart showing the total quantity per city in descending order
total_quantity_city_top <- ggplot(df_grp_city_total_quantity_top, aes(x=reorder(City,total_quantity), y=total_quantity, fill=City)) + 
  geom_bar(stat="identity", width = 0.5, color="black") + scale_fill_discrete(palette("Dark2")) +
  labs(title = "Total Quantity per City (Top 10)", subtitle = "Top 10 cities that had the highest quantities sold") + 
  theme_classic() + theme(axis.text.x = element_blank()) + coord_flip()

# Barchart showing the total profit per sub category in descending order
total_profit_city_top <- ggplot(df_grp_city_total_profit_top, aes(x=reorder(City,total_profit), y=total_profit, fill=City)) + 
  geom_bar(stat="identity", width = 0.5, color="black") + scale_fill_discrete(palette("Dark2")) +
  labs(title = "Total Profit per City (Top 10)", subtitle = "Top 10 cities that had the highest profits on sales") + 
  theme_classic() + theme(axis.text.x = element_blank()) + coord_flip()

average_discount_city_top + total_quantity_city_top + total_profit_city_top


# Barchart showing the average discount per city in descending order 
average_discount_city_last <- ggplot(df_grp_city_average_discount_last, aes(x=reorder(City,average_discount), y=average_discount, fill=City)) + 
  geom_bar(stat="identity", width = 0.5, color="black") + scale_fill_discrete(palette("Dark2")) +
  labs(title = "Average Discount per City (Last 10)", subtitle = "Top 10 cities that had the lowest discount on average") + 
  theme_classic() + theme(axis.text.x = element_blank()) + coord_flip()

# Barchart showing the quantities purchased per city in descending order
total_quantity_city_last <- ggplot(df_grp_city_total_quantity_last, aes(x=reorder(City,total_quantity), y=total_quantity, fill=City)) + 
  geom_bar(stat="identity", width = 0.5, color="black") + scale_fill_discrete(palette("Dark2")) +
  labs(title = "Total Quantity per City (Last 10)", subtitle = "Top 10 cities that had the lowest quantities sold") + 
  theme_classic() + theme(axis.text.x = element_blank()) + coord_flip()

# Barchart showing the total profit per city in descending order
total_profit_city_last <- ggplot(df_grp_city_total_profit_last, aes(x=reorder(City,total_profit), y=total_profit, fill=City)) + 
  geom_bar(stat="identity", width = 0.5, color="black") + scale_fill_discrete(palette("Dark2")) +
  labs(title = "Total Profit per City (Last 10)", subtitle = "Top 10 cities that had the lowest profits on sales") + 
  theme_classic() + theme(axis.text.x = element_blank()) + coord_flip()

average_discount_city_last + total_quantity_city_last + total_profit_city_last
