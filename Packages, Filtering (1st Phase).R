
## Installing packages needed for project

install.packages('tidyverse')
library('tidyverse')
library('tidyr')
library('dplyr')
library("here")
library("skimr")
library(janitor)
library(ggplot2)
install.packages("patchwork")
library("patchwork")

SampleSuperstore <- read_csv("SampleSuperstore.csv") ## Loading dataset for analysis with the name SampleSuperstore
View(SampleSuperstore) ## view loaded dataset   


#checking for null values in the data set
is.null(SampleSuperstore)

## Removing duplicates from the data set 
SampleSuperstore<-unique(SampleSuperstore)
view(SampleSuperstore)

## Showing initial summary on SampleSuperstore dataset
summary(SampleSuperstore)               

## The purpose of the analysis is to find weak areas in SampleSuperstore Sales data and suggest more profitable actions
## Three separate analysis will be made on the three different categories of products in SampleSuperstore

## filters out sales data with Furniture category only
SampleSuperstore_furniture <- filter(SampleSuperstore, Category == 'Furniture')              
View(SampleSuperstore_furniture)

## filters out sales data with Furniture category only
SampleSuperstore_OfficeSupllies <- filter(SampleSuperstore, Category == 'Office Supplies')     

## filters out sales data with Furniture category only
SampleSuperstore_Technology <- filter(SampleSuperstore, Category == 'Technology')     


# This R script involves the first phase of analysis each category of products of the SampleSuperstore

summary(SampleSuperstore_furniture)         ## initial data summary of Furniture sales
summary(SampleSuperstore_OfficeSupllies)    ## initial data summary of Office Supplies sales
summary(SampleSuperstore_Technology)        ## initial data summary of technology sales

#renaming SampleSuperstore to df for analysis
df <- SampleSuperstore

# counting the number of transactions per each category
df_grp_category_count = df %>% group_by(Category) %>% count()
view(df_grp_category_count)

#creating a new column with the column name of No_of_transactions
df_grp_category_count = mutate(df_grp_category_count, No_of_transactions = n)

#dropping column n from the df_grp_category_count table
df_grp_category_count = select(df_grp_category_count, -n)
view(df_grp_category_count)
  
#finding the average and total profit, sales revenue, discount and quantity for each category 
df_grp_category = df %>% group_by(Category) %>% 
  summarise(total_sales = sum(Sales), total_profit = sum(Profit),
    total_quantity = sum(Quantity), total_discount = sum(Discount),
    average_sales = mean(Sales), average_profit = mean(Profit),
    average_quantity = mean(Quantity), average_discount = mean(Discount),
    .groups = "drop")
                  
View(df_grp_category)

#bar charts to compare the Sales, Profit and Discount of all SampleSuperStore categories 
transactions <= ggplot(df_grp_category_count, aes(x=Category, y=No_of_transactions, fill=Category)) + 
  geom_bar(stat="identity", width = 0.5, color="black") + scale_fill_brewer(palette="Dark2") +
  labs(title = "Number of Transactions per Category", subtitle = "Note: One transaction from a client could result multiple quantities of an item purchased") + 
  theme_classic() + theme(axis.text.x = element_blank())

discount_average <- ggplot(df_grp_category, aes(x=Category, y=average_discount, fill=Category)) + 
  geom_bar(stat="identity", width = 0.5, color="black") + scale_fill_brewer(palette="Dark2") +
  labs(title = "Average Discount per Category", subtitle = "Average amount of discount given to clients for one transaction") + 
  theme_classic()

discount_total <- ggplot(df_grp_category, aes(x=Category, y=total_discount, fill=Category)) + 
  geom_bar(stat="identity", width = 0.5, color="black") + scale_fill_brewer(palette="Dark2") +
  labs(title = "Total Discount per Category", subtitle = "Total amount of discount given to clients for one transaction") + 
  theme_classic()

sales_average <- ggplot(df_grp_category, aes(x=Category, y=average_sales, fill=Category)) + 
  geom_bar(stat="identity", width = 0.5, color="black") + scale_fill_brewer(palette="Dark2") +
  labs(title = "Average Sales per Category", subtitle = "Average amount of sales revenue generated for one transaction") + 
  theme_classic()

sales_total <- ggplot(df_grp_category, aes(x=Category, y=total_sales, fill=Category)) + 
  geom_bar(stat="identity", width = 0.5, color="black") + scale_fill_brewer(palette="Dark2") +
  labs(title = "Total Sales per Category", subtitle = "Total amount of sales revenue generated from each category") + 
  theme_classic()

profit_average <- ggplot(df_grp_category, aes(x=Category, y=average_profit, fill=Category)) + 
  geom_bar(stat="identity", width = 0.5, color="black") + scale_fill_brewer(palette="Dark2") +
  labs(title = "Average Profit per Category", subtitle = "Average amount of profit generated for one transaction") + 
  theme_classic()

profit_total <- ggplot(df_grp_category, aes(x=Category, y=total_profit, fill=Category)) + 
  geom_bar(stat="identity", width = 0.5, color="black") + scale_fill_brewer(palette="Dark2") +
  labs(title = "Total Profit per Category", subtitle = "Total amount of profit generated from each category") + 
  theme_classic()

quantity_average <- ggplot(df_grp_category, aes(x=Category, y=average_quantity, fill=Category)) + 
  geom_bar(stat="identity", width = 0.5, color="black") + scale_fill_brewer(palette="Dark2") +
  labs(title = "Average Quantity per Category", subtitle = "Average quantity of a product purchased per transaction") + 
  theme_classic() 

quantity_total <- ggplot(df_grp_category, aes(x=Category, y=total_quantity, fill=Category)) + 
  geom_bar(stat="identity", width = 0.5, color="black") + scale_fill_brewer(palette="Dark2") +
  labs(title = "Total Quantity per Category", subtitle = "Total quantity of a product purchased per transaction") + 
  theme_classic() 

transactions + quantity_average + quantity_total

discount_average +discount_total

sales_average + sales_total

profit_average + profit_total

## scatterplots to determine the relationship between Profit, Sales Revenue and Discount from Office Supplies sales
furniture_profit <- ggplot(data=SampleSuperstore_furniture) + geom_point(mapping=aes(x = Discount, y = Profit, size = Profit, color = Profit )) +
  theme_classic() + labs(title= "Furniture: Profit vs Discount", subtitle = "How discount affect profit") +
  annotate("text", x=0.6, y=5000, label = "Pearson cor(r) -0.4839103")

# Pearson correlation between 2 variables
cor(SampleSuperstore_furniture$Profit, SampleSuperstore_furniture$Discount)

furniture_sales <- ggplot(data=SampleSuperstore_furniture) + geom_point(mapping=aes(x = Discount, y = Sales, size = Sales, color = Sales )) +
  theme_classic() + labs(title= "Furniture Sales: Sales vs Discount", subtitle = "How discount affect sales") +
  annotate("text", x=0.6, y=10000, label = "Pearson cor(r) -0.02880825")

# Pearson correlation between 2 variables
cor(SampleSuperstore_furniture$Sales, SampleSuperstore_furniture$Discount)

furniture_quantity <- ggplot(data=SampleSuperstore_furniture) + geom_point(mapping=aes(x = Discount, y = Quantity, size = Quantity, color = Quantity )) +
  theme_classic() + labs(title= "Furniture Sales: Quantity vs Discount", subtitle = "How discount affect quatity sold") +
  annotate("text", x=0.4, y=14, label = "Pearson cor(r) -0.01576256")

# Pearson correlation between 2 variables
cor(SampleSuperstore_furniture$Quantity, SampleSuperstore_furniture$Discount)


OfficeSupplies_profit <- ggplot(data=SampleSuperstore_OfficeSupllies) + geom_point(mapping=aes(x = Discount, y = Profit, size = Profit, color = Profit )) +
  theme_classic() + labs(title= "Office Sales: Profit vs Discount", subtitle = "How discount affect profit") +
  annotate("text", x=0.6, y=5000, label = "Pearson cor(r)-0.2087872")

# Pearson correlation between 2 variables
cor(SampleSuperstore_OfficeSupllies$Profit, SampleSuperstore_OfficeSupllies$Discount)

OfficeSupplies_sales <- ggplot(data=SampleSuperstore_OfficeSupllies) + geom_point(mapping=aes(x = Discount, y = Sales, size = Sales, color = Sales )) +
  theme_classic() + labs(title= "Office Supplies Sales: Sales vs Discount", subtitle = "How discount affect sales") +
  annotate("text", x=0.6, y=10000, label = "Pearson cor(r)-0.06943072")

# Pearson correlation between 2 variables
cor(SampleSuperstore_OfficeSupllies$Sales, SampleSuperstore_OfficeSupllies$Discount)

OfficeSupplies_quantity <- furniture_quanity <- ggplot(data=SampleSuperstore_OfficeSupllies) + geom_point(mapping=aes(x = Discount, y = Quantity, size = Quantity, color = Quantity )) +
  theme_classic() + labs(title= "Office Supplies Sales: Quantity vs Discount", subtitle = "How discount affect quatity sold") +
  annotate("text", x=0.4, y=14, label = "Pearson cor(r)0.01956852")

# Pearson correlation between 2 variables
cor(SampleSuperstore_OfficeSupllies$Quantity, SampleSuperstore_OfficeSupllies$Discount)

## scatterplots to determine the relationship between Profit, Sales Revenue and Discount from Technology sales
Technology_profit <- ggplot(data=SampleSuperstore_Technology) + geom_point(mapping=aes(x = Discount, y = Profit, size = Profit, color = Profit )) +
  theme_classic() + labs(title= "Technology Sales: Profit vs Discount", subtitle = "How discount affect profit") +
  annotate("text", x=0.6, y=8000, label = "Pearson cor(r)-0.2688532")

# Pearson correlation between 2 variables
cor(SampleSuperstore_Technology$Profit, SampleSuperstore_Technology$Discount)

## scatterplots to determine the relationship between Profit, Sales Revenue and Discount from Technology sales
Technology_sales <- ggplot(data=SampleSuperstore_Technology) + geom_point(mapping=aes(x = Discount, y = Sales, size = Sales, color = Sales )) +
  theme_classic() + labs(title= "Technology Sales: Sales vs Discount", subtitle = "How discount affect sales") +
  annotate("text", x=0.6, y=20000, label = "Pearson cor(r)0.04147965")

# Pearson correlation between 2 variables
cor(SampleSuperstore_Technology$Sales, SampleSuperstore_Technology$Discount)

Technology_quantity <- ggplot(data=SampleSuperstore_Technology) + geom_point(mapping=aes(x = Discount, y = Quantity, size = Quantity, color = Quantity )) +
  theme_classic() + labs(title= "Technology Sales: Quantity vs Discount", subtitle = "How discount affect quatity sold") +
  annotate("text", x=0.6, y=14, label = "Pearson cor(r)-0.01413123")

# Pearson correlation between 2 variables
cor(SampleSuperstore_Technology$Quantity, SampleSuperstore_Technology$Discount)

furniture_quantity + OfficeSupplies_quantity + Technology_quantity

furniture_sales + OfficeSupplies_sales + Technology_sales

furniture_profit + OfficeSupplies_profit + Technology_profit
