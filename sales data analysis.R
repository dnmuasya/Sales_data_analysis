install.packages('tidyverse')
library(tidyverse)
install.packages('ggplot2')
library(ggplot2)
install.packages('lubridate')
library(lubridate)


getwd() 
# STEP 1
# downloaded the data, and uploaded it in Github
download.file(url = 'https://raw.githubusercontent.com/dnmuasya/Data-Sales-Analysis/main/Sales%20Data.csv',
              destfile = '/cloud/project/data_raw/sales-data.csv')

# reading into the data
sales_data <- read_csv('/cloud/project/data_raw/sales-data.csv')
# inspect and preview the data
str(sales_data)
head(sales_data)
View(sales_data)
colnames(sales_data)
nrow(sales_data)

# STEP 2
# extracting time
sales_data$Time <- format(as.POSIXct(sales_data$`Order Date`),format = "%H:%M:%S")

# extracting date
sales_data$Date <- as.Date (sales_data$`Order Date`)

# extracting year
sales_data$Year <- format(sales_data$`Order Date`, "%Y")
str(sales_data)

# dropping unnecessary columns
sales_data_v2 <- sales_data %>%
  select(c(-`Order Date`))
View(sales_data_v2)
tail(sales_data_v2)


# STEP 3
# data visualization
# daily sales
daily_sales <- sales_data_v2 %>%
  group_by(Date) %>%
  summarize(daily_sales = sum(Sales, na.rm = TRUE))
daily_sales

# group by month
monthly_data <- sales_data_v2 %>%
  group_by(Year, Month = factor(month.abb[sales_data_v2$Month], levels = month.abb)) %>%
  summarize(monthly_sales = sum(Sales))
monthly_data
monthly_data %>%
  ggplot( aes(x = Month, y = monthly_sales / 1000000, fill = "Sales"))+ 
  geom_col() + 
  theme(axis.text.x=element_text(angle=45, hjust=0.9)) +
  labs(title = "2019 Monthly Sales(in million)")
ggsave("figures/2019 Monthly Sales.png")

# yearly data
yearly_sales <- sales_data_v2 %>%
  group_by(Year, ) %>%
  summarize(yearly_sales = sum(Sales))
yearly_sales
yearly_sales %>%
  ggplot(aes(x = Year, y = yearly_sales, fill = "Sales"))+
  geom_col() + 
  facet_wrap(~Year)
  labs(title = "Sales by Year")
ggsave("figures/Yearly Sales.png")

#total orders = 209079
sum(sales_data_v2$`Quantity Ordered`, na.rm = TRUE)

#total revenue = 34492036
sum(sales_data_v2$Sales, na.rm = TRUE)

#top 10 Revenue Generating Products
selected_data <- sales_data_v2%>%
  group_by(Product) %>%
  summarize(`Total Product Sales(million)` = sum(order(Sales)/1000000))
selected_data <-selected_data[order(-selected_data$`Total Product Sales(million)`),]
selected_data

selected_data %>%
  slice_max(selected_data$`Total Product Sales(million)`, n=10) %>%
  ggplot(aes(x = reorder(Product,`Total Product Sales(million)`) , y = `Total Product Sales(million)`, fill = "Sales"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ 
  geom_col() + 
  coord_flip() +
  labs(title = "Top 10 Revenue Generating Products")
ggsave("figures/Top 10 Revenue Generating Products.png")
  
             
#City Sales Ranking
city_data <- sales_data_v2%>%
  group_by(City) %>%
  summarize(`Total City Purchases(million)` = sum(order(Sales)/1000000))
city_data <- city_data[order(city_data$`Total City Purchases(million)`),]
city_data

city_data %>%
  ggplot(aes(x = City, y = `Total City Purchases(million)`, fill = "Sales"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ 
  geom_col() +
  geom_text(aes(label = `Total City Purchases(million)`),vjust = 0) +
  coord_flip() +
  labs(title = "City Sales Ranking")
ggsave("figures/City Sales Ranking.png")
 
#hourly sales per product
hourly_sales <- sales_data_v2 %>%
  group_by(Product, `Quantity Ordered`, Hour) %>%
  summarise(Sales) %>%
  arrange(desc(Sales)) 
hourly_sales
hourly_sales %>%
  ggplot(aes(x = Hour)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_line(color = 'Blue') +
  labs(title = "Hourly Sales")
  
 
