# Install and Load libraries
library(ggplot2)
library(lubridate)
library(dplyr)
url <- "https://raw.githubusercontent.com/aiauiaii/BIAjg/refs/heads/master/supermarket_sales%20-%20Sheet1.csv"
data <- read.csv(url)
data <- read.csv(url, stringsAsFactors=FALSE)
data$Date <- as.Date(data$Date, format="%m/%d/%Y")

# 1. Total Sales by Product Line
#  total sales calculation
total_sales <- sum(data$Total, na.rm = TRUE)
product_sales <- data %>%
  group_by(`Product.line`) %>%
  summarise(Total_Sales = sum(`Total`)) %>%
  mutate(Percentage = (Total_Sales / total_sales) * 100) %>%
  arrange(desc(Total_Sales))

# visualization
ggplot(product_sales, aes(x = reorder(`Product.line`, Total_Sales), y = Total_Sales, fill = Product.line)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = 1.5, color = "white", size = 5) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Total Sales by Product Line", x = "Product Line", y = "Total Sales") +
  theme_minimal(base_size = 14)

# 2. Sales by Branch and Product Line
# branch sales calculation
branch_sales <- data %>%
  group_by(City, Product.line) %>%
  summarise(Total_Sales = sum(Total)) %>%
  ungroup()

# visualization
ggplot(branch_sales, aes(x = City, y = Total_Sales, fill = Product.line)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  scale_y_continuous(labels = scales::comma) + 
  scale_fill_brewer(palette = "Set2") +  
  labs(title = "Total Sales by Branch and Product Line", x = "Branch", y = "Total Sales") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") +
  guides(fill = guide_legend(title = NULL))

# 3. Sales by Payment Method
# payment count and percentage calculation
payment_count <- data %>%
  group_by(Payment) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# visualization
ggplot(payment_count, aes(x = "", y = Count, fill = Payment)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 1, color = "white") + 
  coord_polar("y") +
  labs(title = "Payment Method Distribution", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  
        axis.ticks = element_blank(),  
        panel.grid = element_blank(),  
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) + 
  geom_text(aes(label = paste0(Payment, "\n", Count, " (", round(Percentage, 1), "%)")), 
            position = position_stack(vjust = 0.5), color = "black", size = 4) 

# 4. Sales Trends Over Time
# daily sales calculation
daily_sales <- data %>%
  group_by(Date) %>%
  summarise(Daily_Sales = sum(Total))

# visualization
ggplot(daily_sales, aes(x = Date, y = Daily_Sales)) +
  geom_line(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE, size = 0.8) +
  labs(title = "Daily Sales Trend (Jan - Apr)", x = "Date", y = "Total Sales ($)") +
  theme_gray()

# 5. Total Sales by Branch and Gender
# total sales and percentage calculation by city and gender
city_sales <- data %>%
  group_by(City, Gender) %>%
  summarise(Total_Sales = sum(Total)) %>%
  ungroup() %>%
  group_by(City) %>%
  mutate(Percentage = Total_Sales / sum(Total_Sales) * 100) %>%
  mutate(City = reorder(City, Total_Sales))  

# plot visualization
ggplot(city_sales, aes(x = City, y = Total_Sales, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.9), 
            vjust = 1.5, color = "white", size = 4.5) +
  scale_fill_manual(values = c("salmon", "blue")) +
  labs(title = "Total Sales by Branch and Gender (Jan - Apr)", x = "Branch", y = "Total Sales ($)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") +
  guides(fill = guide_legend(title = NULL))

# 6. Customer Ratings Distribution
# average rating calculation by branch
branch_avg <- data %>%
  group_by(City) %>%
  summarise(average_rating = mean(Rating, na.rm = TRUE))

# facet histogram visualization
ggplot(data, aes(x = Rating)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  geom_vline(data = branch_avg, aes(xintercept = average_rating), color = "red", linetype = "dashed", size = 1) +
  geom_text(data = branch_avg, aes(x = average_rating, y = 0, 
                                   label = paste("Avg:", round(average_rating, 2))),
            color = "red", vjust = -48, hjust = -0.2, size = 3.5) +
  labs(title = "Distribution of Customer Ratings by Branch", x = "Rating", y = "Frequency") +
  theme_minimal() +
  facet_wrap(~ City)
