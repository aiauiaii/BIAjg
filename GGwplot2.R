# Install packages
#----
install.packages("ggplot2", "lubridate", "dplyr", "gridExtra", "ggtext")
#----

# Load Library
#----
library(ggplot2)
library(lubridate)
library(dplyr)
library(gridExtra)
library(ggtext)
library(grid)
#----

# Read csv
#----
url <- "https://raw.githubusercontent.com/aiauiaii/BIAjg/refs/heads/master/supermarket_sales%20-%20Sheet1.csv"
data <- read.csv(url)
data <- read.csv(url, stringsAsFactors=FALSE)
data$Date <- as.Date(data$Date, format="%m/%d/%Y")
#----

# 1. Total Sales by Product Line
#----
#  total and average sales calculation
total_sales <- sum(data$Total, na.rm = TRUE)
average_sales <- mean(product_sales$Total_Sales)
product_sales <- data %>%
  group_by(`Product.line`) %>%
  summarise(Total_Sales = sum(`Total`)) %>%
  mutate(Percentage = (Total_Sales / total_sales) * 100) %>%
  arrange(desc(Total_Sales))

# visualization
ggplot(product_sales, aes(x = reorder(`Product.line`, Total_Sales), y = Total_Sales, fill = `Product.line`)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = 1.5, color = "white", size = 5) +
  scale_fill_brewer(palette = "Set2") +
  geom_hline(yintercept = average_sales, linetype = "dashed", color = "red", size = 1) +
  geom_text(aes(x = 1, y = average_sales, label = paste("Avg:", round(average_sales, 2))), 
            vjust = -0.5, color = "red", size = 4, hjust = 0) +
  labs(title = "Total Sales by Product Line", x = "Product Line", y = "Total Sales ($)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  
#----

# 2. Sales by Branch and Product Line
#----
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
  labs(title = "Total Sales by Branch and Product Line", x = "Branch", y = "Total Sales ($)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  guides(fill = guide_legend(title = NULL))
#----


# 3. Sales by Payment Method each Branch
#----
# each city payment method counts, percentage, and rank calculation
payment_count <- data %>%
  group_by(City, Payment) %>%
  summarise(Count = n()) %>%
  group_by(City) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  mutate(Rank = rank(-Count)) %>%
  ungroup()

payment_count <- payment_count %>%
  mutate(Color = case_when(
    Rank == 1 ~ "red",      
    Rank == 2 ~ "salmon",     
    Rank == 3 ~ "pink"  
  ))

# pie chart visualization
create_pie_chart <- function(city) {
  ggplot(subset(payment_count, City == city), aes(x = "", y = Count, fill = Color)) +
    geom_bar(stat = "identity", show.legend = FALSE, width = 1) +
    coord_polar("y", start = 0) +
    scale_fill_identity() +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(city) +
    geom_text(aes(label = paste0(Payment, "\n", Count, " (", round(Percentage, 1), "%)")),
              position = position_stack(vjust = 0.5), color = "black", size = 4)
}

# Create pie charts for each city
plot_mandalay <- create_pie_chart("Mandalay")
plot_naypyitaw <- create_pie_chart("Naypyitaw")
plot_yangon <- create_pie_chart("Yangon")

# Create a text-based legend using ggplot2's annotate
# Define the legend title as a separate plot
legend_text <- ggplot() +
  theme_void() +
  ggtitle(
    "<span style='color:red'>- Most used (Red)</span>     <span style='color:salmon'>- Second Most Used (salmon)</span>     <span style='color:pink'>- Least Used (pink) -</span>"
  ) +
  theme(
    plot.title = element_markdown(hjust = 0.5, size = 20),
    plot.background = element_rect(fill = "sienna4", color = NA)
  )

# Arrange the pie charts and the text-based legend in a grid
grid.arrange(
  textGrob("Sales by Payment Method in Each Branch", gp = gpar(fontsize = 16, fontface = "bold")),
  plot_mandalay, plot_naypyitaw, plot_yangon,
  legend_text,
  ncol = 3,
  heights = c(1, 4, 1),
  layout_matrix = rbind(c(1, 1, 1), c(2, 3, 4), c(5, 5, 5))
)
#----

# 4. Sales Trends Over Time by Branch
#----
# Daily sales per branch
daily_sales <- data %>%
  group_by(Date, City) %>%
  summarise(Daily_Sales = sum(Total), .groups = 'drop')

# visualization
ggplot(daily_sales, aes(x = Date, y = Daily_Sales)) +
  geom_line(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE, size = 0.8) +
  labs(title = "Daily Sales Trend by Branch (Jan - Apr)", x = "Date", y = "Total Sales ($)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 12, face = "bold", color = "darkblue")
  ) +
  facet_wrap(~ City)

#----

# 5. Total Sales by Branch and Gender
#----
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
  theme(legend.position = "top",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  guides(fill = guide_legend(title = NULL))
#----

# 6. Customer Ratings Distribution by Branch
#----
# average rating calculation by branch
branch_avg <- data %>%
  group_by(City) %>%
  summarise(average_rating = mean(Rating, na.rm = TRUE))

# Facet histogram visualization with centered title
ggplot(data, aes(x = Rating)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  geom_vline(data = branch_avg, aes(xintercept = average_rating), color = "red", linetype = "dashed", size = 1) +
  geom_text(data = branch_avg, aes(x = average_rating, y = 0, 
                                   label = paste("Avg:", round(average_rating, 2))),
            color = "red", vjust = 1.2, hjust = -0.2, size = 3.5) +
  labs(title = "Distribution of Customer Ratings by Branch", x = "Rating", y = "Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14)
  ) +
  facet_wrap(~ City)
#----

