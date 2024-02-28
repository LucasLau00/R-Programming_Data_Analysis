## Install Package
install.packages("summarytools")

## library
library(janitor)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(summarytools)

## Import data
House_Rent_Data <- read.csv("C:/Users/Lucas/Desktop/PDFA ASG/House_Rent_Dataset.csv", header = TRUE, sep=",")
## Show raw data
View(House_Rent_Data)
print(as_tibble(House_Rent_Data))

## Data Cleaning start

## Re-Format Variables Names
House_Rent_Data_2 <- clean_names(House_Rent_Data)
print(as_tibble(House_Rent_Data_2))
View(House_Rent_Data_2)

## Remove Duplicate rows
House_Rent_Data_2_cleaned <- distinct(House_Rent_Data_2)
print(as_tibble(House_Rent_Data_2_cleaned))
View(House_Rent_Data_2_cleaned)

## Remove rent outliers
max(House_Rent_Data_2_cleaned$rent)
House_Rent_Data_2_cleaned %>% arrange(rent) %>% tail ## because the rent=3500000 is only one, so we decide to delete it
House_Rent_Data_2_cleaned <- subset(House_Rent_Data_2_cleaned, rent < 3500000) ## filter rent=3500000

## Check for any missing value
sum(is.na(House_Rent_Data_2_cleaned)) #0

## CLeaning end


## LAU ZI YAN TP064326
## Question 4: Identify the rental listing date in affecting on the rental price
## Analysis 4-1	During the month with most rental listing created, what Area Type is available to be rented?
## Find which month have most listing, filter by area type and which is better which has more
## MM/DD/YYYY (4,5,6,7) Area Type - Super Area & Carpet Area

House_Rent_Data_2_cleaned$posted_on <- as.Date(House_Rent_Data_2_cleaned$posted_on, format = "%m/%d/%Y")

O4Q1_monthly_counts <- House_Rent_Data_2_cleaned %>% group_by(Month = format(posted_on, "%m")) %>% 
  summarise(Total_Listings = n()) %>% 
  arrange(desc(Total_Listings))

O4Q1_monthly_bar_chart <- ggplot(data = O4Q1_monthly_counts, aes(x = Month, y = Total_Listings, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Listings Created in Different Months",
       x = "Month",
       y = "Total Listings",
       fill = "Month") +
  theme_minimal() +
  theme(legend.position = "none") 
print(O4Q1_monthly_bar_chart)


O4Q1_most_listings_month <- monthly_counts %>%
  filter(Total_Listings == max(Total_Listings)) %>%
  pull(Month)

O4Q1_filtered_data <- House_Rent_Data_2_cleaned %>% 
  filter(format(posted_on, "%m") == O4Q1_most_listings_month)

O4Q1_area_counts <- O4Q1_filtered_data %>%
  group_by(area_type) %>%
  summarise(Count = n())

# Include Built Area
print(O4Q1_area_counts)

O4Q1_filtered_areadata <- House_Rent_Data_2_cleaned %>% 
  filter(format(posted_on, "%m") == O4Q1_most_listings_month &
           area_type %in% c("Super Area", "Carpet Area"))

O4Q1_area_counts <- O4Q1_filtered_areadata %>%
  group_by(area_type) %>%
  summarise(Count = n())

# Exclude Built Area
print(O4Q1_area_counts)

O4Q1_pie_chart <- ggplot(data = O4Q1_area_counts, aes(x = "", y = "", fill = area_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Count, " (", scales::percent(Count / sum(Count)), ")")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar("y") +
  labs(title = paste("Distribution of Area Types in the Month with Most Listings ( Month: ", O4Q1_most_listings_month, ")"),
       fill = "Area Type") +
  theme_minimal() +
  theme(legend.position = "right") +
  guides(fill = guide_legend(title = "Area Type"))

print(O4Q1_pie_chart)

## During month of June, more carpet area listing, less super area
O4Q1_box_plot <- ggplot(data = O4Q1_filtered_areadata, aes(x = area_type, y = rent, fill = area_type)) +
  geom_boxplot() +
  labs(title = "Distribution of Rental Prices by Area Type",
       x = "Area Type",
       y = "Rental Price") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, 375000)) +
  scale_y_continuous(labels = scales::comma)

print(O4Q1_box_plot)

## Analysis 4-2	During the month with least rental listing created, is the average rental price lower than the month with most rental listing created.
## Find which month have most and least listing created   
## Plot in scatter plot for rental price

O4Q2_monthly_counts <- House_Rent_Data_2_cleaned %>% 
  group_by(Month = format(posted_on, "%m")) %>% 
  summarise(Total_Listings = n()) %>% 
  arrange(Total_Listings)

O4Q2_monthly_dates <- House_Rent_Data_2_cleaned %>%
  group_by(Month = format(posted_on, "%m")) %>%
  summarise(Start_Date = min(posted_on), End_Date = max(posted_on))

O4Q2_monthly_startend <- left_join(O4Q2_monthly_counts, O4Q2_monthly_dates, by = "Month")

print(O4Q2_monthly_startend)

O4Q2_least_listings_data <- House_Rent_Data_2_cleaned %>%
  filter(format(posted_on, "%m") == "05")

O4Q2_most_listings_data <- House_Rent_Data_2_cleaned %>%
  filter(format(posted_on, "%m") == "06")

O4Q2_combined_data <- rbind(
  data.frame(Month = rep("05", nrow(O4Q2_least_listings_data)), Rent = O4Q2_least_listings_data$rent),
  data.frame(Month = rep("06", nrow(O4Q2_most_listings_data)), Rent = O4Q2_most_listings_data$rent)
)

O4Q2_scatter_plot <- ggplot(data = O4Q2_combined_data, aes(x = Month, y = Rent, color = Month)) +
  geom_point() +
  labs(title = "Comparison of Rental Prices between Least and Most Listings Months",
       x = "Month",
       y = "Rental Price",
       color = "Month") +
  theme_minimal() +
  theme(legend.position = "right") +
  ylim(0, 250000)

print(O4Q2_scatter_plot)

O4Q2_specific_rent <- 100000

O4Q2_formatted_specific_rent <- sprintf("%d", O4Q2_specific_rent)
O4Q2_listings_under_specific_rent_m5 <- House_Rent_Data_2_cleaned %>%
  filter(rent < O4Q2_specific_rent, format(posted_on, "%m") == "05") %>%
  summarise(Count = n())

O4Q2_formatted_specific_rent <- sprintf("%d", O4Q2_specific_rent)
O4Q2_listings_under_specific_rent_m6 <- House_Rent_Data_2_cleaned %>%
  filter(rent < O4Q2_specific_rent, format(posted_on, "%m") == "06") %>%
  summarise(Count = n())

cat("Number of listings in Month 05 under", O4Q2_formatted_specific_rent, ":", O4Q2_listings_under_specific_rent_m5$Count)
cat("Number of listings in Month 06 under", O4Q2_formatted_specific_rent, ":", O4Q2_listings_under_specific_rent_m6$Count)

O4Q2_total_listings_m5 <- House_Rent_Data_2_cleaned %>%
  filter(format(posted_on, "%m") == "05") %>%
  summarise(Total_Listings = n())

O4Q2_total_listings_m6 <- House_Rent_Data_2_cleaned %>%
  filter(format(posted_on, "%m") == "06") %>%
  summarise(Total_Listings = n())

O4Q2_percentage_m5 <- (O4Q2_listings_under_specific_rent_m5$Count / O4Q2_total_listings_m5$Total_Listings) * 100
O4Q2_rounded_percentage_m5 <- round(O4Q2_percentage_m5)

O4Q2_percentage_m6 <- round((O4Q2_listings_under_specific_rent_m6$Count / O4Q2_total_listings_m6$Total_Listings) * 100)
O4Q2_rounded_percentage_m6 <- round(O4Q2_percentage_m6)

O4Q2_pie_data <- data.frame(Month = rep(c("05", "06"), each = 2),
                       Label = rep(c("Under 100000", "Above 100000"), times = 2),
                       Value = c(O4Q2_rounded_percentage_m5, 100 - O4Q2_rounded_percentage_m5, 
                                 O4Q2_rounded_percentage_m6, 100 - O4Q2_rounded_percentage_m6))

O4Q2_facetted_pie_chart <- ggplot(O4Q2_pie_data, aes(x = "", y = Value, fill = Label)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(Value, "%")), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Percentage of Listings under 100000", fill = "Label") +
  theme_minimal() + theme(legend.position = "right") +
  guides(fill = guide_legend(title = "Rental Range")) +
  facet_grid(. ~ Month, labeller = as_labeller(c("05" = "May (5)", "06" = "June (6)")))

print(O4Q2_facetted_pie_chart)

## Analysis 4-3 Are average daily rental prices rising or decreasing in a month and finding if there is a best date to rent a unit.
## Line Graph

library(tidyr)
library(cowplot)

O4Q3_months_to_plot <- c("05", "06")
O4Q3_line_graphs <- list()

for (month in O4Q3_months_to_plot) {
  O4Q3_month_data <- House_Rent_Data_2_cleaned %>% filter(format(posted_on, "%m") == month)
  
  O4Q3_average_price_per_day <- O4Q3_month_data %>%
    group_by(day = as.numeric(format(posted_on, "%d")), posted_on) %>%
    summarise(Avg_Rental_Price = mean(rent))
  
  O4Q3_complete_dates <- data.frame(day = 1:31)
  
  O4Q3_merged_data <- O4Q3_complete_dates %>%
    left_join(O4Q3_average_price_per_day, by = "day") %>%
    mutate(Avg_Rental_Price = coalesce(Avg_Rental_Price, 0)) 
  
  O4Q3_line_graph <- ggplot(O4Q3_merged_data, aes(x = day, y = Avg_Rental_Price)) +
    geom_line() +
    labs(title = paste("Average Rental Price Trend -", month),
         x = "Day",
         y = "Average Rental Price") +
    theme_minimal() +
    ylim(0, 80000)
  
  O4Q3_line_graphs[[month]] <- O4Q3_line_graph
}

O4Q3_combined_plot <- plot_grid(plotlist = O4Q3_line_graphs, ncol = 2)

print(O4Q3_combined_plot)

O4Q3_months_to_plot <- c("05", "06")
O4Q3_filtered_data <- House_Rent_Data_2_cleaned %>%
  filter(format(posted_on, "%m") %in% O4Q3_months_to_plot)

O4Q3_average_price_per_day <- O4Q3_filtered_data %>%
  group_by(posted_on, month = format(posted_on, "%m"), day = as.numeric(format(posted_on, "%d"))) %>%
  summarise(Avg_Rental_Price = mean(rent))

O4Q3_intersection_day <- 25.65
O4Q3_intersection_price <- 24300

O4Q3_month_colors <- c("05" = "red", "06" = "blue")

O4Q3_line_graph <- ggplot(O4Q3_average_price_per_day, aes(x = day, y = Avg_Rental_Price, color = month)) +
  geom_line() +
  geom_point(data = data.frame(day = O4Q3_intersection_day, Avg_Rental_Price = O4Q3_intersection_price), color = "black", size = 2) +
  geom_text(data = data.frame(day = O4Q3_intersection_day, Avg_Rental_Price = O4Q3_intersection_price),
            aes(x = day, y = Avg_Rental_Price, label = paste("(", day, ",", round(Avg_Rental_Price), ")")),
            vjust = -1) +
  scale_color_manual(values = O4Q3_month_colors, labels = c("May", "June")) +
  labs(title = "Average Rental Price Trend",
       x = "Day",
       y = "Average Rental Price") +
  theme_minimal() +
  ylim(0, 80000)

print(O4Q3_line_graph)

## LAU ZI YAN TP064326
## EXTRA FEATURE
## HEATMAP CALENDAR

EFLZY_filtered_data <- House_Rent_Data_2_cleaned %>%
  filter(format(posted_on, "%m") %in% c("04", "05", "06", "07"))

EFLZY_listing_counts <- EFLZY_filtered_data %>%
  group_by(month = format(posted_on, "%m"), day = format(posted_on, "%d")) %>%
  summarise(Count = n())

EFLZY_heatmap_calendar <- ggplot(EFLZY_listing_counts, aes(x = month, y = day, fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "lightpink", high = "red") +  # Adjust color scale
  labs(title = "Heatmap Calendar for April to July (Number of Listings)", x = "Month", y = "Day") +
  theme_minimal()

print(EFLZY_heatmap_calendar)