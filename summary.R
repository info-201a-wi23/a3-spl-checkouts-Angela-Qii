library("dplyr")
library("stringr")
library("ggplot2")

SH_Checkouts_df <- read.csv("~/Downloads/Checkouts_by_Title.csv")

SH_Checkouts_df <- SH_Checkouts_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
SH_Checkouts_df$date <- as.Date(SH_Checkouts_df$date, format = "%Y-%m-%d")
SH_Checkouts_df_2005_2022 <- SH_Checkouts_df %>% filter(date < '2023-01-01')

#Average checkout for each self help title in a month since 2005.
avg_checkouts_month_per_title <- round(mean(SH_Checkouts_df_2005_2022$Checkouts), 4)

#Average checkout for all self help books in a month since 2005.
avg_checkouts_month <-SH_Checkouts_df_2005_2022 %>% 
                      group_by(date) %>% 
                      summarize(Checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
                      summarize(Checkouts = mean(Checkouts, na.rm = TRUE))

#Average checkout for all self help books in a year since 2005.
avg_checkouts_year <- SH_Checkouts_df_2005_2022 %>% 
                      group_by(CheckoutYear) %>% 
                      summarize(Checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
                      summarize(Checkouts = mean(Checkouts, na.rm = TRUE))

#Number of self help books checked out per year?
avg_checkouts_throughout_year <- SH_Checkouts_df_2005_2022 %>% 
  group_by(CheckoutYear) %>% 
  summarize(Checkouts = sum(Checkouts, na.rm = TRUE))


#Month or year with the most checkouts for self-help Books
max_checkouts_year <- SH_Checkouts_df_2005_2022 %>% 
  group_by(CheckoutYear) %>% 
  summarize(Checkouts = max(Checkouts, na.rm = TRUE)) %>% 
  filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% 
  pull(CheckoutYear)

#Month or year with the least checkouts for self-help ebooks?
min_checkouts_year <- SH_Checkouts_df_2005_2022 %>% 
  group_by(CheckoutYear) %>% 
  summarize(Checkouts = max(Checkouts, na.rm = TRUE)) %>% 
  filter(Checkouts == min(Checkouts, na.rm = TRUE)) %>% 
  pull(CheckoutYear)

#How has the number of print book checkouts changed over time?
print_checkouts_throughout_year <- SH_Checkouts_df_2005_2022 %>%
  group_by(CheckoutYear) %>%
  filter(str_detect(UsageClass, "Physical") == TRUE) %>% 
  summarize(Checkouts = sum(Checkouts, na.rm = TRUE))

#How has the number of digital book checkouts changed over time?
digital_checkouts_throughout_year <- SH_Checkouts_df_2005_2022 %>%
  group_by(CheckoutYear) %>%
  filter(str_detect(UsageClass, "Digital") == TRUE) %>% 
  summarize(Checkouts = sum(Checkouts, na.rm = TRUE))

  
#What are the most checked out Titles of self help books throughout the years?
max_checkout_title_throughout_year <- SH_Checkouts_df_2005_2022 %>% 
            group_by(CheckoutYear) %>%
            filter(Checkouts == max(Checkouts, na.rm = TRUE))


