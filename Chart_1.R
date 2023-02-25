#Chart 1


library("dplyr")
library("stringr")
library("ggplot2")
SH_Checkouts_df <- read.csv("~/Downloads/Checkouts_by_Title.csv")

SH_Checkouts_df <- SH_Checkouts_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
SH_Checkouts_df$date <- as.Date(SH_Checkouts_df$date, format = "%Y-%m-%d")
SH_Checkouts_df_2005_2022 <- SH_Checkouts_df %>% filter(date < '2023-01-01')

print_checkouts_throughout_year <- SH_Checkouts_df_2005_2022 %>%
  group_by(CheckoutYear) %>%
  filter(str_detect(UsageClass, "Physical") == TRUE) %>% 
  summarize(Checkouts = sum(Checkouts, na.rm = TRUE))

digital_checkouts_throughout_year <- SH_Checkouts_df_2005_2022 %>%
  group_by(CheckoutYear) %>%
  filter(str_detect(UsageClass, "Digital") == TRUE) %>% 
  summarize(Checkouts = sum(Checkouts, na.rm = TRUE))

digital_print_checkouts <- data_frame(print_checkouts_throughout_year) %>% 
  mutate(digital = digital_checkouts_throughout_year$Checkouts)

ggplot(data = digital_print_checkouts , x = CheckoutYear, y = pCheckouts) +
  geom_point(aes(x = CheckoutYear, y = Checkouts, colour="blue")) +
  stat_smooth(aes(x = CheckoutYear, y = Checkouts, colour="blue")) +
  geom_point(aes(x = CheckoutYear, y = digital, colour = "red")) +
  stat_smooth(aes(x = CheckoutYear, y = digital, colour="red")) +
  scale_color_identity(name = "Usage Class",
                       breaks = c("blue", "red"),
                       labels = c("Physical", "Digital"),
                       guide = "legend") +
  labs(title = "Number of Self Help Print Books Checked Out per Year", 
       x = "Checkout Year",
       y = "Number of Checkouts")
