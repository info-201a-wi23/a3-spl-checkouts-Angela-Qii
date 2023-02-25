#Chart 2


library("dplyr")
library("stringr")
library("ggplot2")

SH_Checkouts_df <- read.csv("~/Downloads/Checkouts_by_Title.csv")

SH_Checkouts_df <- SH_Checkouts_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
SH_Checkouts_df$date <- as.Date(SH_Checkouts_df$date, format = "%Y-%m-%d")
SH_Checkouts_df_2005_2022 <- SH_Checkouts_df %>% filter(date < '2023-01-01')

genre_checkouts_throughout_year <- SH_Checkouts_df_2005_2022 %>% 
  mutate(Nonfiction = str_detect(Subjects, "Nonfiction") == TRUE)

ggplot(genre_checkouts_throughout_year, aes(x=CheckoutYear, y=Checkouts, fill = Nonfiction)) + 
  geom_bar(stat = "identity") +
  labs(title = "Checkouts of Fiction vs Nonfiction Self Help Books per Year", 
       x = "Checkout Year",
       y = "Number of Checkouts") +
  guides(fill = guide_legend(title = "Genre")) +
  scale_fill_discrete(labels = c("Fiction", "Nonfiction"))
