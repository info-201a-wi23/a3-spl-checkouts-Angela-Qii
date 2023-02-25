#Chart 3

SH_Checkouts_df <- read.csv("~/Downloads/Checkouts_by_Title.csv")

SH_Checkouts_df <- SH_Checkouts_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
SH_Checkouts_df$date <- as.Date(SH_Checkouts_df$date, format = "%Y-%m-%d")
SH_Checkouts_df_2005_2022 <- SH_Checkouts_df %>% filter(date < '2023-01-01')

material_type_checkouts <- SH_Checkouts_df_2005_2022 %>% 
  group_by(MaterialType) %>% 
  summarize(Checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  filter(Checkouts > 1000)

ggplot(data = material_type_checkouts, aes(x="Material Types", y=Checkouts, fill=MaterialType)) +
  geom_bar(stat="identity", width=1, color="Black") +
  labs(title = "Number of Checkouts from each Material Type",
       x = "Material Type",
       y = "Number of Checkouts") +
  coord_polar(theta = "y", start = 0 ) +
  geom_label_repel(data = material_type_checkouts,
                   aes(y = Checkouts, label = paste0(Checkouts, " checkouts")),
                   size = 4, show.legend = FALSE, position = position_stack(vjust = .5), direction = "y") +
  theme_void()