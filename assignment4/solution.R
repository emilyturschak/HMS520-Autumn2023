#Assignment 4
library("readr")
library("dplyr")
library("ggplot2")

#Prepare data
#save the counts data frame to current folder assignment4

saveRDS(counts, file = "assignment4/counts_data.rds")

#Read data and change column name "mr" to "mortality rate" and "ifr" to "infection fatality rate"

loaded_counts <- readRDS("assignment4/counts_data.rds")
loaded_counts <- loaded_counts %>%
  rename(`mortality rate` = mr, `infection fatality rate` = ifr)
saveRDS(loaded_counts, file = "assignment4/counts_data_modified.rds")

#Create scatter plot
scatter_plot <- loaded_counts %>%
  ggplot(aes(x = date, y = `mortality rate`)) +
  geom_point() +
  facet_wrap(~state, nrow = 10) +
  scale_y_log10() +
  labs(title = "Mortality Rate") +
  ggsave("mr_by_state_date.png", width = 10, height = 20)

#Create boxplot 
box_plot <- loaded_counts %>%
  ggplot(aes(x = reorder(state, `mortality rate`), y = `mortality rate`)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Mortality Rate", x = "State", y = "Mortality Rate") +
  ggsave("mr_by_state.png", width = 8, height = 15)

#Create histogram
mean_counts <- loaded_counts %>%
  group_by(state) %>%
  summarise(mean_mr = mean(`mortality rate`, na.rm = TRUE),
            mean_ifr = mean(`infection fatality rate`, na.rm = TRUE))

mean_counts_long <- mean_counts %>%
  pivot_longer(cols = c("mean_mr", "mean_ifr"), names_to = "rate_type", values_to = "rate")

histogram_plot <- mean_counts_long %>%
  ggplot(aes(x = log10(rate), fill = rate_type)) +
  geom_histogram(binwidth = 0.1, position = "identity", alpha = 0.7) +
  facet_wrap(~rate_type, scales = "free_x") +
  scale_x_continuous(labels = scales::label_number()) +
  labs(title = "Mean Mortality Rate and Infection Fatality Rate",
       x = "Log10 Rate",
       y = "Frequency") +
  ggsave("mr_and_ifr.png", width = 8, height = 8)

  
