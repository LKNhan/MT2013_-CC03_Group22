library(tidyverse) 
dataset_3D_printer <- read_csv("data.csv")

temp_data <- dataset_3D_printer %>%
  mutate(infill_pattern = as.numeric(infill_pattern != "grid"),
         material = as.numeric(material != "abs"))

gathered_data <- gather(temp_data)
gathered_data <- gathered_data %>%
  mutate(key = factor(key, levels = unique(gathered_data$key)))

ggplot(data = gathered_data, aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ key, scales = "free", nrow = 4) +
  labs(x = NULL, y = NULL)
