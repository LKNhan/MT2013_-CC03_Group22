# 1. Load Libraries
library(tidyverse)
library(reshape2)

# 2. Load and Prepare Data
# Replace "data.csv" with your actual file path if needed
mydata <- read_csv("data.csv") 

# Convert categorical variables to numeric (0/1) for correlation
# ABS/Grid = 0, PLA/Honeycomb = 1
temp_data <- mydata %>%
  mutate(
    infill_pattern = as.numeric(infill_pattern != "grid"),
    material = as.numeric(material != "abs")
  )

# 3. Correlation Matrix Logic
cormat <- round(cor(temp_data), 2)

# Helper function to get upper triangle (removes the mirror image)
get_upper_tri <- function(cormat) {
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

# Helper function to reorder by similarity (clustering)
reorder_cormat <- function(cormat) {
  dd <- as.dist((1 - cormat) / 2)
  hc <- hclust(dd)
  cormat[hc$order, hc$order]
}

# Apply reordering and extract upper triangle
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)

# 4. Visualize with ggplot2
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "azure", 
    midpoint = 0, limit = c(-1, 1), space = "lab", 
    name = "Pearson\nCorrelation"
  ) +
  geom_text(aes(label = value), color = "black", size = 3.5) +
  coord_fixed() +
  theme_minimal() +
  labs(title = "3D Printer Parameters Correlation Matrix", x = NULL, y = NULL) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal"
  ) +
  guides(fill = guide_colorbar(
    barwidth = 7, barheight = 1,
    title.position = "top", title.hjust = 0.5
  ))

