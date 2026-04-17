library(tidyverse) 
library(stargazer)

dataset_3D_printer <- read.csv("data.csv")
stargazer(dataset_3D_printer, type = "text", digits = 2, 
          header = FALSE, title = "Discriptive statistic of the dataset",
          summary.stat=c("n","min", "max", "mean", "median", "sd", "p25", "p75"))

