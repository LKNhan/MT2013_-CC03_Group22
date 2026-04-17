
#For chapter 5
library(tidyverse) 
library(MASS)
dataset_3D_printer <- read_csv("data.csv") #must set working directory

temp_data <- dataset_3D_printer %>%
  mutate(infill_pattern = as.numeric(infill_pattern != "grid"),
         material = as.numeric(material != "abs"))

#MLR for all independent variable
MLR1 = lm(formula = roughness ~ layer_height + wall_thickness + infill_density +
            infill_pattern + nozzle_temperature + bed_temperature + print_speed + 
            material + fan_speed, data = temp_data)


# Plot the residuals
model1_residuals = MLR1$residuals
hist(model1_residuals)
qqnorm(model1_residuals)
# Plot the Q-Q line
qqline(model1_residuals)

#From correlation matrix, fan_speed ~ bedtemperature ~= 1 -> remove fan_speed
MLR2 = lm(formula = roughness ~ layer_height + wall_thickness + infill_density +
            infill_pattern + nozzle_temperature + bed_temperature + print_speed + 
            material, data = temp_data)


# Plot the residuals
model2_residuals = MLR2$residuals
hist(model2_residuals)
qqnorm(model2_residuals)
# Plot the Q-Q line
qqline(model2_residuals)

summary(MLR1)
summary(MLR2)
anova(MLR1, MLR2) #compare model 1 to model 2


#For chapter 6
#remove variables with P(r) > 0.05: wall_thickness, infill_density, infill_pattern 
MLR3 = lm(formula = roughness ~ layer_height + 
            nozzle_temperature + bed_temperature + print_speed + 
            material, data = temp_data)


# Plot the residuals
model3_residuals = MLR3$residuals
hist(model3_residuals)
qqnorm(model3_residuals)
# Plot the Q-Q line
qqline(model3_residuals)


summary(MLR3)  #R-adjusted is higher than MLR2 (good)
anova(MLR2, MLR3) #compare model 2 to model 3 
#-> reduce complexity, improve the model

coef(MLR3) #B0, B1, B2....
best_model <- stepAIC(MLR1, direction = "both", trace = TRUE) 
#already leave -+ indicating adding in or leaving out. 
#AIC agree with p-values method

