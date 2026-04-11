# Analysis of Drug Overdose Mortality and Socioeconomic/Health Factors
# 1. Setup and Data Import
# Install and load necessary packages
install.packages(c("ggplot2", "dplyr", "corrplot", "car", "stargazer"))
library(ggplot2)   # For plotting
library(dplyr)     # For data manipulation
library(corrplot)  # For correlation matrix visualization
library(car)       # For Multi-collinearity checks

# Load the cleaned dataset
DrugOverdose <- read.csv("C:/Users/SMY/Downloads/Merged_Drug_Overdose_Socioeconomic_Sleep_Disability.csv")
str(DrugOverdose)
View(DrugOverdose)

# 2.Descriptive Statistics
descr <- summary(DrugOverdose)
# Export the outcome
install.packages("openxlsx")
library(openxlsx)
write.xlsx(descr, "Descriptive_Statistics.xlsx")


# Plotting the trend of Crude Rate (Dependent Variable) over time
ggplot(df, aes(x = Year, y = Crude_Rate)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Trend of Drug Overdose Mortality Rate Over Time (1999-2020)",
       x = "Year",
       y = "Crude Rate (per 100k)") +
  theme_minimal()

#Correlation Matrix
#Selecting numeric variables for correlation analysis
numeric_var <- DrugOverdose[, c("Crude_Rate", "Lack_Insurance", "Food_Insecurity", 
                       "Depression", "Short_Sleep", "Disability")]
cor_matrix <- cor(numeric_var, use = "complete.obs")

# Plotting the correlation heatmap
corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", diag = FALSE,
         title = "Correlation Matrix", mar = c(0,0,1,0))

# 3. Regression Modeling
# Model I: Baseline Model (Main Effects)
# Hypothesis: Mortality is directly affected by Year, Lack of Insurance, Food Insecurity,Depression, Short Sleep, and Disability.
model1 <- lm(Crude_Rate ~ Year + Lack_Insurance + Food_Insecurity + 
               Depression + Short_Sleep + Disability, data = DrugOverdose)
summary(model1)
# Export the outcome
regression_table1 <- summary(model1)$coefficients
write.xlsx(regression_table1, "Regression_Model1.xlsx")

# Model II: Interaction Effects Model
# Hypothesis: The effect of economic factors (Insurance, Food) on mortality changes over time.
model2 <- lm(Crude_Rate ~ Year * Lack_Insurance + Year * Food_Insecurity + 
               Depression + Short_Sleep + Disability, data = DrugOverdose)
summary(model2)
regression_table2 <- summary(model2)$coefficients
# Export the outcome
write.xlsx(regression_table2, "Regression_Model2.xlsx")

# 4. Model Diagnostics and Comparison
# Multi-Collinearity Check (VIF)
# Checking Variance Inflation Factors since health metrics might be correlated
vif(model1)

# Residual Analysis (Checking OLS Assumptions)
par(mfrow = c(2, 2)) # Set up 2x2 plotting grid
plot(model1)
par(mfrow = c(1, 1)) # Reset plotting grid

# Model Comparison (ANOVA)
# Test if Model 2 provides a significantly better fit than Model 1
anova_result <- anova(model1, model2)
print(anova_result)

# 5. Visualization of Key Results
# Visualizing the relationship between Depression and Mortality (controlling for Year trend)
ggplot(DrugOverdose, aes(x = Depression, y = Crude_Rate)) +
  geom_point(aes(color = Year), alpha = 0.6) +
  geom_smooth(method = "lm", color = "darkgreen") +
  labs(title = "Relationship between Depression Prevalence and Overdose Mortality",
       x = "Depression (%)",
       y = "Mortality Rate",
       color = "Year") +
  theme_minimal()