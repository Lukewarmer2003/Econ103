# Econ103
---
title: "Project 1"
author: "Wentian Chen"
date: "2024-10-25"
output:
  pdf_document:
    latex_engine: xelatex
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r}
getwd()
setwd("C:/Users/admin/Desktop/Econ 103")
```


```{r}
library(readxl)
data <- read.csv("C:/Users/admin/Desktop/train.csv")
head(data)
#Response variable: SalePrice
#Predictors: LotFrontage, WoodDeckSF, GarageArea, GrLivArea
```

```{r}
#Summary of all variables
library(psych)
describe(data[c("SalePrice", "LotFrontage", "WoodDeckSF", "GarageArea", "GrLivArea")])
```

```{r}
#Histogram and Fitted Distribution
#Histogram of SalePrice, including density line
library(ggplot2)
ggplot(data, aes(x = SalePrice)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) + geom_rug(sides = "b", color = "darkblue", alpha = 0.6)+
  labs(title = "Histogram of SalePrice with Density Curve",
       x = "SalePrice", y = "Density")
```

```{r}
#Histogram of LotFrontage, including density line
ggplot(data, aes(x = LotFrontage)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) + geom_rug(sides = "b", color = "darkblue", alpha = 0.6)+
  labs(title = "Histogram of LotFrontage with Density Curve",
       x = "LotFrontage", y = "Density")
```


```{r}
#Histogram of WoodDeckSF, including density line
ggplot(data, aes(x = WoodDeckSF)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) + geom_rug(sides = "b", color = "darkblue", alpha = 0.6)+
  labs(title = "Histogram of WoodDeckSF with Density Curve",
       x = "WoodDeckSF", y = "Density")
```

```{r}
#Histogram of GarageArea, including density line
ggplot(data, aes(x = GarageArea)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) + geom_rug(sides = "b", color = "darkblue", alpha = 0.6)+
  labs(title = "Histogram of GarageArea with Density Curve",
       x = "GarageArea", y = "Density")
```

```{r}
#Histogram of GrLivArea, including density line
ggplot(data, aes(x = GrLivArea)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) + geom_rug(sides = "b", color = "darkblue", alpha = 0.6)+
  labs(title = "Histogram of GrLivArea with Density Curve",
       x = "GrLivArea", y = "Density")
```

```{r}
#qqplots for all variables
# List of variables to create Q-Q plots for
library(car)
variables <- c("SalePrice", "LotFrontage", "WoodDeckSF", "GarageArea", "GrLivArea")

# Loop to generate Q-Q plots for each variable
for (var in variables) {
  # Generate Q-Q plot with car::qqPlot
  qqPlot(data[[var]], 
         main = paste("Q-Q Plot of", var), 
         ylab = var,
         id = list(n = 3))  # Label the top 3 extreme points
}
```

```{r}
qqPlot(lm(SalePrice ~ LotFrontage+WoodDeckSF+GarageArea+GrLivArea, data=data),
envelope=.99)
```

```{r}
#boxplots for all variables
for (var in variables) {
  boxplot(data[[var]], main = paste("Boxplot of", var),
          ylab = var, col = "lightgreen")
}
```

```{r}
#scatterplots for all variables
scatterplot(SalePrice ~ LotFrontage, data = data, lwd = 3, id = list(n = 3))
scatterplot(SalePrice ~ WoodDeckSF, data = data, lwd = 3, id = list(n = 3))
scatterplot(SalePrice ~ GarageArea, data = data, lwd = 3, id = list(n = 3))
scatterplot(SalePrice ~ GrLivArea, data = data, lwd = 3, id = list(n = 3))
```

```{r}
# Scatterplot matrix with specified variables and options
scatterplotMatrix(~ SalePrice + LotFrontage + WoodDeckSF + GarageArea + GrLivArea, 
                  data = data, 
                  smooth = FALSE, 
                  ellipse = list(levels = 0.5))
```

```{r}
#linearity test
for (var in variables) {
  formula <- as.formula(paste("~", var))
  symbox(formula, data = data, main = paste("Boxplots of Power Transformations for", var))
}
```
```{r}
# Apply powerTransform to multiple variables
#variable wooddecksf and GarageArea contains negative values so we need to transform it by adding 1 to each value for later linearity test
WoodDeckSF1 = data$WoodDeckSF + 1
GarageArea1 = data$GarageArea + 1
# Apply powerTransform to multiple variables
a3 <- powerTransform(cbind(SalePrice, LotFrontage, WoodDeckSF1, GarageArea1, GrLivArea) ~ 1, data = data)

# View the summary of the transformations
summary(a3)
```
```{r}
# Transform selected variables using Box-Cox with coefficients from a3
transformeddata <- as.data.frame(bcPower(
  with(data, cbind(SalePrice, LotFrontage, WoodDeckSF1, GarageArea1, GrLivArea)),
  coef(a3, round = TRUE)
))

# Rename columns in transformeddata for clarity (optional)
colnames(transformeddata) <- c("SalePrice_transformed", "LotFrontage_transformed", 
                               "WoodDeckSF_transformed", "GarageArea_transformed", 
                               "GrLivArea_transformed")

# Plot scatterplot matrix of transformed variables
scatterplotMatrix(~ SalePrice_transformed + LotFrontage_transformed + WoodDeckSF_transformed + 
                  GarageArea_transformed + GrLivArea_transformed, 
                  data = transformeddata, 
                  smooth = FALSE, ellipse = list(levels = 0.5),
                  main = "Scatterplot Matrix of Transformed Variables")
```

```{r}
#fit data
library(broom)
# Fit linear models
model1 <- lm(SalePrice_transformed ~ LotFrontage_transformed, data = transformeddata)
model2 <- lm(SalePrice_transformed ~ WoodDeckSF_transformed, data = transformeddata)
model3 <- lm(SalePrice_transformed ~ GarageArea_transformed, data = transformeddata)
model4 <- lm(SalePrice_transformed ~ GrLivArea_transformed, data = transformeddata)

# Summarize each model
summary_model1 <- summary(model1)
summary_model2 <- summary(model2)
summary_model3 <- summary(model3)
summary_model4 <- summary(model4)

# Store summaries in a list for easy viewing
model_summaries <- list(
  model1 = summary_model1,
  model2 = summary_model2,
  model3 = summary_model3,
  model4 = summary_model4
)

# Display the summaries
model_summaries

#R-squared and Adjusted R-squared:
#Model 4 has the highest R-squared (0.5333), indicating that approximately 53.33% of the variance in SalePrice_transformed can be explained by GrLivArea_transformed. This suggests a strong relationship between these variables.
#Model 3 is next with an R-squared of 0.4209, indicating that GarageArea_transformed explains about 42.09% of the variance.
#Statistical Significance:
#All models show highly significant p-values (< 2e-16), indicating that the predictors have a statistically significant effect on the transformed sale price.
#Economic Significance:
#The coefficient for GrLivArea_transformed (0.8745) suggests that for each unit increase in GrLivArea_transformed, the sale price increases by approximately 0.8745 units, which is substantial.
#The coefficient for LotFrontage_transformed (0.0551) is smaller, indicating a weaker economic impact compared to GrLivArea.
```
```{r}
#model4 is the best model because of low pvalues and high R^2
# Calculate confidence intervals for Model 4
confint_model4 <- confint(model4, level = 0.95)
print(confint_model4)
#confidence intervals are within a small range
```

```{r}
#perfoming boostraping for parameters with 1000 samples
library(boot)
set.seed(3435)
betahat.boot = Boot(model4, R=1000)
usualEsts = summary(model4)$coef[, 1:2]
summary(betahat.boot)
confint(betahat.boot)
hist(betahat.boot)
#means of parameters after boostrapping are closed to the original values
```

```{r}
#boostrapping for R^2
# Define the bootstrapping function
boot_fn <- function(data, indices) {
  # Resample the data using the indices
  d <- data[indices, ]
  
  # Fit the linear model
  model <- lm(SalePrice_transformed ~ GrLivArea_transformed, data = d)
  
  # Extract coefficients
  coefs <- coef(model)
  
  # Extract R-squared
  r_squared <- summary(model)$r.squared
  
  # Return both coefficients and R-squared
  return(c(coefs, r_squared))
}
# Set parameters for bootstrapping
set.seed(3435)
R <- 1000  # Number of bootstrap samples
dd <- transformeddata  # Your dataset

# Perform bootstrapping
betahat.boot <- boot(data = dd, statistic = boot_fn, R = R)

# Check the structure of the bootstrapped results
str(betahat.boot)
# Check the number of columns in betahat.boot$t
ncol(betahat.boot$t)  # Ensure it returns 3

# Extract coefficients and R-squared
boot_coefs <- betahat.boot$t[, 1:2]  # Coefficients
boot_r2 <- betahat.boot$t[, 3]        # R-squared values
# Plotting the distribution of R-squared
ggplot(data.frame(R2 = boot_r2), aes(x = R2)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Bootstrapped Distribution of R-squared",
       x = "R-squared",
       y = "Density") +
  theme_minimal()
#the mean of boostrapping adjusted R^2 is close to the original value
```

```{r}
#plot residuals of model4 and plot its qqplot
plot(transformeddata$GrLivArea_transformed,model4$residuals,pch=20, ylab="Residuals", xlab="GrLivArea_transformed")
abline(h=0, lwd=2, col="red")
residuals <- resid(model4)
qqPlot(residuals, 
         main = "Q-Q Plot of residuals", 
         ylab = "Residuals",
         id = list(n = 3))  # Label the top 3 extreme points
hist(residuals,breaks ="FD",col="skyblue2", freq = FALSE, ylab = "Density",
main = "Histogram of the Residuals")
lines(density(residuals),lwd = 2, col ="red")
```
