###### R Code for Assignment 1 ###### 
###### Lye En Lih (U2121387B) #######

########## Part (a) ##########

# Data Input for Assignment 1
x <- c(40, 50, 60, 70, 80, 90, 40, 60, 80, 50)  # Concentration Values (in mg/mL)
y <- c(69, 175, 272, 335, 490, 415, 72, 265, 492, 180)  # Colorimeter Reading Values 

# Fit a Simple Linear Model using lm()
slr <- lm(y ~ x)
slr

#### From model, β0 = -252.297101, β1 = 8.528986 #### 

# Plot ScatterPlot
plot(x, y, xlab = "Concentration (mg/mL)", ylab = "Colorimeter Reading", pch = 1, 
     col = "black")

# Plot Best Fit Regression Line
abline(slr, col = "red")

########## Part (b) ##########

r_squared <- summary(slr)$r.squared     
f_stat <- summary(slr)$fstatistic[1]
p_value <- pf(f_stat, df1 = summary(slr)$fstatistic[2], df2 =summary(slr)$fstatistic[3],
              lower.tail = FALSE)

# Displaying Statistics Value
r_squared
f_stat
p_value

#### From Model, (Multiple R-squared) R^2 = 0.9156, F-Statistic: 86.83 on 1 and 8 DF

########## Appendix: Extra Mathematical Derivations ########## 

###### For Part (a) ######

# Calculate Beta Coefficients for Simple Linear Regression Model
x_bar <- mean(x)
y_bar <- mean(y)

Sxy <- sum((x - x_bar)*(y - y_bar))
Sxx <- sum((x - x_bar)^2)
Syy <- sum((y - y_bar)^2)

beta_1 <- Sxy / Sxx
beta_0 <- y_bar - (beta_1 * x_bar)

cat("Intercept (β0):", beta_0)
cat("Slope (β1):", beta_1)

# Derive Predicted Y-values using SLR 
y_predicted <- beta_0 + beta_1 * x

cat("Actual Colorimeter Values (Y):", paste(y, collapse = ", "))
cat("Predicted Colormeter Values (Y_hat):", paste(round(y_predicted,3), collapse = ", "))

###### For Part (b) ######

# R^2 Statistic Value
# Formula: R^2 = SS_Regression (SSR) / SS_Total (Syy)
# SSR = (beta_1)^2 * Sxx

SSR <- (beta_1)^2 * Sxx
R_squared <- SSR / Syy

cat("R_squared Value:", round(R_squared,3))


# F-Statistic Value
# Formula: F = SSR / s^2 
# s^2 = SS_Residual (SSE) / n-2 , where n = 10 in this case
# SSE = Syy - SSR

SSE = Syy - SSR
df = length(x) - 2
s_squared = SSE / df
F_stat <- SSR / s_squared
