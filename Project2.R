library(readxl)
library(ggplot2)
library(lmtest)
library(car)
WA_data <- read_excel("Downloads/STAT530 HWS/Project2/WA_data_STAT530Project2.xlsx")

# Read data
head(WA_data)
colnames(WA_data)
summary(WA_data)

# Line graph for Total Homeless Count Year-on-Year 
ggplot(data=WA_data, aes(x=Year, y=Total_Homeless)) +
  geom_line(size=1, colour = "blue") +
  geom_point(size=1, color = "blue") +
  labs(title = "Total Homeless Count Year-On-Year",
       x = "Year",
       y = "Total Homeless Count") + 
  theme_light() +
  scale_x_continuous(breaks = seq(2000, 2023, by = 1)) +
  scale_y_continuous(breaks = seq(11000, 26000, by = 1000))

# Line graph for Gini Coefficient Year-on-Year
ggplot(data=WA_data, aes(x=Year, y=ACSgini)) +
  geom_line(size=1, colour = "blue") +
  geom_point(size=1, color = "blue") +
  labs(title = "Gini Coefficient Year-On-Year",
       x = "Year",
       y = "Gini Coefficient") + 
  theme_light() +
  scale_x_continuous(breaks = seq(2000, 2023, by = 1)) +
  scale_y_continuous(breaks = seq(0.44, 0.49, by = 0.01))

# Line graph for Poverty Percentage Year-on-Year
ggplot(data=WA_data, aes(x=Year, y=ACSpovertyPCT)) +
  geom_line(size=1, colour = "blue") +
  geom_point(size=1, color = "blue") +
  labs(title = "Poverty Percentage Year-On-Year",
       x = "Year",
       y = "Poverty Percentage") + 
  theme_light() +
  scale_x_continuous(breaks = seq(2000, 2023, by = 1))

# Line graph for Unemployment Rate Year-on-Year
ggplot(data=WA_data, aes(x=Year, y=ACSunemploymentrate)) +
  geom_line(size=1, colour = "blue") +
  geom_point(size=1, color = "blue") +
  labs(title = "Unemployment Rate Year-On-Year",
       x = "Year",
       y = "Unemployment Rate") + 
  theme_light() +
  scale_x_continuous(breaks = seq(2000, 2023, by = 1))

# Line graph for Median Gross Rent Year-on-Year
ggplot(data=WA_data, aes(x=Year, y=ACSmediangrossrent)) +
  geom_line(size=1, colour = "blue") +
  geom_point(size=1, color = "blue") +
  labs(title = "Median Gross Rent Year-On-Year",
       x = "Year",
       y = "Median Gross Rent") + 
  theme_light() +
  scale_x_continuous(breaks = seq(2000, 2023, by = 1))

# Line graph for Median Real Estate Taxes Year-on-Year
ggplot(data=WA_data, aes(x=Year, y=ACSMedianRealETaxes)) +
  geom_line(size=1, colour = "blue") +
  geom_point(size=1, color = "blue") +
  labs(title = "Median Real Estate Taxes Year-On-Year",
       x = "Year",
       y = "Median Real Estate Taxes") + 
  theme_light() +
  scale_x_continuous(breaks = seq(2000, 2023, by = 1))

# Assigning the values to the variables
y = WA_data$Total_Homeless
x1 = WA_data$ACSgini
x2 = WA_data$ACSpovertyPCT
x3 = WA_data$ACSunemploymentrate
x4 = WA_data$ACSmediangrossrent
x5 = WA_data$ACSMedianRealETaxes

# Fitting multiple linear regression
fit=lm(y~x1+x2+x3+x4+x5, data=WA_data)

# Generating summary results from the multiple linear regression
summary(fit)

# Finding SS2
anova(fit)

# Plot of residual and fitted values
par(mfrow=c(1,2))
# Plot of residual vs predicted
plot(fit$fitted, fit$residuals)
abline(h=0)

# Normal probability plot of Residuals
qqnorm(fit$residuals)
qqline(fit$residuals)

# Test for normality
library(stats)
shapiro.test(fit$residuals)

# Breusch pagan test
bptest(fit)

# Durbin-watson test
dwtest(fit)

# Calculating the Correlation Matrix
variables <- WA_data[c("ACSgini", "ACSpovertyPCT", 
                       "ACSunemploymentrate", "ACSmediangrossrent", 
                       "ACSMedianRealETaxes")]
cor_matrix <- cor(variables)
print(cor_matrix)
