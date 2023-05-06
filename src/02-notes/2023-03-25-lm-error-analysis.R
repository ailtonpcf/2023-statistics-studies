library(tidyverse)
library("carData")

data <- Soils
 # Model M0
m0 <- lm(Conduc ~ pH + Ca + K + P ,data = data)
# Condutividade = b0 + b1*ph + b2*Ca + b3*P + b4*K

summary(m0)
# residuals in the sda scale

# Confidence interval
confint(m0)

m1 <- lm(Conduc ~ -1 + pH + Ca + K + P + Group,data = data)
# Condutividade = b0 + b1*ph + b2*Ca + b3*P + b4*K + b5*group

summary(m1)
# Group1 is together with intercept. to get group 1 values, remove intercept

# Compare if is effective add or remove variables when comparing two nested models
anova(m0, m1)

# Model withou P based on M0
m2 <- lm(Conduc ~ -1 + pH + Ca + K + Group,data = data)

summary(m2)

# ANalyze residuals

plot(m1)
# Residuals behaving with expected values close to 0

shapiro.test(residuals(m1))

# p-value bigger than 0.05, we don't reject H0, then the residuals follow a normal distribution.

# According to residuals versus leverage plot, observation 24 is influential

data <- data[-24,]

