# Installing Library to load the data
library(readr)
df <- read_csv("Women_Prestige_Data.csv")
head(df)
#########################################
# Checking data type:                   #
#########################################

str(df)

#########################################
# Data distribution:                    #
#########################################
library(purrr)
library(tidyr)
library(ggplot2)
par(mfrow= c(3, 3))
df %>%
  keep(is.numeric) %>% subset(select=-c(census))%>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(aes(y = ..count..), bins=12, fill = "grey") +
  stat_function(fun = dnorm)

############################################
## Including Plots                         #
############################################

# Graphs to check data distribution:
# Create a function that helps create graphs:
histDenNorm <- function (x, main = "") {
  hist(x, prob = TRUE, main = main) # Histogram
  lines(density(x), col = "blue", lwd = 2) # Density 
  x2 <- seq(min(x), max(x), length = 40)
  f <- dnorm(x2, mean(x), sd(x))
  lines(x2, f, col = "red", lwd = 2) # Normal
  legend("topright", c("Density", "Normal"), box.lty = 3,
         lty = 3, col = c("blue", "red"), lwd = c(1, 2, 2))
}

## Create histograms:

x <- df$education
y <- df$income
z <- df$women
v <- df$prestige
par(mfrow= c(2,2))
histDenNorm(x, main = "education")
histDenNorm(y, main = "income")
histDenNorm(z, main = "women")
histDenNorm(v, main = "prestige")

########################################
# Correlation heatmap:                 #
########################################
library(psych)

df1 <- subset(df, select=-c(census, occupation_name, type))
corPlot(df1, cex = 1.2, numbers=TRUE,stars=TRUE)

########################################
# Linear regression: Models comparison #
########################################

full.model <- lm(prestige ~ education + income + women, data=df)
reduced.model <- step(full.model, direction = "backward")
summary(reduced.model)

########################################
# Best model                           #
########################################

fit <- lm(prestige ~ education + income + women, data=df)
summary(fit)

########################################
# diagnostic plots                     #
########################################

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)

