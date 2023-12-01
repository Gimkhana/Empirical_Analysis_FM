#importing the libraries from the script

library(tidyverse)
library(readxl)

# Read the data from the Excel xlsx file to retrieve the NASDAQ sectors prices

(workshop1 <- read_xlsx("data.xlsx", sheet = "workshop1", skip = 4))

#Construct the return variable

fin_return <- workshop1 %>% select(-"TRADE DATE")

# Focus on the first four columns (sectors) of the dataset (pipeline)

(fin_return_4first_Supersectors <- fin_return %>% select(1:4))

#Explore the data using the package data explorer

library(DataExplorer)
plot_intro(fin_return_4first_Supersectors)

#Statistical data analysis of each column (sector)

summary(fin_return_4first_Supersectors)

#Advanced analysis of the returns for each sector

plot_density(fin_return_4first_Supersectors)

plot_qq(fin_return_4first_Supersectors)


#Correlation plot
corrplot(cor(fin_return), type='upper', tl.col = 'black', tl.cex = 0.1)

#Modelling part

library(scales)

#shrink the dataset into two subsamples

end_date <- nrow(fin_return)
fin_return_learning <- fin_return %>% slice(1:74)
fin_return_backtest <- fin_return %>% slice(75:end_date)


----------------------------------------------

#GMW modelling 
  
#Construction of the covariance matrix for each shrinked dataset
#   
# cov_returns_learning <- cov(fin_return_learning)
# cov_returns_backtest <- cov(fin_return_backtest)
# 
# # Compute the inverse of the covariance matrix
# 
# inverse_cov_returns_learning <- (solve(cov(fin_return_learning)))
# inverse_cov_returns_backtest <- (solve(cov(fin_return_backtest)))
# 
# # Create a vector of 1s with length n
# n <- 19  # Replace with the desired length
# e <- rep(1, n)
# 
# #transpose of e

#Proposed solution

n <- ncol(fin_return_learning)
T <- nrow(fin_return_learning)
e <- rep(1, n)
perio <- 12

#Computation of sigma 

Sigma <- cov(fin_return_learning) * (T - 1) / (T - n - 2) * perio 

#Computation of C

C <- t(e) %*% solve(Sigma) %*% e

#anticipated volatility

sigmag <- sqrt(1 / C) 

#Computation of optimal weights "w"

omega <- 1 / as.numeric(C) * solve(Sigma) %*% e

#Plot GMV unbiased portfolio

barplot(as.numeric(omega), col = 'black')

  
  
  
  


