RMSE <- function(Y,Yhat)
{
n <- length(Yhat)
MSE <- sum((Y- Yhat)**2)/n  
RMSE <- sqrt(MSE)
r <- cor(Y,Yhat)
R_square <- r**2
temp <- c(R_square,RMSE)
names(temp) <- c("R_Square","RMSE")
return(temp)  
} 