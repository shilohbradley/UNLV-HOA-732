normalize <- function(x) 
{
x <- na.omit(x)
return ((x - min(x)) / (max(x) - min(x)))
}