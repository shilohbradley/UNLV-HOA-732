plot.y_yhat <- function(y,yhat.lm,yhat.F,model)
{
df1 <- cbind.data.frame(y,yhat.lm)
df2 <- cbind.data.frame(y,yhat.F)
colnames(df1) <- c("Y","Yhat")
colnames(df2) <- c("Y","Yhat")
df <- rbind.data.frame(df1,df2)
df$model <- c("Linear",model)
pB <- ggplot(data=df,aes(x=Y,y=Yhat, color=model))+geom_point()
pB
}
