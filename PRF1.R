PRF1 <- function(CM)
{
#Precision and Recall for both categories, training set
#CM is confusion matrix

#Precision = TruePositives / (TruePositives + FalsePositives)
#Recall    = TruePositives / (TruePositives + FalseNegatives)
#-------------------------------------------
#         Predicted
#---------0--------1----    ---------0--------1----
#0    CM[1,1]  CM[1,2]              TN       FP
#1    CM[2,1]  CM[2,2]              FN       TP            

#Calculate Precision and Recall from Confusion Matrix 
#Category 1
#Precision1 <- CM[2,2]/(CM[1,2]+CM[2,2]) # diag/column sum
#Recall1 <- CM[2,2]/(CM[2,1]+CM[2,2]) # diag/row sum
#------------------------------------------------------------------
#Category 0
#Precision0 <- CM[1,1]/(CM[1,1]+CM[2,1]) # diag/column sum
#Recall0 <- CM[1,1]/(CM[1,1]+CM[1,2]) # diag/row sum
#------------------------------------------------------------------
#------------------------------------------------------------------
#Compute Precision, Recall, F1 for Category 1, training set
Precision1 <- CM[2,2]/(CM[1,2]+CM[2,2])
Recall1 <- CM[2,2]/(CM[2,1]+CM[2,2])
F1.1 <- 2/((1/Recall1)+(1/Precision1))

PRF1_1 <-(c(Precision1, Recall1, F1.1))

# Compute Precision, Recall, F1 for Category 0, training set
Recall0 <- CM[1,1]/(CM[1,1]+CM[1,2])
Precision0 <- CM[1,1]/(CM[1,1]+CM[2,1])
F1.0 <- 2/((1/Recall0)+(1/Precision0))

PRF1_0 <- c(Precision0, Recall0, F1.0)
temp <- c(PRF1_0,PRF1_0)
names(temp) <- c("Precision_1","Recall_1","F1_1","Precision_0","Recall_0","F1_0")
return(round(temp,2))
}

