
# ===============================================
# This is a script to Generate report of 
# the classification result and save in a txt 
# ===============================================

# import data
X <- iris[,1:4]
actual.label <- gl(3,50)
#actual.label <- as.numeric(actual.label)

# Classification
source("classifyDistance.R")
pre.label <- classify.distance(X,t(actual.label))

# Generate report 
falseIndex <- which(pre.label!=actual.label)

isTrue = pre.label==actual.label
isTrue = as.matrix(isTrue)
pre.label  = as.matrix(pre.label)
actual.label = as.numeric(actual.label)
actual.label = as.matrix(actual.label)

result = cbind(pre.label,actual.label,isTrue)
colnames(result) = list("pre.label","actual.label","isTrue")
resultTable = table(result[,2],result[,3])
resultTableFalse = result[!isTrue, ]

# print result
sink("resultReport.txt")
cat("# =============== Results ================== #")
cat("\n\n")
cat("# ------------- Results Table ------------------ #")
cat("\n")
print(resultTable)
cat("\n")
cat("# ----------- Indexes of False Samples --------- #")
cat("\n")
print(falseIndex)
cat("\n")
print(resultTableFalse)
cat("\n")
cat("# --------------- More Details ----------------#")
cat("\n")
print(result)
