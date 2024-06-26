set.seed(1234)
oob = c()
mvec = c(1:10)
for (m in mvec) { 
  mrand = randomForest(factor(HeartDiseaseorAttack) ~ ., data = data_imp, mtry = m, importance = T)
  confmat = mrand$confusion[,1:2]
  oobER = 1-sum(diag(confmat))/sum(confmat) 
  oob = append(oob, oobER)
}

cbind(mvec, oob)
#9.20% OOB ER with m = 2

#proportion of 1s predicted
optM = randomForest(factor(HeartDiseaseorAttack) ~ ., data = data_imp, mtry = 2, importance = T)
optM$confusion

#var importance
varImpPlot(optM)
