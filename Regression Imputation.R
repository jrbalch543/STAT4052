library(nnet)
set.seed(1234)
data_imp <- heart
summary(data_imp)

data_imp$HeartDiseaseorAttack[is.na(heart$HeartDiseaseorAttack)] <- ifelse(mean(as.numeric(heart$HeartDiseaseorAttack) - 1, na.rm = T) > 0.5, 1, 0)
data_imp$Stroke[is.na(heart$Stroke)] <- round(mean(as.numeric(heart$Stroke) - 1, na.rm = T))
summary(data_imp)
nrounds = 10
for (i in 1:nrounds){
  m_Diabetes <- multinom(Diabetes ~., data = data_imp, subset = !is.na(heart$Diabetes))
  pred_Diabetes <- predict(m_Diabetes, data_imp[is.na(heart$Diabetes), ])
  data_imp$Diabetes[is.na(heart$Diabetes)] <- pred_Diabetes
  
  m_Heart <- glm(HeartDiseaseorAttack ~., data = data_imp, family = "binomial", subset = !is.na(heart$HeartDiseaseorAttack))
  pred_Heart <- ifelse(predict(m_Heart, data_imp[is.na(heart$HeartDiseaseorAttack), ], type = "response") > 0.5, 1, 0)
  data_imp$HeartDiseaseorAttack[is.na(heart$HeartDiseaseorAttack)] <- pred_Heart
  
  m_Stroke <- glm(Stroke ~., data = data_imp, family = "binomial", subset = !is.na(heart$Stroke))
  pred_Stroke <- ifelse(predict(m_Stroke, data_imp[is.na(heart$Stroke), ], type = "response") > 0.5, 1, 0)
  data_imp$Stroke[is.na(heart$Stroke)] <- pred_Stroke
}
summary(data_imp)
