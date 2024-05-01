library(nnet)
set.seed(1234)
data_imp <- heart_no_na
summary(data_imp)

data_imp$HeartDiseaseorAttack[is.na(data_raw$HeartDiseaseorAttack)] <- ifelse(mean(as.numeric(data_raw$HeartDiseaseorAttack) - 1, na.rm = T) > 0.5, 1, 0)
data_imp$Stroke[is.na(data_raw$Stroke)] <- round(mean(as.numeric(data_raw$Stroke) - 1, na.rm = T))
summary(data_imp)
nrounds = 10
for (i in 1:nrounds){
  m_Diabetes <- multinom(Diabetes ~., data = data_imp, subset = !is.na(data_raw$Diabetes))
  pred_Diabetes <- predict(m_Diabetes, data_imp[is.na(data_raw$Diabetes), ])
  data_imp$Diabetes[is.na(data_raw$Diabetes)] <- pred_Diabetes
  
  m_Heart <- glm(HeartDiseaseorAttack ~., data = data_imp, family = "binomial", subset = !is.na(data_raw$HeartDiseaseorAttack))
  pred_Heart <- ifelse(predict(m_Heart, data_imp[is.na(data_raw$HeartDiseaseorAttack), ], type = "response") > 0.5, 1, 0)
  data_imp$HeartDiseaseorAttack[is.na(data_raw$HeartDiseaseorAttack)] <- pred_Heart
  
  m_Stroke <- glm(Stroke ~., data = data_imp, family = "binomial", subset = !is.na(data_raw$Stroke))
  pred_Stroke <- ifelse(predict(m_Stroke, data_imp[is.na(data_raw$Stroke), ], type = "response") > 0.5, 1, 0)
  data_imp$Stroke[is.na(data_raw$Stroke)] <- pred_Stroke
}
summary(data_imp)
