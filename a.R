library(naivebayes)
library(e1071)
install.packages("iris")
library(iris)
A = data.frame(iris)
str(A)
sf = sample(2, nrow(A), replace = TRUE, prob = c(0.8,0.2))
TRD = A[sf==1,]
TSD = A[sf==2,]
model_nb = naive_bayes(Species~ .,data = TRD)
pred_nb = predict(model_nb,TSD)
table(pred_nb,TSD$Species)

model_e1 = naiveBayes(Species ~ ., data = TRD)
pred_e1 = predict(model_e1,TSD)
table(pred_e1,TSD$Species)
