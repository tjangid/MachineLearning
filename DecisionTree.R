library(ISLR)
library(tree)
library(naivebayes)
A = data.frame(iris)
sf = sample(2, nrow(A), replace = TRUE, prob = c(0.80,0.20))
TRD = A[sf==1,]
TSD = A[sf==2,]
#Decision tree model
m1 = tree(Species~., data = TRD)
plot(m1)
text(m1)
pred = predict(m1,TSD)
#to draw confusion matrix
Q = ifelse(pred[,1] > 0.5,"setosa",ifelse(pred[,2] > 0.5,"versicolor","virginica"))
table(Q,TSD$Species)
percentage = (sum(diag(table(Q,TSD$Species)))/nrow(TSD))*100
Miscalss = 100 - percentage
summary(m1)


#naive bayes
model_nb = naive_bayes(Species~ .,data = TRD)
pred1 = predict(model_nb,TSD)
table(pred1,TSD$Species)
summary(model_nb)

