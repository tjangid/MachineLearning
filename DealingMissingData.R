library(psych)
A = data.frame(iris)
A[1,1]=NA
A[2,2]=NA
A[3,3]=NA
head(A)
pairs.panels(A)
#--------------
tsd = A[is.na(A$Petal.Length) == TRUE,]
trd = A[is.na(A$Petal.Length) == FALSE,]
m1 = lm(Petal.Length~Petal.Width, data = trd)
pred = predict(m1,tsd)
tsd[,is.na(tsd$Petal.Length) ==TRUE]=pred[1]
FinalDataSet = sqldf::sqldf("Select * from trd UNION ALL Select * from tsd")

#--------------------
tsd = FinalDataSet[is.na(FinalDataSet$Sepal.Length)==TRUE,]
trd = FinalDataSet[is.na(FinalDataSet$Sepal.Length)==FALSE,]
m2 = lm(Sepal.Length ~ Petal.Width+Petal.Length,data = trd)
pred = predict(m2,tsd)
tsd[,is.na(tsd$Sepal.Length) ==TRUE]=pred[1]
FinalDataSet = sqldf::sqldf("SELECT * FROM trd union all select * from tsd")

#----------------------Sepal.Width
tsd = FinalDataSet[is.na(FinalDataSet$Sepal.Width)==TRUE,]
trd = FinalDataSet[is.na(FinalDataSet$Sepal.Width)==FALSE,]
m3 = lm(Sepal.Width~Sepal.Length+Petal.Length, data = trd)
pred = predict(m3, tsd)
tsd[,is.na(tsd$Sepal.Length) ==TRUE]=pred[1]
FinalDataSet = sqldf::sqldf("Select * from trd UNION ALL Select * from tsd")

NewA = FinalDataSet
