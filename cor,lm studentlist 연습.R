stu<-read.csv(file.choose())

cor(stu$height,stu$weight)

lm(stu$weight ~ stu$height,data=stu)

plot(lm(stu$weight ~ stu$height,data=stu))

summary(lm(stu$weight ~ stu$height,data=stu))

stu$gender

stu$gender<-ifelse(stu$gender=="M",1,0)
stu$gender

lm(stu$weight ~ stu$height+stu$gender)

plot(lm(stu$weight ~ stu$height+stu$gender))

plot(lm(stu$weight ~ stu$height+stu$gender+stu$height:stu$gender))

summary(lm(stu$weight ~ stu$height+stu$gender+stu$height:stu$gender))

#R-squared = cor ???? Á¦??

datasets::cars

str(cars)

datasets::mtcars

mtcars

head(mtcars)

cor(mtcars)

attach(mtcars)

plot(wt,mpg)

lm(mpg ~ wt)
summary(lm(mpg ~ wt))


#????È¸??
attach(cars)
plot(speed,dist)

cars$speed2<-speed^2

cars

lm(dist ~ speed+speed2,data=cars)

#x?? ???Çµ?, y?? Á¦???Å¸?
#y=0.09996*x^2 + 0.91329*x + 2.47014

summary(lm(dist ~ speed+speed2,data=cars))


#Ridge Regression

y<-mtcars$hp
x<-data.matrix(mtcars[,c("mpg","wt","drat","qsec")])
x

install.packages("glmnet")
