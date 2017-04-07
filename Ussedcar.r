# Uploading data usedcar
usedcars<-read.csv("usedcars.csv",stringsAsFactors = FALSE)

View(usedcars)
str(usedcars)
table(usedcars$transmission)
usedcars$transmission <- factor(usedcars$transmission, levels = c("AUTO", "MANUAL"),
                         labels = c("AUTO", "MANUAL"))
## table or proportions with more informative labels
round(prop.table(table(usedcars$transmission))*100,digits=2)
# summary of year,price and milage 
summary(usedcars[c("price","year","mileage")])
#create normalization function
normalize <- function(x) 
{  
  return ((x - min(x)) / (max(x) - min(x)))
}
#checking function working status
normalize(c(1, 2, 3, 4, 5))
usedcars.normalized=as.data.frame(lapply(usedcars[c(1,3,4)],normalize))
summary(usedcars.normalized[c("price","year","mileage")])
# Creating training data set
usedcars_training=usedcars.normalized[1:120,]
#Creating Test data set
usedcars_test=usedcars.normalized[121:150,]
usedcars_training_labels=usedcars[1:120,6]
usedcars_test_labels=usedcars[121:150,6]
#Visualisation of data using levels
plot(usedcars$price,usedcars$mileage,main="Scatter plot",xlab="Price of the car",ylab="Mileage of the car")
pairs(~price+year+mileage,data=usedcars,main="scatter plot of 3 variable")
library(car)
scatterplot(price~mileage|transmission,data=usedcars,main="Scatterplot")
scatterplotMatrix(~price+mileage+year|transmission,data=usedcars)
# Traning a model on a Data
library(class)
usedcars_test_pre<-knn(train = usedcars_training,test = usedcars_test,cl=usedcars_training_labels,k=11)

CrossTable(x=usedcars_test_labels,y=usedcars_test_pre,prop.chisq = FALSE)
length(usedcars_training)
length(usedcars_training_labels)
dim(usedcars_training)
length(usedcars_training_labels)
dim(usedcars_training_labels)
usedcars_test_pre<-knn(train = usedcars_training,test = usedcars_test,cl=usedcars_training_labels,k=13)

CrossTable(x=usedcars_test_labels,y=usedcars_test_pre,prop.chisq = FALSE)
usedcars_test_pre<-knn(train = usedcars_training,test = usedcars_test,cl=usedcars_training_labels,k=23)

usedcars_test_pre
CrossTable(x=usedcars_test_labels,y=usedcars_test_pre,prop.chisq = FALSE)
usedcars_test_pre<-knn(train = usedcars_training,test = usedcars_test,cl=usedcars_training_labels,k=1)
usedcars_test_pre
CrossTable(x=usedcars_test_labels,y=usedcars_test_pre,prop.chisq = FALSE)
