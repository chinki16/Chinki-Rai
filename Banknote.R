#Read Data
Banknote=read.table("C:/Users/chink/Desktop/data_banknote_authentication.txt",sep=",",header = F)
#Applying headers
colnames(Banknote)[1]="variance_Wavelet"
colnames(Banknote)[2]="skewness_Wavelet"
colnames(Banknote)[3]="curtosis_Wavelet"
colnames(Banknote)[4]="entropy"
colnames(Banknote)[5]="class"

# The dataset is too huge for knn analysis, we have chosen random 20%
divide=sample(nrow(Banknote),floor(nrow(Banknote)*0.2))
Banknote=Banknote[divide,]
Banknote
#Examine structure of data frame
str(Banknote)
# Record class as a factor
Banknote$class=factor(Banknote$class,levels = c(0,1),labels=c("Fake","correct"))
#Table of class
table(Banknote$class)
#Proportion with more informative
round(prop.table(table(Banknote$class))*100, digits = 2)
#summary of three variables
summary(Banknote[c("variance_Wavelet","skewness_Wavelet","curtosis_Wavelet","entropy")])
# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))
#Normalized Data Set
Banknote_normalized=as.data.frame(lapply(Banknote[1:4],normalize))
#confirm that normalization is working
summary(Banknote_normalized$skewness_Wavelet)

# create training and test data
Banknote_training=Banknote_normalized[1:200,]
Banknote_test=Banknote_normalized[201:274,]
# create labels for training and test data
Banknote_training_labels=Banknote[1:200, 5]
Banknote_test_labels=Banknote[201:274, 5]
length(Banknote_training_labels)
length(Banknote_training)
# visualize the data using labels
plot(Banknote$variance_Wavelet,Banknote$skewness_Wavelet,main="Scatter plot",xlab = "Variance Wavelet",ylab="Skewness_wavelet")
pairs(~variance_Wavelet+skewness_Wavelet+curtosis_Wavelet+entropy,data = Banknote,main="scatter plot")
#Uploading library car
library(car)
scatterplot(variance_Wavelet ~ skewness_Wavelet |class , data = Banknote)
scatterplotMatrix(~variance_Wavelet+skewness_Wavelet+curtosis_Wavelet+entropy | class, data = Banknote)
## Step 3: Training a model on the data ----
# load the "class" library
library(class)
Banknote_test_pred=knn(train = Banknote_training, test = Banknote_test, cl=Banknote_training_labels,k=3)
head(Banknote_test)
head(Banknote_test_pred)
summary(Banknote_test_pred)

## Step 4: Evaluating model performance 

# load the "gmodels" library
library(gmodels)
# Create the cross tabulation of predicted vs. actual
CrossTable(x=Banknote_test_labels ,y=Banknote_test_pred ,prop.chisq = FALSE)
CrossTable(x=Banknote_test_labels,y=Banknote_test_pred,prop.chisq = FALSE)
Banknote_test_labels
Banknote_test_pred
summary(Banknote_test_pred)
## Step 5: Improving model performance ----
# use the scale() function to z-score standardize a data frame
Banknote_z=as.data.frame(scale(Banknote[-5]))
# confirm that the transformation was applied correctly
summary(Banknote$variance_Wavelet)
# create training and test datasets
Banknote_training=Banknote_z[1:200,]
Banknote_test=Banknote_z[201:274,]
# re-classify test cases
Banknote_test_pred=knn(train = Banknote_training, test = Banknote_test, cl=Banknote_training_labels,k=3)
CrossTable(x=Banknote_test_labels,y=Banknote_test_pred,prop.chisq = FALSE)
summary(Banknote_test_pred)
# try several different values of k
Banknote_training=Banknote_normalized[1:200,]
Banknote_test=Banknote_normalized[201:274,]
Banknote_test_pred=knn(train = Banknote_training, test = Banknote_test, cl=Banknote_training_labels,k=11)
CrossTable(x=Banknote_test_labels,y=Banknote_test_pred,prop.chisq = FALSE)
Banknote_test_pred=knn(train = Banknote_training, test = Banknote_test, cl=Banknote_training_labels,k=33)
CrossTable(x=Banknote_test_labels,y=Banknote_test_pred,prop.chisq = FALSE)
Banknote_test_pred=knn(train = Banknote_training, test = Banknote_test, cl=Banknote_training_labels,k=1)
CrossTable(x=Banknote_test_labels,y=Banknote_test_pred,prop.chisq = FALSE)
Banknote_test_pred=knn(train = Banknote_training, test = Banknote_test, cl=Banknote_training_labels,k=5)
CrossTable(x=Banknote_test_labels,y=Banknote_test_pred,prop.chisq = FALSE)
Banknote_test_pred=knn(train = Banknote_training, test = Banknote_test, cl=Banknote_training_labels,k=9)
CrossTable(x=Banknote_test_labels,y=Banknote_test_pred,prop.chisq = FALSE)
Banknote_test_pred=knn(train = Banknote_training, test = Banknote_test, cl=Banknote_training_labels,k=17)
CrossTable(x=Banknote_test_labels,y=Banknote_test_pred,prop.chisq = FALSE)
Banknote_test_pred=knn(train = Banknote_training, test = Banknote_test, cl=Banknote_training_labels,k=23)
CrossTable(x=Banknote_test_labels,y=Banknote_test_pred,prop.chisq = FALSE)
Banknote_test_pred=knn(train = Banknote_training, test = Banknote_test, cl=Banknote_training_labels,k=33)
CrossTable(x=Banknote_test_labels,y=Banknote_test_pred,prop.chisq = FALSE)
Banknote_test_pred=knn(train = Banknote_training, test = Banknote_test, cl=Banknote_training_labels,k=43)
CrossTable(x=Banknote_test_labels,y=Banknote_test_pred,prop.chisq = FALSE)
