
library(rsparse)
library(Matrix)
library(caret)
library(data.table)
#Library 'Matrix' is used in order to deal with sparse matrices

?sparseMatrix
?WRMF

#Definition of the confidence function
confidence<-function(X,alfa){
  X@x<-1+(alfa)*log(1+X@x)
  #map the (sparse) matrix elements r_ui in 1+alfa*log(1+rui)
  return(X)
}

##########################################################
#Baseline example for the CV phase:

total1<-as(total, "sparseMatrix")
#as rsparse works with sparse matrices + compressing

#training-test split
idxtrain<-createDataPartition(total[,1],p=.75,list = F)[,1]
total_tr<-total1[idxtrain, ]
total_te<-total1[-idxtrain, ]

total_tr.c<-confidence(X=total_tr,20)
total_te.c<-confidence(X=total_te,20)
model<-WRMF$new(rank=4,lambda=10,feedback="implicit")

#user_em and item_em are - respectively - the user and item matrix obtained after the ALS factorization
user_em<-model$fit_transform(x=total_tr.c,n_iter=100,convergence_tol=-1)
item_em<-model$components
test=model$predict(x=total_te.c, k=10,not_recommend=total_te.c)
test #recommendations
ap_k(predictions=test,actual=total_te) #AP for every user

#In this part we split the test data into history and future
#This method is taken by D. Selianov.

#Most operations with sparse matrices are performed using the compressed, column-oriented or CsparseMatrix 
#representation. The triplet representation is convenient for creating a sparse matrix or for reading and 
#writing such matrices. 
#Once it is created, however, the matrix is generally coerced to a CsparseMatrix for further operations.

# convert matrix to COO format and then to data.table
temp = as(total_te, "TsparseMatrix")
temp = data.table(i = temp@i, j = temp@j, x = temp@x)
# mark 50% as history
temp[, history := sample(c(TRUE, FALSE), .N, replace = TRUE, prob = c(0.5, 0.5)), by = i]
head(temp)
X_te_history = temp[history == TRUE]
X_te_future = temp[history == FALSE]
rm(temp)

# and convert back to sparse matrices
X_te_history = sparseMatrix( i = X_te_history$i, j = X_te_history$j, x = X_te_history$x, 
                             dims = dim(total_te), dimnames = dimnames(total_te), index1 = FALSE)
X_te_future = sparseMatrix( i = X_te_future$i, j = X_te_future$j, x = X_te_future$x, 
                            dims = dim(total_te), dimnames = dimnames(total_te), index1 = FALSE)

X_te_history.c<-confidence(X_te_history,10)
test=model$predict(x=X_te_history.c, k=10,not_recommend=X_te_history.c)

#We will use the MAP to cross-validate instead of the loss function
#because the loss function depends on alpha, and alpha gives rise to different models

mean(ap_k(predictions=test,actual=X_te_future))



###########################################################
#Cross-validation phase
#dividing the test dataset into history and future
#In this way we can compute the performance of the model using MAP@k
#We put lambda and alpha into the same grid
#This isn't a "pure" CV as it is not done on the loss function
#but rather using the MAP@10 metric evaluation

?expand.grid #used in order to get a CV-like grid

lambda<-1:20
alpha<-seq(0.1,20,by=0.5)
rank<-3:10
la<-expand.grid(lambda,alpha,rank)
myCV<-numeric(dim(la)[1])
cv.temp<-numeric(4)

#4-fold cross validation
#it takes a LOT
for(i in 1:dim(la)[1]){
    
  for (k in 1:4){
    first<-3*(k-1)+1
    second<-(3*k)
    idxtest<- first:second
    total_tr<-total1[-idxtest, ]
    total_te<-total1[idxtest, ]
    
    total_tr.c<-confidence(X=total_tr,la[i,2]) #la[i,2]=alpha
    total_te.c<-confidence(X=total_te,la[i,2])
    model<-WRMF$new(rank=la[i,3],lambda=la[i,1],feedback="implicit")
    
    user_em<-model$fit_transform(x=total_tr.c,n_iter=30,convergence_tol=-1)
    temp = as(total_te, "TsparseMatrix")
    temp = data.table(i = temp@i, j = temp@j, x = temp@x)
    temp[, history := sample(c(TRUE, FALSE), .N, replace = TRUE, prob = c(0.5, 0.5)), by = i]
    head(temp)
    X_te_history = temp[history == TRUE]
    X_te_future = temp[history == FALSE]
    rm(temp)
    X_te_history = sparseMatrix( i = X_te_history$i, j = X_te_history$j, x = X_te_history$x, 
                                 dims = dim(total_te), dimnames = dimnames(total_te), index1 = FALSE)
    X_te_future = sparseMatrix( i = X_te_future$i, j = X_te_future$j, x = X_te_future$x, 
                                dims = dim(total_te), dimnames = dimnames(total_te), index1 = FALSE)
    
    X_te_history.c<-confidence(X_te_history,la[i,2])
    test=model$predict(x=X_te_history.c, k=10,not_recommend=X_te_history.c)
    
    cv.temp[k]<-mean(ap_k(predictions=test,actual=X_te_future))
    }
    myCV[i]<-mean(cv.temp)
    }
rm(cv.temp)

i.star<-which.max(myCV)
myCV[i.star] #optimal mean cross validated MAP@10
plot(myCV,main="Cross-validated MAP@10",ylab="MAP@10")
abline(v=i.star,col="red",lty=4)

opt.la<-la[i.star,]
opt.la

lambda.opt<-as.numeric(opt.la[1])
alpha.opt<-as.numeric(opt.la[2])
rank.opt<-as.numeric(opt.la[3])

#####Plotting mean MAP@10 for each rank 
#for each rank 
r <- length(myCV)/800
cvmeans <- numeric(r)
cv.matrix = matrix(0, nrow = 800, ncol = r)
for(i in 1:r){
  n1 <- 800*(i-1)+1
  n2 <- 800*i
  cvmeans[i] <- mean(myCV[n1:n2])
  cv.matrix[,i] <- myCV[n1:n2] 
}

plot(y = cvmeans, x = 3:10, 
     main = expression("Average CV-performance by rank (tuning" ~lambda~ "&" ~alpha~")"), 
     xlab = "Rank", ylab = "Average Map Score", type = "l", 
     col = "grey60", lty = 3, lwd = 1.5)
points(x = 3:10, y = cvmeans, pch = 16, col = "grey20")


boxplot(cv.matrix[,1:8], at = 3:10, col = "lemonchiffon3", 
        main = expression("Boxplot of Average CV-performance by rank (tuning" ~lambda~ "&" ~alpha~")"),
        xlab = "Rank", ylab = "Average Map Score", xaxt = "n")
axis(side = 1, at = 3:10, labels = c(3:10))
points(x = 7, y = myCV[i.star], col = "red", pch = 4, lwd = 3)
points(x = 3, y = min(myCV), col = "blue", pch = 4, lwd = 3)


###########################################
#after tuning the parameters, we can set the model with the optimal
#hyperparameters (fixed) and see what happens to the recommendations of our group
#we won't need to split up the dataset into future and history as 
#we are not interested into the MAP@10 eval anymore

idx<-sample(1:5,replace=F,size=3)
#idx<-createDataPartition(total[,1],p=.25,list = F)[,1]
model<-WRMF$new(rank=rank.opt,lambda=lambda.opt,feedback="implicit")
total_tr<-total1[-idx, ]
total_te<-total1[idx, ]

#transforming the dataset matrix with the confidence function
total_tr.c<-confidence(X=total_tr,alpha.opt)
total_te.c<-confidence(X=total_te,alpha.opt)

#fitting the model
user_em<-model$fit_transform(x=total_tr.c,n_iter=20,convergence_tol=-1)
item_em<-model$components
myPrediction=model$predict(x=total_te.c, k=10,not_recommend=total_te.c)
myPrediction

#User matrix embedded using latent factors (with optimal hyperparameters)
View(user_em)
#Item matrix after factorization
View(item_em)






