##################################
###### STAT 557 (Project 1) ######
##################################

rm(list=ls()) ## To clear your environment

## Read the data
xTrain=read.csv("ecoli_xTrain.csv", header=F)
yTrain=read.csv("ecoli_yTrain.csv", header=F)
xTest=read.csv("ecoli_xTest.csv", header=F)
yTest=read.csv("ecoli_yTest.csv", header=F)
colnames(yTrain)<-c('class')
dataframe <- as.data.frame(cbind(xTrain,yTrain))
#### Part 1 ####
logProd <- function(x){sum(x)
  
}

# I was told after class today 9/26/23 that this was appropriate
logSum <- function(x){sum(exp(x-max(x)))+max(x)
  
}

#### Part 2 ####


prior <- function(yTrain){prop.table(table(yTrain))}
p <- prior(yTrain)
likelihood <- function(xTrain, yTrain){
  df <- cbind(xTrain, yTrain)
  colnames(df)[6] <- "class"
  V <<- as.matrix(aggregate(df[, 1:5], list(df$class), FUN = var, simplify = TRUE))
  
  
  M <<- as.matrix(aggregate(df[, 1:5], list(df$class), FUN = mean, simplify = TRUE))

}
likelihood(xTrain, yTrain)
naiveBayesClassify <- function(xTest, M, V, p){
  matrixt <<- matrix(data=NA, nrow=1, ncol=nrow(xTest)) # This vector collects the *classifications* of each row

for (j in 1:nrow(xTest)){
  use <- matrix(data = 0, nrow =1, ncol = ncol(xTest)) # This vector is not permanent, and only holds the probability of each classification for a particular j
  for(c in 1:5){
    use[c] = p[c]*(dnorm(xTest[j,1],mean = M[c,2], sd=sqrt(V[c,2]),))*(dnorm(xTest[j,2],mean = sqrt(M[c,3]), sd=sqrt(V[c,3]),))*(dnorm(xTest[j,3],mean = M[c,4], sd=sqrt(V[c,4]),))*(dnorm(xTest[j,4],mean = M[c,5], sd=sqrt(V[c,5]),))*(dnorm(xTest[j,5],mean = M[c,6], sd=sqrt(V[c,6]),)) #This vector is not permanent, only holds the probabilites of each classification.
    
    
    
    
  }
  
  matrixt[j] = which.max(use)
  
} 
;return(matrixt)
  
}
m <- naiveBayesClassify(xTest, M, V, p)
mcol = t(m)
compare <- cbind(xTest, mcol, yTest)
colnames(compare)[7] <- "trueclass"
colnames(compare)[6] <- 'predclass'
#Recall, etc (will be in text doc)


#fraction of test samples correct. 
## Class 1 
fractioncorrect1 <- (sum(compare[,6] == compare[,7]))/(nrow(compare)) 
# .807
#precision
precision1 <- (sum(compare[,6] == compare[,7] & compare[,6] == 1))/(sum(compare[,6] == 1))
# .867
#recall
recall1 <- (sum(compare[,6] == compare[,7] & compare[,6] == 1))/(sum(compare[,7] == 1)) 

precision5 <- (sum(compare[,6] == compare[,7] & compare[,6] == 5))/(sum(compare[,6] == 5))
# .867
#recall
recall5 <- (sum(compare[,6] == compare[,7] & compare[,6] == 5))/(sum(compare[,7] == 5)) 
#.975


#### Part 3 ####

xTrain = read.csv("ecoli_new.xTrain.csv", header = F)
yTrain = read.csv("ecoli_new.yTrain.csv", header = F)
xTest = read.csv("ecoli_new.xTest.csv",header = F)
yTest = read.csv("ecoli_new.yTest.csv",header = F)
sigmoidProb <- function(y, x, w){
  wid = length(w)
  w_sum <- as.numeric(sum(w[2:wid]*x[2:wid]))
  if (y == 1){
    1-(1/(1+(exp(w[1]+ w_sum))))
  }else{
    (1/(1+(exp(w[1]+ w_sum))))
  }
}
logisticRegressionWeights <- function(xTrain, yTrain, w0, nIter){
  xTrain = as.matrix(xTrain)
  yTrain = as.matrix(yTrain)
  w_0 <- replicate(ncol(xTrain),numeric(nIter+1))
  w_0[1,1:ncol(xTrain)] <- w0
  
  for (h in 1:nIter){
    x <- replicate(ncol(xTrain), numeric(length(xTrain[,1])))
    for (i in 1:nrow(xTrain)){
      for (j in 1:ncol(xTrain)){
        x[i,j] <- xTrain[i,j]*(sigmoidProb(1,xTrain[i,],w_0[h,]) - yTrain[i] )
      }}
    
    x <- apply(x,2,sum)
    for (j in 1:ncol(xTrain)){
      w_0[h+1,j] <- w_0[h,j] - 0.1*x[j]
    }}
  return(w_0[nIter,])
  
}
w_i <- logisticRegressionWeights(xTrain,yTrain,c(1,1,1,1,1,1),2000)



logisticRegressionClassify <- function(xTest, w){
  matrixL <<- matrix(data=NA, nrow=1, ncol=nrow(xTest)) # This vector collects the *classifications* of each row
  
  for (j in 1:nrow(xTest)){
    use <- matrix(data = 0, nrow =1, ncol = 2) # This vector is not permanent, and only holds the probability of each classification for a particular j
    for(c in 1:2){
      use[c] = sigmoidProb((c-1),xTest[j,], w)
      
      
      
    }
    
    matrixL[j] =which.max(use) -1 
    
  } 
  ;return(matrixL)
}
logit <- logisticRegressionClassify(xTest, w_i)

predclass = t(logit)
compare2 <- cbind(xTest, predclass, yTest)
colnames(compare2)[8] <- "trueclass"

#Recall, etc (will be in text doc)

#fraction of test samples correct. 
fractioncorrect2 <- (sum(compare2[,7] == compare2[,8]))/(nrow(compare2)) 
# .963
#precision
precision2 <- (sum(compare2[,7] == compare2[,8] & compare2[,7] == 1))/(sum(compare2[,7] == 1))
# .974
#recall
recall2 <- (sum(compare2[,7] == compare2[,8] & compare2[,7] == 1))/(sum(compare2[,8] == 1)) 
#.925
 



