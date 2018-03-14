# Prepare train and test data
df<-read.csv('train.csv',header = TRUE)
set.seed(5555)
library(dplyr)
# get 70% of data grouped by species as train set
train <- df %>% group_by(species) %>% sample_n(7)
# the left is test set
test<-subset(df, !(id %in% train$id))


#### Random Forest model
set.seed(5555)
library(randomForest) 
modelr <- randomForest(species ~ ., data=train[,-1])
pr<-predict(modelr, test[,-c(1,2)],type = c("class"))

# Get Accuracy
test$predictr<-pr
xtabr <- table(test$species,test$predictr)
library(caret) 
confusionMatrix(xtabr)$overall




#### SVM model
library(e1071)
models <- svm(species ~ ., data=train[,-1])
ps<-predict(models, test[,-c(1,2)], type = c("class"))

#Get accuaracy
test$predicts<-ps
xtab <- table(test$species,test$predicts)
library(caret) 
confusionMatrix(xtab)$overall

#To view miss-classification for SVM model
nonsame<-test[test$species!=test$predicts,c("id","species","predicts")]
# To view miss-classification for random forest
nonsamer<-test[test$species!=test$predictr,c("id","species","predictr")]







##########check outlier within Quercus_Shumardii and id 377 in train set data

lir<-data.frame(train[train$species=='Quercus_Shumardii',])
id<-data.frame(df[df$id==377,])
lir<-rbind(lir,id)

datalist = list()
for (i in 1:(ncol(lir)-2))
{
  p<-lir[lir[,i+2]==boxplot(lir[,i+2])$out,"id"]
    
   if(length(p)==0)
   {
     p=0
   }
    datalist[[i]] <- p
}

n.obs <- sapply(datalist, length)
seq.max <- seq_len(max(n.obs))
# Convert in table form (value in cells is outlier ID)
mat <- t(sapply(datalist, "[", i = seq.max))



# For getting attribute names
dataname<-list()
for (i in 1:(ncol(lir)-2))
{
  dataname[[i]] <-  names(lir)[i+2]
}
variable<-do.call(rbind, dataname)
dataf<-data.frame(variable,mat)
# To view how many times (how many variables) that the observation appears as outlier
table(mat)
# ID 377 appears 49 times

# To see what type of variable that 377 is outlier
subset(dataf, subset=(X1==377|X2==377))$variable
# result: 
# margin34  margin46  margin62  shape4    shape5    shape6    shape7    shape8    shape9   
# shape10   shape11   shape12   shape13   shape14   shape15   shape18   shape24   shape25  
# shape26   shape27   shape28   shape29   shape30   shape31   shape32   shape33   shape34  
# shape42   shape43   shape46   shape47   shape48   shape49   shape50   shape51   shape52  
# shape53   shape54   shape55   shape56   shape57   shape58   shape59   shape60   shape61  
# texture21 texture36 texture46 texture61




##########check outlier within Quercus_Palustris and id 377 in train set data
com<-data.frame(train[train$species=='Quercus_Palustris' ,])
id<-data.frame(df[df$id==377,])
com<-rbind(com,id)

datalistcom = list() 
for (i in 1:(ncol(com)-2))
{
  p<-com[com[,i+2]==boxplot(com[,i+2])$out,"id"]
  
  if(length(p)==0)
  {
    p=0
  }
  datalistcom[[i]] <- p
}
n.obsc <- sapply(datalistcom, length)
seq.maxc <- seq_len(max(n.obsc))
matc <- t(sapply(datalistcom, "[", i = seq.maxc))


variablec<-do.call(rbind, dataname)
datafc<-data.frame(variablec,matc)

table(matc)
#377 appears 5 times (Third highest)

#To see what type of variable that 377 is outlier
subset(datafc, subset=(X1==377|X2==377|X3==377))$variable



######################################################### end





