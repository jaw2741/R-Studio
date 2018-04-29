#---------------------------------------------------------------------------------------------#
###############
##############
#############
############
###########
##########
#########
########
#######
######
#####
####
###
##
#----------------------------------------決策樹-----------------------------------------------#                            
#---根據車齡與新增順序---#

library("rattle")
library("rpart")
library("rpart.plot")
library("rattle")
library("AER")

E55 <- read.csv(file = "car4.csv", head=TRUE, sep = ",")
E55 <- subset(E55 ,select = c(CARTYPE,CARAGE,CARNO,CARCOLOR,ADDR1,COMPANY))


E55$CARAGE <- ifelse(E55$CARAGE == "2005",1,0);


E55[is.na(E55)]<-0

#----取得總筆數
n<-nrow(E55)

#----設定隨機數種子
set.seed(18000)

#----將數據順序重新排列
E63<-E55[sample(n),]

#----取出樣本數的SLR
SLR <- sample(seq_len(n),size = round(0.7*n))

#----訓練資料與測試資料比例: 70%建模，30%驗證
trainE55 <- E63[SLR,]
testE55  <- E63[-SLR,]

#---建立決策樹模型 
dtreeM <- rpart(formula = CARAGE ~ .,data = trainE55,
                method = "class",control = rpart.control(cp = 0.001))

dtreeM



#######################################################################################################

S600 <- predict(dtreeM, newdata = testE55, type = "class")

##建立混淆矩陣觀察模型表現--##
AMG_GT <- table(testE55$CARAGE, S600, dnn = c("實際", "預測"))
AMG_GT

###-預測年份為2005年準確率--###


#賓果率
AMG_GT[4] / sum(AMG_GT[, 2])

#未賓果率
AMG_GT[1] / sum(AMG_GT[, 1])

#整體準確率(取出對角/總數)
accuracy <- sum(diag(AMG_GT)) / sum(AMG_GT)
accuracy


#######################################################################################################


(length((which(E55[,5] == 2005)))/( sum(table(E55$CARAGE))))         

length(which(E55[,5] == 2005))
  