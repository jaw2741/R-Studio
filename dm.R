########------------------------------------------------------------------------------------------------------------------------------------------------------
#######--根據個人資料保護法(下稱個資法)第 2、19、20 條規定，本資料須告知以下事項：
######--車牌、地址應屬第2條第1款「其他得以直接或間接方式識別該個人之資料」。
#####---地址足以讓第三人從此聯絡方式辨識出個人，並且存在於損害該他人的利益，
####----故有個資法之適用。
###-----依第19條第4款、第20條第5款，
##---本資料中所有資料僅供學術研究使用，不得有任何商業行為，且禁止轉提供給第三者。                 
#------------------------------------------------------------------------------------------------------------------------------------------------------


getwd()
setwd("c:\\users/asus/desktop/datam")
dt <- read.csv(file = "car2.csv", head=TRUE, sep = ",")
str(dt)

d4 <- read.csv(file = "car4.csv", head=TRUE, sep = ",")

    for(i in 1:length(d4)) 
        print(toString(d4[[i]]))
clk <- read.csv(file = "new1.csv", header =TRUE, sep = ",")


#---------------------------------------------全台分布------------------------------------------------------------------------------------------------

Alladd<- read.csv(file = "Alladd.csv", head=TRUE, sep = ",")

library("ggmap")
library("mapproj")
library("ggplot2")

#-----衛星圖-----#
map = get_googlemap(center= c(lon=median(Alladd$lon),lat=median(Alladd$lat))
                    ,zoom = 8, maptype = "satellite")
ggmap(map)  
#-----街道圖-----#  
map = get_googlemap(center= c(lon=median(Alladd$lon),lat=median(Alladd$lat))
                    ,zoom = 8, maptype = "roadmap")
ggmap(map)   

#-----熱點散布圖-#
ggmap(map)+geom_point(aes(x = lon, y = lat), size = 2, col="red",data = Alladd, alpha = 1)

#-----熱點等高圖-#
ggmap(map)+geom_point(aes(x = lon, y = lat), size = 2, col="red",data = Alladd, alpha = 0.6)+geom_density2d(data = Alladd, aes(x = lon, y=lat), size = 0.3)

#-----熱圖-------#
ggmap(map)+geom_point(aes(x = lon, y = lat), size = 2, col="red",data = Alladd, alpha = 1)+stat_density2d(data = Alladd, aes(x = lon, y=lat,fill = ..level.., alpha = ..level..)
                                                                                                      ,size = 0.01, bins = 16, geom = "polygon") +scale_fill_gradient(low = "green", high = "red",guide = FALSE)+scale_alpha(range = c(0, 0.3), guide = FALSE)


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------車輛顏色-----------#
table(d4$CARCOLOR)
which.max(table(d4$CARCOLOR))
utils::View(table(d4$CARCOLOR))

#----------車輛分布----------#
##直轄市##
length(grep(c("台北|臺北|新北"),(dt $ ADDR1)))
length(grep("桃園",(dt $ ADDR1)))
length(grep(c("台中|臺中"),(dt $ ADDR1)))
length(grep(c("台南|臺南"),(dt $ ADDR1)))
length(grep("高雄",(dt $ ADDR1)))
##縣市##
length(grep("基隆",(dt $ ADDR1)))
length(grep("新竹",(dt $ ADDR1)))
length(grep("嘉義",(dt $ ADDR1)))
##縣##
length(grep("彰化",(dt $ ADDR1)))
length(grep("南投",(dt $ ADDR1)))
length(grep("雲林",(dt $ ADDR1)))
length(grep("嘉義",(dt $ ADDR1)))
length(grep("屏東",(dt $ ADDR1)))
##東部外島
length(grep("宜蘭",(dt $ ADDR1)))
length(grep(c("台東|臺東"),(dt $ ADDR1)))
length(grep("澎湖",(dt $ ADDR1)))
length(grep("金門",(dt $ ADDR1)))
length(grep("連江",(dt $ ADDR1)))  
#----------------------------------------決策樹-------------------------------------------------#
#---根據車種與車齡---#
s63<- read.csv(file = "S63.csv", head=TRUE, sep = ",")
s63[is.na(s63)]<-0

library(C50)

n=0.3*nrow(s63)
test.index=sample(1:nrow(s63),n)
s63.train=s63[-test.index,]
s63.test=s63[test.index,]
install.packages("rpart")
library(rpart)
#---根據車種來分析年份---#
s63.tree=rpart(CARTYPE~., data= s63.train)
s63.tree
summary(s63.tree)


#---根據年份來分析車種---#
s64.tree = rpart(CARAGE~.,data = s63.train)
s64.tree
summary(s64.tree)
plot(s64.tree)
text(s64.tree)

#---------------------------------------K-means 分群演算法---------------------------------------------------------------------------------------------------------------------------
require(useful)

s500Train = s63[, which(names(s63) != "CARTYPE")]
set.seed(19000)
##############
s500L = kmeans(x = s500Train, centers = 30)
s500L
##-----觀察s500L---##
s500L$size
plot(s500L)
plotHartigan(s500L)
##我利用 nstart 引數來進行不同的初始條件進行分群(起始聚類條件)##
s600L = kmeans(x = s500Train, centers = 30,nstart = 25)
s600L
s600L$size
plot(s600L)
PlotHartigan(s600L)
#---------------------#
#車種分類#
table(s500L$cluster,s63$CARTYPE)
utils::View( table(s500L$cluster,s63$CARTYPE))
#年份分類#
table(s500L$cluster,s63$CARAGE)
utils::View(table(s500L$cluster,s63$CARAGE))

library(party)

ct <- ctree(CARAGE ~ ., data = s63)
plot(ct, main = "條件推論樹")
table(s63$CARAGE, predict(ct))




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


#----------------------------------------條件推論樹-------------------------------------------------------------#

library("party")

clk <- read.csv(file = "new1.csv", header =TRUE, sep = ",")
clk <- subset(clk ,select = c(CARAGE,DATE))

ct <- ctree(CARAGE ~ .,data = clk)
plot(ct, main = "條件推論樹")
table(clk$CARAGE,predict(ct))
#------------------------------------------隨機森林--------------------------------------------------------------------------------------------------------------------------------------------
install.packages("randomForest")
library(randomForest)
set.seed(1000)

#跑隨機樹森林模型
randomforestM <- randomForest(CARAGE ~ ., data = trainE55, importane = T, proximity = T, do.trace = 100)
randomforestM

#錯誤率
plot(randomforestM)

#衡量每一個變數對Y值的重要性，取到小數點第二位
round(importance(randomforestM), 2)
result <- predict(randomforestM, newdata = testE55)
result_Approved <- ifelse(result > 0.6, 1, 0)

#建立混淆矩陣
SLR <- table(testE55$CARAGE, result_Approved, dnn = c("實際", "預測"))
SLR

##-------------------------------------文字雲-------------------------------------------##


library(wordcloud)
wordcloud(dt$CARCOLOR, max.words = 40, scale = c(2.7, 1.3),random.order = FALSE)
wordcloud(dt$CARTYPE, max.words = 60, scale = c(3, 1.5),random.order = F)
wordcloud(dt$CARAGE, max.words = 60, scale = c(3, 1.5),random.order = F)
suppressWarnings(wordcloud(dt$OTH1, max.words = 60, scale = c(3.5, 1.8),random.order = F))

#-------------------------------------------------------------------------------------------------------------------------------------------------------
#####取車類別##

utils::View((summary(dt$OTH1,options(max.print=999999999))))
length(grep(c("限查封|限點交"),(dt$OTH1)))
length(grep(c("交接"),(dt$OTH1)))
length(grep(c("失竊"),(dt$OTH1)))
length(grep(c("殘值"),(dt$OTH1)))

#####保險&公司##
length(grep(c("台灣人壽保險"),(dt $ ADDR1)))
length(grep(c("公司"),(dt $ ADDR1)))

#####銀行&汽車##
length(grep(c("銀行|元大|新光|聯邦|安泰|三信商銀|中租|匯豐|和潤"),(dt $ ADDR1)))
length(grep(c("汽車|台灣福斯|實業|朝欽|新誠"),(dt $ ADDR1)))

#####租賃&貨運##
length(grep(c("租賃|和運租車|格上"),(dt $ ADDR1)))
length(grep(c("融資|資融|資產管理"),(dt $ ADDR1)))
length(grep(c("貨運"),(dt $ ADDR1)))

###################################################################################################################################
  
###出廠年份統計##
d4<- read.csv(file = "car4.csv", head=TRUE, sep = ",")
(table(d4$CARAGE))
summary(d4$CARAGE)


utils::View(summary(dt$CARAGE,options(max.print=999999999)))##表格


###年份關係##
plot(x = dt$CARAGE, y = dt$NO,
     xlab = "CARAGE",
     ylab = "NO")

###汽車出廠年份與新增車單編號###
plot(x = clk$CARAGE, y = clk$DATE,
     xlab = "CARAGE",
     ylab = "DATE")


######################################################################################################################
clk <- read.csv(file = "new1.csv", header =TRUE, sep = ",")
dim(clk)
sum(is.na(clk))
##-------------------------------------- K-means 分群演算法(此處為年份) -------------------------------------------##

clkTrain = clk[, which(names(clk) != "DATE")]
set.seed(19053)
##############
sss = kmeans(x = clkTrain, centers = 11)
sss
##---觀察---##
sss$size

install.packages("FitMeans")
install.packages("useful")
require(useful)
require(FitMeans)

sum(is.na(clkTrain))

##利用useful套件中的 FitMeans 進行 k個分群群內平方和與 k+1 個分群群內平方和的比例##
clkbest= FitKMeans(clkTrain,seed = 19053)

##---觀察---##
clkbest

##---繪圖---##
PlotHartigan(clkbest)

##--根據上述結果最佳群數為11 ，並以此繪出矩陣
table(clk$CARAGE,sss$cluster)
##--新增單號和汽車年份的混淆矩陣--##
plot(table(clk$CARAGE, sss$cluster),
     main="Confusion Matrix for clk Clustering",
     xlab="age", ylab="Cluster")

##############----------------------------------------------##################

##統計最多##
which.max(table(dt$CARAGE))
length(which((dt$CARAGE)==2005))##共 3099 台
which.min(table(dt$CARAGE))

utils::View(summary(dt$DATE,options(max.print=999999999)))##表格

###所有出廠年份統計###
utils::View(summary(dt$CARAGE,options(max.print=999999999)))##表格


###日期數量表##
summary(dt$DATE,options(max.print=999999999))####日期
summary(dt$CHGDATE,options(max.print=999999999))##異動日期
###日期及異動日期(兩者比對)##
summary(dt[c("DATE", "CHGDATE")],options(max.print=999999999))

###車牌統計##
utils::View(summary(dt$CARNO,options(max.print=999999999)))

###所有車種統計###

utils::View(summary(dt$CARTYPE,options(max.print=999999999)))

###出現最多次車種###
which.max(table(dt$CARTYPE))
which.min(table(dt$CARTYPE))

###各廠牌汽車##
length(grep(c("國瑞|豐田|TOYOTA|Toyota|VIOS|CAMRY"),(dt $ CARTYPE)))
length(grep(c("中華|三菱"),(dt $ CARTYPE)))
length(grep(c("日產|NISSA"),(dt $ CARTYPE)))
length(grep(c("三陽"),(dt $ CARTYPE)))
length(grep(c("本田"),(dt $ CARTYPE)))
length(grep(c("拖車|曳引車"),(dt $ CARTYPE)))
length(grep(c("遊覽"),(dt $ CARTYPE)))
length(grep(c("賓士|寶馬|BENZ|BMW|benz|bmw|AUDI|audi|保時捷"),(dt $ CARTYPE)))


#########------------------------尋回地點分布(以台南為例)----------------------------------#########

BD<- read.csv(file = "BD2017.10.27.csv", head=TRUE, sep = ",")

library("ggmap")
library("mapproj")
library("ggplot2")

#-----衛星圖-----#
map = get_googlemap(center= c(lon=median(BD$lon),lat=median(BD$lat))
                    ,zoom = 14 , maptype = "satellite")
ggmap(map)  
#-----街道圖-----#  
map = get_googlemap(center= c(lon=median(BD$lon),lat=median(BD$lat))
                    ,zoom = 12, maptype = "roadmap")
ggmap(map)   

#-----熱點散布圖-#
ggmap(map)+geom_point(aes(x = lon, y = lat), size = 2, col="red",data = BD, alpha = 1)

#-----熱點等高圖-#
ggmap(map)+geom_point(aes(x = lon, y = lat), size = 2, col="red",data = BD, alpha = 0.6)+geom_density2d(data = BD, aes(x = lon, y=lat), size = 0.3)

#-----熱圖-------#
ggmap(map)+geom_point(aes(x = lon, y = lat), size = 2, col="red",data = BD, alpha = 1)+stat_density2d(data = BD, aes(x = lon, y=lat,fill = ..level.., alpha = ..level..)
                ,size = 0.01, bins = 16, geom = "polygon") +scale_fill_gradient(low = "green", high = "red",guide = FALSE)+scale_alpha(range = c(0, 0.3), guide = FALSE)


  


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------















