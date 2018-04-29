########------------------------------------------------------------------------------------------------------------------------------------------------------
#######--�ھڭӤH��ƫO�@�k(�U�٭Ӹ�k)�� 2�B19�B20 ���W�w�A����ƶ��i���H�U�ƶ��G
######--���P�B�a�}���ݲ�2����1�ڡu��L�o�H�����ζ����覡�ѧO�ӭӤH����ơv�C
#####---�a�}���H���ĤT�H�q���p���覡���ѥX�ӤH�A�åB�s�b��l�`�ӥL�H���Q�q�A
####----�G���Ӹ�k���A�ΡC
###-----�̲�19����4�ڡB��20����5�ڡA
##---����Ƥ��Ҧ���ƶȨѾǳN��s�ϥΡA���o������ӷ~�欰�A�B�T���ണ�ѵ��ĤT�̡C                 
#------------------------------------------------------------------------------------------------------------------------------------------------------


getwd()
setwd("c:\\users/asus/desktop/datam")
dt <- read.csv(file = "car2.csv", head=TRUE, sep = ",")
str(dt)

d4 <- read.csv(file = "car4.csv", head=TRUE, sep = ",")

    for(i in 1:length(d4)) 
        print(toString(d4[[i]]))
clk <- read.csv(file = "new1.csv", header =TRUE, sep = ",")


#---------------------------------------------���x����------------------------------------------------------------------------------------------------

Alladd<- read.csv(file = "Alladd.csv", head=TRUE, sep = ",")

library("ggmap")
library("mapproj")
library("ggplot2")

#-----�ìP��-----#
map = get_googlemap(center= c(lon=median(Alladd$lon),lat=median(Alladd$lat))
                    ,zoom = 8, maptype = "satellite")
ggmap(map)  
#-----��D��-----#  
map = get_googlemap(center= c(lon=median(Alladd$lon),lat=median(Alladd$lat))
                    ,zoom = 8, maptype = "roadmap")
ggmap(map)   

#-----���I������-#
ggmap(map)+geom_point(aes(x = lon, y = lat), size = 2, col="red",data = Alladd, alpha = 1)

#-----���I������-#
ggmap(map)+geom_point(aes(x = lon, y = lat), size = 2, col="red",data = Alladd, alpha = 0.6)+geom_density2d(data = Alladd, aes(x = lon, y=lat), size = 0.3)

#-----����-------#
ggmap(map)+geom_point(aes(x = lon, y = lat), size = 2, col="red",data = Alladd, alpha = 1)+stat_density2d(data = Alladd, aes(x = lon, y=lat,fill = ..level.., alpha = ..level..)
                                                                                                      ,size = 0.01, bins = 16, geom = "polygon") +scale_fill_gradient(low = "green", high = "red",guide = FALSE)+scale_alpha(range = c(0, 0.3), guide = FALSE)


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------�����C��-----------#
table(d4$CARCOLOR)
which.max(table(d4$CARCOLOR))
utils::View(table(d4$CARCOLOR))

#----------��������----------#
##���ҥ�##
length(grep(c("�x�_|�O�_|�s�_"),(dt $ ADDR1)))
length(grep("���",(dt $ ADDR1)))
length(grep(c("�x��|�O��"),(dt $ ADDR1)))
length(grep(c("�x�n|�O�n"),(dt $ ADDR1)))
length(grep("����",(dt $ ADDR1)))
##����##
length(grep("��",(dt $ ADDR1)))
length(grep("�s��",(dt $ ADDR1)))
length(grep("�Ÿq",(dt $ ADDR1)))
##��##
length(grep("����",(dt $ ADDR1)))
length(grep("�n��",(dt $ ADDR1)))
length(grep("���L",(dt $ ADDR1)))
length(grep("�Ÿq",(dt $ ADDR1)))
length(grep("�̪F",(dt $ ADDR1)))
##�F���~�q
length(grep("�y��",(dt $ ADDR1)))
length(grep(c("�x�F|�O�F"),(dt $ ADDR1)))
length(grep("���",(dt $ ADDR1)))
length(grep("����",(dt $ ADDR1)))
length(grep("�s��",(dt $ ADDR1)))  
#----------------------------------------�M����-------------------------------------------------#
#---�ھڨ��ػP����---#
s63<- read.csv(file = "S63.csv", head=TRUE, sep = ",")
s63[is.na(s63)]<-0

library(C50)

n=0.3*nrow(s63)
test.index=sample(1:nrow(s63),n)
s63.train=s63[-test.index,]
s63.test=s63[test.index,]
install.packages("rpart")
library(rpart)
#---�ھڨ��بӤ��R�~��---#
s63.tree=rpart(CARTYPE~., data= s63.train)
s63.tree
summary(s63.tree)


#---�ھڦ~���Ӥ��R����---#
s64.tree = rpart(CARAGE~.,data = s63.train)
s64.tree
summary(s64.tree)
plot(s64.tree)
text(s64.tree)

#---------------------------------------K-means ���s�t��k---------------------------------------------------------------------------------------------------------------------------
require(useful)

s500Train = s63[, which(names(s63) != "CARTYPE")]
set.seed(19000)
##############
s500L = kmeans(x = s500Train, centers = 30)
s500L
##-----�[��s500L---##
s500L$size
plot(s500L)
plotHartigan(s500L)
##�ڧQ�� nstart �޼ƨӶi�椣�P����l����i����s(�_�l�E������)##
s600L = kmeans(x = s500Train, centers = 30,nstart = 25)
s600L
s600L$size
plot(s600L)
PlotHartigan(s600L)
#---------------------#
#���ؤ���#
table(s500L$cluster,s63$CARTYPE)
utils::View( table(s500L$cluster,s63$CARTYPE))
#�~������#
table(s500L$cluster,s63$CARAGE)
utils::View(table(s500L$cluster,s63$CARAGE))

library(party)

ct <- ctree(CARAGE ~ ., data = s63)
plot(ct, main = "������׾�")
table(s63$CARAGE, predict(ct))




#----------------------------------------�M����-----------------------------------------------#                            
#---�ھڨ��ֻP�s�W����---#

library("rattle")
library("rpart")
library("rpart.plot")
library("rattle")
library("AER")

E55 <- read.csv(file = "car4.csv", head=TRUE, sep = ",")
E55 <- subset(E55 ,select = c(CARTYPE,CARAGE,CARNO,CARCOLOR,ADDR1,COMPANY))


E55$CARAGE <- ifelse(E55$CARAGE == "2005",1,0);


E55[is.na(E55)]<-0

#----���o�`����
n<-nrow(E55)

#----�]�w�H���ƺؤl
set.seed(18000)

#----�N�ƾڶ��ǭ��s�ƦC
E63<-E55[sample(n),]

#----���X�˥��ƪ�SLR
SLR <- sample(seq_len(n),size = round(0.7*n))

#----�V�m��ƻP���ո�Ƥ��: 70%�ؼҡA30%����
trainE55 <- E63[SLR,]
testE55  <- E63[-SLR,]

#---�إߨM����ҫ� 
dtreeM <- rpart(formula = CARAGE ~ .,data = trainE55,
                method = "class",control = rpart.control(cp = 0.001))

dtreeM



#######################################################################################################

S600 <- predict(dtreeM, newdata = testE55, type = "class")

##�إ߲V�c�x�}�[��ҫ����{--##
AMG_GT <- table(testE55$CARAGE, S600, dnn = c("���", "�w��"))
AMG_GT

###-�w���~����2005�~�ǽT�v--###


#���G�v
AMG_GT[4] / sum(AMG_GT[, 2])

#�����G�v
AMG_GT[1] / sum(AMG_GT[, 1])

#����ǽT�v(���X�﨤/�`��)
accuracy <- sum(diag(AMG_GT)) / sum(AMG_GT)
accuracy


#######################################################################################################


#----------------------------------------������׾�-------------------------------------------------------------#

library("party")

clk <- read.csv(file = "new1.csv", header =TRUE, sep = ",")
clk <- subset(clk ,select = c(CARAGE,DATE))

ct <- ctree(CARAGE ~ .,data = clk)
plot(ct, main = "������׾�")
table(clk$CARAGE,predict(ct))
#------------------------------------------�H���˪L--------------------------------------------------------------------------------------------------------------------------------------------
install.packages("randomForest")
library(randomForest)
set.seed(1000)

#�]�H����˪L�ҫ�
randomforestM <- randomForest(CARAGE ~ ., data = trainE55, importane = T, proximity = T, do.trace = 100)
randomforestM

#���~�v
plot(randomforestM)

#�Ŷq�C�@���ܼƹ�Y�Ȫ����n�ʡA����p���I�ĤG��
round(importance(randomforestM), 2)
result <- predict(randomforestM, newdata = testE55)
result_Approved <- ifelse(result > 0.6, 1, 0)

#�إ߲V�c�x�}
SLR <- table(testE55$CARAGE, result_Approved, dnn = c("���", "�w��"))
SLR

##-------------------------------------��r��-------------------------------------------##


library(wordcloud)
wordcloud(dt$CARCOLOR, max.words = 40, scale = c(2.7, 1.3),random.order = FALSE)
wordcloud(dt$CARTYPE, max.words = 60, scale = c(3, 1.5),random.order = F)
wordcloud(dt$CARAGE, max.words = 60, scale = c(3, 1.5),random.order = F)
suppressWarnings(wordcloud(dt$OTH1, max.words = 60, scale = c(3.5, 1.8),random.order = F))

#-------------------------------------------------------------------------------------------------------------------------------------------------------
#####�������O##

utils::View((summary(dt$OTH1,options(max.print=999999999))))
length(grep(c("���d��|���I��"),(dt$OTH1)))
length(grep(c("�汵"),(dt$OTH1)))
length(grep(c("����"),(dt$OTH1)))
length(grep(c("�ݭ�"),(dt$OTH1)))

#####�O�I&���q##
length(grep(c("�x�W�H�ثO�I"),(dt $ ADDR1)))
length(grep(c("���q"),(dt $ ADDR1)))

#####�Ȧ�&�T��##
length(grep(c("�Ȧ�|���j|�s��|�p��|�w��|�T�H�ӻ�|����|����|�M��"),(dt $ ADDR1)))
length(grep(c("�T��|�x�W�ִ�|��~|�´�|�s��"),(dt $ ADDR1)))

#####����&�f�B##
length(grep(c("����|�M�B����|��W"),(dt $ ADDR1)))
length(grep(c("�ĸ�|���|�겣�޲z"),(dt $ ADDR1)))
length(grep(c("�f�B"),(dt $ ADDR1)))

###################################################################################################################################
  
###�X�t�~���έp##
d4<- read.csv(file = "car4.csv", head=TRUE, sep = ",")
(table(d4$CARAGE))
summary(d4$CARAGE)


utils::View(summary(dt$CARAGE,options(max.print=999999999)))##����


###�~�����Y##
plot(x = dt$CARAGE, y = dt$NO,
     xlab = "CARAGE",
     ylab = "NO")

###�T���X�t�~���P�s�W����s��###
plot(x = clk$CARAGE, y = clk$DATE,
     xlab = "CARAGE",
     ylab = "DATE")


######################################################################################################################
clk <- read.csv(file = "new1.csv", header =TRUE, sep = ",")
dim(clk)
sum(is.na(clk))
##-------------------------------------- K-means ���s�t��k(���B���~��) -------------------------------------------##

clkTrain = clk[, which(names(clk) != "DATE")]
set.seed(19053)
##############
sss = kmeans(x = clkTrain, centers = 11)
sss
##---�[��---##
sss$size

install.packages("FitMeans")
install.packages("useful")
require(useful)
require(FitMeans)

sum(is.na(clkTrain))

##�Q��useful�M�󤤪� FitMeans �i�� k�Ӥ��s�s������M�P k+1 �Ӥ��s�s������M�����##
clkbest= FitKMeans(clkTrain,seed = 19053)

##---�[��---##
clkbest

##---ø��---##
PlotHartigan(clkbest)

##--�ھڤW�z���G�̨θs�Ƭ�11 �A�åH��ø�X�x�}
table(clk$CARAGE,sss$cluster)
##--�s�W�渹�M�T���~�����V�c�x�}--##
plot(table(clk$CARAGE, sss$cluster),
     main="Confusion Matrix for clk Clustering",
     xlab="age", ylab="Cluster")

##############----------------------------------------------##################

##�έp�̦h##
which.max(table(dt$CARAGE))
length(which((dt$CARAGE)==2005))##�@ 3099 �x
which.min(table(dt$CARAGE))

utils::View(summary(dt$DATE,options(max.print=999999999)))##����

###�Ҧ��X�t�~���έp###
utils::View(summary(dt$CARAGE,options(max.print=999999999)))##����


###����ƶq��##
summary(dt$DATE,options(max.print=999999999))####���
summary(dt$CHGDATE,options(max.print=999999999))##���ʤ��
###����β��ʤ��(��̤��)##
summary(dt[c("DATE", "CHGDATE")],options(max.print=999999999))

###���P�έp##
utils::View(summary(dt$CARNO,options(max.print=999999999)))

###�Ҧ����زέp###

utils::View(summary(dt$CARTYPE,options(max.print=999999999)))

###�X�{�̦h������###
which.max(table(dt$CARTYPE))
which.min(table(dt$CARTYPE))

###�U�t�P�T��##
length(grep(c("���|�ץ�|TOYOTA|Toyota|VIOS|CAMRY"),(dt $ CARTYPE)))
length(grep(c("����|�T��"),(dt $ CARTYPE)))
length(grep(c("�鲣|NISSA"),(dt $ CARTYPE)))
length(grep(c("�T��"),(dt $ CARTYPE)))
length(grep(c("����"),(dt $ CARTYPE)))
length(grep(c("�쨮|���ި�"),(dt $ CARTYPE)))
length(grep(c("�C��"),(dt $ CARTYPE)))
length(grep(c("���h|�_��|BENZ|BMW|benz|bmw|AUDI|audi|�O�ɱ�"),(dt $ CARTYPE)))


#########------------------------�M�^�a�I����(�H�x�n����)----------------------------------#########

BD<- read.csv(file = "BD2017.10.27.csv", head=TRUE, sep = ",")

library("ggmap")
library("mapproj")
library("ggplot2")

#-----�ìP��-----#
map = get_googlemap(center= c(lon=median(BD$lon),lat=median(BD$lat))
                    ,zoom = 14 , maptype = "satellite")
ggmap(map)  
#-----��D��-----#  
map = get_googlemap(center= c(lon=median(BD$lon),lat=median(BD$lat))
                    ,zoom = 12, maptype = "roadmap")
ggmap(map)   

#-----���I������-#
ggmap(map)+geom_point(aes(x = lon, y = lat), size = 2, col="red",data = BD, alpha = 1)

#-----���I������-#
ggmap(map)+geom_point(aes(x = lon, y = lat), size = 2, col="red",data = BD, alpha = 0.6)+geom_density2d(data = BD, aes(x = lon, y=lat), size = 0.3)

#-----����-------#
ggmap(map)+geom_point(aes(x = lon, y = lat), size = 2, col="red",data = BD, alpha = 1)+stat_density2d(data = BD, aes(x = lon, y=lat,fill = ..level.., alpha = ..level..)
                ,size = 0.01, bins = 16, geom = "polygon") +scale_fill_gradient(low = "green", high = "red",guide = FALSE)+scale_alpha(range = c(0, 0.3), guide = FALSE)


  


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------














