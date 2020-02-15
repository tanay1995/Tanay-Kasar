
library(ggplot2) 
library(lattice)
library(readr) 
library(data.table)
library(plyr)
library(randomForest)
library(e1071)
library(caret)
coverData <- read.csv("train.csv", header = T)
str(coverData)
summary(coverData)
coverData[,12:56] <- lapply(coverData[,12:56], as.factor)
coverDT <- as.data.table(coverData)
reGroup <- function(oldColumns, newLabels, columnName){
  for(i in 1:length(newLabels)) {
    coverDT<-coverDT[get(oldColumns[i])==1,paste(columnName):=newLabels[i]]
  }
}

newLabels <- c("Rawah","Neota","Comanche Peak","Cache la Poudre")
oldColumns <- c("Wilderness_Area1","Wilderness_Area2","Wilderness_Area3","Wilderness_Area4")
columnName <- "Wilderness_Area"

reGroup(oldColumns, newLabels, columnName)
newLabels<-c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31','32','33','34','35','36','37','38','39','40')
oldColumns <- c("Soil_Type1","Soil_Type2","Soil_Type3","Soil_Type4","Soil_Type5","Soil_Type6","Soil_Type7","Soil_Type8",
                "Soil_Type9","Soil_Type10","Soil_Type11","Soil_Type12","Soil_Type13","Soil_Type14","Soil_Type15","Soil_Type16",
                "Soil_Type17","Soil_Type18","Soil_Type19","Soil_Type20","Soil_Type21","Soil_Type22","Soil_Type23","Soil_Type24",
                "Soil_Type25","Soil_Type26","Soil_Type27","Soil_Type28","Soil_Type29","Soil_Type30","Soil_Type31","Soil_Type32",
                "Soil_Type33","Soil_Type34","Soil_Type35","Soil_Type36","Soil_Type37","Soil_Type38","Soil_Type39","Soil_Type40")
columnName <- "Soil_Type"


reGroup(oldColumns, newLabels, columnName)


coverDT <- coverDT[,colnames(coverDT[,12:55,with=F]):=NULL]

coverDT$Cover_Type <- mapvalues(coverDT$Cover_Type, 
                                from = c(1,2,3,4,5,6,7), to = c("Spruce/Fir","Lodgepole Pine","Ponderosa Pine",
                                                                "Cottonwood/Willow","Aspen","Douglas-fir","Krummholz"))

coverData <- as.data.frame(coverDT)
rm(coverDT)


par(mfrow=c(3,4))
for(i in 2:11){
  hist(coverData[,i], xlab = '', col='steelblue',  main=names(coverData[i]))
}

table(coverData$Cover_Type)
theme_update(plot.title = element_text(hjust = 0.5))
ggplot(coverData, aes(Cover_Type, fill=as.factor(Cover_Type))) +
  geom_bar() +
  labs(title="Cover Type Distribution", x="Cover Type", y="Count") +
  scale_fill_discrete(name="Cover Type")

ggplot(coverData, aes(x=Cover_Type, y=Elevation, fill = as.factor(Cover_Type))) +
  geom_boxplot() +
  labs(title="Elevation by Cover Type", x="Cover Type", y="Elevation") +
  scale_fill_discrete(name = "Cover Type")

ggplot(coverData, aes(Elevation, fill=as.factor(Cover_Type))) +
  geom_density(alpha=0.4) +
  labs(title="Elevation Density by Cover Type", x="", y="") +
  scale_fill_discrete(name="Cover Type")

ggplot(coverData, aes(Aspect, fill=as.factor(Cover_Type))) +
  geom_histogram(bins = 20) +
  coord_polar() +
  labs(title="Aspect by Cover Type", x="Aspect", y="") +
  scale_fill_discrete(name="Cover Type")

ggplot(coverData, aes(Wilderness_Area, fill=as.factor(Cover_Type))) +
  geom_bar(position = "dodge") +
  labs(title="Wilderness Area by Cover Type", x="Wilderness Area", y="Count") +
  scale_fill_discrete(name="Cover Type")

rm(coverData)
coverDataTrain <- data.table::fread("train.csv",header=T)
coverDataTest  <- data.table::fread("test.csv",header=T)
summary(coverDataTrain)
coverDataTrain <- coverDataTrain[,Soil_Type7:=NULL]
coverDataTrain <- coverDataTrain[,Soil_Type15:=NULL]


coverDataTrain[,12:54] <- lapply(coverDataTrain[,12:54], as.factor)
coverDataTest[,12:55] <- lapply(coverDataTest[,12:55], as.factor)

set.seed(123)
sample <- sample(2, nrow(coverDataTrain), replace = T, prob = c(0.8,0.2))
coverDataDev <- coverDataTrain[sample==1,]
coverDataVal <- coverDataTrain[sample==2,]


table(coverDataDev$Cover_Type)/nrow(coverDataDev)
table(coverDataVal$Cover_Type)/nrow(coverDataVal)

mod <- randomForest(Cover_Type ~ .-Id, data=coverDataDev, mtry=sqrt(ncol(coverDataDev)), ntree = 300, importance =T, do.trace=25)

print(mod)
importance(mod, type = 2)
par(mfrow=c(1,1))
varImpPlot(mod, type=1, main="Feature Importance", col="red", pch=20)
varImpPlot(mod, type=2, main="Feature Importance", col="steelblue", pch=20)
plot(mod)
coverDataVal$predictedCoverType <- predict(mod,coverDataVal)
confusionMatrix(data=coverDataVal$predictedCoverType,
                reference=coverDataVal$Cover_Type,
                positive='yes')

coverDataTest$CoverType <- predict(mod,coverDataTest)

outputran <- as.data.frame(cbind(Id = coverDataTest$Id, Cover_Type = coverDataTest$CoverType))
str(outputran)
summary(outputran)
write.csv(submission, "output.csv", row.names=FALSE)
#SVM
library(readr)

train1 <- read.csv("train.csv")
test1 <- read.csv("test.csv")


cat(sprintf("Training set has %d rows and %d columns\n", nrow(train1), ncol(train1)))
cat(sprintf("Test set has %d rows and %d columns\n", nrow(test1), ncol(test1)))


test1$Cover_Type<- NA
comb <- rbind(train1,test1)


comb$Soil <- NA
comb$Wilderness <- NA

comb$Soil <- apply(comb[grep("Soil_Ty+",colnames(comb))], 1 , function(x){which(x == 1)})
comb$Wilderness <- apply(comb[grep("Wilderness_Are+",colnames(comb))], 1 , function(x){which(x == 1)})
comb$Soil <- as.factor(comb$Soil)
comb$Wilderness <- as.factor(comb$Wilderness)
comb$Cover_Type <- as.factor(comb$Cover_Type)
comb[grep("Soil_Type+",colnames(comb))] <- NULL
comb[grep("Wilderness_Area+",colnames(comb))] <- NULL


train1 <- comb[1:15120,]
test1 <- comb[15121:581012,]


library(e1071)
library(forecast)
model2 <- svm(Cover_Type ~ ., train1[-1], cost = 64, epsilon = 0.01)
predictY <- predict(model2,train1[-1])
print(model2)
plot(predictY)
summary(model2)
confusionMatrix(train1$Cover_Type, predict(model2))
plot(outputR)
summary(outputR)
str(outputR)
outputR <- data.frame(Id = train1$Id, Cover_Type = predictY)
write.csv(outputR, file = "svmForestCover.csv", row.names = FALSE)

train <-read.csv("train.csv",sep=",",stringsAsFactors = TRUE)
dim(train)
class(train)
test <-read.csv("test.csv",sep=",",stringsAsFactors = TRUE)
str(test)
dim(test)
library(corrplot)
cor(train$Cover_Type, train)
Corr <-cor(train[1:13],method="pearson")
corrplot(Corr,method="circle")
library(reshape2)
library(ggplot2)
library(GGally)
plot<-ggcorr(coverData[,1:10,with=FALSE], label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE, hjust = 00.75, size = 3,layout.exp = 1)

fig_nums("corrTen","Correlation Matrix of 10 Numerical Features")


plot(train$Cover_Type ~., data = train)


index <- which(names(train)=="Cover_Type")
library(rpart)
library(caret)
library(corrplot)
fitrm <- rpart(Cover_Type ~., method = "class", data = train)
summary(fitrm)

printcp(fitrm) 
plotcp(fitrm) 

plot(fitrm, uniform = FALSE, main = "classification tree")
text(fitrm, all = TRUE, use.n = TRUE, cex = 0.8)


ctree <- prune(fitrm, 0.018364) 

plot(ctree, uniform = FALSE, main = "classification tree")
text(ctree, all = TRUE, use.n = TRUE, cex = 0.6)
summary(ctree)

pred = predict(ctree, train, type="class")
table(pred, train$Cover_Type)
base_accuracy <- mean(pred == train$Cover_Type)
data.frame(base_accuracy)
confusionMatrix(pred, train$Cover_Type)
outputree <- data.frame(Id = train1$Id, Cover_Type = pred)
str(outputree)
summary(outputree)








