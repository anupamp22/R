.libPaths(c(.libPaths(), "/usr/lib/R/site-library"))
.libPaths(c(.libPaths(), "/usr/lib/R/library"))

row.has.na <- apply(final, 1, function(x){any(is.na(x))})
sum(row.has.na)
final.filtered <- final[!row.has.na,]

tapply(flights$dep_delay,flights$origin,FUN=length)
aggregate(flights$dep_delay~flights$carrier,data=flights,FUN = "mean")
ddply(flights,'origin',function(x) c(count=nrow(x), mean=mean(x$dep_delay)), .progress = "text")


library(AppliedPredictiveModeling)
data(segmentationOriginal)
segData <- subset(segmentationOriginal, Case=='Train')
head(segData)
cellId <- segData$Cell
class <- segData$Class
segData <- segData[,-(1:3)]
grep("Status",names(segData))
statusColumnNum <- grep("Status",names(segData))
segData <- segData[,-statusColumnNum]

library(e1071)
skewness(segData$AngleCh1)
skewValues <- apply(segData, 2, skewness)
head(skewValues)

library(caret)
ch1AreaTrans <- BoxCoxTrans(segData$AreaCh1)
head(segData$AreaCh1)
predict(ch1AreaTrans, segData$AreaCh1)
pcaObject <- prcomp(segData, center = TRUE, scale. = TRUE)
percentVariance <- pcaObject$sdev^2/sum(pcaObject$sdev^2)*100
percentVariance[1:3]
names(pcaObject)
head(pcaObject$x[,1:5])
head(pcaObject$rotation[,1:3])
trans <- preProcess(segData,method = c('BoxCox','center','scale','pca'))
transformed <- predict(trans,segData)
head(transformed[,1:5])
nearZeroVar(segData)

correlations <- cor(segData)
dim(correlations)
correlations[1:4,1:4]

library(corrplot)
corrplot(correlations,order='hclust')
highCor <- findCorrelation(correlations,cutoff = 0.75)
length(highCor)
head(highCor)
filteredSegData <- segData[,-highCor]

data("cars")
names(cars)
cars$Type[cars$sedan==1] <- "sedan"
cars$Type[cars$convertible==1] <- "convertible"
cars$Type[cars$coupe==1] <- "coupe"
cars$Type[cars$hatchback==1] <- "hatchback"
cars$Type[cars$wagon==1] <- "wagon"

carsubset <- subset(cars[c('Price','Mileage','Type')],!is.na(cars$Type))
simpleMod <- dummyVars(~Mileage+Type, data=carsubset,levelsOnly=TRUE)
predict(simpleMod,head(carsubset))
withInteraction <- dummyVars(~Mileage+Type+Mileage:Type, data=carsubset,levelsOnly=TRUE)
predict(withInteraction,head(carsubset))


data(FuelEconomy)
names(cars2010)
cars2010 <- cars2010[order(cars2010$EngDispl),]
cars2011 <- cars2011[order(cars2010$EngDispl),]
cars2010a <- cars2010
cars2011a <- cars2011
cars2010a$Year <- "2010 Model Year"
cars2011a$Year <- "2011 Model Year"
plotData <- rbind(cars2010a,cars2011a)
library(lattice)
xyplot(FE~EngDispl|Year, plotData,xlab="Engine Displacement", ylab = "Fuel Efficiency", between=list(x=1.2))
library(caret)
set.seed(1)
lm1Fit <- train(FE ~ EngDispl, data = cars2010,method = "lm", trControl = trainControl(method= "cv"))
lm1Fit
set.seed(1)
cars2010$ED2 <- cars2010$EngDispl^2
cars2011$ED2 <- cars2011$EngDispl^2
lm2Fit <- train(FE ~ EngDispl+ED2, data = cars2010,method = "lm", trControl = trainControl(method= "cv"))
lm2Fit
set.seed(1)
library(earth)
marsFit <- train(FE ~ EngDispl, data = cars2010,method = "earth", trControl = trainControl(method= "cv"))
marsFit
plot(marsFit)
cars2011$lm1 <- predict(lm1Fit,cars2011)
is.na(cars2011$lm1)
cars2011$lm2 <- predict(lm2Fit,cars2011)
cars2011$mars <- predict(marsFit,cars2011)

postResample(pred = cars2011$lm1,  obs = cars2011$FE)

