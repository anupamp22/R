setwd('/home/anupam/python/miller-datascience/book1/Chapter_10')

library(maps)
library(mapproj)
library(spgwr)  # spatially-weighted regression
library(rpart)  # tree-structured modeling
library(randomForest)  # random forests
library(rpart.plot)  # plot tree-structured model information
library(lattice)  # statistical graphics
library(cvTools)  # cross-validation tools including rmspe

houses <- read.table("houses_data.txt", header=FALSE, sep = "",
                     dec = ".", row.names = NULL,
                     col.names = c("value","income","age","rooms","bedrooms",
                                   "pop","hh","latitude","longitude"))
houses$log_value <- log(houses$value)
houses$income_squared <- houses$income^2
houses$income_cubed <- houses$income^3
houses$log_age <- log(houses$age)
houses$log_pc_rooms <- log(houses$rooms/houses$pop)
houses$log_pc_bedrooms <- log(houses$bedrooms/houses$pop)
houses$log_pp_hh <- log(houses$pop/houses$hh)
houses$log_hh <- log(houses$hh)

simple.model <- {log_value~income+age+rooms+bedrooms+pop+hh}
pace.barry.model <- {log_value~income+income_squared+income_cubed+log_age+log_pc_rooms+log_pc_bedrooms+log_pp_hh+log_hh}
full.model <- {log_value~income+age+rooms+bedrooms+pop+hh+log_pc_rooms+log_pc_bedrooms+log_pp_hh}

BB.TOP <- 33
BB.BOTTOM <- 32
BB.RIGHT <- -116.75
BB.LEFT <- -125

houses$select <- ifelse(((houses$latitude < BB.TOP)),
                        ifelse((houses$longitude < BB.RIGHT),
                               ifelse((houses$latitude > BB.BOTTOM),
                                      ifelse((houses$longitude > BB.LEFT),1,2),2),2),2)

houses$select <- factor(houses$select,levels=c(1,2),labels = c("Selected", "Not Selected"))
houses.selected <- subset(houses, subset = (select=="Selected"))
houses.notselected <- subset(houses, subset = (select=="Not Selected"))

pdf(file = "fig_spatial_map_selected_region.pdf",width = 8.5,height = 8.5)
pointsize <- 0.5
map("state", region=c("california",project="albers",par=c(39,45)))
  points(mapproject(houses.selected$longitude,houses.selected$latitude,projection=""),pch=20,cex=pointsize,col="red")
  points(mapproject(houses.notselected$longitude,houses.notselected$latitude,projection=""),pch=20,cex=pointsize,col="blue")
  legend("right",legend = c("Selected Region","Not Selected"),col=c("red","blue"),pch=20)
  map.scale()
dev.off()

set.seed(4444)
partition <- sample(nrow(houses.selected))
houses.selected$Group <- ifelse(partition<nrow(houses.selected)/(3/2),1,2)
houses.selected$Group <- factor(houses.selected$Group,levels=c(1,2), labels = c("TRAIN","TEST"))
print(table(houses.selected$Group))
print(head(houses.selected))

houses.train <- subset(houses.selected, subset = (Group=="TRAIN"))
houses.test <- subset(houses.selected, subset = (Group=="TEST"))

names(houses.train)

houses.train.df.vars <- houses.train[,c("log_value","income","age","rooms","bedrooms","pop","hh","log_pc_rooms","log_pc_bedrooms","log_pp_hh")]
houses.train.cormat <- cor(as.matrix(houses.train.df.vars))
houses.train.cormat.line <- houses.train.cormat["log_value",]
ordered.houses.train.cormat <- houses.train.cormat[names(sort(houses.train.cormat.line, decreasing = TRUE)), names(sort(houses.train.cormat.line, decreasing = FALSE))]



# code to obtain default colors from ggplot2...
number.of.default.colors <- 2  # two end-points for our scale
end.point.colors <- hcl(h=seq(15, 375-360/number.of.default.colors,length=number.of.default.colors)%%360, c=100, l=65)
pdf(file="fig_spatial_correlation_heat_map.pdf",width = 11,height = 8.5)
x <- rep(1:nrow(ordered.houses.train.cormat),times=ncol(ordered.houses.train.cormat))
y <- NULL
for(i in 1:ncol(ordered.houses.train.cormat))
  y <- c(y,rep(i,times=nrow(ordered.houses.train.cormat)))
cortext <- sprintf("%0.3f", as.numeric(ordered.houses.train.cormat))
text.data.frame <- data.frame(x, y, cortext)
text.data.frame$cortext <- as.character(text.data.frame$cortext)
text.data.frame$cortext <- ifelse((text.data.frame$cortext == "1.000"),NA,text.data.frame$cortext)  # define diagonal cells as missing
text.data.frame <- na.omit(text.data.frame)  # diagonal cells have no text
print(levelplot(ordered.houses.train.cormat, cuts = 25, tick.number = 9,
                col.regions = 
                  colorRampPalette(c(end.point.colors[1], "white", end.point.colors[2])),
                scales=list(tck=0, x=list(rot=90)),
                xlab = "", 
                ylab = "",
                panel = function(...) {
                  panel.levelplot(...)  
                  panel.text(text.data.frame$x, text.data.frame$y, 
                             labels = text.data.frame$cortext)
                }))
dev.off() 











