## Helper function to parse the SaleDate Column ##
getDate <- function(saleDateCol) {
  result <- matrix(0,nrow=length(saleDateCol),ncol=3)
  for(i in 1:length(saleDateCol)) {
    spt <- strsplit(as.character(saleDateCol[i]),'/')[[1]]
    result[i,1] <- as.numeric(as.character(spt[1]))
    result[i,2] <- as.numeric(as.character(spt[2]))
    result[i,3] <- as.numeric(as.character(strsplit(spt[3]," ")[[1]][1]))
  }
  
  colnames(result) <- c("SaleMonth","SaleDay","SaleYear")
  return(result)
}

## Read in the data ##
dat <- read.csv("../data/Train.csv")
valid <- read.csv("../data/Valid.csv")
appendix <- read.csv("../data/Machine_Appendix_edited.csv")

train.IDs <- dat$SalesID
valid.IDs <- valid$SalesID

## I stack the training data on top of the validation data ##
## This is so I have access to every possible level for a factor ##
stacked <- rbind(dat[,-2],valid)

## Pull in the data from the machine appendix ##
merged <- merge(stacked,appendix,by="MachineID",all.x=TRUE)
## Get rid of duplicated columns ##
old.cols <- grep("\\.x",names(merged))
merged <- merged[,-old.cols]
new.cols <- grep("\\.y",names(merged))
new.names <- unlist(lapply(names(merged)[new.cols],strsplit,'\\.'))[seq(1,13,2)]
names(merged)[new.cols] <- new.names

final.data <- cbind(stacked[,1:3])
feature.map <- NULL
for(i in 4:ncol(stacked)) {
  print(i)
  if(names(stacked)[i] == "saledate") {
    date <- getDate(stacked[,i])
    final.data <- cbind(final.data,date)
  } else {
    col <- stacked[,i]   
    if(names(stacked)[i] != "MachineHoursCurrentMeter" & names(stacked)[i] != "YearMade") {
      missing <- which(col == "" | is.na(col))
      col[missing] <- NA
      ## convert to integer ##
      int.version <- match(col,levels(factor(col)))
    
      ## save the mapping ##
      map <- cbind(levels(factor(col)),1:length(levels(factor(col))))
      map <- cbind(rep(names(stacked)[i],nrow(map)),map)
      feature.map <- rbind(feature.map,map)
  
      col <- int.version
    }
    final.data <- cbind(final.data,col)
    names(final.data)[ncol(final.data)] <- names(stacked)[i]
  }
}

SalePrice <- dat$SalePrice
train.processed <- cbind(SalePrice,final.data[match(train.IDs,final.data$SalesID),])
valid.processed <-  final.data[match(valid.IDs,final.data$SalesID),]

colnames(feature.map) <- c("Feature","Original.Name","Integer.Rep")

write.csv(train.processed,"../data/train.processed.csv",row.names=FALSE)
write.csv(valid.processed,"../data/valid.processed.csv",row.names=FALSE)
write.csv(feature.map,"../data/feature.map.csv",row.names=FALSE)
