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
dat <- read.csv("D:\\Courses\\data_mining - CSC522\\project\\data2\\TrainAndValid.csv")
appendix <- read.csv("../data/Machine_Appendix_edited.csv")

## Pull in the data from the machine appendix ##
merged <- merge(dat,appendix,by="MachineID",all.x=TRUE)
## Get rid of duplicated columns ##
old.cols <- grep("\\.x",names(merged))
merged <- merged[,-old.cols]
new.cols <- grep("\\.y",names(merged))
new.names <- unlist(lapply(names(merged)[new.cols],strsplit,'\\.'))[seq(1,13,2)]
names(merged)[new.cols] <- new.names

## Pull in the machineID, SalesID, and SalePrice ##
final.data <- cbind(merged[,1:3])
feature.map <- NULL
col.index <- 4
for(i in 4:ncol(merged)) {
  print(i)
  ## Parse the saledate column and break it up into day, month, year columns ##
  if(names(merged)[i] == "saledate") {
    print("Parsing saledate column...")
    date <- getDate(merged[,i])
    final.data <- cbind(final.data,date)
    col.index <- ncol(final.data) + 1
  } else {
    current.col <- merged[,i]   
    ## Test to see if it's factor, if it is convert it to an int representation ##
    if(is.factor(current.col)) {
       ## Set all missing values to NA ##
       missing.vals <- which(current.col == "")
       current.col[missing.vals] <- NA
       
       ## Remove the "" level from current.col ##
       current.col <- factor(current.col)
       
       ## Keep track of the mapping ##
       feature.name <- names(merged)[i]
       feature.map <- rbind(feature.map,cbind(feature.name,
                                              1:length(levels(current.col)),
                                              levels(current.col)))
         
       # Convert to an int #
       int.version <- match(current.col,levels(current.col))
       current.col <- int.version
       
    }
    final.data <- cbind(final.data,current.col)
    names(final.data)[col.index] <- names(merged)[i]
    col.index <- col.index + 1
  }
}

colnames(feature.map) <- c("Feature.Name","Integer.Version","Orignal.Version")
write.csv(final.data,"../data/combined.processed.data.csv",row.names=FALSE)
write.csv(feature.map,"../data/feature.map.csv",row.names=FALSE)

