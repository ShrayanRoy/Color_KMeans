rm(list = ls(all = T))  #removes all objects
library(imager) # loaded the required package

Kmeanspp <- function(imageData,k){
  imagedim <- dim(imageData)                 #dimension of the array
  indexcombos <- data.frame(expand.grid(1:imagedim[1],1:imagedim[2]),
                            R = as.vector(imageData[,,1]),G = as.vector(imageData[,,2]),
                            B = as.vector(imageData[,,3])) #considering all possible spatial indexes
  clusterCenters <- NULL;clusterCentersIndex <- NULL
  clusterCentersIndex <- c(clusterCentersIndex,sample(1:nrow(indexcombos),size = 1))
  initCenter <- indexcombos[clusterCentersIndex[1],-c(1,2)] 
  clusterCenters <- rbind(clusterCenters,initCenter)  #initial randomly chosen cluster
  cat("|--->")
  
  for(clustnum in 1:(k-1)){
    distances <- apply(indexcombos[-clusterCentersIndex,],1,FUN = function(coord){
      min(apply(t(clusterCenters) - coord[-c(1,2)],2,
                FUN = function(y){sum(y^2)}))
    })  #calculating the minimum distance between the chosen cluster centers and the point
    chosenindex <- sample(rownames(indexcombos[-clusterCentersIndex,]),size = 1,prob = distances/sum(distances))
    clusterCentersIndex <- c(clusterCentersIndex,as.integer(chosenindex))
    clusterCenters <- rbind(clusterCenters,indexcombos[chosenindex,-c(1,2)])
    cat("|--->")
  }
 return(list(indexcombos = indexcombos,clusterCenters = clusterCenters))  
}

FindClusterIndex <- function(clusterCenters,indexcombos){
  clusterNumbers <- apply(indexcombos,1,FUN = function(coord){
    distances <- apply(t(clusterCenters) - coord[-c(1,2)],2,
                       FUN = function(y){sum(y^2)})
    return(which.min(distances))
  })
  return(data.frame(indexcombos,clusterNumbers))
}

kmeansImage <- function(clusterCenters,indexcombos){

  #Find Initial clusters
  clusterIndexCombo <- FindClusterIndex(clusterCenters,indexcombos)
  cat("|--->")
  adjustment = 1
  while(adjustment != 0){
    index1 <- clusterIndexCombo$clusterNumbers
    
    newclusterCenters <- aggregate(clusterIndexCombo[,c("R","G","B")],by = 
                                     list(clusterIndexCombo$clusterNumbers),FUN = mean)[,-1]
    
    clusterIndexCombo <- FindClusterIndex(newclusterCenters,indexcombos)
    
    index2 <- clusterIndexCombo$clusterNumbers
    
    if(isTRUE(all.equal(index1,index2))){
      adjustment = 0
    }
    cat("|--->")
  }
  return(clusterIndexCombo)

}


findPalette <- function(clusterIndexCombo,byprop = TRUE){
  
  cols <- floor(aggregate(clusterIndexCombo[,c("R","G","B")],by = 
                    list(clusterIndexCombo$clusterNumbers),FUN = mean)[,-1]*255)/255
  colvec <- apply(cols,1,FUN = function(x) {rgb(x[1],x[2],x[3])})
  colproptrue <- prop.table(table(clusterIndexCombo$clusterNumbers))
  if(byprop){
    barplot(matrix(sort(colproptrue,decreasing = TRUE),byrow = T),border = NA,
            horiz = T,axes = F,col = colvec[order(colproptrue,decreasing = TRUE)])
  }else{
    colpropfalse <- rep(1/length(colvec),length(colvec))
    barplot(matrix(sort(colpropfalse,decreasing = TRUE),byrow = T),border = NA,
            horiz = T,axes = F,col = colvec[order(colproptrue,decreasing = TRUE)])
  }
  return(colvec)
  
}

#loaded the image
#loadedimage <- load.image("C:/Users/User/Pictures/4ocbtgyvq7_XL_279141.jpg")
#loadedimage <- load.image("C:/Users/User/Pictures/Famous-Art-Portraits.jpg")
#loadedimage <- load.image("C:/Users/User/Pictures/exam1.jpg")
#loadedimage <- load.image("C:/Users/User/Pictures/van_gogh_img130.jpeg")
#loadedimage <- load.image("C:/Users/User/Pictures/bau.jpeg")
#loadedimage <- load.image("C:/Users/User/Pictures/craw.png")
#loadedimage <- load.image("C:/Users/User/Pictures/scan.jpeg")
#loadedimage2 <- load.image("C:/Users/User/Pictures/pp3.jpg")
loadedimage <- load.image("C:/Users/User/Pictures/most-famous-paintings-2.jpg")

imageData <- as.array(loadedimage[, ,1, 1:3]) #extracted the three dimensional RGB array
kmeansppResults <- Kmeanspp(imageData,k = 10)
indexcombos <- kmeansppResults[[1]]
clusterCenters  <- kmeansppResults[[2]]
clusterIndexCombo <- kmeansImage(clusterCenters,indexcombos)
findPalette(clusterIndexCombo,byprop = T)

clusterIndexCombo1 <- clusterIndexCombo

cols <- floor(aggregate(clusterIndexCombo[,c("R","G","B")],by = 
                          list(clusterIndexCombo$clusterNumbers),FUN = mean)[,-1]*255)/255

clusterIndexCombo1[clusterIndexCombo$clusterNumbers == 1,c("R","G","B")] = cols[1,] 
clusterIndexCombo1[clusterIndexCombo$clusterNumbers == 2,c("R","G","B")] = cols[2,]
clusterIndexCombo1[clusterIndexCombo$clusterNumbers == 3,c("R","G","B")] = cols[3,]
clusterIndexCombo1[clusterIndexCombo$clusterNumbers == 4,c("R","G","B")] = cols[4,] 
clusterIndexCombo1[clusterIndexCombo$clusterNumbers == 5,c("R","G","B")] = cols[5,]

newimg <- array(0,dim = dim(imageData))
newimg[,,1] <- matrix(clusterIndexCombo1$R,byrow = F)
newimg[,,2] <- matrix(clusterIndexCombo1$G,byrow = F)
newimg[,,3] <- matrix(clusterIndexCombo1$B,byrow = F)

par(mfrow = c(1,2))
plot(loadedimage,axes = F)
plot(as.cimg(newimg),axes = F)
