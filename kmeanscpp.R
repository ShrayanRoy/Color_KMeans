rm(list = ls(all = T))  #removes all objects
library(imager)
library(Rcpp)
sourceCpp("colorfirst.cpp")

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

#--------------------------------------------------------------------------------------------------

#loadedimage <- load.image("C:/Users/User/Pictures/most-famous-paintings-2.jpg")
#loadedimage <- load.image("C:/Users/User/Pictures/bau.jpeg")
#loadedimage <- load.image("C:/Users/User/Pictures/Famous-Art-Portraits.jpg")
#loadedimage <- load.image("C:/Users/User/Pictures/pp3.jpg")
#loadedimage <- load.image("C:/Users/User/Pictures/durgapuja.jpeg")
loadedimage <- load.image("C:/Users/User/Pictures/indiagate.jpeg")

plot(loadedimage,axes = FALSE)
imageData <- as.array(loadedimage[, ,1, 1:3]) #extracted the three dimensional RGB array
imagedim <- dim(imageData)                 #dimension of the array
indexcombos <- as.matrix(data.frame(R = as.vector(imageData[,,1]),G = as.vector(imageData[,,2]),
                          B = as.vector(imageData[,,3]))) #considering all possible spatial indexes

kmeansppResults <- kmeansppC(indexcombos,k = 10)
clusterIndexCombo <- kmeansImageC(indexcombos,kmeansppResults)
outresults <- data.frame(clusterIndexCombo$indexcombos,
                clusterNumbers = clusterIndexCombo$clusterIndex)

findPalette(outresults,byprop = T)

#-------------------------------------------------------------------------------------------------

#loadedimage1 <- load.image("C:/Users/User/Pictures/img3.jpg")
#loadedimage2 <- load.image("C:/Users/User/Pictures/img4.jpg")
#loadedimage1 <- load.image("C:/Users/User/Pictures/pgli.jpeg")
#loadedimage1 <- load.image("C:/Users/User/Pictures/craw.png")
#loadedimage1 <-load.image("C:/Users/User/Pictures/smile.jpeg")
#loadedimage1 <- load.image("C:/Users/User/Pictures/snapeDumbledor.jpg")
loadedimage1 <- load.image("C:/Users/User/Pictures/snape.jpg")

plot(loadedimage1,axes = FALSE)
imageData1 <- as.array(loadedimage1[, ,1, 1:3]) #extracted the three dimensional RGB array
imagedim1 <- dim(imageData1)                 #dimension of the array
indexcombos1 <- as.matrix(data.frame(R = as.vector(imageData1[,,1]),G = as.vector(imageData1[,,2]),
                                    B = as.vector(imageData1[,,3]))) #considering all possible spatial indexes

kmeansppResults1 <- kmeansppC(indexcombos1,k = 5)
clusterIndexCombo1 <- kmeansImageC(indexcombos1,kmeansppResults1)
outresults1 <- data.frame(clusterIndexCombo1$indexcombos,
                         clusterNumbers = clusterIndexCombo1$clusterIndex)
findPalette(outresults1,byprop = T)

plot(loadedimage2,axes = FALSE)
imageData2 <- as.array(loadedimage2[, ,1, 1:3]) #extracted the three dimensional RGB array
imagedim2 <- dim(imageData2)                 #dimension of the array
indexcombos2 <- as.matrix(data.frame(R = as.vector(imageData2[,,1]),G = as.vector(imageData2[,,2]),
                                     B = as.vector(imageData2[,,3]))) #considering all possible spatial indexes

kmeansppResults2 <- kmeansppC(indexcombos2,k = 20)
clusterIndexCombo2 <- kmeansImageC(indexcombos2,kmeansppResults2)
outresults2 <- data.frame(clusterIndexCombo2$indexcombos,
                          clusterNumbers = clusterIndexCombo2$clusterIndex)
findPalette(outresults2,byprop = T)

#indx <- FindClusterIndexC(clusterIndexCombo1$clusterCenters,clusterIndexCombo2$clusterCenters)
img2col <- clusterIndexCombo1$clusterCenters

# img2col <- matrix(0,ncol = 3,nrow = 3)
# img2col[1,] <- clusterIndexCombo1$clusterCenters[1,]
# img2col[2,] <- clusterIndexCombo1$clusterCenters[2,]
# img2col[3,] <- clusterIndexCombo1$clusterCenters[2,]
Newimgmat <- ReplaceMat(indexcombos1,img2col,outresults1$clusterNumbers)

newimg <- array(0,dim = dim(imageData1))
newimg[,,1] <- matrix(Newimgmat[,1],byrow = F)
newimg[,,2] <- matrix(Newimgmat[,2],byrow = F)
newimg[,,3] <- matrix(Newimgmat[,3],byrow = F)
par(mfrow = c(1,2))
plot(loadedimage1,axes = F,main = "Original Image")
plot(as.cimg(newimg),axes = F,main = "KNN Constructed Image")
