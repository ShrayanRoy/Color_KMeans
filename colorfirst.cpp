#include <Rcpp.h>
#include <random>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
double euclideanDistance(NumericVector v1, NumericVector v2) {
  double distance = 0.0;
  int size = v1.size();
  for (int i = 0; i < size; i++) {
    double diff = v1[i] - v2[i];
    distance += diff * diff;
  }
  return distance;
}


// [[Rcpp::export]]
NumericMatrix kmeansppC(NumericMatrix indexcombos, int k) {
  
  int dimMat = indexcombos.nrow();
  
  // Vector of 1:(no_obs)
  NumericVector indices(dimMat);
  for (int n = 0; n < dimMat; n++) {
    indices[n] = n;
  }
  
  NumericMatrix clusterCenters(k, 3);
  
  //Initial Cluster Center
  int randomIndex = Rcpp::sample(indices, 1, true)[0];
  for (int l = 0; l < 3; l++) {
    clusterCenters(0, l) = indexcombos(randomIndex, l);
  }
  
  Rcout << "|--->";
  
  for (int indx = 1; indx < k; indx++) {
    
    NumericVector distance(dimMat); //Vector to store distances
    
    for (int obsnum = 0; obsnum < dimMat; obsnum++) {
      
      double d = 4.0,d1 = 0.0; // Distance cannot be more than 4
      
      for (int clustindex = 0; clustindex < indx; clustindex++) {
        
        d1 = euclideanDistance(indexcombos(obsnum, _), clusterCenters(clustindex, _));
        
        if (d1 < d) { //If current distance less than prev. distance
          d = d1;    
        }
        
      }
      distance[obsnum] = d; //store the minimum centroid distance
   }
    
    //Probability Vector proportional to D(x)^2
    NumericVector p = distance / sum(distance);
    
    //Selecting New Cluster
    int selectedIdx = Rcpp::sample(indices, 1, true, p)[0];
    for (int l = 0; l < 3; l++) {
      clusterCenters(indx, l) = indexcombos(selectedIdx, l);
    }
    
    Rcout << "|--->";
    
  }
  return clusterCenters;
}


//[[Rcpp::export]]
NumericVector FindClusterIndexC(NumericMatrix indexcombos,NumericMatrix clusterCenters){
  
  int dimMat = indexcombos.nrow();
  
  NumericVector clusterIndex(dimMat);
  
  for(int i = 0;i < dimMat;i++){
    
    double d = 4,d1 = 0; //Distance cannot be more than 4
    
    for(int j = 0; j < clusterCenters.nrow();j++){
      
       d1 = euclideanDistance(indexcombos(i,_),clusterCenters(j,_));
      
       if(d1 < d){
         clusterIndex[i] = (j+1);
         d = d1;
       }
       
     }
   }
  return(clusterIndex);
}

//[[Rcpp::export]]
List kmeansImageC(NumericMatrix indexcombos,NumericMatrix clusterCenters){
  
  int dimMat = indexcombos.nrow();
  
  NumericVector clusterIndex1 = FindClusterIndexC(indexcombos,clusterCenters);
  NumericMatrix clusterCentersNew(clusterCenters.nrow(),3);
  
  int adjustment = 1;
  
  while (adjustment != 0) {
    
    Rcout << "|---->";
    
    adjustment = 0;
    
    for(int clustindx = 0;clustindx < clusterCenters.nrow();clustindx++){
      
      double Rcoord = 0,Gcoord = 0,Bcoord = 0;
      int count = 0;
      
      for(int obsnum = 0;obsnum < dimMat;obsnum++){
        
        if(clusterIndex1[obsnum] == (clustindx + 1)){
          Rcoord += indexcombos(obsnum,0);
          Gcoord += indexcombos(obsnum,1);
          Bcoord += indexcombos(obsnum,2);
          count += 1;
        }
        
      }
      clusterCentersNew(clustindx,0) = Rcoord/count;
      clusterCentersNew(clustindx,1) = Gcoord/count;
      clusterCentersNew(clustindx,2) = Bcoord/count;
    }
    
    NumericVector clusterIndex2 = FindClusterIndexC(indexcombos,clusterCentersNew);
    
    for(int i = 0;i < clusterIndex1.length();i++){
      if(clusterIndex1[i] != clusterIndex2[i]){
        adjustment = 1;
        break;
      }
    }
    clusterIndex1 = clusterIndex2; 
  }
  
  List returnlist = List::create(Named("clusterCenters") = clusterCentersNew,Named("clusterIndex") = clusterIndex1,Named("indexcombos") = indexcombos);
  return(returnlist);
}

//[[Rcpp::export]]
NumericMatrix ReplaceMat(NumericMatrix indexcombos,NumericMatrix colors,NumericVector clusterIndex){
  
  for(int i = 0;i < indexcombos.nrow();i++){
   for(int j = 0;j < colors.nrow();j++){
     if((j+1) == clusterIndex[i]){
       for(int k = 0;k < 3;k++){
         indexcombos(i,k) = colors(j,k);
       }
     }
    } 
  }
  return indexcombos;
}










