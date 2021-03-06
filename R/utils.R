#' Function to discretize a vector given a list of cutpoints
#'
#' @description This function dicretizes a vector given the list of points to cut
#' @param x a vector composed by real numbers
#' @param cut.points a list with the points at which the vector has to be cut
#' @return A factor with the discretization
#' @example
#'discretizePoints(c(2,3,4,5,7),list(c(-Inf, 3.4), c(3.4, 5.6), c(5.6, Inf)))
#'
discretizePoints <- function(x,cut.points) {
  vCat<- c(1:length(x))
  for (i in 1:length(x)){
    for(j in 1:length(cut.points)){
      if(x[i]>= cut.points[[j]][1] & x[i] <= cut.points[[j]][2]){
        vCat[i] <- cut.points[j]
      }
    }
  }
  names(x) <- lapply(vCat,as.character)

  return (factor(vCat,cut.points))
}

#' Function to apply equal width discretization to a vector
#'
#' @description This function applies equal width discretization to a vector
#' @param x a vector composed by real numbers
#' @param num.bins number of intervals
#' @return A factor with the equal width discretization
#' @example
#'discretizeEW(c(3.5,6.7,2.4,7.8,1.2),3)
#'
discretizeEW<- function (x, num.bins) {
  width <- (max(x)-min(x))/num.bins
  nombresIntervalos <- paste("I",1:num.bins,sep="")
  puntosCorte<-list()
  limiteIn<- -Inf
  for (i in 1:num.bins){
    limiteFin <- min(x)+width*i
    if(i< num.bins){
      puntosCorte[[i]]<- c(limiteIn, limiteFin)
      limiteIn <- limiteFin
    }
    else{
      puntosCorte[[i]]<- c(limiteIn, Inf)
    }
  }
  names(puntosCorte)<- nombresIntervalos
  puntosCorte

  return (discretizePoints(x,puntosCorte))
}


#' Function to apply equal frequency discretization to a vector
#'
#' @description This function applies equal frequency discretization to a vector
#' @param x a vector composed by real numbers
#' @param num.bins number of intervals
#' @return A factor with the equal frequency discretization
#' @example
#' discretizeEF(c(3.5,6.7,2.4,7.8,1.2),6)

discretizeEF <- function(x,num.bins) {
  n<-length(x)
  numXIntervalo <- floor(n/num.bins)
  vectorOrdenado<- sort(x)
  puntosCorte <-list()
  limiteInf<--Inf
  limiteSup <- 0
  if(numXIntervalo>1){
    for(i in 1:n){
      if(i%%numXIntervalo==0 | i==n){
        limiteSup=vectorOrdenado[i]
        if(i==n){
          puntosCorte[[length(puntosCorte)+1]]<-c(limiteInf, Inf)
          limiteInf<-limiteSup
        }
        else{
          puntosCorte[[length(puntosCorte)+1]]<-c(limiteInf, limiteSup)
          limiteInf<-limiteSup
        }

      }
      else if(length(puntosCorte)==num.bins){
        break
      }
    }
  }
  else{
    for(i in 1:num.bins){
      limiteSup=vectorOrdenado[i]
      if(i==num.bins){
        limiteSup=vectorOrdenado[i]
        puntosCorte[[length(puntosCorte)+1]]<-c(limiteInf, Inf)
        limiteInf<-limiteSup

      }
      else{
        puntosCorte[[length(puntosCorte)+1]]<-c(limiteInf, limiteSup)
        limiteInf<-limiteSup

      }

    }

  }


  return (discretizePoints(x,puntosCorte))
}

#' Function to compute the entropy of a given vector
#'
#' @description This function computes the entropy of a given vector
#' @param x a vector composed by discrete variables
#' @param normaliza a boolean to tell if the function has to be normalized
#' @return A real number
#' @examples
#' computEntropy(c(2,3,4,5,6,7,7),FALSE)
#' computEntropy(c(2,3,4,5,6,7,7),TRUE)
#'
computEntropy <- function(x,normalize){
  if(missing(normalize)){
    normalize=FALSE
  }
  cuenta<- table(x)
  n<-(length(x))
  nCuenta<-length(cuenta)
  entropia<-0
  for(i in 1:nCuenta){
    probabilidad<-cuenta[i]/n
    if(probabilidad>0){
      entropia<- entropia+((-1)*probabilidad*(log2(probabilidad)))

    }
  }
  entropia<-unname(entropia)
  if(normalize){
    entropia<-entropia/length(cuenta)
  }
  return(entropia)
}


#' Function to compute the correlation between two objects of class \code{\linkS4class{Attribute}}
#'
#' @description This function computes the correlation between two objects of class \code{\linkS4class{Attribute}}
#' @param x an object of class \code{\linkS4class{Attribute}}
#' @param y an object of class \code{\linkS4class{Attribute}}
#' @param discretizationType if x and y are a mix of continuous and discrete variables a discretization is computed
#' in the continuous one. This parameter indicates the discretization type to compute: Equal Width "EW"
#' (default) or Equal Frequency "EF"
#' @param num.bins if x and y are a mix of continuous and discrete variables a discretization is computed
#' in the continuous one. This parameter indicates the number of intervals to use in the discretization. The default value is 3.
#' @return A real number with the correlation between both \code{\linkS4class{Attribute}}
#' @examples
#' attr1<-attribute(c(2.3,4.5,6.7,8.9))
#' attr2<-attribute(c(2,2,1,3))
#' computeCorrelation(attr1,attr2)
#' computeCorrelation(attr1,attr2,"EF")
#' computeCorrelation(attr1,attr2,"EF",4)
#'
computeCorrelation<-function(x,y,discretizationType,num.bins){
  library(infotheo)
  if(missing(discretizationType)){
    discretizationType="EW"
  }
  if(missing(num.bins)){
    num.bins=3
  }

  if(class(x@vector)!="factor" && class(y@vector)=="factor"){
    if(discretizationType=="EF"){
      x@vector<-discretizeEF(x@vector,num.bins)

    }
    else{
      x@vector<-discretizeEW(x@vector,num.bins)
    }
  }
  if(class(y@vector)!="factor" && class(x@vector)=="factor"){
    if(discretizationType=="EF"){
      y@vector<-discretizeEF(y@vector,num.bins)

    }
    else{
      y@vector<-discretizeEW(y@vector,num.bins)
    }
  }
  if(class(y@vector)=="factor" && class(x@vector)=="factor"){
    return(mutinformation(x@vector,y@vector))
  }
  else{
    return(cor(x@vector,y@vector))
  }
}

#' Function to save a log text into a file
#'
#' @description This function saves the given text with the actual time and date in a file in the given path
#' @param text a character that contains the text to write
#' @param file.path a character with the path where the text has to be saved

writeFile <- function (text, file.path) {
  # Open connection
  con <- file(description = file.path, open="w")

  # Check all rows are correct permutations
  completeText<-paste(Sys.time(),": ",text)
  writeLines(completeText, con=con)
  close(con)
}

#' Function to read a CSV file and save it into a \code{\linkS4class{DataSet}}
#'
#' @description This function loads a CSV file into a \code{\linkS4class{DataSet}} object
#' @param path a character that contains a path to the CSV file
#' @param header logical indicating if the first row of data corresponds to the names of the \code{\linkS4class{Attribute}}. Default value is FALSE.
#' @param sep the character separator of the data. Default value is ",".
#' @return A DataSet containing the data of the CSV file.

loadDataSet <- function(path,header,sep){
  if(missing(header)){
    header="FALSE"
  }
  if(missing(sep)){
    sep=","
  }
  datos <- read.csv(path,header =header, sep=sep )
  cols<-colnames(datos)
  data<-dataset()
  for(col in 1:dim(datos)[2]){
    attr<-attribute(datos[,col],name=cols[col])
    data<-addAttribute(data,attr)
  }
  return(data)
}

#' Function to write a CSV file with a \code{\linkS4class{DataSet}} information
#'
#' @description This function writes a CSV file with a \code{\linkS4class{DataSet}} information
#' @param data the \code{\linkS4class{DataSet}} object to save
#' @param file.path a character with the path where the \code{\linkS4class{DataSet}} has to be saved
#' @return It creates a file in the given file.path containing the data of the \code{\linkS4class{DataSet}} object.

writeDataSet <- function(data,file.path){
  matriz<-asMatrix(data)
  write.csv(matriz,file=file.path,sep=";")
}

