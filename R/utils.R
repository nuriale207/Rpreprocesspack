#' Function to discretize a vector given a list of cutpoints
#'
#' @description This function dicretizes a vector given the list of points to cut
#' @param x a vector composed by real numbers
#' @param cut.points a list with the points at which the vector has to be cut
#' @return A factor with the discretization
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
#'
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

# vector <- c(11.5, 10.2, 1.2, 0.5, 5.3, 20.5)
# discretizeEF(vector,6)
#
# discretizeEW(vector,4)

#' Function to compute the entropy of a given vector
#'
#' @description This function computes the entropy of a given vector
#' @param x a vector composed by discrete variables
#' @return A real number
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


#' Function to compute the correlation between two vectors
#'
#' @description This function computes the correlation between two vectors
#' @param x a vector
#' @param y a vector
#' @param discretizationType
#' @param num.bins
#' @return A real number
#'
computeCorrelation<-function(x,y){
  if(class(x)!="factor" && class(y)=="factor"){
    x<-discretizeEW(y,length(y)/3)
  }
  if(class(y)!="factor" && class(x)=="factor"){
    y<-discretizeEW(x,length(x)/3)
  }
  if(class(y)=="factor" && class(x)=="factor"){
    return(multinformation(x,y)[0])
  }
  else{
    return(cor(x,y))
  }
}

#' Function to save a log text into a file
#'
#' @description This function saves the given text with the date in a file in the given path
#' @param text a character that contains the text to write
#' @param file.path a character with the path where the text has to be saved
#'

writeFile <- function (text, file.path) {
  # Open connection
  con <- file(description = file.path, open="w")

  # Check all rows are correct permutations
  completeText<-paste(Sys.time(),": ",text)
  writeLines(completeText, con=con)
  close(con)
}

#' Function to read a CSV file and save it into a DataSet
#'
#' @description This function loads a CSV file into a DataSet object
#' @param path path to the CSV file
#' @param header logical indicating if the first row of data corresponds to the names
#' @param sep the character separator of the data
#' @return A DataSet containing the data of the CSV file.
#'
#'
loadDataSet <- function(path,header,sep){
  datos <- read.csv(path,header =header, sep=sep )
  cols<-colnames(datos)
  data<-dataset()
  for(col in 1:dim(datos)[2]){
    attr<-attribute(datos[,col],name=cols[col])
    data<-addAttribute(data,attr)
  }
  return(data)
}

#' Function to write a CSV file with a DataSet information
#'
#' @description This function writes a CSV file with a DataSet information
#' @param data the DataSet object to save
#' @param file.path a character with the path where the DataSet has to be saved
#' @return It creates a file in the given file.path containing the data of the DataSet object.
#'
#'
writeDataSet <- function(data,file.path){
  matriz<-asMatrix(data)
  write.csv(matriz,file=file.path,sep=";")
}
# setwd("/media/nuria/Datos/KISA/SoftwareMatemáticoEstadístico/R")
# datos <- loadDataSet("temperat.csv",header =T, sep="," )
#
# writeDataSet(datos,"escrito.csv")
# vector <- c(11.5, 10.2, 1.2, 0.5, 5.3)
# x<-discretizeEW(vector,3)
# table(x)
# entropia<-entropy(x)
# entropia
