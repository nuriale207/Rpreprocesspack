#' An S4 class to represent a Dataset
#'
#' @slot size Numeric value indicating the size of the dataset
#' @slot data List containing Attribute objects
#' @slot name Character with the name of the dataset
#'
setClass(Class="DataSet",
         slots=c("size"="numeric","data"="list","name"="character"),
         prototype=list("data"=list(), "size"=0,"name"="")
)

checkValidityDataSet <- function (object) {
  if(object@size!=length(object@data)){
    stop("The 'size' parameter is not equal to the length of the data")
  }
  else{
    return(TRUE)
  }
  return(TRUE)
}
setValidity(Class="DataSet", method=checkValidityDataSet)


#' Basic constructor of the Dataset class
#'
#' @description This function creates an object of class \code{\linkS4class{DataSet}}
#' @slot data List containing Attribute objects
#' @slot name Character with the name of the dataset
#' @return An object of class \code{\linkS4class{Dataset}}
#'
dataset<- function(data,name){
  if(missing(data)){
    if(missing(name)){
      dataset<-new (Class="DataSet",data=list())
    }
    else{
      dataset<-new (Class="DataSet",data=list(),name=name)
    }

  }
  else{
    if(missing(name)){
      dataset<-new (Class="DataSet",data=data,size=length(data))
    }
    else{
      dataset<-new (Class="DataSet",data=data,size=length(data),name=name)
    }
  }
  return(dataset)
}


#' Function to add an attribute class object to a DataSet
#'
#' @description This adds an attribute to a DataSet
#' @param dataset Object of class \code{\linkS4class{Dataset}}
#' @param attribute Object of class \code{\linkS4class{Attribute}} or vector to add to the dataset
#' @return The object of class \code{\linkS4class{Dataset}} with a new object of class \code{\linkS4class{Attribute}} on its' list
#'

setGeneric(name="addAttribute", def=function(dataset, attribute) standardGeneric("addAttribute"))

setMethod(f="addAttribute",
          signature="DataSet",
          definition=function (dataset, attribute) {
            if(class(attribute)!="Attribute"){
              attribute<-attribute(attribute)
            }
            dataset@data<-append(dataset@data,attribute)
            return(dataset(dataset@data,dataset@name))
          })


#' Function to normalize a DataSet
#'
#' @description This function normalizes a given DataSet
#' @param data a DataSet class vector
#' @return A normalized DataSet
#'

setMethod(f="normalize",
          signature = "DataSet",
          definition = function(x){
            normalized<-sapply(x@data,FUN=normalize)
            return(dataset(normalized,x@name))
          })



#' Function to standardize a given DataSet
#'
#' @description This function standardizes a given DataSet
#' @param data a DataSet class vector
#' @return A standardized DataSet
#'

setMethod(f="standardize",
          signature = "DataSet",
          definition = function(x){
            standardized<-sapply(x@data,FUN=standardize)
            return(dataset(standardized,x@name))
          })



#' Function to compute the variance of a given DataSet
#'
#' @description This function computes the variance of a given DataSet
#' @param data an Attribute class vector
#' @return A vector containing the variance of each column data
#'

setMethod(f="variance",
          signature = "DataSet",
          definition = function(x){
            return(sapply(x@data,FUN=variance))
          })



#' Function to compute the entropy of a given DataSet
#'
#' @description This function computes the entropy of a given DataSet
#' @param data a DataSet class vector
#' @return A vector containing the entropy of each column data if the data is discrete. NA otherwise
#'

setMethod(f="entropy",
          signature = 'DataSet',
          definition = function(x){
            vector<-sapply(x@data,FUN=entropy)
            return(vector)
          })



#' Function to discretize a given DataSet
#'
#' @description This function computes the dicretization of a given DataSet
#' @param data DataSet class vector
#' @param num.bins Numeric value indicating the number of intervals. Default: half the length of the data
#' @param type a Character indicating the type of discretization: Default "EW"(Equal Width) or "EF"(Equal Frequency)
#' @param columns Numeric vector indicating the columns in which the discretization must be applied. By default the discretization of every column will be computed
#' @return A vector containing the entropy of each column data if the data is discrete. NA otherwise
#'

setMethod(f="discretize",
          signature = "DataSet",
          definition = function(x,num.bins,type,columns){

            if(missing(num.bins)){
              num.bins<-length(x@data)/2
            }
            if(missing(type)|type!="EF"){
              type<-"EW"
            }
            if(missing(columns)){
              columns<-seq(1:length(x@data))
            }
            newData=list()
            discrList<-sapply(x@data[columns],FUN=discretize,num.bins=num.bins,type=type)
            x@data[columns]<-discrList
            return(x)

          })

#' @description This function returns the names of the columns of a given DataSet
#' @param data DataSet class vector
#' @return A vector containing the name of each column data.
#'

setGeneric(name="getNames",def=function(x) standardGeneric("getNames"))

setMethod(f="getNames",
          signature = "DataSet",
          definition = function(x){
            return(sapply(x@data,FUN=getName))
          })

#' Function to convert the DataSet into a Matrix
#'
#' @description This function returns a Matrix with the data of a given DataSet
#' @param data DataSet class vector
#' @return A matrix with the data of the DataSet.
#'
setGeneric(name="asMatrix",def=function(x) standardGeneric("asMatrix"))

setMethod(f="asMatrix",
          signature = "DataSet",
          definition = function(x){
            lst<-sapply(x@data,FUN=getVector)
            completeVector<- Reduce(c,lst)
            return(matrix(completeVector,ncol=length(x@data),byrow=FALSE))
          })

#' Function to print the DataSet
#'
#' @description This function prints the DataSet
#' @param data DataSet class vector
#' @return Shows the information of the DataSet.
#'
setMethod(f="show",
          signature = "DataSet",
          definition = function(dat){
            cat("S4 object of class 'DataSet'\n")
            cat(dat@name)
            cat('\n')
            attr<-dat@data[[1]]
            for(j in 1:dat@size){
              attr<-dat@data[[j]]
              cat(attr@name)
              cat('\t')
            }
            cat("\n")
            for(i in 1:attr@size){
              for(j in 1:dat@size){
                attr<-dat@data[[j]]
                cat(attr@vector[i])
                cat('\t')
              }
              cat("\n")
            }
          })

#' Function to compute the AUC-ROC of the DataSet
#'
#' @description This function computes the Area Under the Curve ROC of the DataSet
#' @param data DataSet class vector
#' @param vIndex index of the continuous variable to compute the AUC
#' @param classIndex index of the class
#' @return The value of the AUC-ROC
#'
#'

setGeneric(name="rocAuc",def=function(dat,vIndex,classIndex) standardGeneric("rocAuc"))

setMethod(f="rocAuc",
          signature = "DataSet",
          definition = function(dat,vIndex,classIndex){
            attr<-dat@data[[vIndex]]
            nValues<-length(attr@vector)
            class<-dat@data[[classIndex]]
            TPR<-c()
            FPR<-c()
            for(i in 1:nValues){
              predicciones<- rep(TRUE,nValues)
              predicciones<-attr@vector>=attr@vector[i]
              TP<-sum(class@vector&predicciones==TRUE)
              TN<-sum(!(class@vector|predicciones))
              FN<-sum((class@vector==1)&(predicciones==0))
              FP<-sum((class@vector==0)&(predicciones==1))
              TPR<-c(TPR,TP/(TP+FN))
              FPR<-c(FPR,FP/(FP+TN))

            }
            TPR[is.na(TPR)] <- 0
            FPR[is.na(FPR)] <- 0
            dFPR <- c(diff(FPR), 0)
            dTPR <- c(diff(TPR), 0)
            AUC<-sum(TPR * dFPR) + sum(dTPR * dFPR)/2
            return(AUC)
                     })



