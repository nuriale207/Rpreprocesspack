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
          definition = function(x,columns){
            if(missing(columns)){
              columns<-seq(1:length(x@data))
            }
            normalized<-sapply(x@data[columns],FUN=normalize)
            x@data[columns]<-normalized
            return(x)
          })



#' Function to standardize a given DataSet
#'
#' @description This function standardizes a given DataSet
#' @param data a DataSet class vector
#' @return A standardized DataSet
#'

setMethod(f="standardize",
          signature = "DataSet",
          definition = function(x,columns){
            if(missing(columns)){
              columns<-seq(1:length(x@data))
            }
            standardized<-sapply(x@data[columns],FUN=standardize)
            x@data[columns]<-standardized
            return(x)
          })



#' Function to compute the variance of a given DataSet
#'
#' @description This function computes the variance of a given DataSet
#' @param x a DataSet Object
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
          definition = function(object){
            cat("S4 object of class 'DataSet'\n")
            cat(object@name)
            cat('\n')
            greater<-FALSE
            attr<-object@data[[1]]
            for(j in 1:object@size){
              attr<-object@data[[j]]
              name<-attr@name
              # if(nchar(attr@name)>4){
              #   greater<-TRUE
              # }
              if(nchar(attr@name)>8){
                name<-strtrim(name, 8)
              }
              else{
                cuantos<-8-nchar(attr@name)
                name<-paste(name,paste(rep(" ",cuantos),collapse=""),sep="",collapse="")
              }

              cat(name)
              cat('\t')
              # if(greater==TRUE){
              #   cat('\t')
              #   #cat('\t')
              # }
              # else{
              #   cat('\t')
              # }

            }
            cat("\n")
            for(i in 1:attr@size){
              for(j in 1:object@size){
                attr<-object@data[[j]]
                cat(attr@vector[i])
                cat('\t')
                cat('\t')
              }
              cat("\n")
            }
          })

#' Function to compute the correlation matrix between the Attribute pairs of the dataset
#'
#' @description This function returns the correlation matrix between the Attribute pairs of the DataSet
#' @param data DataSet class vector
#' @return A matrix containing the correlation between attribute pairs.
#'
setGeneric(name="correlation", def=function(x,discretizationType,num.bins) standardGeneric("correlation"))
setMethod(f="correlation",
          signature = "DataSet",
          definition = function(x,discretizationType,num.bins){
              if(missing(discretizationType)){
                discretizationType="EW"
              }
              if(missing(num.bins)){
                num.bins=3
              }
              correlation=matrix(rep(0,length(x@data)**2),ncol = length(x@data))
              for(i in seq(1,length(x@data))){
                  v1<-x@data[[i]]

                  for(j in seq(i,length(x@data))){
                    v2<-x@data[[j]]
                    valor<-computeCorrelation(v1,v2)
                    correlation[i,j]<-valor
                    correlation[j,i]<-valor
                  }
              }

              return(correlation)

          })

#' Function to filter the dataset attribute
#'
#' @description This function returns the filtered DataSet without the unnecessary attributes
#' @param x DataSet class vector
#' @param FUN function by which filter the data, it has to return a value per attribute. FUN=correlation by default
#' @param threshold numeric value that indicates the limit from which to remove the attribute
#' @param inverse If TRUE the attribute has to be below the threshold to remove ir.By default FALSE
#' @return A DataSet without the filtered attributes
#'
setGeneric(name="filter",def=function(x,threshold,FUN,inverse) standardGeneric("filter"))
setMethod(f="filter",
          signature="DataSet",
          definition=function(x,threshold,FUN,inverse){
            if(missing(inverse)){
              inverse=FALSE
            }
              if(missing(FUN)){
                cor<-correlation(x)
                elimino<-c()
                for(i in seq(1,length(x@data))){
                  for(j in seq(i,length(x@data))){
                    if(i!=j && cor[i,j]>=threshold && inverse==FALSE){
                       elimino<-c(elimino,i)
                    }
                    else if(i!=j && cor[i,j]<threshold && inverse==TRUE){
                       elimino<-c(elimino,i)
                    }
                  }
                }
                data<-x@data[-elimino]
              }
              else{
                result<-sapply(x@data,FUN=FUN)
                #print(result)
                if(inverse==TRUE){
                  result[is.na(result)] <- threshold+0.1

                  filter<- result>threshold
                }
                else{
                  result[is.na(result)] <- threshold-0.1

                  filter<- result<threshold
                  #print(filter)

                }
                data<-x@data[filter]
                #print(data)
              }


             return(dataset(data,x@name))


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

            orderedData <- data.frame(attr@vector,class@vector)
            orderedData<-orderedData[order(orderedData$attr),]

            attr<-orderedData$attr
            class<-orderedData$class

            if(class(attr)!="factor"){
              TPR<-c()
              FPR<-c()
              for(i in 1:nValues){
                predicciones<- rep(TRUE,nValues)
                predicciones<-attr>=attr[i]
                TP<-sum(class&predicciones==1)
                TN<-sum(!(class|predicciones))
                FN<-sum((class==1)&(predicciones==0))
                FP<-sum((class==0)&(predicciones==1))
                TPR<-c(TPR,TP/(TP+FN))
                FPR<-c(FPR,FP/(FP+TN))

              }
              TPR[is.na(TPR)] <- 0
              FPR[is.na(FPR)] <- 0
              dFPR <- c(diff(FPR), 0)
              dTPR <- c(diff(TPR), 0)
              AUC<-sum(TPR * dFPR) + sum(dTPR * dFPR)/2
              return(AUC)
            }

                     })



