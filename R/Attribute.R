#' An S4 class to represent attributes
#'
#' @slot type Character value indicating the type of the vector: double, factor or numeric
#' @slot size Numeric value indicating the size of the vector
#' @slot vector Numeric vector containing the information of the \code{\linkS4class{Attribute}}
#' @slot name Character with the name of the \code{\linkS4class{Attribute}}

setClass(Class="Attribute",
         slots=c("type"="character", "size"="numeric","vector"="numeric","name"="character"),
         prototype=list("name"="")
)

checkValidityInstan <- function(object) {
  if (length(object@vector) != object@size) {
    stop("The 'size' parameter is not equal to the length of the vector")
  }
  if (object@type!="double"&object@type!="factor"&object@type!="numeric"&object@type!="integer"){
    stop("The 'vector' parameter must be double, factor, integer or numeric type")
  } else {
    if(class(object@vector)=="numeric" & object@type=="factor"){
      stop("The vector type doesn't match the type parameter")
    }
    else if(class(object@vector)!=object@type){
      stop("The vector type doesn't match the type parameter")
    }
    return(TRUE)
  }

}
setValidity(Class="Attribute", method=checkValidityInstan)


#' Basic constructor of the Attribute class
#'
#' @description This function creates an object of class \code{\linkS4class{Attribute}}
#' @param vector Numeric vector containing the information of the attribute
#' @param name Character indicating the name given to the attribute
#' @param discretize Logical optional value indicating if the attribute has to be discretized
#' @param discretizationType Character optional value "EW" indicates to compute equal width discretization. "EF" indicates to compute equal frequency discretization
#' @param num.bins Numeric number of intervals for discretization
#' @return An object of class \code{\linkS4class{Attribute}} that represents the attribute passed as an argument
#' @examples
#' attribute(c(2,56,87,32,15))
#' attribute(c(0,1,1,0,1,0))
#'
attribute <- function (vector,name,discretize, discretizationType,num.bins) {

  size <- length(vector)
  type <-class(vector)
  if(missing(name)){
    name<-""
  }
  if(type!="double"&type!="factor"&type!="numeric"&type!="integer"){
    stop("The vector must be double, factor, integer or numeric type")
  }
  if(missing(discretize)&missing(discretizationType)&missing(num.bins)){
    object <- new("Attribute", type=type, size=size,vector=vector,name=name)

  }
  else if(!missing(discretize) &(missing(discretizationType)|missing(num.bins))){
    stop("You must indicate the type of discretization and the number of intervals")
  }
  else if(missing(num.bins)){
    stop("You must indicate the type of discretization and the number of intervals")

  }
  else if(missing(discretizationType)){
    stop("You must indicate the type of discretization and the number of intervals")

  }
  else{
    if(discretizationType=="EF"){
      vector<-discretizeEF(vector,num.bins)
    }
    else{
      vector<-discretizeEW(vector,num.bins)
    }
    object <- new("Attribute", type=type, size=size,name=name,vector=vector)

  }
  return(object)
}


#' Function to apply discretization to an Attribute
#'
#' @description This function applies equal width discretization to an Attribute
#' @param x an attribute composed by real numbers
#' @param num.bins Numeric value indicating the number of intervals. Default: half the length of the data
#' @param type a Character indicating the type of discretization: Default "EW"(Equal Width) or "EF"(Equal Frequency)
#' @return An Attribute with the factor containing the equal width discretization
#'
setGeneric(name="discretize",def=function(x,num.bins,type,columns) standardGeneric("discretize"))


setMethod(f="discretize",
          signature="Attribute",
          definition=function (x,num.bins,type) {
            if(missing(type)|type!="EF"){
              type<-"EW"
            }
            if(type=="EW"){
              x@vector<-discretizeEW(x@vector,num.bins)
            }
            else{
              x@vector<-discretizeEF(x@vector,num.bins)
            }
            return(attribute(x@vector,x@name))
          })


#' Function to compute the entropy of a given Attribute
#'
#' @description This function computes the entropy of a given vector
#' @param x a vector composed by discrete variables
#' @return A real number
#' @examples
#' attr<-attribute(c(0,1,2,0,1,2))
#' attr<-discretize(attr,3,"EF")
#' entropy(attr)
#'
setGeneric(name="entropy", def=function(x) standardGeneric("entropy"))

setMethod(f="entropy",
          signature="Attribute",
          definition=function (x) {
            if(x@type!="factor" && x@type!="integer" ){
              return(NA)
            }
            else{
              return(computEntropy(x@vector,FALSE))
            }
          })




#' Function to normalize a given Attribute
#'
#' @description This function normalizes a given vector
#' @param x an Attribute class vector
#' @return A normalized attribute
#' @examples
#' attr<- attribute(c(2.3,5.8,4.3,3.2))
#' normalize(attr)
#'
setGeneric(name="normalize",def=function(x,columns) standardGeneric("normalize"))

setMethod(f="normalize",
          signature = "Attribute",
          definition = function(x){
            if(x@type!="factor"){
              mod<- sqrt(sum(x@vector^2))
              x@vector<-x@vector/mod
              return(attribute(x@vector,x@name))
            }
            else{
              return(x)
            }

          })



#' Function to standardize a given Attribute
#'
#' @description This function standardizes a given vector
#' @param x an Attribute class vector
#' @param columns columns that need to be standardized
#' @return A standardized attribute
#' @examples
#' attr<- attribute(c(2.3,5.8,4.3,3.2))
#' standardize(attr)
#'
setGeneric(name="standardize",def=function(x,columns) standardGeneric("standardize"))

setMethod(f="standardize",
          signature = "Attribute",
          definition = function(x){
            if(x@type!="factor"){
              mean<-mean(x@vector)
              desv<-sd(x@vector)
              x@vector<-x@vector-mean
              x@vector<-x@vector/desv
              return(attribute(x@vector,x@name))
            }
            else{
              return(x)
            }

          })

#' Function to compute the variance of a given Attribute
#'
#' @description This function computes the variance of a given Attribute
#' @param x an Attribute class vector
#' @return A real number containing the variance of the attribute
#' @examples
#' attr<- attribute(c(2.3,5.8,4.3,3.2))
#' variance(attr)
#'
setGeneric(name="variance",def=function(x) standardGeneric("variance"))

setMethod(f="variance",
          signature = "Attribute",
          definition = function(x){
            if(class(x@vector)!="factor"){

              return(var(x@vector))
            }
            else{
              return(NA)
            }
          })


#' Function to get the name of the Attribute
#'
#' @description This function returns the names of the columns of a given DataSet
#' @param x an Attribute class vector
#' @return A character containing the name of the attribute.
#' @examples
#' attr<- attribute(c(2.3,5.8,4.3,3.2),name="Mark")
#' getName(attr)
#'

setGeneric(name="getName",def=function(x) standardGeneric("getName"))

setMethod(f="getName",
          signature = "Attribute",
          definition = function(x){
            return(x@name)
          })


#' Function to get the vector of the Attribute
#'
#' @description This function returns the vector of a given Attribute
#' @param attr an Attribute class vector
#' @return A vector containing the information of the attribute.
#' @examples
#' attr<- attribute(c(2.3,5.8,4.3,3.2),name="Mark")
#' getVector(attr)
#'

setGeneric(name="getVector",def=function(attr) standardGeneric("getVector"))

setMethod(f="getVector",
          signature = "Attribute",
          definition = function(attr){
            return(attr@vector)
          })

#' Function to show the Attribute
#'
#' @description This function shows the Attribute
#' @param object Attribute class object
#' @return A character object with the information of the Attribute
#' @examples
#' attr<- attribute(c(2.3,5.8,4.3,3.2),name="Mark")
#' attr

setMethod(f="show",
          signature = "Attribute",
          definition = function(object){
            cat("S4 object of class 'Attribute'\n")

            cat(object@name)
            cat('\n')
            for(i in 1:object@size){
              cat(object@vector[i])
              cat('\n')
            }
          })





