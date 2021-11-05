#' Function to visualize the entropy of a given dataset
#'
#' @description This function estimates the entropy from a DataSet instances and visualizes them
#' @param dataset object of class \code{\linkS4class{DataSet}}
#' @return An object of class ggplot with the visualization
#' @examples
#'TODO
#'
setGeneric(name="entropyPlot",def=function(x) standardGeneric("entropyPlot"))
setMethod(f="entropyPlot",
          signature = "DataSet",
          entropyPlot<- function(x){
            library(ggplot2)
            entropia<-entropy(x)
            names(entropia)<-getNames(x)
            entropia = entropia[!is.na(entropia)]
            names<-names(entropia)
            datos<-data.frame(Variables=names,Entropy=entropia)
            p<-ggplot(data=datos, aes(x=Variables, y=Entropy)) +
              geom_bar(stat="identity", width=0.75,fill=rgb(0.1,0.4,0.5,0.7))
            p

          })

#' Function to visualize the correlation of a given dataset
#'
#' @description This function estimates the correlation from a DataSet instances and visualizes them
#' @param dataset object of class \code{\linkS4class{DataSet}}
#' @return An object of class ggplot with the visualization
#' @examples
#'TODO
#'
setGeneric(name="correlationPlot",def=function(x) standardGeneric("correlationPlot"))
setMethod(f="correlationPlot",
          signature = "DataSet",
          correlationPlot<- function(x){
            library(reshape2)
            library(ggplot2)

            df = melt(cor(asMatrix(x)))
            df
            #colnames(df)<-getNames(data)
            colnames(df)<-c("V1","V2","Correlation")

            df<-as.data.frame(df)

            ggplot(data=df, aes(x = V1, y = V2, fill = Correlation)) + geom_tile()+
              scale_fill_gradient(low = "white", high = "steelblue")
          })


#' Function to visualize the ROC curve of a given DataSet
#'
#' @description This function visualizes the Curve ROC of the DataSet
#' @param data DataSet class vector
#' @param vIndex index of the continuous variable to compute the AUC
#' @param classIndex index of the class
#' @return A plot with the curve
#'
#'

setGeneric(name="rocPlot",def=function(dat,vIndex,classIndex) standardGeneric("rocPlot"))

setMethod(f="rocPlot",
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
              plot(TPR,FPR,type="b")
            }

          })


