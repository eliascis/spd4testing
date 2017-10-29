#' @title Small Panel Data Set for Testing
#'
#' @description spd4testing creates a panel data set with individual observations over time that belong to different groups.
#' Individuals are coded with the variable "id". The time variable is given by "year". The group identifier is "gid".
#' The data consits of one dependent variable "y", and two explanatory variables "x" and "u".
#' In addtion spd4testing adds a constant factor "z" equal to one and weights "w" with values 1 or 0.5.
#' To simulate a more complex data sttructure it is possible to randomly create missing values in "y" and/ or "x".
#' @param N Number of individuals, id
#' @param G Number of groups, gid
#' @param Y Number of years, year
#' @param missingY create missing outcome values for one observation in dataset
#' @param missingX create missing explanatory values for one observation in dataset
#' @import stats
#' @import plm
#' @details
#' N*Y observations. \cr
#' G is the number of groups. \cr
#' Each group has the same size: N/G. \cr
#' N/G must be a whole number.
#' @return data.frame
#' @author Elías Cisneros <ec@elias-cisneros.de>
#' @example man/eg.spd4testing.R
#' @export

#lets test


spd4testing<-function(
  N=5,
  G=2,
  Y=5,
  missingY=FALSE,
  missingX=FALSE,
  missing.pre.years=TRUE
){

  N=6
  G=2
  Y=5
  missingY=T
  missingX=T
  missing.pre.years=T

  #check if numbers add up
  if ((N/G)!=round(N/G)){
    stop("Too many or too few groups for the amout of individuals - N/G must be an integer")
  }
  # (N/G*Y==N*Y)

  #data structure
  d<-data.frame(
    id=rep(1:N,each=Y),
    year=rep(1:Y,N)+2000,
    gid=rep(1:G,each=Y*(N/G))
  )

  #weights
  d[,"w"]<-rep(c(1,0.5,sample(c(1,1,1,0.5),(N-2),replace=T)),each=Y)

  #error
  d[,"e"]<-rnorm(N*Y,0,1)

  #explanatory
  d[,"x"]<-rnorm(N*Y,0,1)
  d[,"z"]<-1
  d[,"u"]<-2*(d[,"x"]+rnorm(N*Y,0,10)) + 3*(d[,"e"] + rnorm(N*Y,0,10))

  #outcome
  coef.x<-2
  coef.u<-5
  d[,"y"] = coef.x*d[,"x"] + coef.u*d[,"u"] + d[,"e"]

  #missing years in first two ids
  if (missing.pre.years==T){
    i<-which(d$id==1 & d$year %in% c(2001,2002))
    ii<-which(d$id==2 & d$year %in% c(2001))
    iii<-c(i,ii)
    d<-d[-iii,]
  }

  #missing outcome values
  if (missingY==T){
    s<-3:N
    i<-sample(s,1)
    d[d$id==i,"y"]<-NA
  }

  #missing explanatory values
  if (missingX==T){
    s<-3:N
    if(missingY==T){
      s<-s[s!=i]
    }
    ii<-sample(s,1)
    d[d$id==ii,"x"]<-NA
  }
  #out
  return(d)
}
