spd4testing()

spd4testing(missingY=TRUE,missingX=TRUE)

spd4testing(N=6,G=3,Y=4)

\dontrun{
  spd4testing(N=6,G=4,Y=4) }

library(plm)
d <- spd4testing(missingY=FALSE,missingX=TRUE)
e<-plm(y ~ x + factor(year)*factor(gid), data=d, model="fd")
summary(e)


citation("spd4testing")
