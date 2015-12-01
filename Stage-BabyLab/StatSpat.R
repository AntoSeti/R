library(plotrix)
library(geoR)
library(fields)
library(maps)
library(spatstat)
source("http://www.math.ntnu.no/inla/givemeINLA.R") # Package INLA

# Single Index Method en R
# Implémentation d'une fonction pour calculer log-likelihood.est(theta,x)=ls.est(teta,x) for a ppp (3.12)
# Nécessite le calcul de G(t(teta)x), G fonction inconnue, t(teta)x called an index
# Pour cela, utilisation de la fonction rhohat.
# Nécessite également les poids wi : fonction quadscheme

# Pour calculer theta.est=argmax(ls.est(teta,x))
# Besoin de la fonction optim
# Initial values : simulation d'une ppm (maximum likelihood using log-linear model)
# Option : paramètres pour fonction rhohat, "ratio","reweight","transform"

# Fonction finale utilisant :
# - les valeurs initiales obtenues par ppm
# - theta.est obtenue par optim
# - combinaison des covariables en utilisant theta.est
# - Faire une estimation non paramétrique avec rhohat utilisant la combinaison des covariables

# Dataset Beilschmiedia
data(bei)

##Load and examine the data
data(bei)
plot(bei)
##extract elevation and gradient
elevation <- bei.extra$elev$v
gradient <- bei.extra$grad$v
## compute counts in each grid-cell
counts <- matrix(0,dim(gradient)[1],dim(gradient)[2] )
##size of each grid cell
dx <- (bei.extra$elev$xcol[2] - bei.extra$elev$xcol[1])/2
dy <- (bei.extra$elev$yrow[2] - bei.extra$elev$yrow[1])/2
##loop over the grid cells, and count points
for(i in 1:dim(counts)[1]){
    Y <- bei.extra$grad$yrow[i]
    for(j in 1:dim(counts)[2]){
       X <- bei.extra$grad$xcol[j]
       counts[i,j] <- sum( (X-dx)<bei$x & bei$x<=(X+dy) & (Y-dy)<bei$y & bei$y<=(Y+dx) )
    }
}
# Let's plot the resulting gridded data,
par(mfrow=c(2,2))
image.plot(elevation)
image.plot(gradient)
image.plot(counts)
# and collect the data into a suitable data-frame
bei <- data.frame(counts=c(counts),
       x=rep(bei.extra$elev$xcol, each=length(bei.extra$elev$yrow)),
       y=rep(bei.extra$elev$yrow, length(bei.extra$elev$xcol)),
       elevation=c(elevation), gradient=c(gradient))

# Questions : Différences entre :
image(bei.extra$grad)
image.plot(bei.extra$grad$v) # package fields
# Pourquoi ?

# Package INLA (Constrained Delaunay Triangulation)
mesh.lattice <- inla.mesh.lattice(x=bei.extra$elev$xcol,
                                  y=bei.extra$elev$yrow)
mesh.lattice <- inla.mesh.create(lattice=mesh.lattice)
plot(mesh.lattice)
summary(mesh.lattice)

loc <- as.matrix(bei[,c("x","y")])
mesh <- inla.mesh.create.helper(points.domain=loc,
                                  offset = c(-0.05, -0.2),
                                  n=c(8,16), max.edge = c(25,150))
summary(mesh)
##plot
plot(mesh)
##add the points for reference (note the possible aliasing effect)
points(loc[,1], loc[,2], col="red", pch=19, cex=.1)

# PARTIE TP JF
####################################################################################
## 2. Intensity estimation of inhomogeneous spatial point processes
###################################################################################

## dataset bei: is this point process stationary or Poisson?

bei
par(mfrow=c(1,2))
plot(bei,cex=.4)
quadrat.test(bei,nx=3)->q;plot(q,lwd=2,col='red');points(bei,cex=.1);q

## non-parametric estimation of the intensity

density(bei)->dbei
par(mfrow=c(1,1));plot(dbei);points(bei,cex=.1);q

## Plot with extra covariates: elevation field and slope of elevation.

image(bei.extra$elev);points(bei,pch=3,cex=.4)
image(bei.extra$grad);points(bei,pch=3,cex=.4)

## Parametric estimation of the intensity: log-linear model for the intensity
## Estimation using the "Poisson likelihood" contrast function (by default)

ppm(bei,~elev+grad,covariates=list(elev=bei.extra$elev,grad=bei.extra$grad),nd=200)->ppmbei
ppmbei

par(mfrow=c(1,2))
plot(ppmbei,ngrid=c(128,128),se=FALSE,superimpose=FALSE,pause=FALSE)->trend
points(bei,pch=3,cex=.4)

plot(ppmbei,ngrid=c(128,128),se=TRUE,trend=FALSE,superimpose=FALSE,pause=FALSE)->se
points(bei,pch=3,cex=.4)

par(mfrow=c(1,1))
Mdbei<-as.matrix(dbei)
Mtrendbei<-as.matrix(trend$trend[[1]])
image(as.im(Mdbei-Mtrendbei),main='difference between non parametric and parametric estimations')


## Do simulated replications of Poisson point processes using the estimated intensity look like the dataset? 

rhoEst<-function(x,y,coef=ppmbei$coef){
  r<-coef[1]+interp.im(bei.extra$elev,x,y)*coef[2] + interp.im(bei.extra$grad,x,y)*coef[3]
  exp(r)
}


par(mfrow=c(3,3))
for (i in 1:9){
  sim<-rpoispp(rhoEst,lmax=0.03,win=as.owin(bei))
  if (i==5) plot(bei,pch=3,cex=.4,main="") else plot(sim,pch=3,cex=.4,main="")
}

par(mfrow=c(1,1))