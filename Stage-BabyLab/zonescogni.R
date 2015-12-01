load("~/Desktop/WorkSpaceBabyLab.RData")
load("~/Desktop/hyperframe2.RData")
library(multippm)
library(plotrix)

X_adult_Im<<-list()
for (i in 1:6) {
  X_adult_Im[[i]]<-ppp(Fixations_coordx_adult_Im[[i]], Fixations_coordy_adult_Im[[i]], c(0,1024), c(0,768), unitname=c("pixels"))
}

# Recherche zone cognitive, à partir du groupe adulte
X_Im<-list()
for (i in 1:6) {
  x<-c(Fixations_coordx_baby3month_Im[[i]],Fixations_coordx_baby6month_Im[[i]],Fixations_coordx_baby9month_Im[[i]],Fixations_coordx_baby12month_Im[[i]],Fixations_coordx_adult_Im[[i]])
  y<-c(Fixations_coordy_baby3month_Im[[i]],Fixations_coordy_baby6month_Im[[i]],Fixations_coordy_baby9month_Im[[i]],Fixations_coordy_baby12month_Im[[i]],Fixations_coordy_adult_Im[[i]])
  mark1 <-c(rep("3mois",X_baby3month_Im[[i]]$n),rep("6mois",X_baby6month_Im[[i]]$n),rep("9mois",X_baby9month_Im[[i]]$n),rep("12mois",X_baby12month_Im[[i]]$n),rep("Adulte",X_adult_Im[[i]]$n))
  mark1 <- factor(mark1, levels=c("3mois","6mois","9mois", "12mois", "Adulte"))
  X_Im[[i]] <- ppp(x, y, c(0,1024), c(0,768), unitname=c("pixels"), marks=mark1)
  plot(X_Im[[i]],paste("Fixations, Image",i,", selon les groupes"))
  points(X_Im[[i]],col=mark1)
}

# Density.ppp
par(mfrow=c(3,2))
for (i in 6:6) {
plot(density.ppp(unmark(X_Im[[i]][X_Im[[i]]$marks=="Adulte"]),at="pixels"),main="Par défaut")
}

# Avec sigma=bw.diggle
for (i in 6:6) {
  plot(density.ppp(unmark(X_Im[[i]][X_Im[[i]]$marks=="Adulte"]),bw.diggle,at="pixels"),main="Diggle")
}

# Avec sigma=bw.ppl
for (i in 6:6) {
  plot(density.ppp(unmark(X_Im[[i]][X_Im[[i]]$marks=="Adulte"]),bw.ppl,at="pixels"),main="PPL")
}

# En fixant manuellement sigma
for (i in 6:6) {
  plot(density.ppp(unmark(X_Im[[i]][X_Im[[i]]$marks=="Adulte"]),at="pixels",sigma=10),main="sigma=10")
}

for (i in 6:6) {
  plot(density.ppp(unmark(X_Im[[i]][X_Im[[i]]$marks=="Adulte"]),at="pixels",sigma=50),main="sigma=50")
}

for (i in 6:6) {
  plot(density.ppp(unmark(X_Im[[i]][X_Im[[i]]$marks=="Adulte"]),at="pixels",sigma=100),main="sigma=100")
} 


# En sélectionnant l'estimation faite par défaut
density1 <- density.ppp(unmark(X_Im[[1]][X_Im[[1]]$marks=="Adulte"]),at="pixels")
quantile(density1$v,p=0.9) 

matrice.def=matrix(nrow=128,ncol=128)
for(i in 1:128) {
  for(j in 1:128) {
    if (density1[i,j]>quantile(density1$v,p=0.9)) {matrice.def[i,j]=1}
    else matrice.def[i,j]=0
  }
}

# En sélectionnant l'estimation faite par sigma=bw.diggle
density1 <- density.ppp(unmark(X_Im[[1]][X_Im[[1]]$marks=="Adulte"]),bw.diggle,at="pixels")
quantile(density1$v,p=0.9)

# On parcourt la matrice
matrice.diggle=matrix(nrow=128,ncol=128)
for(i in 1:128) {
  for(j in 1:128) {
   if (density1[i,j]>quantile(density1$v,p=0.9)) {matrice.diggle[i,j]=1}
   else matrice.diggle[i,j]=0
  }
}

# En sélectionnant l'estimation faite par sigma=bw.ppl
density1 <- density.ppp(unmark(X_Im[[1]][X_Im[[1]]$marks=="Adulte"]),bw.ppl,at="pixels")
quantile(density1$v,p=0.9) # 0.0020179

# On parcourt la matrice
matrice.ppl=matrix(nrow=128,ncol=128)
for(i in 1:128) {
  for(j in 1:128) {
    if (density1[i,j]>quantile(density1$v,p=0.9)) {matrice.ppl[i,j]=1}
    else matrice.ppl[i,j]=0
  }
}

# En sélectionnant l'estimation faite par sigma=10
density1 <- density.ppp(unmark(X_Im[[1]][X_Im[[1]]$marks=="Adulte"]),sigma=10,at="pixels")
quantile(density1$v,p=0.9)

matrice.sig10=matrix(nrow=128,ncol=128)
for(i in 1:128) {
  for(j in 1:128) {
    if (density1[i,j]>quantile(density1$v,p=0.9)) {matrice.sig10[i,j]=1}
    else matrice.sig10[i,j]=0
  }
}

# En sélectionnant l'estimation faite par sigma=50
density1 <- density.ppp(unmark(X_Im[[1]][X_Im[[1]]$marks=="Adulte"]),sigma=50,at="pixels")
quantile(density1$v,p=0.9)

matrice.sig50=matrix(nrow=128,ncol=128)
for(i in 1:128) {
  for(j in 1:128) {
    if (density1[i,j]>quantile(density1$v,p=0.9)) {matrice.sig50[i,j]=1}
    else matrice.sig50[i,j]=0
  }
}

# En sélectionnant l'estimation faite par sigma=100
density1 <- density.ppp(unmark(X_Im[[1]][X_Im[[1]]$marks=="Adulte"]),sigma=100,at="pixels")
quantile(density1$v,p=0.9)

matrice.sig100=matrix(nrow=128,ncol=128)
for(i in 1:128) {
  for(j in 1:128) {
    if (density1[i,j]>quantile(density1$v,p=0.9)) {matrice.sig100[i,j]=1}
    else matrice.sig100[i,j]=0
  }
}

# En sélectionnant l'estimation faite par sigma=35
density1 <- density.ppp(unmark(X_Im[[1]][X_Im[[1]]$marks=="Adulte"]),sigma=35,at="pixels")
quantile(density1$v,p=0.9)

matrice.sig100=matrix(nrow=128,ncol=128)
for(i in 1:128) {
  for(j in 1:128) {
    if (density1[i,j]>quantile(density1$v,p=0.9)) {matrice.sig100[i,j]=1}
    else matrice.sig100[i,j]=0
  }
}

par(mfrow=c(2,3))
plot(as.im(matrice.def))
plot(as.im(matrice.diggle))
plot(as.im(matrice.ppl))
plot(as.im(matrice.sig10))
plot(as.im(matrice.sig50))
plot(as.im(matrice.sig100))

# Pour toutes les images
# En sélectionnant l'estimation faite par défaut
par(mfrow=c(2,3))
density.def <- list()
matrice.def <- list()
density.diggle <- list()
matrice.diggle <- list()
density.ppl <- list()
matrice.ppl <- list()

for(i in 1:6){
density.def[[i]] <- density.ppp(unmark(X_Im[[i]][X_Im[[i]]$marks=="Adulte"]),at="pixels")

matrice.def[[i]]=matrix(nrow=128,ncol=128)
for(j in 1:128) {
  for(k in 1:128) {
    if (density.def[[i]][j,k]>quantile(density.def[[i]]$v,p=0.9)) {matrice.def[[i]][j,k]=1}
    else matrice.def[[i]][j,k]=0
  }
}
plot(im(matrice.def[[i]],xrange=c(0,1024), yrange=c(0,768)),main=paste("Default, Image",i))

# En sélectionnant l'estimation faite par sigma=bw.diggle
density.diggle[[i]] <- density.ppp(unmark(X_Im[[i]][X_Im[[i]]$marks=="Adulte"]),bw.diggle,at="pixels")

matrice.diggle[[i]]=matrix(nrow=128,ncol=128)
for(j in 1:128) {
  for(k in 1:128) {
    if (density.diggle[[i]][j,k]>quantile(density.diggle[[i]]$v,p=0.9)) {matrice.diggle[[i]][j,k]=1}
    else matrice.diggle[[i]][j,k]=0
  }
}
plot(im(matrice.diggle[[i]],xrange=c(0,1024), yrange=c(0,768)),main=paste("Diggle, Image",i))


# En sélectionnant l'estimation faite par sigma=35, p=0.9
par(mfrow=c(2,3))
density.35 <- list()
density35bin <- list()
for(i in 1:6){
density.35[[i]] <- density.ppp(unmark(X_Im[[i]][X_Im[[i]]$marks=="Adulte"]),sigma=35,at="pixels")
matrice.35[[i]]=matrix(nrow=128,ncol=128)
for(j in 1:128) {
  for(k in 1:128) {
    if (density.35[[i]][j,k]>quantile(density.35[[i]]$v,p=0.9)) {matrice.35[[i]][j,k]=1}
    else matrice.35[[i]][j,k]=0
  }
}
density35bin[[i]] <- im(matrice.35[[i]],xrange=c(0,1024), yrange=c(0,768))
# plot(density35bin[[i]],main=paste("Image",i))
}

# En sélectionnant l'estimation faite par sigma=35, p=0.95
par(mfrow=c(2,3))
density.35 <- list()
density35bin1 <- list()
matrice.35 <- list()
for(i in 1:6){
  density.35[[i]] <- density.ppp(unmark(X_Im[[i]][X_Im[[i]]$marks=="Adulte"]),sigma=35,at="pixels")
  matrice.35[[i]]=matrix(nrow=128,ncol=128)
  for(j in 1:128) {
    for(k in 1:128) {
      if (density.35[[i]][j,k]>quantile(density.35[[i]]$v,p=0.95)) {matrice.35[[i]][j,k]=1}
      else matrice.35[[i]][j,k]=0
    }
  }
  density35bin1[[i]] <- im(matrice.35[[i]],xrange=c(0,1024), yrange=c(0,768))
  plot(density35bin1[[i]],main=paste("Carte Bin, Image",i))
}

# sp, rgeos
x = cbind(c(0,700,700,0,0),c(0,0,700,700,0))
pol = SpatialPolygons(list(Polygons(list(Polygon(x)), "ID")))
pts <- SpatialPoints(cbind(X_baby3month_Im[[1]]$x,X_baby3month_Im[[1]]$y))
library(rgeos)
gDistance(pts, pol, byid = TRUE) # will be 0, all inside
gDistance(pts, as(pol, "SpatialLines"), byid = TRUE) # dist to line
plot(pol)
text(coordinates(pts),
     as.character(
       round(as.vector(gDistance(pts, as(pol, "SpatialLines"), byid = TRUE)), 3)),
     pos = 4)

# Avec JF
# Avec sigma=35, pour toutes les images
# Essayer de passer par le log pour les problemes d'echelle
par(mfrow=c(2,3))
for(i in 1:6){
density1 <- density.ppp(unmark(X_Im[[i]][X_Im[[i]]$marks=="Adulte"]),sigma=35,at="pixels",dimyx=c(768,1024))
m <- density1$v>quantile(density1$v,p=0.9)
plot(owin(mask=m),main=paste("ZC, Image", i))
}


# On cherche à associer les points de fixations du groupe 3 mois avec les distances associées par rapport aux zones cognitives

# 0.9
babyIm.Distlevel35 <- list()
density35 <- list()
density35norm <-list()
m <- list()
for (i in 1:6) {
  density35[[i]] <- density.ppp(unmark(X_Im[[i]][X_Im[[i]]$marks=="Adulte"]),sigma=35,at="pixels",dimyx=c(768,1024))
  # plot(density35[[i]],main=paste("Intensité Ad., Image Rafting"))
  #m[[i]] <- density35[[i]]$v>quantile(density35[[i]]$v,p=0.95)
  #babyIm.Distlevel35[[i]] <- distmap(owin(mask=m[[i]]))
  # plot(owin(mask=m[[i]]),main=paste("ZC, Image", i))
  # plot(babyIm.Distlevel35[[i]],main=paste("Dist. ZC, Image",i))
  Dens35 <- as.numeric(density35[[i]]$v)
  Dens35norm <- Dens35 *(1/max(Dens35)) # Echelle [0;1]
  matrixDens35norm<-matrix(Dens35norm,nrow=768,ncol=1024,byrow=FALSE)
  density35norm[[i]]<-im(matrixDens35norm,xrange=c(0,1024),yrange=c(0,768))
  plot(density35norm[[i]],main=paste("Int. Norm. Image",i))
}

# 0.95
babyIm.Distlevel351 <- list()
density35 <- list()
density35norm1 <-list()
m <- list()
par(mfrow=c(2,3))
for (i in 1:6){
  density35[[i]] <- density.ppp(unmark(X_Im[[i]][X_Im[[i]]$marks=="Adulte"]),sigma=35,at="pixels",dimyx=c(768,1024))
  plot(density35[[i]],main=paste("Intensité Ad., Image", i))
  m[[i]] <- density35[[i]]$v>quantile(density35[[i]]$v,p=0.95)
  babyIm.Distlevel351[[i]] <- distmap(owin(mask=m[[i]]))
  plot(owin(mask=m[[i]]),main=paste("ZC, 5%, Image", i))
  plot(babyIm.Distlevel351[[i]],main=paste("Dist. ZC, Image", i))
}


# Test sigma
babyIm.Distlevel175 <- list()
density175 <- list()
m <- list()
for (i in 1:6){
  density175[[i]] <- density.ppp(unmark(X_Im[[i]][X_Im[[i]]$marks=="Adulte"]),sigma=17.5,at="pixels",dimyx=c(768,1024))
  m[[i]] <- density175[[i]]$v>quantile(density175[[i]]$v,p=0.9)
  babyIm.Distlevel175[[i]] <- distmap(owin(mask=m[[i]]))
  # plot(owin(mask=m[[i]]),main=paste("ZC, Image", i))
  plot(babyIm.Distlevel175[[i]],main=paste("Dist. ZC, Image", i))
}


# Par defaut :
par(mfrow=c(2,3))
babyIm.Distleveldef <- list()
densitydef <- list()
densitydefnorm <- list()
m <- list()
for (i in 1:6){
  densitydef[[i]] <- density.ppp(unmark(X_Im[[i]][X_Im[[i]]$marks=="Adulte"]),at="pixels",dimyx=c(768,1024))
  # plot(densitydef[[i]],main=paste("Intensité Ad., Image", i))
  # m[[i]] <- densitydef[[i]]$v>quantile(densitydef[[i]]$v,p=0.9)
  # babyIm.Distleveldef[[i]] <- distmap(owin(mask=m[[i]]))
  # plot(owin(mask=m[[i]]),main=paste("ZC, Image", i))
  # plot(babyIm.Distleveldef[[i]],main=paste("Dist. ZC, Image", i))
  Densdef <- as.numeric(densitydef[[i]]$v)
  Densdefnorm <- Densdef *(1/max(Densdef)) # Echelle [0;1]
  matrixDensdefnorm<-matrix(Densdefnorm,nrow=768,ncol=1024,byrow=FALSE)
  densitydefnorm[[i]]<-im(matrixDensdefnorm,xrange=c(0,1024),yrange=c(0,768))
  # plot(densitydefnorm[[i]])
}

# Par bw.ppl
par(mfrow=c(2,3))
babyIm.Distlevelppl <- list()
densityppl <- list()
m <- list()
for (i in 1:6){
  densityppl[[i]] <- density.ppp(unmark(X_Im[[i]][X_Im[[i]]$marks=="Adulte"]),sigma=bw.ppl,at="pixels",dimyx=c(768,1024))
  # plot(densityppl[[i]],main=paste("Intensité Ad., Image", i))
  m[[i]] <- densityppl[[i]]$v>quantile(densityppl[[i]]$v,p=0.9)
  babyIm.Distlevelppl[[i]] <- distmap(owin(mask=m[[i]]))
  # plot(owin(mask=m[[i]]),main=paste("ZC, Image", i))
  # plot(babyIm.Distlevelppl[[i]],main=paste("Dist. ZC, Image", i))
}

# Par bw.diggle
par(mfrow=c(2,3))
babyIm.Distleveldiggle <- list()
densitydiggle <- list()
m <- list()
for (i in 1:6){
  densitydiggle[[i]] <- density.ppp(unmark(X_Im[[i]][X_Im[[i]]$marks=="Adulte"]),sigma=bw.diggle,at="pixels",dimyx=c(768,1024))
  # plot(densitydiggle[[i]],main=paste("Intensité Ad., Image", i))
  m[[i]] <- densitydiggle[[i]]$v>quantile(densitydiggle[[i]]$v,p=0.9)
  babyIm.Distleveldiggle[[i]] <- distmap(owin(mask=m[[i]]))
  # plot(owin(mask=m[[i]]),main=paste("ZC, Image", i))
  # plot(babyIm.Distleveldiggle[[i]],main=paste("Dist. ZC, Image", i))
}


# Fixations avec distances par rapport aux ZC associées, tous les groupes
par(mfrow=c(2,3))
for (i in 1:6){
  Distlevel <- babyIm.Distlevel[[i]]
  YDistlevel <- X_Im[[i]] %mark% Distlevel[X_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(YDistlevel$marks))
  plot(YDistlevel, pch=21, bg=A, cex=.8, main=paste("Distances aux zones, Image"), i)
}

# En séparant les groupes
# Fixations avec distances par rapport aux ZC associées, bébés 3 mois
par(mfrow=c(2,3))
for (i in 1:6){
  Distlevel <- babyIm.Distlevel[[i]]
  YDistlevel <- X_baby3month_Im[[i]] %mark% Distlevel[X_baby3month_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(YDistlevel$marks))
  plot(YDistlevel, pch=21, bg=A, cex=.8, main=paste("Dist zones, 3m, Image", i))
}

# Fixations avec distances par rapport aux ZC associées, bébés 6 mois
par(mfrow=c(2,3))
for (i in 1:6){
  Distlevel <- babyIm.Distlevel[[i]]
  YDistlevel <- X_baby6month_Im[[i]] %mark% Distlevel[X_baby6month_Im[[i]]]
A <- colourmap(heat.colors(16), range=range(YDistlevel$marks))
plot(YDistlevel, pch=21, bg=A, cex=.8, main=paste("Dist zones, 6m, Image", i))
}

# Fixations avec distances par rapport aux ZC associées, bébés 9 mois
par(mfrow=c(2,3))
for (i in 1:6){
  Distlevel <- babyIm.Distlevel[[i]]
  YDistlevel <- X_baby9month_Im[[i]] %mark% Distlevel[X_baby9month_Im[[i]]]
A <- colourmap(heat.colors(16), range=range(YDistlevel$marks))
plot(YDistlevel, pch=21, bg=A, cex=.8, main=paste("Dist zones, 9m, Image", i))
}

# Fixations avec distances par rapport aux ZC associées, bébés 12 mois
par(mfrow=c(2,3))
for (i in 1:6){
  Distlevel <- babyIm.Distlevel[[i]]
  YDistlevel <- X_baby12month_Im[[i]] %mark% Distlevel[X_baby12month_Im[[i]]]
A <- colourmap(heat.colors(16), range=range(YDistlevel$marks))
plot(YDistlevel, pch=21, bg=A, cex=.8, main=paste("Dist zones, 12m, Image", i))
}

# Fixations avec distances par rapport aux ZC associées, adultes
par(mfrow=c(2,3))
for (i in 1:6){
  Distlevel <- babyIm.Distlevel[[i]]
  YDistlevel <- X_adult_Im[[i]] %mark% Distlevel[X_adult_Im[[i]]]
A <- colourmap(heat.colors(16), range=range(YDistlevel$marks))
plot(YDistlevel, pch=21, bg=A, cex=.8, main=paste("Dist zones, ad, Image", i))
}


