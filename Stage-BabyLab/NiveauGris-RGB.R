load("~/Desktop/MASSS/Stage/Codes R Antoine/WorkSpace_Antoine.RData")

m1 <- as.raster(r1, max = 255)
m2 <- as.raster(r2, max = 255)
m3 <- as.raster(r3, max = 255)
m4 <- as.raster(r4, max = 255)
m5 <- as.raster(r5, max = 255)
m6 <- as.raster(r6, max = 255)
m=list(m1,m2,m3,m4,m5,m6)

### Récupérer niveau de gris ###
## Bon code ? (pas inversement 1-niveau ?)
# Image 1
m <- as.raster(r[[i]], max = 255)
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main="Image 1")
usr <- par("usr")
rasterImage(m, usr[1], usr[3], usr[2], usr[4]) 
plot(images[[1]],main="Niveau de gris") # Carte niveau de gris

images[[1]]$v
# Pour nous aider à vérifier les coordonnées (intéraction) :
locator()
images[[1]]$v[523,294] # 0.98 (ligne,colonne)=(y,x)

babyIm.greylevel=list(images[[1]],images[[2]],images[[3]],images[[4]],images[[5]],images[[6]])

# Image 1
par(mfrow=c(2,1))
image(babyIm.greylevel[[1]]);points(X_Im[[1]],pch=3,cex=.4)
greylevel <- babyIm.greylevel[[1]]
Ygreylevel <- X_Im[[1]] %mark% greylevel[X_Im[[1]]]
A <- colourmap(heat.colors(16), range=range(Ygreylevel$marks))
plot(Ygreylevel, pch=21, bg=A, cex=.7)

# Pour toutes les images
for(i in 1:6){
par(mfrow=c(2,1))
image(babyIm.greylevel[[i]]);points(X_Im[[i]],pch=3,cex=.4)
greylevel <- babyIm.greylevel[[i]]
Ygreylevel <- X_Im[[i]] %mark% greylevel[X_Im[[i]]]
A <- colourmap(heat.colors(16), range=range(Ygreylevel$marks))
plot(Ygreylevel, pch=21, bg=A, cex=.5)
}

### Récupérer niveau RGB ###
t<-col2rgb(m1)
red_Im1<-t[1,]
green_Im1<-t[2,]
blue_Im1<-t[3,]

# Niveau red
matrixred<-matrix(red_Im1,nrow=768,ncol=1024,byrow=TRUE)

nb_rows<-dim(matrixred)[1]
nb_columns<-dim(matrixred)[2]
flip_matrixred<-matrix(nrow=nb_rows,ncol=nb_columns)
for(i in 1:nb_rows) { flip_matrixred[i,]=matrixred[nb_rows+1-i,] }
matrixred<-flip_matrixred

# Builds a pixel image in spatstat format
babyIm1.redlevel<-im(matrixred,xrange=c(0,1024),yrange=c(0,768))
str(babyIm1.redlevel)
# Ok, sauf integer instead of real

par(mfrow=c(1,2))
image(babyIm1.redlevel);points(X_Im[[1]],pch=3,cex=.4)
redlevel <- babyIm1.redlevel
Yredlevel <- X_Im[[1]] %mark% redlevel[X_Im[[1]]]
A <- colourmap(heat.colors(16), range=range(Yredlevel$marks))
plot(Yredlevel, pch=21, bg=A, cex=.7)

# Niveau green
matrixgreen<-matrix(green_Im1,nrow=768,ncol=1024,byrow=TRUE)

nb_rows<-dim(matrixgreen)[1]
nb_columns<-dim(matrixgreen)[2]
flip_matrixgreen<-matrix(nrow=nb_rows,ncol=nb_columns)
for(i in 1:nb_rows) { flip_matrixgreen[i,]=matrixgreen[nb_rows+1-i,] }
matrixgreen<-flip_matrixgreen

# Builds a pixel image in spatstat format
babyIm1.greenlevel<-im(matrixgreen,xrange=c(0,1024),yrange=c(0,768))
str(babyIm1.greenlevel)
# Ok, sauf integer instead of real

par(mfrow=c(1,2))
image(babyIm1.greenlevel);points(X_Im[[1]],pch=3,cex=.4)
greenlevel <- babyIm1.greenlevel
Ygreenlevel <- X_Im[[1]] %mark% greenlevel[X_Im[[1]]]
A <- colourmap(heat.colors(16), range=range(Ygreenlevel$marks))
plot(Ygreenlevel, pch=21, bg=A, cex=.7)

# Niveau blue
matrixblue<-matrix(blue_Im1,nrow=768,ncol=1024,byrow=TRUE)

nb_rows<-dim(matrixblue)[1]
nb_columns<-dim(matrixblue)[2]
flip_matrixblue<-matrix(nrow=nb_rows,ncol=nb_columns)
for(i in 1:nb_rows) { flip_matrixblue[i,]=matrixblue[nb_rows+1-i,] }
matrixblue<-flip_matrixblue

# Builds a pixel image in spatstat format
babyIm1.bluelevel<-im(matrixblue,xrange=c(0,1024),yrange=c(0,768))
str(babyIm1.bluelevel)
# Ok, sauf integer instead of real

par(mfrow=c(1,2))
image(babyIm1.bluelevel);points(X_Im[[1]],pch=3,cex=.4)
bluelevel <- babyIm1.bluelevel
Ybluelevel <- X_Im[[1]] %mark% bluelevel[X_Im[[1]]]
A <- colourmap(heat.colors(16), range=range(Ybluelevel$marks))
plot(Ybluelevel, pch=21, bg=A, cex=.7)

# Affichage des cartes de niveau de gris et rgb, Image 1
par(mfrow=c(2,2))
image(babyIm1.greylevel,main="Carte niveau de gris")
image(babyIm1.redlevel,main="Carte niveau de rouge")
image(babyIm1.greenlevel,main="Carte niveau de vert")
image(babyIm1.bluelevel,main="Carte niveau de bleu")

# Pour toutes les images, niveau de gris
x11()
par(mfrow=c(3,2))
for(i in 1:6){
image(babyIm.greylevel[[i]])
greylevel <- babyIm.greylevel[[i]]
Ygreylevel <- X_Im[[i]] %mark% greylevel[X_Im[[i]]]
A <- colourmap(heat.colors(16), range=range(Ygreylevel$marks))
plot(Ygreylevel, pch=21, bg=A, cex=.5)
}

# Pour toutes les images, niveau de rouge
red_Im<-list()
babyIm.redlevel <- list()
x11()
par(mfrow=c(3,2))
for (i in 1:6) {
  t<-col2rgb(m[[i]])
  red_Im[[i]]<-t[1,]
  
  matrixred<-matrix(red_Im[[i]],nrow=768,ncol=1024,byrow=TRUE)
  
  nb_rows<-dim(matrixred)[1]
  nb_columns<-dim(matrixred)[2]
  flip_matrixred<-matrix(nrow=nb_rows,ncol=nb_columns)
  
  for(j in 1:nb_rows) { flip_matrixred[j,]=matrixred[nb_rows+1-j,] }
  matrixred<-flip_matrixred
  
  # Builds a pixel image in spatstat format
  babyIm.redlevel[[i]]<-im(matrixred,xrange=c(0,1024),yrange=c(0,768))

  image(babyIm.redlevel[[i]])
}

# Pour toutes les images, niveau de rouge associé aux fixations
red_Im<-list()
babyIm.redlevel <- list()
x11()
par(mfrow=c(3,4))
for(i in 1:6){
  t<-col2rgb(m[[i]])
  red_Im[[i]]<-t[1,]
  
  matrixred<-matrix(red_Im[[i]],nrow=768,ncol=1024,byrow=TRUE)
  
  nb_rows<-dim(matrixred)[1]
  nb_columns<-dim(matrixred)[2]
  flip_matrixred<-matrix(nrow=nb_rows,ncol=nb_columns)
  
  for(j in 1:nb_rows) { flip_matrixred[j,]=matrixred[nb_rows+1-j,] }
  matrixred<-flip_matrixred
  
  # Builds a pixel image in spatstat format
  babyIm.redlevel[[i]]<-im(matrixred,xrange=c(0,1024),yrange=c(0,768))
  
  image(babyIm.redlevel[[i]])
  redlevel <- babyIm.redlevel[[i]]
  Yredlevel <- X_Im[[i]] %mark% redlevel[X_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(Yredlevel$marks))
  plot(Yredlevel, pch=21, bg=A, cex=.5, main="Niveau de rouge")
}

# Pour toutes les images, niveau de vert
green_Im<-list()
babyIm.greenlevel <- list()
x11()
par(mfrow=c(3,2))
for (i in 1:6) {
  t<-col2rgb(m[[i]])
  green_Im[[i]]<-t[2,]

  matrixgreen<-matrix(green_Im[[i]],nrow=768,ncol=1024,byrow=TRUE)
  
  nb_rows<-dim(matrixgreen)[1]
  nb_columns<-dim(matrixgreen)[2]
  flip_matrixgreen<-matrix(nrow=nb_rows,ncol=nb_columns)
  
  for(j in 1:nb_rows) { flip_matrixgreen[j,]=matrixgreen[nb_rows+1-j,] }
  matrixgreen<-flip_matrixgreen
  
  # Builds a pixel image in spatstat format
  babyIm.greenlevel[[i]]<-im(matrixgreen,xrange=c(0,1024),yrange=c(0,768))
  
  image(babyIm.greenlevel[[i]])
}

# Pour toutes les images, niveau de vert associé aux fixations
green_Im<-list()
babyIm.greenlevel <- list()
par(mfrow=c(3,4))
for(i in 1:6){
  t<-col2rgb(m[[i]])
  green_Im[[i]]<-t[2,]
  
  matrixgreen<-matrix(green_Im[[i]],nrow=768,ncol=1024,byrow=TRUE)
  
  nb_rows<-dim(matrixgreen)[1]
  nb_columns<-dim(matrixgreen)[2]
  flip_matrixgreen<-matrix(nrow=nb_rows,ncol=nb_columns)
  
  for(j in 1:nb_rows) { flip_matrixgreen[j,]=matrixgreen[nb_rows+1-j,] }
  matrixgreen<-flip_matrixgreen
  
  # Builds a pixel image in spatstat format
  babyIm.greenlevel[[i]]<-im(matrixgreen,xrange=c(0,1024),yrange=c(0,768))
  
  image(babyIm.greenlevel[[i]])
  greenlevel <- babyIm.greenlevel[[i]]
  Ygreenlevel <- X_Im[[i]] %mark% greenlevel[X_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(Ygreenlevel$marks))
  plot(Ygreenlevel, pch=21, bg=A, cex=.5, main="Niveau de vert")
}

# Pour toutes les images, niveau de bleu
blue_Im<-list()
babyIm.bluelevel <- list()
x11()
par(mfrow=c(3,2))
for (i in 1:6) {
  t<-col2rgb(m[[i]])
  blue_Im[[i]]<-t[3,]
  
  matrixblue<-matrix(blue_Im[[i]],nrow=768,ncol=1024,byrow=TRUE)
  
  nb_rows<-dim(matrixblue)[1]
  nb_columns<-dim(matrixblue)[2]
  flip_matrixblue<-matrix(nrow=nb_rows,ncol=nb_columns)
  
  for(j in 1:nb_rows) { flip_matrixblue[j,]=matrixblue[nb_rows+1-j,] }
  matrixblue<-flip_matrixblue
  
  # Builds a pixel image in spatstat format
  babyIm.bluelevel[[i]]<-im(matrixblue,xrange=c(0,1024),yrange=c(0,768))
  
  image(babyIm.bluelevel[[i]])
}

# Pour toutes les images, niveau bleu associé aux fixations
blue_Im<-list()
babyIm.bluelevel <- list()
par(mfrow=c(3,4))
for(i in 1:6){
  t<-col2rgb(m[[i]])
  blue_Im[[i]]<-t[3,]
  
  matrixblue<-matrix(blue_Im[[i]],nrow=768,ncol=1024,byrow=TRUE)
  
  nb_rows<-dim(matrixblue)[1]
  nb_columns<-dim(matrixblue)[2]
  flip_matrixblue<-matrix(nrow=nb_rows,ncol=nb_columns)
  
  for(j in 1:nb_rows) { flip_matrixblue[j,]=matrixblue[nb_rows+1-j,] }
  matrixblue<-flip_matrixblue
  
  # Builds a pixel image in spatstat format
  babyIm.bluelevel[[i]]<-im(matrixblue,xrange=c(0,1024),yrange=c(0,768))
  
  image(babyIm.bluelevel[[i]])
  bluelevel <- babyIm.bluelevel[[i]]
  Ybluelevel <- X_Im[[i]] %mark% bluelevel[X_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(Ybluelevel$marks))
  plot(Ybluelevel, pch=21, bg=A, cex=.5, main="Niveau de bleu")
}

# Niveau de rouge, de gris semblable !!

# Pour toutes les images, niveau de gris et de rgb
for (i in 1:6) {
  par(mfrow=c(2,2))
  image(babyIm.greylevel[[i]],main="Carte niveau de gris")
  image(babyIm.redlevel[[i]],main="Carte niveau de rouge")
  image(babyIm.greenlevel[[i]],main="Carte niveau de vert")
  image(babyIm.bluelevel[[i]],main="Carte niveau de bleu")
}

# Pour toutes les images, niveau de gris et rgb sur les fixations
for (i in c(1)) {
  t<-col2rgb(m[[i]])
  par(mfrow=c(2,2))
  
  #Niveau gris
  greylevel <- babyIm.greylevel[[i]]
  Ygreylevel <- X_Im[[i]] %mark% greylevel[X_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(Ygreylevel$marks))
  plot(Ygreylevel, pch=21, bg=A, cex=.8, main=paste("Niveau de gris, Image",i))
  
  #Niveau rouge
  t<-col2rgb(m[[i]])
  red_Im[[i]]<-t[1,]/255
  matrixred<-matrix(red_Im[[i]],nrow=768,ncol=1024,byrow=TRUE)
  nb_rows<-dim(matrixred)[1]
  nb_columns<-dim(matrixred)[2]
  flip_matrixred<-matrix(nrow=nb_rows,ncol=nb_columns)
  for(j in 1:nb_rows) { flip_matrixred[j,]=matrixred[nb_rows+1-j,] }
  matrixred<-flip_matrixred
  # Builds a pixel image in spatstat format
  babyIm.redlevel[[i]]<-im(matrixred,xrange=c(0,1024),yrange=c(0,768))
  redlevel <- babyIm.redlevel[[i]]
  Yredlevel <- X_Im[[i]] %mark% redlevel[X_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(Yredlevel$marks))
  plot(Yredlevel, pch=21, bg=A, cex=.8, main=paste("Niveau de rouge, Image",i))

  # Niveau vert
  green_Im[[i]]<-t[2,]/255
  matrixgreen<-matrix(green_Im[[i]],nrow=768,ncol=1024,byrow=TRUE)
  nb_rows<-dim(matrixgreen)[1]
  nb_columns<-dim(matrixgreen)[2]
  flip_matrixgreen<-matrix(nrow=nb_rows,ncol=nb_columns)
  for(j in 1:nb_rows) { flip_matrixgreen[j,]=matrixgreen[nb_rows+1-j,] }
  matrixgreen<-flip_matrixgreen
  babyIm.greenlevel[[i]]<-im(matrixgreen,xrange=c(0,1024),yrange=c(0,768))
  greenlevel <- babyIm.greenlevel[[i]]
  Ygreenlevel <- X_Im[[i]] %mark% greenlevel[X_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(Ygreenlevel$marks))
  plot(Ygreenlevel, pch=21, bg=A, cex=.8, main=paste("Niveau de vert, Image",i))
  
  # Niveau bleu
  blue_Im[[i]]<-t[3,]/255
  matrixblue<-matrix(blue_Im[[i]],nrow=768,ncol=1024,byrow=TRUE)
  nb_rows<-dim(matrixblue)[1]
  nb_columns<-dim(matrixblue)[2]
  flip_matrixblue<-matrix(nrow=nb_rows,ncol=nb_columns)
  for(j in 1:nb_rows) { flip_matrixblue[j,]=matrixblue[nb_rows+1-j,] }
  matrixblue<-flip_matrixblue
  # Builds a pixel image in spatstat format
  babyIm.bluelevel[[i]]<-im(matrixblue,xrange=c(0,1024),yrange=c(0,768))
  bluelevel <- babyIm.bluelevel[[i]]
  Ybluelevel <- X_Im[[i]] %mark% bluelevel[X_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(Ybluelevel$marks))
  plot(Ybluelevel, pch=21, bg=A, cex=.8, main=paste("Niveau de bleu, Image",i))
}

## Tentative de ppm
# Avec bei
ppm(bei,~elev+grad,covariates=list(elev=bei.extra$elev,grad=bei.extra$grad))->ppmbei
ppmbei

# Simplest example
fit <- ppm(X_Im[[1]] ~ 1)
fit

# Avec x et y
fitxy <- ppm(X_Im[[1]],~ x+y,covariates=list(x=X_Im[1]$x,y=X_Im[1]$y))
fitxy

# Image 1
# Niveau gris
ppmIm1grey <- ppm(X_Im[[1]],~grey,covariates=list(grey=babyIm.greylevel[[1]]))
ppmIm1grey

#Niveau rouge
ppmIm1red <- ppm(X_Im[[1]],~red,covariates=list(red=babyIm.redlevel[[1]]))
ppmIm1red

# Niveau vert
ppmIm1green <- ppm(X_Im[[1]],~green,covariates=list(green=babyIm.greenlevel[[1]]))
ppmIm1green

# Niveau bleu
ppmIm1blue <- ppm(X_Im[[1]],~blue,covariates=list(blue=babyIm.bluelevel[[1]]))
ppmIm1blue

# Niveau gris et rouge (problème car red=gris)
ppmIm1grisrouge <- ppm(X_Im[[1]],~grey+red,covariates=list(grey=babyIm.greylevel[[1]],red=babyIm.redlevel[[1]]))
ppmIm1grisrouge

# Tous les niveaux (problème car red=gris)
ppmIm1all <- ppm(X_Im[[1]],~grey+red+green+blue,covariates=list(grey=babyIm.greylevel[[1]],red=babyIm.redlevel[[1]],green=babyIm.greenlevel[[1]],blue=babyIm.bluelevel[[i]]))
ppmIm1all

# Tous les niveaux sauf rouge (plus de problème)
ppmIm1ggb <- ppm(X_Im[[1]],~grey+green+blue,covariates=list(grey=babyIm.greylevel[[1]],green=babyIm.greenlevel[[1]],blue=babyIm.bluelevel[[i]]))
ppmIm1ggb

# Vérificatiion red=gris, Image 1
ppmIm1grey <- ppm(X_Im[[1]],~grey,covariates=list(grey=babyIm.greylevel[[1]]))
plot(ppmIm1grey,se=FALSE,superimpose=FALSE,pause=FALSE)->trendIm1grey
ppmIm1red <- ppm(X_Im[[1]],~red,covariates=list(red=babyIm.redlevel[[1]]))
plot(ppmIm1red,se=FALSE,superimpose=FALSE,pause=FALSE)->trendIm1red

MtrendIm1grey<-as.matrix(trendIm1grey$trend[[1]])
MtrendIm1red<-as.matrix(trendIm1red$trend[[1]])
image(as.im(MtrendIm1grey-MtrendIm1red),main='difference between grey and red estimations')

# Pour toutes les images
par(mfrow=c(3,3))
for(i in 1:3) {
  ppmImgrey <- ppm(X_Im[[i]],~grey,covariates=list(grey=babyIm.greylevel[[i]]))
  plot(ppmImgrey,se=FALSE,superimpose=FALSE,pause=FALSE,main="grey estimation")->trendImgrey
  ppmImred <- ppm(X_Im[[i]],~red,covariates=list(red=babyIm.redlevel[[i]]))
  plot(ppmImred,se=FALSE,superimpose=FALSE,pause=FALSE,main="red estimation")->trendImred
  
  MtrendImgrey<-as.matrix(trendImgrey$trend[[1]])
  MtrendImred<-as.matrix(trendImred$trend[[1]])
  image(as.im(MtrendImgrey-MtrendImred),main='difference between grey and red estimations')
}

# Pour toutes les images
ppmImgrey <- list()
ppmImred <- list()
ppmImgreen <- list()
ppmImblue <- list()
for (i in 1:6){
  # Niveau gris
  ppmImgrey[[i]] <- ppm(unmark(X_Im[[i]]),~grey,covariates=list(grey=babyIm.greylevel[[i]]))
  # Niveau rouge
  ppmImred[[i]] <- ppm(unmark(X_Im[[i]]),~red,covariates=list(red=babyIm.redlevel[[i]]))
  # Niveau vert
  ppmImgreen[[i]] <- ppm(unmark(X_Im[[i]]),~green,covariates=list(green=babyIm.greenlevel[[i]]))
  # Niveau bleu
  ppmImblue[[i]] <- ppm(unmark(X_Im[[i]]),~blue,covariates=list(blue=babyIm.bluelevel[[i]]))
}

ppmImgrey[[1]]
ppmImblue[[1]]

ppmImgrey[[3]]
ppmImblue[[3]]

# Intensitées estimées, toutes les images, ajouteer ngrid=c(128,128) ?
for(i in 1:6){ 
par(mfrow=c(2,2))
plot(ppmImgrey[[i]],se=FALSE,superimpose=FALSE,pause=FALSE,main=paste("Niveau gris, Image",i))
plot(ppmImred[[i]],se=FALSE,superimpose=FALSE,pause=FALSE,main=paste("Niveau rouge, Image",i))
plot(ppmImgreen[[i]],se=FALSE,superimpose=FALSE,pause=FALSE,main=paste("Niveau vert, Image",i))
plot(ppmImblue[[i]],se=FALSE,superimpose=FALSE,pause=FALSE,main=paste("Niveau bleu, Image",i))
}

# Image gris
# Dans sa recommandation 709, qui concerne les couleurs « vraies » ou naturelles : 
# Gris = 0.2125 Rouge + 0.7154 Vert + 0.0721 Bleu
# Dans sa recommandation 601 pour les couleurs non-linéaires, c'est-à-dire avec correction du gamma (image vue à partir d'un écran vidéo) : 
# Gris = 0.299 Rouge + 0.587 Vert + 0.114 Bleu

# Image 1
# Autre méthode
pixmapGrey(r1)@grey
pixmapRGB(r1)@red
pixmapRGB(r1)@green
pixmapRGB(r1)@blue

pixmapGrey(r1)@grey==pixmapRGB(r1)@red
# Toujours le même problème, gris=red

# Pour toutes les images
r=list(r1,r2,r3,r4,r5,r6)
mat.grey=list()
mat.red=list()
mat.green=list()
mat.blue=list()

for(i in 1:6){
  mat.grey[[i]] <- pixmapGrey(r[[i]])@grey
  mat.red[[i]] <- pixmapRGB(r[[i]])@red
  mat.green[[i]] <- pixmapRGB(r[[i]])@green
  mat.blue[[i]] <- pixmapRGB(r[[i]])@blue
}
mat.grey[[i]]==mat.red[[i]]

# Tester Poisson inhomogène
# Image 1
# Adjusting for inhomogeneity
# K de Ripley, fonction inhomogène
X_Im[[1]]
plot(X_Im[[1]])
lambda1 <- density(X_Im[[1]], bw.ppl)
X_ImK <- Kinhom(X_Im[[1]],lambda1)
plot(X_ImK)

# lambda bien fixé ? 

# Enveloppes de confiance

# pair correlation function (pcf) inhomogène
g <- pcfinhom(X_Im[[1]])
plot(g)

# F
Finhom(cells)
plot(Finhom(cells))

# G
Ginhom(cells)
plot(Ginhom(cells))

# Construire enveloppes de confiance
lire et relire
effet contraste, factoriel
ppm RGB sur chacun des groupes, pour chacune des images
faire résumer des coefficients avec histogramme (simulations)
9.5
9.6
wikipedia poisson regression glm
lien

voir lien avec ppm

