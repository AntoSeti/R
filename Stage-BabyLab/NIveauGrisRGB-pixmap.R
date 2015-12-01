load("~/Desktop/MASSS/Stage/Codes R Antoine/WorkSpace_Antoine.RData")

m1 <- as.raster(r1, max = 255)
m2 <- as.raster(r2, max = 255)
m3 <- as.raster(r3, max = 255)
m4 <- as.raster(r4, max = 255)
m5 <- as.raster(r5, max = 255)
m6 <- as.raster(r6, max = 255)
m=list(m1,m2,m3,m4,m5,m6)

babyIm.greylevel=list(imagesgris[[1]],imagesgris[[2]],imagesgris[[3]],imagesgris[[4]],imagesgris[[5]],imagesgris[[6]])
babyIm.redlevel=list(imagesrouge[[1]],imagesrouge[[2]],imagesrouge[[3]],imagesrouge[[4]],imagesrouge[[5]],imagesrouge[[6]])
babyIm.greenlevel=list(imagesvert[[1]],imagesvert[[2]],imagesvert[[3]],imagesvert[[4]],imagesvert[[5]],imagesvert[[6]])
babyIm.bluelevel=list(imagesbleu[[1]],imagesbleu[[2]],imagesbleu[[3]],imagesbleu[[4]],imagesbleu[[5]],imagesbleu[[6]])

# Pour toutes les images
for(i in 1:6){
  par(mfrow=c(2,2))
  image(babyIm.greylevel[[i]]);points(X_Im[[i]],pch=3,cex=.4)
  image(babyIm.redlevel[[i]]);points(X_Im[[i]],pch=3,cex=.4)
  image(babyIm.greenlevel[[i]]);points(X_Im[[i]],pch=3,cex=.4)
  image(babyIm.bluelevel[[i]]);points(X_Im[[i]],pch=3,cex=.4)
}

# Fixations associées au niveau des couleurs
# Pour toutes les images
for(i in 1:6){
  par(mfrow=c(2,2))
  
  # Gris
  greylevel <- babyIm.greylevel[[i]]
  Ygreylevel <- X_Im[[i]] %mark% greylevel[X_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(Ygreylevel$marks))
  plot(Ygreylevel, pch=21, bg=A, cex=.5)
  
  # Rouge
  redlevel <- babyIm.redlevel[[i]]
  Yredlevel <- X_Im[[i]] %mark% redlevel[X_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(Yredlevel$marks))
  plot(Yredlevel, pch=21, bg=A, cex=.5)
  
  # Vert
  greenlevel <- babyIm.greenlevel[[i]]
  Ygreenlevel <- X_Im[[i]] %mark% greenlevel[X_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(Ygreenlevel$marks))
  plot(Ygreenlevel, pch=21, bg=A, cex=.5)
  
  # Bleu
  bluelevel <- babyIm.bluelevel[[i]]
  Ybluelevel <- X_Im[[i]] %mark% bluelevel[X_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(Ybluelevel$marks))
  plot(Ybluelevel, pch=21, bg=A, cex=.5)
}

