# Pour toutes les images, luminance HF
root="~/Desktop/SaliencyMaps/L001_Im00"
suffix=".txt"
babyIm.LumHFlevel <- list()
for (i in 1:6){
  Lum.HF_Im <- read.delim(paste(root,i,suffix,sep=""),header=FALSE, stringsAsFactors=FALSE)
  Lum.HF <- as.numeric(as.matrix(Lum.HF_Im))
  Lum.HF <- Lum.HF*(1/max(Lum.HF)) # Echelle [0;1]
  matrixLum.HF<-matrix(Lum.HF,nrow=768,ncol=1024,byrow=FALSE)
  nb_rows<-dim(matrixLum.HF)[1]
  nb_columns<-dim(matrixLum.HF)[2]
  flip_matrixLum.HF<-matrix(nrow=nb_rows,ncol=nb_columns)
  for(j in 1:nb_rows) { flip_matrixLum.HF[j,]=matrixLum.HF[nb_rows+1-j,] }
  matrixLum.HF<-flip_matrixLum.HF
  
  # Builds a pixel image in spatstat format
  babyIm.LumHFlevel[[i]]<-im(matrixLum.HF,xrange=c(0,1024),yrange=c(0,768))
}

# Cartes, niveau de luminance HF, pour toutes les images
par(mfrow=c(2,3))
for (i in 1:6){
  image(babyIm.LumHFlevel[[i]],main=paste("Carte niveau de luminances HF, Image"), i)
}

# Fixations avec niveau de luminance HF associé
par(mfrow=c(2,3))
for (i in 1:6){
  LumHFlevel <- babyIm.LumHFlevel[[i]]
  YLumHFlevel <- X_Im[[i]] %mark% LumHFlevel[X_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(YLumHFlevel$marks))
  plot(YLumHFlevel, pch=21, bg=A, cex=.8, main=paste("Niveau de luminances HF, Image"), i)
}

# Luminance Basse Frequence
# Pour toutes les images, luminance BF
root="~/Desktop/SaliencyMaps/L002_Im00"
suffix=".txt"
babyIm.LumBFlevel <- list()
for (i in 1:6){
  Lum.BF_Im <- read.delim(paste(root,i,suffix,sep=""),header=FALSE, stringsAsFactors=FALSE)
  Lum.BF <- as.numeric(as.matrix(Lum.BF_Im))
  Lum.BF <- Lum.BF*(1/max(Lum.BF))
  matrixLum.BF<-matrix(Lum.BF,nrow=768,ncol=1024,byrow=FALSE)
  nb_rows<-dim(matrixLum.BF)[1]
  nb_columns<-dim(matrixLum.BF)[2]
  flip_matrixLum.BF<-matrix(nrow=nb_rows,ncol=nb_columns)
  for(j in 1:nb_rows) { flip_matrixLum.BF[j,]=matrixLum.BF[nb_rows+1-j,] }
  matrixLum.BF<-flip_matrixLum.BF
  
  # Builds a pixel image in spatstat format
  babyIm.LumBFlevel[[i]]<-im(matrixLum.BF,xrange=c(0,1024),yrange=c(0,768))
}

# Cartes, niveau de luminance BF, pour toutes les images
par(mfrow=c(2,3))
for (i in 1:6){
  image(babyIm.LumBFlevel[[i]],main=paste("Carte niveau de luminances BF, Image"), i)
}

# Fixations avec niveau de luminance BF associé
par(mfrow=c(2,3))
for (i in 1:6){
  LumBFlevel <- babyIm.LumBFlevel[[i]]
  YLumBFlevel <- X_Im[[i]] %mark% LumBFlevel[X_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(YLumBFlevel$marks))
  plot(YLumBFlevel, pch=21, bg=A, cex=.8, main=paste("Niveau de luminances BF, Image"), i)
}

# Chromatique R/V Haute Frequence
# Pour toutes les images, chromatique R/V HF
root="~/Desktop/SaliencyMaps/L003_Im00"
suffix=".txt"
babyIm.Chro.RV.HFlevel <- list()
for (i in 1:6){
  Chro.RV.HF_Im <- read.delim(paste(root,i,suffix,sep=""),header=FALSE, stringsAsFactors=FALSE)
  Chro.RV.HF <- as.numeric(as.matrix(Chro.RV.HF_Im))
  Chro.RV.HF <- Chro.RV.HF*(1/max(Chro.RV.HF))
  matrixChro.RV.HF<-matrix(Chro.RV.HF,nrow=768,ncol=1024,byrow=FALSE)
  nb_rows<-dim(matrixChro.RV.HF)[1]
  nb_columns<-dim(matrixChro.RV.HF)[2]
  flip_matrixChro.RV.HF<-matrix(nrow=nb_rows,ncol=nb_columns)
  for(j in 1:nb_rows) { flip_matrixChro.RV.HF[j,]=matrixChro.RV.HF[nb_rows+1-j,] }
  matrixChro.RV.HF<-flip_matrixChro.RV.HF
  
  # Builds a pixel image in spatstat format
  babyIm.Chro.RV.HFlevel[[i]]<-im(matrixChro.RV.HF,xrange=c(0,1024),yrange=c(0,768))
}

# Cartes, niveau Chromatique R/V Haute Frequence, pour toutes les images
par(mfrow=c(2,3))
for (i in 1:6){
  image(babyIm.Chro.RV.HFlevel[[i]],main=paste("Carte niveau chromatique RV HF, Image"), i)
}

# Fixations avec niveau Chromatique R/V Haute Frequence associé
par(mfrow=c(2,3))
for (i in 1:6){
  Chro.RV.HFlevel <- babyIm.Chro.RV.HFlevel[[i]]
  YChro.RV.HFlevel <- X_Im[[i]] %mark% Chro.RV.HFlevel[X_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(YChro.RV.HFlevel$marks))
  plot(YChro.RV.HFlevel, pch=21, bg=A, cex=.8, main=paste("Niveau chromatique RV HF, Image"), i)
}

# Chromatique R/V Basse Frequence
# Pour toutes les images, chromatique R/V BF
root="~/Desktop/SaliencyMaps/L004_Im00"
suffix=".txt"
babyIm.Chro.RV.BFlevel <- list()
for (i in 1:6){
  Chro.RV.BF_Im <- read.delim(paste(root,i,suffix,sep=""),header=FALSE, stringsAsFactors=FALSE)
  Chro.RV.BF <- as.numeric(as.matrix(Chro.RV.BF_Im))
  Chro.RV.BF <- Chro.RV.BF*(1/max(Chro.RV.BF))
  matrixChro.RV.BF<-matrix(Chro.RV.BF,nrow=768,ncol=1024,byrow=FALSE)
  nb_rows<-dim(matrixChro.RV.BF)[1]
  nb_columns<-dim(matrixChro.RV.BF)[2]
  flip_matrixChro.RV.BF<-matrix(nrow=nb_rows,ncol=nb_columns)
  for(j in 1:nb_rows) { flip_matrixChro.RV.BF[j,]=matrixChro.RV.BF[nb_rows+1-j,] }
  matrixChro.RV.BF<-flip_matrixChro.RV.BF
  
  # Builds a pixel image in spatstat format
  babyIm.Chro.RV.BFlevel[[i]]<-im(matrixChro.RV.BF,xrange=c(0,1024),yrange=c(0,768))
}

# Cartes, niveau Chromatique R/V Haute Frequence, pour toutes les images
par(mfrow=c(2,3))
for (i in 1:6){
  image(babyIm.Chro.RV.BFlevel[[i]],main=paste("Carte niveau chromatique RV BF, Image"), i)
}

# Fixations avec niveau Chromatique R/V Haute Frequence associé
par(mfrow=c(2,3))
for (i in 1:6){
  Chro.RV.BFlevel <- babyIm.Chro.RV.BFlevel[[i]]
  YChro.RV.BFlevel <- X_Im[[i]] %mark% Chro.RV.BFlevel[X_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(YChro.RV.BFlevel$marks))
  plot(YChro.RV.BFlevel, pch=21, bg=A, cex=.8, main=paste("Niveau chromatique RV BF, Image"), i)
}

# Chromatique B/J Haute Frequence
# Pour toutes les images, chromatique B/J HF
root="~/Desktop/SaliencyMaps/L005_Im00"
suffix=".txt"
babyIm.Chro.BJ.HFlevel <- list()
for (i in 1:6){
  Chro.BJ.HF_Im <- read.delim(paste(root,i,suffix,sep=""),header=FALSE, stringsAsFactors=FALSE)
  Chro.BJ.HF <- as.numeric(as.matrix(Chro.BJ.HF_Im))
  Chro.BJ.HF <- Chro.BJ.HF*(1/max(Chro.BJ.HF))
  matrixChro.BJ.HF<-matrix(Chro.BJ.HF,nrow=768,ncol=1024,byrow=FALSE)
  nb_rows<-dim(matrixChro.BJ.HF)[1]
  nb_columns<-dim(matrixChro.BJ.HF)[2]
  flip_matrixChro.BJ.HF<-matrix(nrow=nb_rows,ncol=nb_columns)
  for(j in 1:nb_rows) { flip_matrixChro.BJ.HF[j,]=matrixChro.BJ.HF[nb_rows+1-j,] }
  matrixChro.BJ.HF<-flip_matrixChro.BJ.HF
  
  # Builds a pixel image in spatstat format
  babyIm.Chro.BJ.HFlevel[[i]]<-im(matrixChro.BJ.HF,xrange=c(0,1024),yrange=c(0,768))
}

# Cartes, niveau Chromatique B/J Haute Frequence, pour toutes les images
par(mfrow=c(2,3))
for (i in 1:6){
  image(babyIm.Chro.BJ.HFlevel[[i]],main=paste("Carte niveau chromatique BJ HF, Image"), i)
}

# Fixations avec niveau Chromatique B/J Haute Frequence associé
par(mfrow=c(2,3))
for (i in 1:6){
  Chro.BJ.HFlevel <- babyIm.Chro.BJ.HFlevel[[i]]
  YChro.BJ.HFlevel <- X_Im[[i]] %mark% Chro.BJ.HFlevel[X_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(YChro.BJ.HFlevel$marks))
  plot(YChro.BJ.HFlevel, pch=21, bg=A, cex=.8, main=paste("Niveau chromatique BJ HF, Image"), i)
}

# Chromatique B/J Basse Frequence
# Pour toutes les images, chromatique B/J BF
root="~/Desktop/SaliencyMaps/L006_Im00"
suffix=".txt"
babyIm.Chro.BJ.BFlevel <- list()
for (i in 1:6){
  Chro.BJ.BF_Im <- read.delim(paste(root,i,suffix,sep=""),header=FALSE, stringsAsFactors=FALSE)
  Chro.BJ.BF <- as.numeric(as.matrix(Chro.BJ.BF_Im))
  Chro.BJ.BF <- Chro.BJ.BF*(1/max(Chro.BJ.BF))
  matrixChro.BJ.BF<-matrix(Chro.BJ.BF,nrow=768,ncol=1024,byrow=FALSE)
  nb_rows<-dim(matrixChro.BJ.BF)[1]
  nb_columns<-dim(matrixChro.BJ.BF)[2]
  flip_matrixChro.BJ.BF<-matrix(nrow=nb_rows,ncol=nb_columns)
  for(j in 1:nb_rows) { flip_matrixChro.BJ.BF[j,]=matrixChro.BJ.BF[nb_rows+1-j,] }
  matrixChro.BJ.BF<-flip_matrixChro.BJ.BF
  
  # Builds a pixel image in spatstat format
  babyIm.Chro.BJ.BFlevel[[i]]<-im(matrixChro.BJ.BF,xrange=c(0,1024),yrange=c(0,768))
}

# Cartes, niveau Chromatique B/J Haute Frequence, pour toutes les images
par(mfrow=c(2,3))
for (i in 1:6){
  image(babyIm.Chro.BJ.BFlevel[[i]],main=paste("Carte niveau chromatique BJ BF, Image"), i)
}

# Fixations avec niveau Chromatique B/J Haute Frequence associé
par(mfrow=c(2,3))
for (i in 1:6){
  Chro.BJ.BFlevel <- babyIm.Chro.BJ.BFlevel[[i]]
  YChro.BJ.BFlevel <- X_Im[[i]] %mark% Chro.BJ.BFlevel[X_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(YChro.BJ.BFlevel$marks))
  plot(YChro.BJ.BFlevel, pch=21, bg=A, cex=.8, main=paste("Niveau chromatique BJ BF, Image"), i)
}

# Pour une même image, toutes les cartes de saillance
for(i in 1:6){
  par(mfrow=c(3,2))
  image(babyIm.LumHFlevel[[i]],main=paste("Carte niveau de luminances HF, Image"), i)
  image(babyIm.LumBFlevel[[i]],main=paste("Carte niveau de luminances BF, Image"), i)
  image(babyIm.Chro.RV.HFlevel[[i]],main=paste("Carte niveau chromatique RV HF, Image"), i)
  image(babyIm.Chro.RV.BFlevel[[i]],main=paste("Carte niveau chromatique RV BF, Image"), i)
  image(babyIm.Chro.BJ.HFlevel[[i]],main=paste("Carte niveau chromatique BJ HF, Image"), i)
  image(babyIm.Chro.BJ.BFlevel[[i]],main=paste("Carte niveau chromatique BJ BF, Image"), i)
}

# Moyenne des 6 cartes
# Pour toutes les images, moyenne des 6 cartes
root="~/Desktop/SaliencyMaps/L007_Im00"
suffix=".txt"
babyIm.Moylevel <- list()
for (i in 1:6){
  Moy_Im <- read.delim(paste(root,i,suffix,sep=""),header=FALSE, stringsAsFactors=FALSE)
  Moy <- as.numeric(as.matrix(Moy_Im))
  Moy <- Moy*(1/max(Moy))
  matrixMoy<-matrix(Moy,nrow=768,ncol=1024,byrow=FALSE)
  nb_rows<-dim(matrixMoy)[1]
  nb_columns<-dim(matrixMoy)[2]
  flip_matrixMoy<-matrix(nrow=nb_rows,ncol=nb_columns)
  for(j in 1:nb_rows) { flip_matrixMoy[j,]=matrixMoy[nb_rows+1-j,] }
  matrixMoy<-flip_matrixMoy
  
  # Builds a pixel image in spatstat format
  babyIm.Moylevel[[i]]<-im(matrixMoy,xrange=c(0,1024),yrange=c(0,768))
}

# Cartes, niveau saillance moyen, pour toutes les images
par(mfrow=c(2,3))
for (i in 1:6){
  image(babyIm.Moylevel[[i]],main="")
}

# Fixations avec niveau saillance moyen associé
for (i in 1:6){
  par(mfrow=c(2,3))
  Moylevel <- babyIm.Moylevel[[i]]
  YMoylevel <- X_3m_Im[[i]] %mark% Moylevel[X_3m_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(YMoylevel$marks))
  plot(YMoylevel, pch=21, bg=A, cex=.8, main=paste("Saillance moyen 3m, Image",i))
  
  Moylevel <- babyIm.Moylevel[[i]]
  YMoylevel <- X_6m_Im[[i]] %mark% Moylevel[X_6m_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(YMoylevel$marks))
  plot(YMoylevel, pch=21, bg=A, cex=.8, main=paste("Saillance moyen 6m, Image",i))
  
  Moylevel <- babyIm.Moylevel[[i]]
  YMoylevel <- X_9m_Im[[i]] %mark% Moylevel[X_9m_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(YMoylevel$marks))
  plot(YMoylevel, pch=21, bg=A, cex=.8, main=paste("Saillance moyen 9m, Image",i))
  
  Moylevel <- babyIm.Moylevel[[i]]
  YMoylevel <- X_12m_Im[[i]] %mark% Moylevel[X_12m_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(YMoylevel$marks))
  plot(YMoylevel, pch=21, bg=A, cex=.8, main=paste("Saillance moyen 12m, Image",i))
  
  Moylevel <- babyIm.Moylevel[[i]]
  YMoylevel <- X_adult_Im[[i]] %mark% Moylevel[X_adult_Im[[i]]]
  A <- colourmap(heat.colors(16), range=range(YMoylevel$marks))
  plot(YMoylevel, pch=21, bg=A, cex=.8, main=paste("Saillance moyen Ad, Image",i))
  
}

for(i in 1:6){
  par(mfrow=c(3,3))
  m <- as.raster(r[[i]], max = 255)
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main=paste("Image",i))
  usr <- par("usr")
  rasterImage(m, usr[1], usr[3], usr[2], usr[4]) 
  image(babyIm.LumHFlevel[[i]],main=paste("Carte niveau de luminances HF, Image"), i)
  image(babyIm.LumBFlevel[[i]],main=paste("Carte niveau de luminances BF, Image"), i)
  image(babyIm.Chro.RV.HFlevel[[i]],main=paste("Carte niveau chromatique RV HF, Image"), i)
  image(babyIm.Chro.RV.BFlevel[[i]],main=paste("Carte niveau chromatique RV BF, Image"), i)
  image(babyIm.Chro.BJ.HFlevel[[i]],main=paste("Carte niveau chromatique BJ HF, Image"), i)
  image(babyIm.Chro.BJ.BFlevel[[i]],main=paste("Carte niveau chromatique BJ BF, Image"), i)
  image(babyIm.Moylevel[[i]],main=paste("Carte moyenne, Image"), i)
  par(op)
}

# On effectue un ppm sur la covariable "moyenne" carte de saillance
# Pour toutes les images, groupe confondus
ppmCarteSail_Im <- list()
for (i in 1:6){
  # Niveau saillance moyen
  ppmCarteSail_Im[[i]] <- ppm(unmark(X_Im[[i]]),~sail,covariates=list(sail=babyIm.Moylevel[[i]]))
}
ppmCarteSail_Im

# Pour toutes les images, tous les groupes
ppmCarteSail_3m_Im <- list()
ppmCarteSail_6m_Im <- list()
ppmCarteSail_9m_Im <- list()
ppmCarteSail_12m_Im <- list()
ppmCarteSail_ad_Im <- list()

beta0_cartesail <- list()
beta1_cartesail <- list()

S.E.beta0_cartesail  <- list()
S.E.beta1_cartesail  <- list()

CI.lo.beta0_cartesail <- list()
CI.lo.beta1_cartesail <- list()

CI.hi.beta0_cartesail <- list()
CI.hi.beta1_cartesail <- list()

for (i in 1:6){
  # Bébés 3 mois
  ppmCarteSail_3m_Im[[i]] <- ppm(X_baby3month_Im[[i]],~sail,covariates=list(sail=babyIm.Moylevel[[i]]))
  # Bébés 6 mois
  ppmCarteSail_6m_Im[[i]] <- ppm(X_baby6month_Im[[i]],~sail,covariates=list(sail=babyIm.Moylevel[[i]]))
  # Bébés 9 mois
  ppmCarteSail_9m_Im[[i]] <- ppm(X_baby9month_Im[[i]],~sail,covariates=list(sail=babyIm.Moylevel[[i]]))
  # Bébés 12 mois
  ppmCarteSail_12m_Im[[i]] <- ppm(X_baby12month_Im[[i]],~sail,covariates=list(sail=babyIm.Moylevel[[i]]))
  # Adultes
  ppmCarteSail_ad_Im[[i]] <- ppm(X_adult_Im[[i]],~sail,covariates=list(sail=babyIm.Moylevel[[i]]))
  
  beta0_cartesail[[i]] <- c(summary(ppmCarteSail_3m_Im[[i]])$coefs.SE.CI$Estimate[1], summary(ppmCarteSail_6m_Im[[i]])$coefs.SE.CI$Estimate[1], summary(ppmCarteSail_9m_Im[[i]])$coefs.SE.CI$Estimate[1], summary(ppmCarteSail_12m_Im[[i]])$coefs.SE.CI$Estimate[1], summary(ppmCarteSail_ad_Im[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesail[[i]] <- c(summary(ppmCarteSail_3m_Im[[i]])$coefs.SE.CI$Estimate[2], summary(ppmCarteSail_6m_Im[[i]])$coefs.SE.CI$Estimate[2], summary(ppmCarteSail_9m_Im[[i]])$coefs.SE.CI$Estimate[2], summary(ppmCarteSail_12m_Im[[i]])$coefs.SE.CI$Estimate[2], summary(ppmCarteSail_ad_Im[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesail[[i]]  <- c(summary(ppmCarteSail_3m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmCarteSail_6m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmCarteSail_9m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmCarteSail_12m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmCarteSail_ad_Im[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesail[[i]]  <- c(summary(ppmCarteSail_3m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmCarteSail_6m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmCarteSail_9m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmCarteSail_12m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmCarteSail_ad_Im[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesail[[i]]  <- c(summary(ppmCarteSail_3m_Im[[i]])$coefs.SE.CI$CI95.lo[1],summary(ppmCarteSail_6m_Im[[i]])$coefs.SE.CI$CI95.lo[1],summary(ppmCarteSail_9m_Im[[i]])$coefs.SE.CI$CI95.lo[1],summary(ppmCarteSail_12m_Im[[i]])$coefs.SE.CI$CI95.lo[1],summary(ppmCarteSail_ad_Im[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesail[[i]]  <- c(summary(ppmCarteSail_3m_Im[[i]])$coefs.SE.CI$CI95.lo[2],summary(ppmCarteSail_6m_Im[[i]])$coefs.SE.CI$CI95.lo[2],summary(ppmCarteSail_9m_Im[[i]])$coefs.SE.CI$CI95.lo[2],summary(ppmCarteSail_12m_Im[[i]])$coefs.SE.CI$CI95.lo[2],summary(ppmCarteSail_ad_Im[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesail[[i]]  <- c(summary(ppmCarteSail_3m_Im[[i]])$coefs.SE.CI$CI95.hi[1],summary(ppmCarteSail_6m_Im[[i]])$coefs.SE.CI$CI95.hi[1],summary(ppmCarteSail_9m_Im[[i]])$coefs.SE.CI$CI95.hi[1],summary(ppmCarteSail_12m_Im[[i]])$coefs.SE.CI$CI95.hi[1],summary(ppmCarteSail_ad_Im[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesail[[i]]  <- c(summary(ppmCarteSail_3m_Im[[i]])$coefs.SE.CI$CI95.hi[2],summary(ppmCarteSail_6m_Im[[i]])$coefs.SE.CI$CI95.hi[2],summary(ppmCarteSail_9m_Im[[i]])$coefs.SE.CI$CI95.hi[2],summary(ppmCarteSail_12m_Im[[i]])$coefs.SE.CI$CI95.hi[2],summary(ppmCarteSail_ad_Im[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)
# Présenter sous forme de matrices/dataframe

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSail_Im <- list()
for(i in 1:6){
  resume.ppm_CarteSail_Im[[i]] <- data.frame(b0=beta0_cartesail[[i]],b1=beta1_cartesail[[i]],se.b0=S.E.beta0_cartesail[[i]],se.b1=S.E.beta1_cartesail[[i]],ci.lo.b0=CI.lo.beta0_cartesail[[i]],ci.hi.b0=CI.hi.beta0_cartesail[[i]],ci.lo.b1=CI.lo.beta1_cartesail[[i]],ci.hi.b1=CI.hi.beta1_cartesail[[i]])
  rownames(resume.ppm_CarteSail_Im[[i]])=c("bb3m","bb6m","bb9m","bb12m","adult")
}
resume.ppm_CarteSail_Im



# Faire maintenant par sujets
#### On peut facilement faire par groupes d'âges (sous ensemble sujet dans groupe)
# Pour toutes les images
# Sous groupe bébés 3 mois

x.l <- list(x11,x12,x13,x14,x15,x16)
y.l <- list(768-y11,768-y12,768-y13,768-y14,768-y15,768-y16)

mark11=vector()
mark12=vector()
mark13=vector()
mark14=vector()
mark15=vector()
mark16=vector()
for(i in 1:43) {
  mark11 <-c(mark11,rep(paste("Sujet",i),length(baby3month_Im[[i]]$Im1[,1])))
  mark12 <-c(mark12,rep(paste("Sujet",i),length(baby3month_Im[[i]]$Im2[,1])))
  mark13 <-c(mark13,rep(paste("Sujet",i),length(baby3month_Im[[i]]$Im3[,1])))
  mark14 <-c(mark14,rep(paste("Sujet",i),length(baby3month_Im[[i]]$Im4[,1])))
  mark15 <-c(mark15,rep(paste("Sujet",i),length(baby3month_Im[[i]]$Im5[,1])))
  mark16 <-c(mark16,rep(paste("Sujet",i),length(baby3month_Im[[i]]$Im6[,1])))
}

mark1=factor(mark11)
mark2=factor(mark12)
mark3=factor(mark13)
mark4=factor(mark14)
mark5=factor(mark15)
mark6=factor(mark16)
mark.l=list(mark1,mark2,mark3,mark4,mark5,mark6)

X_3m_Im <- list()
par(mfrow=c(2,3))
for (i in 1:6) {
  X_3m_Im[[i]] <- ppp(x.l[[i]],y.l[[i]], c(0,1024), c(0,768), unitname=c("pixels"), marks=mark.l[[i]])
  plot(X_3m_Im[[i]],paste("Fixations, Image",i,", selon les sujets de 3 mois"))
  points(X_3m_Im[[i]],col=mark.l[[i]])
}

# Aperçu
baby3month_Im

# Intéressons-nous à l'image 1
X_3m_Im[[1]]

# Visualisation, Sujet 14, bb 3m, Image 1
i=1
a <- X_3m_Im[[1]][marks(X_3m_Im[[1]]) == "Sujet 14"]
plot(a,paste("Fixations, Image",i,", Sujet 14"))

# ppm
# Pour Image 1, tous les sujets confondus
ppmCarteSailbb3m_Im1  <- ppm(unmark(X_3m_Im[[1]]),~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
ppmCarteSailbb3m_Im1

# Pour Image 1, sujet 1
i=1
a <- X_3m_Im[[1]][marks(X_3m_Im[[1]]) == paste("Sujet",i)]
ppmCarteSailbb3m_Im1  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
ppmCarteSailbb3m_Im1

# Pour Image 1, tous les sujets (tous sauf sujet 11) ayant une fixation sur l'image 1

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb3m_Im1 <- list()
beta0_cartesailbb3m <- list()
beta1_cartesailbb3m <- list()
S.E.beta0_cartesailbb3m <- list()
S.E.beta1_cartesailbb3m <- list()
CI.lo.beta0_cartesailbb3m <- list()
CI.lo.beta1_cartesailbb3m <- list()
CI.hi.beta0_cartesailbb3m <- list()
CI.hi.beta1_cartesailbb3m <- list()

for(i in c(1:10,12:41,43)){
  a <- X_3m_Im[[1]][marks(X_3m_Im[[1]]) == paste("Sujet",i)]
  ppmCarteSailbb3m_Im1[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailbb3m[[i]] <- c(summary(ppmCarteSailbb3m_Im1[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb3m[[i]] <- c(summary(ppmCarteSailbb3m_Im1[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im1[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im1[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im1[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im1[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im1[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im1[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb3m_Im1 <- list()
for(i in c(1:10,12:41,43)){
  resume.ppm_CarteSailbb3m_Im1[[i]] <- data.frame(b0=beta0_cartesailbb3m[[i]],b1=beta1_cartesailbb3m[[i]],se.b0=S.E.beta0_cartesailbb3m[[i]],se.b1=S.E.beta1_cartesailbb3m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb3m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb3m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb3m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb3m[[i]])
  rownames(resume.ppm_CarteSailbb3m_Im1[[i]])=c(i)
}
resume.ppm_CarteSailbb3m_Im1

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:10,12:41,43)){
b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im1[[i]][1]))))
b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im1[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im1, bb3m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im1, bb3m", col="lightblue")
plot(density(b1))

#### Image 2
# Pour Image 2, tous les sujets (tous sauf sujet 1) ayant une fixation sur l'image 2
X_3m_Im[[2]]

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb3m_Im2 <- list()
beta0_cartesailbb3m <- list()
beta1_cartesailbb3m <- list()
S.E.beta0_cartesailbb3m <- list()
S.E.beta1_cartesailbb3m <- list()
CI.lo.beta0_cartesailbb3m <- list()
CI.lo.beta1_cartesailbb3m <- list()
CI.hi.beta0_cartesailbb3m <- list()
CI.hi.beta1_cartesailbb3m <- list()

for(i in c(2:43)){
  a <- X_3m_Im[[2]][marks(X_3m_Im[[2]]) == paste("Sujet",i)]
  ppmCarteSailbb3m_Im2[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[2]]))
  
  beta0_cartesailbb3m[[i]] <- c(summary(ppmCarteSailbb3m_Im2[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb3m[[i]] <- c(summary(ppmCarteSailbb3m_Im2[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im2[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im2[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im2[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im2[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im2[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im2[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb3m_Im2 <- list()
for(i in c(2:43)){
  resume.ppm_CarteSailbb3m_Im2[[i]] <- data.frame(b0=beta0_cartesailbb3m[[i]],b1=beta1_cartesailbb3m[[i]],se.b0=S.E.beta0_cartesailbb3m[[i]],se.b1=S.E.beta1_cartesailbb3m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb3m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb3m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb3m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb3m[[i]])
  rownames(resume.ppm_CarteSailbb3m_Im2[[i]])=c(i)
}
resume.ppm_CarteSailbb3m_Im2

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(2:43)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im2[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im2[[i]][2]))))
}

hist(b0,10,main="Distribution coefficient beta0, Im2, bb3m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im2, bb3m", col="lightblue")
plot(density(b1))

# Probleme sujet 42, Im2
resume.ppm_CarteSailbb3m_Im2[[42]]
baby3month_Im[[42]]

# En eliminant nos 2 valeurs extrmes (sujet 16 et 42) :
# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(2:15,17:41,43)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im2[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im2[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im2, bb3m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im2, bb3m", col="lightblue")
plot(density(b1))

######### Image 3
# Pour Image 3, tous les sujets (tous sauf sujet 37) ayant une fixation sur l'image 3
X_3m_Im[[3]]

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb3m_Im3 <- list()
beta0_cartesailbb3m <- list()
beta1_cartesailbb3m <- list()
S.E.beta0_cartesailbb3m <- list()
S.E.beta1_cartesailbb3m <- list()
CI.lo.beta0_cartesailbb3m <- list()
CI.lo.beta1_cartesailbb3m <- list()
CI.hi.beta0_cartesailbb3m <- list()
CI.hi.beta1_cartesailbb3m <- list()

for(i in c(1:36,38:43)){
  a <- X_3m_Im[[3]][marks(X_3m_Im[[3]]) == paste("Sujet",i)]
  ppmCarteSailbb3m_Im3[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[2]]))
  
  beta0_cartesailbb3m[[i]] <- c(summary(ppmCarteSailbb3m_Im3[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb3m[[i]] <- c(summary(ppmCarteSailbb3m_Im3[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im3[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im3[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im3[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im3[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im3[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im3[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb3m_Im3 <- list()
for(i in c(1:36,38:43)){
  resume.ppm_CarteSailbb3m_Im3[[i]] <- data.frame(b0=beta0_cartesailbb3m[[i]],b1=beta1_cartesailbb3m[[i]],se.b0=S.E.beta0_cartesailbb3m[[i]],se.b1=S.E.beta1_cartesailbb3m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb3m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb3m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb3m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb3m[[i]])
  rownames(resume.ppm_CarteSailbb3m_Im3[[i]])=c(i)
}
resume.ppm_CarteSailbb3m_Im3

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:36,39:43)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im3[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im3[[i]][2]))))
}

hist(b0,10,main="Distribution coefficient beta0, Im3, bb3m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im3, bb3m", col="lightblue")
plot(density(b1))

# En supprimant la valeur extrême, individu 18, 38
# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:17,19:36,39:43)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im3[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im3[[i]][2]))))
}

hist(b0,10,main="Distribution coefficient beta0, Im3, bb3m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im3, bb3m", col="lightblue")
plot(density(b1))

### Image 4
# Pour Image 4, tous les sujets ont au moins une fixation sur l'image
X_3m_Im[[4]]

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb3m_Im4 <- list()
beta0_cartesailbb3m <- list()
beta1_cartesailbb3m <- list()
S.E.beta0_cartesailbb3m <- list()
S.E.beta1_cartesailbb3m <- list()
CI.lo.beta0_cartesailbb3m <- list()
CI.lo.beta1_cartesailbb3m <- list()
CI.hi.beta0_cartesailbb3m <- list()
CI.hi.beta1_cartesailbb3m <- list()

for(i in c(1:43)){
  a <- X_3m_Im[[4]][marks(X_3m_Im[[4]]) == paste("Sujet",i)]
  ppmCarteSailbb3m_Im4[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[2]]))
  
  beta0_cartesailbb3m[[i]] <- c(summary(ppmCarteSailbb3m_Im4[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb3m[[i]] <- c(summary(ppmCarteSailbb3m_Im4[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im4[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im4[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im4[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im4[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im4[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im4[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb3m_Im4<- list()
for(i in c(1:43)){
  resume.ppm_CarteSailbb3m_Im4[[i]] <- data.frame(b0=beta0_cartesailbb3m[[i]],b1=beta1_cartesailbb3m[[i]],se.b0=S.E.beta0_cartesailbb3m[[i]],se.b1=S.E.beta1_cartesailbb3m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb3m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb3m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb3m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb3m[[i]])
  rownames(resume.ppm_CarteSailbb3m_Im4[[i]])=c(i)
}
resume.ppm_CarteSailbb3m_Im4

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:43)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im4[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im4[[i]][2]))))
}

hist(b0,10,main="Distribution coefficient beta0, Im4, bb3m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im4, bb3m", col="lightblue")
plot(density(b1))

# Individu 10, 39 en moins
# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:9,11:38,40:43)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im4[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im4[[i]][2]))))
}

hist(b0,10,main="Distribution coefficient beta0, Im4, bb3m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im4, bb3m", col="lightblue")
plot(density(b1))

### Image 5
# Pour Image 5, tous les sujets (tous sauf sujet 40) ayant une fixation sur l'image 5
X_3m_Im[[5]]

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb3m_Im5 <- list()
beta0_cartesailbb3m <- list()
beta1_cartesailbb3m <- list()
S.E.beta0_cartesailbb3m <- list()
S.E.beta1_cartesailbb3m <- list()
CI.lo.beta0_cartesailbb3m <- list()
CI.lo.beta1_cartesailbb3m <- list()
CI.hi.beta0_cartesailbb3m <- list()
CI.hi.beta1_cartesailbb3m <- list()

for(i in c(1:39,41:43)){
  a <- X_3m_Im[[5]][marks(X_3m_Im[[5]]) == paste("Sujet",i)]
  ppmCarteSailbb3m_Im5[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[2]]))
  
  beta0_cartesailbb3m[[i]] <- c(summary(ppmCarteSailbb3m_Im5[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb3m[[i]] <- c(summary(ppmCarteSailbb3m_Im5[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im5[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im5[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im5[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im5[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im5[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im5[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb3m_Im5 <- list()
for(i in c(1:39,41:43)){
  resume.ppm_CarteSailbb3m_Im5[[i]] <- data.frame(b0=beta0_cartesailbb3m[[i]],b1=beta1_cartesailbb3m[[i]],se.b0=S.E.beta0_cartesailbb3m[[i]],se.b1=S.E.beta1_cartesailbb3m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb3m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb3m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb3m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb3m[[i]])
  rownames(resume.ppm_CarteSailbb3m_Im5[[i]])=c(i)
}
resume.ppm_CarteSailbb3m_Im5

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:39,41:43)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im5[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im5[[i]][2]))))
}

hist(b0,10,main="Distribution coefficient beta0, Im5, bb3m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im5, bb3m", col="lightblue")
plot(density(b1))

# En supprimant individu 17
# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:16,18:39,41:43)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im5[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im5[[i]][2]))))
}

hist(b0,10,main="Distribution coefficient beta0, Im5, bb3m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im5, bb3m", col="lightblue")
plot(density(b1))

### Image 6
# Pour Image 6, tous les sujets (tous sauf sujet 3, 11, 37) ayant une fixation sur l'image 6
X_3m_Im[[6]]

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb3m_Im6 <- list()
beta0_cartesailbb3m <- list()
beta1_cartesailbb3m <- list()
S.E.beta0_cartesailbb3m <- list()
S.E.beta1_cartesailbb3m <- list()
CI.lo.beta0_cartesailbb3m <- list()
CI.lo.beta1_cartesailbb3m <- list()
CI.hi.beta0_cartesailbb3m <- list()
CI.hi.beta1_cartesailbb3m <- list()

for(i in c(1:2,4:10,12:36,38:43)){
  a <- X_3m_Im[[6]][marks(X_3m_Im[[6]]) == paste("Sujet",i)]
  ppmCarteSailbb3m_Im6[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[2]]))
  
  beta0_cartesailbb3m[[i]] <- c(summary(ppmCarteSailbb3m_Im6[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb3m[[i]] <- c(summary(ppmCarteSailbb3m_Im6[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im6[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im6[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im6[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im6[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im6[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb3m[[i]]  <- c(summary(ppmCarteSailbb3m_Im6[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb3m_Im6 <- list()
for(i in c(1:2,4:10,12:36,38:43)){
  resume.ppm_CarteSailbb3m_Im6[[i]] <- data.frame(b0=beta0_cartesailbb3m[[i]],b1=beta1_cartesailbb3m[[i]],se.b0=S.E.beta0_cartesailbb3m[[i]],se.b1=S.E.beta1_cartesailbb3m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb3m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb3m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb3m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb3m[[i]])
  rownames(resume.ppm_CarteSailbb3m_Im6[[i]])=c(i)
}
resume.ppm_CarteSailbb3m_Im6

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:2,4:10,12:36,38:43)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im6[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im6[[i]][2]))))
}

hist(b0,10,main="Distribution coefficient beta0, Im6, bb3m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im6, bb3m", col="lightblue")
plot(density(b1))

# Individu 28 en moins
# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:2,4:10,12:27,29:36,38:43)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im6[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb3m_Im6[[i]][2]))))
}

hist(b0,10,main="Distribution coefficient beta0, Im6, bb3m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im6, bb3m", col="lightblue")
plot(density(b1))


# Exemple bb6m, Im1
# Pour toutes les images
# Sous groupe bébés 6 mois
x.l <- list(x21,x22,x23,x24,x25,x26)
y.l <- list(768-y21,768-y22,768-y23,768-y24,768-y25,768-y26)

mark11=vector()
mark12=vector()
mark13=vector()
mark14=vector()
mark15=vector()
mark16=vector()
for(i in 1:47) {
  mark11 <-c(mark11,rep(paste("Sujet",43+i),length(baby6month_Im[[i]]$Im1[,1])))
  mark12 <-c(mark12,rep(paste("Sujet",43+i),length(baby6month_Im[[i]]$Im2[,1])))
  mark13 <-c(mark13,rep(paste("Sujet",43+i),length(baby6month_Im[[i]]$Im3[,1])))
  mark14 <-c(mark14,rep(paste("Sujet",43+i),length(baby6month_Im[[i]]$Im4[,1])))
  mark15 <-c(mark15,rep(paste("Sujet",43+i),length(baby6month_Im[[i]]$Im5[,1])))
  mark16 <-c(mark16,rep(paste("Sujet",43+i),length(baby6month_Im[[i]]$Im6[,1])))
}

mark1=factor(mark11)
mark2=factor(mark12)
mark3=factor(mark13)
mark4=factor(mark14)
mark5=factor(mark15)
mark6=factor(mark16)
mark.l=list(mark1,mark2,mark3,mark4,mark5,mark6)

X_6m_Im <- list()
par(mfrow=c(2,3))
for (i in 1:6) {
  X_6m_Im[[i]] <- ppp(x.l[[i]],y.l[[i]], c(0,1024), c(0,768), unitname=c("pixels"), marks=mark.l[[i]])
  plot(X_6m_Im[[i]],paste("Fixations, Image",i,", selon les sujets de 6 mois"))
  points(X_6m_Im[[i]],col=mark.l[[i]])
}

# Aperçu
baby6month_Im[[20]]

# Intéressons-nous à l'image 1
X_6m_Im[[1]]

# Manque sujet 63 et 73 (fixations, image 1)
baby6month_Im[[20]]
baby6month_Im[[30]]

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb6m_Im1 <- list()
beta0_cartesailbb6m <- list()
beta1_cartesailbb6m <- list()
S.E.beta0_cartesailbb6m <- list()
S.E.beta1_cartesailbb6m <- list()
CI.lo.beta0_cartesailbb6m <- list()
CI.lo.beta1_cartesailbb6m <- list()
CI.hi.beta0_cartesailbb6m <- list()
CI.hi.beta1_cartesailbb6m <- list()

for(i in c(1:19,21:29,31:47)){
  a <- X_6m_Im[[1]][marks(X_6m_Im[[1]]) == paste("Sujet",43+i)]
  ppmCarteSailbb6m_Im1[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailbb6m[[i]] <- c(summary(ppmCarteSailbb6m_Im1[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb6m[[i]] <- c(summary(ppmCarteSailbb6m_Im1[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im1[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im1[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im1[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im1[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im1[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im1[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb6m_Im1 <- list()
for(i in c(1:19,21:29,31:47)){
  resume.ppm_CarteSailbb6m_Im1[[i]] <- data.frame(b0=beta0_cartesailbb6m[[i]],b1=beta1_cartesailbb6m[[i]],se.b0=S.E.beta0_cartesailbb6m[[i]],se.b1=S.E.beta1_cartesailbb6m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb6m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb6m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb6m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb6m[[i]])
  rownames(resume.ppm_CarteSailbb6m_Im1[[i]])=c(43+i)
}
resume.ppm_CarteSailbb6m_Im1

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:19,21:29,31:47)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb6m_Im1[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb6m_Im1[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im1, bb6m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im1, bb6m", col="lightblue")
plot(density(b1))

# En enlevant le sujet problématique (sujet 14)
# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:13,15:19,21:29,31:47)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb6m_Im1[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb6m_Im1[[i]][2]))))
}

par(mfrow=c(1,2))
hist(b0,main="Distribution coefficient beta0, Im1, bb6m", col="lightblue")
hist(b1,main="Distribution coefficient beta1, Im1, bb6m", col="lightblue")

# Intéressons-nous à l'image 2
X_6m_Im[[2]]

# Manque sujet 59 et 73, 90 (fixations, image 2)
baby6month_Im[[16]]
baby6month_Im[[30]]
baby6month_Im[[46]]

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb6m_Im2 <- list()
beta0_cartesailbb6m <- list()
beta1_cartesailbb6m <- list()
S.E.beta0_cartesailbb6m <- list()
S.E.beta1_cartesailbb6m <- list()
CI.lo.beta0_cartesailbb6m <- list()
CI.lo.beta1_cartesailbb6m <- list()
CI.hi.beta0_cartesailbb6m <- list()
CI.hi.beta1_cartesailbb6m <- list()

for(i in c(1:15,17:29,31:46)){
  a <- X_6m_Im[[2]][marks(X_6m_Im[[2]]) == paste("Sujet",43+i)]
  ppmCarteSailbb6m_Im2[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailbb6m[[i]] <- c(summary(ppmCarteSailbb6m_Im2[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb6m[[i]] <- c(summary(ppmCarteSailbb6m_Im2[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im2[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im2[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im2[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im2[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im2[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im2[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb6m_Im2 <- list()
for(i in c(1:15,17:29,31:46)){
  resume.ppm_CarteSailbb6m_Im2[[i]] <- data.frame(b0=beta0_cartesailbb6m[[i]],b1=beta1_cartesailbb6m[[i]],se.b0=S.E.beta0_cartesailbb6m[[i]],se.b1=S.E.beta1_cartesailbb6m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb6m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb6m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb6m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb6m[[i]])
  rownames(resume.ppm_CarteSailbb6m_Im2[[i]])=c(43+i)
}
resume.ppm_CarteSailbb6m_Im2

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:15,17:29,31:46)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb6m_Im2[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb6m_Im2[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im2, bb6m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im2, bb6m", col="lightblue")
plot(density(b1))

## Image 3
X_6m_Im[[3]]

# Manque sujet 45 et 47, 63, 73 (fixations, image 3)
baby6month_Im[[2]]
baby6month_Im[[4]]
baby6month_Im[[20]]
baby6month_Im[[30]]

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb6m_Im3 <- list()
beta0_cartesailbb6m <- list()
beta1_cartesailbb6m <- list()
S.E.beta0_cartesailbb6m <- list()
S.E.beta1_cartesailbb6m <- list()
CI.lo.beta0_cartesailbb6m <- list()
CI.lo.beta1_cartesailbb6m <- list()
CI.hi.beta0_cartesailbb6m <- list()
CI.hi.beta1_cartesailbb6m <- list()

for(i in c(1,3,5:19,21:29,31:47)){
  a <- X_6m_Im[[3]][marks(X_6m_Im[[3]]) == paste("Sujet",43+i)]
  ppmCarteSailbb6m_Im3[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailbb6m[[i]] <- c(summary(ppmCarteSailbb6m_Im3[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb6m[[i]] <- c(summary(ppmCarteSailbb6m_Im3[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im3[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im3[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im3[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im3[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im3[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im3[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb6m_Im3 <- list()
for(i in c(1,3,5:19,21:29,31:47)){
  resume.ppm_CarteSailbb6m_Im3[[i]] <- data.frame(b0=beta0_cartesailbb6m[[i]],b1=beta1_cartesailbb6m[[i]],se.b0=S.E.beta0_cartesailbb6m[[i]],se.b1=S.E.beta1_cartesailbb6m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb6m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb6m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb6m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb6m[[i]])
  rownames(resume.ppm_CarteSailbb6m_Im3[[i]])=c(43+i)
}
resume.ppm_CarteSailbb6m_Im3

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1,3,5:19,21:29,31:47)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb6m_Im3[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb6m_Im3[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im3, bb6m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im3, bb6m", col="lightblue")
plot(density(b1))

# Probleme individu 18
# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1,3,5:17,19,21:29,31:47)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb6m_Im3[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb6m_Im3[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im3, bb6m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im3, bb6m", col="lightblue")
plot(density(b1))


## Image 4
X_6m_Im[[4]]

# Manque sujet 68 (fixations, image 4)
baby6month_Im[[25]]

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb6m_Im4 <- list()
beta0_cartesailbb6m <- list()
beta1_cartesailbb6m <- list()
S.E.beta0_cartesailbb6m <- list()
S.E.beta1_cartesailbb6m <- list()
CI.lo.beta0_cartesailbb6m <- list()
CI.lo.beta1_cartesailbb6m <- list()
CI.hi.beta0_cartesailbb6m <- list()
CI.hi.beta1_cartesailbb6m <- list()

for(i in c(1:24,26:47)){
  a <- X_6m_Im[[4]][marks(X_6m_Im[[4]]) == paste("Sujet",43+i)]
  ppmCarteSailbb6m_Im4[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailbb6m[[i]] <- c(summary(ppmCarteSailbb6m_Im4[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb6m[[i]] <- c(summary(ppmCarteSailbb6m_Im4[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im4[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im4[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im4[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im4[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im4[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im4[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb6m_Im4 <- list()
for(i in c(1:24,26:47)){
  resume.ppm_CarteSailbb6m_Im4[[i]] <- data.frame(b0=beta0_cartesailbb6m[[i]],b1=beta1_cartesailbb6m[[i]],se.b0=S.E.beta0_cartesailbb6m[[i]],se.b1=S.E.beta1_cartesailbb6m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb6m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb6m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb6m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb6m[[i]])
  rownames(resume.ppm_CarteSailbb6m_Im4[[i]])=c(43+i)
}
resume.ppm_CarteSailbb6m_Im4

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:24,26:47)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb6m_Im4[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb6m_Im4[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im4, bb6m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im4, bb6m", col="lightblue")
plot(density(b1))

# On supprime l'individu 16 et 21
# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:15,17:20,22:24,26:47)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb6m_Im4[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb6m_Im4[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im4, bb6m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im4, bb6m", col="lightblue")
plot(density(b1))


### Image 5
X_6m_Im[[5]]

# Manque sujet 47, 59, 63, 68, 73 (fixations, image 5)
baby6month_Im[[4]]
baby6month_Im[[16]]
baby6month_Im[[20]]
baby6month_Im[[25]]
baby6month_Im[[30]]

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb6m_Im5 <- list()
beta0_cartesailbb6m <- list()
beta1_cartesailbb6m <- list()
S.E.beta0_cartesailbb6m <- list()
S.E.beta1_cartesailbb6m <- list()
CI.lo.beta0_cartesailbb6m <- list()
CI.lo.beta1_cartesailbb6m <- list()
CI.hi.beta0_cartesailbb6m <- list()
CI.hi.beta1_cartesailbb6m <- list()

for(i in c(1:3,5:15,17:19,21:24,26:29,31:47)){
  a <- X_6m_Im[[5]][marks(X_6m_Im[[5]]) == paste("Sujet",43+i)]
  ppmCarteSailbb6m_Im5[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailbb6m[[i]] <- c(summary(ppmCarteSailbb6m_Im5[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb6m[[i]] <- c(summary(ppmCarteSailbb6m_Im5[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im5[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im5[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im5[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im5[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im5[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im5[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb6m_Im5 <- list()
for(i in c(1:3,5:15,17:19,21:24,26:29,31:47)){
  resume.ppm_CarteSailbb6m_Im5[[i]] <- data.frame(b0=beta0_cartesailbb6m[[i]],b1=beta1_cartesailbb6m[[i]],se.b0=S.E.beta0_cartesailbb6m[[i]],se.b1=S.E.beta1_cartesailbb6m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb6m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb6m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb6m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb6m[[i]])
  rownames(resume.ppm_CarteSailbb6m_Im5[[i]])=c(43+i)
}
resume.ppm_CarteSailbb6m_Im5

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:3,5:15,17:19,21:24,26:29,31:47)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb6m_Im5[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb6m_Im5[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im5, bb6m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im5, bb6m", col="lightblue")
plot(density(b1))


# On enleve l'individu  23 et 43
# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:3,5:15,17:19,21,22,24,26:29,31:42,44:47)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb6m_Im5[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb6m_Im5[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im5, bb6m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im5, bb6m", col="lightblue")
plot(density(b1))

### Image 6
X_6m_Im[[6]]

# Manque sujet 59, 63, 73 (fixations, image 6)
baby6month_Im[[16]]
baby6month_Im[[20]]
baby6month_Im[[30]]

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb6m_Im6 <- list()
beta0_cartesailbb6m <- list()
beta1_cartesailbb6m <- list()
S.E.beta0_cartesailbb6m <- list()
S.E.beta1_cartesailbb6m <- list()
CI.lo.beta0_cartesailbb6m <- list()
CI.lo.beta1_cartesailbb6m <- list()
CI.hi.beta0_cartesailbb6m <- list()
CI.hi.beta1_cartesailbb6m <- list()

for(i in c(1:15,17:19,21:29,31:47)){
  a <- X_6m_Im[[6]][marks(X_6m_Im[[6]]) == paste("Sujet",43+i)]
  ppmCarteSailbb6m_Im6[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailbb6m[[i]] <- c(summary(ppmCarteSailbb6m_Im6[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb6m[[i]] <- c(summary(ppmCarteSailbb6m_Im6[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im6[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im6[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im6[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im6[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im6[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb6m[[i]]  <- c(summary(ppmCarteSailbb6m_Im6[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb6m_Im6 <- list()
for(i in c(1:15,17:19,21:29,31:47)){
  resume.ppm_CarteSailbb6m_Im6[[i]] <- data.frame(b0=beta0_cartesailbb6m[[i]],b1=beta1_cartesailbb6m[[i]],se.b0=S.E.beta0_cartesailbb6m[[i]],se.b1=S.E.beta1_cartesailbb6m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb6m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb6m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb6m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb6m[[i]])
  rownames(resume.ppm_CarteSailbb6m_Im6[[i]])=c(43+i)
}
resume.ppm_CarteSailbb6m_Im6

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:15,17:19,21:29,31:47)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb6m_Im6[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb6m_Im6[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im6, bb6m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im6, bb6m", col="lightblue")
plot(density(b1))

### Groupe 9 mois
x.l <- list(x31,x32,x33,x34,x35,x36)
y.l <- list(768-y31,768-y32,768-y33,768-y34,768-y35,768-y36)

mark11=vector()
mark12=vector()
mark13=vector()
mark14=vector()
mark15=vector()
mark16=vector()
for(i in 1:44) {
  mark11 <-c(mark11,rep(paste("Sujet",43+47+i),length(baby9month_Im[[i]]$Im1[,1])))
  mark12 <-c(mark12,rep(paste("Sujet",43+47+i),length(baby9month_Im[[i]]$Im2[,1])))
  mark13 <-c(mark13,rep(paste("Sujet",43+47+i),length(baby9month_Im[[i]]$Im3[,1])))
  mark14 <-c(mark14,rep(paste("Sujet",43+47+i),length(baby9month_Im[[i]]$Im4[,1])))
  mark15 <-c(mark15,rep(paste("Sujet",43+47+i),length(baby9month_Im[[i]]$Im5[,1])))
  mark16 <-c(mark16,rep(paste("Sujet",43+47+i),length(baby9month_Im[[i]]$Im6[,1])))
}

mark1=factor(mark11)
mark2=factor(mark12)
mark3=factor(mark13)
mark4=factor(mark14)
mark5=factor(mark15)
mark6=factor(mark16)
mark.l=list(mark1,mark2,mark3,mark4,mark5,mark6)

X_9m_Im <- list()
par(mfrow=c(2,3))
for (i in 1:6) {
  X_9m_Im[[i]] <- ppp(x.l[[i]],y.l[[i]], c(0,1024), c(0,768), unitname=c("pixels"), marks=mark.l[[i]])
  #plot(X_9m_Im[[i]],paste("Fixations, Image",i,", selon les sujets de 9 mois"))
  #points(X_9m_Im[[i]],col=mark.l[[i]])
}

# Aperçu
baby9month_Im[[20]]

# Intéressons-nous à l'image 1
X_9m_Im[[1]]

# Tous les sujets ont des fixations sur cette image

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb9m_Im1 <- list()
beta0_cartesailbb9m <- list()
beta1_cartesailbb9m <- list()
S.E.beta0_cartesailbb9m <- list()
S.E.beta1_cartesailbb9m <- list()
CI.lo.beta0_cartesailbb9m <- list()
CI.lo.beta1_cartesailbb9m <- list()
CI.hi.beta0_cartesailbb9m <- list()
CI.hi.beta1_cartesailbb9m <- list()

for(i in c(1:44)){
  a <- X_9m_Im[[1]][marks(X_9m_Im[[1]]) == paste("Sujet",43+47+i)]
  ppmCarteSailbb9m_Im1[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailbb9m[[i]] <- c(summary(ppmCarteSailbb9m_Im1[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb9m[[i]] <- c(summary(ppmCarteSailbb9m_Im1[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im1[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im1[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im1[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im1[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im1[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im1[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb9m_Im1 <- list()
for(i in c(1:44)){
  resume.ppm_CarteSailbb9m_Im1[[i]] <- data.frame(b0=beta0_cartesailbb9m[[i]],b1=beta1_cartesailbb9m[[i]],se.b0=S.E.beta0_cartesailbb9m[[i]],se.b1=S.E.beta1_cartesailbb9m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb9m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb9m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb9m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb9m[[i]])
  rownames(resume.ppm_CarteSailbb9m_Im1[[i]])=c(43+47+i)
}
resume.ppm_CarteSailbb9m_Im1

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:44)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im1[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im1[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im1, bb9m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im1, bb9m", col="lightblue")
plot(density(b1))

# Intéressons-nous à l'image 2
X_9m_Im[[2]]

# Tous les sujets ont des fixations sur cette image, sauf sujet 106 et 108
baby9month_Im[[16]]
baby9month_Im[[18]]

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb9m_Im2 <- list()
beta0_cartesailbb9m <- list()
beta1_cartesailbb9m <- list()
S.E.beta0_cartesailbb9m <- list()
S.E.beta1_cartesailbb9m <- list()
CI.lo.beta0_cartesailbb9m <- list()
CI.lo.beta1_cartesailbb9m <- list()
CI.hi.beta0_cartesailbb9m <- list()
CI.hi.beta1_cartesailbb9m <- list()

for(i in c(1:15,17,19:44)){
  a <- X_9m_Im[[2]][marks(X_9m_Im[[2]]) == paste("Sujet",43+47+i)]
  ppmCarteSailbb9m_Im2[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailbb9m[[i]] <- c(summary(ppmCarteSailbb9m_Im2[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb9m[[i]] <- c(summary(ppmCarteSailbb9m_Im2[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im2[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im2[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im2[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im2[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im2[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im2[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb9m_Im2 <- list()
for(i in c(1:15,17,19:44)){
  resume.ppm_CarteSailbb9m_Im2[[i]] <- data.frame(b0=beta0_cartesailbb9m[[i]],b1=beta1_cartesailbb9m[[i]],se.b0=S.E.beta0_cartesailbb9m[[i]],se.b1=S.E.beta1_cartesailbb9m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb9m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb9m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb9m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb9m[[i]])
  rownames(resume.ppm_CarteSailbb9m_Im2[[i]])=c(43+47+i)
}
resume.ppm_CarteSailbb9m_Im2

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:15,17,19:44)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im2[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im2[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im2, bb9m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im2, bb9m", col="lightblue")
plot(density(b1))

# On enleve l'individu 10 et 22
# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:9,11:17,19:21,23:44)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im2[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im2[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im2, bb9m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im2, bb9m", col="lightblue")
plot(density(b1))

# Intéressons-nous à l'image 3
X_9m_Im[[3]]

# Tous les sujets ont des fixations sur cette image, sauf sujet 103 et 108
baby9month_Im[[13]]
baby9month_Im[[18]]

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb9m_Im3 <- list()
beta0_cartesailbb9m <- list()
beta1_cartesailbb9m <- list()
S.E.beta0_cartesailbb9m <- list()
S.E.beta1_cartesailbb9m <- list()
CI.lo.beta0_cartesailbb9m <- list()
CI.lo.beta1_cartesailbb9m <- list()
CI.hi.beta0_cartesailbb9m <- list()
CI.hi.beta1_cartesailbb9m <- list()

for(i in c(1:15,17,19:44)){
  a <- X_9m_Im[[3]][marks(X_9m_Im[[3]]) == paste("Sujet",43+47+i)]
  ppmCarteSailbb9m_Im3[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailbb9m[[i]] <- c(summary(ppmCarteSailbb9m_Im3[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb9m[[i]] <- c(summary(ppmCarteSailbb9m_Im3[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im3[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im3[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im3[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im3[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im3[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im3[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb9m_Im3 <- list()
for(i in c(1:15,17,19:44)){
  resume.ppm_CarteSailbb9m_Im3[[i]] <- data.frame(b0=beta0_cartesailbb9m[[i]],b1=beta1_cartesailbb9m[[i]],se.b0=S.E.beta0_cartesailbb9m[[i]],se.b1=S.E.beta1_cartesailbb9m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb9m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb9m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb9m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb9m[[i]])
  rownames(resume.ppm_CarteSailbb9m_Im3[[i]])=c(43+47+i)
}
resume.ppm_CarteSailbb9m_Im3

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:15,17,19:44)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im3[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im3[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im3, bb9m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im3, bb9m", col="lightblue")
plot(density(b1))

# On supprime l'individu 13
# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:12,14:15,17,19:44)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im3[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im3[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im3, bb9m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im3, bb9m", col="lightblue")
plot(density(b1))

# Intéressons-nous à l'image 4
X_9m_Im[[4]]

# Tous les sujets ont des fixations sur cette image

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb9m_Im4 <- list()
beta0_cartesailbb9m <- list()
beta1_cartesailbb9m <- list()
S.E.beta0_cartesailbb9m <- list()
S.E.beta1_cartesailbb9m <- list()
CI.lo.beta0_cartesailbb9m <- list()
CI.lo.beta1_cartesailbb9m <- list()
CI.hi.beta0_cartesailbb9m <- list()
CI.hi.beta1_cartesailbb9m <- list()

for(i in c(1:44)){
  a <- X_9m_Im[[4]][marks(X_9m_Im[[4]]) == paste("Sujet",43+47+i)]
  ppmCarteSailbb9m_Im4[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailbb9m[[i]] <- c(summary(ppmCarteSailbb9m_Im4[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb9m[[i]] <- c(summary(ppmCarteSailbb9m_Im4[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im4[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im4[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im4[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im4[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im4[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im4[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb9m_Im4 <- list()
for(i in c(1:44)){
  resume.ppm_CarteSailbb9m_Im4[[i]] <- data.frame(b0=beta0_cartesailbb9m[[i]],b1=beta1_cartesailbb9m[[i]],se.b0=S.E.beta0_cartesailbb9m[[i]],se.b1=S.E.beta1_cartesailbb9m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb9m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb9m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb9m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb9m[[i]])
  rownames(resume.ppm_CarteSailbb9m_Im4[[i]])=c(43+47+i)
}
resume.ppm_CarteSailbb9m_Im4

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:44)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im4[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im4[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im4, bb9m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im4, bb9m", col="lightblue")
plot(density(b1))

# On supprime l'individu 10
# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:9,11:44)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im4[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im4[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im4, bb9m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im4, bb9m", col="lightblue")
plot(density(b1))


# Intéressons-nous à l'image 5
X_9m_Im[[5]]

# Tous les sujets ont des fixations sur cette image, sauf sujet 103 et 108
baby9month_Im[[13]]baby9month_Im[[13]]
baby9month_Im[[18]]

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb9m_Im5 <- list()
beta0_cartesailbb9m <- list()
beta1_cartesailbb9m <- list()
S.E.beta0_cartesailbb9m <- list()
S.E.beta1_cartesailbb9m <- list()
CI.lo.beta0_cartesailbb9m <- list()
CI.lo.beta1_cartesailbb9m <- list()
CI.hi.beta0_cartesailbb9m <- list()
CI.hi.beta1_cartesailbb9m <- list()

for(i in c(1:12,14:17,19:44)){
  a <- X_9m_Im[[5]][marks(X_9m_Im[[5]]) == paste("Sujet",43+47+i)]
  ppmCarteSailbb9m_Im5[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailbb9m[[i]] <- c(summary(ppmCarteSailbb9m_Im5[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb9m[[i]] <- c(summary(ppmCarteSailbb9m_Im5[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im5[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im5[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im5[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im5[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im5[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im5[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb9m_Im5 <- list()
for(i in c(1:12,14:17,19:44)){
  resume.ppm_CarteSailbb9m_Im5[[i]] <- data.frame(b0=beta0_cartesailbb9m[[i]],b1=beta1_cartesailbb9m[[i]],se.b0=S.E.beta0_cartesailbb9m[[i]],se.b1=S.E.beta1_cartesailbb9m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb9m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb9m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb9m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb9m[[i]])
  rownames(resume.ppm_CarteSailbb9m_Im5[[i]])=c(43+47+i)
}
resume.ppm_CarteSailbb9m_Im5

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:12,14:17,19:44)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im5[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im5[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im5, bb9m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im5, bb9m", col="lightblue")
plot(density(b1))

# On supprime l'individu 35
# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:12,14:17,19:34,36:44)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im5[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im5[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im5, bb9m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im5, bb9m", col="lightblue")
plot(density(b1))

# Intéressons-nous à l'image 6
X_9m_Im[[6]]

# Tous les sujets ont des fixations sur cette image, sauf sujet 102, 106, 108 et 132
baby9month_Im[[12]]
baby9month_Im[[16]]
baby9month_Im[[18]]
baby9month_Im[[42]]

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb9m_Im6 <- list()
beta0_cartesailbb9m <- list()
beta1_cartesailbb9m <- list()
S.E.beta0_cartesailbb9m <- list()
S.E.beta1_cartesailbb9m <- list()
CI.lo.beta0_cartesailbb9m <- list()
CI.lo.beta1_cartesailbb9m <- list()
CI.hi.beta0_cartesailbb9m <- list()
CI.hi.beta1_cartesailbb9m <- list()

for(i in c(1:11,13:15,17,19:41,43,44)){
  a <- X_9m_Im[[6]][marks(X_9m_Im[[6]]) == paste("Sujet",43+47+i)]
  ppmCarteSailbb9m_Im6[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailbb9m[[i]] <- c(summary(ppmCarteSailbb9m_Im6[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb9m[[i]] <- c(summary(ppmCarteSailbb9m_Im6[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im6[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im6[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im6[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im6[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im6[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb9m[[i]]  <- c(summary(ppmCarteSailbb9m_Im6[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb9m_Im6 <- list()
for(i in c(1:11,13:15,17,19:41,43,44)){
  resume.ppm_CarteSailbb9m_Im6[[i]] <- data.frame(b0=beta0_cartesailbb9m[[i]],b1=beta1_cartesailbb9m[[i]],se.b0=S.E.beta0_cartesailbb9m[[i]],se.b1=S.E.beta1_cartesailbb9m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb9m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb9m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb9m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb9m[[i]])
  rownames(resume.ppm_CarteSailbb9m_Im6[[i]])=c(43+47+i)
}
resume.ppm_CarteSailbb9m_Im6

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:11,13:15,17,19:41,43,44)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im6[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im6[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im6, bb9m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im6, bb9m", col="lightblue")
plot(density(b1))

# On supprime individu 10 et 38
# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:9,11,13:15,17,19:37,39,40,41,43,44)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im6[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb9m_Im6[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im6, bb9m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im6, bb9m", col="lightblue")
plot(density(b1))

### 12 mois
# Sous groupe bébés 12 mois
x.l <- list(x41,x42,x43,x44,x45,x46)
y.l <- list(768-y41,768-y42,768-y43,768-y44,768-y45,768-y46)

mark11=vector()
mark12=vector()
mark13=vector()
mark14=vector()
mark15=vector()
mark16=vector()
for(i in 1:46) {
  mark11 <-c(mark11,rep(paste("Sujet",43+47+44+i),length(baby12month_Im[[i]]$Im1[,1])))
  mark12 <-c(mark12,rep(paste("Sujet",43+47+44+i),length(baby12month_Im[[i]]$Im2[,1])))
  mark13 <-c(mark13,rep(paste("Sujet",43+47+44+i),length(baby12month_Im[[i]]$Im3[,1])))
  mark14 <-c(mark14,rep(paste("Sujet",43+47+44+i),length(baby12month_Im[[i]]$Im4[,1])))
  mark15 <-c(mark15,rep(paste("Sujet",43+47+44+i),length(baby12month_Im[[i]]$Im5[,1])))
  mark16 <-c(mark16,rep(paste("Sujet",43+47+44+i),length(baby12month_Im[[i]]$Im6[,1])))
}

mark1=factor(mark11)
mark2=factor(mark12)
mark3=factor(mark13)
mark4=factor(mark14)
mark5=factor(mark15)
mark6=factor(mark16)
mark.l=list(mark1,mark2,mark3,mark4,mark5,mark6)

X_12m_Im <- list()
par(mfrow=c(2,3))
for (i in 1:6) {
  X_12m_Im[[i]] <- ppp(x.l[[i]],y.l[[i]], c(0,1024), c(0,768), unitname=c("pixels"), marks=mark.l[[i]])
  #plot(X_12m_Im[[i]],paste("Fixations, Image",i,", selon les sujets de 12 mois"))
  #points(X_12m_Im[[i]],col=mark.l[[i]])
}

# Aperçu
baby12month_Im[[20]]

# Intéressons-nous à l'image 1
X_12m_Im[[1]]

# Il manque le sujet 136, 146 sur cette image
baby12month_Im[[2]]
baby12month_Im[[12]]



# On enleve l'individu 44
# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1,3:11,13:43,45,46)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb12m_Im1[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb12m_Im1[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im1, bb12m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im1, bb12m", col="lightblue")
plot(density(b1))

# Intéressons-nous à l'image 2
X_12m_Im[[2]]

# Il manque le sujet 151 sur cette image
baby12month_Im[[17]]

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb12m_Im2 <- list()
beta0_cartesailbb12m <- list()
beta1_cartesailbb12m <- list()
S.E.beta0_cartesailbb12m <- list()
S.E.beta1_cartesailbb12m <- list()
CI.lo.beta0_cartesailbb12m <- list()
CI.lo.beta1_cartesailbb12m <- list()
CI.hi.beta0_cartesailbb12m <- list()
CI.hi.beta1_cartesailbb12m <- list()

for(i in c(1:16,18:46)){
  a <- X_12m_Im[[2]][marks(X_12m_Im[[2]]) == paste("Sujet",43+47+44+i)]
  ppmCarteSailbb12m_Im2[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailbb12m[[i]] <- c(summary(ppmCarteSailbb12m_Im2[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb12m[[i]] <- c(summary(ppmCarteSailbb12m_Im2[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im2[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im2[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im2[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im2[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im2[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im2[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb12m_Im2 <- list()
for(i in c(1:16,18:46)){
  resume.ppm_CarteSailbb12m_Im2[[i]] <- data.frame(b0=beta0_cartesailbb12m[[i]],b1=beta1_cartesailbb12m[[i]],se.b0=S.E.beta0_cartesailbb12m[[i]],se.b1=S.E.beta1_cartesailbb12m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb12m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb12m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb12m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb12m[[i]])
  rownames(resume.ppm_CarteSailbb12m_Im2[[i]])=c(43+47+44+i)
}
resume.ppm_CarteSailbb12m_Im2

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:16,18:46)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb12m_Im2[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb12m_Im2[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im2, bb12m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im2, bb12m", col="lightblue")
plot(density(b1))

# Intéressons-nous à l'image 3
X_12m_Im[[3]]

# Il manque le sujet 146 sur cette image
baby12month_Im[[12]]

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb12m_Im3 <- list()
beta0_cartesailbb12m <- list()
beta1_cartesailbb12m <- list()
S.E.beta0_cartesailbb12m <- list()
S.E.beta1_cartesailbb12m <- list()
CI.lo.beta0_cartesailbb12m <- list()
CI.lo.beta1_cartesailbb12m <- list()
CI.hi.beta0_cartesailbb12m <- list()
CI.hi.beta1_cartesailbb12m <- list()

for(i in c(1:11,13:46)){
  a <- X_12m_Im[[3]][marks(X_12m_Im[[3]]) == paste("Sujet",43+47+44+i)]
  ppmCarteSailbb12m_Im3[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailbb12m[[i]] <- c(summary(ppmCarteSailbb12m_Im3[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb12m[[i]] <- c(summary(ppmCarteSailbb12m_Im3[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im3[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im3[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im3[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im3[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im3[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im3[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb12m_Im3 <- list()
for(i in c(1:11,13:46)){
  resume.ppm_CarteSailbb12m_Im3[[i]] <- data.frame(b0=beta0_cartesailbb12m[[i]],b1=beta1_cartesailbb12m[[i]],se.b0=S.E.beta0_cartesailbb12m[[i]],se.b1=S.E.beta1_cartesailbb12m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb12m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb12m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb12m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb12m[[i]])
  rownames(resume.ppm_CarteSailbb12m_Im3[[i]])=c(43+47+44+i)
}
resume.ppm_CarteSailbb12m_Im3

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:11,13:46)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb12m_Im3[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb12m_Im3[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im3, bb12m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im3, bb12m", col="lightblue")
plot(density(b1))

# Intéressons-nous à l'image 4
X_12m_Im[[4]]

# Il ne manque aucun sujet

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb12m_Im4 <- list()
beta0_cartesailbb12m <- list()
beta1_cartesailbb12m <- list()
S.E.beta0_cartesailbb12m <- list()
S.E.beta1_cartesailbb12m <- list()
CI.lo.beta0_cartesailbb12m <- list()
CI.lo.beta1_cartesailbb12m <- list()
CI.hi.beta0_cartesailbb12m <- list()
CI.hi.beta1_cartesailbb12m <- list()

for(i in c(1:46)){
  a <- X_12m_Im[[4]][marks(X_12m_Im[[4]]) == paste("Sujet",43+47+44+i)]
  ppmCarteSailbb12m_Im4[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailbb12m[[i]] <- c(summary(ppmCarteSailbb12m_Im4[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb12m[[i]] <- c(summary(ppmCarteSailbb12m_Im4[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im4[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im4[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im4[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im4[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im4[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im4[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb12m_Im4 <- list()
for(i in c(1:46)){
  resume.ppm_CarteSailbb12m_Im4[[i]] <- data.frame(b0=beta0_cartesailbb12m[[i]],b1=beta1_cartesailbb12m[[i]],se.b0=S.E.beta0_cartesailbb12m[[i]],se.b1=S.E.beta1_cartesailbb12m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb12m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb12m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb12m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb12m[[i]])
  rownames(resume.ppm_CarteSailbb12m_Im4[[i]])=c(43+47+44+i)
}
resume.ppm_CarteSailbb12m_Im4

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:46)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb12m_Im4[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb12m_Im4[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im4, bb12m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im4, bb12m", col="lightblue")
plot(density(b1))

# On supprime l'individu 39
# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:38,40:46)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb12m_Im4[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb12m_Im4[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im4, bb12m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im4, bb12m", col="lightblue")
plot(density(b1))

# Intéressons-nous à l'image 5
X_12m_Im[[5]]

# Il manque le sujet 158 sur cette image
baby12month_Im[[24]]

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb12m_Im5 <- list()
beta0_cartesailbb12m <- list()
beta1_cartesailbb12m <- list()
S.E.beta0_cartesailbb12m <- list()
S.E.beta1_cartesailbb12m <- list()
CI.lo.beta0_cartesailbb12m <- list()
CI.lo.beta1_cartesailbb12m <- list()
CI.hi.beta0_cartesailbb12m <- list()
CI.hi.beta1_cartesailbb12m <- list()

for(i in c(1:23,25:46)){
  a <- X_12m_Im[[5]][marks(X_12m_Im[[5]]) == paste("Sujet",43+47+44+i)]
  ppmCarteSailbb12m_Im5[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailbb12m[[i]] <- c(summary(ppmCarteSailbb12m_Im5[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb12m[[i]] <- c(summary(ppmCarteSailbb12m_Im5[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im5[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im5[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im5[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im5[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im5[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im5[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb12m_Im5 <- list()
for(i in c(1:23,25:46)){
  resume.ppm_CarteSailbb12m_Im5[[i]] <- data.frame(b0=beta0_cartesailbb12m[[i]],b1=beta1_cartesailbb12m[[i]],se.b0=S.E.beta0_cartesailbb12m[[i]],se.b1=S.E.beta1_cartesailbb12m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb12m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb12m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb12m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb12m[[i]])
  rownames(resume.ppm_CarteSailbb12m_Im5[[i]])=c(43+47+44+i)
}
resume.ppm_CarteSailbb12m_Im5

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:23,25:46)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb12m_Im5[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb12m_Im5[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im5, bb12m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im5, bb12m", col="lightblue")
plot(density(b1))

# Intéressons-nous à l'image 6
X_12m_Im[[6]]

# Tous les sujets ont au moins une fixation

# Recuperation des coefficients, S.E et IC
ppmCarteSailbb12m_Im6 <- list()
beta0_cartesailbb12m <- list()
beta1_cartesailbb12m <- list()
S.E.beta0_cartesailbb12m <- list()
S.E.beta1_cartesailbb12m <- list()
CI.lo.beta0_cartesailbb12m <- list()
CI.lo.beta1_cartesailbb12m <- list()
CI.hi.beta0_cartesailbb12m <- list()
CI.hi.beta1_cartesailbb12m <- list()

for(i in c(1:46)){
  a <- X_12m_Im[[6]][marks(X_12m_Im[[6]]) == paste("Sujet",43+47+44+i)]
  ppmCarteSailbb12m_Im6[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailbb12m[[i]] <- c(summary(ppmCarteSailbb12m_Im6[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailbb12m[[i]] <- c(summary(ppmCarteSailbb12m_Im6[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im6[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im6[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im6[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im6[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im6[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailbb12m[[i]]  <- c(summary(ppmCarteSailbb12m_Im6[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailbb12m_Im6 <- list()
for(i in c(1:46)){
  resume.ppm_CarteSailbb12m_Im6[[i]] <- data.frame(b0=beta0_cartesailbb12m[[i]],b1=beta1_cartesailbb12m[[i]],se.b0=S.E.beta0_cartesailbb12m[[i]],se.b1=S.E.beta1_cartesailbb12m[[i]],ci.lo.b0=CI.lo.beta0_cartesailbb12m[[i]],ci.hi.b0=CI.hi.beta0_cartesailbb12m[[i]],ci.lo.b1=CI.lo.beta1_cartesailbb12m[[i]],ci.hi.b1=CI.hi.beta1_cartesailbb12m[[i]])
  rownames(resume.ppm_CarteSailbb12m_Im6[[i]])=c(43+47+44+i)
}
resume.ppm_CarteSailbb12m_Im6

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:46)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailbb12m_Im6[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailbb12m_Im6[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im6, bb12m", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im6, bb12m", col="lightblue")
plot(density(b1))

# Sous groupe adultes
x.l <- list(x51,x52,x53,x54,x55,x56)
y.l <- list(768-y51,768-y52,768-y53,768-y54,768-y55,768-y56)

mark11=vector()
mark12=vector()
mark13=vector()
mark14=vector()
mark15=vector()
mark16=vector()
for(i in 1:44) {
  mark11 <-c(mark11,rep(paste("Sujet",43+47+44+46+i),length(adult_Im[[i]]$Im1[,1])))
  mark12 <-c(mark12,rep(paste("Sujet",43+47+44+46+i),length(adult_Im[[i]]$Im2[,1])))
  mark13 <-c(mark13,rep(paste("Sujet",43+47+44+46+i),length(adult_Im[[i]]$Im3[,1])))
  mark14 <-c(mark14,rep(paste("Sujet",43+47+44+46+i),length(adult_Im[[i]]$Im4[,1])))
  mark15 <-c(mark15,rep(paste("Sujet",43+47+44+46+i),length(adult_Im[[i]]$Im5[,1])))
  mark16 <-c(mark16,rep(paste("Sujet",43+47+44+46+i),length(adult_Im[[i]]$Im6[,1])))
}

mark1=factor(mark11)
mark2=factor(mark12)
mark3=factor(mark13)
mark4=factor(mark14)
mark5=factor(mark15)
mark6=factor(mark16)
mark.l=list(mark1,mark2,mark3,mark4,mark5,mark6)

X_Adult_Im <- list()
par(mfrow=c(2,3))
for (i in 1:6) {
  X_Adult_Im[[i]] <- ppp(x.l[[i]],y.l[[i]], c(0,1024), c(0,768), unitname=c("pixels"), marks=mark.l[[i]])
  plot(X_Adult_Im[[i]],paste("Fixations, Image",i,", selon les sujets adultes"))
  points(X_Adult_Im[[i]],col=mark.l[[i]])
}

# Aperçu
adult_Im[[20]]

# Intéressons-nous à l'image 1
X_Adult_Im[[1]]

# Il ne manque aucun individu

# Recuperation des coefficients, S.E et IC
ppmCarteSailAdult_Im1 <- list()
beta0_cartesailAdult <- list()
beta1_cartesailAdult <- list()
S.E.beta0_cartesailAdult <- list()
S.E.beta1_cartesailAdult <- list()
CI.lo.beta0_cartesailAdult <- list()
CI.lo.beta1_cartesailAdult <- list()
CI.hi.beta0_cartesailAdult <- list()
CI.hi.beta1_cartesailAdult <- list()

for(i in c(1:44)){
  a <- X_Adult_Im[[1]][marks(X_Adult_Im[[1]]) == paste("Sujet",43+47+44+46+i)]
  ppmCarteSailAdult_Im1[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailAdult[[i]] <- c(summary(ppmCarteSailAdult_Im1[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailAdult[[i]] <- c(summary(ppmCarteSailAdult_Im1[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im1[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im1[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im1[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im1[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im1[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im1[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailAdult_Im1 <- list()
for(i in c(1:44)){
  resume.ppm_CarteSailAdult_Im1[[i]] <- data.frame(b0=beta0_cartesailAdult[[i]],b1=beta1_cartesailAdult[[i]],se.b0=S.E.beta0_cartesailAdult[[i]],se.b1=S.E.beta1_cartesailAdult[[i]],ci.lo.b0=CI.lo.beta0_cartesailAdult[[i]],ci.hi.b0=CI.hi.beta0_cartesailAdult[[i]],ci.lo.b1=CI.lo.beta1_cartesailAdult[[i]],ci.hi.b1=CI.hi.beta1_cartesailAdult[[i]])
  rownames(resume.ppm_CarteSailAdult_Im1[[i]])=c(43+47+44+46+i)
}
resume.ppm_CarteSailAdult_Im1

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:44)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailAdult_Im1[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailAdult_Im1[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im1, Adult", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im1, Adult", col="lightblue")
plot(density(b1))

# Intéressons-nous à l'image 2
X_Adult_Im[[2]]

# Il ne manque aucun individu

# Recuperation des coefficients, S.E et IC
ppmCarteSailAdult_Im2 <- list()
beta0_cartesailAdult <- list()
beta1_cartesailAdult <- list()
S.E.beta0_cartesailAdult <- list()
S.E.beta1_cartesailAdult <- list()
CI.lo.beta0_cartesailAdult <- list()
CI.lo.beta1_cartesailAdult <- list()
CI.hi.beta0_cartesailAdult <- list()
CI.hi.beta1_cartesailAdult <- list()

for(i in c(1:44)){
  a <- X_Adult_Im[[2]][marks(X_Adult_Im[[2]]) == paste("Sujet",43+47+44+46+i)]
  ppmCarteSailAdult_Im2[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailAdult[[i]] <- c(summary(ppmCarteSailAdult_Im2[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailAdult[[i]] <- c(summary(ppmCarteSailAdult_Im2[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im2[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im2[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im2[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im2[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im2[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im2[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailAdult_Im2 <- list()
for(i in c(1:44)){
  resume.ppm_CarteSailAdult_Im2[[i]] <- data.frame(b0=beta0_cartesailAdult[[i]],b1=beta1_cartesailAdult[[i]],se.b0=S.E.beta0_cartesailAdult[[i]],se.b1=S.E.beta1_cartesailAdult[[i]],ci.lo.b0=CI.lo.beta0_cartesailAdult[[i]],ci.hi.b0=CI.hi.beta0_cartesailAdult[[i]],ci.lo.b1=CI.lo.beta1_cartesailAdult[[i]],ci.hi.b1=CI.hi.beta1_cartesailAdult[[i]])
  rownames(resume.ppm_CarteSailAdult_Im2[[i]])=c(43+47+44+46+i)
}
resume.ppm_CarteSailAdult_Im2

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:44)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailAdult_Im2[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailAdult_Im2[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im2, Adult", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im2, Adult", col="lightblue")
plot(density(b1))

# Intéressons-nous à l'image 3
X_Adult_Im[[3]]

# Il ne manque aucun individu

# Recuperation des coefficients, S.E et IC
ppmCarteSailAdult_Im3 <- list()
beta0_cartesailAdult <- list()
beta1_cartesailAdult <- list()
S.E.beta0_cartesailAdult <- list()
S.E.beta1_cartesailAdult <- list()
CI.lo.beta0_cartesailAdult <- list()
CI.lo.beta1_cartesailAdult <- list()
CI.hi.beta0_cartesailAdult <- list()
CI.hi.beta1_cartesailAdult <- list()

for(i in c(1:44)){
  a <- X_Adult_Im[[3]][marks(X_Adult_Im[[3]]) == paste("Sujet",43+47+44+46+i)]
  ppmCarteSailAdult_Im3[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailAdult[[i]] <- c(summary(ppmCarteSailAdult_Im3[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailAdult[[i]] <- c(summary(ppmCarteSailAdult_Im3[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im3[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im3[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im3[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im3[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im3[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im3[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailAdult_Im3 <- list()
for(i in c(1:44)){
  resume.ppm_CarteSailAdult_Im3[[i]] <- data.frame(b0=beta0_cartesailAdult[[i]],b1=beta1_cartesailAdult[[i]],se.b0=S.E.beta0_cartesailAdult[[i]],se.b1=S.E.beta1_cartesailAdult[[i]],ci.lo.b0=CI.lo.beta0_cartesailAdult[[i]],ci.hi.b0=CI.hi.beta0_cartesailAdult[[i]],ci.lo.b1=CI.lo.beta1_cartesailAdult[[i]],ci.hi.b1=CI.hi.beta1_cartesailAdult[[i]])
  rownames(resume.ppm_CarteSailAdult_Im3[[i]])=c(43+47+44+46+i)
}
resume.ppm_CarteSailAdult_Im3

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:44)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailAdult_Im3[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailAdult_Im3[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im3, Adult", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im3, Adult", col="lightblue")
plot(density(b1))

# Intéressons-nous à l'image 4
X_Adult_Im[[4]]

# Il manque le sujet 192
adult_Im[[12]]

# Recuperation des coefficients, S.E et IC
ppmCarteSailAdult_Im4 <- list()
beta0_cartesailAdult <- list()
beta1_cartesailAdult <- list()
S.E.beta0_cartesailAdult <- list()
S.E.beta1_cartesailAdult <- list()
CI.lo.beta0_cartesailAdult <- list()
CI.lo.beta1_cartesailAdult <- list()
CI.hi.beta0_cartesailAdult <- list()
CI.hi.beta1_cartesailAdult <- list()

for(i in c(1:11,13:44)){
  a <- X_Adult_Im[[4]][marks(X_Adult_Im[[4]]) == paste("Sujet",43+47+44+46+i)]
  ppmCarteSailAdult_Im4[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailAdult[[i]] <- c(summary(ppmCarteSailAdult_Im4[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailAdult[[i]] <- c(summary(ppmCarteSailAdult_Im4[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im4[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im4[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im4[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im4[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im4[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im4[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailAdult_Im4 <- list()
for(i in c(1:11,13:44)){
  resume.ppm_CarteSailAdult_Im4[[i]] <- data.frame(b0=beta0_cartesailAdult[[i]],b1=beta1_cartesailAdult[[i]],se.b0=S.E.beta0_cartesailAdult[[i]],se.b1=S.E.beta1_cartesailAdult[[i]],ci.lo.b0=CI.lo.beta0_cartesailAdult[[i]],ci.hi.b0=CI.hi.beta0_cartesailAdult[[i]],ci.lo.b1=CI.lo.beta1_cartesailAdult[[i]],ci.hi.b1=CI.hi.beta1_cartesailAdult[[i]])
  rownames(resume.ppm_CarteSailAdult_Im4[[i]])=c(43+47+44+46+i)
}
resume.ppm_CarteSailAdult_Im4

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:11,13:44)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailAdult_Im4[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailAdult_Im4[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im4, Adult", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im4, Adult", col="lightblue")
plot(density(b1))

# Intéressons-nous à l'image 5
X_Adult_Im[[5]]

# Il ne manque aucun individu

# Recuperation des coefficients, S.E et IC
ppmCarteSailAdult_Im5 <- list()
beta0_cartesailAdult <- list()
beta1_cartesailAdult <- list()
S.E.beta0_cartesailAdult <- list()
S.E.beta1_cartesailAdult <- list()
CI.lo.beta0_cartesailAdult <- list()
CI.lo.beta1_cartesailAdult <- list()
CI.hi.beta0_cartesailAdult <- list()
CI.hi.beta1_cartesailAdult <- list()

for(i in c(1:44)){
  a <- X_Adult_Im[[5]][marks(X_Adult_Im[[5]]) == paste("Sujet",43+47+44+46+i)]
  ppmCarteSailAdult_Im5[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailAdult[[i]] <- c(summary(ppmCarteSailAdult_Im5[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailAdult[[i]] <- c(summary(ppmCarteSailAdult_Im5[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im5[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im5[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im5[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im5[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im5[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im5[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailAdult_Im5 <- list()
for(i in c(1:44)){
  resume.ppm_CarteSailAdult_Im5[[i]] <- data.frame(b0=beta0_cartesailAdult[[i]],b1=beta1_cartesailAdult[[i]],se.b0=S.E.beta0_cartesailAdult[[i]],se.b1=S.E.beta1_cartesailAdult[[i]],ci.lo.b0=CI.lo.beta0_cartesailAdult[[i]],ci.hi.b0=CI.hi.beta0_cartesailAdult[[i]],ci.lo.b1=CI.lo.beta1_cartesailAdult[[i]],ci.hi.b1=CI.hi.beta1_cartesailAdult[[i]])
  rownames(resume.ppm_CarteSailAdult_Im5[[i]])=c(43+47+44+46+i)
}
resume.ppm_CarteSailAdult_Im5

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:44)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailAdult_Im5[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailAdult_Im5[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im5, Adult", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im5, Adult", col="lightblue")
plot(density(b1))

# Intéressons-nous à l'image 6
X_Adult_Im[[6]]

# Il manque l'individu 217
adult_Im[[37]]

# Recuperation des coefficients, S.E et IC
ppmCarteSailAdult_Im6 <- list()
beta0_cartesailAdult <- list()
beta1_cartesailAdult <- list()
S.E.beta0_cartesailAdult <- list()
S.E.beta1_cartesailAdult <- list()
CI.lo.beta0_cartesailAdult <- list()
CI.lo.beta1_cartesailAdult <- list()
CI.hi.beta0_cartesailAdult <- list()
CI.hi.beta1_cartesailAdult <- list()

for(i in c(1:36,38:44)){
  a <- X_Adult_Im[[6]][marks(X_Adult_Im[[6]]) == paste("Sujet",43+47+44+46+i)]
  ppmCarteSailAdult_Im6[[i]]  <- ppm(a,~sail,covariates=list(sail=babyIm.Moylevel[[1]]))
  
  beta0_cartesailAdult[[i]] <- c(summary(ppmCarteSailAdult_Im6[[i]])$coefs.SE.CI$Estimate[1])
  beta1_cartesailAdult[[i]] <- c(summary(ppmCarteSailAdult_Im6[[i]])$coefs.SE.CI$Estimate[2])
  
  S.E.beta0_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im6[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im6[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im6[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im6[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.hi.beta0_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im6[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_cartesailAdult[[i]]  <- c(summary(ppmCarteSailAdult_Im6[[i]])$coefs.SE.CI$CI95.hi[2])
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)

# Pour toutes les images, ppm carte saillance moyen
resume.ppm_CarteSailAdult_Im6 <- list()
for(i in c(1:36,38:44)){
  resume.ppm_CarteSailAdult_Im6[[i]] <- data.frame(b0=beta0_cartesailAdult[[i]],b1=beta1_cartesailAdult[[i]],se.b0=S.E.beta0_cartesailAdult[[i]],se.b1=S.E.beta1_cartesailAdult[[i]],ci.lo.b0=CI.lo.beta0_cartesailAdult[[i]],ci.hi.b0=CI.hi.beta0_cartesailAdult[[i]],ci.lo.b1=CI.lo.beta1_cartesailAdult[[i]],ci.hi.b1=CI.hi.beta1_cartesailAdult[[i]])
  rownames(resume.ppm_CarteSailAdult_Im6[[i]])=c(43+47+44+46+i)
}
resume.ppm_CarteSailAdult_Im6

# Histogramme
b0 <- vector()
b1 <- vector()
for(i in c(1:36,38:44)){
  b0 <- c(b0,as.numeric(unlist(c(resume.ppm_CarteSailAdult_Im6[[i]][1]))))
  b1 <- c(b1,as.numeric(unlist(c(resume.ppm_CarteSailAdult_Im6[[i]][2]))))
}

par(mfrow=c(2,2))
hist(b0,10,main="Distribution coefficient beta0, Im6, Adult", col="lightblue")
plot(density(b0))
hist(b1,10,main="Distribution coefficient beta1, Im6, Adult", col="lightblue")
plot(density(b1))


