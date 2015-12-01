## Tentative de ppm
load("~/Desktop/MASSS/Stage/Codes R Antoine/WorkSpace_Antoine.RData")

# Image 1
# Niveau gris
ppmIm1grey <- ppm(X_Im[[1]],~grey,covariates=list(grey=babyIm.greylevel[[1]]))
ppmIm1grey

# Niveau rouge
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

# Vérification red=gris, Image 1
ppmIm1grey <- ppm(X_Im[[1]],~grey,covariates=list(grey=babyIm.greylevel[[1]]))
plot(ppmIm1grey,se=FALSE,superimpose=FALSE,pause=FALSE)->trendIm1grey
ppmIm1red <- ppm(X_Im[[1]],~red,covariates=list(red=babyIm.redlevel[[1]]))
plot(ppmIm1red,se=FALSE,superimpose=FALSE,pause=FALSE)->trendIm1red

MtrendIm1grey<-as.matrix(trendIm1grey$trend[[1]])
MtrendIm1red<-as.matrix(trendIm1red$trend[[1]])
image(as.im(MtrendIm1grey-MtrendIm1red),main='difference between grey and red estimations')

# Pour toutes les images, red=gris
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

### PPM RGB
# Pour toutes les images, tous les groupes
ppmred_3m_Im <- list()
ppmred_6m_Im <- list()
ppmred_9m_Im <- list()
ppmred_12m_Im <- list()
ppmred_ad_Im <- list()
ppmgreen_3m_Im <- list()
ppmgreen_6m_Im <- list()
ppmgreen_9m_Im <- list()
ppmgreen_12m_Im <- list()
ppmgreen_ad_Im <- list()
ppmblue_3m_Im <- list()
ppmblue_6m_Im <- list()
ppmblue_9m_Im <- list()
ppmblue_12m_Im <- list()
ppmblue_ad_Im <- list()
ppmrgb_3m_Im <- list()
ppmrgb_6m_Im <- list()
ppmrgb_9m_Im <- list()
ppmrgb_12m_Im <- list()
ppmrgb_ad_Im <- list()

beta0_red <- list()
beta1_red <- list()
beta0_green <- list()
beta1_green <- list()
beta0_blue <- list()
beta1_blue <- list()

beta0_rgb <- list()
beta1_rgb <- list()
beta2_rgb <- list()
beta3_rgb <- list()

S.E.beta0_red  <- list()
S.E.beta1_red  <- list()
S.E.beta0_green  <- list()
S.E.beta1_green  <- list()
S.E.beta0_blue  <- list()
S.E.beta1_blue  <- list()

S.E.beta0_rgb  <- list()
S.E.beta1_rgb  <- list()
S.E.beta2_rgb  <- list()
S.E.beta3_rgb  <- list()

CI.lo.beta0_red <- list()
CI.lo.beta1_red <- list()
CI.lo.beta0_green <- list()
CI.lo.beta1_green <- list()
CI.lo.beta0_blue <- list()
CI.lo.beta1_blue <- list()

CI.lo.beta0_rgb <- list()
CI.lo.beta1_rgb <- list()
CI.lo.beta2_rgb <- list()
CI.lo.beta3_rgb <- list()

CI.hi.beta0_red <- list()
CI.hi.beta1_red <- list()
CI.hi.beta0_green <- list()
CI.hi.beta1_green <- list()
CI.hi.beta0_blue <- list()
CI.hi.beta1_blue <- list()

CI.hi.beta0_rgb <- list()
CI.hi.beta1_rgb <- list()
CI.hi.beta2_rgb <- list()
CI.hi.beta3_rgb <- list()

for (i in 1:6){
  # Bébés 3 mois
  ppmred_3m_Im[[i]] <- ppm(X_baby3month_Im[[i]],~red,covariates=list(red=babyIm.redlevel[[i]]))
  ppmgreen_3m_Im[[i]] <- ppm(X_baby3month_Im[[i]],~green,covariates=list(green=babyIm.greenlevel[[i]]))
  ppmblue_3m_Im[[i]] <- ppm(X_baby3month_Im[[i]],~blue,covariates=list(blue=babyIm.bluelevel[[i]]))

  # Bébés 6 mois
  ppmred_6m_Im[[i]] <- ppm(X_baby6month_Im[[i]],~red,covariates=list(red=babyIm.redlevel[[i]]))
  ppmgreen_6m_Im[[i]] <- ppm(X_baby6month_Im[[i]],~green,covariates=list(green=babyIm.greenlevel[[i]]))
  ppmblue_6m_Im[[i]] <- ppm(X_baby6month_Im[[i]],~blue,covariates=list(blue=babyIm.bluelevel[[i]]))

  # Bébés 9 mois
  ppmred_9m_Im[[i]] <- ppm(X_baby9month_Im[[i]],~red,covariates=list(red=babyIm.redlevel[[i]]))
  ppmgreen_9m_Im[[i]] <- ppm(X_baby9month_Im[[i]],~green,covariates=list(green=babyIm.greenlevel[[i]]))
  ppmblue_9m_Im[[i]] <- ppm(X_baby9month_Im[[i]],~blue,covariates=list(blue=babyIm.bluelevel[[i]]))
  
  # Bébés 12 mois
  ppmred_12m_Im[[i]] <- ppm(X_baby12month_Im[[i]],~red,covariates=list(red=babyIm.redlevel[[i]]))
  ppmgreen_12m_Im[[i]] <- ppm(X_baby12month_Im[[i]],~green,covariates=list(green=babyIm.greenlevel[[i]]))
  ppmblue_12m_Im[[i]] <- ppm(X_baby12month_Im[[i]],~blue,covariates=list(blue=babyIm.bluelevel[[i]]))

  # Adultes
  ppmred_ad_Im[[i]] <- ppm(X_adult_Im[[i]],~red,covariates=list(red=babyIm.redlevel[[i]]))
  ppmgreen_ad_Im[[i]] <- ppm(X_adult_Im[[i]],~green,covariates=list(green=babyIm.greenlevel[[i]]))
  ppmblue_ad_Im[[i]] <- ppm(X_adult_Im[[i]],~blue,covariates=list(blue=babyIm.bluelevel[[i]]))

  # RGB réuni
  ppmrgb_3m_Im[[i]] <- ppm(X_baby3month_Im[[i]],~red+green+blue,covariates=list(red=babyIm.redlevel[[i]],green=babyIm.greenlevel[[i]],blue=babyIm.bluelevel[[i]]))
  ppmrgb_6m_Im[[i]] <- ppm(X_baby6month_Im[[i]],~red+green+blue,covariates=list(red=babyIm.redlevel[[i]],green=babyIm.greenlevel[[i]],blue=babyIm.bluelevel[[i]]))
  ppmrgb_9m_Im[[i]] <- ppm(X_baby9month_Im[[i]],~red+green+blue,covariates=list(red=babyIm.redlevel[[i]],green=babyIm.greenlevel[[i]],blue=babyIm.bluelevel[[i]]))
  ppmrgb_12m_Im[[i]] <- ppm(X_baby12month_Im[[i]],~red+green+blue,covariates=list(red=babyIm.redlevel[[i]],green=babyIm.greenlevel[[i]],blue=babyIm.bluelevel[[i]]))
  ppmrgb_ad_Im[[i]] <- ppm(X_adult_Im[[i]],~red+green+blue,covariates=list(red=babyIm.redlevel[[i]],green=babyIm.greenlevel[[i]],blue=babyIm.bluelevel[[i]]))

  beta0_red[[i]] <- c(summary(ppmred_3m_Im[[i]])$coefs.SE.CI$Estimate[1], summary(ppmred_6m_Im[[i]])$coefs.SE.CI$Estimate[1], summary(ppmred_9m_Im[[i]])$coefs.SE.CI$Estimate[1], summary(ppmred_12m_Im[[i]])$coefs.SE.CI$Estimate[1], summary(ppmred_ad_Im[[i]])$coefs.SE.CI$Estimate[1])
  beta1_red[[i]] <- c(summary(ppmred_3m_Im[[i]])$coefs.SE.CI$Estimate[2], summary(ppmred_6m_Im[[i]])$coefs.SE.CI$Estimate[2], summary(ppmred_9m_Im[[i]])$coefs.SE.CI$Estimate[2], summary(ppmred_12m_Im[[i]])$coefs.SE.CI$Estimate[2], summary(ppmred_ad_Im[[i]])$coefs.SE.CI$Estimate[2])

  beta0_green[[i]] <- c(summary(ppmgreen_3m_Im[[i]])$coefs.SE.CI$Estimate[1], summary(ppmgreen_6m_Im[[i]])$coefs.SE.CI$Estimate[1], summary(ppmgreen_9m_Im[[i]])$coefs.SE.CI$Estimate[1], summary(ppmgreen_12m_Im[[i]])$coefs.SE.CI$Estimate[1], summary(ppmgreen_ad_Im[[i]])$coefs.SE.CI$Estimate[1])
  beta1_green[[i]] <- c(summary(ppmgreen_3m_Im[[i]])$coefs.SE.CI$Estimate[2], summary(ppmgreen_6m_Im[[i]])$coefs.SE.CI$Estimate[2], summary(ppmgreen_9m_Im[[i]])$coefs.SE.CI$Estimate[2], summary(ppmgreen_12m_Im[[i]])$coefs.SE.CI$Estimate[2], summary(ppmgreen_ad_Im[[i]])$coefs.SE.CI$Estimate[2])
  
  beta0_blue[[i]] <- c(summary(ppmblue_3m_Im[[i]])$coefs.SE.CI$Estimate[1], summary(ppmblue_6m_Im[[i]])$coefs.SE.CI$Estimate[1], summary(ppmblue_9m_Im[[i]])$coefs.SE.CI$Estimate[1], summary(ppmblue_12m_Im[[i]])$coefs.SE.CI$Estimate[1], summary(ppmblue_ad_Im[[i]])$coefs.SE.CI$Estimate[1])
  beta1_blue[[i]] <- c(summary(ppmblue_3m_Im[[i]])$coefs.SE.CI$Estimate[2], summary(ppmblue_6m_Im[[i]])$coefs.SE.CI$Estimate[2], summary(ppmblue_9m_Im[[i]])$coefs.SE.CI$Estimate[2], summary(ppmblue_12m_Im[[i]])$coefs.SE.CI$Estimate[2], summary(ppmblue_ad_Im[[i]])$coefs.SE.CI$Estimate[2])
  
  beta0_rgb[[i]] <- c(summary(ppmrgb_3m_Im[[i]])$coefs.SE.CI$Estimate[1], summary(ppmrgb_6m_Im[[i]])$coefs.SE.CI$Estimate[1], summary(ppmrgb_9m_Im[[i]])$coefs.SE.CI$Estimate[1], summary(ppmrgb_12m_Im[[i]])$coefs.SE.CI$Estimate[1], summary(ppmrgb_ad_Im[[i]])$coefs.SE.CI$Estimate[1])
  beta1_rgb[[i]] <- c(summary(ppmrgb_3m_Im[[i]])$coefs.SE.CI$Estimate[2], summary(ppmrgb_6m_Im[[i]])$coefs.SE.CI$Estimate[2], summary(ppmrgb_9m_Im[[i]])$coefs.SE.CI$Estimate[2], summary(ppmrgb_12m_Im[[i]])$coefs.SE.CI$Estimate[2], summary(ppmrgb_ad_Im[[i]])$coefs.SE.CI$Estimate[2])
  beta2_rgb[[i]] <- c(summary(ppmrgb_3m_Im[[i]])$coefs.SE.CI$Estimate[3], summary(ppmrgb_6m_Im[[i]])$coefs.SE.CI$Estimate[3], summary(ppmrgb_9m_Im[[i]])$coefs.SE.CI$Estimate[3], summary(ppmrgb_12m_Im[[i]])$coefs.SE.CI$Estimate[3], summary(ppmrgb_ad_Im[[i]])$coefs.SE.CI$Estimate[3])
  beta3_rgb[[i]] <- c(summary(ppmrgb_3m_Im[[i]])$coefs.SE.CI$Estimate[4], summary(ppmrgb_6m_Im[[i]])$coefs.SE.CI$Estimate[4], summary(ppmrgb_9m_Im[[i]])$coefs.SE.CI$Estimate[4], summary(ppmrgb_12m_Im[[i]])$coefs.SE.CI$Estimate[4], summary(ppmrgb_ad_Im[[i]])$coefs.SE.CI$Estimate[4])

  S.E.beta0_red[[i]]  <- c(summary(ppmred_3m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmred_6m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmred_9m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmred_12m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmred_ad_Im[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_red[[i]]  <- c(summary(ppmred_3m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmred_6m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmred_9m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmred_12m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmred_ad_Im[[i]])$coefs.SE.CI$S.E[2])
 
  S.E.beta0_green[[i]]  <- c(summary(ppmgreen_3m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmgreen_6m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmgreen_9m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmgreen_12m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmgreen_ad_Im[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_green[[i]]  <- c(summary(ppmgreen_3m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmgreen_6m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmgreen_9m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmgreen_12m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmgreen_ad_Im[[i]])$coefs.SE.CI$S.E[2])
  
  S.E.beta0_blue[[i]]  <- c(summary(ppmblue_3m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmblue_6m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmblue_9m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmblue_12m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmblue_ad_Im[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_blue[[i]]  <- c(summary(ppmblue_3m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmblue_6m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmblue_9m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmblue_12m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmblue_ad_Im[[i]])$coefs.SE.CI$S.E[2])
  
  S.E.beta0_rgb[[i]]  <- c(summary(ppmrgb_3m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmred_6m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmrgb_9m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmrgb_12m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmrgb_ad_Im[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_rgb[[i]]  <- c(summary(ppmrgb_3m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmrgb_6m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmrgb_9m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmrgb_12m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmrgb_ad_Im[[i]])$coefs.SE.CI$S.E[2])
  S.E.beta2_rgb[[i]]  <- c(summary(ppmrgb_3m_Im[[i]])$coefs.SE.CI$S.E[3],summary(ppmrgb_6m_Im[[i]])$coefs.SE.CI$S.E[3],summary(ppmrgb_9m_Im[[i]])$coefs.SE.CI$S.E[3],summary(ppmrgb_12m_Im[[i]])$coefs.SE.CI$S.E[3],summary(ppmrgb_ad_Im[[i]])$coefs.SE.CI$S.E[3])
  S.E.beta3_rgb[[i]]  <- c(summary(ppmrgb_3m_Im[[i]])$coefs.SE.CI$S.E[4],summary(ppmrgb_6m_Im[[i]])$coefs.SE.CI$S.E[4],summary(ppmrgb_9m_Im[[i]])$coefs.SE.CI$S.E[4],summary(ppmrgb_12m_Im[[i]])$coefs.SE.CI$S.E[4],summary(ppmrgb_ad_Im[[i]])$coefs.SE.CI$S.E[4]) 
  S.E.beta0_red[[i]]  <- c(summary(ppmred_3m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmred_6m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmred_9m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmred_12m_Im[[i]])$coefs.SE.CI$S.E[1],summary(ppmred_ad_Im[[i]])$coefs.SE.CI$S.E[1])
  S.E.beta1_red[[i]]  <- c(summary(ppmred_3m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmred_6m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmred_9m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmred_12m_Im[[i]])$coefs.SE.CI$S.E[2],summary(ppmred_ad_Im[[i]])$coefs.SE.CI$S.E[2])
  
  CI.lo.beta0_red[[i]]  <- c(summary(ppmred_3m_Im[[i]])$coefs.SE.CI$CI95.lo[1],summary(ppmred_6m_Im[[i]])$coefs.SE.CI$CI95.lo[1],summary(ppmred_9m_Im[[i]])$coefs.SE.CI$CI95.lo[1],summary(ppmred_12m_Im[[i]])$coefs.SE.CI$CI95.lo[1],summary(ppmred_ad_Im[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_red[[i]]  <- c(summary(ppmred_3m_Im[[i]])$coefs.SE.CI$CI95.lo[2],summary(ppmred_6m_Im[[i]])$coefs.SE.CI$CI95.lo[2],summary(ppmred_9m_Im[[i]])$coefs.SE.CI$CI95.lo[2],summary(ppmred_12m_Im[[i]])$coefs.SE.CI$CI95.lo[2],summary(ppmred_ad_Im[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.lo.beta0_green[[i]]  <- c(summary(ppmgreen_3m_Im[[i]])$coefs.SE.CI$CI95.lo[1],summary(ppmgreen_6m_Im[[i]])$coefs.SE.CI$CI95.lo[1],summary(ppmgreen_9m_Im[[i]])$coefs.SE.CI$CI95.lo[1],summary(ppmgreen_12m_Im[[i]])$coefs.SE.CI$CI95.lo[1],summary(ppmgreen_ad_Im[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_green[[i]]  <- c(summary(ppmgreen_3m_Im[[i]])$coefs.SE.CI$CI95.lo[2],summary(ppmgreen_6m_Im[[i]])$coefs.SE.CI$CI95.lo[2],summary(ppmgreen_9m_Im[[i]])$coefs.SE.CI$CI95.lo[2],summary(ppmgreen_12m_Im[[i]])$coefs.SE.CI$CI95.lo[2],summary(ppmgreen_ad_Im[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.lo.beta0_blue[[i]]  <- c(summary(ppmblue_3m_Im[[i]])$coefs.SE.CI$CI95.lo[1],summary(ppmblue_6m_Im[[i]])$coefs.SE.CI$CI95.lo[1],summary(ppmblue_9m_Im[[i]])$coefs.SE.CI$CI95.lo[1],summary(ppmblue_12m_Im[[i]])$coefs.SE.CI$CI95.lo[1],summary(ppmblue_ad_Im[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_blue[[i]]  <- c(summary(ppmblue_3m_Im[[i]])$coefs.SE.CI$CI95.lo[2],summary(ppmblue_6m_Im[[i]])$coefs.SE.CI$CI95.lo[2],summary(ppmblue_9m_Im[[i]])$coefs.SE.CI$CI95.lo[2],summary(ppmblue_12m_Im[[i]])$coefs.SE.CI$CI95.lo[2],summary(ppmblue_ad_Im[[i]])$coefs.SE.CI$CI95.lo[2])
  
  CI.lo.beta0_rgb[[i]]  <- c(summary(ppmrgb_3m_Im[[i]])$coefs.SE.CI$CI95.lo[1],summary(ppmred_6m_Im[[i]])$coefs.SE.CI$CI95.lo[1],summary(ppmrgb_9m_Im[[i]])$coefs.SE.CI$CI95.lo[1],summary(ppmrgb_12m_Im[[i]])$coefs.SE.CI$CI95.lo[1],summary(ppmrgb_ad_Im[[i]])$coefs.SE.CI$CI95.lo[1])
  CI.lo.beta1_rgb[[i]]  <- c(summary(ppmrgb_3m_Im[[i]])$coefs.SE.CI$CI95.lo[2],summary(ppmrgb_6m_Im[[i]])$coefs.SE.CI$CI95.lo[2],summary(ppmrgb_9m_Im[[i]])$coefs.SE.CI$CI95.lo[2],summary(ppmrgb_12m_Im[[i]])$coefs.SE.CI$CI95.lo[2],summary(ppmrgb_ad_Im[[i]])$coefs.SE.CI$CI95.lo[2])
  CI.lo.beta2_rgb[[i]]  <- c(summary(ppmrgb_3m_Im[[i]])$coefs.SE.CI$CI95.lo[3],summary(ppmrgb_6m_Im[[i]])$coefs.SE.CI$CI95.lo[3],summary(ppmrgb_9m_Im[[i]])$coefs.SE.CI$CI95.lo[3],summary(ppmrgb_12m_Im[[i]])$coefs.SE.CI$CI95.lo[3],summary(ppmrgb_ad_Im[[i]])$coefs.SE.CI$CI95.lo[3])
  CI.lo.beta3_rgb[[i]]  <- c(summary(ppmrgb_3m_Im[[i]])$coefs.SE.CI$CI95.lo[4],summary(ppmrgb_6m_Im[[i]])$coefs.SE.CI$CI95.lo[4],summary(ppmrgb_9m_Im[[i]])$coefs.SE.CI$CI95.lo[4],summary(ppmrgb_12m_Im[[i]])$coefs.SE.CI$CI95.lo[4],summary(ppmrgb_ad_Im[[i]])$coefs.SE.CI$CI95.lo[4]) 

  CI.hi.beta0_red[[i]]  <- c(summary(ppmred_3m_Im[[i]])$coefs.SE.CI$CI95.hi[1],summary(ppmred_6m_Im[[i]])$coefs.SE.CI$CI95.hi[1],summary(ppmred_9m_Im[[i]])$coefs.SE.CI$CI95.hi[1],summary(ppmred_12m_Im[[i]])$coefs.SE.CI$CI95.hi[1],summary(ppmred_ad_Im[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_red[[i]]  <- c(summary(ppmred_3m_Im[[i]])$coefs.SE.CI$CI95.hi[2],summary(ppmred_6m_Im[[i]])$coefs.SE.CI$CI95.hi[2],summary(ppmred_9m_Im[[i]])$coefs.SE.CI$CI95.hi[2],summary(ppmred_12m_Im[[i]])$coefs.SE.CI$CI95.hi[2],summary(ppmred_ad_Im[[i]])$coefs.SE.CI$CI95.hi[2])
  
  CI.hi.beta0_green[[i]]  <- c(summary(ppmgreen_3m_Im[[i]])$coefs.SE.CI$CI95.hi[1],summary(ppmgreen_6m_Im[[i]])$coefs.SE.CI$CI95.hi[1],summary(ppmgreen_9m_Im[[i]])$coefs.SE.CI$CI95.hi[1],summary(ppmgreen_12m_Im[[i]])$coefs.SE.CI$CI95.hi[1],summary(ppmgreen_ad_Im[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_green[[i]]  <- c(summary(ppmgreen_3m_Im[[i]])$coefs.SE.CI$CI95.hi[2],summary(ppmgreen_6m_Im[[i]])$coefs.SE.CI$CI95.hi[2],summary(ppmgreen_9m_Im[[i]])$coefs.SE.CI$CI95.hi[2],summary(ppmgreen_12m_Im[[i]])$coefs.SE.CI$CI95.hi[2],summary(ppmgreen_ad_Im[[i]])$coefs.SE.CI$CI95.hi[2])
  
  CI.hi.beta0_blue[[i]]  <- c(summary(ppmblue_3m_Im[[i]])$coefs.SE.CI$CI95.hi[1],summary(ppmblue_6m_Im[[i]])$coefs.SE.CI$CI95.hi[1],summary(ppmblue_9m_Im[[i]])$coefs.SE.CI$CI95.hi[1],summary(ppmblue_12m_Im[[i]])$coefs.SE.CI$CI95.hi[1],summary(ppmblue_ad_Im[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_blue[[i]]  <- c(summary(ppmblue_3m_Im[[i]])$coefs.SE.CI$CI95.hi[2],summary(ppmblue_6m_Im[[i]])$coefs.SE.CI$CI95.hi[2],summary(ppmblue_9m_Im[[i]])$coefs.SE.CI$CI95.hi[2],summary(ppmblue_12m_Im[[i]])$coefs.SE.CI$CI95.hi[2],summary(ppmblue_ad_Im[[i]])$coefs.SE.CI$CI95.hi[2])
  
  CI.hi.beta0_rgb[[i]]  <- c(summary(ppmrgb_3m_Im[[i]])$coefs.SE.CI$CI95.hi[1],summary(ppmred_6m_Im[[i]])$coefs.SE.CI$CI95.hi[1],summary(ppmrgb_9m_Im[[i]])$coefs.SE.CI$CI95.hi[1],summary(ppmrgb_12m_Im[[i]])$coefs.SE.CI$CI95.hi[1],summary(ppmrgb_ad_Im[[i]])$coefs.SE.CI$CI95.hi[1])
  CI.hi.beta1_rgb[[i]]  <- c(summary(ppmrgb_3m_Im[[i]])$coefs.SE.CI$CI95.hi[2],summary(ppmrgb_6m_Im[[i]])$coefs.SE.CI$CI95.hi[2],summary(ppmrgb_9m_Im[[i]])$coefs.SE.CI$CI95.hi[2],summary(ppmrgb_12m_Im[[i]])$coefs.SE.CI$CI95.hi[2],summary(ppmrgb_ad_Im[[i]])$coefs.SE.CI$CI95.hi[2])
  CI.hi.beta2_rgb[[i]]  <- c(summary(ppmrgb_3m_Im[[i]])$coefs.SE.CI$CI95.hi[3],summary(ppmrgb_6m_Im[[i]])$coefs.SE.CI$CI95.hi[3],summary(ppmrgb_9m_Im[[i]])$coefs.SE.CI$CI95.hi[3],summary(ppmrgb_12m_Im[[i]])$coefs.SE.CI$CI95.hi[3],summary(ppmrgb_ad_Im[[i]])$coefs.SE.CI$CI95.hi[3])
  CI.hi.beta3_rgb[[i]]  <- c(summary(ppmrgb_3m_Im[[i]])$coefs.SE.CI$CI95.hi[4],summary(ppmrgb_6m_Im[[i]])$coefs.SE.CI$CI95.hi[4],summary(ppmrgb_9m_Im[[i]])$coefs.SE.CI$CI95.hi[4],summary(ppmrgb_12m_Im[[i]])$coefs.SE.CI$CI95.hi[4],summary(ppmrgb_ad_Im[[i]])$coefs.SE.CI$CI95.hi[4]) 
}

# Soutirer les coefficients estimés (ok), erreurs standards (ok) et intervalles de confiances (ok)
# Présenter sous forme de matrices/dataframe
# Image 1
resume.ppm_red_Im1 <- data.frame(b0=beta0_red[[1]],b1=beta1_red[[1]],se.b0=S.E.beta0_red[[1]],se.b1=S.E.beta1_red[[1]],ci.lo.b0=CI.lo.beta0_red[[1]],ci.hi.b0=CI.hi.beta0_red[[1]],ci.lo.b1=CI.lo.beta1_red[[1]],ci.hi.b1=CI.hi.beta1_red[[1]])
rownames(resume.ppm_red_Im1)=c("bb3m","bb6m","bb9m","bb12m","adult")
resume.ppm_red_Im1 

# Pour toutes les images, ppm red
resume.ppm_red_Im <- list()
for(i in 1:6){
  resume.ppm_red_Im[[i]] <- data.frame(b0=beta0_red[[i]],b1=beta1_red[[i]],se.b0=S.E.beta0_red[[i]],se.b1=S.E.beta1_red[[i]],ci.lo.b0=CI.lo.beta0_red[[i]],ci.hi.b0=CI.hi.beta0_red[[i]],ci.lo.b1=CI.lo.beta1_red[[i]],ci.hi.b1=CI.hi.beta1_red[[i]])
  rownames(resume.ppm_red_Im[[i]])=c("bb3m","bb6m","bb9m","bb12m","adult")
}
resume.ppm_red_Im

# Pour toutes les images, ppm green
resume.ppm_green_Im <- list()
for(i in 1:6){
  resume.ppm_green_Im[[i]] <- data.frame(b0=beta0_green[[i]],b1=beta1_green[[i]],se.b0=S.E.beta0_green[[i]],se.b1=S.E.beta1_green[[i]],ci.lo.b0=CI.lo.beta0_green[[i]],ci.hi.b0=CI.hi.beta0_green[[i]],ci.lo.b1=CI.lo.beta1_green[[i]],ci.hi.b1=CI.hi.beta1_green[[i]])
  rownames(resume.ppm_green_Im[[i]])=c("bb3m","bb6m","bb9m","bb12m","adult")
}
resume.ppm_green_Im

# Pour toutes les images, ppm blue
resume.ppm_blue_Im <- list()
for(i in 1:6){
  resume.ppm_blue_Im[[i]] <- data.frame(b0=beta0_blue[[i]],b1=beta1_blue[[i]],se.b0=S.E.beta0_blue[[i]],se.b1=S.E.beta1_blue[[i]],ci.lo.b0=CI.lo.beta0_blue[[i]],ci.hi.b0=CI.hi.beta0_blue[[i]],ci.lo.b1=CI.lo.beta1_blue[[i]],ci.hi.b1=CI.hi.beta1_blue[[i]])
  rownames(resume.ppm_blue_Im[[i]])=c("bb3m","bb6m","bb9m","bb12m","adult")
}
resume.ppm_blue_Im

# Pour toutes les images, ppm red+blue+green
resume.ppm_rgb_Im <- list()
for(i in 1:6){
  resume.ppm_rgb_Im[[i]] <- data.frame(b0=beta0_rgb[[i]],b1=beta1_rgb[[i]],b2=beta2_rgb[[i]],b3=beta3_rgb[[i]],se.b0=S.E.beta0_rgb[[i]],se.b1=S.E.beta1_rgb[[i]],se.b2=S.E.beta2_rgb[[i]],se.b3=S.E.beta3_rgb[[i]],ci.lo.b0=CI.lo.beta0_rgb[[i]],ci.hi.b0=CI.hi.beta0_rgb[[i]],ci.lo.b1=CI.lo.beta1_rgb[[i]],ci.hi.b1=CI.hi.beta1_rgb[[i]],ci.lo.b2=CI.lo.beta2_rgb[[i]],ci.hi.b2=CI.hi.beta2_rgb[[i]],ci.lo.b3=CI.lo.beta3_rgb[[i]],ci.hi.b3=CI.hi.beta3_rgb[[i]])
  rownames(resume.ppm_rgb_Im[[i]])=c("bb3m","bb6m","bb9m","bb12m","adult")
}
resume.ppm_rgb_Im

