load("~/Desktop/MASSS/Stage/Codes R Antoine/WorkSpace_Antoine.RData")

#### Tester Poisson homogène sur chacune des images (tout groupes confondus)
### quadratest, enveloppe K de Ripley...
# Image 1
par(mfrow=c(1,2))
plot(unmark(X_Im1),cex=.4)
quadrat.test(unmark(X_Im1),nx=3)->q;plot(q,lwd=2,col='red');points(unmark(X_Im1),cex=.1);q

# on rejette l'hypothèse d'homogénéité Poissonienne 
# => Poisson non homogène

# Pour toutes les images
q <- list()
for(i in 1:6) {
  par(mfrow=c(1,2))
  plot(unmark(X_Im[[i]]),cex=.4)
  quadrat.test(unmark(X_Im[[i]]),nx=3)->q[[i]];plot(q[[i]],lwd=2,col='red');points(unmark(X_Im[[i]]),cex=.1);q[[i]]
}
q

# on rejette l'hypothèse d'homogénéité Poissonienne pour toutes les images
# => Poisson non homogène pour toutes les images

#### Tester Poisson homogène sur chacune des images (groupes séparés)
### quadratest, enveloppe K de Ripley...
q_b3m <- list()
q_b6m <- list()
q_b9m <- list()
q_b12m <- list()
q_adu <- list()
pvalue_3m <- vector()
pvalue_6m <- vector()
pvalue_9m <- vector()
pvalue_12m <- vector()
pvalue_adu <- vector()
for(i in 1:6) {
  # Groupe 3 mois
  par(mfrow=c(1,2))
  plot(X_baby3month_Im[[i]],cex=.4)
  quadrat.test(X_baby3month_Im[[i]],nx=3)->q_b3m[[i]];plot(q_b3m[[i]],lwd=2,col='red');points(X_baby3month_Im[[i]],cex=.1);q_b3m[[i]]
  pvalue_3m <- c(pvalue_3m,q_b3m[[i]]$p.value)
  # Groupe 6 mois
  par(mfrow=c(1,2))
  plot(X_baby6month_Im[[i]],cex=.4)
  quadrat.test(X_baby6month_Im[[i]],nx=3)->q_b6m[[i]];plot(q_b6m[[i]],lwd=2,col='red');points(X_baby6month_Im[[i]],cex=.1);q_b6m[[i]]
  pvalue_6m <- c(pvalue_6m,q_b6m[[i]]$p.value)
  # Groupe 9 mois
  par(mfrow=c(1,2))
  plot(X_baby9month_Im[[i]],cex=.4)
  quadrat.test(X_baby9month_Im[[i]],nx=3)->q_b9m[[i]];plot(q_b9m[[i]],lwd=2,col='red');points(X_baby9month_Im[[i]],cex=.1);q_b9m[[i]]
  pvalue_9m <- c(pvalue_9m,q_b9m[[i]]$p.value)
  # Groupe 12 mois
  par(mfrow=c(1,2))
  plot(X_baby12month_Im[[i]],cex=.4)
  quadrat.test(X_baby12month_Im[[i]],nx=3)->q_b12m[[i]];plot(q_b12m[[i]],lwd=2,col='red');points(X_baby12month_Im[[i]],cex=.1);q_b12m[[i]]
  pvalue_12m <- c(pvalue_12m,q_b12m[[i]]$p.value)
  # Groupe Adulte
  par(mfrow=c(1,2))
  plot(X_adult_Im[[i]],cex=.4)
  quadrat.test(X_adult_Im[[i]],nx=3)->q_adu[[i]];plot(q_adu[[i]],lwd=2,col='red');points(X_adult_Im[[i]],cex=.1);q_adu[[i]]
  pvalue_adu <- c(pvalue_adu,q_adu[[i]]$p.value)
}

# p-value groupe 3 mois
pvalue_3m
# p-value groupe 6 mois
pvalue_6m
# p-value groupe 9 mois
pvalue_9m
# p-value groupe 12 mois
pvalue_12m
# p-value groupe adulte
pvalue_adu

# => Poisson non homogène quelque soit l'image, quelque soit le groupe

# On peut également montré la non homogénéité avec les enveloppes de confiance
# Image 1
rr<-seq(0,50,l=100)

x11()
par(mfrow=c(3,2))
# PCF
plot(envelope(X_Im1,pcf,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Pcf, Image",i))
# Kest de Ripley
plot(envelope(X_Im1,Kest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Kest, Image",i))
# Lest
plot(envelope(X_Im1,Lest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Lest, Image",i))
# Fest
plot(envelope(X_Im1,Fest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Fest, Image",i))
# Gest
plot(envelope(X_Im1,Gest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Gest, Image",i))
# Jest
plot(envelope(X_Im1,Jest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Jest, Image",i))

# Pour toutes les images
i=1
rr<-seq(0,20,l=50)
for(i in 1:6){
  par(mfrow=c(2,2))
  # PCF
  #plot(envelope(X_Im[[i]],pcf,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Pcf, Image",i))
  # Kest de Ripley
  plot(envelope(X_Im[[i]],Kest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Kest, Image",i))
  # Lest
  plot(envelope(X_Im[[i]],Lest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Lest, Image",i))
  # Fest
  plot(envelope(X_Im[[i]],Fest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Fest, Image",i))
  # Gest
  plot(envelope(X_Im[[i]],Gest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Gest, Image",i))
  # Jest
  #plot(envelope(X_Im[[i]],Jest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Jest, Image",i))
}

# Pour toutes les images, pour tous les groupes
rr<-seq(0,50,l=100)
for(i in 1:1){
  # Baby 3 month
  x11()
  par(mfrow=c(3,2))
  plot(envelope(X_baby3month_Im[[i]],pcf,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Pcf, Bebes 3 mois, Image",i))
  plot(envelope(X_baby3month_Im[[i]],Kest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Kest, Bebes 3 mois, Image",i))
  plot(envelope(X_baby3month_Im[[i]],Lest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Lest, Bebes 3 mois, Image",i))
  plot(envelope(X_baby3month_Im[[i]],Fest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Fest, Bebes 3 mois, Image",i))
  plot(envelope(X_baby3month_Im[[i]],Gest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Gest, Bebes 3 mois, Image",i))
  plot(envelope(X_baby3month_Im[[i]],Jest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Jest, Bebes 3 mois, Image",i))
  
  # Baby 6 month
  x11()
  par(mfrow=c(3,2))
  plot(envelope(X_baby6month_Im[[i]],pcf,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Pcf, Bebes 6 mois, Image",i))
  plot(envelope(X_baby6month_Im[[i]],Kest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Kest, Bebes 6 mois, Image",i))
  plot(envelope(X_baby6month_Im[[i]],Lest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Lest, Bebes 6 mois, Image",i))
  plot(envelope(X_baby6month_Im[[i]],Fest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Fest, Bebes 6 mois, Image",i))
  plot(envelope(X_baby6month_Im[[i]],Gest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Gest, Bebes 6 mois, Image",i))
  plot(envelope(X_baby6month_Im[[i]],Jest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Jest, Bebes 6 mois, Image",i))
  
  # Baby 9 month
  x11()
  par(mfrow=c(3,2))
  plot(envelope(X_baby9month_Im[[i]],pcf,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Pcf, Bebes 9 mois, Image",i))
  plot(envelope(X_baby9month_Im[[i]],Kest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Kest, Bebes 9 mois, Image",i))
  plot(envelope(X_baby9month_Im[[i]],Lest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Lest, Bebes 9 mois, Image",i))
  plot(envelope(X_baby9month_Im[[i]],Fest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Fest, Bebes 9 mois, Image",i))
  plot(envelope(X_baby9month_Im[[i]],Gest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Gest, Bebes 9 mois, Image",i))
  plot(envelope(X_baby9month_Im[[i]],Jest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Jest, Bebes 9 mois, Image",i))
  
  # Baby 12 month
  x11()
  par(mfrow=c(3,2))
  plot(envelope(X_baby12month_Im[[i]],pcf,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Pcf, Bebes 12 mois, Image",i))
  plot(envelope(X_baby12month_Im[[i]],Kest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Kest, Bebes 12 mois, Image",i))
  plot(envelope(X_baby12month_Im[[i]],Lest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Lest, Bebes 12 mois, Image",i))
  plot(envelope(X_baby12month_Im[[i]],Fest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Fest, Bebes 12 mois, Image",i))
  plot(envelope(X_baby12month_Im[[i]],Gest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Gest, Bebes 12 mois, Image",i))
  plot(envelope(X_baby12month_Im[[i]],Jest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Jest, Bebes 12 mois, Image",i))
  
  # Adulte
  x11()
  par(mfrow=c(3,2))
  plot(envelope(X_adult_Im[[i]],pcf,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Pcf, adultes, Image",i))
  plot(envelope(X_adult_Im[[i]],Kest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Kest, adultes, Image",i))
  plot(envelope(X_adult_Im[[i]],Lest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Lest, adultes, Image",i))
  plot(envelope(X_adult_Im[[i]],Fest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Fest, adultes, Image",i))
  plot(envelope(X_adult_Im[[i]],Gest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Gest, adultes, Image",i))
  plot(envelope(X_adult_Im[[i]],Jest,r=rr,nsim=100,nrank=trunc(.025*100)),main=paste("Jest, adultes, Image",i))
}

# On a montré la non homogénéité poissonnienne
# Qu'en est il de la non inhomogénéité Poissonnienne ?

# Tester Poisson inhomogène
# Toutes les images, tout groupe confondus
# Adjusting for inhomogeneity

# Pour toutes les images
for(i in 1:6){
  par(mfrow=c(2,2))
  plot(Kinhom(X_Im[[i]], density(X_Im[[i]],bw.ppl)),main=paste("Kinhom, Image",i))
  plot(Linhom(X_Im[[i]], density(X_Im[[i]],bw.ppl)),main=paste("Linhom, Image",i))
  plot(Finhom(X_Im[[i]], density(X_Im[[i]],bw.ppl)),main=paste("Finhom, Image",i))
  plot(Ginhom(X_Im[[i]], density(X_Im[[i]],bw.ppl)),main=paste("Ginhom, Image",i))
}

# Enveloppes de confiance
for(i in 1:6){
  W=X_Im[[i]]
  lambda=density(X_Im[[i]], sigma=35) # Estimation non paramétrique de l'intensité
  plot(envelope(X_Im[[i]], Kinhom, sigma=100, simulate=expression(rpoispp(lambda, win=W)), nsim=19, global=TRUE),main=paste("Kinhom, Image",i))
}

# Pour toutes les images, pour chaque groupe
for(i in 1:6){
  # Bébés 3 mois
  par(mfrow=c(1,2))

  plot(Kinhom(X_baby3month_Im[[i]], density(X_baby3month_Im[[i]],bw.ppl)), main=paste("Kinhom, baby3m, Image",i))
  plot(Linhom(X_baby3month_Im[[i]], density(X_baby3month_Im[[i]],bw.ppl)), main=paste("Linhom, baby3m, Image",i))
  #plot(Finhom(X_baby3month_Im[[i]], density(X_baby3month_Im[[i]],bw.ppl)), main=paste("Finhom, baby3m, Image",i))
  #plot(Ginhom(X_baby3month_Im[[i]], density(X_baby3month_Im[[i]],bw.ppl)), main=paste("Ginhom, baby3m, Image",i))
  # Bébés 6 mois
  par(mfrow=c(1,2))
  plot(Kinhom(X_baby6month_Im[[i]], density(X_baby6month_Im[[i]],bw.ppl)), main=paste("Kinhom, baby6m, Image",i))
  plot(Linhom(X_baby6month_Im[[i]], density(X_baby6month_Im[[i]],bw.ppl)), main=paste("Linhom, baby6m, Image",i))
  #plot(Finhom(X_baby6month_Im[[i]], density(X_baby6month_Im[[i]],bw.ppl)), main=paste("Finhom, baby6m, Image",i))
  #plot(Ginhom(X_baby6month_Im[[i]], density(X_baby6month_Im[[i]],bw.ppl)), main=paste("Ginhom, baby6m, Image",i))
  # Bébés 9 mois
  par(mfrow=c(1,2))
  plot(Kinhom(X_baby9month_Im[[i]], density(X_baby9month_Im[[i]],bw.ppl)), main=paste("Kinhom, baby9m, Image",i))
  plot(Linhom(X_baby9month_Im[[i]], density(X_baby9month_Im[[i]],bw.ppl)), main=paste("Linhom, baby9m, Image",i))
  #plot(Finhom(X_baby9month_Im[[i]], density(X_baby9month_Im[[i]],bw.ppl)), main=paste("Finhom, baby9m, Image",i))
  #plot(Ginhom(X_baby9month_Im[[i]], density(X_baby9month_Im[[i]],bw.ppl)), main=paste("Ginhom, baby9m, Image",i))
  # Bébés 12 mois
  par(mfrow=c(1,2))
  plot(Kinhom(X_baby12month_Im[[i]], density(X_baby12month_Im[[i]],bw.ppl)), main=paste("Kinhom, baby12m, Image",i))
  plot(Linhom(X_baby12month_Im[[i]], density(X_baby12month_Im[[i]],bw.ppl)), main=paste("Linhom, baby12m, Image",i))
  #plot(Finhom(X_baby12month_Im[[i]], density(X_baby12month_Im[[i]],bw.ppl)), main=paste("Finhom, baby12m, Image",i))
  #plot(Ginhom(X_baby12month_Im[[i]], density(X_baby12month_Im[[i]],bw.ppl)), main=paste("Ginhom, baby12m, Image",i))
  # Adultes
  par(mfrow=c(1,2))
  plot(Kinhom(X_adult_Im[[i]], density(X_adult_Im[[i]],bw.ppl)), main=paste("Kinhom, adultes, Image",i))
  plot(Linhom(X_adult_Im[[i]], density(X_adult_Im[[i]],bw.ppl)), main=paste("Linhom, adultes, Image",i))
  #plot(Finhom(X_adult_Im[[i]], density(X_adult_Im[[i]],bw.ppl)), main=paste("Finhom, adultes, Image",i))
  #plot(Ginhom(X_adult_Im[[i]], density(X_adult_Im[[i]],bw.ppl)), main=paste("Ginhom, adultes, Image",i))
}

for(i in 4:4){
  W=X_baby3month_Im[[i]]
  # Baby 3 month
  lambda=density(X_baby3month_Im[[i]], sigma=35) # Estimation non paramétrique de l'intensité
  plot(envelope(X_baby3month_Im[[i]], Kinhom, sigma=100, simulate=expression(rpoispp(lambda,win=W)), nsim=19, global=TRUE),main=paste("Kinhom, 3m, Image Rafting"))

  W=X_baby6month_Im[[i]]
  # Baby 6 month
  lambda=density(X_baby6month_Im[[i]], sigma=35) # Estimation non paramétrique de l'intensité
  plot(envelope(X_baby6month_Im[[i]], Kinhom, sigma=100, simulate=expression(rpoispp(lambda,win=W)), nsim=19, global=TRUE),main=paste("Kinhom, 6m, Image Rafting"))

  W=X_baby9month_Im[[i]]
  # Baby 9 month
  lambda=density(X_baby9month_Im[[i]], sigma=35) # Estimation non paramétrique de l'intensité
  plot(envelope(X_baby9month_Im[[i]], Kinhom, sigma=100, simulate=expression(rpoispp(lambda,win=W)), nsim=19, global=TRUE),main=paste("Kinhom, 9m, Image Rafting"))

  W=X_baby12month_Im[[i]]
  # Baby 12 month
  lambda=density(X_baby12month_Im[[i]], sigma=35) # Estimation non paramétrique de l'intensité
  plot(envelope(X_baby12month_Im[[i]], Kinhom, sigma=100, simulate=expression(rpoispp(lambda,win=W)), nsim=19, global=TRUE),main=paste("Kinhom, 12m, Image Rafting"))

  W=X_adult_Im[[i]]
  # Adultes
  lambda=density(X_adult_Im[[i]], sigma=35) # Estimation non paramétrique de l'intensité
  plot(envelope(X_adult_Im[[i]], Kinhom, sigma=100, simulate=expression(rpoispp(lambda,win=W)), nsim=19, global=TRUE),main=paste("Kinhom, adultes, Image Rafting"))
}

# Fonctionne
# Kinhom, image 6, groupe bébés 3 mois
i=6
plot(Kinhom(X_baby3month_Im[[i]],density(X_baby3month_Im[[i]], bw.ppl)),main=paste("Kinhom, bb3m, Image",i))

# Ne fonctionne pas
# Enveloppes de confiance
i=6
W=X_baby3month_Im[[6]]
lambda=density(X_baby3month_Im[[i]], sigma=50) # Estimation non paramétrique de l'intensité
plot(envelope(X_baby3month_Im[[i]], Kinhom, sigma=100, simulate=expression(rpoispp(lambda,win=W)), nsim=19, global=TRUE),main=paste("Kinhom, bb3m, Image",i))
