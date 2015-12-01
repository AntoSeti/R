# Workspace nécessaire à la lecture images sous Mac (r1--r6)
load("~/Desktop/MASSS/Stage/Codes R Antoine/WorkSpace_Antoine.RData")
r=list(r1,r2,r3,r4,r5,r6)

## Créer un ppp général avec "mark"
# Commençons par créer un ppp comprenant tous les individus pour une image
# Image 1
x<-c(Fixations_coordx_baby3month_Im1,Fixations_coordx_baby6month_Im1,Fixations_coordx_baby9month_Im1,Fixations_coordx_baby12month_Im1,Fixations_coordx_adult_Im1)
y<-c(Fixations_coordy_baby3month_Im1,Fixations_coordy_baby6month_Im1,Fixations_coordy_baby9month_Im1,Fixations_coordy_baby12month_Im1,Fixations_coordy_adult_Im1)
X_Im1 <- ppp(x, y, c(0,1024), c(0,768), unitname=c("pixels"))

# Image 2
x<-c(Fixations_coordx_baby3month_Im2,Fixations_coordx_baby6month_Im2,Fixations_coordx_baby9month_Im2,Fixations_coordx_baby12month_Im2,Fixations_coordx_adult_Im2)
y<-c(Fixations_coordy_baby3month_Im2,Fixations_coordy_baby6month_Im2,Fixations_coordy_baby9month_Im2,Fixations_coordy_baby12month_Im2,Fixations_coordy_adult_Im2)
X_Im2 <- ppp(x, y, c(0,1024), c(0,768), unitname=c("pixels"))

# Image 3
x<-c(Fixations_coordx_baby3month_Im3,Fixations_coordx_baby6month_Im3,Fixations_coordx_baby9month_Im3,Fixations_coordx_baby12month_Im3,Fixations_coordx_adult_Im3)
y<-c(Fixations_coordy_baby3month_Im3,Fixations_coordy_baby6month_Im3,Fixations_coordy_baby9month_Im3,Fixations_coordy_baby12month_Im3,Fixations_coordy_adult_Im3)
X_Im3 <- ppp(x, y, c(0,1024), c(0,768), unitname=c("pixels"))

# Image 4
x<-c(Fixations_coordx_baby3month_Im4,Fixations_coordx_baby6month_Im4,Fixations_coordx_baby9month_Im4,Fixations_coordx_baby12month_Im4,Fixations_coordx_adult_Im4)
y<-c(Fixations_coordy_baby3month_Im4,Fixations_coordy_baby6month_Im4,Fixations_coordy_baby9month_Im4,Fixations_coordy_baby12month_Im4,Fixations_coordy_adult_Im4)
X_Im4 <- ppp(x, y, c(0,1024), c(0,768), unitname=c("pixels"))

# Image 5
x<-c(Fixations_coordx_baby3month_Im5,Fixations_coordx_baby6month_Im5,Fixations_coordx_baby9month_Im5,Fixations_coordx_baby12month_Im5,Fixations_coordx_adult_Im5)
y<-c(Fixations_coordy_baby3month_Im5,Fixations_coordy_baby6month_Im5,Fixations_coordy_baby9month_Im5,Fixations_coordy_baby12month_Im5,Fixations_coordy_adult_Im5)
X_Im5 <- ppp(x, y, c(0,1024), c(0,768), unitname=c("pixels"))

# Image 6
x<-c(Fixations_coordx_baby3month_Im6,Fixations_coordx_baby6month_Im6,Fixations_coordx_baby9month_Im6,Fixations_coordx_baby12month_Im6,Fixations_coordx_adult_Im6)
y<-c(Fixations_coordy_baby3month_Im6,Fixations_coordy_baby6month_Im6,Fixations_coordy_baby9month_Im6,Fixations_coordy_baby12month_Im6,Fixations_coordy_adult_Im6)
X_Im6 <- ppp(x, y, c(0,1024), c(0,768), unitname=c("pixels"))

X_Im=list(X_Im1,X_Im2,X_Im3,X_Im4,X_Im5,X_Im6)
# Avec images
x11()
par(mfrow=c(3,2))
for(i in 1:6) {
m <- as.raster(r[[i]], max = 255)
op <- par(bg = "cornsilk")
plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main="Fixations de tous les sujets")
rasterImage(m, 0, 0, 1024, 768)
par(op)
points(X_Im[[i]],pch=3,col="green")
}

###### Créer des "mark" (5 groupes d'âges) ######
# Image 1
# 1901 points
# Pour récupérer le nombre de fixations par groupes pour une image donnée
X_baby3month_Im[[1]]$n
mark1 <-c(rep("3mois",X_baby3month_Im[[1]]$n),rep("6mois",X_baby6month_Im[[1]]$n),rep("9mois",X_baby9month_Im[[1]]$n),rep("12mois",X_baby12month_Im[[1]]$n),rep("Adulte",X_adult_Im[[1]]$n))
mark1 <- factor(mark1, levels=c("3mois","6mois","9mois", "12mois", "Adulte"))
length(mark1)
# 1901 points OK

# Rappel
x<-c(Fixations_coordx_baby3month_Im1,Fixations_coordx_baby6month_Im1,Fixations_coordx_baby9month_Im1,Fixations_coordx_baby12month_Im1,Fixations_coordx_adult_Im1)
y<-c(Fixations_coordy_baby3month_Im1,Fixations_coordy_baby6month_Im1,Fixations_coordy_baby9month_Im1,Fixations_coordy_baby12month_Im1,Fixations_coordy_adult_Im1)
X_Im1 <- ppp(x, y, c(0,1024), c(0,768), unitname=c("pixels"), marks=mark1)
plot(X_Im1)
points(X_Im1,pch=3,col=mark1)

# Avec images
x11()
m <- as.raster(r[[1]], max = 255)
op <- par(bg = "cornsilk")
plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main="Fixations de tous les sujets")
rasterImage(m, 0, 0, 1024, 768)
par(op)
points(X_Im1,pch=3,col=mark1)

# Pour les 6 images
x11()
par(mfrow=c(2,3))
for (i in 1:6) {
  x<-c(Fixations_coordx_baby3month_Im[[i]],Fixations_coordx_baby6month_Im[[i]],Fixations_coordx_baby9month_Im[[i]],Fixations_coordx_baby12month_Im[[i]],Fixations_coordx_adult_Im[[i]])
  y<-c(Fixations_coordy_baby3month_Im[[i]],Fixations_coordy_baby6month_Im[[i]],Fixations_coordy_baby9month_Im[[i]],Fixations_coordy_baby12month_Im[[i]],Fixations_coordy_adult_Im[[i]])
  mark1 <-c(rep("3mois",X_baby3month_Im[[i]]$n),rep("6mois",X_baby6month_Im[[i]]$n),rep("9mois",X_baby9month_Im[[i]]$n),rep("12mois",X_baby12month_Im[[i]]$n),rep("Adulte",X_adult_Im[[i]]$n))
  mark1 <- factor(mark1, levels=c("3mois","6mois","9mois", "12mois", "Adulte"))
  X_Im[[i]] <- ppp(x, y, c(0,1024), c(0,768), unitname=c("pixels"), marks=mark1)
  plot(X_Im[[i]],paste("Fixations, Image",i,", selon les groupes"))
  points(X_Im[[i]],col=mark1)
}

# Avec images
x11()
par(mfrow=c(2,3))
for (i in 1:6) {
x<-c(Fixations_coordx_baby3month_Im[[i]],Fixations_coordx_baby6month_Im[[i]],Fixations_coordx_baby9month_Im[[i]],Fixations_coordx_baby12month_Im[[i]],Fixations_coordx_adult_Im[[i]])
y<-c(Fixations_coordy_baby3month_Im[[i]],Fixations_coordy_baby6month_Im[[i]],Fixations_coordy_baby9month_Im[[i]],Fixations_coordy_baby12month_Im[[i]],Fixations_coordy_adult_Im[[i]])
mark1 <-c(rep("3mois",X_baby3month_Im[[i]]$n),rep("6mois",X_baby6month_Im[[i]]$n),rep("9mois",X_baby9month_Im[[i]]$n),rep("12mois",X_baby12month_Im[[i]]$n),rep("Adulte",X_adult_Im[[i]]$n))
mark1 <- factor(mark1, levels=c("3mois","6mois","9mois", "12mois", "Adulte"))
X_Im[[i]] <- ppp(x, y, c(0,1024), c(0,768), unitname=c("pixels"), marks=mark1)
m <- as.raster(r[[i]], max = 255)
op <- par(bg = "cornsilk")
plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", selon les groupes"))
rasterImage(m, 0, 0, 1024, 768)
par(op)
points(X_Im[[i]],col=mark1)
}

# On devrait retrouver le travail précédemment fait en divisant par "mark" :
for(i in 1:6) {
plot(split(X_Im[[i]]),main=paste("Fixations, Image",i,", divisées en groupes"))
}


for(i in 1:6) {
  par(mfrow=c(2,3))
  plot(X_baby3month_Im[[i]],main=paste("Fixations, Image",i,", bébés de 3 mois"))
  plot(X_baby6month_Im[[i]],main=paste("Fixations, Image",i,", bébés de 6 mois"))
  plot(X_baby9month_Im[[i]],main=paste("Fixations, Image",i,", bébés de 9 mois"))
  plot(X_baby12month_Im[[i]],main=paste("Fixations, Image",i,", bébés de 12 mois"))
  plot(X_adult_Im[[i]],main=paste("Fixations, Image",i,", Adultes"))
}

plot(split(X_Im[[6]]),main=paste("Fixations, Image",i,", divisées en groupes"))
# OK

# On retrouve toutes les fixations :
for(i in 1:6) {
plot(unmark(X_Im[[i]]))
}

# Attention, beaucoup plus de points adultes que de bébés quelque soit l'image
tableau <- list()
for(i in 1:6) {
  tableau[[i]] <- table(marks(X_Im[[i]]))
  barplot(tableau[[i]],main=paste("Nombre de fixations, Image",i),col="lightblue")
}

# On peut aussi calculer le nombre total de fixations par groupe d'âge
tableau <- list()
for(i in 1:6) {
  tableau[[i]] <- table(marks(X_Im[[i]]))
}
barplot(tableau[[1]]+tableau[[2]]+tableau[[3]]+tableau[[4]]+tableau[[5]]+
          tableau[[6]],main=paste("Nombre de fixations total par groupe"),col="lightblue")
tableau[[1]]+tableau[[2]]+tableau[[3]]+tableau[[4]]+tableau[[5]]+tableau[[6]]

# On peut aussi calculer le nombre total de fixations par images
par(mfrow=c(2,3))
tableau <- list()
for(i in 1:6) {
  tableau[[i]] <- table(marks(X_Im[[i]]))
  barplot(sum(tableau[[i]]),main=paste("Nombre de fixations total pour l'image",i),
          col="lightblue")
  sum(tableau[[i]])
}
c(sum(tableau[[1]]),sum(tableau[[2]]),sum(tableau[[3]]),sum(tableau[[4]]),
  sum(tableau[[5]]),sum(tableau[[6]]))

# On peut aussi calculer le nombre total de fixations
tableau <- list()
for(i in 1:6) {
  tableau[[i]] <- table(marks(X_Im[[i]]))
}
barplot(sum(tableau[[1]])+sum(tableau[[2]])+sum(tableau[[3]])+sum(tableau[[4]])+
          sum(tableau[[5]])+sum(tableau[[6]]),
        main=paste("Nombre de fixations par groupe"),col="lightblue")
sum(tableau[[1]])+sum(tableau[[2]])+sum(tableau[[3]])+sum(tableau[[4]])+
  sum(tableau[[5]])+sum(tableau[[6]])

# Interprétations ?
par(mfrow=c(3,2))
for(i in 1:3) {
x<-c(Fixations_coordx_baby3month_Im[[i]],Fixations_coordx_baby6month_Im[[i]],Fixations_coordx_baby9month_Im[[i]],Fixations_coordx_baby12month_Im[[i]],Fixations_coordx_adult_Im[[i]])
y<-c(Fixations_coordy_baby3month_Im[[i]],Fixations_coordy_baby6month_Im[[i]],Fixations_coordy_baby9month_Im[[i]],Fixations_coordy_baby12month_Im[[i]],Fixations_coordy_adult_Im[[i]])
mark1 <-c(rep("3mois",X_baby3month_Im[[i]]$n),rep("6mois",X_baby6month_Im[[i]]$n),rep("9mois",X_baby9month_Im[[i]]$n),rep("12mois",X_baby12month_Im[[i]]$n),rep("Adulte",X_adult_Im[[i]]$n))
mark1 <- factor(mark1, levels=c("3mois","6mois","9mois", "12mois", "Adulte"))

# Multitype intensity and relative risk (p.166)
b <- bw.relrisk(X_Im[[i]])
rr <- relrisk(X_Im[[i]], sigma=b)
# Estimates of spatially-varying proportions of each species
# plot(rr)
dominant <- im.apply(rr, which.max)
species <- levels(marks(X_Im[[i]]))
dominant <- eval.im(factor(dominant, levels=1:5,labels=species))

m <- as.raster(r[[i]], max = 255)
op <- par(bg = "cornsilk")
plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", pour tous les groupes"))
rasterImage(m, 0, 0, 1024, 768) 
par(op)
points(unmark(X_Im[[i]]),pch=3,col=mark1)
textureplot(dominant,main=paste("Porportion spatiale des groupes d'âge, Image",i))
}
par(mfrow=c(3,2))
for(i in 4:6) {
  x<-c(Fixations_coordx_baby3month_Im[[i]],Fixations_coordx_baby6month_Im[[i]],Fixations_coordx_baby9month_Im[[i]],Fixations_coordx_baby12month_Im[[i]],Fixations_coordx_adult_Im[[i]])
  y<-c(Fixations_coordy_baby3month_Im[[i]],Fixations_coordy_baby6month_Im[[i]],Fixations_coordy_baby9month_Im[[i]],Fixations_coordy_baby12month_Im[[i]],Fixations_coordy_adult_Im[[i]])
  mark1 <-c(rep("3mois",X_baby3month_Im[[i]]$n),rep("6mois",X_baby6month_Im[[i]]$n),rep("9mois",X_baby9month_Im[[i]]$n),rep("12mois",X_baby12month_Im[[i]]$n),rep("Adulte",X_adult_Im[[i]]$n))
  mark1 <- factor(mark1, levels=c("3mois","6mois","9mois", "12mois", "Adulte"))
  
  # Multitype intensity and relative risk
  b <- bw.relrisk(X_Im[[i]])
  rr <- relrisk(X_Im[[i]], sigma=b)
  # Estimates of spatially-varying proportions of each species
  # plot(rr)
  dominant <- im.apply(rr, which.max)
  species <- levels(marks(X_Im[[i]]))
  dominant <- eval.im(factor(dominant, levels=1:5,labels=species))
  
  m <- as.raster(r[[i]], max = 255)
  op <- par(bg = "cornsilk")
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", pour tous les groupes"))
  rasterImage(m, 0, 0, 1024, 768) 
  par(op)
  points(unmark(X_Im[[i]]),pch=3,col=mark1)
  textureplot(dominant,main=paste("Porportion spatiale des groupes d'âge, Image",i))
}

# Intensités estimées par groupes
# Affichage images
par(mfrow=c(3,2))
for(i in 1:6){
m <- as.raster(r[[i]], max = 255)
op <- par(bg = "cornsilk")
plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Image",i))
rasterImage(m, 0, 0, 1024, 768)
par(op)
}

for(i in 1:6) {
plot(density(split(X_Im[[i]])),main=paste("Intensités estimées pour les différents groupes, Image", i))
}

###### Créer des "mark" (une marque par sujet) (même raisonnement) ######
## Image 1
# Bébé de 3 mois (43), Image 1
mark1=vector()
for(i in 1:43) {
  mark1 <-c(mark1,rep(paste("Sujet",i),length(baby3month_Im[[i]]$Im1[,1])))
}

# On ajoute les bébés de 6 mois (47),Image 1
mark2=vector()
for(i in 1:47) {
  mark2 <-c(mark2,rep(paste("Sujet",43+i),length(baby6month_Im[[i]]$Im1[,1])))
}

# On ajoute les bébés de 9 mois (44),Image 1
mark3=vector()
for(i in 1:44) {
  mark3 <-c(mark3,rep(paste("Sujet",43+47+i),length(baby9month_Im[[i]]$Im1[,1])))
}

# On ajoute les bébés de 12 mois (46),Image 1
mark4=vector()
for(i in 1:46) {
  mark4 <-c(mark4,rep(paste("Sujet",43+47+44+i),length(baby12month_Im[[i]]$Im1[,1])))
}

# On ajoute les adultes (44),Image 1
mark5=vector()
for(i in 1:44) {
  mark5 <-c(mark5,rep(paste("Sujet",43+47+44+46+i),length(adult_Im[[i]]$Im1[,1])))
}
mark=c(mark1,mark2,mark3,mark4,mark5)
mark <- factor(mark)

## Pour toutes les images
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

mark21=vector()
mark22=vector()
mark23=vector()
mark24=vector()
mark25=vector()
mark26=vector()
for(i in 1:47) {
  mark21 <-c(mark21,rep(paste("Sujet",43+i),length(baby6month_Im[[i]]$Im1[,1])))
  mark22 <-c(mark22,rep(paste("Sujet",43+i),length(baby6month_Im[[i]]$Im2[,1])))
  mark23 <-c(mark23,rep(paste("Sujet",43+i),length(baby6month_Im[[i]]$Im3[,1])))
  mark24 <-c(mark24,rep(paste("Sujet",43+i),length(baby6month_Im[[i]]$Im4[,1])))
  mark25 <-c(mark25,rep(paste("Sujet",43+i),length(baby6month_Im[[i]]$Im5[,1])))
  mark26 <-c(mark26,rep(paste("Sujet",43+i),length(baby6month_Im[[i]]$Im6[,1])))
}

mark31=vector()
mark32=vector()
mark33=vector()
mark34=vector()
mark35=vector()
mark36=vector()
for(i in 1:44) {
  mark31 <-c(mark31,rep(paste("Sujet",43+47+i),length(baby9month_Im[[i]]$Im1[,1])))
  mark32 <-c(mark32,rep(paste("Sujet",43+47+i),length(baby9month_Im[[i]]$Im2[,1])))
  mark33 <-c(mark33,rep(paste("Sujet",43+47+i),length(baby9month_Im[[i]]$Im3[,1])))
  mark34 <-c(mark34,rep(paste("Sujet",43+47+i),length(baby9month_Im[[i]]$Im4[,1])))
  mark35 <-c(mark35,rep(paste("Sujet",43+47+i),length(baby9month_Im[[i]]$Im5[,1])))
  mark36 <-c(mark36,rep(paste("Sujet",43+47+i),length(baby9month_Im[[i]]$Im6[,1])))
}

mark41=vector()
mark42=vector()
mark43=vector()
mark44=vector()
mark45=vector()
mark46=vector()
for(i in 1:46) {
  mark41 <-c(mark41,rep(paste("Sujet",43+47+44+i),length(baby12month_Im[[i]]$Im1[,1])))
  mark42 <-c(mark42,rep(paste("Sujet",43+47+44+i),length(baby12month_Im[[i]]$Im2[,1])))
  mark43 <-c(mark43,rep(paste("Sujet",43+47+44+i),length(baby12month_Im[[i]]$Im3[,1])))
  mark44 <-c(mark44,rep(paste("Sujet",43+47+44+i),length(baby12month_Im[[i]]$Im4[,1])))
  mark45 <-c(mark45,rep(paste("Sujet",43+47+44+i),length(baby12month_Im[[i]]$Im5[,1])))
  mark46 <-c(mark46,rep(paste("Sujet",43+47+44+i),length(baby12month_Im[[i]]$Im6[,1])))
}

mark51=vector()
mark52=vector()
mark53=vector()
mark54=vector()
mark55=vector()
mark56=vector()
for(i in 1:44) {
  mark51 <-c(mark51,rep(paste("Sujet",43+47+44+46+i),length(adult_Im[[i]]$Im1[,1])))
  mark52 <-c(mark52,rep(paste("Sujet",43+47+44+46+i),length(adult_Im[[i]]$Im2[,1])))
  mark53 <-c(mark53,rep(paste("Sujet",43+47+44+46+i),length(adult_Im[[i]]$Im3[,1])))
  mark54 <-c(mark54,rep(paste("Sujet",43+47+44+46+i),length(adult_Im[[i]]$Im4[,1])))
  mark55 <-c(mark55,rep(paste("Sujet",43+47+44+46+i),length(adult_Im[[i]]$Im5[,1])))
  mark56 <-c(mark56,rep(paste("Sujet",43+47+44+46+i),length(adult_Im[[i]]$Im6[,1])))
}

mark1=factor(c(mark11,mark21,mark31,mark41,mark51))
mark2=factor(c(mark12,mark22,mark32,mark42,mark52))
mark3=factor(c(mark13,mark23,mark33,mark43,mark53))
mark4=factor(c(mark14,mark24,mark34,mark44,mark54))
mark5=factor(c(mark15,mark25,mark35,mark45,mark55))
mark6=factor(c(mark16,mark26,mark36,mark46,mark56))
mark.l=list(mark1,mark2,mark3,mark4,mark5,mark6)

# 10462 fixations totales (vérifié de 2 façons)
length(mark1)+length(mark2)+length(mark3)+length(mark4)+length(mark5)+length(mark6)
sum(X_Im1$n+X_Im2$n+X_Im3$n+X_Im4$n+X_Im5$n+X_Im6$n)

##### Récupération des coordonnées
# Groupe 3 mois
# Image 1
x11<-vector()
y11<-vector()
for (i in 1:43) {
  x11<-c(x11,baby3month_Im[[i]]$Im1[[3]])
  y11<-c(y11,baby3month_Im[[i]]$Im1[[4]])
}

# Groupe 3 mois
# Image 2
x12<-vector()
y12<-vector()
for (i in 1:43) {
  x12<-c(x12,baby3month_Im[[i]]$Im2[[3]])
  y12<-c(y12,baby3month_Im[[i]]$Im2[[4]])
}

# Groupe 3 mois
# Image 3
x13<-vector()
y13<-vector()
for (i in 1:43) {
  x13<-c(x13,baby3month_Im[[i]]$Im3[[3]])
  y13<-c(y13,baby3month_Im[[i]]$Im3[[4]])
}

# Groupe 3 mois
# Image 4
x14<-vector()
y14<-vector()
for (i in 1:43) {
  x14<-c(x14,baby3month_Im[[i]]$Im4[[3]])
  y14<-c(y14,baby3month_Im[[i]]$Im4[[4]])
}

# Groupe 3 mois
# Image 5
x15<-vector()
y15<-vector()
for (i in 1:43) {
  x15<-c(x15,baby3month_Im[[i]]$Im5[[3]])
  y15<-c(y15,baby3month_Im[[i]]$Im5[[4]])
}
  
# Groupe 3 mois
# Image 6
x16<-vector()
y16<-vector()
for (i in 1:43) {
  x16<-c(x16,baby3month_Im[[i]]$Im6[[3]])
  y16<-c(y16,baby3month_Im[[i]]$Im6[[4]])
}

# Groupe 6 mois
# Image 1
x21<-vector()
y21<-vector()
for (i in 1:47) {
  x21<-c(x21,baby6month_Im[[i]]$Im1[[3]])
  y21<-c(y21,baby6month_Im[[i]]$Im1[[4]])
}

# Groupe 6 mois
# Image 2
x22<-vector()
y22<-vector()
for (i in 1:47) {
  x22<-c(x22,baby6month_Im[[i]]$Im2[[3]])
  y22<-c(y22,baby6month_Im[[i]]$Im2[[4]])
}

# Groupe 6 mois
# Image 3
x23<-vector()
y23<-vector()
for (i in 1:47) {
  x23<-c(x23,baby6month_Im[[i]]$Im3[[3]])
  y23<-c(y23,baby6month_Im[[i]]$Im3[[4]])
}

# Groupe 6 mois
x24<-vector()
y24<-vector()
for (i in 1:47) {
  x24<-c(x24,baby6month_Im[[i]]$Im4[[3]])
  y24<-c(y24,baby6month_Im[[i]]$Im4[[4]])
}

# Groupe 6 mois
# Image 5
x25<-vector()
y25<-vector()
for (i in 1:47) {
  x25<-c(x25,baby6month_Im[[i]]$Im5[[3]])
  y25<-c(y25,baby6month_Im[[i]]$Im5[[4]])
}

# Groupe 6 mois
# Image 6
x26<-vector()
y26<-vector()
for (i in 1:47) {
  x26<-c(x26,baby6month_Im[[i]]$Im6[[3]])
  y26<-c(y26,baby6month_Im[[i]]$Im6[[4]])
}

# Groupe 9 mois
# Image 1
x31<-vector()
y31<-vector()
for (i in 1:44) {
  x31<-c(x31,baby9month_Im[[i]]$Im1[[3]])
  y31<-c(y31,baby9month_Im[[i]]$Im1[[4]])
}

# Groupe 9 mois
# Image 2
x32<-vector()
y32<-vector()
for (i in 1:44) {
  x32<-c(x32,baby9month_Im[[i]]$Im2[[3]])
  y32<-c(y32,baby9month_Im[[i]]$Im2[[4]])
}

# Groupe 9 mois
# Image 3
x33<-vector()
y33<-vector()
for (i in 1:44) {
  x33<-c(x33,baby9month_Im[[i]]$Im3[[3]])
  y33<-c(y33,baby9month_Im[[i]]$Im3[[4]])
}

# Groupe 9 mois
# Image 4
x34<-vector()
y34<-vector()
for (i in 1:44) {
  x34<-c(x34,baby9month_Im[[i]]$Im4[[3]])
  y34<-c(y34,baby9month_Im[[i]]$Im4[[4]])
}

# Groupe 9 mois
# Image 5
x35<-vector()
y35<-vector()
for (i in 1:44) {
  x35<-c(x35,baby9month_Im[[i]]$Im5[[3]])
  y35<-c(y35,baby9month_Im[[i]]$Im5[[4]])
}

# Groupe 9 mois
# Image 6
x36<-vector()
y36<-vector()
for (i in 1:44) {
  x36<-c(x36,baby9month_Im[[i]]$Im6[[3]])
  y36<-c(y36,baby9month_Im[[i]]$Im6[[4]])
}

# Groupe 12 mois
# Image 1
x41<-vector()
y41<-vector()
for (i in 1:46) {
  x41<-c(x41,baby12month_Im[[i]]$Im1[[3]])
  y41<-c(y41,baby12month_Im[[i]]$Im1[[4]])
}

# Groupe 12 mois
# Image 2
x42<-vector()
y42<-vector()
for (i in 1:46) {
  x42<-c(x42,baby12month_Im[[i]]$Im2[[3]])
  y42<-c(y42,baby12month_Im[[i]]$Im2[[4]])
}

# Groupe 12 mois
# Image 3
x43<-vector()
y43<-vector()
for (i in 1:46) {
  x43<-c(x43,baby12month_Im[[i]]$Im3[[3]])
  y43<-c(y43,baby12month_Im[[i]]$Im3[[4]])
}

# Groupe 12 mois
# Image 4
x44<-vector()
y44<-vector()
for (i in 1:46) {
  x44<-c(x44,baby12month_Im[[i]]$Im4[[3]])
  y44<-c(y44,baby12month_Im[[i]]$Im4[[4]])
}

# Groupe 12 mois
# Image 5
x45<-vector()
y45<-vector()
for (i in 1:46) {
  x45<-c(x45,baby12month_Im[[i]]$Im5[[3]])
  y45<-c(y45,baby12month_Im[[i]]$Im5[[4]])
}

# Groupe 12 mois
# Image 6
x46<-vector()
y46<-vector()
for (i in 1:46) {
  x46<-c(x46,baby12month_Im[[i]]$Im6[[3]])
  y46<-c(y46,baby12month_Im[[i]]$Im6[[4]])
}

# Groupe adulte
# Image 1
x51<-vector()
y51<-vector()
for (i in 1:44) {
  x51<-c(x51,adult_Im[[i]]$Im1[[3]])
  y51<-c(y51,adult_Im[[i]]$Im1[[4]])
}

# Groupe adulte
# Image 2
x52<-vector()
y52<-vector()
for (i in 1:44) {
  x52<-c(x52,adult_Im[[i]]$Im2[[3]])
  y52<-c(y52,adult_Im[[i]]$Im2[[4]])
}

# Groupe adulte
# Image 3
x53<-vector()
y53<-vector()
for (i in 1:44) {
  x53<-c(x53,adult_Im[[i]]$Im3[[3]])
  y53<-c(y53,adult_Im[[i]]$Im3[[4]])
}

# Groupe adulte
# Image 4
x54<-vector()
y54<-vector()
for (i in 1:44) {
  x54<-c(x54,adult_Im[[i]]$Im4[[3]])
  y54<-c(y54,adult_Im[[i]]$Im4[[4]])
}

# Groupe adulte
# Image 5
x55<-vector()
y55<-vector()
for (i in 1:44) {
  x55<-c(x55,adult_Im[[i]]$Im5[[3]])
  y55<-c(y55,adult_Im[[i]]$Im5[[4]])
}

# Groupe adulte
# Image 6
x56<-vector()
y56<-vector()
for (i in 1:44) {
  x56<-c(x56,adult_Im[[i]]$Im6[[3]])
  y56<-c(y56,adult_Im[[i]]$Im6[[4]])
}

# Bonne configuration (ordre) de lecture des données
x1 <- c(x11,x21,x31,x41,x51)
x2 <- c(x12,x22,x32,x42,x52)
x3 <- c(x13,x23,x33,x43,x53)
x4 <- c(x14,x24,x34,x44,x54)
x5 <- c(x15,x25,x35,x45,x55)
x6 <- c(x16,x26,x36,x46,x56)
y1 <- c(y11,y21,y31,y41,y51)
y2 <- c(y12,y22,y32,y42,y52)
y3 <- c(y13,y23,y33,y43,y53)
y4 <- c(y14,y24,y34,y44,y54)
y5 <- c(y15,y25,y35,y45,y55)
y6 <- c(y16,y26,y36,y46,y56)
length(x1)==length(mark1)
length(y1)==length(mark1)

length(x2)==length(mark2)
length(y2)==length(mark2)

length(x3)==length(mark3)
length(y3)==length(mark3)

length(x4)==length(mark4)
length(y4)==length(mark4)

length(x5)==length(mark5)
length(y5)==length(mark5)

length(x6)==length(mark6)
length(y6)==length(mark6)
## OK pour les dimensions

## Application :
mark1=factor(mark1)
# Image 1
x11()
for (i in 1:1) {
  X_Im[[i]] <- ppp(x1,768-y1, c(0,1024), c(0,768), unitname=c("pixels"), marks=mark1)
  #plot(X_Im[[i]],paste("Fixations, Image",i,", selon les sujets"))
  #points(X_Im[[i]],col=mark1)
}

# Pour toutes les images
x11()
par(mfrow=c(2,3))
x.l=list(x1,x2,x3,x4,x5,x6)
y.l=list(768-y1,768-y2,768-y3,768-y4,768-y5,768-y6)
mark.l=list(mark1,mark2,mark3,mark4,mark5,mark6)
for (i in 1:6) {
  X_Im[[i]] <- ppp(x.l[[i]],y.l[[i]], c(0,1024), c(0,768), unitname=c("pixels"), marks=mark.l[[i]])
  #plot(X_Im[[i]],paste("Fixations, Image",i,", selon les sujets"))
  #points(X_Im[[i]],col=mark.l[[i]])
}

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


par(mfrow=c(1,1))
X_3m_Im <- list()
for (i in 1:6) {
  # m <- as.raster(r[[i]], max = 255)
  # plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main="", axes=FALSE)
  # rasterImage(m, 0, 0, 1024, 768)
  X_3m_Im[[i]] <- ppp(x.l[[i]],y.l[[i]], c(0,1024), c(0,768), unitname=c("pixels"), marks=mark.l[[i]])
  # points(X_3m_Im[[i]],cex=.3,pch=19,col=mark.l[[i]])
}

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

par(mfrow=c(2,3))
X_6m_Im <- list()
for (i in 1:6) {
  X_6m_Im[[i]] <- ppp(x.l[[i]],y.l[[i]], c(0,1024), c(0,768), unitname=c("pixels"), marks=mark.l[[i]])
  # plot(X_6m_Im[[i]],paste("Fixations, Image",i,", selon les sujets de 6 mois"))
  # points(X_6m_Im[[i]],col=mark.l[[i]])
}

# Pour toutes les images
# Sous groupe bébés 9 mois
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

par(mfrow=c(2,3))
X_9m_Im <- list()
for (i in 1:6) {
  X_9m_Im[[i]] <- ppp(x.l[[i]],y.l[[i]], c(0,1024), c(0,768), unitname=c("pixels"), marks=mark.l[[i]])
  # plot(X_9m_Im[[i]],paste("Fixations, Image",i,", selon les sujets de 9 mois"))
  # points(X_9m_Im[[i]],col=mark.l[[i]])
}

# Pour toutes les images
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

par(mfrow=c(2,3))
X_12m_Im <- list()
for (i in 1:6) {
  X_12m_Im[[i]] <- ppp(x.l[[i]],y.l[[i]], c(0,1024), c(0,768), unitname=c("pixels"), marks=mark.l[[i]])
  # plot(X_12m_Im[[i]],paste("Fixations, Image",i,", selon les sujets de 12 mois"))
  # points(X_12m_Im[[i]],col=mark.l[[i]])
}

# Pour toutes les images
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

par(mfrow=c(2,3))
X_adult_Im <- list()
for (i in 1:6) {
  X_adult_Im[[i]] <- ppp(x.l[[i]],y.l[[i]], c(0,1024), c(0,768), unitname=c("pixels"), marks=mark.l[[i]])
  # plot(X_adult_Im[[i]],main=paste("Image",i,"adultes"))
  # points(X_adultIm[[i]],col=mark.l[[i]])
}

par(mfrow=c(1,1))
for (i in 4:4) {
  m <- as.raster(r[[i]], max = 255)
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main="", axes=FALSE)
  rasterImage(m, 0, 0, 1024, 768)
  X_adult_Im[[i]] <- ppp(x.l[[i]],y.l[[i]], c(0,1024), c(0,768), unitname=c("pixels"), marks=mark.l[[i]])
  points(X_adult_Im[[i]],cex=.3,pch=19,col=mark.l[[i]])
}


### Faire decoupage de zones précises pour image (intéractivité)

