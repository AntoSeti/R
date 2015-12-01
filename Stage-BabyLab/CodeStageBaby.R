### LECTURE IMAGE .BMP ###
# Packages nécessaires
library(R.matlab)
library("pixmap")
library("bmp")
library("spatstat")

# Workspace nécessaire à la lecture images sous Mac (r1--r6)
load("~/Desktop/MASSS/Stage/Codes R Antoine/TestWorkSpace.RData")
r=list(r1,r2,r3,r4,r5,r6)
# Répertoire Images
setwd("~/Desktop/MASSS/Stage/Donnees/Images") # Mac Perso
setwd("/kora/home/stage/asetif/Bureau/Images") # PC stagiaire

####### PARTIE ANTOINE #######
#### Lecture données bébés 3 mois
# Bébé 1
Fix_Baby1 <- read.delim("~/Desktop/MASSS/Stage/Donnees/Fixations/ListFix_Age_3_Su_1.txt", header=FALSE)
colnames(Fix_Baby1) <- c("Image","Temps","Coord_X","Coord_Y","TempsFix","Ref1","Ref2","BonneFixation")
attach(Fix_Baby1)
# 1ère Image
Fix_Baby1_Im1<-Fix_Baby1[Image==1,] 
# Suppression 1ère ligne (fixation référence avant lancement des images)
Fix_Baby1_Im1<-Fix_Baby1_Im1[-1,]
Fix_Baby1_Im1
# Récupération des coordonnées (on garde toutes les fixations (sauf référence))
x_Baby1_Im1<-Fix_Baby1_Im1[,3]
y_Baby1_Im1<-768-Fix_Baby1_Im1[,4] # A vérifier

# Construction du PPP :
# Image 1024 * 768 pixels
X_Baby1_Im1 <- ppp(x_Baby1_Im1, y_Baby1_Im1, c(0,1024), c(0,768), unitname=c("pixels"))
str(X_Baby1_Im1)
plot(X_Baby1_Im1)

# Afficher images + points de fixation (Image 1, baby 1)
op <- par(bg = "cornsilk")
plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main="Image 1, Baby 1")
rasterImage(m1, 0, 0, 1024, 768) 
par(op)
points(X_Baby1_Im1,pch=3,col="blue")

# Exemple pour adulte
# Lecture données Adulte
# Adulte 1
Fix_Adult1 <- read.delim("~/Desktop/MASSS/Stage/Donnees/Fixations/ListFix_Age_99_Su_1.txt", header=FALSE)
colnames(Fix_Adult1) <- c("Image","Temps","Coord_X","Coord_Y","TempsFix","Ref1","Ref2","BonneFixation")
attach(Fix_Adult1)
# 1ère Image
Fix_Adult1_Im1<-Fix_Adult1[Image==1,] 
# Suppression 1ère ligne (fixation référence avant lancement des images)
Fix_Adult1_Im1<-Fix_Adult1_Im1[-1,]
Fix_Adult1_Im1
# Récupération des coordonnées (on garde toutes les fixations (sauf référence))
x_Adult1_Im1<-Fix_Adult1_Im1[,3]
y_Adult1_Im1<-768-Fix_Adult1_Im1[,4] # A vérifier

# Construction du PPP :
# Image 1024 * 768 pixels
X_Adult1_Im1 <- ppp(x_Adult1_Im1, y_Adult1_Im1, c(0,1024), c(0,768), unitname=c("pixels"))
str(X_Adult1_Im1)
plot(X_Adult1_Im1)

# Afficher images + points
op <- par(bg = "cornsilk")
plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main="Image 1, Adult 1")
rasterImage(m1, 0, 0, 1024, 768) 
par(op)
points(X_Adult1_Im1,pch=3,col="blue")


######## AUTOMATISER #########
# Pour un individu (toutes les images), premier sujet de 3 mois
Fix_Baby1 <- read.delim("~/Desktop/MASSS/Stage/Donnees/Fixations/ListFix_Age_3_Su_1.txt", header=FALSE)
colnames(Fix_Baby1) <- c("Image","Temps","Coord_X","Coord_Y","TempsFix","Ref1","Ref2","BonneFixation")
attach(Fix_Baby1)

# Pour toutes les images, nettoyage/suppression fixations non pertinentes 
for(i in 1:6) {
    nam1 <- paste("Fix_Baby1_Im", i, sep = "")
    assign(nam1, Fix_Baby1[Image==i & BonneFixation==1,])
}

Fix_Baby1_Im<-list(Im1=Fix_Baby1_Im1,Im2=Fix_Baby1_Im2,Im3=Fix_Baby1_Im3,Im4=Fix_Baby1_Im4,Im5=Fix_Baby1_Im5,Im6=Fix_Baby1_Im6)
Fix_Baby1_Im

# Récupération des coordonnées des fixations
for(i in 1:6) {
    # En x
    nam2 <- paste("x_Baby1_Im", i, sep = "")
    assign(nam2, Fix_Baby1_Im[[i]][3])
    # En y
    nam3 <- paste("y_Baby1_Im", i, sep = "")
    assign(nam3, 768 - Fix_Baby1_Im[[i]][4])
}

x_Baby1_Im<-list(Im1=x_Baby1_Im1,Im2=x_Baby1_Im2,Im3=x_Baby1_Im3,Im4=x_Baby1_Im4,Im5=x_Baby1_Im5,Im6=x_Baby1_Im6)
y_Baby1_Im<-list(Im1=y_Baby1_Im1,Im2=y_Baby1_Im2,Im3=y_Baby1_Im3,Im4=y_Baby1_Im4,Im5=y_Baby1_Im5,Im6=y_Baby1_Im6)
x_Baby1_Im
y_Baby1_Im

# Construction du PPP :
# Image 1024 * 768 pixels
# Convertir listes en vecteurs numériques
for(i in 1:6) {
  nam4 <- paste("X_Baby1_Im", i, sep = "")
  assign(nam4, ppp(as.numeric(unlist(x_Baby1_Im[[i]])), as.numeric(unlist(y_Baby1_Im[[i]])), c(0,1024), c(0,768), unitname=c("pixels")))
}

X_Baby1_Im<-list(Im1=X_Baby1_Im1,Im2=X_Baby1_Im2,Im3=X_Baby1_Im3,Im4=X_Baby1_Im4,Im5=X_Baby1_Im5,Im6=X_Baby1_Im6)
X_Baby1_Im

# Afficher points de fixations
par(mfrow=c(3,2))
for(i in 1:6) plot(X_Baby1_Im[[i]])

# Afficher points de fixation + images
r=list(r1,r2,r3,r4,r5,r6)

par(mfrow=c(3,2))
for (i in 1:6) {
m <- as.raster(r[[i]], max = 255)
op <- par(bg = "cornsilk")
plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Image",i, "Baby 1"))
rasterImage(m, 0, 0, 1024, 768) 
par(op)
points(X_Baby1_Im[[i]],pch=3,col="green")
}

### Automatiser pour tous les bébés, pour toutes les images ###
## Commençons par afficher les fixations de tous les bébés de 3 mois sur une image
# 43 bébés de 3 mois
root="~/Desktop/MASSS/Stage/Donnees/Fixations/ListFix_Age_3_Su_"
suffix=".txt"
baby3month<-list()
baby3month_Im1<-list()
for (i in 1:43) {
  baby3month[[i]] <- read.delim(paste(root,i,suffix,sep=""),col.names=c("Image","Temps","Coord_X","Coord_Y","TempsFix","Ref1","Ref2","BonneFixation"), header=FALSE)
  baby3month_Im1[[i]] <- baby3month[[i]][baby3month[[i]]$Image==1 & baby3month[[i]]$BonneFixation==1,]
}
baby3month_Im1

# On regroupe les coordonnées en x et y des fixations pour tous les bébés
coordx_baby3month_Im1<-vector()
coordy_baby3month_Im1<-vector()
for (i in 1:43) {
  coordx_baby3month_Im1<-c(coordx_baby3month_Im1,baby3month_Im1[[i]][3])
  coordy_baby3month_Im1<-c(coordy_baby3month_Im1,baby3month_Im1[[i]][4])
}
coordx_baby3month_Im1
coordy_baby3month_Im1

# Convertir listes en vecteurs numériques
(Fixations_coordx_baby3month_Im1<-as.numeric(unlist(coordx_baby3month_Im1)))
(Fixations_coordy_baby3month_Im1<-768-as.numeric(unlist(coordy_baby3month_Im1)))

## PPP pour 1ère image pour toues les bébés de 3 mois
X_baby3month_Im1<-ppp(Fixations_coordx_baby3month_Im1, Fixations_coordy_baby3month_Im1, c(0,1024), c(0,768), unitname=c("pixels"))

# Afficher points de fixations
plot(X_baby3month_Im1)

# Afficher points de fixation + images
for (i in 1:1) {
  m <- as.raster(r[[i]], max = 255)
  op <- par(bg = "cornsilk")
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Points de fixations, Image",i, "pour tous les bébés de 3 mois"))
  rasterImage(m, 0, 0, 1024, 768) 
  par(op)
  points(X_baby3month_Im1,pch=3,col="green")
}

######### Toutes les fixations pour les bébés de 3 mois ##########
## Faire pareil pour les autres images pour les bébés de 3 mois
root="~/Desktop/MASSS/Stage/Donnees/Fixations/ListFix_Age_3_Su_"
suffix=".txt"
baby3month<-list()
baby3month_Im<-list()
for (i in 1:43) {
      baby3month[[i]] <- read.delim(paste(root,i,suffix,sep=""),col.names=c("Image","Temps","Coord_X","Coord_Y","TempsFix","Ref1","Ref2","BonneFixation"), header=FALSE)
      baby3M <- new.env()
      baby3M$Im1 <- baby3month[[i]][baby3month[[i]]$Image==1 & baby3month[[i]]$BonneFixation==1 & baby3month[[i]]$Temps>0,]
      baby3M$Im2 <- baby3month[[i]][baby3month[[i]]$Image==2 & baby3month[[i]]$BonneFixation==1 & baby3month[[i]]$Temps>0,]
      baby3M$Im3 <- baby3month[[i]][baby3month[[i]]$Image==3 & baby3month[[i]]$BonneFixation==1 & baby3month[[i]]$Temps>0,]
      baby3M$Im4 <- baby3month[[i]][baby3month[[i]]$Image==4 & baby3month[[i]]$BonneFixation==1 & baby3month[[i]]$Temps>0,]
      baby3M$Im5 <- baby3month[[i]][baby3month[[i]]$Image==5 & baby3month[[i]]$BonneFixation==1 & baby3month[[i]]$Temps>0,]
      baby3M$Im6 <- baby3month[[i]][baby3month[[i]]$Image==6 & baby3month[[i]]$BonneFixation==1 & baby3month[[i]]$Temps>0,]
      baby3month_Im[[i]]<-as.list(baby3M)
}

# Attention, bonnes fixations avec temps négatifs (les supprimer ??)
# JE (!) décide de les supprimer.

# 1er bébé, toutes les images
baby3month_Im[[1]]
# 1er bébé, image 1
baby3month_Im[[1]]$Im1
baby3month_Im[[1]][1]

# On regroupe les coordonnées en x et y des fixations pour tous les bébés
coordx_baby3month_Im1<-vector()
coordy_baby3month_Im1<-vector()
coordx_baby3month_Im2<-vector()
coordy_baby3month_Im2<-vector()
coordx_baby3month_Im3<-vector()
coordy_baby3month_Im3<-vector()
coordx_baby3month_Im4<-vector()
coordy_baby3month_Im4<-vector()
coordx_baby3month_Im5<-vector()
coordy_baby3month_Im5<-vector()
coordx_baby3month_Im6<-vector()
coordy_baby3month_Im6<-vector()
for (i in 1:43) {
  coordx_baby3month_Im1<-c(coordx_baby3month_Im1,baby3month_Im[[i]]$Im1[3])
  coordy_baby3month_Im1<-c(coordy_baby3month_Im1,baby3month_Im[[i]]$Im1[4])
  coordx_baby3month_Im2<-c(coordx_baby3month_Im2,baby3month_Im[[i]]$Im2[3])
  coordy_baby3month_Im2<-c(coordy_baby3month_Im2,baby3month_Im[[i]]$Im2[4])
  coordx_baby3month_Im3<-c(coordx_baby3month_Im3,baby3month_Im[[i]]$Im3[3])
  coordy_baby3month_Im3<-c(coordy_baby3month_Im3,baby3month_Im[[i]]$Im3[4])
  coordx_baby3month_Im4<-c(coordx_baby3month_Im4,baby3month_Im[[i]]$Im4[3])
  coordy_baby3month_Im4<-c(coordy_baby3month_Im4,baby3month_Im[[i]]$Im4[4])
  coordx_baby3month_Im5<-c(coordx_baby3month_Im5,baby3month_Im[[i]]$Im5[3])
  coordy_baby3month_Im5<-c(coordy_baby3month_Im5,baby3month_Im[[i]]$Im5[4])
  coordx_baby3month_Im6<-c(coordx_baby3month_Im6,baby3month_Im[[i]]$Im6[3])
  coordy_baby3month_Im6<-c(coordy_baby3month_Im6,baby3month_Im[[i]]$Im6[4])
}

# Convertir listes en vecteurs numériques
Fixations_coordx_baby3month_Im1<-as.numeric(unlist(coordx_baby3month_Im1))
Fixations_coordy_baby3month_Im1<-768-as.numeric(unlist(coordy_baby3month_Im1))
Fixations_coordx_baby3month_Im2<-as.numeric(unlist(coordx_baby3month_Im2))
Fixations_coordy_baby3month_Im2<-768-as.numeric(unlist(coordy_baby3month_Im2))
Fixations_coordx_baby3month_Im3<-as.numeric(unlist(coordx_baby3month_Im3))
Fixations_coordy_baby3month_Im3<-768-as.numeric(unlist(coordy_baby3month_Im3))
Fixations_coordx_baby3month_Im4<-as.numeric(unlist(coordx_baby3month_Im4))
Fixations_coordy_baby3month_Im4<-768-as.numeric(unlist(coordy_baby3month_Im4))
Fixations_coordx_baby3month_Im5<-as.numeric(unlist(coordx_baby3month_Im5))
Fixations_coordy_baby3month_Im5<-768-as.numeric(unlist(coordy_baby3month_Im5))
Fixations_coordx_baby3month_Im6<-as.numeric(unlist(coordx_baby3month_Im6))
Fixations_coordy_baby3month_Im6<-768-as.numeric(unlist(coordy_baby3month_Im6))

Fixations_coordx_baby3month_Im<-list(Fixations_coordx_baby3month_Im1,Fixations_coordx_baby3month_Im2,Fixations_coordx_baby3month_Im3,Fixations_coordx_baby3month_Im4,Fixations_coordx_baby3month_Im5,Fixations_coordx_baby3month_Im6)
Fixations_coordy_baby3month_Im<-list(Fixations_coordy_baby3month_Im1,Fixations_coordy_baby3month_Im2,Fixations_coordy_baby3month_Im3,Fixations_coordy_baby3month_Im4,Fixations_coordy_baby3month_Im5,Fixations_coordy_baby3month_Im6)

## PPP pour les 6 images pour tous les bébés de 3 mois
X_baby3month_Im<<-list()
for (i in 1:6) {
  X_baby3month_Im[[i]]<-ppp(Fixations_coordx_baby3month_Im[[i]], Fixations_coordy_baby3month_Im[[i]], c(0,1024), c(0,768), unitname=c("pixels"))
}

# Afficher points de fixations
par(mfrow=c(2,3))
for (i in 1:6) {
plot(X_baby3month_Im[[i]],main=paste("Fixations, Image",i,", bébés de 3 mois"))
}

# Afficher points de fixation + images

par(mfrow=c(2,3))
for (i in 1:6) {
  m <- as.raster(r[[i]], max = 255)
  op <- par(bg = "cornsilk")
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", bébés de 3 mois"))
  rasterImage(m, 0, 0, 1024, 768) 
  par(op)
  points(X_baby3month_Im[[i]],pch=3,col=i)
}


######### Toutes les fixations pour les bébés de 6 mois ##########
####  47 sujets
root="~/Desktop/MASSS/Stage/Donnees/Fixations/ListFix_Age_6_Su_"
suffix=".txt"
baby6month<-list()
baby6month_Im<-list()
for (i in 1:47) {
  baby6month[[i]] <- read.delim(paste(root,i,suffix,sep=""),col.names=c("Image","Temps","Coord_X","Coord_Y","TempsFix","Ref1","Ref2","BonneFixation"), header=FALSE)
  baby6M <- new.env()
  baby6M$Im1 <- baby6month[[i]][baby6month[[i]]$Image==1 & baby6month[[i]]$BonneFixation==1 & baby6month[[i]]$Temps>0,]
  baby6M$Im2 <- baby6month[[i]][baby6month[[i]]$Image==2 & baby6month[[i]]$BonneFixation==1 & baby6month[[i]]$Temps>0,]
  baby6M$Im3 <- baby6month[[i]][baby6month[[i]]$Image==3 & baby6month[[i]]$BonneFixation==1 & baby6month[[i]]$Temps>0,]
  baby6M$Im4 <- baby6month[[i]][baby6month[[i]]$Image==4 & baby6month[[i]]$BonneFixation==1 & baby6month[[i]]$Temps>0,]
  baby6M$Im5 <- baby6month[[i]][baby6month[[i]]$Image==5 & baby6month[[i]]$BonneFixation==1 & baby6month[[i]]$Temps>0,]
  baby6M$Im6 <- baby6month[[i]][baby6month[[i]]$Image==6 & baby6month[[i]]$BonneFixation==1 & baby6month[[i]]$Temps>0,]
  baby6month_Im[[i]]<-as.list(baby6M)
}

# On regroupe les coordonnées en x et y des fixations pour tous les bébés
coordx_baby6month_Im1<-vector()
coordy_baby6month_Im1<-vector()
coordx_baby6month_Im2<-vector()
coordy_baby6month_Im2<-vector()
coordx_baby6month_Im3<-vector()
coordy_baby6month_Im3<-vector()
coordx_baby6month_Im4<-vector()
coordy_baby6month_Im4<-vector()
coordx_baby6month_Im5<-vector()
coordy_baby6month_Im5<-vector()
coordx_baby6month_Im6<-vector()
coordy_baby6month_Im6<-vector()
for (i in 1:47) {
  coordx_baby6month_Im1<-c(coordx_baby6month_Im1,baby6month_Im[[i]]$Im1[3])
  coordy_baby6month_Im1<-c(coordy_baby6month_Im1,baby6month_Im[[i]]$Im1[4])
  coordx_baby6month_Im2<-c(coordx_baby6month_Im2,baby6month_Im[[i]]$Im2[3])
  coordy_baby6month_Im2<-c(coordy_baby6month_Im2,baby6month_Im[[i]]$Im2[4])
  coordx_baby6month_Im3<-c(coordx_baby6month_Im3,baby6month_Im[[i]]$Im3[3])
  coordy_baby6month_Im3<-c(coordy_baby6month_Im3,baby6month_Im[[i]]$Im3[4])
  coordx_baby6month_Im4<-c(coordx_baby6month_Im4,baby6month_Im[[i]]$Im4[3])
  coordy_baby6month_Im4<-c(coordy_baby6month_Im4,baby6month_Im[[i]]$Im4[4])
  coordx_baby6month_Im5<-c(coordx_baby6month_Im5,baby6month_Im[[i]]$Im5[3])
  coordy_baby6month_Im5<-c(coordy_baby6month_Im5,baby6month_Im[[i]]$Im5[4])
  coordx_baby6month_Im6<-c(coordx_baby6month_Im6,baby6month_Im[[i]]$Im6[3])
  coordy_baby6month_Im6<-c(coordy_baby6month_Im6,baby6month_Im[[i]]$Im6[4])
}

# Convertir listes en vecteurs numériques
Fixations_coordx_baby6month_Im1<-as.numeric(unlist(coordx_baby6month_Im1))
Fixations_coordy_baby6month_Im1<-768-as.numeric(unlist(coordy_baby6month_Im1))
Fixations_coordx_baby6month_Im2<-as.numeric(unlist(coordx_baby6month_Im2))
Fixations_coordy_baby6month_Im2<-768-as.numeric(unlist(coordy_baby6month_Im2))
Fixations_coordx_baby6month_Im3<-as.numeric(unlist(coordx_baby6month_Im3))
Fixations_coordy_baby6month_Im3<-768-as.numeric(unlist(coordy_baby6month_Im3))
Fixations_coordx_baby6month_Im4<-as.numeric(unlist(coordx_baby6month_Im4))
Fixations_coordy_baby6month_Im4<-768-as.numeric(unlist(coordy_baby6month_Im4))
Fixations_coordx_baby6month_Im5<-as.numeric(unlist(coordx_baby6month_Im5))
Fixations_coordy_baby6month_Im5<-768-as.numeric(unlist(coordy_baby6month_Im5))
Fixations_coordx_baby6month_Im6<-as.numeric(unlist(coordx_baby6month_Im6))
Fixations_coordy_baby6month_Im6<-768-as.numeric(unlist(coordy_baby6month_Im6))

Fixations_coordx_baby6month_Im<-list(Fixations_coordx_baby6month_Im1,Fixations_coordx_baby6month_Im2,Fixations_coordx_baby6month_Im3,Fixations_coordx_baby6month_Im4,Fixations_coordx_baby6month_Im5,Fixations_coordx_baby6month_Im6)
Fixations_coordy_baby6month_Im<-list(Fixations_coordy_baby6month_Im1,Fixations_coordy_baby6month_Im2,Fixations_coordy_baby6month_Im3,Fixations_coordy_baby6month_Im4,Fixations_coordy_baby6month_Im5,Fixations_coordy_baby6month_Im6)

## PPP pour les 6 images pour tous les bébés de 6 mois
X_baby6month_Im<<-list()
for (i in 1:6) {
  X_baby6month_Im[[i]]<-ppp(Fixations_coordx_baby6month_Im[[i]], Fixations_coordy_baby6month_Im[[i]], c(0,1024), c(0,768), unitname=c("pixels"))
}

# Afficher points de fixations
par(mfrow=c(2,3))
for (i in 1:6) {
  plot(X_baby6month_Im[[i]],main=paste("Fixations, Image",i,", bébés de 6 mois"))
}

# Afficher points de fixation + images

par(mfrow=c(2,3))
for (i in 1:6) {
  m <- as.raster(r[[i]], max = 255)
  op <- par(bg = "cornsilk")
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", bébés de 6 mois"))
  rasterImage(m, 0, 0, 1024, 768) 
  par(op)
  points(X_baby6month_Im[[i]],pch=3,col=i)
}

######### Toutes les fixations pour les bébés de 9 mois ##########
####  44 sujets
root="~/Desktop/MASSS/Stage/Donnees/Fixations/ListFix_Age_9_Su_"
suffix=".txt"
baby9month<-list()
baby9month_Im<-list()
for (i in 1:44) {
  baby9month[[i]] <- read.delim(paste(root,i,suffix,sep=""),col.names=c("Image","Temps","Coord_X","Coord_Y","TempsFix","Ref1","Ref2","BonneFixation"), header=FALSE)
  baby9M <- new.env()
  baby9M$Im1 <- baby9month[[i]][baby9month[[i]]$Image==1 & baby9month[[i]]$BonneFixation==1 & baby9month[[i]]$Temps>0,]
  baby9M$Im2 <- baby9month[[i]][baby9month[[i]]$Image==2 & baby9month[[i]]$BonneFixation==1 & baby9month[[i]]$Temps>0,]
  baby9M$Im3 <- baby9month[[i]][baby9month[[i]]$Image==3 & baby9month[[i]]$BonneFixation==1 & baby9month[[i]]$Temps>0,]
  baby9M$Im4 <- baby9month[[i]][baby9month[[i]]$Image==4 & baby9month[[i]]$BonneFixation==1 & baby9month[[i]]$Temps>0,]
  baby9M$Im5 <- baby9month[[i]][baby9month[[i]]$Image==5 & baby9month[[i]]$BonneFixation==1 & baby9month[[i]]$Temps>0,]
  baby9M$Im6 <- baby9month[[i]][baby9month[[i]]$Image==6 & baby9month[[i]]$BonneFixation==1 & baby9month[[i]]$Temps>0,]
  baby9month_Im[[i]]<-as.list(baby9M)
}

# On regroupe les coordonnées en x et y des fixations pour tous les bébés
coordx_baby9month_Im1<-vector()
coordy_baby9month_Im1<-vector()
coordx_baby9month_Im2<-vector()
coordy_baby9month_Im2<-vector()
coordx_baby9month_Im3<-vector()
coordy_baby9month_Im3<-vector()
coordx_baby9month_Im4<-vector()
coordy_baby9month_Im4<-vector()
coordx_baby9month_Im5<-vector()
coordy_baby9month_Im5<-vector()
coordx_baby9month_Im6<-vector()
coordy_baby9month_Im6<-vector()
for (i in 1:44) {
  coordx_baby9month_Im1<-c(coordx_baby9month_Im1,baby9month_Im[[i]]$Im1[3])
  coordy_baby9month_Im1<-c(coordy_baby9month_Im1,baby9month_Im[[i]]$Im1[4])
  coordx_baby9month_Im2<-c(coordx_baby9month_Im2,baby9month_Im[[i]]$Im2[3])
  coordy_baby9month_Im2<-c(coordy_baby9month_Im2,baby9month_Im[[i]]$Im2[4])
  coordx_baby9month_Im3<-c(coordx_baby9month_Im3,baby9month_Im[[i]]$Im3[3])
  coordy_baby9month_Im3<-c(coordy_baby9month_Im3,baby9month_Im[[i]]$Im3[4])
  coordx_baby9month_Im4<-c(coordx_baby9month_Im4,baby9month_Im[[i]]$Im4[3])
  coordy_baby9month_Im4<-c(coordy_baby9month_Im4,baby9month_Im[[i]]$Im4[4])
  coordx_baby9month_Im5<-c(coordx_baby9month_Im5,baby9month_Im[[i]]$Im5[3])
  coordy_baby9month_Im5<-c(coordy_baby9month_Im5,baby9month_Im[[i]]$Im5[4])
  coordx_baby9month_Im6<-c(coordx_baby9month_Im6,baby9month_Im[[i]]$Im6[3])
  coordy_baby9month_Im6<-c(coordy_baby9month_Im6,baby9month_Im[[i]]$Im6[4])
}

# Convertir listes en vecteurs numériques
Fixations_coordx_baby9month_Im1<-as.numeric(unlist(coordx_baby9month_Im1))
Fixations_coordy_baby9month_Im1<-768-as.numeric(unlist(coordy_baby9month_Im1))
Fixations_coordx_baby9month_Im2<-as.numeric(unlist(coordx_baby9month_Im2))
Fixations_coordy_baby9month_Im2<-768-as.numeric(unlist(coordy_baby9month_Im2))
Fixations_coordx_baby9month_Im3<-as.numeric(unlist(coordx_baby9month_Im3))
Fixations_coordy_baby9month_Im3<-768-as.numeric(unlist(coordy_baby9month_Im3))
Fixations_coordx_baby9month_Im4<-as.numeric(unlist(coordx_baby9month_Im4))
Fixations_coordy_baby9month_Im4<-768-as.numeric(unlist(coordy_baby9month_Im4))
Fixations_coordx_baby9month_Im5<-as.numeric(unlist(coordx_baby9month_Im5))
Fixations_coordy_baby9month_Im5<-768-as.numeric(unlist(coordy_baby9month_Im5))
Fixations_coordx_baby9month_Im6<-as.numeric(unlist(coordx_baby9month_Im6))
Fixations_coordy_baby9month_Im6<-768-as.numeric(unlist(coordy_baby9month_Im6))

Fixations_coordx_baby9month_Im<-list(Fixations_coordx_baby9month_Im1,Fixations_coordx_baby9month_Im2,Fixations_coordx_baby9month_Im3,Fixations_coordx_baby9month_Im4,Fixations_coordx_baby9month_Im5,Fixations_coordx_baby9month_Im6)
Fixations_coordy_baby9month_Im<-list(Fixations_coordy_baby9month_Im1,Fixations_coordy_baby9month_Im2,Fixations_coordy_baby9month_Im3,Fixations_coordy_baby9month_Im4,Fixations_coordy_baby9month_Im5,Fixations_coordy_baby9month_Im6)

## PPP pour les 6 images pour tous les bébés de 9 mois
X_baby9month_Im<<-list()
for (i in 1:6) {
  X_baby9month_Im[[i]]<-ppp(Fixations_coordx_baby9month_Im[[i]], Fixations_coordy_baby9month_Im[[i]], c(0,1024), c(0,768), unitname=c("pixels"))
}

# Afficher points de fixations
par(mfrow=c(2,3))
for (i in 1:6) {
  plot(X_baby9month_Im[[i]],main=paste("Fixations, Image",i,", bébés de 9 mois"))
}

# Afficher points de fixation + images

par(mfrow=c(2,3))
for (i in 1:6) {
  m <- as.raster(r[[i]], max = 255)
  op <- par(bg = "cornsilk")
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", bébés de 9 mois"))
  rasterImage(m, 0, 0, 1024, 768) 
  par(op)
  points(X_baby9month_Im[[i]],pch=3,col=i)
}

######### Toutes les fixations pour les bébés de 12 mois ##########
####  46 sujets
root="~/Desktop/MASSS/Stage/Donnees/Fixations/ListFix_Age_12_Su_"
suffix=".txt"
baby12month<-list()
baby12month_Im<-list()
for (i in 1:46) {
  baby12month[[i]] <- read.delim(paste(root,i,suffix,sep=""),col.names=c("Image","Temps","Coord_X","Coord_Y","TempsFix","Ref1","Ref2","BonneFixation"), header=FALSE)
  baby12M <- new.env()
  baby12M$Im1 <- baby12month[[i]][baby12month[[i]]$Image==1 & baby12month[[i]]$BonneFixation==1 & baby12month[[i]]$Temps>0,]
  baby12M$Im2 <- baby12month[[i]][baby12month[[i]]$Image==2 & baby12month[[i]]$BonneFixation==1 & baby12month[[i]]$Temps>0,]
  baby12M$Im3 <- baby12month[[i]][baby12month[[i]]$Image==3 & baby12month[[i]]$BonneFixation==1 & baby12month[[i]]$Temps>0,]
  baby12M$Im4 <- baby12month[[i]][baby12month[[i]]$Image==4 & baby12month[[i]]$BonneFixation==1 & baby12month[[i]]$Temps>0,]
  baby12M$Im5 <- baby12month[[i]][baby12month[[i]]$Image==5 & baby12month[[i]]$BonneFixation==1 & baby12month[[i]]$Temps>0,]
  baby12M$Im6 <- baby12month[[i]][baby12month[[i]]$Image==6 & baby12month[[i]]$BonneFixation==1 & baby12month[[i]]$Temps>0,]
  baby12month_Im[[i]]<-as.list(baby12M)
}

# On regroupe les coordonnées en x et y des fixations pour tous les bébés
coordx_baby12month_Im1<-vector()
coordy_baby12month_Im1<-vector()
coordx_baby12month_Im2<-vector()
coordy_baby12month_Im2<-vector()
coordx_baby12month_Im3<-vector()
coordy_baby12month_Im3<-vector()
coordx_baby12month_Im4<-vector()
coordy_baby12month_Im4<-vector()
coordx_baby12month_Im5<-vector()
coordy_baby12month_Im5<-vector()
coordx_baby12month_Im6<-vector()
coordy_baby12month_Im6<-vector()
for (i in 1:46) {
  coordx_baby12month_Im1<-c(coordx_baby12month_Im1,baby12month_Im[[i]]$Im1[3])
  coordy_baby12month_Im1<-c(coordy_baby12month_Im1,baby12month_Im[[i]]$Im1[4])
  coordx_baby12month_Im2<-c(coordx_baby12month_Im2,baby12month_Im[[i]]$Im2[3])
  coordy_baby12month_Im2<-c(coordy_baby12month_Im2,baby12month_Im[[i]]$Im2[4])
  coordx_baby12month_Im3<-c(coordx_baby12month_Im3,baby12month_Im[[i]]$Im3[3])
  coordy_baby12month_Im3<-c(coordy_baby12month_Im3,baby12month_Im[[i]]$Im3[4])
  coordx_baby12month_Im4<-c(coordx_baby12month_Im4,baby12month_Im[[i]]$Im4[3])
  coordy_baby12month_Im4<-c(coordy_baby12month_Im4,baby12month_Im[[i]]$Im4[4])
  coordx_baby12month_Im5<-c(coordx_baby12month_Im5,baby12month_Im[[i]]$Im5[3])
  coordy_baby12month_Im5<-c(coordy_baby12month_Im5,baby12month_Im[[i]]$Im5[4])
  coordx_baby12month_Im6<-c(coordx_baby12month_Im6,baby12month_Im[[i]]$Im6[3])
  coordy_baby12month_Im6<-c(coordy_baby12month_Im6,baby12month_Im[[i]]$Im6[4])
}

# Convertir listes en vecteurs numériques
Fixations_coordx_baby12month_Im1<-as.numeric(unlist(coordx_baby12month_Im1))
Fixations_coordy_baby12month_Im1<-768-as.numeric(unlist(coordy_baby12month_Im1))
Fixations_coordx_baby12month_Im2<-as.numeric(unlist(coordx_baby12month_Im2))
Fixations_coordy_baby12month_Im2<-768-as.numeric(unlist(coordy_baby12month_Im2))
Fixations_coordx_baby12month_Im3<-as.numeric(unlist(coordx_baby12month_Im3))
Fixations_coordy_baby12month_Im3<-768-as.numeric(unlist(coordy_baby12month_Im3))
Fixations_coordx_baby12month_Im4<-as.numeric(unlist(coordx_baby12month_Im4))
Fixations_coordy_baby12month_Im4<-768-as.numeric(unlist(coordy_baby12month_Im4))
Fixations_coordx_baby12month_Im5<-as.numeric(unlist(coordx_baby12month_Im5))
Fixations_coordy_baby12month_Im5<-768-as.numeric(unlist(coordy_baby12month_Im5))
Fixations_coordx_baby12month_Im6<-as.numeric(unlist(coordx_baby12month_Im6))
Fixations_coordy_baby12month_Im6<-768-as.numeric(unlist(coordy_baby12month_Im6))

Fixations_coordx_baby12month_Im<-list(Fixations_coordx_baby12month_Im1,Fixations_coordx_baby12month_Im2,Fixations_coordx_baby12month_Im3,Fixations_coordx_baby12month_Im4,Fixations_coordx_baby12month_Im5,Fixations_coordx_baby12month_Im6)
Fixations_coordy_baby12month_Im<-list(Fixations_coordy_baby12month_Im1,Fixations_coordy_baby12month_Im2,Fixations_coordy_baby12month_Im3,Fixations_coordy_baby12month_Im4,Fixations_coordy_baby12month_Im5,Fixations_coordy_baby12month_Im6)

## PPP pour les 6 images pour tous les bébés de 12 mois
X_baby12month_Im<<-list()
for (i in 1:6) {
  X_baby12month_Im[[i]]<-ppp(Fixations_coordx_baby12month_Im[[i]], Fixations_coordy_baby12month_Im[[i]], c(0,1024), c(0,768), unitname=c("pixels"))
}

# Afficher points de fixations
par(mfrow=c(2,3))
for (i in 1:6) {
  plot(X_baby12month_Im[[i]],main=paste("Fixations, Image",i,", bébés de 12 mois"))
}

# Afficher points de fixation + images

par(mfrow=c(2,3))
for (i in 1:6) {
  m <- as.raster(r[[i]], max = 255)
  op <- par(bg = "cornsilk")
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", bébés de 12 mois"))
  rasterImage(m, 0, 0, 1024, 768) 
  par(op)
  points(X_baby12month_Im[[i]],pch=3,col=i)
}

######### Toutes les fixations pour les Adultes ##########
####  44 sujets
root="~/Desktop/MASSS/Stage/Donnees/Fixations/ListFix_Age_99_Su_"
suffix=".txt"
adult<-list()
adult_Im<-list()
for (i in 1:44) {
  adult[[i]] <- read.delim(paste(root,i,suffix,sep=""),col.names=c("Image","Temps","Coord_X","Coord_Y","TempsFix","Ref1","Ref2","BonneFixation"), header=FALSE)
  adultA <- new.env()
  adultA$Im1 <- adult[[i]][adult[[i]]$Image==1 & adult[[i]]$BonneFixation==1 & adult[[i]]$Temps>0,]
  adultA$Im2 <- adult[[i]][adult[[i]]$Image==2 & adult[[i]]$BonneFixation==1 & adult[[i]]$Temps>0,]
  adultA$Im3 <- adult[[i]][adult[[i]]$Image==3 & adult[[i]]$BonneFixation==1 & adult[[i]]$Temps>0,]
  adultA$Im4 <- adult[[i]][adult[[i]]$Image==4 & adult[[i]]$BonneFixation==1 & adult[[i]]$Temps>0,]
  adultA$Im5 <- adult[[i]][adult[[i]]$Image==5 & adult[[i]]$BonneFixation==1 & adult[[i]]$Temps>0,]
  adultA$Im6 <- adult[[i]][adult[[i]]$Image==6 & adult[[i]]$BonneFixation==1 & adult[[i]]$Temps>0,]
  adult_Im[[i]]<-as.list(adultA)
}

# On regroupe les coordonnées en x et y des fixations pour tous les bébés
coordx_adult_Im1<-vector()
coordy_adult_Im1<-vector()
coordx_adult_Im2<-vector()
coordy_adult_Im2<-vector()
coordx_adult_Im3<-vector()
coordy_adult_Im3<-vector()
coordx_adult_Im4<-vector()
coordy_adult_Im4<-vector()
coordx_adult_Im5<-vector()
coordy_adult_Im5<-vector()
coordx_adult_Im6<-vector()
coordy_adult_Im6<-vector()
for (i in 1:44) {
  coordx_adult_Im1<-c(coordx_adult_Im1,adult_Im[[i]]$Im1[3])
  coordy_adult_Im1<-c(coordy_adult_Im1,adult_Im[[i]]$Im1[4])
  coordx_adult_Im2<-c(coordx_adult_Im2,adult_Im[[i]]$Im2[3])
  coordy_adult_Im2<-c(coordy_adult_Im2,adult_Im[[i]]$Im2[4])
  coordx_adult_Im3<-c(coordx_adult_Im3,adult_Im[[i]]$Im3[3])
  coordy_adult_Im3<-c(coordy_adult_Im3,adult_Im[[i]]$Im3[4])
  coordx_adult_Im4<-c(coordx_adult_Im4,adult_Im[[i]]$Im4[3])
  coordy_adult_Im4<-c(coordy_adult_Im4,adult_Im[[i]]$Im4[4])
  coordx_adult_Im5<-c(coordx_adult_Im5,adult_Im[[i]]$Im5[3])
  coordy_adult_Im5<-c(coordy_adult_Im5,adult_Im[[i]]$Im5[4])
  coordx_adult_Im6<-c(coordx_adult_Im6,adult_Im[[i]]$Im6[3])
  coordy_adult_Im6<-c(coordy_adult_Im6,adult_Im[[i]]$Im6[4])
}

# Convertir listes en vecteurs numériques
Fixations_coordx_adult_Im1<-as.numeric(unlist(coordx_adult_Im1))
Fixations_coordy_adult_Im1<-768-as.numeric(unlist(coordy_adult_Im1))
Fixations_coordx_adult_Im2<-as.numeric(unlist(coordx_adult_Im2))
Fixations_coordy_adult_Im2<-768-as.numeric(unlist(coordy_adult_Im2))
Fixations_coordx_adult_Im3<-as.numeric(unlist(coordx_adult_Im3))
Fixations_coordy_adult_Im3<-768-as.numeric(unlist(coordy_adult_Im3))
Fixations_coordx_adult_Im4<-as.numeric(unlist(coordx_adult_Im4))
Fixations_coordy_adult_Im4<-768-as.numeric(unlist(coordy_adult_Im4))
Fixations_coordx_adult_Im5<-as.numeric(unlist(coordx_adult_Im5))
Fixations_coordy_adult_Im5<-768-as.numeric(unlist(coordy_adult_Im5))
Fixations_coordx_adult_Im6<-as.numeric(unlist(coordx_adult_Im6))
Fixations_coordy_adult_Im6<-768-as.numeric(unlist(coordy_adult_Im6))

Fixations_coordx_adult_Im<-list(Fixations_coordx_adult_Im1,Fixations_coordx_adult_Im2,Fixations_coordx_adult_Im3,Fixations_coordx_adult_Im4,Fixations_coordx_adult_Im5,Fixations_coordx_adult_Im6)
Fixations_coordy_adult_Im<-list(Fixations_coordy_adult_Im1,Fixations_coordy_adult_Im2,Fixations_coordy_adult_Im3,Fixations_coordy_adult_Im4,Fixations_coordy_adult_Im5,Fixations_coordy_adult_Im6)

## PPP pour les 6 images pour tous les Adultes
X_adult_Im<<-list()
for (i in 1:6) {
  X_adult_Im[[i]]<-ppp(Fixations_coordx_adult_Im[[i]], Fixations_coordy_adult_Im[[i]], c(0,1024), c(0,768), unitname=c("pixels"))
}

# Afficher points de fixations
par(mfrow=c(2,3))
for (i in 1:6) {
  plot(X_adult_Im[[i]],main=paste("Fixations, Image",i,", Adulte"))
}

# Afficher points de fixation + images

par(mfrow=c(2,3))
for (i in 1:6) {
  m <- as.raster(r[[i]], max = 255)
  op <- par(bg = "cornsilk")
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", Adultes"))
  rasterImage(m, 0, 0, 1024, 768) 
  par(op)
  points(X_adult_Im[[i]],pch=3,col=i)
}

## Comparaison visuelle des points de fixation entre les groupes
# Image 1
par(mfrow=c(3,2))
plot(X_baby3month_Im[[1]],main=paste("Fixations, Image 1, bébés de 3 mois"))
plot(X_baby6month_Im[[1]],main=paste("Fixations, Image 1, bébés de 6 mois"))
plot(X_baby9month_Im[[1]],main=paste("Fixations, Image 1, bébés de 9 mois"))
plot(X_baby12month_Im[[1]],main=paste("Fixations, Image 1, bébés de 12 mois"))
plot(X_adult_Im[[1]],main=paste("Fixations, Image 1, Adultes"))

## Avec image
par(mfrow=c(3,2))
m <- as.raster(r1, max = 255)
op <- par(bg = "cornsilk")
plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image 1, bébés de 3 mois"))
rasterImage(m, 0, 0, 1024, 768) 
points(X_baby3month_Im[[1]],pch=3,col="green")

plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image 1, bébés de 6 mois"))
rasterImage(m, 0, 0, 1024, 768) 
points(X_baby6month_Im[[1]],pch=3,col="green")

plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image 1, bébés de 9 mois"))
rasterImage(m, 0, 0, 1024, 768) 
points(X_baby9month_Im[[1]],pch=3,col="green")

plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image 1, bébés de 12 mois"))
rasterImage(m, 0, 0, 1024, 768) 
points(X_baby12month_Im[[1]],pch=3,col="green")

plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image 1, Adulte"))
rasterImage(m, 0, 0, 1024, 768) 
points(X_adult_Im[[1]],pch=3,col="green")

## Comparaison visuelle des points de fixation entre les groupes
# Pour toutes les images
for(i in 1:6) {
par(mfrow=c(3,2))
plot(X_baby3month_Im[[i]],main=paste("Fixations, Image",i,", bébés de 3 mois"))
plot(X_baby6month_Im[[i]],main=paste("Fixations, Image",i,", bébés de 6 mois"))
plot(X_baby9month_Im[[i]],main=paste("Fixations, Image",i,", bébés de 9 mois"))
plot(X_baby12month_Im[[i]],main=paste("Fixations, Image",i,", bébés de 12 mois"))
plot(X_adult_Im[[i]],main=paste("Fixations, Image",i,", Adultes"))
}

## Avec image
for(i in 1:6) {

par(mfrow=c(2,3))
m <- as.raster(r[[i]], max = 255)
op <- par(bg = "cornsilk")
plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", bébés de 3 mois"))
rasterImage(m, 0, 0, 1024, 768) 
par(op)
points(X_baby3month_Im[[i]],pch=3,col="green")

plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", bébés de 6 mois"))
rasterImage(m, 0, 0, 1024, 768) 
points(X_baby6month_Im[[i]],pch=3,col="green")

plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", bébés de 9 mois"))
rasterImage(m, 0, 0, 1024, 768) 
points(X_baby9month_Im[[i]],pch=3,col="green")

plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", bébés de 12 mois"))
rasterImage(m, 0, 0, 1024, 768) 
points(X_baby12month_Im[[i]],pch=3,col="green")

plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", Adultes"))
rasterImage(m, 0, 0, 1024, 768) 
points(X_adult_Im[[i]],pch=3,col="green")
}

## Comparaison visuelle des points de fixation entre les images pour un groupe donné
# Afficher points de fixation + images
 
par(mfrow=c(2,3))
for (i in 1:6) {
  m <- as.raster(r[[i]], max = 255)
  op <- par(bg = "cornsilk")
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", bébés de 3 mois"))
  rasterImage(m, 0, 0, 1024, 768) 
  par(op)
  points(X_baby3month_Im[[i]],pch=3,col=i)
}
 
par(mfrow=c(2,3))
for (i in 1:6) {
  m <- as.raster(r[[i]], max = 255)
  op <- par(bg = "cornsilk")
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", bébés de 6 mois"))
  rasterImage(m, 0, 0, 1024, 768) 
  par(op)
  points(X_baby6month_Im[[i]],pch=3,col=i)
}
 
par(mfrow=c(2,3))
for (i in 1:6) {
  m <- as.raster(r[[i]], max = 255)
  op <- par(bg = "cornsilk")
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", bébés de 9 mois"))
  rasterImage(m, 0, 0, 1024, 768) 
  par(op)
  points(X_baby9month_Im[[i]],pch=3,col=i)
}
 
par(mfrow=c(2,3))
for (i in 1:6) {
  m <- as.raster(r[[i]], max = 255)
  op <- par(bg = "cornsilk")
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", bébés de 12 mois"))
  rasterImage(m, 0, 0, 1024, 768) 
  par(op)
  points(X_baby12month_Im[[i]],pch=3,col=i)
}
 
par(mfrow=c(2,3))
for (i in 1:6) {
  m <- as.raster(r[[i]], max = 255)
  op <- par(bg = "cornsilk")
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", Adultes"))
  rasterImage(m, 0, 0, 1024, 768) 
  par(op)
  points(X_adult_Im[[i]],pch=3,col=i)
}

# Pour mettre toutes les sous-fenêtres (5x6=30) dans une unique fenêtre principale
 
par(mfrow=c(3,5))
for (i in 1:3) {
  m <- as.raster(r[[i]], max = 255)
  op <- par(bg = "cornsilk")
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", bébés de 3 mois"))
  rasterImage(m, 0, 0, 1024, 768) 
  points(X_baby3month_Im[[i]],pch=3,col="green")
  
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", bébés de 6 mois"))
  rasterImage(m, 0, 0, 1024, 768) 
  points(X_baby6month_Im[[i]],pch=3,col="green")
  
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", bébés de 9 mois"))
  rasterImage(m, 0, 0, 1024, 768) 
  points(X_baby9month_Im[[i]],pch=3,col="green")
  
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", bébés de 12 mois"))
  rasterImage(m, 0, 0, 1024, 768) 
  points(X_baby12month_Im[[i]],pch=3,col="green")
  
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", Adultes"))
  rasterImage(m, 0, 0, 1024, 768) 
  points(X_adult_Im[[i]],pch=3,col="green")
}
 
par(mfrow=c(3,5))
for (i in 4:6) {
  m <- as.raster(r[[i]], max = 255)
  op <- par(bg = "cornsilk")
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", bébés de 3 mois"))
  rasterImage(m, 0, 0, 1024, 768) 
  points(X_baby3month_Im[[i]],pch=3,col="green")
  
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", bébés de 6 mois"))
  rasterImage(m, 0, 0, 1024, 768) 
  points(X_baby6month_Im[[i]],pch=3,col="green")
  
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", bébés de 9 mois"))
  rasterImage(m, 0, 0, 1024, 768) 
  points(X_baby9month_Im[[i]],pch=3,col="green")
  
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", bébés de 12 mois"))
  rasterImage(m, 0, 0, 1024, 768) 
  points(X_baby12month_Im[[i]],pch=3,col="green")
  
  plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main=paste("Fixations, Image",i,", Adultes"))
  rasterImage(m, 0, 0, 1024, 768) 
  points(X_adult_Im[[i]],pch=3,col="green")
}
