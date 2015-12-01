library(R.matlab)
library("pixmap")
library("bmp")
library("spatstat")
library("readbitmap")

# WorkSpace nécessaire pour charger les images


# Répertoire Images
setwd("~/Desktop/MASSS/Stage/Donnees/Images") # Mac Perso
setwd("/kora/home/stage/asetif/Bureau/Images") # PC stagiaire

# Application données BabyLab
load_data <- function() {
  root="~/Desktop/MASSS/Stage/Donnees/Images/"
  suffix="_bw.bmp"
  images<<-list()
  for(i in 1:6) {
    images[[length(images)+1]] <<-retrieve_image(paste(root,i,suffix,sep=""),flip=T)
  }
}

# fonction retrieve_image
retrieve_image<-function(image_file_name,flip,invert_colscale=F) {
  pixmap_matrix <- pixmapGrey(read.bmp(image_file_name))@grey
  if (invert_colscale==T) {
    pixmap_matrix <- 1-pixmap_matrix
  }
  nb_rows<-dim(pixmap_matrix)[1]
  nb_columns<-dim(pixmap_matrix)[2]
  if (flip==T ) {
    flip_pixmap_matrix<-matrix(nrow=nb_rows,ncol=nb_columns)
    for(i in 1:nb_rows) { flip_pixmap_matrix[i,]=pixmap_matrix[nb_rows+1-i,] }
    pixmap_matrix<-flip_pixmap_matrix
  }
  # Builds a pixel image in spatstat format
  pixel_image<-im(pixmap_matrix)
  return(pixel_image)
}

load_data()
# Image 1 et 2
par(mfrow=c(2,2))
# Image 1
r1=read.bmp("~/Desktop/MASSS/Stage/Donnees/Images/1_bw.bmp")
m1 <- as.raster(r1, max = 255)
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main="Image 1")
usr <- par("usr")
rasterImage(m1, usr[1], usr[3], usr[2], usr[4]) 
plot(images[[1]],main="Niveau de gris") # Carte niveau de gris
# Image 2
r2=read.bmp("/kora/home/stage/asetif/Bureau/Images/2_bw.bmp")
m2 <- as.raster(r2, max = 255)
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main="Image 2")
usr <- par("usr")
rasterImage(m2, usr[1], usr[3], usr[2], usr[4]) 
plot(images[[2]],main="Niveau de gris") # Carte niveau de gris

# Image 3 et 4
# Image 3
par(mfrow=c(2,2))
r3=read.bmp("/kora/home/stage/asetif/Bureau/Images/3_bw.bmp")
m3 <- as.raster(r3, max = 255)
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main="Image 3")
usr <- par("usr")
rasterImage(m3, usr[1], usr[3], usr[2], usr[4]) 
plot(images[[3]],main="Niveau de gris") # Carte niveau de gris
# Image 4
r4=read.bmp("/kora/home/stage/asetif/Bureau/Images/4_bw.bmp")
m4 <- as.raster(r4, max = 255)
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main="Image 4")
usr <- par("usr")
rasterImage(m4, usr[1], usr[3], usr[2], usr[4]) 
plot(images[[4]],main="Niveau de gris") # Carte niveau de gris

# Image 5 et 6
# Image 5
par(mfrow=c(2,2))
r5=read.bmp("/kora/home/stage/asetif/Bureau/Images/5_bw.bmp")
m5 <- as.raster(r5, max = 255)
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main="Image 5")
usr <- par("usr")
rasterImage(m5, usr[1], usr[3], usr[2], usr[4])
plot(images[[5]],main="Niveau de gris") # Carte niveau de gris
# Image 6
r6=read.bmp("/kora/home/stage/asetif/Bureau/Images/6_bw.bmp")
m6 <- as.raster(r6, max = 255)
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main="Image 6")
usr <- par("usr")
rasterImage(m6, usr[1], usr[3], usr[2], usr[4]) 
plot(images[[6]],main="Niveau de gris") # Carte niveau de gris


# Lecture données bébés 3 mois
# Bébé 1
Fix_baby1 <- read.delim("/users/stage/asetif/Bureau/ListFixations/ListFix_Age_3_Su_392.txt", header=FALSE)
colnames(Fix_baby1) <- c("Image","Temps","Coord_X","Coord_Y","V5","V6","V7","BonneFixation")
attach(Fix_baby1)
# Suppression 1ère ligne (fixation référence avant lancement des images)
Fix_baby1<-Fix_baby1[-1,]
Fix_baby1
# Suppression 3ème colonne (temps), 5ème, 6ème et 7ème colonne
# C'est à dire qu'on ne garde que les coordonnées et la qualité de la fixation
Fix_baby1<-Fix_baby1[,c(1,3,4,8)]
Fix_baby1
# 1ère Image
# Fix_baby_Im1<-Fix_baby1[Image==1,] Ne fonctionne pas correctement ?? Incompréhension totale
Fix_baby1_Im1<-Fix_baby1[1:11,]
Fix_baby1_Im1
# Récupération des coordonnées
x_baby1_Im1<-Fix_baby1_Im1[,2]
y_baby1_Im1<-768-Fix_baby1_Im1[,3] # A vérifier

# Construction du PPP :
# Image 1024 * 768 pixels
X_baby1_Im1 <- ppp(x_baby1_Im1, y_baby1_Im1, c(0,1024), c(0,768), unitname=c("pixels"))
str(X_baby1_Im1)
plot(X_baby1_Im1)
points(X_baby1_Im1)

# Afficher images + points
r1=read.bmp("~/Desktop/MASSS/Stage/Donnees/Images/1_bw.bmp")
m1 <- as.raster(r1, max = 255)
op <- par(bg = "cornsilk")
plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main="Image 1, Baby 1")
rasterImage(m1, 0, 0, 1024, 768) 
par(op)
points(X_baby1_Im1)

# Exemple pour adulte
# Lecture données Adulte
# Adulte 1
Fix_Adult1 <- read.delim("/users/stage/asetif/Bureau/ListFixations/ListFix_Age_99_Su_7777.txt", header=FALSE)
colnames(Fix_Adult1) <- c("Image","Temps","Coord_X","Coord_Y","V5","V6","V7","BonneFixation")
attach(Fix_Adult1)
# Suppression 1ère ligne (fixation référence avant lancement des images)
Fix_Adult1<-Fix_Adult1[-1,]
Fix_Adult1
# Suppression 3ème colonne (temps), 5ème, 6ème et 7ème colonne
# C'est à dire qu'on ne garde que les coordonnées et la qualité de la fixation
Fix_Adult1<-Fix_Adult1[,c(1,3,4,8)]
Fix_Adult1
# 1ère Image
# Fix_baby_Im1<-Fix_baby1[Image==1,] Ne fonctionne pas correctement ?? Incompréhension totale
Fix_Adult1_Im1<-Fix_Adult1[1:18,]
Fix_Adult1_Im1
# Récupération des coordonnées
x_Adult1_Im1<-Fix_Adult1_Im1[,2]
y_Adult1_Im1<-768-Fix_Adult1_Im1[,3] # A vérifier

# Construction du PPP :
# Image 1024 * 768 pixels
X_Adult1_Im1 <- ppp(x_Adult1_Im1, y_Adult1_Im1, c(0,1024), c(0,768), unitname=c("pixels"))
str(X_Adult1_Im1)
plot(X_Adult1_Im1)
points(X_Adult1_Im1)

# Afficher images + points
r1=read.bmp("/kora/home/stage/asetif/Bureau/Images/1_bw.bmp")
m1 <- as.raster(r1, max = 255)
op <- par(bg = "cornsilk")
plot(c(0,1024), c(0, 768), type = "n", xlab = "", ylab = "", main="Image 1, Baby 1")
rasterImage(m1, 0, 0, 1024, 768) 
par(op)
points(X_Adult1_Im1)