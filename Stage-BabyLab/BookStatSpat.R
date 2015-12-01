#### BASICS ####
# Nouveautés :
x <- c(1,7,4,10,2,14,15,4,9,6,12.5,122,1675,165,0.1,1)
y <- cut(x, c(0,5,10,15,100,2000))

plot(stack.loss ~ Water.Temp, data=stackloss) # Quanti/Quanti
plot(weight ~ feed, data=chickwts) # Quanti/Quali

# 2 graphiques séparés
plot(stack.loss ~ Water.Temp + Air.Flow, data=stackloss)

# 1 unique graphique 3D
require(scatterplot3d)
par(mfrow=c(1,1))
with(stackloss,scatterplot3d(stack.loss ~ Water.Temp + Air.Flow))

# Modèle linéaire simple
lm(stack.loss ~ Water.Temp, data=stackloss)

# Modèle linéaire multiple
lm(stack.loss ~ Water.Temp + Air.Flow, data=stackloss)

# Modèle linéaire simple sans intercept (constante)
lm(stack.loss ~ Water.Temp - 1, data=stackloss)

lm(weight ~ feed, data=chickwts) 
lm(weight ~ feed - 1, data=chickwts) # Pas d'intercept mais présence modalité référentielle du modèle précédent

# Reading Data :
chordata <- read.table("chordatafile.txt", header=TRUE)
chordata <- read.csv("chordatafile.csv")

# ppp object
# fenêtre = rectangle
chordata <- read.table("chordatafile.txt", header=TRUE)
east <- chordata$Easting
north <- chordata$Northing
X <- ppp(east, north, c(174,178), c(29,33))
# ou plus élégamment
X <- with(chordata, ppp(Easting, Northing, c(174,178), c(29,33)))
# Si autre format (pas rectangle, voir page 58)

# Problèmes valeurs manquantes/aberrantes
x <- c(1.5, 2.1, 4.0, -999, 2.2, 3.8, 0.9, -999, 1.9)
stem(x)
x[x == -999] <- NA
x

# Fonction convertie en image pixels
f <- function(x,y){15*(cos(2*pi*sqrt((x-3)^2+(y-3)^2)))^2}
A <- as.im(f, W=square(6))
plot(A)

# Factor valued pixel image
vec <- rep(1:3, each = 400)
mat <- matrix(vec, nrow=40, ncol=30)
f <- factor(mat)
is.factor(f)
is.matrix(f)

factorim <- im(f,xcol=seq(0,1,length=30),yrow=seq(0,1,length=40))
factorim
plot(factorim)

dim(f) <- c(40,30)
factorim <- im(f)
factorim
plot(factorim)

# Fenêtre rectangulaire
owin(c(0,3), c(1,2))
# Fenêtre carrée
square(5)
square(c(1,3))

# Polygone window
Z <- owin(poly=list(x=c(0,10,0), y=c(0,0,10)))
plot(Z)

# Polygone avec trou
ZH <- owin(poly=list(list(x=c(0,8,0),y=c(0,0,8)),list(x=c(2,2,3,3), y=c(2,3,3,2))))
plot(ZH)

# Circular and elliptical windows
par(mfrow=c(2,2))
W <- disc(radius=2, centre=c(0,0))
plot(W)

# Plotting a point pattern
# 3 types de données : unmarked point pattern, multitype point pattern, point pattern with numeric marks
# Options graphiques : pch
(plot(amacrine))
(plot(longleaf))

# Marche pour markformat=vector
A <- colourmap(heat.colors(128), range=range(longleaf$marks))
plot(longleaf, pch=21, bg=A, cex=1)

# Pour Bei :
# Idée : plot(bei$x,bei$y,pch=21, bg=(heat.colors(128), range=range(bei.extra$grad$v)),cex=1)

juveniles <- subset(longleaf, marks <= 30)
a <- plot(longleaf)
plot(juveniles, symap=a)

# Symbol map ban be created
g1 <- symbolmap(inputs=letters[1:10], pch=11:20)
plot(g1)
g2 <- symbolmap(range=c(0,100), size=function(x) {x/50})
plot(g2)
g3 <- update(g2, col="red")
plot(g3)
g4 <- update(g3, col=function(x) ifelse(x < 30, "red", "black"))
plot(g4)

juveniles <- subset(longleaf, marks <= 30)
a <- plot(longleaf)
plot(juveniles, symap=g3)

# Mettre une légende :
g2 <- symbolmap(range=c(-1,1),shape=function(x) ifelse(x > 0, "circles", "squares"),
                size=function(x) sqrt(ifelse(x > 0, x/pi, -x)),
                bg = function(x) ifelse(abs(x) < 1, "red", "black"))
plot(g2, vertical=TRUE, side="left", col.axis="blue", cex.axis=2)

# Plotting a window
plot(murchison$greenstone, main = "")
plot(square(c(-1,1)), main = "")
plot(ellipse(1,.5), col = rgb(0,0,0,.2), add = TRUE)
plot(ellipse(.5,1), col = rgb(0,0,0,.2), add = TRUE)

# Plotting an image
g <- colourmap(rainbow(128), range=c(0,100))
plot(g)
h <- colourmap(c("green", "yellow", "red"), inputs=c("Low", "Medium", "High"))
plot(h)

# Image en Perspective
par(mfrow=c(2,2))
persp(bei.extra$elev, expand=6,theta=-30, phi=20,
      colmap=terrain.colors(128),shade=0.2,apron=TRUE, main="", box=FALSE)

M <- persp(bei.extra$elev,theta=-45, phi=18, expand=6,
           colmap=terrain.colors(128),border=NA, apron=TRUE, shade=0.3, box=FALSE, visible=TRUE)
perspPoints(bei, Z=bei.extra$elev, M=M, pch=16,cex=0.2)

M <- persp(bei.extra$grad,theta=-45, phi=18, expand=17,
           colmap=terrain.colors(128),border=NA, apron=TRUE, shade=0.3, box=FALSE, visible=TRUE)
perspPoints(bei, Z=bei.extra$grad, M=M, pch=16,cex=0.2)

# Transect : diagonial line (bottom left to top right)
with(bei.extra,plot(transect.im(elev)))
with(bei.extra,plot(transect.im(grad)))     

# Distribution des valeurs des pixels
with(bei.extra, hist(elev, freq=TRUE))
with(bei.extra, plot(spatialcdf(elev, normalise=TRUE)))

with(bei.extra, hist(grad, freq=TRUE))
with(bei.extra, plot(spatialcdf(grad, normalise=TRUE)))

# Interactive Plotting
plot(amacrine)
identify(amacrine)

# Plotting several objects
X <- layered(density(cells), cells)
layerplotargs(X)[[2]] <- list(pch=16)
plot(X, main="")

# Bei
# Layered Plots
X <- layered(density(bei), bei)
layerplotargs(X)[[2]] <- list(pch=16)
plot(X, main="",cex=0.35)

# "Listof" Objects
X <- swedishpines
QC <- quadratcount(X)
QCI <- as.im(X,dimyx=5)
DI <- density(X)
L <- listof(X, QC, QCI, DI)
names(L) <- c("Swedish pines pattern", "Quadrat count image",
              "Quadrat counts", "Estimated intensity")
plot(L)

# Plotting several images
pairs(density(split(lansing)[c(2,3,5)]))
# Corrélation entre les densités estimées
L <- density(split(lansing)[c(2,3,5)])
df <- pairs(L, plot=FALSE)
co <- cor(df)
round(co, 2)

# Distance du plus proche voisin
nndist(redwood)

# Type de fenêtres
W <- Window(clmfires)
U <- simplify.owin(W,10) # Moins de bord (~200), contour moins précis
par(mfrow=c(1,2))
plot(W)
plot(U)

# Extracting subsets of a point pattern
# Subset defined by an index
bei
bei[1:10]
bei[-(1:10)]
swedishpines[nndist(swedishpines) > 10]
longleaf[marks(longleaf) >= 42]

# Subset defined by a window
W <- owin(c(100,800), c(100,400))
W
bei
bei[W]

# Subset defined by an expression
subset(cells, x > 0.5 & y < 0.4)
subset(longleaf, marks >= 42)
subset(finpines, diameter > 2 & height < 4)
subset(finpines, diameter > 2, select=height)
subset(nbfires, year == 1999, select=cause:fnl.size)
subset(finpines, select = -height)

# Manipulating Marks
par(mfrow=c(2,1))
plot(anemones)
plot(unmark(anemones))
radii <- rexp(npoints(redwood), rate=10)
plot(redwood)
str(redwood)
str(redwood %mark% radii)
plot(redwood %mark% radii)

# Bei, elev
par(mfrow=c(2,2))
image(bei.extra$elev);points(bei,pch=3,cex=.4)
elev <- bei.extra$elev
Yelev <- bei %mark% elev[bei]
A <- colourmap(heat.colors(16), range=range(Yelev$marks))
plot(Yelev, pch=21, bg=A, cex=1)

# Bei, grad
image(bei.extra$grad);points(bei,pch=3,cex=.4)
grad <- bei.extra$grad
Ygrad <- bei %mark% grad[bei]
B <- colourmap(heat.colors(16), range=range(Ygrad$marks))
plot(Ygrad, pch=21, bg=B, cex=1)

# Colour :
pie(rep(1, 12), col = rainbow(12))

# Palette
demo.pal <-
  function(n, border = if (n < 32) "light gray" else NA,
           main = paste("color palettes;  n=", n),
           ch.col = c("rainbow(n, start=.7, end=.1)", "heat.colors(n)",
                      "terrain.colors(n)", "topo.colors(n)",
                      "cm.colors(n)"))
  {
    nt <- length(ch.col)
    i <- 1:n; j <- n / nt; d <- j/6; dy <- 2*d
    plot(i, i+d, type = "n", yaxt = "n", ylab = "", main = main)
    for (k in 1:nt) {
      rect(i-.5, (k-1)*j+ dy, i+.4, k*j,
           col = eval(parse(text = ch.col[k])), border = border)
      text(2*j,  k * j + dy/4, ch.col[k])
    }
  }
n <- if(.Device == "postscript") 64 else 16
# Since for screen, larger n may give color allocation problem
demo.pal(n)


X <- amacrine
plot(X)
str(amacrine)
marks(X) <- data.frame(type=marks(X), nn=nndist(amacrine))
str(marks(X))
plot(marks(X))

Y <- finpines
plot(Y)
str(Y)
vol <- with(marks(Y), (100 * pi/12) * height * diameter^2)
marks(Y) <- cbind(marks(Y), volume=vol)
str(marks(Y))
plot(marks(Y))

par(mfrow=c(2,2))
plot(longleaf)
Y <- cut(longleaf, breaks=c(0,5, 20, Inf))
plot(Y)
Y <- cut(longleaf, breaks=3)
plot(Y)

# Converting to another unit of length
lansing
rescale(lansing)

# metres -> kilometres
murchison
murch2 <- lapply(murchison, rescale, s=1000, unitname="km")
murch2 <- as.listof(murch2)

# Geometrical transformations
chorley
plot(chorley)
rotate(chorley, pi/2, centre="centroid")
plot(rotate(chorley, pi/2, centre="centroid"))

# Random perturbations of a point pattern
# rjitter : displaces each point of the pattern by a small random distance
par(mfrow=c(1,2))
X <- rsyst(owin(), 10, 10)
Y <- rjitter(X, 0.02)
plot(X)
plot(Y)

# rshift : applies the same random shift
par(mfrow=c(3,2))
plot(amacrine)
# random toroidal shift
# shift "on" and "off" points separately
X <- rshift(amacrine)
plot(X)
# shift "on" points and leave "off" points fixed
X <- rshift(amacrine, which="on")
plot(X)
# shift all points simultaneously
X <- rshift(amacrine, group=NULL)
plot(X)
# maximum displacement distance 0.1 units
X <- rshift(amacrine, radius=0.1)
plot(X)
# shift with erosion
X <- rshift(amacrine, radius=0.1, edge="erode")
plot(X)


# rlabel : randomly assigns new mark values to the points in a pattern
par(mfrow=c(2,2))
plot(amacrine)
# Randomly permute the marks "on" and "off"
# Result always has 142 "off" and 152 "on"
Y <- rlabel(amacrine)
plot(Y)
# randomly allocate marks "on" and "off"
# with probabilities p(off) = 0.48, p(on) = 0.52
Y <- rlabel(amacrine, permute=FALSE)
plot(Y)

par(mfrow=c(1,2))
# randomly allocate marks "A" and "B" with equal probability
data(cells)
plot(cells)
Y <- rlabel(cells, labels=factor(c("A", "B")), permute=FALSE)
plot(Y)

# quadratresample : performs a block resampling procedure
par(mfrow=c(1,2))
data(bei)
plot(bei)
X<-quadratresample(bei, 6, 3)
plot(X)

# rthin : randomly deletes some of the points 
par(mfrow=c(2,2))
data(redwood)
plot(redwood, main="thinning")
# delete 20% of points
Y <- rthin(redwood, 0.8)
points(Y, col="green", cex=1.4)
plot(Y)
# function
f <- function(x,y) { ifelse(x < 0.4, 1, 0.5) }
Y <- rthin(redwood, f)
plot(redwood, main="thinning")
points(Y, col="green", cex=1.4)
plot(Y)
# pixel image
Z <- as.im(f, redwood$window)
Y <- rthin(redwood, Z)
plot(redwood, main="thinning")
points(Y, col="green", cex=1.4)
plot(Y)

# Splitting a point pattern into sub-patterns
lansing
plot(lansing)
V <- split(lansing)
plot(V)
A <- lapply(V, adaptive.density)
plot(as.listof(A))

plot(amacrine)
plot(split(amacrine))
plot(as.listof(lapply(split(amacrine), adaptive.density)))

A <- by(lansing, FUN=adaptive.density)
plot(A)

# Un-splitting
# points with type Messor have been slightly displaced.
par(mfrow=c(2,1))
X <- ants
u <- split(X)
plot(u)
u$Messor <- rjitter(u$Messor,20)
split(X) <- u
plot(split(X))

# Combining point patterns
par(mfrow=c(2,2))
plot(ants)
plot(split(ants))
X <- superimpose(u$Messor, u$Cataglyphis)
plot(X)
X <- superimpose(Cataglyphis=u$Cataglyphis, Messor=u$Messor)
plot(X)

par(mfrow=c(2,2))
X <- runifpoint(50, square(c(0,2)))
Y <- runifpoint(50, square(c(1,3)))
plot(X)
plot(Y)
A=superimpose(X,Y)
plot(A)
B=superimpose(X, Y, W=square(3))
plot(B)

# Basic summaries of point patterns and windows
summary(chorley)
plot(chorley)
plot(split(chorley))
intensity(chorley) # nombre moyen de points par unité d'aire
D=density(chorley)
plot(D)
pairdist(chorley) # matrice contenant les distances entre toutes les paires
nndist(chorley) # Distance du plus proche voisin
nnwhich(chorley) # Renvoie l'indice (index) du voisin le plus proche

# Marks(X) : facteur
table(marks(chorley))
barplot(table(marks(chorley)))

# Si marks(X) numérique
hist(marks(X))
plot(ecdf(marks(X)))
plot(density(marks(X)))
smooth(X)
markmean(X) 
markvar(X)

# Voir page 108 pour toutes les commandes utiles

# Exemple dataset : nztrees (/bei)
nztrees # [0;153]x[0;95]
plot(nztrees)
contour(density(nztrees, 10), axes=FALSE)
plot(density(nztrees))
# contour(density(bei, 25), axes=FALSE)
# plot(bei, add=TRUE)

hist(coords(nztrees)$x)
# Si on veut enlever le coin en haute à droite (car intensité trop forte)
chopped <- owin(c(0,148),c(0,95))
# ou
win <- Window(nztrees)
chopped <- trim.rectangle(win, xmargin=c(0,5), ymargin=0)

nzchop <- nztrees[chopped]
summary(nzchop)
plot(density(nzchop,10))
plot(nzchop, add=TRUE)
contour(density(nzchop, 10), axes=FALSE)
plot(nzchop, add=TRUE)

# Exploring images
elev[list(x=142,y=356)] # value of the pixel images
# Par intéraction
plot(elev)
elev[locator(1)]

# Choisir une sous-région
S <- owin(c(200,300), c(100,200))
plot(elev[S])
# Par intéraction
plot(elev)
S <- clickpoly(add=TRUE)
plot(elev[S, drop=FALSE, tight=TRUE])

# Calcul pixels
Y <- eval.im(Z + 10)
C <- eval.im(A + B)
eval.im(sqrt(Z))
eval.im(sin(pi * Z))
eval.im(Z > 3)
eval.im(log(X) + Y - 3)
eval.im(if(Z < 3) 3 else 1)
eval.im(ifelse(X < Y, 3, 1)) ## Throws an error.
eval.im(ifelse(X < Y, 3, 1)) ## Works
ecdf(Z)
# ...

# Manipulating images
elev <- bei.extra$elev
W <- levelset(elev, 145, ">")
plot(W)
points(bei,cex=.1)

grad <- bei.extra$grad
V <- solutionset(elev <= 140 & grad > 0.1)
plot(V)
points(bei,cex=.1)

# Tessellations (division de fenêtres qui ne se chevauchent pas)
# Rectangulaire
T=tess(xgrid=c(0,.5,1), ygrid=c(0,.5,1))
plot(T)
# ou
T1=quadrats(bei$w, 5, 10)
plot(T1)
# Tile list (format mosaïque)
S <- owin(c(200,300), c(100,200))
S1 <- owin(c(200,211), c(80,525))
T2=tess(tiles=list(S,S1))
plot(T2)
# Tessellation pixel image
T3=tess(image=D)
plot(T3)

# Computed tessellattions
par(mfrow=c(2,2))
Z=dirichlet(bei)
plot(bei)
plot(Z)
Z=delaunay(bei)
plot(Z)

# Operations involving a tessellation
par(mfrow=c(2,2))
X <- runifpoint(10)
plot(X)
V <- dirichlet(X)
plot(V)
U <- tiles(V)
unlist(lapply(U, area)) # Aires pour chaque figure
# plot(U) Décomposition
cut(X,V)
plot(cut(X,V))
split(X,V)
plot(split(X,V))

# Application Bei, Dirichlet/Voronoi (Thiessen)
# Marche pas (trop grande décomposition)
plot(bei)
V<-dirichlet(bei)
plot(V)
U <- tiles(V)
unlist(lapply(U, area)) # Aires pour chaque figure
# plot(U) Décomposition
cut(bei,V)
plot(cut(bei,V))
split(bei,V)
plot(split(bei,V))

# Application Bei, Delaunay (Marche pas, trop grande décomposition)
plot(bei)
V<-delaunay(bei)
plot(V)
U <- tiles(V)
unlist(lapply(U, area)) # Aires pour chaque figure
# plot(U) Décomposition
cut(bei,V)
plot(cut(bei,V))
split(bei,V)
plot(split(bei,V))

# Superposition de 2 tessellations
T=tess(xgrid=c(0,.5,1), ygrid=c(0,.5,1))
plot(T)
X <- runifpoint(10)
plot(X)
V <- dirichlet(X)
plot(V)
I=intersect.tess(T,V)
plot(I)

# Réduire le "blanc" autour des graphiques d'un pattern point
plot(bei)
par(mar=rep(0.5, 4))
plot(bei)

# Package Rpanel
plot(amacrine)
identify(amacrine)
iplot(amacrine)

#### Exploration Data Analysis ####
# Estimating homogeneous intensity
X <- rescale(swedishpines)
W <- as.owin(X)
lam <- intensity(X)
sdX <- sqrt(lam/area.owin(W))
sdX
unitname(amacrine)
X <- rescale(amacrine, 1000/662)
unitname(X) <- "mm"
intensity(amacrine)
intensity(X)
sum(intensity(X))
intensity(unmark(X))

finpines
height <- marks(finpines)$height
diameter <- marks(finpines)$diameter
volume <- (pi/12) * height * (diameter/100)^2
# ou
volume <- with(marks(finpines),(pi/12) * height * (diameter/100)^2)
intensity(finpines)
unitname(finpines)
intensity(finpines, weights=volume)

# Quadrat counts
par(mfrow=c(1,2))
Q3 <- quadratcount(swedishpines, nx=3, ny=3)
Q3
plot(Q3)
points(swedishpines)
L3<-intensity(Q3,image=TRUE)
plot(L3)
H <- hextess(swedishpines, 10)
hQ<-quadratcount(swedishpines, tess=H)
plot(hQ)
L3<-intensity(hQ,image=TRUE)
plot(L3)

# Quadrat counting test of homogeneity
tS <- quadrat.test(swedishpines, 3,3)
tS
tS$p.value
plot(swedishpines)
plot(tS,add=TRUE)
quadrat.test(swedishpines, 5, alternative="regular",method="MonteCarlo")

# Smoothing estimation of intensity function
# Kernel Gaussien
par(mfrow=c(1,2))
den <- density(swedishpines, sigma=10)
plot(den)
# Autre méthodes
# Pour trouver le meilleur sigma, CV
b <- bw.ppl(swedishpines)
plot(b)
plot(b, xlim=c(30,60))
den1 <- density(swedishpines, sigma=b)
plot(den1)
D <- density(swedishpines, sigma=bw.diggle(swedishpines))
plot(D)

# Estimation of intensity at the data points
dX <- density(swedishpines, sigma=10, at="points")
plot(dX)
dX[1:5]

# Computation
den <- density(swedishpines, sigma=10)
denXpixel <- den[swedishpines]
denXpixel[1:5]
denXexact <- density(swedishpines, sigma=10, at="points",leaveoneout=FALSE)
denXexact[1:5]

# Weighted kernel estimators
vols <- with(marks(finpines),(pi/12) * height * (diameter/100)^2)
Dvol <- density(finpines, weights=vols, sigma=bw.ppl)
plot(density(finpines))
plot(Dvol)

# Spatially adaptive smoothing
par(mfrow=c(2,2))
plot(density(swedishpines))
aden <- adaptive.density(swedishpines, f=0.1, nrep=30)
nden <- nndensity(swedishpines, k=10)
plot(aden)
plot(nden)

# Projections, transformations, change of coordinates
grad <- bei.extra$grad
dens.map <- density(bei, W=grad)
dens.ter <- eval.im(dens.map * sqrt(1+grad^2))
persp(bei.extra$grad, colin=dens.ter,expand=6,theta=-30, phi=20,shade=0.2,apron=TRUE)
dens.ter2 <- density(bei, weights=sqrt(1+grad[bei]^2))
persp(bei.extra$grad, colin=dens.ter2)

# Investigating dependence of intensity on a covariate
# Spatial covariates
# Quadrats determined by a covariate
elev <- bei.extra$elev
b <- quantile(elev, probs=(0:4)/4)
Zcut <- cut(elev, breaks=b, labels=1:4)
V <- tess(image=Zcut)
V
plot(V)
qb <- quadratcount(bei, tess=V)
qb
plot(qb)
lam <- intensity(qb)
L3<-intensity(qb,image=TRUE)
plot(L3)

b5 <- seq(0, 5 * ceiling(max(elev)/5), by=5)
Zcut5 <- cut(elev, breaks=b5, include.lowest=TRUE)
Q5 <- quadratcount(bei, tess=tess(image=Zcut5))
plot(Q5)
lam5 <- intensity(Q5)
L3<-intensity(Q5,image=TRUE)
plot(L3)
barplot(lam5)

# Estimation of ρ
# Bei.Elev
rh <- rhohat(bei, elev)
plot(rh)
rhp<-predict(rh)
plot(rhp)
image(bei.extra)
rhf <- as.function(rh)
rhf(130) # Intensité prédictive à 130m

# Bei.Grad
rh <- rhohat(bei, grad)
plot(rh)
rhp<-predict(rh)
plot(rhp)
image(bei.extra)

# Bei combiné (extra+grad)
rh <- rhohat(bei,grad+elev)
plot(rh)
rhp<-predict(rh)
plot(rhp)
points(bei)

# Comparaison intensités estimées
#1
elev <- bei.extra$elev
b <- quantile(elev, probs=(0:4)/4)
Zcut <- cut(elev, breaks=b, labels=1:4)
V <- tess(image=Zcut)
V
plot(V)
qb <- quadratcount(bei, tess=V)
qb
plot(qb)
lam <- intensity(qb)
L4<-intensity(qb,image=TRUE)
plot(L4)

#2
b5 <- seq(0, 5 * ceiling(max(elev)/5), by=5)
Zcut5 <- cut(elev, breaks=b5, include.lowest=TRUE)
Q5 <- quadratcount(bei, tess=tess(image=Zcut5))
plot(Q5)
lam5 <- intensity(Q5)
L5<-intensity(Q5,image=TRUE)
plot(L5)

#3
grad=bei.extra$grad
elev=bei.extra$elev
rh <- rhohat(bei,elev)
plot(rh)
rhp<-predict(rh)
plot(rhp)
points(bei)

par(mfrow=c(2,2))
plot(L4)
plot(L5)
plot(rhp)
image(bei.extra$elev)
M <- persp(bei.extra$elev,theta=-45, phi=18, expand=6,
           colmap=terrain.colors(128),border=NA, apron=TRUE, shade=0.3, box=FALSE, visible=TRUE)
perspPoints(bei, Z=bei.extra$elev, M=M, pch=16,cex=.5)
Yelev <- bei %mark% elev[bei]
A <- colourmap(heat.colors(16), range=range(Yelev$marks))
plot(Yelev, pch=21, bg=A, cex=1)
plot(bei)

# Différences entre :
plot(density(bei))
plot(predict(rhohat(bei,elev+grad)))

# Comparer deux intensités estimées
# Elev avec bei
rh <- rhohat(bei, elev)
pred <- predict(rh)
kden <- density(bei, 50)
pairs(pred,kden)
plot(pred)
plot(kden)
P1<-eval.im(kden-pred)
plot(P1)

# Grad avec bei
rh <- rhohat(bei, grad)
pred <- predict(rh)
kden <- density(bei, 50)
pairs(pred,kden)
plot(pred)
plot(kden)
P2<-eval.im(kden-pred)
plot(P2)

# Avec 2 covariables
rh <- rhohat(bei,grad+elev)
plot(rh)
rhp<-predict(rh)
plot(rhp)

A<-with(bei.extra, rho2hat(bei, grad, elev))
plot(A)

# Rotation, Distance Map
X <- rotate(copper$SouthPoints, pi/2)
L <- rotate(copper$SouthLines, pi/2)
plot(X)
plot(L,add=TRUE)
Z<-distmap(L)
contour(Z)
plot(Z)

# Formal tests of (non-)dependence on a covariate
Z <- bei.extra$elev
b <- quantile(Z, probs=(0:4)/4)
Zcut <- cut(Z, breaks=b, labels=1:4)
V <- tess(image=Zcut)
quadrat.test(bei, tess=V)->q;plot(q,lwd=2,col='red');points(bei,cex=.1);q
quadrat.test(bei,nx=3)->q;plot(q,lwd=2,col='red');points(bei,cex=.1);q
Z <- bei.extra$grad
b <- quantile(Z, probs=(0:4)/4)
Zcut <- cut(Z, breaks=b, labels=1:4)
V <- tess(image=Zcut)
quadrat.test(bei, tess=V)->q;plot(q,lwd=2,col='red');points(bei,cex=.1);q
quadrat.test(bei,nx=3)->q;plot(q,lwd=2,col='red');points(bei,cex=.1);q

# More Powerful : Kolmogorov-Smirnov test of CSR
elev <- bei.extra$elev
cdf.test(bei, elev)
plot(cdf.test(bei, elev))
cdf.test(swedishpines, "x")
plot(cdf.test(swedishpines, "x"))

# Berman’s tests
elev <- bei.extra$elev
B <- berman.test(bei, elev)
B
plot(B)

# Hot spots, clusters, and local features
# Clusters
denRed <- density(redwood, bw.ppl, ns=16) # bw.ppl= Likelihood CV
# bw.ppl when the pattern consists predominantly of tight clusters
# bw.diggle : detect a single tight cluster in the midst of random noise
plot(redwood)
plot(denRed)
LR <- scanLRTS(redwood, r = 2 * bw.ppl(redwood))
plot(LR)
pvals <- eval.im(pchisq(LR, df=1, lower.tail=FALSE))
plot(pvals)
clusterset(redwood, what="domain")
plot(redwood)
plot(clusterset(redwood, what="domain",fast=TRUE))
plot(bei)
plot(clusterset(bei, what="domain",fast=TRUE))
# 10ème plus proche voisin
Z <- nnclean(redwood, k=5, plothist=TRUE)
plot(Z)

# Exemple forte concentration d'intensité
require(datasets)
require(mapdata)
require(maps)
qk <- ppp(quakes$long, quakes$lat, c(164, 190), c(-39,-10))
plot(qk)
dq.5 <- density(qk, 0.5)
plot(dq.5)
ht.5 <- hextess(as.owin(qk), 1.09)
hq.5 <- intensity(quadratcount(qk, tess=ht.5), image=TRUE)
plot(hq.5)
clusterset(qk, what="domain")
plot(clusterset(qk, what="domain"))
nnclean(qk, k=5, plothist=TRUE)
par(mfrow=c(1,1))
plot(nnclean(qk, k=5, plothist=TRUE))

par(mfrow=c(2,1))
plot(unmark(shapley))
# Attraction des points
Y <- sharpen(unmark(shapley), sigma=0.5, edgecorrect=TRUE)
plot(Y)

# Kernel smoothing of marks
plot(longleaf)
plot(density(longleaf))
plot(Smooth(longleaf,bw.smoothppp))

# Variance/Ecart-Type
mvar <- markvar(longleaf, bw.smoothppp)
msd <- eval.im(sqrt(mvar))
plot(msd) # plus fortes valeurs sur les bordures où arbres sont jeunes
mfit <- Smooth(longleaf, bw.smoothppp, at="points")
res <- marks(longleaf) - mfit
marks(longleaf)<-res
plot(longleaf)
plot(nnmark(longleaf), k=5, plothist=TRUE)

# Multitype intensity and relative risk
plot(lansing)
plot(split(lansing))
b <- bw.relrisk(lansing)
plot(b)
rr <- relrisk(lansing, sigma=b)
# Estimates of spatially-varying proportions of each species
plot(rr)

dominant <- im.apply(rr, which.max)
species <- levels(marks(lansing))
dominant <- eval.im(factor(dominant, levels=1:6,labels=species))
textureplot(dominant)

#### Corrélation ####
# 3 grands types : régularité (répulsion), indépendance et cluster
fryplot(anemones)
plot(frypoints(anemones))
# Peu de points au centre (certaine régularité des points)

# Fonction K de Ripley, transformation en L
# Using the K-function implicitly assumes that the point process has homogeneous intensity
K <- Kest(cells)
Ki <- Kest(cells, correction="isotropic")
Lc <- Lest(cells)
# Warning : absence de corrélation != indépendance
# Cell process !!
X <- rcell(nx=15)
plot(X)
plot(Kest(X))
# Swedishpines
Ks <- Kest(swedishpines)
plot(Ks, iso ~ r)
plot(Ks, cbind(iso, trans, theo) ~ r)

lambda <- intensity(swedishpines)
plot(Ks, lambda * . ~ r)
Ko <- subset(Ks, r < 0.1, select= -border)
plot(Ko)
Ks <- Kest(swedishpines)
K <- as.function(Ks)
K(9)

Kr <- Kest(redwood)
y <- with(Kr, iso - theo)
x <- with(Kr, r)
# Calcul
K1 <- Kest(redwood)
K2 <- Kest(cells)
DK <- eval.fv(K1-K2)
plot(DK)

# Estimating the pair correlation function
g <- pcf(cells)
fryplot(cells)
plot(g)

## Standard errors and confidence intervals
X <- copper$SouthPoints
Kvb <- varblock(X, Kest, nx=3, ny=3)
plot(Kvb)
# Loh’s bootstrap
Kloh <- lohboot(X, Kest)
plot(Kloh)
Lg <- lohboot(X, Lest, global=TRUE)
Kg <- eval.fv(pi * Lg^2)
plot(Lg)
plot(Kg)

# Testing statistical significance
# Pointwise envelopes
plot(Kest(runifpoint(npoints(cells), Window(cells))))
E <- envelope(cells, Kest, nsim=39, fix.n=TRUE)
plot(E)
# Global envelopes
Ek<-envelope(cells, Kest, nsim=19, rank=1, global=TRUE)
plot(Ek)
El<-envelope(cells, Lest, nsim=39, fix.n=TRUE)
plot(El)

# Detecting anisotropy
# A point process is ‘isotropic’ if all its statistical properties are unchanged when it is rotated
X <- rSSI(0.05, win=owin(c(0,1), c(0, 3)))
Y <- affine(X, mat=diag(c(1, 1/3)))
plot(frypoints(Y))
Khoriz <- Ksector(Y, begin = -15, end = 15, units="degrees")
Kvert <- Ksector(Y, begin = 90-15, end = 90+15, units="degrees")
plot(Khoriz, trans/theo ~ r, lty=2)
plot(Kvert, trans/theo ~ r, add=TRUE)
dK <- function(X, ...) {
  K1 <- Ksector(X, ..., begin = -15, end = 15, units="degrees")
  K2 <- Ksector(X, ..., begin = 90-15, end = 90+15, units="degrees")
  eval.fv(K1-K2)
}
CIdK <- varblock(Y, dK, nx=5)
plot(CIdK)

# Adjusting for inhomogeneity (K, g)
# K function inhomogène
numata <- residualspaper$Fig1
plot(numata)
lambda <- density(numata, bw.ppl)
numataK <- Kinhom(numata, lambda)
plot(numataK)
numataK <- Kinhom(numata, sigma=bw.ppl)
plot(numataK)
# pair correlation function (pcf) inhomogène
plot(pcf(bei))
g <- pcfinhom(bei)
plot(g)

### Shortest distances and empty spaces ###
M <- pairdist(redwood)
M
v <- nndist(redwood)
v
Z <- distmap(redwood)
plot(Z)
points(redwood)

# Tests of CSR based on shortest distances
# Complete Spatial Randomness (CSR)
clarkevans(redwood)
clarkevans.test(redwood, correction="donnelly",alternative="clustered")
clarkevans.test(cells, correction="donnelly",alternative="clustered")

hopskel(redwood)
hopskel.test(redwood, alternative="clustered")
hopskel.test(cells, alternative="clustered")

# Exploratory graphics
plot(redwoodfull)
plot(redwoodfull %mark% nndist(redwoodfull), markscale=1)
stienen(redwoodfull) # Stienen Diagram
plot(dirichlet(redwoodfull)) # Dirichlet tessellation

# Nearest-neighbour function G, empty-space function F
Fs <- Fest(swedishpines)
Gs <- Gest(swedishpines)
plot(Fs)
plot(Gs)
Swedish <- rescale(swedishpines)
plot(Fest(Swedish))
plot(Gest(Swedish))

# Formal inference and diagnostic plots
Fci <- varblock(Swedish, Fest, nx=5, correction="best")
Gci <- varblock(Swedish, Gest, nx=5, correction="best")
Fenv <- envelope(Swedish, Fest, nsim=39, fix.n=TRUE)
Genv <- envelope(Swedish, Gest, nsim=39, fix.n=TRUE)
plot(Fci);plot(Gci);plot(Fenv);plot(Genv)


# Empty space hazard
# Here is a need for alternative summary functions (derived from F and G) which contain only contributions from distances equal to r

plot(Fest(cells), cbind(hazard, theohaz)~ r)
hazenv <- envelope(Swedish, Fhazard, nsim=39, fix.n=T,transform=expression(./(2*pi*r)))
plot(hazenv)          

# J-function
# Values J(r) > 1 are consistent with a regular pattern, and J(r) < 1 is consistent with clustering, at scales less than or equal to r.
plot(allstats(cells))
plot(allstats(cells)$J)

# Inhomogeneous F, G and J functions
Finhom(cells)
Ginhom(cells)
plot(Jinhom(cells))

#### STATISTICAL INFERENCE ####
### POISSON ###
# The key property of a Poisson process is that the random points are independent of each other
# The ppm() function
# Bei data
# Simplest example
fit <- ppm(bei ~ 1)
fit
# Models with a single numerical covariate (p. 278)
bei.extra
fit <- ppm(bei ~ grad, data=bei.extra)
fit
plot(effectfun(fit,"grad", se.fit=TRUE))

# These results tell us that the estimated intensity of Beilschmiedia trees on a flat surface (slope s = 0) is about exp(−5.391) = 0.004559 trees per square metre, or 45.59 trees per hectare, and would increase by a factor of exp(5.022) = 151.7 if the slope increased to 1.0. The largest slope value in the data is about 0.3, at which stage the predicted intensity has risen by a factor of exp(0.3×5.022) = 4.511 from its value on a flat surface.
ppm(bei ~ atan(grad), data=bei.extra)
ppm(bei ~ I(atan(grad) * 180/pi),data=bei.extra)
degrees <- function(x) { x * 180/pi }
ppm(bei ~ degrees(atan(grad)), data=bei.extra)
# Quadratic function
fit<-ppm(bei ~ grad + I(grad^2), data=bei.extra)
plot(effectfun(fit,"grad", se.fit=TRUE))

# Murchison data
mur <- lapply(murchison, rescale, s=1000, unitname="km")
plot(mur$gold)
plot(mur$faults)
# dfault = Distance par rapport à "faults"
dfault <- with(mur,distfun(faults))
plot(dfault)
fit <- ppm(gold ~ dfault,data=mur)
fit

# Models with a logical covariate
# greenstone : polygonal boundary, spatial window
# intensité inside/outside greenstone
ppm(gold ~ greenstone, data=mur)
ppm(gold ~ greenstone-1,data=mur)
# Mêmes résultats/interprétations (p. 284)

# Models with a factor covariate
# Gorilla nest data
gor <- rescale(gorillas, 1000, unitname="km")
gor <- unmark(gor)
plot(gor)
gex <- lapply(gorillas.extra, rescale,s=1000, unitname="km")
plot(gex$vegetation)

names(gex)
shorten <- function(x) substr(x, 1, 4)
names(gex) <- shorten(names(gex))
names(gex)
isfactor <- !unlist(lapply(lapply(gex, levels), is.null))
for(i in which(isfactor))
levels(gex[[i]]) <- shorten(levels(gex[[i]]))
levels(gex$vege)

vt <- tess(image=gex$vege)
plot(vt)
I<-intensity(quadratcount(gor, tess=vt))
plot(I)

# Additive models (p. 289)
fitadd <- ppm(bei ~ elev + grad,data=bei.extra)
fitadd

# Modelling spatial trend using Cartesian coordinates (p. 290)
jpines <- residualspaper[["Fig1"]]
ppm(jpines ~ x + y)
ppm(jpines ~ polynom(x,y,2))
# Fit a model with constant but unequal intensities on each side of the vertical line x = 0.5
ppm(jpines ~ (x < 0.5))

# Models with interaction between covariates
# Interaction between two numerical covariates
fit <- ppm(bei ~ elev + grad + I(elev*grad), data=bei.extra)
fit
# Interaction between two factors
ppm(gor ~ vege * heat, data=gex)
# Interaction between factor and numerical covariate
ppm(gold ~ dfault * greenstone,data=mur)

# Nested interaction (intéractions emboitées)
ppm(gold ~ greenstone/dfault, data=mur)
ppm(gold ~ greenstone/dfault-1, data=mur)

# Formulas involving many variables
ppm(gor~ . , data=gex)

## Statistical inference for Poisson models
# Fitted models
beikm <- rescale(bei, 1000, unitname="km")
bei.extrakm <- lapply(bei.extra, rescale, s=1000, unitname="km")
fitkm <- ppm(beikm ~ x + y)
fitkm
coef(fitkm)
plot(fitkm, how="image", se=FALSE)
summary(fitkm)
coef(summary(fitkm))
# Matrice variance-covariance
vcov(fitkm)
# Erreurs standards sur la diagonale
sqrt(diag(vcov(fitkm)))
# Intervalle de confiance des coefficients
confint(fitkm, level=0.95)
# Matrice de corrélation entre coefficients (variables)
co <- vcov(fitkm, what="corr")
round(co, 2)
# Attention, corrélations changent selon l'emplacement de l'origine des covariables
# Ici, changement au centre de la région d'étude
fitch <- update(fitkm, . ~ I(x-0.5) + I(y-0.25))
co <- vcov(fitch, what="corr")
round(co, 2)

# Prediction
fit <- ppm(bei ~ polynom(grad, elev, 2), data=bei.extra)
lamhat <- predict(fit)
contour(lamhat)
plot(lamhat)

M <- persp(bei.extra$elev, colin=lamhat,
           colmap=topo.colors, shade=0.4,
           theta=-55, phi=25, expand=6,
           box=FALSE, apron=TRUE, visible=TRUE)

perspPoints(bei, Z=bei.extra$elev, M=M,
              pch=".", col="red", cex=1.25)

# Intervalle de confiance de la prédiction
contour(predict(fit, interval="confidence"))
plot(predict(fit, interval="confidence"))

# To find the expected number of trees at elevations below 130 metres
B <- levelset(bei.extra$elev, 130)
predict(fit, total=B)
predict(fit, total=B, type="se")
predict(fit, total=B, interval="confidence")

# Intervalle prédiction
predict(fit, total=B, interval="prediction")

# Updating a model
X <- rpoispp(42)
m <- ppm(X ~ 1)
fitcsr <- ppm(bei ~ 1, data=bei.extra)
update(fitcsr, bei ~ grad)
# ou
fitgrad <- update(fitcsr, .~ grad)

fitall <- update(fitgrad, . ~ . + elev)
fitall

# Model selection
fit1 <- ppm(bei ~ grad,data=bei.extra)
fitnull <- ppm(bei ~ 1)
anova(fitnull, fit1, test="Chi")
# < 0.05, on préfère le modèle avec le plus de paramètres

# Akaike Information Criterion : AIC for model selection
AIC(fit1)
AIC(fitnull)

# AIC automatisé
fit <- ppm(bei ~ elev+grad,data=bei.extra)
step(fit,trace=1)

# Simulating the fitted model
fit=ppm(bei ~ polynom(grad, elev, 2),data=bei.extra)
X <- simulate(fit, data=bei.extra)
plot(X[[1]])
     
# Quadrature schemes
# Gorilla nests example
ppm(gor ~ vege, data=gex)
vt <- tess(image=gex$vege)
plot(vt)
intensity(quadratcount(gor, tess=vt))

fitveg2 <- ppm(gor~ vege-1, data=gex, nd=256)
exp(coef(fitveg2))

## Hypothesis Tests and Simulation Envelopes (p. 331)
# Testing for a covariate effect in a parametric model (p. 333)
mur <- lapply(murchison, rescale, s=1000, unitname="km")
mur$dfault <- with(mur, distfun(faults))
mfit0 <- ppm(gold ~ greenstone, data=mur)
mfit1 <- ppm(gold ~ greenstone + dfault, data=mur)
# ou
mfit1 <- update(mfit0, . ~ . + dfault)

copper$dist <- with(copper, distfun(SouthLines))
cfit0 <- ppm(SouthPoints ~ 1, data=copper)
cfit1 <- ppm(SouthPoints ~ dist, data=copper)

# Likelihood ratio test (p. 334)
# Test : H0 : Absence de l'effet due à la variable
anova(mfit0, mfit1, test="Chi") # < 0.05. Effet "dfault"
anova(cfit0, cfit1, test="Chi") # >0.05. Pas d'effet "dist"

# Wald test for single parameter
coef(summary(mfit1)) # Z-test. Effet "dfault"
coef(summary(cfit1)) # Pas d'effet "dist"
V <- coef(summary(mfit1))["dfault", "Zval"]
pnorm(V, lower.tail=TRUE)

# Score test (moins performant) (p. 336)
# Avertissements (Caveats, p. 337) : H0 faux => pas forcément H1 Vrai

# Model selection using AIC (p. 338)
fitxy <- ppm(swedishpines ~ x + y)
step(fitxy)

drop1(fitxy)
# The output indicates that the lowest AIC (i.e. 842) would be achieved by deleting the x term. 

fitcsr <- ppm(swedishpines~1)
add1(fitcsr, ~x+y)
# A l'inverse

# Meilleur modèle (AIC CSR sans x et y)

# Autre exemple
bigfit <- ppm(swedishpines ~ polynom(x,y,3))
formula(bigfit)
formula(step(bigfit, trace=1))

# Goodness-of-fit tests for an intensity model (p. 343)
# H0 : le modèle est bon (sous Poisson (CSR))
X <- copper$SouthPoints
D <- distfun(copper$SouthLines)
cdf.test(X, D, test="ad")

cdf.test(mfit0, mur$dfault, test="ad")

# Goodness-of-fit tests of independence between points (p. 343)
# H0 : indépendance entre les points (Poisson)
quadrat.test(bei)
clarkevans.test(bei)

# Monte Carlo tests
# Voir page 344 -> 357

### Envelopes in spatstat ###
plot(envelope(redwood, Lest, nsim=39))
plot(envelope(redwood, Lest, nsim=39, global=TRUE))

# Envelopes for any fitted model
numata <- residualspaper$Fig1
fit <- ppm(numata ~ polynom(x,y,3))
E <- envelope(fit, Lest, nsim=19, global=TRUE, correction="border")
plot(E)

# Envelopes based on any simulation procedure
e <- expression(rpoispp(100))
eval(e)

e <- expression(rlabel(amacrine))
E <- envelope(amacrine, Lcross, nsim=19, global=TRUE, simulate=e)
plot(E)

# Envelopes based on a set of point patterns
Xlist <- list()
for(i in 1:99) Xlist[[i]] <- runifpoint(42)
envelope(cells, Kest, nsim=99, simulate=Xlist)
plot(envelope(cells, Kest, nsim=99, simulate=Xlist))

EK <- envelope(cells, Kest, nsim=99, savepatterns=TRUE) # Kest
Ep <- envelope(cells, pcf, nsim=99, simulate=EK) # PCF
plot(Ep)

EK <- envelope(cells, Kest, nsim=99, savepatterns=TRUE)
Ep <- envelope(EK, pcf)
plot(Ep)

# Pointwise envelopes
envelope(redwood, Lest)
# Simultaneous envelopes
envelope(redwood, Lest, global=TRUE)

# Envelopes based on sample mean & variance
E<-envelope(cells, Kest, nsim=100, VARIANCE=TRUE)
plot(E)


# One-sided envelopes
E<-envelope(cells, Kest, nsim=100, alternative="greater")
E<-envelope(cells, Kest, nsim=100, alternative="less")
plot(E)

# Re-using envelope data (p. 363)
E1 <- envelope(redwood, Kest, savepatterns=TRUE)
E2 <- envelope(E1, Gest, global=TRUE, transform=expression(fisher(.)))
### MARCHE PAS
plot(E2)

A1 <- envelope(redwood, Kest, nsim=39, savefuns=TRUE)
A2 <- envelope(A1, global=TRUE, nsim=19,
                 transform=expression(sqrt(./pi)))
plot(A1)
plot(A2)

# Pooling several envelopes
E1 <- envelope(cells, Kest, nsim=10, savefuns=TRUE)
E2 <- envelope(cells, Kest, nsim=20, savefuns=TRUE)
plot(E1)
plot(E2)
E <- pool(E1, E2)
plot(E)

#### Validation of Poisson models ####
# Goodness-of-fit tests of a fitted model
fit2e <- ppm(bei ~ polynom(elev,2), data=bei.extra)
fit2e
M <- quadrat.test(fit2e, nx=4, ny=2)
M
# ddl = nombre de cases (4*2-3(intercept+elev+I(elev²))=5)
plot(M)

elev <- bei.extra$elev
grad <- bei.extra$grad
b <- quantile(elev, probs=(0:4)/4)
Zcut <- cut(elev, breaks=b, labels=1:4)
V <- tess(image=Zcut)
quadrat.test(fit2e, tess=V)
plot(V)

# Variantes
cdf.test(fit2e, grad, test="ks")
berman.test(fit2e, grad)

fit2e1g <- update(fit2e, ~ . + grad)
anova(fit2e, fit2e1g, test="Chi")
# Modèle avec grad meilleur
AIC(fit2e1g)
AIC(fit2e)

# Relative intensity
# Inverse-lambda weightin
lam0 <- fitted(fit2e, dataonly=TRUE)
rel2e <- density(bei, weights=1/lam0)
range(rel2e)
plot(rel2e)

# Relative intensity as function of covariate
lambda0 <- predict(fit2e)
rh1 <- rhohat(bei, grad, baseline=lambda0)
plot(rh1);plot(predict(rh1))
rh2 <- rhohat(fit2e, grad)
plot(rh2);plot(predict(rh2))

# Residuals for Poisson processes
res2e <- residuals(fit2e)
plot(res2e)

# Smoothed residual field
plot(Smooth(res2e))
plot(contour(Smooth(res2e)))

# Scaled or weighted residuals
pres2e <- residuals(fit2e, type="pearson")
plot(pres2e)
plot(Smooth(residuals(fit2e, type="pearson")))
plot(contour(Smooth(residuals(fit2e, type="pearson"))))

# Four-panel residual plot
diagnose.ppm(fit2e)

# Partial residuals plot (p. 384)
fit1g <- ppm(bei ~ grad)
coef(fit1g)
parres(fit1g, "grad")
plot(parres(fit1g, "grad"))

# Added variable plots
fit2g <- update(fit1g, ~ polynom(grad,2))
add<-addvar(fit2g, elev)
plot(add)

## Leverage and influence (Points leviers et point d'influence)
lev <- leverage(fit2g)
inf <- influence(fit2g)
dfb <- dfbetas(fit2g)
plot(lev);plot(inf);plot(dfb)

# Murchison example
mur <- lapply(murchison, rescale, s=1000, unitname="km")
attach(mur)
green <- greenstone
dfault <- distfun(faults)
murfit1x<-ppm(gold ~ green * dfault)
murlev1x<-leverage(murfit1x)
murinf1x<-influence(murfit1x)
murdfb1x<-dfbetas(murfit1x)

persp(as.im(murlev1x))
plot(murinf1x)
marks(as.ppp(murinf1x))

# Residual summary functions
# K-Function
cellKr <- Kres(cells, correction="best")
cellGr <- Gres(cells, correction="best")
plot(cellKr);plot(cellGr)
# Pas poisson homogène

jfit <- ppm(residualspaper$Fig1 ~ polynom(x,y,3))
jKr <- Kres(jfit, correction="best")
fvnames(jKr, ".s") <- c("ihi", "ilo")
plot(jKr)
# Poisson homogène