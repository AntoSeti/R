waterstriders
plot(waterstriders)

solist()

sapply(waterstriders,npoints)

K <- as.anylist(lapply(waterstriders, Kest))
plot(K,main="",main.panel=letters[1:3],legend=FALSE)

D <- solapply(split(urkiola),density)
plot(D, equal.ribbon=TRUE,main="")

# Hyperframe
WS<-hyperframe()
WS$larvae <- waterstriders
WS$experiment <- factor(1:3)
plot(WS)

H <- hyperframe(Bugs=waterstriders)
plot(H, quote(plot(Kest(Bugs))))
with(H, npoints(Bugs))
D <- with(H,distmap(Bugs))

H <- hyperframe(Gerris=waterstriders)
K <- with(H,Kest(Gerris))
plot(K)

H <- hyperframe(Gerris=waterstriders)
m <- with(H, nndist(Gerris))
m

with(H, min(nndist(Gerris)))

# Simulation
lambda <- rexp(3,rate=1/50)
H<-hyperframe(lambda=lambda)
H$Points<-with(H,rpoispp(lambda))
plot(H, quote(plot(Points, main=lambda)))

H$Title<-with(H, parse(text=paste("lambda==",signif(lambda,3))))
plot(H,quote(plot(Points,main=Title)))

H$X<-with(H,rpoispp(50))

# exemples hyperframe
head(osteo)
summary(pyramidal)

py<-pyramidal
py$n<-with(py,npoints(Neurons))
py$area<-with(py,area(Neurons))
py<-as.data.frame(py,warn=FALSE)

plot(sqrt(n)~group, data=py)
v<-pretty(py$n);axis(4,at=sqrt(v),labels=v)
sapply(split(py$n/py$area,py$group),mean)

# if window areas inequual
ntot <- sapply(split(py$n,py$group),sum)
atot <- sapply(split(py$area,py$group),sum)
ntot/atot

# same result as (JF !!!)
fitn<-glm(n~offset(log(area))+group,family=poisson,data=py)
newd<-data.frame(area=1,group=levels(py$group))
predict(fitn, newdata=newd,type="response")

# diff entre groupes ?
anova(fitn, test="Chi")

plot(pyramidal,quote(plot(density(Neurons),main=group)))
plot(with(pyramidal, density(Neurons)))

Keach<-lapply(waterstriders, Kest, ratio=TRUE)
K<-pool(Keach[[1]],Keach[[2]],Keach[[3]])
# or K<-do.call(pool,Keach)
plot(K,cbind(pooliso,pooltheo,loiso,hiiso)~r,shade=c("loiso","hiiso"))


# Poisson models
# exemple simba
mppm(Points~group, simba, Poisson())
mppm(Points~1,simba)
# for each pattern
mppm(Points~id-1,simba)

# spatial covariate
mppm(Points~Image,data=demohyper)

# interaction
mppm(Points~Group/Image,data=demohyper)
mppm(Points~(Group-1)/Image,data=demohyper)

# mixed effect (ne marche pas)
mppm(Points~1, simba, random=~1|id)

H <- hyperframe(P=waterstriders)
mppm(P~1, data=H, random=~1|id) # ne donne pas les sorties que l'on espere

mppm(Neurons~group, data=pyramidal, random=~1|id)
anova(mppm(Neurons~group, data=pyramidal, random=~1|id))

# Application
# Creer un hyperframe
# hyperframe tous groupes confondus, pour les 6 images (5 lignes (5 groupes))

# Image 1
Im1.sol <- solist(X_baby3month_Im[[1]],X_baby6month_Im[[1]],X_baby9month_Im[[1]],X_baby12month_Im[[1]],X_adult_Im[[1]])
hyp.Im1<-hyperframe()
hyp.Im1$ppp <- Im1.sol
hyp.Im1$group <- c("3 mois","6 mois","9 mois","12 mois","adultes")
hyp.Im1
plot(hyp.Im1,quote(plot(ppp,main=group)))

# Pour toutes les images
Im.sol <- list()
for(i in 1:6){
Im.sol[[i]] <- solist(X_baby3month_Im[[i]],X_baby6month_Im[[i]],X_baby9month_Im[[i]],X_baby12month_Im[[i]],X_adult_Im[[i]])
hyp.Im<-hyperframe()
hyp.Im$ppp <- Im.sol[[i]]
hyp.Im$group <- c("3 mois","6 mois","9 mois","12 mois","adultes")
plot(hyp.Im,quote(plot(ppp,main=group)))
}

# Il faudrait maintenant rajouter la covariable spatiale (saillance moyen)
babyIm.Moylevel


# Exemple avec demohyper
plot(demohyper,quote({ plot(Image, main="");plot(Points, add=TRUE)}))

# Image 1
pppIm1.sol <- solist(X_baby3month_Im[[1]],X_baby6month_Im[[1]],X_baby9month_Im[[1]],X_baby12month_Im[[1]],X_adult_Im[[1]])
sailIm1.sol <- solist(babyIm.Moylevel[[1]],babyIm.Moylevel[[1]],babyIm.Moylevel[[1]],babyIm.Moylevel[[1]],babyIm.Moylevel[[1]])
hyp.Im1<-hyperframe()
hyp.Im1$ppp <- pppIm1.sol
hyp.Im1$group <- c("3 mois","6 mois","9 mois","12 mois","adultes")
hyp.Im1$sail <- sailIm1.sol
hyp.Im1
plot(hyp.Im1,quote(plot(ppp,main=group)))
plot(hyp.Im1,quote({ plot(sail, main=group);plot(ppp, add=TRUE)}))

# Pour toutes les images
pppIm.sol <- list()
sailIm.sol  <- list()
for(i in 1:6) {
pppIm.sol[[i]] <- solist(X_baby3month_Im[[i]],X_baby6month_Im[[i]],X_baby9month_Im[[i]],X_baby12month_Im[[i]],X_adult_Im[[i]])
sailIm.sol[[i]] <- solist(babyIm.Moylevel[[i]],babyIm.Moylevel[[i]],babyIm.Moylevel[[i]],babyIm.Moylevel[[i]],babyIm.Moylevel[[i]])
hyp.Im<-hyperframe()
hyp.Im$ppp <- pppIm.sol[[i]]
hyp.Im$group <- c("3 mois","6 mois","9 mois","12 mois","adultes")
hyp.Im$sail <- sailIm.sol[[i]]
# plot(hyp.Im1,quote(plot(ppp,main=group)))
plot(hyp.Im,quote({ plot(sail, main=group);plot(ppp, add=TRUE)}))
}


# Tentative list d'hyperframe
pppIm.sol <- list()
sailIm.sol  <- list()
hyp.Im <- list()
for(i in 1:6) {
  pppIm.sol[[i]] <- solist(X_baby3month_Im[[i]],X_baby6month_Im[[i]],X_baby9month_Im[[i]],X_baby12month_Im[[i]],X_adult_Im[[i]])
  sailIm.sol[[i]] <- solist(babyIm.Moylevel[[i]],babyIm.Moylevel[[i]],babyIm.Moylevel[[i]],babyIm.Moylevel[[i]],babyIm.Moylevel[[i]])
  hyp.Im[[i]]<-hyperframe()
  hyp.Im[[i]]$ppp <- pppIm.sol[[i]]
  hyp.Im[[i]]$group <- c("3 mois","6 mois","9 mois","12 mois","adultes")
  hyp.Im[[i]]$sail <- sailIm.sol[[i]]
  # plot(hyp.Im[[i]],quote(plot(ppp,main=group)))
  plot(hyp.Im[[i]],quote({ plot(sail, main=group);plot(ppp, add=TRUE)}))
}

# Image originale
# Pour toutes les images
m <- list(m1,m2,m3,m4,m5,m6)
babyIm.originallevel <- list()
par(mfrow=c(3,2))
for (i in 1:6) {
  
  matrixoriginale<-matrix(m[[i]],nrow=768,ncol=1024,byrow=TRUE)
  
  nb_rows<-dim(matrixoriginale)[1]
  nb_columns<-dim(matrixoriginale)[2]
  flip_matrixoriginale<-matrix(nrow=nb_rows,ncol=nb_columns)
  
  for(j in 1:nb_rows) { flip_matrixoriginale[j,]=matrixoriginale[nb_rows+1-j,] }
  matrixoriginale<-flip_matrixoriginale
  
  # Builds a pixel image in spatstat format
  babyIm.originallevel[[i]]<-im(matrixoriginale,xrange=c(0,1024),yrange=c(0,768))
  
  image(babyIm.originallevel[[i]])
}

# List hyperframe avec vraies images + images saillance
pppIm.sol <- list()
originIm.sol <- list()
sailIm.sol  <- list()
hyp.Im <- list()
for(i in 1:6) {
  pppIm.sol[[i]] <- solist(X_baby3month_Im[[i]],X_baby6month_Im[[i]],X_baby9month_Im[[i]],X_baby12month_Im[[i]],X_adult_Im[[i]])
  originIm.sol[[i]] <- solist(babyIm.originallevel[[i]],babyIm.originallevel[[i]],babyIm.originallevel[[i]],babyIm.originallevel[[i]],babyIm.originallevel[[i]])
  sailIm.sol[[i]] <- solist(babyIm.Moylevel[[i]],babyIm.Moylevel[[i]],babyIm.Moylevel[[i]],babyIm.Moylevel[[i]],babyIm.Moylevel[[i]])
  hyp.Im[[i]]<-hyperframe()
  hyp.Im[[i]]$ppp <- pppIm.sol[[i]]
  hyp.Im[[i]]$group <- as.factor(c("3mois","6mois","9mois","12mois","adultes"))
  hyp.Im[[i]]$origin  <- originIm.sol[[i]]
  hyp.Im[[i]]$sail <- sailIm.sol[[i]]
  hyp.Im[[i]]$n <- with(hyp.Im[[i]],npoints(ppp))
  hyp.Im[[i]]$area  <- with(hyp.Im[[i]],area(ppp))
  # plot(hyp.Im[[i]],quote(plot(ppp,main=group)))
  # plot(hyp.Im[[i]],quote({ plot(sail, main=group);plot(ppp, add=TRUE)}))
}
hyp.Im[[1]]
# Box-plot du nombre de points par groupe, ex Image 1
plot(sqrt(n)~group, data=hyp.Im[[1]])

# Intensité estimé par groupes, pour une unité d'aire
sapply(split(hyp.Im[[1]]$n/hyp.Im[[1]]$area,hyp.Im[[1]]$group),mean)

ntot <- sapply(split(hyp.Im[[1]]$n,hyp.Im[[1]]$group),sum)
atot <- sapply(split(hyp.Im[[1]]$area,hyp.Im[[1]]$group),sum)
ntot/atot

fitn <- glm(n~offset(log(area))+group, family=poisson,data=hyp.Im[[1]])
newd <- data.frame(area=1,group=levels(hyp.Im[[1]]$group))
predict(fitn, newdata=newd, type="response")

anova(fitn, test="Chi")

# spatial covariate
mppm(Points~Image/Group,data=demohyper)

# application, Image 1, groupe bb 3 mois
mppm(ppp~sail,data=hyp.Im[[1]][1])

# pour les 5 groupes, image 1
mppm.Im1.list <- list()
for(i in 1:5){
  mppm.Im1.list[[i]] <- mppm(ppp~sail,data=hyp.Im[[1]][i])
}
su

# Image 2
mppm.Im2.list <- list()
for(i in 1:5){
  mppm.Im2.list[[i]] <- mppm(ppp~sail,data=hyp.Im[[2]][i])
}

# Image 3
mppm.Im3.list <- list()
for(i in 1:5){
  mppm.Im3.list[[i]] <- mppm(ppp~sail,data=hyp.Im[[3]][i])
}

# Image 4
mppm.Im4.list <- list()
for(i in 1:5){
  mppm.Im4.list[[i]] <- mppm(ppp~sail,data=hyp.Im[[4]][i])
}

# Image 5
mppm.Im5.list <- list()
for(i in 1:5){
  mppm.Im5.list[[i]] <- mppm(ppp~sail,data=hyp.Im[[5]][i])
}

# Image 6
mppm.Im6.list <- list()
for(i in 1:5){
  mppm.Im6.list[[i]] <- mppm(ppp~sail,data=hyp.Im[[6]][i])
}

# spatial covariate
fit1 <- mppm(ppp~sail,data=hyp.Im[[1]])
fit2 <- mppm(ppp~group,data=hyp.Im[[1]])
fit2b <- mppm(ppp~group-1,data=hyp.Im[[1]])
fit3 <- mppm(ppp~(group-1)/sail,data=hyp.Im[[1]])

fit1
fit2
fit3

# diff entre groupes ?
anova(fit1, test="Chi")
anova(fit2, test="Chi")
# ==
anova(fitn,test="Chi")
# !!!
# fit2=fitn

anova(fit3, test="Chi")

# p-value tres faible => diff significatives entre les groupes

# To fit a uniform Poisson process to each pattern, with different intensity for each pattern
mppm(ppp~id-1,data=hyp.Im[[1]])
# ==
mppm(ppp~group-1,data=hyp.Im[[1]])

mppm(ppp~sail,data=hyp.Im[[1]])
mppm(ppp~group+sail,data=hyp.Im[[1]])
mppm(ppp~group/sail,data=hyp.Im[[1]])
...

# On passe par l'exponentiel et on retrouve l'intensité par unité d'aire
sapply(split(hyp.Im[[1]]$n/hyp.Im[[1]]$area,hyp.Im[[1]]$group),mean)

fitn <- glm(n~offset(log(area))+group, family=poisson,data=hyp.Im[[1]])
newd <- data.frame(area=1,group=levels(hyp.Im[[1]]$group))
predict(fitn, newdata=newd, type="response")

# Tentative Effet mixte
res=mppm(ppp~group,random=~1|id,data=hyp.Im[[1]])
summary(mppm(ppp~1,random=~1|group,data=hyp.Im[[1]]))

anova(mppm(ppp~group,random=~1|id,data=hyp.Im[[1]]))
