# Exo Machines
Machines
attach(Machines)
(fm1Machine=lme(score~Machine,random=~1|Worker))
(fm2Machine=lme(score~Machine,random=~1|Worker/Machine))

summary(fm1Machine)
summary(fm2Machine)

anova(fm1Machine)
anova(fm2Machine)

# Exo Oats (avoine)
Oats
attach(Oats)
(fm2Oats=lme(yield~ordered(nitro)+Variety,random=~1|Block/Variety))
anova(fm2Oats)
summary(fm2Oats)


# ex
OrthoFem <- Orthodont[Orthodont$Sex=='Female',]

lme(distance~age, data=OrthoFem)
(fm10rthF <- lme(distance~age, data=OrthoFem, random=~age|Subject))


summary(fm10rthF)$coe
summary(lme(distance~age, data=OrthoFem, random=~1|Subject))

random.effects(fm10rthF)
coef(fm10rthF)
