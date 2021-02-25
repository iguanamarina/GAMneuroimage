#############################################################
## Carga de paquetes y datos para el ejemplo Brain de Wood ##
#############################################################

#install.packages("mgcv")
#install.packages("gamair")

library(mgcv)
library(gamair)
data(brain)
brain
attach(brain)
?brain
#0 is the base region; 1 is the region of interest; 2 is the region activated by the experimental stimulus
plot(brain$X~brain$Y) # base estructural

#### 

brain <- brain[brain$medFPQ>5e-3,] # Seleccionar los datos de brain que presenten un medFPQ mayor que 5e-3 (eliminar outliers)

####

# The skewed nature of the response data (medFPQ) and the fact that it is a positive quantity, 
# suggest that some transformation may be required if a Gaussian error model is to be used. 
# Attempting to use a Gaussian model without transformation confirms this:

m0 <- gam(medFPQ ~ s(Y,X,k=100), data=brain)
par(mfrow=c(2,2))
gam.check(m0,old.style = F) 

# gam.check produces basic residual plots. As explained in the figure caption, 
# there are clear problems with the constant variance assumption: the variance is increasing with the mean.

# Normal Q-Q Plot muestra que la asumpción Gaussiana no se sostiene.
# Parece sensato asumir que la varianza constante no ocurre, sino que aumenta con la media.

# A simple informal approach. If we assume that var(yi) ∝ μβi, where μi = E(yi) and β is some parameter,
# then a simple regression estimate of β can be obtained:

e <- residuals(m0)
fv <- fitted(m0)
lm(log(e^2) ~ log(fv))

# β ≈ 2; from the residuals of the simple fit, variance increases with the square of the mean - Suggests: Gamma distrib.
# log link para garantizar que todas las predicciones de FPQ del modelo sean positivas:

m1 <- gam(medFPQ^0.25 ~ s(Y,X,k=100),data=brain) # m1 -> modelo de datos transformados
gam.check(m1)
#nothing problematic here


m2 <- gam(medFPQ~s(Y,X,k=100),data=brain,family = Gamma(link=log)) # m2 -> modelo de log-gamma
gam.check(m2)
#also nothing problematic

#Estimamos las medias de los modelos para compararlas con las del raw data

mean(fitted(m1)^4); mean(fitted(m2)); mean(medFPQ)

# [1] 0.9855539  # m1 tends to underestimate
# [1] 1.211483   # m2 se acerca más a la media real
# [1] 1.247911   # media a partir de los datos brutos

# m2 (gamma log-link model) parece el más adecuado hasta el momento. Vamos a examinarlo:

m2
par(mfrow=c(2,2))
vis.gam(m2,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),main="m2 - Suavizado isotrópico")
# vis.gam sirve para plotear predicciones de un ajuste gam frente a las covariables indicadas

# Color = "topo", "heat", "cm", "terrain", "gray" or "bw"


###################################################
## 7.2.2 - SERÍA MEJOR UNA ESTRUCTURA ADITIVA?   ##
###################################################


# Dado el alto número de grados de libertad del modelo m2, vamos a ver si un GAM sería más eficiente

m3 <- gam(medFPQ ~ s(Y,k=30) + s(X,k=30), data=brain, family = Gamma(link=log))
m3

#GCV=Generalized Cros-Validation
#m2 GCV score: 0.6216871 
#m3 GCV score: 0.6453502 
#GCV es mayor en el modelo 3, lo que indica que no hay mejoría

summary(m2);summary(m3)
#Deviance explained = 26.4%
#Deviance explained = 20.5%
#La variabilidad explicada por los modelos tampoco supportea la idea de usar el m3

AIC(m2,m3) #Akaike information criteria; se ofrece una estimación relativa de la información perdida 
           #cuando se utiliza un modelo determinado para representar el proceso que genera los datos.

#       df      AIC
#m2 62.61062 3321.681 - bivariate smooth
#m3 31.77467 3393.738 - additive model

# Se pierde más información con el m3 (modelo aditivo)

vis.gam(m3,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),main="m3 - Modelo aditivo")

# Además del GCV y el AIC, una de las pruebas más fuertes en contra del modelo aditivo (m3) es el gráfico de 
# predicción de log(actividad cerebral). Esta estructura aditiva produce rayas horizontales y verticales sin
# soporte real en los datos ni en el funcionamiento cerebral natural.


###################################################
## 7.2.3 - ISOTROPIC OR TENSOR PRODUCT SMOOTHS?   #
###################################################


# SINGLE TENSOR PRODUCT SMOOTH:

tm <- gam(medFPQ ~ te(Y,X,k=10), data=brain, family = Gamma(link=log))

vis.gam(tm,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),main="tm - Single tensor smooth")

# ADDITIVE + INTERACTION MODEL:

tm1 <- gam(medFPQ ~ s(X,k=10,bs="cr") + s(Y,k=10,bs="cr") + ti(X,Y,k=10), data=brain, family = Gamma(link=log)) # bs="cr" - These have a cubic spline basis

vis.gam(tm1,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),main="tm1 - Additive + interaction")


gam.check(tm);gam.check(tm1) #A priori estos modelos son ambos razonables tras analizar sus plots de residuos.

AIC(m2,tm,tm1) #AIC sgue sin apoyar el nuevo modelo. Parece que es mejor el tm1 que el tm, pero siguen siendo 
# ambos peores que el m2 isotrópico

#       df      AIC
#m2  62.61062 3321.681 - isotropic
#tm  60.34768 3332.817 - tensor product  
#tm1 57.08366 3330.187 - main + interaction
    

# ¿Estructura de modelo aditivo vs bivariate? Examinar tm1 con summary/ANOVA #

anova(tm1)

# p-valor de ti(X,Y) sugiere que el "term" suavizador no es cero, por lo tanto la interacción ES NECESARIA
# y un modelo aditivo por si solo NO ES SUFICIENTE.


###################################################
##  7.2.4 - DETECTAR SIMETRÍAS CON VARIABLES "by":#
###################################################


# Es posible que queramos averiguar si la imagen debajo de unos datos ruidosos es simétrica. 
# Vamos a testear si los niveles de actividad subyacentes son simétricos al rededor de X=64'5

brain$Xc <- abs(brain$X - 64.5) # valor absoluto de X-64'5, o sea, distancia al eje de simetría en X

brain$right <- as.numeric(brain$X<64.5) #dummy variable: 0=Hemisferio Derecho; 1=Hemisferio Izquierdo

m.sim <- gam(medFPQ~s(Y,Xc,k=100),data=brain,family=Gamma(link=log)) 

m.asim <- gam(medFPQ~s(Y,Xc,k=100)+s(Y,Xc,k=100,by=right),data=brain,family=Gamma(link=log))

m.sim #GCV score: 0.6489799
m.asim #GCV score: 0.6176281

# Esto apoya la idea de que el modelo asimétrico se corresponde mejor con la realidad de los datos

AIC(m.sim,m.asim) # Akaike también apoya que el asimétrico deja menos información sin explicar
#          df      AIC
#m.sim  53.44110 3397.790
#m.asim 97.20431 3300.231

anova(m.asim)
#s(Y,Xc):right -> p-valor=1.06e-08. Si el modelo simétrico fuese adecuado, el término s(Y,Xc):right no
#debería ser diferente de cero, y sí que apunta claramente a serlo.

anova(m.sim,m.asim) #también se puede obtener la misma conclusión de aquí pero con menos detalle

## Gráficamente se puede ver por qué se rechaza el modelo simétrico y se acepta uno asimétrico ##

par(mfrow=c(2,2))

vis.gam(m.sim,plot.type = "contour",view=c("Xc","Y"),too.far = .03,
        color="terrain",n.grid=60,zlim=c(-1,2),main="M.Sim - Both Sides")

vis.gam(m.asim,plot.type = "contour",view=c("Xc","Y"),too.far = .03,
        cond=list(right=0),
        color="terrain",n.grid=60,zlim=c(-1,2),main="M.Asim - Both Sides")

vis.gam(m.asim,plot.type = "contour",view=c("Xc","Y"),too.far = .03,
        cond=list(right=1),
        color="terrain",n.grid=60,zlim=c(-1,2),main="M.Asim - Both Sides")



###################################################
##      7.2.5 - COMPARING TWO SURFACES           ##
###################################################


# Utilizar el mismo procedimiento que hemos usado para comparar dos imagenes espejo para comparar
# dos superficies (imágenes cerebrales) completamente independientes. Primero simulamos datos con 'm2'.


brain1 <- brain  #copy

mu <- fitted(m2)
n <- length(mu)
ind <- brain1$X<60 & brain1$Y<20
mu[ind] <- mu[ind]/3

set.seed(12345)
brain1$medFPQ <- rgamma(rep(1,n),mu/m2$sig2,scale=m2$sig2)
brain1$medFPQ # Cerebro dummie simulado

# Ahora se combinan los datos y luego se crean variables dummy para identificar que datos corresponden a qué cerebro

brain2 = rbind(brain,brain1)

brain2$sample1 <- c(rep(1,n),rep(0,n))
brain2$sample0 <- 1-brain2$sample1 #dummy (?)


# Ahora sacamos dos modelos:
  
  ## Modelo 1 -> Superficie única combinada para ambos datasets
  ## Modelo 2 -> Modelo en que las superficies pueden diferir

# Se suele llevar a cabo así -con una única superficie- para evitar tener dos superficies parsimónicas
# De este modo se tiene una superficie compleja y en ella se calculan las diferencias (pequeñas) que podrían ser observadas
# Simple Surface + Differences Map -> Efficient way, reduced degrees of freedom.


m.same <- gam(medFPQ~s(Y,X,k=100),data=brain2,family=Gamma(link=log))

m.diff <- gam(medFPQ~s(Y,X,k=100)+s(Y,X,by=sample1,k=100),data=brain2,family=Gamma(link=log))

m.same;m.diff

#1) GCV score Same: 0.649933 
#2) GCV score Diff: 0.6434488 

AIC(m.same,m.diff)

#           df      AIC
#m.same 70.86991 6464.853
#m.diff 87.48844 6426.150

## Tanto la estimación de GCV como el AIC supportean la idea de que es mejor m.diff que m.same

anova(m.diff)

# El test de significancia de las diferencias (ANOVA) también apoya esta idea.

gam.check(m.diff)
gam.check(m.same)

par(mfrow=c(1,2))

#vis.gam(m.diff) #no apropiado (pero reshu)
    ##plot types -> one of "contour" or "persp"

vis.gam(m.diff,
        plot.type = "contour",
        too.far = .03,
        color="terrain",
        n.grid=60,
        zlim=c(-1,2),
        main="Modelo Diff.")


#vis.gam(m.same) #no apropiado (pero reshu)
vis.gam(m.same,
        plot.type = "contour",
        too.far = .03,
        color="terrain",
        n.grid=60,
        zlim=c(-1,2),
        main="Modelo Same")



###################################################
##     7.2.6 - PREDICTION WITH predict.gam       ##
###################################################

# Usar la función predict.gam para calcular valores nuevos a partir de un modelo GAM ajustado previamente

# On the scale of the linear predictor:

predict(m2)[10]
pv <- predict(m2,se=T) #se:standard error
pv$fit[1:5] #prediction
pv$se[1:5] #error


# On the response scale:

predict(m2,type="response")[1:10]
pv<-predict(m2,type="response",se=T)
pv$fit[1:10]
pv$se[1:10] # SE by means of the Taylor expansion approach

# Dataframe para indicar las coordenadas del predict

pd<-data.frame(X=c(80.1,68.3),Y=c(41.8,41.8))
predict(m2,newdata = pd)

pr<-predict(m2,newdata = pd,type="response",se=T)
pr$fit
pr$se

# También se puede calcular las contribuciones de cada término del modelo -excluyendo el intercepto- va a 
# a aportar al resultado final (linear predictor) [usamos m3 porque tiene varios términos, ejemplo]

predict(m3,newdata = pd, type="terms",se=T) # $fit serán los valores predichos; $se.fit, el error.


####### PREDICTION WITH lpmatrix #######

# Es posible obtener una matriz de predicción Xp que mapea los parámetros del modelo, B(eta). 
# lpmatrix puede obtener esto con type = "lpmatrix"

Xp <- predict(m2,newdata=pd,type="lpmatrix")
fv <- Xp  %*% coef(m2)  # %*% es multiplicación de matrices (matrix multiplication)
fv

## Variance: Algunos cálculos de varianza se vuelven directos.

d<-t(c(1,-1))
d%*%fv
d%*%Xp%*%m2$Vp%*%t(Xp)%*%t(d)





