##########################################################
###              PAQUETES   NECESARIOS                 ###
##########################################################


#install.packages("mgcv")
#install.packages("gamair")
#install.packages("oro.nifti")
#install.packages("memsic")

library(mgcv)
library(gamair)
library(oro.nifti)
library(memisc)


##########################################################
###         VISUALIZACION DE NEURO-IMAGENES            ###
##########################################################


#Lectura de la imagen NIFTI 'PETdataExample' y muestra de datos generales
img <- readNIfTI("PETdataEXAMPLE", verbose = FALSE, warn = -1, reorient = TRUE,
       call = NULL, read_data = TRUE); img


#Visualizacion de la imagen completa
image(img) #axial por defecto
image(img,plane="coronal") #coronal
image(img,plane="sagittal") #sagital
orthographic(img,text="Plano Ortografico PET") #plano ortografico


##########################################################
###                    RAW  DATA                       ###
##########################################################


#Info. numerica en el NIfTI:
data=img_data(img)
class(data)
dim(data)
print(data)
hist(data)


#Diferentes coordenadas de la imagen (comprobacion)
img[1,2,3]
img[35,1,40]
img[35,45,40]


#Convertir los datos de la imagen en un data.frame
data = img@.Data
data = to.data.frame(data)


#Nuevas dimensiones 
dim(data);dim(img) 
  #79*95=7505, se ve que cambia de dimensiones a una bidimensional en que una de las variables es 
  #la propia coordenada Z. Al ganar dos columnas (variables Y y Z) tambien una dimension pasa de 79 a 81
  #comprobando en Data y con img[1,2,3] se ve que X es la horizontal, Y=Var1, y Z=var2


##########################################################
###                    FILETEADO                       ###
##########################################################


#Creamos unas slices a diferentes alturas de Z para hacer pruebas de diferentes modelos plausibles 
#Z=79 de modo que para evaluar a diferentes alturas -> 1,
#!!!# ¿Mereceria la pena hacer una funcion que automatice el proceso y lo haga para todas las slices?

slice1  <- data[data$Var2==1,]  #Z=1
slice20 <- data[data$Var2==20,] #Z=20
slice40 <- data[data$Var2==40,] #Z=40
slice60 <- data[data$Var2==60,] #Z=60
slice79 <- data[data$Var2==79,] #Z=79



##########################################################
###                  FILETE    Z=1                     ###
##########################################################


#Limpiamos data.frame
slice1$Var1=NULL
slice1$Var2=NULL
slice1


#Creamos variables dummy para un data.frame limpio y logico
x<-rep(1:79, each=95)
y<-seq(1:95)


#Unificamos valores de PET uno tras otro en fila india
attach(slice1)
pet<-c(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,
        `21`,`22`,`23`,`24`,`25`,`26`,`27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`,`38`,`39`,`40`,
        `41`,`42`,`43`,`44`,`45`,`46`,`47`,`48`,`49`,`50`,`51`,`52`,`53`,`54`,`55`,`56`,`57`,`58`,`59`,`60`,
        `61`,`62`,`63`,`64`,`65`,`66`,`67`,`68`,`69`,`70`,`71`,`72`,`73`,`74`,`75`,`76`,`77`,`78`,`79`)

#Nuevo data.frame:
slice1=data.frame(x,y,pet)



#### PROPUESTA DE MODELOS PARA ESTE FILETE ####


# M0: Modelo Gaussiano sin transformaciones


M0 <- gam(pet ~ s(x,y,k=100), data=slice1)

par(mfrow=c(2,2))
gam.check(M0,old.style = F) 

par(mfrow=c(1,2))
vis.gam(M0,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
vis.gam(M0,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1)

plot.gam(M0,residuals=T,rug=T,se=T,pers = F)
plot.gam(M0,residuals=T,rug=F,se=F,pers = F)


# M1: Modelo con datos transformados (siguiendo el ejemplo de Wood^1/4) ¿Por que transforma asi? Check in the book.


E <- residuals(M0)
FV <- fitted(M0)
lm(log(E^2) ~ log(FV)) ## log(FV)=0.6934, de modo que no cuadra como antes, de hecho sería reducción de varianza con media


M1 <- gam(pet^0.25 ~ s(x,y,k=100),data=slice1) 

par(mfrow=c(2,2))
gam.check(M1,old.style = F) 

par(mfrow=c(1,2))
vis.gam(M1,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
vis.gam(M1,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1) 

plot.gam(M1,residuals=T,rug=T,se=T,pers = F)
plot.gam(M1,residuals=T,rug=F,se=F,pers = F)



# M2: fit with Gamma distribution (modelo de log-gamma) // Bivariate Smooth


M2 <- gam(pet~s(x,y,k=100),data=slice1,family = Gamma(link=log)) 

par(mfrow=c(2,2))
gam.check(M2,old.style = F) #QQplot muestra problemas con asumpciones gaussianas

par(mfrow=c(1,2))
vis.gam(M2,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
vis.gam(M2,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1) 

plot.gam(M2,residuals=T,rug=T,se=T,pers = F)
plot.gam(M2,residuals=T,rug=F,se=F,pers = F)


##
mean(pet);mean(fitted(M2));mean(fitted(M1)^4);mean(fitted(M0)) # En cuanto a la estimación de las medias, el modelo M0 se aproxima mejor
##


# M3: GAM Model // Additive model


M3 <- gam(pet ~ s(x,k=30) + s(y,k=30), data=slice1, family = Gamma(link=log))

par(mfrow=c(2,2))
gam.check(M3,old.style = F) 

par(mfrow=c(1,2))
vis.gam(M3,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
vis.gam(M3,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1) 

plot.gam(M3,residuals=T,rug=T,se=T,pers = F)
plot.gam(M3,residuals=T,rug=F,se=F,pers = F)


# M4: Single Tensor Product Smooth


M4 <- gam(pet ~ te(x,y,k=10), data=slice1, family = Gamma(link=log))

par(mfrow=c(2,2))
gam.check(M4,old.style = F) 

par(mfrow=c(1,2))
vis.gam(M4,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
vis.gam(M4,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1) 

plot.gam(M4,residuals=T,rug=T,se=T,pers = F)
plot.gam(M4,residuals=T,rug=F,se=F,pers = F)


# M5: Additive + Interaction


M5 <- gam(pet ~ s(x,k=10,bs="cr") + s(y,k=10,bs="cr") + ti(x,y,k=10), data=slice1, family = Gamma(link=log))

par(mfrow=c(2,2))
gam.check(M5,old.style = F) 

par(mfrow=c(1,2))
vis.gam(M5,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
vis.gam(M5,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1) 

plot.gam(M5,residuals=T,rug=T,se=T,pers = F)
plot.gam(M5,residuals=T,rug=F,se=F,pers = F)


#### EVALUACIÓN DE MODELOS PARA ESTE FILETE ####


summary(M0);summary(M1);summary(M2);summary(M3);summary(M4);summary(M5)

#M0 GCV score: 0.01255409 (df: 99.25)
#M1 GCV score: 0.002570961 (df: 99.04)
#M2 GCV score: 0.07716117 (df: 99.19)
#M3 GCV score: 0.1739448 (df: 41.15)
#M4 GCV score: 0.07952487 (df: 98.29)
#M5 GCV score: 0.07952356 (df: 98.19)


#Deviance explained M0 = 92.3%
#Deviance explained M1 = 92.6%
#Deviance explained M2 = 90.5%
#Deviance explained M3 = 78.1%
#Deviance explained M4 = 90.2%
#Deviance explained M5 = 90.2%


AIC(M0,M1,M2,M3,M4,M5) # the smaller the AIC, the better the fit.
 
#        df        AIC
# M0 100.25324 -11555.762
# M1 100.04051 -23456.936
# M2 100.18612 -13390.192
# M3  42.14888  -7167.698
# M4  99.29127 -13160.816
# M5  99.18902 -13160.935



##########################################################
###                  FILETE    Z=20                    ###
##########################################################


#Limpiamos data.frame
slice20$Var1=NULL
slice20$Var2=NULL
slice20


#Creamos variables dummy para un data.frame limpio y logico
x<-rep(1:79, each=95)
y<-seq(1:95)


#Unificamos valores de PET uno tras otro en fila india

detach(slice1)
attach(slice40)
pet<-c(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,
       `21`,`22`,`23`,`24`,`25`,`26`,`27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`,`38`,`39`,`40`,
       `41`,`42`,`43`,`44`,`45`,`46`,`47`,`48`,`49`,`50`,`51`,`52`,`53`,`54`,`55`,`56`,`57`,`58`,`59`,`60`,
       `61`,`62`,`63`,`64`,`65`,`66`,`67`,`68`,`69`,`70`,`71`,`72`,`73`,`74`,`75`,`76`,`77`,`78`,`79`)

#Nuevo data.frame:
slice20=data.frame(x,y,pet)
slice20



#### PROPUESTA DE MODELOS PARA ESTE FILETE ####


# M0: Modelo Gaussiano sin transformaciones


M0 <- gam(pet ~ s(x,y,k=100), data=slice20)

par(mfrow=c(2,2))
gam.check(M0,old.style = F) 

par(mfrow=c(1,2))
vis.gam(M0,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
vis.gam(M0,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1)

plot.gam(M0,residuals=T,rug=T,se=T,pers = F)
plot.gam(M0,residuals=T,rug=F,se=F,pers = F)



# M1: Modelo con datos transformados (siguiendo el ejemplo de Wood^1/4) ¿Por que transforma asi? Check in the book.

E <- residuals(M0)
FV <- fitted(M0)
lm(log(E^2) ~ log(FV)) ## log(FV)=0.6934

M1 <- gam(pet^0.25 ~ s(x,y,k=100),data=slice20) 

par(mfrow=c(2,2))
gam.check(M1,old.style = F) 

par(mfrow=c(1,2))
vis.gam(M1,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
vis.gam(M1,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1) 

plot.gam(M1,residuals=T,rug=T,se=T,pers = F)
plot.gam(M1,residuals=T,rug=F,se=F,pers = F)


# M2: fit with Gamma distribution (modelo de log-gamma) // Bivariate Smooth


M2 <- gam(pet~s(x,y,k=100),data=slice20,family = Gamma(link=log)) 

par(mfrow=c(2,2))
gam.check(M2,old.style = F) 

par(mfrow=c(1,2))
vis.gam(M2,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
vis.gam(M2,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1) 

plot.gam(M2,residuals=T,rug=T,se=T,pers = F)
plot.gam(M2,residuals=T,rug=F,se=F,pers = F)


# M3: GAM Model // Additive model


M3 <- gam(pet ~ s(x,k=30) + s(y,k=30), data=slice20, family = Gamma(link=log))

par(mfrow=c(2,2))
gam.check(M3,old.style = F) 

par(mfrow=c(1,2))
vis.gam(M3,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
vis.gam(M3,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1) 

plot.gam(M3,residuals=T,rug=T,se=T,pers = T)
plot.gam(M3,residuals=T,rug=F,se=F,pers = F)


# M4: Single Tensor Product Smooth


M4 <- gam(pet ~ te(x,y,k=10), data=slice20, family = Gamma(link=log))

par(mfrow=c(2,2))
gam.check(M4,old.style = F) 

par(mfrow=c(1,2))
vis.gam(M4,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
vis.gam(M4,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1) 

plot.gam(M4,residuals=T,rug=T,se=T,pers = F)
plot.gam(M4,residuals=T,rug=F,se=F,pers = F)


# M5: Additive + Interaction


M5 <- gam(pet ~ s(x,k=10,bs="cr") + s(y,k=10,bs="cr") + ti(x,y,k=10), data=slice20, family = Gamma(link=log))

par(mfrow=c(2,2))
gam.check(M5,old.style = F) 

par(mfrow=c(1,2))
vis.gam(M5,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
vis.gam(M5,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1) 

plot.gam(M5,residuals=T,rug=T,se=T,pers = F)
plot.gam(M5,residuals=T,rug=F,se=F,pers = F)


###############TESTS##############

M6 <- gam(pet ~ s(x,k=10,bs="ad") + s(y,k=10,bs="ad") + ti(x,y,k=10), data=slice40, family = Gamma(link=log))

par(mfrow=c(2,2))
gam.check(M6,old.style = F) 

par(mfrow=c(1,2))
vis.gam(M6,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
vis.gam(M6,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1) 

plot.gam(M6,residuals=T,rug=T,se=T,pers = F)
plot.gam(M6,residuals=T,rug=F,se=F,pers = F)

###########

M6 <- gam(pet ~ s(x,k=10,bs="ad") + s(y,k=10,bs="ad") + ti(x,y,k=10), data=slice20, family = Gamma(link=log))
M7 <- gam(pet ~ s(x,k=30,bs="ad") + s(y,k=30,bs="ad") + ti(x,y,k=30), data=slice20, family = Gamma(link=log))


par(mfrow=c(2,2))
gam.check(M7,old.style = F) 


par(mfrow=c(1,2))
vis.gam(M7,plot.type = "contour",too.far=0.03,
        color="heat",n.grid=200,zlim=c(-1,2),se=0,main="M7") 
vis.gam(M6,plot.type = "contour",too.far=0.03,
        color="heat",n.grid=200,zlim=c(-1,2),se=0,main="M6")



vis.gam(M6,plot.type = "contour",too.far=0.03,
        color="heat",n.grid=60,zlim=c(-4,4),se=0,main="heat")
vis.gam(M6,plot.type = "contour",too.far=0.03,
        color="cm",n.grid=60,zlim=c(-4,4),se=0,main="cm")
vis.gam(M6,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-4,4),se=0,main="terrain")
vis.gam(M6,plot.type = "contour",too.far=0.03,
        color="gray",n.grid=60,zlim=c(-4,4),se=0,main="gray")
vis.gam(M6,plot.type = "contour",too.far=0.03,
        color="bw",n.grid=60,zlim=c(-4,4),se=0,main="bw")



par(mfrow=c(1,3))
plot.gam(M6,residuals=T,rug=F,se=T,pers = T)


# bam(formula...) is an adaptation of GAM for large datasets.
# gamm(formula...) is particularly useful for modelling correlated data.
# magic(formula...) is Stable Multiple Smoothing Parameter Estimation by GCV or UBRE.


###############TESTS##############




#### EVALUACIÓN DE MODELOS PARA ESTE FILETE ####


summary(M0);summary(M1);summary(M2);summary(M3);summary(M4);summary(M5)

#M0 GCV score: 0.012554 (df: 98.99)
#M1 GCV score: 0.002571 (df: 98.98)
#M2 GCV score: 0.077161 (df: 98.99)
#M3 GCV score: 0.17394 (df: 20.49//26.56)
#M4 GCV score: 0.079525 (df: 98.92)
#M5 GCV score: 0.07952356 (df: 98.19)


#Deviance explained M0 = 92.3%
#Deviance explained M1 = 92.6%
#Deviance explained M2 = 90.5%
#Deviance explained M3 = 78.1%
#Deviance explained M4 = 90.2%
#Deviance explained M5 = 90.2%


AIC(M0,M1,M2,M3,M4,M5) # the smaller the AIC, the better the fit.

#        df        AIC
# M0 100.25324 -11555.762
# M1 100.04051 -23456.936
# M2 100.18612 -13390.192
# M3  42.14888  -7167.698
# M4  99.29127 -13160.816
# M5  99.18902 -13160.935



##########################################################
###                  FILETE    Z=40                    ###
##########################################################



#Limpiamos data.frame
slice40$Var1=NULL
slice40$Var2=NULL
slice40


#Creamos variables dummy para un data.frame limpio y logico
x<-rep(1:79, each=95)
y<-seq(1:95)


#Unificamos valores de PET uno tras otro en fila india

detach(slice20)
attach(slice40)
pet<-c(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,
       `21`,`22`,`23`,`24`,`25`,`26`,`27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`,`38`,`39`,`40`,
       `41`,`42`,`43`,`44`,`45`,`46`,`47`,`48`,`49`,`50`,`51`,`52`,`53`,`54`,`55`,`56`,`57`,`58`,`59`,`60`,
       `61`,`62`,`63`,`64`,`65`,`66`,`67`,`68`,`69`,`70`,`71`,`72`,`73`,`74`,`75`,`76`,`77`,`78`,`79`)

#Nuevo data.frame:
slice40=data.frame(x,y,pet)
slice40



#### PROPUESTA DE MODELOS PARA ESTE FILETE ####


# M0: Modelo Gaussiano sin transformaciones


M0 <- gam(pet ~ s(x,y,k=100), data=slice40)

par(mfrow=c(2,2))
gam.check(M0,old.style = F) 

par(mfrow=c(1,2))
vis.gam(M0,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
vis.gam(M0,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1)

plot.gam(M0,residuals=T,rug=T,se=T,pers = F)
plot.gam(M0,residuals=T,rug=F,se=F,pers = F)


# M1: Modelo con datos transformados (siguiendo el ejemplo de Wood^1/4) ¿Por que transforma asi? Check in the book.

E <- residuals(M0)
FV <- fitted(M0)
lm(log(E^2) ~ log(FV)) ## log(FV)=0.6208

M1 <- gam(pet^0.25 ~ s(x,y,k=100),data=slice40) 

par(mfrow=c(2,2))
gam.check(M1,old.style = F) 

par(mfrow=c(1,2))
vis.gam(M1,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
vis.gam(M1,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1) 

plot.gam(M1,residuals=T,rug=T,se=T,pers = F)
plot.gam(M1,residuals=T,rug=F,se=F,pers = F)


# M2: fit with Gamma distribution (modelo de log-gamma) // Bivariate Smooth


M2 <- gam(pet~s(x,y,k=100),data=slice40,family = Gamma(link=log)) 

par(mfrow=c(2,2))
gam.check(M2,old.style = F) 

par(mfrow=c(1,2))
vis.gam(M2,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
vis.gam(M2,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1) 

plot.gam(M2,residuals=T,rug=T,se=T,pers = F)
plot.gam(M2,residuals=T,rug=F,se=F,pers = F)


# M3: GAM Model // Additive model


M3 <- gam(pet ~ s(x,k=30) + s(y,k=30), data=slice40, family = Gamma(link=log))

par(mfrow=c(2,2))
gam.check(M3,old.style = F) 

par(mfrow=c(1,2))
vis.gam(M3,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
vis.gam(M3,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1) 

plot.gam(M3,residuals=T,rug=T,se=T,pers = T)
plot.gam(M3,residuals=T,rug=F,se=F,pers = F)


# M4: Single Tensor Product Smooth


M4 <- gam(pet ~ te(x,y,k=10), data=slice40, family = Gamma(link=log))

par(mfrow=c(2,2))
gam.check(M4,old.style = F) 

par(mfrow=c(1,2))
vis.gam(M4,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
vis.gam(M4,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1) 

plot.gam(M4,residuals=T,rug=T,se=T,pers = F)
plot.gam(M4,residuals=T,rug=F,se=F,pers = F)


# M5: Additive + Interaction


M5 <- gam(pet ~ s(x,k=10,bs="cr") + s(y,k=10,bs="cr") + ti(x,y,k=10), data=slice40, family = Gamma(link=log))

par(mfrow=c(2,2))
gam.check(M5,old.style = F) 

par(mfrow=c(1,2))
vis.gam(M5,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
vis.gam(M5,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1) 

plot.gam(M5,residuals=T,rug=T,se=T,pers = F)
plot.gam(M5,residuals=T,rug=F,se=F,pers = F)



#### EVALUACIÓN DE MODELOS PARA ESTE FILETE ####


summary(M0);summary(M1);summary(M2);summary(M3);summary(M4);summary(M5)

#M0 GCV score: 0.023667 (df: 99)
#M1 GCV score: 0.0018881 (df: 95.4)
#M2 GCV score: 0.038649 (df: 99)
#M3 GCV score: 0.24639 (df: 24.67//26.53)
#M4 GCV score: 0.051292 (df: 98.95)
#M5 GCV score: 0.051289 (df: 80.936)


#Deviance explained M0 = 90.3%
#Deviance explained M1 = 95.4%
#Deviance explained M2 = 95%
#Deviance explained M3 = 67.8%
#Deviance explained M4 = 93.4%
#Deviance explained M5 = 93.4%


AIC(M0,M1,M2,M3,M4,M5) # the smaller the AIC, the better the fit.

#        df         AIC
# M0 100.67003  -6797.3061
# M1 100.47959 -25773.6621
# M2 100.57459 -13266.4394
# M3  46.23537    893.6895
# M4  99.65969 -11126.9931
# M5  99.31374 -11127.3417



##########################################################
###                  FILETE    Z=60                    ###
##########################################################







##########################################################
###                  FILETE    Z=79                    ###
##########################################################