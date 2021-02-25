##Useful urls: 

  #https://cran.csiro.au/web/packages/neurobase/vignettes/neurobase.html
  #https://www.r-bloggers.com/working-with-nifti-images-in-r/

## install.packages("tractor.base")
## library(tractor.base)  ##DICOM?? No abre las nifti ni las analyze que yo vea

## install.packages("neuroim")
## library(neuroim)


install.packages("oro.nifti")
library(oro.nifti)


img <- readNIfTI("PETdataEXAMPLE", verbose = FALSE, warn = -1, reorient = TRUE,
        call = NULL, read_data = TRUE)
# Con este comando se pueden leer las imagenes en formato NIfTI

img #muestra datos generales sobre la imagen

image(img)
#plot de la imagen completa; plano axial por defecto; otras opciones:

image(img,plane="coronal") #coronal
image(img,plane="sagittal") #sagital


orthographic(img) #plano ortográfico


image(img,z=40,plot.type="single")
#Visualización de la neuro-imagen; z = Valores a plotear (altura de la imagen = 79)
#En MRIcro hay correspondencia con lo que se ve con oro.nifti, se ve al comparar
#la imagen en diferentes valores de Z.


#Info del NIfTI:
data = img_data(img)
class(data)
dim(data)
print(data)
hist(data)

## DATOS ASOCIADOS A LA IMAGEN ##

slotNames(img) #Nombre de las variables
dim(data)
imgd <- img_data(img);imgd

#Diferentes coordenadas de la imagen
img[1,1,40]
img[35,1,40]
img[35,45,40]
img[35,80,40]
img[35,40,45]
img[35,40,45]


# Para transformar los datos de la imagen (img@.Data) en un data frame
# Package: "memsic" from -> http://www.martin-elff.net/knitr/memisc/toDataFrame.html

install.packages("memsic")
library(memisc)
idata = img@.Data
idata = to.data.frame(idata)

dim(idata);dim(img) 

# 79*95=7505, se ve que cambia de dimensiones a una bidimensional en que una de las variables es 
# la propia coordenada Z. Al ganar dos columnas (variables Y y Z [o X y Z?]) también una dimension 
# pasa de 79 a 81

# Slice d eun NifTi para poder seleccionar en función de la Z, no como con el data.frame
# X=79dim,ejeX ; Y=95dim,ejeY

slice3 = img[,,3]
image(slice3,xlab="X",ylab="Y",main="Slice at Z=3")

slice15 = img[,,15]
image(slice15,xlab="X",ylab="Y",main="Slice at Z=15")

slice40 = img[,,40]
image(slice40,xlab="X",ylab="Y",main="Slice at Z=40")


par(mfrow=c(1,3))
image(slice3,main="Slice 3")
image(slice15,main="Slice 15")
image(slice40,main="Slice 40")

###########################################

##FILETEADO

test.slice <- idata[idata$Var2==40,]
test.slice$Var2=NULL
test.slice$Var1=NULL
test.slice

###########################################

#Dataframe con las coordenadas X e Y y los valores de actividad cerebral en fMRI todos tabulados

x<-rep(1:79, each=95)
y<-seq(1:95)
attach(test.slice)
fmri<-c(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,
        `21`,`22`,`23`,`24`,`25`,`26`,`27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`,`38`,`39`,`40`,
        `41`,`42`,`43`,`44`,`45`,`46`,`47`,`48`,`49`,`50`,`51`,`52`,`53`,`54`,`55`,`56`,`57`,`58`,`59`,`60`,
        `61`,`62`,`63`,`64`,`65`,`66`,`67`,`68`,`69`,`70`,`71`,`72`,`73`,`74`,`75`,`76`,`77`,`78`,`79`)

neurodata1=data.frame(x,y,fmri)


### So now what? GAM model ###

library(mgcv)
library(gamair)
attach(neurodata1)


M0 <- gam(fmri ~ s(x,y,k=100), data=neurodata1)

par(mfrow=c(2,2))

gam.check(M0,old.style = F) 

par(mfrow=c(1,2))

vis.gam(M0,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
vis.gam(M0,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1)

#Standard Error // se=0 da un mapa estándar, se=1 o mayor, da una superposición en 3D
#n.grid=60: superior, aumenta el tiempo de computación dando lugar a resultados accurate, 
#inferior tarda menos pero da peores resultados


plot.gam(M0,residuals=T,rug=T,se=T,pers = F)

##

E <- residuals(M0)
FV <- fitted(M0)
lm(log(E^2) ~ log(FV))

##

M1 <- gam(fmri^0.25 ~ s(x,y,k=100),data=neurodata1) # M1 -> modelo de datos transformados

par(mfrow=c(2,2))
gam.check(M1,old.style = F) #Mucho mejor ajuste

par(mfrow=c(1,2))
vis.gam(M1,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) #Peor prediccion?
vis.gam(M1,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1) #Peor prediccion?

##

M2 <- gam(fmri~s(x,y,k=100),data=neurodata1,family = Gamma(link=log)) # m2 -> modelo de log-gamma

par(mfrow=c(2,2))
gam.check(M2,old.style = F) #Mal ajuste de residuos

par(mfrow=c(1,2))
vis.gam(M2,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0) #Muy mal ajuste de residuos
vis.gam(M2,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=1) #Muy mal ajuste de residuos

##

mean(fitted(M1)^4); mean(fitted(M2)); mean(fmri)

#[1] 0.1929687 // [1] 0.1957526 // [1] 0.1953251

##


###################################################
##       SERÍA MEJOR UNA ESTRUCTURA ADITIVA?     ##
###################################################


M3 <- gam(fmri ~ s(y,k=30) + s(x,k=30), data=neurodata1, family = Gamma(link=log))

M1;M2;M3

#GCV=Generalized Cros-Validation
#M1 GCV score: 0.001013106 
#M2 GCV score: 0.05836896 
#M3 GCV score: 0.3202334
#GCV es mayor en el modelo 3, lo que indica que no hay mejoría

summary(M1);summary(M2);summary(M3)
#Deviance explained M1 = 94.8%
#Deviance explained M2 = 92%
#Deviance explained M3 = 55.5%
#La variabilidad explicada por los modelos tampoco supportea la idea de usar el m3

AIC(M1,M2,M3) #Akaike information criteria; se ofrece una estimación relativa de la información perdida 
#cuando se utiliza un modelo determinado para representar el proceso que genera los datos.

#       df       AIC
#M1 100.2496 -30446.04
#M2 100.5109 -29809.82
#M3  35.8197 -16708.67

# Se pierde más información con el m3 (modelo aditivo) ¿?¿? o no? !!

vis.gam(M3,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),main="m3 - Modelo aditivo")



###################################################
##      ISOTROPIC OR TENSOR PRODUCT SMOOTHS?     ##
###################################################


# SINGLE TENSOR PRODUCT SMOOTH:

TM <- gam(fmri ~ te(y,x,k=10), data=neurodata1, family = Gamma(link=log))

vis.gam(TM,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),main="TM - Single tensor smooth")

# ADDITIVE + INTERACTION MODEL:

TM1 <- gam(fmri ~ s(x,k=10,bs="cr") + s(y,k=10,bs="cr") + ti(x,y,k=10), data=neurodata1, family = Gamma(link=log)) # bs="cr" - These have a cubic spline basis

vis.gam(TM1,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),main="TM1 - Additive + interaction")


par(mfrow=c(2,2))
gam.check(TM)
gam.check(TM1)

AIC(TM,TM1) #AIC sgue sin apoyar el nuevo modelo. Parece que es mejor el tm1 que el tm, pero siguen siendo 
# ambos peores que el m2 isotrópico

#       df       AIC
#TM  100.2810 -29655.87
#TM1 100.1987 -29655.96


# ¿Estructura de modelo aditivo vs bivariate? Examinar tm1 con summary/ANOVA #

anova(TM1)

