##########################################################
###              NECESSARY   PACKAGES                  ###
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


#Lectura de la imagen NIFTI y muestra de datos generales

img_003_S_1059 <- readNIfTI("003_S_1059", verbose = FALSE, warn = -1, reorient = TRUE,
                 call = NULL, read_data = TRUE); img_003_S_1059


#Visualizacion de la imagen completa

image(img_003_S_1059) #axial por defecto
image(img_003_S_1059,plane="coronal") #coronal
image(img_003_S_1059,plane="sagittal") #sagital
orthographic(img_003_S_1059,text="Plano Ortografico PET") #plano ortografico


#Datos asociados a la imagen

img_data=img_data(img_003_S_1059)
hist(img_data) #Histograma mostrando que hay valores de PET inferiores a 0, lo cual es ilogico

