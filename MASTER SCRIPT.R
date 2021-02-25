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
negatives=img_data[img_data<0] # de hecho hay muchos valores inferiores a 0 por lo que lo más rápido será, en la database, igualarlos a 0 (MRIcro los identifica como 0's d ehecho)
a=img_data


#Diferentes coordenadas de la imagen con valores negativos (comprobacion)
img_data[1,2,3]
img_003_S_1059[1,1,1]
img_003_S_1059[10,30,20]
img_data[66,34,24]



##########################################################
###             f.clean  (PPT,z,x,y,pet)               ###
##########################################################

## Set as working directory -> directory with .hdr/.img NIfTI files and Demographics

f.clean <- function(name) { #### f.clean is meant for CLEANING ONE SINGLE PPT DATA
  
  
  # Read the NIFTI image, transform it to dataframe, preserve slice Z and organize the table
  
  ## Load Data
  
  file <- readNIfTI(fname = name, verbose = FALSE, warn = -1, reorient = TRUE, call = NULL, read_data = TRUE)
  namex <- as.character(name)
  n = img_data(file)
  n = to.data.frame(n)
  
  ## Prepare data.frame base where surther data from the loop will be integrated
  
  dataframe <- data.frame(z=integer(),x=integer(),y=integer(),pet=integer()) 
  
  # Loop for 79 slices of Z in the NiFtI image -> move to dataframe
  
  for (i in seq(1:79)) {
    
    n_lim = n[n$Var2==i,] # Select just one Z slice
    n_lim$Var1=NULL
    n_lim$Var2=NULL
    
    z <-rep(i,length.out=7505)
    x <-rep(1:79, each=95, length.out = 7505) 
    y <-rep(1:95,length.out = 7505)
    attach(n_lim)
    pet<-c(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,
           `21`,`22`,`23`,`24`,`25`,`26`,`27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`,`38`,`39`,`40`,
           `41`,`42`,`43`,`44`,`45`,`46`,`47`,`48`,`49`,`50`,`51`,`52`,`53`,`54`,`55`,`56`,`57`,`58`,`59`,`60`,
           `61`,`62`,`63`,`64`,`65`,`66`,`67`,`68`,`69`,`70`,`71`,`72`,`73`,`74`,`75`,`76`,`77`,`78`,`79`)
    detach(n_lim)
    
    temp0 = data.frame(z,x,y,pet) # temporal dataframe
    temp1 <- print(temp0) # is this necessary?
    dataframe <- rbind(dataframe,temp1) # sum new data with previous data
    
  }
  
  #Demographics: PPT, group (AD/CN), sex, age.
  
  demo <- read.csv2("Demographics.csv")
  demo <- demo[demo$PPT==namex,]
  
  PPT <- rep(demo$PPT,length.out=7505)
  group <-rep(demo$Group,length.out=7505)
  sex <-rep(demo$Sex,length.out=7505)
  age <-rep(demo$Age,length.out=7505)
  
  temp2 <- data.frame(PPT,group,sex,age)
  dataframe <- cbind(temp2,dataframe)
  
  print(dataframe) # Necessary for asigning an object name
  
  
}


#Example(s):
"003_S_1059" = f.clean("003_S_1059")
"005_S_0221" = f.clean("005_S_0221")



##########################################################
###               COMPLETE   DATABASE                  ###
##########################################################



files <- list.files(path="D:/Usuario/Desktop/MegaSync/PhD/Wood Example/Brain Imaging Example (Simon Wood)/PETimg", 
                    pattern="*.img", full.names=F, recursive=FALSE) # list of files

files <- gsub(files, pattern=".img$", replacement="") # remove file extension .img


database <- data.frame(PPT=integer(),group=integer(),sex=integer(),age=integer(),z=integer(),x=integer(),y=integer(),pet=integer())
#create data.frame to include data


for (i in 1:length(files)) { #loop to include every PPT in the dataframe
  
  temporal <- f.clean(files[i])
  database <- rbind(database,temporal)
  
}

database$pet[database$pet=0] <- NaN # IMPORTANTE: Los valores de PET inferiores a 0 se van a igualar a cero (errores post-normalizacion)


write.csv2(database,file="Database.csv",sep=";",na="NA") #export as .csv IF DESIRABLE



##########################################################
###               MODELOS PROPUESTOS                   ###
##########################################################


ppt_005_S_0221 <- database[database$PPT=="005_S_0221",] 
ppt_005_S_0221$pet[which(ppt_005_S_0221$pet==0)] = NA
ppt_005_S_0221_S30 <- ppt_005_S_0221[ppt_005_S_0221$z==30,]
ppt_005_S_0221_S30$pet[which(ppt_005_S_0221_S30$pet<0,)] = NA


attach(`ppt_005_S_0221_S30`)


M0 <- gam(pet ~ s(x,y,k=50), data=ppt_005_S_0221_S30)
    
    par(mfrow=c(2,2))
    gam.check(M0,old.style = F)
    
    par(mfrow=c(1,2))
    vis.gam(M0,plot.type = "contour",too.far=0.03,
            color="terrain",n.grid=60,zlim=c(-1,2),se=0) 
    vis.gam(M0,plot.type = "contour",too.far=0.03,
            color="terrain",n.grid=60,zlim=c(-1,2),se=1)
    

M1 <- gam(pet ~ s(x,k=50) + s(y,k=50), data=ppt_005_S_0221_S30)
    
    par(mfrow=c(2,2))
    gam.check(M1,old.style = F)
    
    par(mfrow=c(1,2))
    vis.gam(M1,plot.type = "contour",too.far=0.03,
            color="terrain",n.grid=60,zlim=c(-1,2),se=0,main="M1") 
    vis.gam(M1,plot.type = "persp",too.far=0.03,
            color="terrain",n.grid=60,zlim=c(-1,2),se=0,main="gam(pet ~ s(x,k=50) + s(y,k=50)",theta=0,phi=60) 
    
                                
M2 <- gam(pet ~ s(x,k=50) + s(y,k=50) + te(x,y,k=50), data=ppt_005_S_0221_S30)   # BUEN AJUSTE AQUÍ EL M2
    
    par(mfrow=c(2,2))
    gam.check(M2,old.style = F)
    
    par(mfrow=c(1,2))
    vis.gam(M2,plot.type = "contour",too.far=0.03,
            color="terrain",n.grid=60,zlim=c(-1,2),se=0,main="M2") 
    vis.gam(M2,plot.type = "persp",too.far=0.03,
            color="terrain",n.grid=60,zlim=c(-1,2),se=0,main="gam(pet ~ s(x,k=50) + s(y,k=50) + te(x,y,k=50)",theta=0,phi=60)     
    
    
M3 <- gam(pet ~ s(x,k=50,bs="ad") + s(y,k=50,bs="ad") + s(x,y,k=50,bs="ad"), data=ppt_005_S_0221_S30)

    par(mfrow=c(2,2))
    gam.check(M3,old.style = F)

    par(mfrow=c(1,2))
    vis.gam(M3,plot.type = "contour",too.far=0.03,
            color="terrain",n.grid=60,zlim=c(-1,2),se=0,main="M3")
    vis.gam(M3,plot.type = "persp",too.far=0.03,
            color="terrain",n.grid=60,zlim=c(-1,2),se=0,main="gam(pet ~ s(x,k=50,bs=ad) + s(y,k=50,bs=ad) + s(x,y,k=50,bs=ad)",theta=0,phi=60)

    
M4 <- gam(pet ~ s(x,k=50,bs="cr") + s(y,k=50,bs="cr") + te(x,y,k=50,bs="cr"), data=ppt_005_S_0221_S30) #Bastante similar a M2
    
    par(mfrow=c(2,2))
    gam.check(M4,old.style = F)
    
    par(mfrow=c(1,2))
    vis.gam(M4,plot.type = "contour",too.far=0.03,
            color="terrain",n.grid=60,zlim=c(-1,2),se=0, main = "M4") 
    vis.gam(M4,plot.type = "persp",too.far=0.03,
            color="terrain",n.grid=60,zlim=c(-1,2),se=0, main="gam(pet ~ s(x,k=50,bs=cr) + s(y,k=50,bs=cr) + te(x,y,k=50,bs=cr)",theta=0,phi=60)    
    
    
    
M5 <- gam(pet ~ s(x,k=50,bs="re") + s(y,k=50,bs="re") + te(x,y,k=50,bs="re"), data=ppt_005_S_0221_S30) # Horrible xD
    
    par(mfrow=c(2,2))
    gam.check(M5,old.style = F)
    
    par(mfrow=c(1,2))
    vis.gam(M5,plot.type = "contour",too.far=0.03,
            color="terrain",n.grid=60,zlim=c(-1,2),se=0, main="M5") 
    vis.gam(M5,plot.type = "persp",too.far=0.03,
            color="terrain",n.grid=60,zlim=c(-1,2),se=0)   
  

          
M6 <- bam(pet ~ s(x,k=50,bs="ad") + s(y,k=50,bs="ad") + ti(x,y,k=50), data=ppt_005_S_0221_S30, family = Gamma(link=log))

    par(mfrow=c(2,2))
    gam.check(M6,old.style = F)
    
    par(mfrow=c(1,2))
    vis.gam(M6,plot.type = "contour",too.far=0.03,
            color="terrain",n.grid=60,zlim=c(-1,2),se=0, main = "M6") 
    vis.gam(M6,plot.type = "persp",too.far=0.03,
            color="terrain",n.grid=60,zlim=c(-1,2),se=0, main="bam(pet ~ s(x,k=10,bs=ad) + s(y,k=10,bs=ad) + ti(x,y,k=10)",theta=0,phi=60)    
    
    

#############################################################
#               FUNCIÓN   GAM'S     PPT                     #
#############################################################


GAM <- function(PPT,Z){

# Preámbulo:

start_time <- Sys.time()

ppt <- database[database$PPT==PPT,]   # Choose PPT
ppt <- ppt[ppt$z==Z,]                 # Choose Z level
ppt$pet[which(ppt$pet<0)] = NA        # Clean negatives
attach(ppt)

# Modelo:

model <- gam(pet ~ s(x,k=30) + s(y,k=30) + te(x,y,k=30), data=ppt)   ## este es el modelo real, el de abajo solo lo estoy usando para testear

#model <- gam(pet ~ s(x,k=40) + s(y,k=40), data=ppt)   

detach(ppt)

end_time <- Sys.time()
necc.time <<- end_time - start_time
show(necc.time)  # Calculador del tiempo que lleva obtener el Modelo propuesto

return(model)

}
  
      
## Testeo de tiempos según K:
    
K5_test_003_S_1059 <- GAM("003_S_1059",40) 
# 0.63412 secs
K10_test_003_S_1059 <- GAM("003_S_1059",40) 
# 1.280386 secs
K20_test_003_S_1059 <- GAM("003_S_1059",40) 
# 15.33445 secs
K30_test_003_S_1059 <- GAM("003_S_1059",40) 
# 2.68752 mins 


dias_aprox = ((2.68752*79)/60 * 130)/24  # Un slice = 2.68min; un cerebro = 3.53h; 130 pacientes = 19 días SIN PARALELIZAR.

Wang <- GAM("003_S_1059",35)

#############################################################
#               FUNCIÓN   VIS.GAM     PPT                   #
#############################################################


VIS.GAM <- function(model){
  
  par(mfrow=c(2,2))
  gam.check(model,old.style = F)
  
  par(mfrow=c(1,1))
  vis.gam(model,plot.type = "contour",too.far=0.03,
          color="terrain",n.grid=60,zlim=c(-1,2),se=0,main="model") 
  vis.gam(model,plot.type = "persp",too.far=0.03,
          color="terrain",n.grid=60,zlim=c(-1,2),se=1,main="VIS.GAM",theta=0,phi=60)
 
}

# Ejemplos:

VIS.GAM(M.009.S.0862.40)
VIS.GAM(M.009.S.0862.55)
VIS.GAM(M.009.S.0862.65)
VIS.GAM(M.009.S.0862.70)

Wang.vis<-vis.gam(Wang,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=60,zlim=c(-1,2),se=0,main="Sample Visualization: PPT=003_S_1059, Z=30") 

# Ejemplos diferentes K's:

system.time(VIS.GAM(K5_test_003_S_1059)) # elapsed 2.05
system.time(VIS.GAM(K10_test_003_S_1059)) # elapsed 2.12
system.time(VIS.GAM(K20_test_003_S_1059)) # elapsed 2.91
system.time(VIS.GAM(K30_test_003_S_1059)) # elapsed 5.13 




#############################################################
#                FUNCIÓN   GAM'S + VIS                      #
#############################################################


  # 3 ALZHEIMER DISEASE:


M.041.S.1391 <- GAM("041_S_1391",30) # 2.838412 mins
system.time(VIS.GAM(M.041.S.1391)) # elapsed 5.29 

M.041.S.1368 <- GAM("041_S_1368",30) # 2.587263 mins
system.time(VIS.GAM(M.041.S.1368)) # elapsed 5.51  

M.037.S.0627 <- GAM("037_S_0627",30) # 2.62189 mins
system.time(VIS.GAM(M.037.S.0627)) # elapsed 4.95 


  # 3 CONTROL GROUP:
 
 
M.130.S.0232 <- GAM("130_S_0232",30) # 3.160643 mins
system.time(VIS.GAM(M.130.S.0232)) # elapsed 5.10 

M.129.S.0778 <- GAM("129_S_0778",30) # 2.44551 mins
system.time(VIS.GAM(M.129.S.0778)) # elapsed 5.03 

M.128.S.0863 <- GAM("128_S_0863",30) # 2.640595 mins
system.time(VIS.GAM(M.128.S.0863)) # elapsed 4.99 
 


par(mfrow=c(1,1))
vis.gam(M.128.S.0863,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=80,zlim=c(-1,2),se=0,main="heat",theta=0,phi=60) 



###########################################################
#                          mgcViz                         #
###########################################################

# mgcViz

install.packages("mgcViz")
library(mgcViz)

test30=subset(ppt_005_S_0221,ppt_005_S_0221$z==30);test30
attach(test30)

b <- gam(pet ~ s(x) + s(y) + z, data = test30, method = "REML")

b <- getViz(M666_single)

o <- plot( sm(b, 1) ); o
o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()


listLayers(o)

print(plot(b, allTerms = T), pages = 1)


pl <- plotSlice(x = sm(b, 1), 
                fix = list("z" = seq(-2, 2, length.out = 3), "x" = c(-1, 0, 1)))
pl + l_fitRaster() + l_fitContour() + l_points() + l_rug()



## FIN mgcViz ##



