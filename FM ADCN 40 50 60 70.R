###### PRUEBAS CON DISTINTOS GRUPOS, EDADES, Y SEXOS ######


##### Paquetes ##### 

#install.packages("mgcv")
#install.packages("gamair")
#install.packages("oro.nifti")
#install.packages("memsic")

library(mgcv)
library(gamair)
library(oro.nifti)
library(memisc)


##### Imagenes Origen ##### 


AD_F_59 <- readNIfTI("AD_F_59", verbose = FALSE, warn = -1, reorient = TRUE,
                 call = NULL, read_data = TRUE)
AD_F_71 <- readNIfTI("AD_F_71", verbose = FALSE, warn = -1, reorient = TRUE,
                     call = NULL, read_data = TRUE)
AD_F_86 <- readNIfTI("AD_F_86", verbose = FALSE, warn = -1, reorient = TRUE,
                     call = NULL, read_data = TRUE)
AD_M_60 <- readNIfTI("AD_M_60", verbose = FALSE, warn = -1, reorient = TRUE,
                     call = NULL, read_data = TRUE)
AD_M_72 <- readNIfTI("AD_M_72", verbose = FALSE, warn = -1, reorient = TRUE,
                     call = NULL, read_data = TRUE)
AD_M_85 <- readNIfTI("AD_M_85", verbose = FALSE, warn = -1, reorient = TRUE,
                     call = NULL, read_data = TRUE)
CN_F_62 <- readNIfTI("CN_F_62", verbose = FALSE, warn = -1, reorient = TRUE,
                     call = NULL, read_data = TRUE)
CN_F_71 <- readNIfTI("CN_F_71", verbose = FALSE, warn = -1, reorient = TRUE,
                     call = NULL, read_data = TRUE)
CN_F_85 <- readNIfTI("CN_F_85", verbose = FALSE, warn = -1, reorient = TRUE,
                     call = NULL, read_data = TRUE)
CN_M_63 <- readNIfTI("CN_M_63", verbose = FALSE, warn = -1, reorient = TRUE,
                     call = NULL, read_data = TRUE)
CN_M_71 <- readNIfTI("CN_M_71", verbose = FALSE, warn = -1, reorient = TRUE,
                     call = NULL, read_data = TRUE)
CN_M_85 <- readNIfTI("CN_M_85", verbose = FALSE, warn = -1, reorient = TRUE,
                     call = NULL, read_data = TRUE)


##### Data.frame Conversion ##### 


AD_F_59 = img_data(AD_F_59)
AD_F_59 = to.data.frame(AD_F_59)
AD_F_59 = AD_F_59[AD_F_59$Var2==30,] #Z=30 --> Intentando ver zonas parietales e hipocampo.
AD_F_59$Var1=NULL
AD_F_59$Var2=NULL
x<-rep(1:79, each=95)
y<-seq(1:95)
attach(AD_F_59)
pet<-c(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,
       `21`,`22`,`23`,`24`,`25`,`26`,`27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`,`38`,`39`,`40`,
       `41`,`42`,`43`,`44`,`45`,`46`,`47`,`48`,`49`,`50`,`51`,`52`,`53`,`54`,`55`,`56`,`57`,`58`,`59`,`60`,
       `61`,`62`,`63`,`64`,`65`,`66`,`67`,`68`,`69`,`70`,`71`,`72`,`73`,`74`,`75`,`76`,`77`,`78`,`79`)
AD_F_59=data.frame(x,y,pet)
detach(AD_F_59)

##

AD_F_71 = img_data(AD_F_71)
AD_F_71 = to.data.frame(AD_F_71)
AD_F_71 = AD_F_71[AD_F_71$Var2==30,] #Z=30 --> Intentando ver zonas parietales e hipocampo.
AD_F_71$Var1=NULL
AD_F_71$Var2=NULL
x<-rep(1:79, each=95)
y<-seq(1:95)
attach(AD_F_71)
pet<-c(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,
       `21`,`22`,`23`,`24`,`25`,`26`,`27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`,`38`,`39`,`40`,
       `41`,`42`,`43`,`44`,`45`,`46`,`47`,`48`,`49`,`50`,`51`,`52`,`53`,`54`,`55`,`56`,`57`,`58`,`59`,`60`,
       `61`,`62`,`63`,`64`,`65`,`66`,`67`,`68`,`69`,`70`,`71`,`72`,`73`,`74`,`75`,`76`,`77`,`78`,`79`)
AD_F_71=data.frame(x,y,pet)
detach(AD_F_71)

##

AD_F_86 = img_data(AD_F_86)
AD_F_86 = to.data.frame(AD_F_86)
AD_F_86 = AD_F_86[AD_F_86$Var2==30,] #Z=30 --> Intentando ver zonas parietales e hipocampo.
AD_F_86$Var1=NULL
AD_F_86$Var2=NULL
x<-rep(1:79, each=95)
y<-seq(1:95)
attach(AD_F_86)
pet<-c(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,
       `21`,`22`,`23`,`24`,`25`,`26`,`27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`,`38`,`39`,`40`,
       `41`,`42`,`43`,`44`,`45`,`46`,`47`,`48`,`49`,`50`,`51`,`52`,`53`,`54`,`55`,`56`,`57`,`58`,`59`,`60`,
       `61`,`62`,`63`,`64`,`65`,`66`,`67`,`68`,`69`,`70`,`71`,`72`,`73`,`74`,`75`,`76`,`77`,`78`,`79`)
AD_F_86=data.frame(x,y,pet)
detach(AD_F_86)

##

AD_M_60 = img_data(AD_M_60)
AD_M_60 = to.data.frame(AD_M_60)
AD_M_60 = AD_M_60[AD_M_60$Var2==30,] #Z=30 --> Intentando ver zonas parietales e hipocampo.
AD_M_60$Var1=NULL
AD_M_60$Var2=NULL
x<-rep(1:79, each=95)
y<-seq(1:95)
attach(AD_M_60)
pet<-c(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,
       `21`,`22`,`23`,`24`,`25`,`26`,`27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`,`38`,`39`,`40`,
       `41`,`42`,`43`,`44`,`45`,`46`,`47`,`48`,`49`,`50`,`51`,`52`,`53`,`54`,`55`,`56`,`57`,`58`,`59`,`60`,
       `61`,`62`,`63`,`64`,`65`,`66`,`67`,`68`,`69`,`70`,`71`,`72`,`73`,`74`,`75`,`76`,`77`,`78`,`79`)
AD_M_60=data.frame(x,y,pet)
detach(AD_M_60)

##

AD_M_72 = img_data(AD_M_72)
AD_M_72 = to.data.frame(AD_M_72)
AD_M_72 = AD_M_72[AD_M_72$Var2==30,] #Z=30 --> Intentando ver zonas parietales e hipocampo.
AD_M_72$Var1=NULL
AD_M_72$Var2=NULL
x<-rep(1:79, each=95)
y<-seq(1:95)
attach(AD_M_72)
pet<-c(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,
       `21`,`22`,`23`,`24`,`25`,`26`,`27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`,`38`,`39`,`40`,
       `41`,`42`,`43`,`44`,`45`,`46`,`47`,`48`,`49`,`50`,`51`,`52`,`53`,`54`,`55`,`56`,`57`,`58`,`59`,`60`,
       `61`,`62`,`63`,`64`,`65`,`66`,`67`,`68`,`69`,`70`,`71`,`72`,`73`,`74`,`75`,`76`,`77`,`78`,`79`)
AD_M_72=data.frame(x,y,pet)
detach(AD_M_72)

##

AD_M_85 = img_data(AD_M_85)
AD_M_85 = to.data.frame(AD_M_85)
AD_M_85 = AD_M_85[AD_M_85$Var2==30,] #Z=30 --> Intentando ver zonas parietales e hipocampo.
AD_M_85$Var1=NULL
AD_M_85$Var2=NULL
x<-rep(1:79, each=95)
y<-seq(1:95)
attach(AD_M_85)
pet<-c(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,
       `21`,`22`,`23`,`24`,`25`,`26`,`27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`,`38`,`39`,`40`,
       `41`,`42`,`43`,`44`,`45`,`46`,`47`,`48`,`49`,`50`,`51`,`52`,`53`,`54`,`55`,`56`,`57`,`58`,`59`,`60`,
       `61`,`62`,`63`,`64`,`65`,`66`,`67`,`68`,`69`,`70`,`71`,`72`,`73`,`74`,`75`,`76`,`77`,`78`,`79`)
AD_M_85=data.frame(x,y,pet)
detach(AD_M_85)

##

CN_F_62 = img_data(CN_F_62)
CN_F_62 = to.data.frame(CN_F_62)
CN_F_62 = CN_F_62[CN_F_62$Var2==30,] #Z=30 --> Intentando ver zonas parietales e hipocampo.
CN_F_62$Var1=NULL
CN_F_62$Var2=NULL
x<-rep(1:79, each=95)
y<-seq(1:95)
attach(CN_F_62)
pet<-c(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,
       `21`,`22`,`23`,`24`,`25`,`26`,`27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`,`38`,`39`,`40`,
       `41`,`42`,`43`,`44`,`45`,`46`,`47`,`48`,`49`,`50`,`51`,`52`,`53`,`54`,`55`,`56`,`57`,`58`,`59`,`60`,
       `61`,`62`,`63`,`64`,`65`,`66`,`67`,`68`,`69`,`70`,`71`,`72`,`73`,`74`,`75`,`76`,`77`,`78`,`79`)
CN_F_62=data.frame(x,y,pet)
detach(CN_F_62)

##

CN_F_71 = img_data(CN_F_71)
CN_F_71 = to.data.frame(CN_F_71)
CN_F_71 = CN_F_71[CN_F_71$Var2==30,] #Z=30 --> Intentando ver zonas parietales e hipocampo.
CN_F_71$Var1=NULL
CN_F_71$Var2=NULL
x<-rep(1:79, each=95)
y<-seq(1:95)
attach(CN_F_71)
pet<-c(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,
       `21`,`22`,`23`,`24`,`25`,`26`,`27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`,`38`,`39`,`40`,
       `41`,`42`,`43`,`44`,`45`,`46`,`47`,`48`,`49`,`50`,`51`,`52`,`53`,`54`,`55`,`56`,`57`,`58`,`59`,`60`,
       `61`,`62`,`63`,`64`,`65`,`66`,`67`,`68`,`69`,`70`,`71`,`72`,`73`,`74`,`75`,`76`,`77`,`78`,`79`)
CN_F_71=data.frame(x,y,pet)
detach(CN_F_71)

##

CN_F_85 = img_data(CN_F_85)
CN_F_85 = to.data.frame(CN_F_85)
CN_F_85 = CN_F_85[CN_F_85$Var2==30,] #Z=30 --> Intentando ver zonas parietales e hipocampo.
CN_F_85$Var1=NULL
CN_F_85$Var2=NULL
x<-rep(1:79, each=95)
y<-seq(1:95)
attach(CN_F_85)
pet<-c(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,
       `21`,`22`,`23`,`24`,`25`,`26`,`27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`,`38`,`39`,`40`,
       `41`,`42`,`43`,`44`,`45`,`46`,`47`,`48`,`49`,`50`,`51`,`52`,`53`,`54`,`55`,`56`,`57`,`58`,`59`,`60`,
       `61`,`62`,`63`,`64`,`65`,`66`,`67`,`68`,`69`,`70`,`71`,`72`,`73`,`74`,`75`,`76`,`77`,`78`,`79`)
CN_F_85=data.frame(x,y,pet)
detach(CN_F_85)

##


CN_M_63 = img_data(CN_M_63)
CN_M_63 = to.data.frame(CN_M_63)
CN_M_63 = CN_M_63[CN_M_63$Var2==30,] #Z=30 --> Intentando ver zonas parietales e hipocampo.
CN_M_63$Var1=NULL
CN_M_63$Var2=NULL
x<-rep(1:79, each=95)
y<-seq(1:95)
attach(CN_M_63)
pet<-c(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,
       `21`,`22`,`23`,`24`,`25`,`26`,`27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`,`38`,`39`,`40`,
       `41`,`42`,`43`,`44`,`45`,`46`,`47`,`48`,`49`,`50`,`51`,`52`,`53`,`54`,`55`,`56`,`57`,`58`,`59`,`60`,
       `61`,`62`,`63`,`64`,`65`,`66`,`67`,`68`,`69`,`70`,`71`,`72`,`73`,`74`,`75`,`76`,`77`,`78`,`79`)
CN_M_63=data.frame(x,y,pet)
detach(CN_M_63)

##

CN_M_71 = img_data(CN_M_71)
CN_M_71 = to.data.frame(CN_M_71)
CN_M_71 = CN_M_71[CN_M_71$Var2==30,] #Z=30 --> Intentando ver zonas parietales e hipocampo.
CN_M_71$Var1=NULL
CN_M_71$Var2=NULL
x<-rep(1:79, each=95)
y<-seq(1:95)
attach(CN_M_71)
pet<-c(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,
       `21`,`22`,`23`,`24`,`25`,`26`,`27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`,`38`,`39`,`40`,
       `41`,`42`,`43`,`44`,`45`,`46`,`47`,`48`,`49`,`50`,`51`,`52`,`53`,`54`,`55`,`56`,`57`,`58`,`59`,`60`,
       `61`,`62`,`63`,`64`,`65`,`66`,`67`,`68`,`69`,`70`,`71`,`72`,`73`,`74`,`75`,`76`,`77`,`78`,`79`)
CN_M_71=data.frame(x,y,pet)
detach(CN_M_71)

##

CN_M_85 = img_data(CN_M_85)
CN_M_85 = to.data.frame(CN_M_85)
CN_M_85 = CN_M_85[CN_M_85$Var2==30,] #Z=30 --> Intentando ver zonas parietales e hipocampo.
CN_M_85$Var1=NULL
CN_M_85$Var2=NULL
x<-rep(1:79, each=95)
y<-seq(1:95)
attach(CN_M_85)
pet<-c(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,
       `21`,`22`,`23`,`24`,`25`,`26`,`27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`,`38`,`39`,`40`,
       `41`,`42`,`43`,`44`,`45`,`46`,`47`,`48`,`49`,`50`,`51`,`52`,`53`,`54`,`55`,`56`,`57`,`58`,`59`,`60`,
       `61`,`62`,`63`,`64`,`65`,`66`,`67`,`68`,`69`,`70`,`71`,`72`,`73`,`74`,`75`,`76`,`77`,`78`,`79`)
CN_M_85=data.frame(x,y,pet)
detach(CN_M_85)



##### Model Comparation ##### 

##### AD_F_59 #####

AD_F_59 = AD_F_59[AD_F_59$pet>0,] # valores PET superiores a 0, de otra forma no tiene sentido
M_AD_F_59 <- gam(pet ~ s(x,k=20,bs="ad") + s(y,k=20,bs="ad") + ti(x,y,k=20), data=AD_F_59, family = Gamma(link=log))
par(mfrow=c(2,2))
gam.check(M_AD_F_59,old.style = F) 
par(mfrow=c(1,2))
vis.gam(M_AD_F_59,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="AD_F_59") 


##### AD_F_71 #####

AD_F_71 = AD_F_71[AD_F_71$pet>0,] # valores PET superiores a 0, de otra forma no tiene sentido
M_AD_F_71 <- gam(pet ~ s(x,k=20,bs="ad") + s(y,k=20,bs="ad") + ti(x,y,k=20), data=AD_F_71, family = Gamma(link=log))
par(mfrow=c(2,2))
gam.check(M_AD_F_71,old.style = F)
par(mfrow=c(1,2))
vis.gam(M_AD_F_71,plot.type = "contour",too.far=0.03,
       color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="AD_F_71")


##### AD_F_86 #####

AD_F_86 = AD_F_86[AD_F_86$pet>0,] # valores PET superiores a 0, de otra forma no tiene sentido
M_AD_F_86 <- gam(pet ~ s(x,k=20,bs="ad") + s(y,k=20,bs="ad") + ti(x,y,k=20), data=AD_F_86, family = Gamma(link=log))
par(mfrow=c(2,2))
gam.check(M_AD_F_86,old.style = F)
par(mfrow=c(1,2))
vis.gam(M_AD_F_86,plot.type = "contour",too.far=0.03,
       color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="AD_F_86")


##### AD_M_60 #####

AD_M_60 = AD_M_60[AD_M_60$pet>0,] # valores PET superiores a 0, de otra forma no tiene sentido
M_AD_M_60 <- gam(pet ~ s(x,k=20,bs="ad") + s(y,k=20,bs="ad") + ti(x,y,k=20), data=AD_M_60, family = Gamma(link=log))
par(mfrow=c(2,2))
gam.check(M_AD_M_60,old.style = F)
par(mfrow=c(1,2))
vis.gam(M_AD_M_60,plot.type = "contour",too.far=0.03,
       color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="AD_M_60")


##### AD_M_72 #####

AD_M_72 = AD_M_72[AD_M_72$pet>0,] # valores PET superiores a 0, de otra forma no tiene sentido
M_AD_M_72 <- gam(pet ~ s(x,k=20,bs="ad") + s(y,k=20,bs="ad") + ti(x,y,k=20), data=AD_M_72, family = Gamma(link=log))
par(mfrow=c(2,2))
gam.check(M_AD_M_72,old.style = F)
par(mfrow=c(1,2))
vis.gam(M_AD_M_72,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="AD_M_72")


##### AD_M_85 #####

AD_M_85 = AD_M_85[AD_M_85$pet>0,] # valores PET superiores a 0, de otra forma no tiene sentido
M_AD_M_85 <- gam(pet ~ s(x,k=20,bs="ad") + s(y,k=20,bs="ad") + ti(x,y,k=20), data=AD_M_85, family = Gamma(link=log))
par(mfrow=c(2,2))
gam.check(M_AD_M_85,old.style = F)
par(mfrow=c(1,2))
vis.gam(M_AD_M_85,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="AD_M_85")


##### CN_F_62 #####

CN_F_62 = CN_F_62[CN_F_62$pet>0,] # valores PET superiores a 0, de otra forma no tiene sentido
M_CN_F_62 <- gam(pet ~ s(x,k=20,bs="ad") + s(y,k=20,bs="ad") + ti(x,y,k=20), data=CN_F_62, family = Gamma(link=log))
par(mfrow=c(2,2))
gam.check(M_CN_F_62,old.style = F)
par(mfrow=c(1,2))
vis.gam(M_CN_F_62,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="CN_F_62")


##### CN_F_71 #####

CN_F_71 = CN_F_71[CN_F_71$pet>0,] # valores PET superiores a 0, de otra forma no tiene sentido
M_CN_F_71 <- gam(pet ~ s(x,k=20,bs="ad") + s(y,k=20,bs="ad") + ti(x,y,k=20), data=CN_F_71, family = Gamma(link=log))
par(mfrow=c(2,2))
gam.check(M_CN_F_71,old.style = F)
par(mfrow=c(1,2))
vis.gam(M_CN_F_71,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="CN_F_71")


##### CN_F_85 #####

CN_F_85 = CN_F_85[CN_F_85$pet>0,] # valores PET superiores a 0, de otra forma no tiene sentido
M_CN_F_85 <- gam(pet ~ s(x,k=20,bs="ad") + s(y,k=20,bs="ad") + ti(x,y,k=20), data=CN_F_85, family = Gamma(link=log))
par(mfrow=c(2,2))
gam.check(M_CN_F_85,old.style = F)
par(mfrow=c(1,2))
vis.gam(M_CN_F_85,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="CN_F_85")


##### CN_M_63 #####

CN_M_63 = CN_M_63[CN_M_63$pet>0,] # valores PET superiores a 0, de otra forma no tiene sentido
M_CN_M_63 <- gam(pet ~ s(x,k=20,bs="ad") + s(y,k=20,bs="ad") + ti(x,y,k=20), data=CN_M_63, family = Gamma(link=log))
par(mfrow=c(2,2))
gam.check(M_CN_M_63,old.style = F)
par(mfrow=c(1,2))
vis.gam(M_CN_M_63,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="CN_M_63")


##### CN_M_71 #####

CN_M_71 = CN_M_71[CN_M_71$pet>0,] # valores PET superiores a 0, de otra forma no tiene sentido
M_CN_M_71 <- gam(pet ~ s(x,k=20,bs="ad") + s(y,k=20,bs="ad") + ti(x,y,k=20), data=CN_M_71, family = Gamma(link=log))
par(mfrow=c(2,2))
gam.check(M_CN_M_71,old.style = F)
par(mfrow=c(1,2))
vis.gam(M_CN_M_71,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="CN_M_71")


##### CN_M_85 #####

CN_M_85 = CN_M_85[CN_M_85$pet>0,] # valores PET superiores a 0, de otra forma no tiene sentido
M_CN_M_85 <- gam(pet ~ s(x,k=20,bs="ad") + s(y,k=20,bs="ad") + ti(x,y,k=20), data=CN_M_85, family = Gamma(link=log))
par(mfrow=c(2,2))
gam.check(M_CN_M_85,old.style = F)
par(mfrow=c(1,2))
vis.gam(M_CN_M_85,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="CN_M_85")



par(mfrow=c(2,3))


vis.gam(M_CN_F_62,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="CN_F_62")
vis.gam(M_CN_M_63,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="CN_M_63")
vis.gam(M_CN_F_71,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="CN_F_71")
vis.gam(M_CN_M_71,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="CN_M_71")
vis.gam(M_CN_F_85,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="CN_F_85")
vis.gam(M_CN_M_85,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="CN_M_85")
vis.gam(M_AD_F_59,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="AD_F_59") 
vis.gam(M_AD_M_60,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="AD_M_60")
vis.gam(M_AD_F_71,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="AD_F_71")
vis.gam(M_AD_M_72,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="AD_M_72")
vis.gam(M_AD_F_86,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="AD_F_86")
vis.gam(M_AD_M_85,plot.type = "contour",too.far=0.03,
        color="terrain",n.grid=200,zlim=c(-1,2),se=0,type="response",main="AD_M_85")


