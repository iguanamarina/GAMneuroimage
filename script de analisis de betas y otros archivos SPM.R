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

# ¿Que son estas imagenes beta y demas que aparecen por ahi?

beta_0001 <- readNIfTI("beta_0001", verbose = FALSE, warn = -1, reorient = TRUE,
                       call = NULL, read_data = TRUE);
image(beta_0001)

#### Beta_1

beta_0001 <- readNIfTI("beta_0001", verbose = FALSE, warn = -1, reorient = TRUE,
                       call = NULL, read_data = TRUE);
image(beta_0001)


"beta_0001" = f.clean("beta_0001")

#### Beta_2

beta_0002 <- readNIfTI("beta_0002", verbose = FALSE, warn = -1, reorient = TRUE,
                       call = NULL, read_data = TRUE);
image(beta_0002)


"beta_0002" = f.clean("beta_0002")

#### Residuos

resMS <- readNIfTI("ResMS.nii", verbose = FALSE, warn = -1, reorient = TRUE,
                   call = NULL, read_data = TRUE);
image(resMS)

clean_resMS = f.clean("ResMS")
show(Res_0001)

# > sum(Res_0001$pet,na.rm = T)
# [1] 1645.299

####
