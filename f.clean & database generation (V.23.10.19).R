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
          
          temp0 = data.frame(z,x,y,pet) # tmeporal dataframe
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


write.csv2(database,file="Database.csv",sep=";",na="NA") #export as .csv



##########################################################
###            MEAN AVERAGE NORMALIZATION              ###
##########################################################


Data <- data.frame(PPT=integer(),group=integer(),sex=integer(),age=integer(),z=integer(),x=integer(),y=integer(),pet=integer(),pet_normal=integer())
# Data is the data.framecreated for masked and mean averaged data

for (i in 1:length(files)) {
  
  temp <- database[database$PPT==files[i],]
  temp$pet[temp$pet==0]<-NaN
  mean <- mean(as.numeric(temp$pet),na.rm=T)
  pet_normal <- as.data.frame((temp$pet)/mean)
  colnames(pet_normal)<-c("pet_normal")
  temp <- cbind(temp,pet_normal)
  show(temp)    
  Data <- rbind(Data,temp)
  
}




