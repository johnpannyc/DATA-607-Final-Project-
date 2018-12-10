
#smartbind607

# Libraries (Imported) ----------------------------------------------------
library(tidyverse)
library(gtools)
library(haven)
library(ggplot2)


# Smart Bind but with edited Excel for Vlookup Mistakes -------------------

oral08 <- read.csv("OralData/oral08.csv")
oral09 <- read.csv("OralData/oral09.csv")
oral10 <- read.csv("OralData/oral10.csv")
oral11 <- read.csv("OralData/oral11.csv")

year2008 <- read.csv("Complete Year 2008 Glaucoma.csv")
year2009 <- read.csv("Year 2009.csv")
year2009$Glaucoma[is.na(year2009$Glaucoma)] <- 1
year2010 <- read.csv("Complete Year 2010 Glaucoma.csv")
year2011 <- read.csv("Complete Year 2011 Glaucoma.csv")

year2008 <- year2008[,1:which(colnames(year2008)=="Glaucoma")]
year2009 <- year2009[,1:which(colnames(year2009)=="Glaucoma")]
year2010 <- year2010[,1:which(colnames(year2010)=="Glaucoma")]
year2011 <- year2011[,c(1:which(colnames(year2011)=="E_DL_14"),
                        which(colnames(year2011)=="Glaucoma"))]

# year2008 <- inner_join(year2008,oral08,by = "id",suffix = c("",".1"))
year2008 <- inner_join(year2008,oral08,by = "id",suffix = c("",".1"))

test2009 <- inner_join(year2009,oral09,by = "id",suffix = c("",".1"))
year2009 <- test2009

year2009$X <- NULL
year2009$X.1 <- NULL
test2010 <- inner_join(year2010,oral10,by="ID",suffix = c("",".1"))
year2010 <- test2010
test2011 <- inner_join(year2011,oral11,by = "ID",suffix = c("",".1"))
year2011 <- test2011


# Smart Binding For TestAll Years Fix -----------------------------------------------------------

test2008 <- year2008
test2009 <- year2009
test2010 <- year2010
test2011 <- year2011

colnames(test2008) <- toupper(colnames(test2008))
colnames(test2009) <- toupper(colnames(test2009))
colnames(test2010) <- toupper(colnames(test2010))
colnames(test2011) <- toupper(colnames(test2011))
source("reading HNALL.R")


#factorizes the CPI cols
test2010$O_CPI_LL[which(test2010$O_CPI_LL==9)] <- 8

perio <- colnames(data)[which(substr(colnames(data),1,5)=="O_CPI")]
for (i in perio) {
  test2008[,i] <-  as.factor(test2008[,i])
  test2009[,i] <- as.factor(test2009[,i])
}
#insert diabetes cols 

#FINAL
testAllYearsFix <- smartbind(test2008,test2009,test2010,test2011)
write_csv (testAllYearsFix, 'Output/testAllYearsTix.csv')  ## GH successful

# Finding NumTeeth --------------------------------------------------------
data <- testAllYearsFix
    # Create A HTeethCount Col ------------------------------------------------
        ###   ____ creating a HTeethCount Col ____  ###
        firTeethCol <- which(colnames(data)=="O_18B")
        secTeethCol <- which(colnames(data)=="O_38B")#18B - 38B adult teeth
        
        teethdata <- data[,c(1,which(colnames(data)=="GLAUCOMA"),firTeethCol:secTeethCol)] 
        #Take out Wisdom teeth 18,28,48,38
        wisdomIndicies <- c(which(substr(colnames(teethdata),3,4)=="18"),
                            which(substr(colnames(teethdata),3,4)=="28"),
                            which(substr(colnames(teethdata),3,4)=="38"),
                            which(substr(colnames(teethdata),3,4)=="48"))
        teethdata <- teethdata[,-wisdomIndicies]
        ##
        teethdata$numHTeeth <- 0
        for (i in 1:NROW(teethdata)) {
          uHealthyI <- c()
          healthyI <- c()
          empty <- FALSE
          for (j in which(colnames(teethdata)=="O_17B"):which(colnames(teethdata)=="O_37B")) {
            #Edge Case
            if (is.na(teethdata[i,j]) == TRUE) {
              empty <- TRUE
              teethdata[i,"numHTeeth"] <- "EMPTY"
              break
            }
            if (!(teethdata[i,j] == 0 | teethdata[i,j] == 3 | 
                  teethdata[i,j] == 6 | teethdata[i,j] == 1)) {#changed to include even cavities
              uHealthyI <- append(uHealthyI,substr(colnames(teethdata)[j],3,4))
            }
            healthyI <- append(healthyI,substr(colnames(teethdata)[j],3,4))
          }
          if (!empty) { 
            teethdata[i,"numHTeeth"] <- length(setdiff(healthyI,uHealthyI))
          } 
        }
        #this produces strings, but they will be coercied into N/a by as.numeric and then ignored
        
        
    # Create HTeethWC (with Cavities) Col -------------------------------------
        teethdata$HteethWC <- 0
        for (i in 1:NROW(teethdata)) {
          uHealthyI <- c()
          healthyI <- c()
          empty <- FALSE
          for (j in which(colnames(teethdata)=="O_17B"):which(colnames(teethdata)=="O_37B")) {
            #Edge Case
            if (is.na(teethdata[i,j]) == TRUE) {
              empty <- TRUE
              teethdata[i,"HteethWC"] <- "EMPTY"
              break
            }
            if (!(teethdata[i,j] == 0 | teethdata[i,j] == 3 |teethdata[i,j] == 6)) {
              uHealthyI <- append(uHealthyI,substr(colnames(teethdata)[j],3,4))
            }
            healthyI <- append(healthyI,substr(colnames(teethdata)[j],3,4))
          }
          if (!empty) { 
            teethdata[i,"HteethWC"] <- length(setdiff(healthyI,uHealthyI))
          } 
        }

    # Splitting NTeeth Into Categorical  --------------------------------------
countHealthyTeeth <- function(teethdata,indicies) {
  firQ <- 0 # has 0 - 6 teeth
  secQ <- 0 # has 7 - 13 teeth
  thrQ <- 0 # has 14 - 20 teeth
  fourQ <- 0 # 21+ teeth
  numericTeeth <- as.numeric(teethdata)
  for (i in indicies) {
    if (is.na(numericTeeth[i])) {
      next
    }
    else if (numericTeeth[i] %/% 7 == 0) {
      firQ <- firQ + 1
    }
    else if (numericTeeth[i] %/% 7 == 1) {
      secQ <- secQ + 1
    }
    else if (numericTeeth[i] %/% 7 == 2) {
      thrQ <- thrQ + 1
    }
    else if (numericTeeth[i] %/% 7 == 3 | numericTeeth[i] == 28) {
      fourQ <- fourQ + 1
          }
  }
    return (c(firQ,secQ,thrQ,fourQ))
}

# Creating a Categorial Variable for numTeeth -----------------------------
temp <- data
source("CountingHTN.R")
data$numHTeeth <- teethdata$numHTeeth
data <- createTeethPresentCol2(data)

# NPERODONTAL -------------------------------------------------------------
data$NPERODONTAL <- 0
data$gumTrouble <- 0 
data$newPero <- 0 
data$PERO <- 0
      

firPeroCol <- which(colnames(data) == "O_CPI_UR")
secPeroCol <- which(colnames(data) == "O_CPI_LL")

#Loops through data to look for people with gingival sulcus > 5mm
for (i in 1:dim(data)[1]) {
  data[i,"NPERODONTAL"] <- sum(data[i,firPeroCol:secPeroCol]==4) + 
    sum(data[i,firPeroCol:secPeroCol]==3)
  data[i,"gumTrouble"] <- sum(data[i,firPeroCol:secPeroCol] == 1) + sum(data[i,firPeroCol:secPeroCol] == 2)
  +sum(data[i,firPeroCol:secPeroCol] == 3)+sum(data[i,firPeroCol:secPeroCol] == 4)
}

#Creates a binary col for Periodontal disease
for (i in 1:dim(data)[1]) {
  if (is.na(data$NPERODONTAL[i])) {
    data$PERO[i] <- NA
    
  } else if (data$NPERODONTAL[i] >= 1) {
    data$PERO[i] <- 1
  }
}

##defining pero as everything from gum bleeding and up
for (i in 1:dim(data)[1]) {
  if (is.na(data$gumTrouble[i])) {
    data$newPero[i] <- NA
    
  } else if (data$gumTrouble[i] >= 1) {
    data$newPero[i] <- 1
  }
}

data$PER0two <- 0 
for (i in 1:dim(data)[1]) {
  if (is.na(data$NPERODONTAL[i])) {
    data$PER0two[i] <- NA
    
  } else if (data$NPERODONTAL[i] >= 4) {
    data$PER0two[i] <- 1
  }
}

temp <- data

write_csv (temp, 'Output/dataAllYrFixFinal.csv')  # GH did
#glaucoma data is all good, teeth final VR all good

table (data$ER0two,data$Glaucoma)

table (data$GLAUCOMA)
