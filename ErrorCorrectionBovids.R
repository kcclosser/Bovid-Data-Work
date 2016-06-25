#SETUP
setwd("/Users/kennyclosser/Downloads")
install.packages("xlsx")
install.packages("zoo")
library("zoo")
library("xlsx")
xfile <- read.xlsx("Scott 1985 Bovid Postcrania Data.xlsx.xlsx", sheetName = "Sheet1", header = TRUE)
xfile <- subset(xfile, select=-c(NA..1,NA..2,NA..3,NA..4,NA..5,NA..6))
xfile$Genus <- na.locf(xfile$Genus)
xfile$Species <- na.locf(xfile$Species)
xfile$MinLen <- lapply(strsplit(as.character(xfile$Range), "\\-"), "[", 1)
xfile$MaxLen <- lapply(strsplit(as.character(xfile$Range), "\\-"), "[", 2)
xfile$MinLen <- gsub("\\(", "", paste(xfile$MinLen))
xfile$MaxLen <- gsub( "\\)", "", paste(xfile$MaxLen))
xfile$Bone <- gsub("1", "I", paste(xfile$Bone))
xfile$Bone <- gsub("i", "I", paste(xfile$Bone))
xfile$Bone <- gsub("l", "I", paste(xfile$Bone))
View(xfile)

#Objective 1 -- Developmental and Functional Homologue Comparison
#o1a <- grepl("HI", xfile$Bone)
#o1b <- grepl("FI", xfile$Bone)
#o1c <- grepl("RI", xfile$Bone)
#o1d <- grepl("TI", xfile$Bone)
#o1e <- grepl("MCL", xfile$Bone)
#o1f <- grepl("MTL", xfile$Bone)
install.packages("tidyr")
install.packages("dplyr")
library("tidyr")
library("dplyr")
library("zoo")
odf <- xfile[xfile$Bone == "HI" | xfile$Bone == "FI", ]
odf <- spread(odf, Bone, Mean.Length)
odf$HI <- na.locf(odf$HI)
odf <- subset(odf, !is.na(FI))
plot(odf$HI, odf$FI, main="HI:FI", xlab="HI", ylab="FI")

odf1 <- xfile[xfile$Bone == "RI" | xfile$Bone == "TI", ]
odf1 <- spread(odf1, Bone, Mean.Length)
odf1$HI <- na.locf(odf1$RI)
odf1 <- subset(odf1, !is.na(RI))
plot(odf1$HI, odf1$FI, main="RI:TI", xlab="RI", ylab="TI")

odf2 <- xfile[xfile$Bone == "MCI" | xfile$Bone == "MTI", ]
odf2 <- spread(odf2, Bone, Mean.Length)
odf2$HI <- na.locf(odf2$MCI)
odf2 <- subset(odf2, !is.na(MTI))
plot(odf2$HI, odf2$FI, main="MCI:MTI", xlab="MCI", ylab="MTI")

odf3 <- xfile[xfile$Bone == "HI" | xfile$Bone == "TI", ]
odf3 <- spread(odf, Bone, Mean.Length)
odf3$HI <- na.locf(odf$HI)
odf3 <- subset(odf3, !is.na(TI))
plot(odf3$HI, odf3$FI, main="HI:TI", xlab="TI", ylab="tI")

odf4 <- xfile[xfile$Bone == "RI" | xfile$Bone == "MCI", ]
odf4 <- spread(odf4, Bone, Mean.Length)
odf4$HI <- na.locf(odf4$RI)
odf4 <- subset(odf4, !is.na(MCI))
plot(odf4$HI, odf4$FI, main="RI:MCI", xlab="RI", ylab="MCI")

#Objective 2: In file association.r
