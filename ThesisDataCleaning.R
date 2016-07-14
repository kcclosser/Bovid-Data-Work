#SETUP
setwd("C:/Users/ZiXiang/Dropbox/Thesis")
install.packages("xlsx")
library(xlsx)
install.packages("zoo")
library(zoo)

xfile <- read.csv("Scott 1985 Bovid Postcrania Data.csv",header = TRUE)
xfile <- as.data.frame(xfile)
#####FIX CODE ABOVE####

#xfile$Genus <- na.locf(xfile$Genus)
#xfile$Species <- na.locf(xfile$Species)
xfile$MinLen <- lapply(strsplit(as.character(xfile$Range), "\\-"), "[", 1)
xfile$MaxLen <- lapply(strsplit(as.character(xfile$Range), "\\-"), "[", 2)
xfile$MinLen <- gsub("\\(", "", paste(xfile$MinLen))
xfile$MaxLen <- gsub( "\\)", "", paste(xfile$MaxLen))
#xfile$Bone <- gsub("1", "I", paste(xfile$Bone))
#xfile$Bone <- gsub("i", "I", paste(xfile$Bone))
#xfile$Bone <- gsub("l", "I", paste(xfile$Bone))
View(xfile)

xfile

#Objective 1 -- Developmental and Functional Homologue Comparison
#o1a <- grepl("H1", xfile$Bone)
#o1b <- grepl("FI", xfile$Bone)
#o1c <- grepl("RI", xfile$Bone)
#o1d <- grepl("TI", xfile$Bone)
#o1e <- grepl("MCL", xfile$Bone)
#o1f <- grepl("MTL", xfile$Bone)
install.packages("tidyr")
install.packages("dplyr")

library(dplyr)
library(tidyr)

odf <- xfile[xfile$Bone == "H1" | xfile$Bone == "F1", ]
odf <- spread(odf, Bone, Mean.Length)
odf$H1 <- na.locf(odf$H1)
odf <- subset(odf, !is.na(F1))
plot(odf$H1, odf$F1, main="H1:F1", xlab="H1", ylab="F1")

odf1 <- xfile[xfile$Bone == "R1" | xfile$Bone == "T1", ]
odf1 <- spread(odf1, Bone, Mean.Length)
odf1$R1 <- na.locf(odf1$R1)
odf1 <- subset(odf1, !is.na(T1))
plot(odf1$R1, odf1$T1, main="R1:T1", xlab="R1", ylab="T1")

odf2 <- xfile[xfile$Bone == "MC1" | xfile$Bone == "MT1", ]
head(odf2)
odf2 <- spread(odf2, Bone, Mean.Length)
odf2$MC1 <- na.locf(odf2$MC1)
odf2 <- subset(odf2, !is.na(MT1))
plot(odf2$MC1, odf2$MT1, main="MC1:MT1", xlab="MC1", ylab="MT1")

odf3 <- xfile[xfile$Bone == "H1" | xfile$Bone == "T1", ]
odf3 <- spread(odf3, Bone, Mean.Length)
odf3$H1 <- na.locf(odf3$H1)
odf3 <- subset(odf3, !is.na(T1))
plot(odf3$H1, odf3$T1, main="H1:T1", xlab="H1", ylab="T1")

odf4 <- xfile[xfile$Bone == "R1" | xfile$Bone == "MC1", ]
odf4 <- spread(odf4, Bone, Mean.Length)
odf4$R1 <- na.locf(odf4$R1)
odf4 <- subset(odf4, !is.na(MC1))
plot(odf4$R1, odf4$MC1, main="R1:MC1", xlab="R1", ylab="MC1")

#rawdata is the final output from Scott 1985 where all the bone measurements are columns)#
myvars <- c("Genus", "Species", "Bone", "Mean.Length")
newdata <- xfile[myvars]
rawdata <- spread(newdata, Bone, Mean.Length)
head(rawdata)
write.xlsx(rawdata, "C:/Users/ZiXiang/Dropbox/Thesis/rawdata.xlsx")

#######FINDING MEAN, MIN, MAX FOR ECOLOGY DATA#########

ecodata <- read.csv("Ungulata_Alt_MAP_MAT.csv",header = TRUE)
altmin <-aggregate(alt_min ~ binomial, ecodata, min)
altmax <-aggregate(alt_max ~ binomial, ecodata, max)
MATmin <-aggregate(MAT_min ~ binomial, ecodata, min)
MATmax <-aggregate(MAT_max ~ binomial, ecodata, max)
MAPmin <-aggregate(MAP_min ~ binomial, ecodata, min)
MAPmax <-aggregate(MAP_max ~ binomial, ecodata, max)

####KENNY consolidating across range polygons. summing altitude mean min max std. Outfile file is Ungulate_alt_computed.csv#####

#TASK1
df <- read.csv(file="Ungulata_Alt_MAP_MAT.csv", header=TRUE)
View(df)

newdf <- data.frame(aggregate(df$shape_Area, by=list(IDnum = df$id_no, df$binomial), "sum"))
View(newdf)
colnames(newdf) <- c("id_no", "binomial", "shape_Area_s")
ndf <- merge(df, newdf, by="id_no",all=TRUE)
View(ndf)
ndf[, "div"] <- ndf[, "shape_Area"] / ndf[, "shape_Area_s"]
sapply(ndf, class)
View(ndf)
ndf[,"div2"] <- ndf$div * ndf$alt_mean
tempfile <- data.frame(aggregate(ndf$div2, by=list(ndf$id_no), "sum"))
View(tempfile)
colnames(tempfile) <- c("id_no", "alt_mean_ss")
prefinal <- merge(ndf, tempfile, by="id_no", all=TRUE)

#Final file still have multiple rows for each species.
View(prefinal)

#TASK2
#find the smallest minimum of all the minimums for a species' multiple range polygons
temp2 <- aggregate(df$alt_min,by=list(df$id_no), function(x) (min(x)) )
View(temp2)
colnames(temp2) <- c("id_no", "alt_min_ss")

temp3 <- aggregate(df$alt_max,by=list(df$id_no), function(x) (max(x)) )
colnames(temp3) <- c("id_no", "alt_max_ss")

#temp3 has alt_mean and alt_min
temp4 <- merge(prefinal, temp2, by="id_no")
temp5 <- merge(temp4, temp3, by="id_no")

#Has alt_mean, max, min.
View(temp5)

#TASK3
ndf$alt_stdev <- ndf$alt_stdev^2
ndf[,"div3"] <- ndf$div * ndf$alt_stdev
tempfile1 <- data.frame(aggregate(ndf$div3, by=list(ndf$id_no), "sum"))
tempfile1$div3 <- tempfile1$div3^1/2
colnames(tempfile1) <- c("id_no", "alt_stdev_ss")
final <- merge(temp5, tempfile1, by="id_no", all=TRUE)
View(final)
write.csv(final, "C:/Users/ZiXiang/Dropbox/Thesis/Ungulata_alt_computed.csv")

colnames(final)
View(total)
colnames(total)[3] <- "species_name"
temptemp <- merge(total, final, by="species_name")
View(temptemp)
write.csv(temptemp, "C:/Users/ZiXiang/Dropbox/Thesis/Ungulata_alt_computed_McWa.csv")

###Pilot Data Ternary with Gradient#####
library(ggtern)
total1 <- read.csv("Ungulata_alt_computed_McWa.csv",header = TRUE)
as.data.frame(total1)
View(total1)
set.seed(1)
zz = total1$alt_stdev_ss
plot <- ggtern(data = data.frame(x = total1$FL_t,
                                 y = total1$TL_t,
                                 z = total1$MTL_t),
               aes(x, y, z))
plot + stat_density_tern(geom='polygon',
                         n         = 200,
                         aes(fill  = ..level.., weight = zz)) +
  geom_density_tern(aes(weight=zz,color=..level..)) +
  geom_point() +
  theme_rgbw() +
  scale_fill_gradient(low = "blue",high = "red")   +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) + 
  guides(fill = guide_colorbar(order=1),
         color="none") + 
  labs(title= "Stylopod:Zeugopod:Autopod Diameter",
       fill = "Altitude Stdev")

#Data all clumped in Ternary Diagram#
library(scatterplot3d)
library(zoom)
scatterplot3d(total$FL,total$TL,total$MTL, main="3D Scatterplot")
zm()
