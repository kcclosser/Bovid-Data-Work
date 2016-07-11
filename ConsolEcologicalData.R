#TASK1
setwd("/Users/kennyclosser/Downloads")
df <- read.csv(file="Ungulata_Alt_MAP_MAT.csv", header=FALSE, stringsAsFactors=FALSE,
                  fileEncoding="latin1", skip=16)
newdf <- data.frame(aggregate(df$V25, by=list(IDnum = df$V1, V2=df$V2, df$V1), "sum")) 
ndf <- merge(df, newdf, by="V2",all=TRUE)
ndf[, "div"] <- ndf[, "V36"] / ndf[, "x"]
ndf$product <- ndf$div * ndf$V36
tempfile <- data.frame(aggregate(ndf$product, by=list(V2=ndf$V2, ndf$V1), "sum"))
finalfile <- merge(ndf, tempfile, by="V2", all=TRUE)
finalfile <- finalfile[ , -which(names(finalfile) %in% c("div","product","Group.2"))]
colnames(finalfile)[49] <- "temp_aggregate" #unsure which mean to use here (MAP or MAT) but the structure is correct
View(finalfile) #Shows a new file with the same data as the file read in and an additional column at the end with the desired overall mean

#TASK2
newdf <- data.frame(aggregate(df$V33, by=list(IDnum = df$V1, V2=df$V2), "min" ))
ndf <- merge(df, newdf, by="V2",all=TRUE)
View(ndf) #final column contains selected minimum and maximum, easily repeated

#TASK3
df$squares_alt <- df$V32^2
newdf <- data.frame(aggregate(df$squares_alt, by=list(IDnum = df$V1, V2 = df$V2), "sum"))
ndf <- merge(df, newdf, by="V2", all=TRUE)
ndf$sum_stddev_alt <- ndf$x^(1/2)
View(ndf) #last column is a sum of variances square rooted, so a standard deviation for the entire species
