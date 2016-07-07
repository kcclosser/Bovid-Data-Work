#TASK1
setwd("/Users/kennyclosser/Downloads")
df <- read.csv(file="Ungulata_Alt_MAP_MAT.csv", header=FALSE, stringsAsFactors=FALSE,
                  fileEncoding="latin1", skip=16)
newdf <- data.frame(aggregate(df$V25, by=list(IDnum = df$V1, V2=df$V2, df$V1), "sum")) #not working, need to find a way to expand sum vector
ndf <- merge(df, newdf, by="V2",all=TRUE)
ndf[, "div"] <- ndf[, "V31"] / ndf[, "x"]
sapply(ndf, class)
ndf$product <- ndf$div * ndf$V31
tempfile <- data.frame(aggregate(ndf$product, by=list(V2=ndf$V2, ndf$V1), "sum")) #same issue as line 4
finalfile <- merge(ndf, tempfile, by="V2", all=TRUE)
finalfile <- finalfile[ , -which(names(finalfile) %in% c("div","product","Group.2"))]
colnames(finalfile)[49] <- "alt_mean"

#TASK2
aggregate(df$V33,by=list(df$V1), function(x) (min(x)) ) #same as above, can easily replicate for selection of other columns after debugged
#for species min/max, do the same thing as above. Figure out how to replace column too

#TASK3
df$squares_alt <- df$V32^2
df$sum_variances_alt <- aggregate(df$squares_alt,by=list(df$V1), "sum")#same issue as above
df$sum_stddev_alt <- df$sum_variances_alt^1/2
