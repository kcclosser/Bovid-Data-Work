#TASK1
setwd("/Users/kennyclosser/Downloads")
df <- read.csv(file="Ungulata_Alt_MAP_MAT.csv", header=FALSE, stringsAsFactors=FALSE,
                  fileEncoding="latin1", skip=16)
df$total_area <- aggregate(df$V25, by=list(df$V1), "sum") #not working, need to find a way to expand sum vector
df$factors <- transform(df, new =  V25/ total_area)
df$product <- df$factors * df$V31
df$V26 <- aggregate(df$product, by=list(df$V1), "sum") #same issue as line 4

#TASK2
aggregate(df$V33,by=list(df$V1), function(x) (min(x)) ) #same as above, can easily replicate for selection of other columns after debugged
#for species min/max, do the same thing as above. Figure out how to replace column too

#TASK3
df$squares_alt <- df$V32^2
df$sum_variances_alt <- aggregate(df$squares_alt,by=list(df$V1), "sum")#same issue as above
df$sum_stddev_alt <- df$sum_variances_alt^1/2
