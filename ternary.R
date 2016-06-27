install.packages("ggtern")
library(ggtern)
zz = runif(100)
plot <- ggtern(data = data.frame(x = runif(100),
                                 y = runif(100),
                                 z = runif(100)),
               aes(x, y, z))
plot + stat_density_tern(geom='polygon',
                         n         = 200,
                         aes(fill  = ..level.., weight = zz)) +
  geom_density_tern(aes(weight=zz,color=..level..)) +
  geom_point() +
  theme_rgbw() +
  labs(title = "Example Density/Contour Plot")    +
  scale_fill_gradient(low = "blue",high = "red")   +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) + 
  guides(fill = guide_colorbar(order=1),
         color="none") + 
  labs(title= "Ternary Plot and Filled Contour",
       fill = "Value, V")

