library(raster)
library(rasterVis)
library(gganimate)
library(reshape)
library(ggplot2)
library(viridis)

# Load rasters for bird species in the benthic-bivalve feeders ecological group
avian_all <- list.files("./data/abd/avian_all" , pattern = "*.tif$", 
                        full.names = TRUE)

# Create a raster stack from the individual species layers and set value <0 to 0 

avian_all <-stack(avian_all)
avian_all[avian_all < 0] <- 0

# Crop the rasters to the Northeast
northeast <- extent(200000, 941556.8, 600000, 1111272)
avian_all <-crop(avian_all, northeast)

a1 <- avian_all$arte
a2 <- sum(a1, avian_all$atpu)
a3 <- sum(a2, avian_all$blki)
a4 <- sum(a3, avian_all$blsc)
a5 <- sum(a4, avian_all$bogu)
a6 <- sum(a5, avian_all$brpe)
a7 <- sum(a6, avian_all$brsp)
a8 <- sum(a7, avian_all$colo)
a9 <- sum(a8, avian_all$comu)
a10 <- sum(a9, avian_all$cosh)
a11 <- sum(a10, avian_all$cote)
a12 <- sum(a11, avian_all$dcco)
a13 <- sum(a12, avian_all$dove)
a14 <- sum(a13, avian_all$gbbg)
a15 <- sum(a14, avian_all$grsh)
a16 <- sum(a15, avian_all$herg)
a17 <- sum(a16, avian_all$hogr)
a18 <- sum(a17, avian_all$lagu)
a19 <- sum(a18, avian_all$lesp)
a20 <- sum(a19, avian_all$lete)
a21 <- sum(a20, avian_all$lete)
a22 <- sum(a21, avian_all$mash)
a23 <- sum(a22, avian_all$nofu)
a24 <- sum(a23, avian_all$noga)
a25 <- sum(a24, avian_all$poja)
a26 <- sum(a25, avian_all$razo)
a27 <- sum(a26, avian_all$rbgu)
a28 <- sum(a27, avian_all$reph)
a29 <- sum(a28, avian_all$rnph)
a30 <- sum(a29, avian_all$rost)
a31 <- sum(a30, avian_all$royt)
a32 <- sum(a31, avian_all$rtlo)
a33 <- sum(a32, avian_all$sosh)
a34 <- sum(a33, avian_all$susc)
a35 <- sum(a34, avian_all$wisp)
a36 <- sum(a35, avian_all$wwsc)


stack_state <-brick(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
                    a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, 
                    a27, a28, a29, a30, a31, a32, a33, a34, a35, a36)

breaks <- seq(from = min(0),
              to = max(25),
              length.out = 12)

x <-function(x) (save.image(file = "/data"))

raster_plot <- function(x, stack_state) {
  jpeg(filename = paste(names(stack_state[[x]]), ".jpg"))
  plot(stack_state[[x]], breaks = breaks, col = viridis(11))
  dev.off()
}

sapply(1:nlayers(stack_state), function(x) raster_plot(x, stack_state))
