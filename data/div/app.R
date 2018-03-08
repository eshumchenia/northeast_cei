## required packages
library(dplyr)
library(raster)
library(viridis)
library(Hmisc)
library(knitr)
library(ggplot2)
library(plotly)
library(GGally)
library(shiny)

## load data: these are rasters of species richness, shannon index, and simpson index for mdat cetacean models.

cet_richness <-raster("~/Dropbox/NROC/1 IEA DATA DEV/northeast_cei/data/div/mammal_richness_Cetaceans.tif")
cet_simpson <-raster("~/Dropbox/NROC/1 IEA DATA DEV/northeast_cei/data/div/mammal_simpson_Cetaceans.tif")
cet_shannon <-raster("~/Dropbox/NROC/1 IEA DATA DEV/northeast_cei/data/div/mammal_shannon_Cetaceans.tif")

## clip all rasters to the same extent as richness
cet_richness <-crop(cet_richness, extent(-347000, 1500000, -1081000, 1400000))
cet_simpson <-crop(cet_simpson, extent(-347000, 1500000, -1081000, 1400000))
cet_shannon <-crop(cet_shannon, extent(-347000, 1500000, -1081000, 1400000))

## convert rasters to data frames
richness_pts <- rasterToPoints(cet_richness)
richness_df <-data.frame(richness_pts)
colnames(richness_df) <- c("lon", "lat", "richness")

simpson_pts <- rasterToPoints(cet_simpson)
simpson_df <- data.frame(simpson_pts)
colnames(simpson_df) <- c("lon", "lat", "simpson")

shannon_pts <- rasterToPoints(cet_shannon)
shannon_df <- data.frame(shannon_pts)
colnames(shannon_df) <- c("lon", "lat", "shannon")

h1 <- hist(cet_richness, breaks=12)
h2 <- hist(cet_simpson, breaks=13)
h3 <- hist(cet_shannon, breaks=18)

## plot the three maps

m1 <- ggplot(data=richness_df, aes(y=lat, x=lon)) + geom_raster(aes(fill=richness)) +
  scale_fill_gradientn(colors = viridis(256)) + theme_void()

m2 <- ggplot(data=simpson_df, aes(y=lat, x=lon)) + geom_raster(aes(fill=simpson)) +
  scale_fill_gradientn(colors = viridis(256)) + theme_void()

m3 <- ggplot(data=shannon_df, aes(y=lat, x=lon)) + geom_raster(aes(fill=shannon)) +
  scale_fill_gradientn(colors = viridis(256)) + theme_void()


hdat1 <- data.frame(counts= h1$counts, breaks = h1$mids)
ph1 <- ggplot(hdat1, aes(x = breaks, y = counts, fill = breaks)) + 
  geom_bar(stat = "identity",alpha = 0.8) +
  xlab("richness")+ ylab("frequency") +
  scale_fill_gradientn(colors = viridis(12)) + theme_void() + theme(legend.position = "none")


hdat2 <- data.frame(counts= h2$counts, breaks = h2$mids)
ph2 <- ggplot(hdat2, aes(x = breaks, y = counts, fill = breaks)) + 
  geom_bar(stat = "identity",alpha = 0.8) +
  xlab("simpson")+ ylab("frequency") +
  scale_fill_gradientn(colors = viridis(13)) + theme_void() + theme(legend.position = "none")


hdat3 <- data.frame(counts= h3$counts, breaks = h3$mids)
ph3 <- ggplot(hdat3, aes(x = breaks, y = counts, fill = breaks)) + 
  geom_bar(stat = "identity",alpha = 0.8) +
  xlab("shannon")+ ylab("frequency") +
  scale_fill_gradientn(colors = viridis(18)) + theme_void() + theme(legend.position = "none")

## species richness map and histogram
map1 <-ggplotly(m1)
hist1 <-ggplotly(ph1)

## simpson index map and histogram
map2 <-ggplotly(m2)
hist2 <-ggplotly(ph2)

## shannon index map and histogram
map3 <-ggplotly(m3)
hist3 <-ggplotly(ph3)

richness <- subplot(map1, hist1)
simpson <- subplot(map2, hist2)
shannon <- subplot(map3, hist3)


ui <- fluidPage(
    
    mainPanel(
      column(12, 
             tabsetPanel(
               tabPanel("Species Richness", fluidRow(plotOutput("richness"))),
               tabPanel("Simpson Index", fluidRow(plotOutput("simpson"))),
               tabPanel("Shannon Index", fluidRow(plotOutput("shannon")))

                        )
               )
             )
      )

server <- (function(input, output) {
  
  output$richness <- renderPlot({
    
    richness
    
  })
  
  output$simpson <- renderPlot({
    
    simpson
    
  })

  output$shannon <- renderPlot({
    
    shannon
    
  })
  
})

shinyApp(ui = ui, server = server)
