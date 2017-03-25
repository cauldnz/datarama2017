library(mbie)
library(dplyr)
library(scales)
library(ggplot2)
library(ggthemes)
library(treemap)

library(sp)
library(lattice)
library(latticeExtra)
library(mbiemaps)

data(RTO)
data(RTEs)
data(IVStrips)


baseLine <- 2009
comparison <- 2015

# Estimate compound annual growth rate
growth <- RTEs %>%
  group_by(Territorial_Authority, Region) %>%
  summarise(Growth = CAGR(sum(Spend[YEMar == comparison]) / sum(Spend[YEMar == baseLine]), 4)) %>%
  data.frame()

# Merge with the absolute size of tourism in 2013
RTE2 <- RTEs %>%
  filter(YEMar == comparison) %>%
  group_by(Territorial_Authority, Region) %>%
  summarise(Spend = sum(Spend)) %>%
  inner_join(growth, by = c("Territorial_Authority", "Region")) %>%
  data.frame()


# Draw treemap to show size and growth by hierarchical TA and Region
treemap(RTE2,
        index = c("Region", "Territorial_Authority"),
        vSize = "Spend",
        vColor = "Growth",
        palette = "Spectral",
        title = sprintf("Regional Tourism %s, and CAGR %s to %s",comparison,baseLine,comparison),
        type = "value",
        inflate.labels = TRUE,
        algorithm = "squarified")



# Aggregate domestic tourism spend and average annual growth rate
dom <- RTEs %>%
  filter(Type == "Domestic") %>%
  group_by(RTO) %>%
  summarise(
    Growth = CAGR(sum(Spend[YEMar==comparison]) / sum(Spend[YEMar==baseLine]), 4),
    SpendComparison = sum(Spend[YEMar == comparison])
  ) %>%
  data.frame()

#This wrangles some data downloaded from Stats infoshare
library(data.table)
dtRTO <- fread("./RTO_Occupancy_2009_t_2015.csv")
dtRTO[,Year:=sapply(strsplit(V1,"M"),"[[",1)]
dtRTO[,Month:=sapply(strsplit(V1,"M"),"[[",2)]
dtRTO <- dtRTO[,lapply(.SD, mean, na.rm=TRUE),by="Year",.SDcols=-c("V1","Month","Year")]
dtRTO <- melt(dtRTO,id.vars = "Year")
setnames(dtRTO,c("Year","variable","value"),c("Year","RTO","Value"))

# merge the RTE with the data from the data slot of the RTO map
data2 <- merge(RTO@data, dom, by="RTO")

#AHHHHHHHHHHH Using stringdist because the names are different
dtRTO2 <- as.data.table(RTO)


# replace the data slot of our RTO map with the new data frame
RTO@data <- data2


#----------------draw map - fancy version - title, palette, border colour, added circle layer---------------

# defining our own palette
# First, how many colours are needed that are in the negative band
lessthanzero <- round((-min(RTO@data$Growth)) / sum(abs(range(RTO@data$Growth))) * 100)

# then create a palette of colours from red to grey and out to MBIE blue
cols <- c(colorRampPalette(c("red", "grey95"))(lessthanzero), colorRampPalette(c("grey95", mbie.cols(1)))(100-lessthanzero))

# coordinates to add circles
coords <- SpatialPoints(coordinates(RTO))

# how big will the circles be?  "sizes" variable for later use
sizes <- sqrt(RTO@data$SpendComparison)/8

# set parameters so no axis line ie no box around the plot
trellis.par.set("axis.line", list(col=NA,lty=1,lwd=1))

spplot(RTO, zcol="Growth", col.regions=cols, main="Domestic tourist spend", col="white", 
       sp.layout=list("sp.points", coords, pch=1, col="black", lwd=2, cex=sizes)) # add circles layer
grid.text(paste("Circle size is proportional to\ndomestic tourism spend in",comparison), .2,.7)
grid.text(paste("Average growth per year in domestic tourism",sep = "-", baseLine,comparison), .93,.5, , rot=-90)
