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

# Estimate compound annual growth rate
growth <- RTEs %>%
  group_by(Territorial_Authority, Region) %>%
  summarise(Growth = CAGR(sum(Spend[YEMar == 2013]) / sum(Spend[YEMar == 2009]), 4)) %>%
  data.frame()

# Merge with the absolute size of tourism in 2013
RTE2 <- RTEs %>%
  filter(YEMar == 2013) %>%
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
        title = "Regional Tourism 2013, and CAGR 2009 to 2013",
        type = "value",
        inflate.labels = TRUE,
        algorithm = "squarified")



tmp <- IVStrips %>%
  group_by(YearQuarter, CountryGroup(COPRTop5)) %>%
  summarise(Total = sum(SmoothTotalSpend * FinalWeight * Adjusted_Factor)) %>%
  data.frame()

tmp$period <- with(tmp, as.numeric(substring(YearQuarter, 1, 4)) + (as.numeric(substring(YearQuarter, 6, 6))-.5)/4)
names(tmp)[2] <- "Country"

ggplot(tmp, aes(x = period, y = Total / 10^6, color = Country)) +
  stat_sa(frequency = 4, geom = "point", size = 2) +
  stat_sa(frequency = 4, size = 2) +
  geom_line(frequency = 4, size = 0.8, alpha = 0.4) +
  theme_economist() +
  labs(x = "", y = "Total spend ($m)\n") +
  scale_color_manual("", values=tourism.cols("Alternating")) +
  ggtitle("Seasonally adjusted NZ tourism spend by country of origin")


# Aggregate domestic tourism spend and average annual growth rate
dom <- RTEs %>%
  filter(Type == "Domestic") %>%
  group_by(RTO) %>%
  summarise(
    Growth = CAGR(sum(Spend[YEMar==2013]) / sum(Spend[YEMar==2009]), 4),
    Spend2013 = sum(Spend[YEMar == 2013])
  ) %>%
  data.frame()

# merge the RTE with the data from the data slot of the RTO map
data2 <- merge(RTO@data, dom, by="RTO")

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
sizes <- sqrt(RTO@data$Spend2013)/8

# set parameters so no axis line ie no box around the plot
trellis.par.set("axis.line", list(col=NA,lty=1,lwd=1))

spplot(RTO, zcol="Growth", col.regions=cols, main="Domestic tourist spend", col="white", 
       sp.layout=list("sp.points", coords, pch=1, col="black", lwd=2, cex=sizes)) # add circles layer
grid.text("Circle size is proportional to\ndomestic tourism spend in 2013", .2,.7)
grid.text("Average growth per year in domestic tourism 2009 - 2013", .93,.5, , rot=-90)