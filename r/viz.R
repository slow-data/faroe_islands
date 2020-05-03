#===========================================
# @Project: Faroe Islands
# @Name: viz
# @author: jprimav
# @date: 2020/05
#===========================================

rm(list=ls())

# ------------------------
# 1. Setup
# ------------------------

# libraries
library(rgdal) # version 1.2.18
library(broom) # 0.4.4
library(dplyr) # 0.8.0.1
library(ggplot2) # 3.3.0.9000
library(ggsn) # 0.5.0
library(ggrepel) # 0.8.2
library(rgeos) # 0.5.2
library(scales) # 1.1.0
library(here)

# ------------------------
# 2. Prepare Geometries
# ------------------------

# Faroe island OpenStreetMap extract downloaded from http://glunimore.geofabrik.de/index.html and stored in /shapes subfolder.
# All needed shapes will be read rgdal library and reprojected to Web Mercator (EPSG: 3857).

# Read natural (areas)
gis_osm_natural_a_free_1 <- readOGR(dsn = here("shapes", "faroe-islands-latest-free.shp"), layer = "gis_osm_natural_a_free_1")
gis_osm_natural_a_free_1 <- spTransform(gis_osm_natural_a_free_1, CRS=CRS("+init=epsg:3857"))

# Extract beaches and cliffs (areas)
beach_a <- gis_osm_natural_a_free_1[gis_osm_natural_a_free_1$fclass=="beach",]
cliff_a <- gis_osm_natural_a_free_1[gis_osm_natural_a_free_1$fclass=="cliff",]

# Read natural (points or lines)
gis_osm_natural_free_1 <- readOGR(dsn = here("shapes", "faroe-islands-latest-free.shp"), layer = "gis_osm_natural_free_1")
gis_osm_natural_free_1 <- spTransform(gis_osm_natural_free_1, CRS=CRS("+init=epsg:3857"))

# Extract beaches and cliffs (points or lines)
beach <- gis_osm_natural_free_1[gis_osm_natural_free_1$fclass=="beach",]
cliff <- gis_osm_natural_free_1[gis_osm_natural_free_1$fclass=="cliff",]

# Read shapes of main 18 islands (processed in get_data)
main18_islands <- readOGR(dsn = here("shapes", "main18_islands"), layer = "main18_islands")
main18_islands <- spTransform(main18_islands, CRS=CRS("+init=epsg:3857"))
plot(main18_islands, col = "orange")

# Add sysla (region) names with unicode
main18_islands$sysla_unicode <- c("Streymoy",
                                           "Streymoy",
                                           "Streymoy",
                                           "\u0056\u00e1\u0067\u0061\u0072",
                                           "Sandoy",
                                           "\u0053\u0075\u00f0\u0075\u0072\u006f\u0079",
                                           "\u004e\u006f\u0072\u00f0\u006f\u0079\u0061\u0072",
                                           "Streymoy",
                                           "\u0056\u00e1\u0067\u0061\u0072",
                                           "\u0053\u0075\u00f0\u0075\u0072\u006f\u0079",
                                           "\u004e\u006f\u0072\u00f0\u006f\u0079\u0061\u0072",
                                           "\u004e\u006f\u0072\u00f0\u006f\u0079\u0061\u0072",
                                           "\u004e\u006f\u0072\u00f0\u006f\u0079\u0061\u0072",
                                           "Eysturoy",
                                           "Sandoy",
                                           "Sandoy",
                                           "\u004e\u006f\u0072\u00f0\u006f\u0079\u0061\u0072",
                                           "\u004e\u006f\u0072\u00f0\u006f\u0079\u0061\u0072")

# Add island names with unicode
main18_islands$name_unicode <- c("\u004e\u00f3\u006c\u0073\u006f\u0079",
                                          "Hestur",
                                          "Koltur",
                                          "Mykines",
                                          "\u0053\u0074\u00f3\u0072\u0061 \u0044\u00ed\u006d\u0075\u006e",
                                          "\u004c\u00ed\u0074\u006c\u0061 \u0044\u00ed\u006d\u0075\u006e",
                                          "\u0053\u0076\u00ed\u006e\u006f\u0079",
                                          "Streymoy",
                                          "\u0056\u00e1\u0067\u0061\u0072",
                                          "\u0053\u0075\u00f0\u0075\u0072\u006f\u0079",
                                          "Kalsoy",
                                          "\u0056\u0069\u00f0\u006f\u0079",
                                          "Fugloy",
                                          "Eysturoy",
                                          "Sandoy",
                                          "\u0053\u006b\u00fa\u0076\u006f\u0079",
                                          "Kunoy",
                                          "\u0042\u006f\u0072\u00f0\u006f\u0079")

# Read places (points or lines)
gis_osm_places_free_1 <- readOGR(dsn = here("shapes", "faroe-islands-latest-free.shp"), layer = "gis_osm_places_free_1")
gis_osm_places_free_1 <- spTransform(gis_osm_places_free_1, CRS=CRS("+init=epsg:3857"))

# Extract capital of Faroe islands
capital <- gis_osm_places_free_1[gis_osm_places_free_1$name=="Tórshavn",]

# Read roads
gis_osm_roads_free_1 <- readOGR(dsn = here("shapes", "faroe-islands-latest-free.shp"), layer = "gis_osm_roads_free_1")
gis_osm_roads_free_1 <- spTransform(gis_osm_roads_free_1, CRS=CRS("+init=epsg:3857"))

# Read waterways (areas)
gis_osm_water_a_free_1 <- readOGR(dsn = here("shapes", "faroe-islands-latest-free.shp"), layer = "gis_osm_water_a_free_1")
gis_osm_water_a_free_1 <- spTransform(gis_osm_water_a_free_1, CRS=CRS("+init=epsg:3857"))

# Read waterways (points or lines)
gis_osm_waterways_free_1 <- readOGR(dsn = here("shapes", "faroe-islands-latest-free.shp"), layer = "gis_osm_waterways_free_1")
gis_osm_waterways_free_1 <- spTransform(gis_osm_waterways_free_1, CRS=CRS("+init=epsg:3857"))

# Extract single islands
nolsoy <- main18_islands[main18_islands$name=="Nolsoy",]
hestur <- main18_islands[main18_islands$name=="Hestur",]
koltur <- main18_islands[main18_islands$name=="Koltur",]
mykines <- main18_islands[main18_islands$name=="Mykines",]
stora_dimun <- main18_islands[main18_islands$name=="Stora Dimun",]
litla_dimun <- main18_islands[main18_islands$name=="Litla Dimun",]
svinoy <- main18_islands[main18_islands$name=="Svinoy",]
streymoy <- main18_islands[main18_islands$name=="Streymoy",]
vagar <- main18_islands[main18_islands$name=="Vagar",]
suduroy <- main18_islands[main18_islands$name=="Suduroy",]
kalsoy <- main18_islands[main18_islands$name=="Kalsoy",]
vidoy <- main18_islands[main18_islands$name=="Vidoy",]
fugloy <- main18_islands[main18_islands$name=="Fugloy",]
eysturoy <- main18_islands[main18_islands$name=="Eysturoy",]
sandoy <- main18_islands[main18_islands$name=="Sandoy",]
skuvoy <- main18_islands[main18_islands$name=="Skuvoy",]
kunoy <- main18_islands[main18_islands$name=="Kunoy",]
bordoy <- main18_islands[main18_islands$name=="Bordoy",]

# Fortify main islands polygons
main18_islands_ff <- broom::tidy(main18_islands, region = "osm_id")
main18_islands_ff <- dplyr::left_join(x = main18_islands_ff, main18_islands@data, by = c("id" = "osm_id"))

# Create a point for Vagar airport
airport_df <- data.frame(lon=-7.277253, lat=62.067428)
airport <- SpatialPoints(airport_df, proj4string = CRS("+init=epsg:4326"))
airport <- spTransform(airport, CRS=CRS("+init=epsg:3857"))

# ---------------------------------
# 3. Prepare plot style and annotations
# ---------------------------------

# set fonts
windowsFonts(
  Impact=windowsFont("Impact"),
  Times=windowsFont("TT Times New Roman"),
  ArialBlack=windowsFont("Arial Black"),
  BookmanOldStyle=windowsFont("Bookman Old Style"),
  ComicSansMS=windowsFont("Comic Sans MS"),
  OldEngText=windowsFont("Old English Text MT"),
  Matura=windowsFont("Matura MT Script Capitals"),
  Kunstler=windowsFont("Kunstler Script"),
  HighTower=windowsFont("High Tower Text"),
  Goudy=windowsFont("Goudy Old Style"),
  Symbol=windowsFont("Symbol")
)

# Colors
col_highlighter <- "#f35029"

# -----------------------------------------------------------
# Plot - distances to norway, island and Scotland
# -----------------------------------------------------------

# set plot zoom
mxmin <- main18_islands@bbox[1,1]-20000
mxmax <- main18_islands@bbox[1,2]+20000
mymin <- main18_islands@bbox[2,1]-3000
mymax <- main18_islands@bbox[2,2]+3000

coord <- coord_equal(xlim = c(mxmin, mxmax), ylim = c(mymin, mymax), expand = FALSE)
asp <- coord$aspect(list(x.range = range(c(mxmin, mxmax)), y.range = range(c(mymin, mymax))))
asp # e.g. Instagram maximum Height/Width ratio is 1.25

p <- ggplot() +
  coord_equal(xlim = c(mxmin, mxmax), ylim = c(mymin, mymax), expand = F) +
  # land areas
  geom_polygon(data = main18_islands, aes(x = long, y = lat, group=group), fill="orange", col = "black") +
  # water areas
  geom_polygon(data = gis_osm_water_a_free_1, aes(x = long, y = lat, group=group), fill="RoyalBlue4") +
  # water lines
  geom_path(data = gis_osm_waterways_free_1, aes(x = long, y = lat, group=group), col="RoyalBlue4") +
  # Theme
  theme(
    axis.title = element_blank(),
    axis.text =  element_blank(), 
    axis.ticks= element_blank(),
    axis.ticks.length = unit(0, "pt"),
    legend.box.spacing = unit(0, "mm"),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    plot.margin=grid::unit(c(0, 0, 0, 0), "mm"),
    plot.background = element_rect(fill="green"),
    panel.background = element_rect(fill="RoyalBlue4"),
    panel.border = element_blank()
  ) +
  # Title
  geom_text(data = as.data.frame(list(x = mxmin+30000, y = mymax-5000)),
            aes(x = x, y = y),
            hjust = "center",
            vjust = "top",
            label = "Faroe\nIslands",
            family = "BookmanOldStyle", size=11, fontface = "italic", col = "white") +
  # North symbol
  north(x.min=mxmin+10000, x.max=mxmax, y.min=mymin+10000, y.max=mymax, location = "bottomleft", symbol = 16, scale = 0.2) +
  # annotations Island
  geom_curve(aes(x = mxmin+40000, y = mymax-130000, 
                 xend = mxmin+15000, yend = mymax-100000),
             color = "black", size=.5,
             lty = 2,
             lineend = "round",
             curvature = -0.2,
             angle = 80,
             arrow = arrow(length = unit(0.03, "npc"))) +
  geom_text(data = as.data.frame(list(x = mxmin+17000, y = mymax-120000)),
            aes(x = x, y = y),
            hjust = "center",
            vjust = "top",
            label = "Island\n450Km",
            family = "BookmanOldStyle", size=4, col = "black") +
  # annotations Scotland
  geom_curve(aes(x = mxmax-25000, y = mymax-210000, 
                 xend = mxmax-15000, yend = mymax-230000),
             color = "black", size=.5,
             lty = 2,
             lineend = "round",
             curvature = -0.2,
             angle = 80,
             arrow = arrow(length = unit(0.03, "npc"))) +
  geom_text(data = as.data.frame(list(x = mxmax-35000, y = mymax-215000)),
            aes(x = x, y = y),
            hjust = "center",
            vjust = "top",
            label = "(Mainland)\nScotland\n320Km",
            family = "BookmanOldStyle", size=4, col = "black") +
  # annotations Norway
  geom_curve(aes(x = mxmax-40000, y = mymax-110000, 
                 xend = mxmax-15000, yend = mymax-100000),
             color = "black", size=.5,
             lty = 2,
             lineend = "round",
             curvature = -0.3,
             angle = 80,
             arrow = arrow(length = unit(0.03, "npc"))) +
  geom_text(data = as.data.frame(list(x = mxmax-30000, y = mymax-87000)),
            aes(x = x, y = y),
            hjust = "center",
            vjust = "top",
            label = "Norway\n670Km",
            family = "BookmanOldStyle", size=4, col = "black") +
  # Notes
  geom_text(data = as.data.frame(list(x = mxmin+2000, y = mymin+3000)),
            aes(x = x, y = y),
            label = "Data from \u00A9 OpenStreetMap contributors", 
            family = "BookmanOldStyle", size=3, col = "black", hjust="left") +
  # Margins (only to avoid error in border pixelation when export to PNG)
  geom_rect(aes(xmin = mxmin-9999, xmax = mxmin+10, ymin = mymin-9999, ymax = mymax+9999), fill = "RoyalBlue4") + 
  geom_rect(aes(xmin = mxmax-10, xmax = mxmax+9999, ymin = mymin-9999, ymax = mymax+9999), fill = "RoyalBlue4") + 
  geom_rect(aes(xmin = mxmin-9999, xmax = mxmax+9999, ymin = mymin-9999, ymax = mymin+10), fill = "RoyalBlue4")

p

# Save plot in PNG
w <- 2000
r <- 230
png(here("out", "faroe_distances.png"), bg = "RoyalBlue4", width = w, height = w*asp, res = r, units = "px") 
print(p)
dev.off()


# -----------------------------------------------------------
# Plot - Sysla
# -----------------------------------------------------------

mxmin <- main18_islands@bbox[1,1]-20000
mxmax <- main18_islands@bbox[1,2]+20000
mymin <- main18_islands@bbox[2,1]-3000
mymax <- main18_islands@bbox[2,2]+3000

coord <- coord_equal(xlim = c(mxmin, mxmax), ylim = c(mymin, mymax), expand = FALSE)
asp <- coord$aspect(list(x.range = range(c(mxmin, mxmax)), y.range = range(c(mymin, mymax))))
asp

p <- ggplot() +
  coord_equal(xlim = c(mxmin, mxmax), ylim = c(mymin, mymax), expand = F) +
  # land areas
  geom_polygon(data = main18_islands_ff, aes(x = long, y = lat, group=group, fill = sysla_unicode), col = "black") +
  # Capital city
  geom_text(data = data.frame(lon = capital@coords[1], lat = capital@coords[2]), 
            aes(x = lon, y = lat), label = "\u2605", 
            size = 8, family = "BookmanOldStyle", fontface = "bold", col = "red") +
  # Theme
  theme(
    axis.title = element_blank(),
    axis.text =  element_blank(), 
    axis.ticks= element_blank(),
    axis.ticks.length = unit(0, "pt"),
    legend.box.spacing = unit(0, "mm"),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    plot.margin=grid::unit(c(0, 0, 0, 0), "mm"),
    plot.background = element_rect(fill="green"),
    panel.background = element_rect(fill="white"),
    panel.border = element_blank(),
    legend.position = c(0.05, .45),
    legend.justification = "left",
    legend.background = element_rect(fill = "white"),
    legend.key.height=unit(1, "cm"),
    #legend.key = element_rect(colour = "#292929"),
    legend.direction = "vertical",
    #legend.box.background = element_rect(colour = "#292929"),
    legend.title = element_text(size = 20, family = "BookmanOldStyle", colour = "black"),
    legend.text = element_text(size = 18, family = "BookmanOldStyle", colour = "black")
  ) +
  # legend
  guides(fill=guide_legend(title="S\u00fd\u0073\u006c\u0061 (Region)")) +
  geom_text(data = as.data.frame(list(x = (mxmin+(mxmax-mxmin)*0.05)+8500, y = mymin+70000)),
            aes(x = x, y = y),
            label = "\u2605",
            family = "BookmanOldStyle", size=10, col = "red", fontface = "bold", hjust="right") +
  geom_text(data = as.data.frame(list(x = (mxmin+(mxmax-mxmin)*0.05)+10500, y = mymin+68500)),
            aes(x = x, y = y),
            label = "\u0054\u00f3\u0072\u0073\u0068\u0061\u0076\u006e, capital of\nthe Faroe Islands",
            family = "BookmanOldStyle", size=4, col = "black", hjust="left") +
  # Title
  geom_text(data = as.data.frame(list(x = mxmin+39000, y = mymax-5000)),
            aes(x = x, y = y),
            hjust = "center",
            vjust = "top",
            label = "\u0046\u00f8\u0072\u006f\u0079\u0061\u0072",
            family = "BookmanOldStyle", size=20, fontface = "bold", col = "black") +
  # North symbol
  north(x.min=mxmin+10000, x.max=mxmax, y.min=mymin+2000, y.max=mymax, location = "bottomleft", symbol = 16, scale = 0.2) +
  # Notes
  geom_text(data = as.data.frame(list(x = mxmin+140000, y = mymin+3000)),
            aes(x = x, y = y),
            label = "\u00A9 OpenStreetMap contributors", 
            family = "BookmanOldStyle", size=4, col = "black", hjust="left") +
  # Margins (only to avoid error when export to PNG)
  geom_rect(aes(xmin = mxmin-9999, xmax = mxmin+10, ymin = mymin-9999, ymax = mymax+9999), fill = "white") + 
  geom_rect(aes(xmin = mxmax-10, xmax = mxmax+9999, ymin = mymin-9999, ymax = mymax+9999), fill = "white") + 
  geom_rect(aes(xmin = mxmin-9999, xmax = mxmax+9999, ymin = mymin-9999, ymax = mymin+10), fill = "white")

p

# Save plot in PNG
w <- 2000
r <- 230
png(here("out", "faroe_sysla.png"), bg = "white", width = w, height = w*asp, res = r, units = "px") 
print(p)
dev.off()


# -----------------------------------------------------------
# Plot - Choroplet popoulation density
# -----------------------------------------------------------

mxmin <- main18_islands@bbox[1,1]-30000
mxmax <- main18_islands@bbox[1,2]+10000
mymin <- main18_islands@bbox[2,1]-3000
mymax <- main18_islands@bbox[2,2]+3000

coord <- coord_equal(xlim = c(mxmin, mxmax), ylim = c(mymin, mymax), expand = FALSE)
asp <- coord$aspect(list(x.range = range(c(mxmin, mxmax)), y.range = range(c(mymin, mymax))))
asp

# calculate population density
main18_islands_ff$density <- main18_islands_ff$p_20181/main18_islands_ff$are_km2

p <- ggplot() +
  coord_equal(xlim = c(mxmin, mxmax), ylim = c(mymin, mymax), expand = F) +
  # land areas
  geom_polygon(data = main18_islands_ff, aes(x = long, y = lat, group=group, fill = density), col = "black") +
  # Capital city
  geom_text(data = data.frame(lon = capital@coords[1], lat = capital@coords[2]), 
            aes(x = lon, y = lat), label = "\u2605", 
            size = 8, family = "BookmanOldStyle", fontface = "bold", col = "black") +
  # Theme
  theme(
    axis.title = element_blank(),
    axis.text =  element_blank(), 
    axis.ticks= element_blank(),
    axis.ticks.length = unit(0, "pt"),
    legend.box.spacing = unit(0, "mm"),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    plot.margin=grid::unit(c(0, 0, 0, 0), "mm"),
    plot.background = element_rect(fill="green"),
    panel.background = element_rect(fill="white"),
    panel.border = element_blank(), #element_rect(colour = "black", fill = NA, size = 1)
    legend.position = c(0.05, .45),
    legend.justification = "left",
    #legend.background = element_rect(fill = "white"),
    legend.key.height=unit(1, "cm"),
    #legend.key = element_rect(colour = "#292929"),
    legend.direction = "vertical",
    #legend.box.background = element_rect(colour = "#292929"),
    legend.title = element_text(size = 18, family = "BookmanOldStyle", colour = "black"),
    legend.text = element_text(size = 18, family = "BookmanOldStyle", colour = "black")
  ) +
  # legend
  scale_fill_gradient2() +
  guides(fill=guide_colourbar(barwidth = 1, barheight = 10, 
                              title = expression ("Pop. per"~Km^2), 
                              frame.colour = "black",
                              ticks = F)) +
  geom_text(data = as.data.frame(list(x = (mxmin+(mxmax-mxmin)*0.05)+8500, y = mymin+70000)),
            aes(x = x, y = y),
            label = "\u2605",
            family = "BookmanOldStyle", size=10, col = "black", fontface = "bold", hjust="right") +
  geom_text(data = as.data.frame(list(x = (mxmin+(mxmax-mxmin)*0.05)+10500, y = mymin+68500)),
            aes(x = x, y = y),
            label = "\u0054\u00f3\u0072\u0073\u0068\u0061\u0076\u006e, capital of\nthe Faroe Islands",
            family = "BookmanOldStyle", size=4, col = "black", hjust="left") +
  # Title
  geom_text(data = as.data.frame(list(x = mxmin+35000, y = mymax-5000)),
            aes(x = x, y = y),
            hjust = "center",
            vjust = "top",
            label = "Faroe\nIslands",
            family = "HighTower", size=20, col = "black") +
  # Annotations Litla Dimun
  geom_text_repel(data = as.data.frame(gCentroid(litla_dimun)@coords), aes(x = x, y = y), label = "Among the 18 major islands,\n\u004c\u00ed\u0074\u006c\u0061 \u0044\u00ed\u006d\u0075\u006e is the smallest\nand the only\nuninhabitated one",
                  nudge_y = -5000, nudge_x = 8000,
                  family = "BookmanOldStyle", size=3, col = "black", hjust="left") +
  # scale bar
  ggsn::scalebar(location = "bottomright", x.min = mxmin, x.max = mxmax-10000,
                 y.min = mymin+10000, y.max = mymax, dist = 10, dist_unit = "km", 
                 transform = FALSE, model="WGS84",
                 st.dist = 0.02, st.size = 4) +
  # Notes
  geom_text(data = as.data.frame(list(x = mxmin+5000, y = mymin+3000)),
            aes(x = x, y = y),
            label = "\u00A9 OpenStreetMap contributors", 
            family = "BookmanOldStyle", size=4, col = "black", hjust="left") +
  # Margins (only to avoid error when export to PNG)
  geom_rect(aes(xmin = mxmin-9999, xmax = mxmin+10, ymin = mymin-9999, ymax = mymax+9999), fill = "white") + 
  geom_rect(aes(xmin = mxmax-10, xmax = mxmax+9999, ymin = mymin-9999, ymax = mymax+9999), fill = "white") + 
  geom_rect(aes(xmin = mxmin-9999, xmax = mxmax+9999, ymin = mymin-9999, ymax = mymin+10), fill = "white")

p


# Save plot in PNG
w <- 2000
r <- 230
png(here("out", "faroe_density.png"), bg = "white", width = w, height = w*asp, res = r, units = "px") 
print(p)
dev.off()



# -----------------------------------------------------------
# Plot - Choroplet building density
# -----------------------------------------------------------

# plot
mxmin <- main18_islands@bbox[1,1]-30000
mxmax <- main18_islands@bbox[1,2]+10000
mymin <- main18_islands@bbox[2,1]-3000
mymax <- main18_islands@bbox[2,2]+3000

coord <- coord_equal(xlim = c(mxmin, mxmax), ylim = c(mymin, mymax), expand = FALSE)
asp <- coord$aspect(list(x.range = range(c(mxmin, mxmax)), y.range = range(c(mymin, mymax))))
asp

p <- ggplot() +
  coord_equal(xlim = c(mxmin, mxmax), ylim = c(mymin, mymax), expand = F) +
  # land areas
  geom_polygon(data = main18_islands_ff, aes(x = long, y = lat, group=group, fill = bldng_d), col = "black") +
  # road network
  geom_path(data = gis_osm_roads_free_1, aes(x = long, y = lat, group=group), col="orange") +
  # island names
  geom_text(data = as.data.frame(gCentroid(nolsoy)@coords), aes(x = x, y = y), 
            label = nolsoy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 0, nudge_x = 13000) +
  geom_text(data = as.data.frame(gCentroid(hestur)@coords), aes(x = x, y = y), 
            label = hestur$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 0, nudge_x = -12000) +
  geom_text(data = as.data.frame(gCentroid(koltur)@coords), aes(x = x, y = y), 
            label = koltur$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 0, nudge_x = -11000) +
  geom_text(data = as.data.frame(gCentroid(mykines)@coords), aes(x = x, y = y), 
            label = mykines$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = -5000, nudge_x = 0) +
  geom_text(data = as.data.frame(gCentroid(stora_dimun)@coords), aes(x = x, y = y), 
            label = stora_dimun$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 0, nudge_x = 18000) +
  geom_text(data = as.data.frame(gCentroid(litla_dimun)@coords), aes(x = x, y = y), 
            label = litla_dimun$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 0, nudge_x = 17000) +
  geom_text(data = as.data.frame(gCentroid(svinoy)@coords), aes(x = x, y = y), 
            label = svinoy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = -10500, nudge_x = 6000) +
  geom_text(data = as.data.frame(gCentroid(streymoy)@coords), aes(x = x, y = y), 
            label = streymoy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 1000, nudge_x = -2000) +
  geom_text(data = as.data.frame(gCentroid(vagar)@coords), aes(x = x, y = y), 
            label = vagar$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 2000, nudge_x = 0) +
  geom_text(data = as.data.frame(gCentroid(suduroy)@coords), aes(x = x, y = y), 
            label = suduroy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 0, nudge_x = -17000) +
  geom_text(data = as.data.frame(gCentroid(kalsoy)@coords), aes(x = x, y = y), 
            label = kalsoy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 13000, nudge_x = -4500) +
  geom_text(data = as.data.frame(gCentroid(vidoy)@coords), aes(x = x, y = y), 
            label = vidoy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 1000, nudge_x = 3000) +
  geom_text(data = as.data.frame(gCentroid(fugloy)@coords), aes(x = x, y = y), 
            label = fugloy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 9000, nudge_x = 0) +
  geom_text(data = as.data.frame(gCentroid(eysturoy)@coords), aes(x = x, y = y), 
            label = eysturoy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 0, nudge_x = -1000) +
  geom_text(data = as.data.frame(gCentroid(sandoy)@coords), aes(x = x, y = y), 
            label = sandoy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 3000, nudge_x = 0) +
  geom_text(data = as.data.frame(gCentroid(skuvoy)@coords), aes(x = x, y = y), 
            label = skuvoy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 0, nudge_x = -12500) +
  geom_text(data = as.data.frame(gCentroid(kunoy)@coords), aes(x = x, y = y), 
            label = kunoy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 1000, nudge_x = 500) +
  geom_text(data = as.data.frame(gCentroid(bordoy)@coords), aes(x = x, y = y), 
            label = bordoy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = -20000, nudge_x = 13000) +
  # Capital city
  geom_text(data = data.frame(lon = capital@coords[1], lat = capital@coords[2]), 
            aes(x = lon, y = lat), label = "\u2605", 
            size = 8, family = "BookmanOldStyle", fontface = "bold", col = "black") +
  # Theme
  theme(
    axis.title = element_blank(),
    axis.text =  element_blank(), 
    axis.ticks= element_blank(),
    axis.ticks.length = unit(0, "pt"),
    legend.box.spacing = unit(0, "mm"),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    plot.margin=grid::unit(c(0, 0, 0, 0), "mm"),
    plot.background = element_rect(fill="green"),
    panel.background = element_rect(fill="lightblue"),
    panel.border = element_blank(), #element_rect(colour = "black", fill = NA, size = 1)
    legend.position = c(0.05, .45),
    legend.background = element_rect(fill = "lightblue"),
    legend.justification = "left",
    legend.key.height=unit(1, "cm"),
    legend.direction = "vertical",
    legend.title = element_text(size = 18, family = "BookmanOldStyle", colour = "black"),
    legend.text = element_text(size = 18, family = "BookmanOldStyle", colour = "black")
  ) +
  # legend
  scale_fill_gradient2(high = muted("purple"), breaks = c(3, 15, 28), labels=c("3", "15", "28")) +
  guides(fill=guide_colourbar(barwidth = 1, barheight = 10, 
                              title = expression ("N. buildings per"~Km^2), 
                              frame.colour = "black",
                              ticks = F 
                                )) +
  geom_text(data = as.data.frame(list(x = (mxmin+(mxmax-mxmin)*0.05)+8500, y = mymin+70000)),
            aes(x = x, y = y),
            label = "\u2605",
            family = "BookmanOldStyle", size=10, col = "black", fontface = "bold", hjust="right") +
  geom_text(data = as.data.frame(list(x = (mxmin+(mxmax-mxmin)*0.05)+10500, y = mymin+68500)),
            aes(x = x, y = y),
            label = "\u0054\u00f3\u0072\u0073\u0068\u0061\u0076\u006e, capital of\nthe Faroe Islands",
            family = "BookmanOldStyle", size=4, col = "black", hjust="left") +
  geom_text(data = as.data.frame(list(x = (mxmin+(mxmax-mxmin)*0.05)+8500, y = mymin+60000)),
            aes(x = x, y = y),
            label = "\u2014",
            family = "BookmanOldStyle", size=10, col = "orange", fontface = "bold", hjust="right") +
  geom_text(data = as.data.frame(list(x = (mxmin+(mxmax-mxmin)*0.05)+10500, y = mymin+59000)),
            aes(x = x, y = y),
            label = "Road Network",
            family = "BookmanOldStyle", size=4, col = "black", hjust="left") +

  # Title
  geom_text(data = as.data.frame(list(x = mxmin+35000, y = mymax-5000)),
            aes(x = x, y = y),
            hjust = "center",
            vjust = "top",
            label = "Faroe\nIslands",
            family = "HighTower", size=20, col = "black") +
  # scale bar
  ggsn::scalebar(location = "bottomright", x.min = mxmin, x.max = mxmax-10000,
                 y.min = mymin+10000, y.max = mymax, dist = 10, dist_unit = "km", 
                 transform = FALSE, model="WGS84",
                 st.dist = 0.02, st.size = 4) +
  # Notes
  geom_text(data = as.data.frame(list(x = mxmin+5000, y = mymin+3000)),
            aes(x = x, y = y),
            label = "Data from \u00A9 OpenStreetMap contributors, Apr. 2020", 
            family = "BookmanOldStyle", size=4, col = "black", hjust="left") +
  # Margins (only to avoid error when export to PNG)
  geom_rect(aes(xmin = mxmin-9999, xmax = mxmin+10, ymin = mymin-9999, ymax = mymax+9999), fill = "white") + 
  geom_rect(aes(xmin = mxmax-10, xmax = mxmax+9999, ymin = mymin-9999, ymax = mymax+9999), fill = "white") + 
  geom_rect(aes(xmin = mxmin-9999, xmax = mxmax+9999, ymin = mymin-9999, ymax = mymin+10), fill = "white")

p

# Save plot in PNG
w <- 2000
r <- 230
png(here("out", "faroe_building_density.png"), bg = "white", width = w, height = w*asp, res = r, units = "px") 
print(p)
dev.off()

# -----------------------------------------------------------
# Plot - Beaches and Cliffs
# -----------------------------------------------------------

# plot
mxmin <- main18_islands@bbox[1,1]-30000
mxmax <- main18_islands@bbox[1,2]+10000
mymin <- main18_islands@bbox[2,1]-3000
mymax <- main18_islands@bbox[2,2]+3000

coord <- coord_equal(xlim = c(mxmin, mxmax), ylim = c(mymin, mymax), expand = FALSE)
asp <- coord$aspect(list(x.range = range(c(mxmin, mxmax)), y.range = range(c(mymin, mymax))))
asp

p <- ggplot() +
  coord_equal(xlim = c(mxmin, mxmax), ylim = c(mymin, mymax), expand = F) +
  # land areas
  geom_polygon(data = main18_islands_ff, aes(x = long, y = lat, group=group), fill = "grey") +
  # water areas
  geom_polygon(data = gis_osm_water_a_free_1, aes(x = long, y = lat, group=group), fill="lightblue") +
  # water lines
  geom_path(data = gis_osm_waterways_free_1, aes(x = long, y = lat, group=group), col="lightblue") +
  # cliffs
  geom_point(data = as.data.frame(cliff@coords), aes(x = coords.x1, y = coords.x2), shape=21, size = 4, col = "#567836", fill="#567836", alpha = .75) +
  geom_point(data = as.data.frame(gCentroid(beach_a, byid = TRUE)@coords), aes(x = x, y = y), shape=21, size = 4, col = "#567836", fill="#567836", alpha = .75) +
  # beaches
  geom_point(data = as.data.frame(beach@coords), aes(x = coords.x1, y = coords.x2), shape=21, size = 2, col = "#D9BB51", fill="#D9BB51", alpha = .85) +
  geom_point(data = as.data.frame(gCentroid(beach_a, byid = TRUE)@coords), aes(x = x, y = y), shape=21, size = 2, col = "#D9BB51", fill="#D9BB51", alpha = .85) +
  # island names
  geom_text(data = as.data.frame(gCentroid(nolsoy)@coords), aes(x = x, y = y), 
            label = nolsoy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 0, nudge_x = 13000) +
  geom_text(data = as.data.frame(gCentroid(hestur)@coords), aes(x = x, y = y), 
            label = hestur$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 0, nudge_x = -12000) +
  geom_text(data = as.data.frame(gCentroid(koltur)@coords), aes(x = x, y = y), 
            label = koltur$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 0, nudge_x = -11000) +
  geom_text(data = as.data.frame(gCentroid(mykines)@coords), aes(x = x, y = y), 
            label = mykines$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = -5000, nudge_x = 0) +
  geom_text(data = as.data.frame(gCentroid(stora_dimun)@coords), aes(x = x, y = y), 
            label = stora_dimun$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 0, nudge_x = 18000) +
  geom_text(data = as.data.frame(gCentroid(litla_dimun)@coords), aes(x = x, y = y), 
            label = litla_dimun$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 0, nudge_x = 17000) +
  geom_text(data = as.data.frame(gCentroid(svinoy)@coords), aes(x = x, y = y), 
            label = svinoy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = -10500, nudge_x = 6000) +
  geom_text(data = as.data.frame(gCentroid(streymoy)@coords), aes(x = x, y = y), 
            label = streymoy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 1000, nudge_x = -2000) +
  geom_text(data = as.data.frame(gCentroid(vagar)@coords), aes(x = x, y = y), 
            label = vagar$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 2000, nudge_x = 0) +
  geom_text(data = as.data.frame(gCentroid(suduroy)@coords), aes(x = x, y = y), 
            label = suduroy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 0, nudge_x = -17000) +
  geom_text(data = as.data.frame(gCentroid(kalsoy)@coords), aes(x = x, y = y), 
            label = kalsoy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 13000, nudge_x = -4500) +
  geom_text(data = as.data.frame(gCentroid(vidoy)@coords), aes(x = x, y = y), 
            label = vidoy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 1000, nudge_x = 3000) +
  geom_text(data = as.data.frame(gCentroid(fugloy)@coords), aes(x = x, y = y), 
            label = fugloy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 9000, nudge_x = 0) +
  geom_text(data = as.data.frame(gCentroid(eysturoy)@coords), aes(x = x, y = y), 
            label = eysturoy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 0, nudge_x = -1000) +
  geom_text(data = as.data.frame(gCentroid(sandoy)@coords), aes(x = x, y = y), 
            label = sandoy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 3000, nudge_x = 0) +
  geom_text(data = as.data.frame(gCentroid(skuvoy)@coords), aes(x = x, y = y), 
            label = skuvoy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 0, nudge_x = -12500) +
  geom_text(data = as.data.frame(gCentroid(kunoy)@coords), aes(x = x, y = y), 
            label = kunoy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = 1000, nudge_x = 500) +
  geom_text(data = as.data.frame(gCentroid(bordoy)@coords), aes(x = x, y = y), 
            label = bordoy$name_unicode, size = 5, family = "BookmanOldStyle", fontface = "italic",
            nudge_y = -20000, nudge_x = 13000) +
  # Capital city
  geom_text(data = data.frame(lon = capital@coords[1], lat = capital@coords[2]), 
            aes(x = lon, y = lat), label = "\u2605", 
            size = 8, family = "BookmanOldStyle", fontface = "bold", col = "black") +
  # Airport
  geom_text(data = data.frame(lon = airport@coords[1], lat = airport@coords[2]), 
            aes(x = lon, y = lat), label = "\u2708", 
            size = 6, family = "BookmanOldStyle", fontface = "bold", col = "black") +
  # Theme
  theme(
    axis.title = element_blank(),
    axis.text =  element_blank(), 
    axis.ticks= element_blank(),
    axis.ticks.length = unit(0, "pt"),
    legend.box.spacing = unit(0, "mm"),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    plot.margin=grid::unit(c(0, 0, 0, 0), "mm"),
    plot.background = element_rect(fill="green"),
    panel.background = element_rect(fill="lightblue"),
    panel.border = element_blank(), #element_rect(colour = "black", fill = NA, size = 1)
    legend.position = c(0.05, .45),
    legend.background = element_rect(fill = "lightblue"),
    legend.justification = "left",
    legend.key.height=unit(1, "cm"),
    legend.direction = "vertical",
    legend.title = element_text(size = 18, family = "BookmanOldStyle", colour = "black"),
    legend.text = element_text(size = 18, family = "BookmanOldStyle", colour = "black")
  ) +
  # legend cliff
  geom_point(data = as.data.frame(list(x = (mxmin+(mxmax-mxmin)*0.05)+7500, y = mymin+110000)), aes(x = x, y = y), shape=21, size = 6, col = "#567836", fill="#567836", alpha = .75) +
  geom_text(data = as.data.frame(list(x = (mxmin+(mxmax-mxmin)*0.05)+10500, y = mymin+110000)),
            aes(x = x, y = y),
            label = "Cliffs",
            family = "BookmanOldStyle", size=4, col = "black", hjust="left") +
  # legend beach
  geom_point(data = as.data.frame(list(x = (mxmin+(mxmax-mxmin)*0.05)+7500, y = mymin+102000)), aes(x = x, y = y), shape=21, size = 4, col = "#D9BB51", fill="#D9BB51", alpha = .85) +
  geom_text(data = as.data.frame(list(x = (mxmin+(mxmax-mxmin)*0.05)+10500, y = mymin+102000)),
          aes(x = x, y = y),
          label = "Beaches",
          family = "BookmanOldStyle", size=4, col = "black", hjust="left") +
  # legend airport
  geom_text(data = as.data.frame(list(x = (mxmin+(mxmax-mxmin)*0.05)+8500, y = mymin+82000)),
            aes(x = x, y = y),
            label = "\u2708",
            family = "BookmanOldStyle", size=7, col = "black", fontface = "bold", hjust="right") +
  geom_text(data = as.data.frame(list(x = (mxmin+(mxmax-mxmin)*0.05)+10500, y = mymin+80500)),
            aes(x = x, y = y),
            label = "V\u00e1gar Aiport (FAE)",
            family = "BookmanOldStyle", size=4, col = "black", hjust="left") +
  # legend capital
  geom_text(data = as.data.frame(list(x = (mxmin+(mxmax-mxmin)*0.05)+8500, y = mymin+70000)),
            aes(x = x, y = y),
            label = "\u2605",
            family = "BookmanOldStyle", size=8, col = "black", fontface = "bold", hjust="right") +
  geom_text(data = as.data.frame(list(x = (mxmin+(mxmax-mxmin)*0.05)+10500, y = mymin+68500)),
            aes(x = x, y = y),
            label = "\u0054\u00f3\u0072\u0073\u0068\u0061\u0076\u006e, capital of\nthe Faroe Islands",
            family = "BookmanOldStyle", size=4, col = "black", hjust="left") +
  # Title
  geom_text(data = as.data.frame(list(x = mxmin+35000, y = mymax-5000)),
            aes(x = x, y = y),
            hjust = "center",
            vjust = "top",
            label = "Faroe\nIslands",
            family = "HighTower", size=20, col = "black") +
  # scale bar
  ggsn::scalebar(location = "bottomright", x.min = mxmin, x.max = mxmax-10000,
                 y.min = mymin+10000, y.max = mymax, dist = 10, dist_unit = "km", 
                 transform = FALSE, model="WGS84",
                 st.dist = 0.02, st.size = 4) +
  # Notes
  geom_text(data = as.data.frame(list(x = mxmin+5000, y = mymin+3000)),
            aes(x = x, y = y),
            label = "Data from \u00A9 OpenStreetMap contributors, Apr. 2020", 
            family = "BookmanOldStyle", size=4, col = "black", hjust="left") +
  # Margins (only to avoid errors in border pixels when export to PNG)
  geom_rect(aes(xmin = mxmin-9999, xmax = mxmin+10, ymin = mymin-9999, ymax = mymax+9999), fill = "lightblue") + 
  geom_rect(aes(xmin = mxmax-10, xmax = mxmax+9999, ymin = mymin-9999, ymax = mymax+9999), fill = "lightblue") + 
  geom_rect(aes(xmin = mxmin-9999, xmax = mxmax+9999, ymin = mymin-9999, ymax = mymin+10), fill = "lightblue")

p


# Save plot in PNG
w <- 2000
r <- 230
png(here("out", "faroe_beach.png"), bg = "lightblue", width = w, height = w*asp, res = r, units = "px") 
print(p)
dev.off()
