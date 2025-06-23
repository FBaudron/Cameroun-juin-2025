#' ---
#' title: "Formation R - cartes & analyses spatiales"
#' author: "Frédéric Baudron"
#' date: "19-20 Juin 2025"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# INSTALLING NECESSARY PACKAGES-------------------------------------------------

if (!require('openxlsx')) install.packages("openxlsx")
if (!require('geodata')) install.packages("geodata")
if (!require('terra')) install.packages("terra")
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('ggspatial')) install.packages("ggspatial")
if (!require('tidyterra')) install.packages("tidyterra")


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(openxlsx)
library(geodata)
library(terra)
library(ggplot2)
library(ggspatial)
library(tidyterra)


# Set your directory (where your input files are located, and where your output
# files will be save)

setwd("D:\\Mes Donnees\\1. Cirad\\Cameroun\\Formation\\")

path = "D:\\Mes Donnees\\1. Cirad\\Cameroun\\Formation\\"

# Load input files

data = read.xlsx("2025_06_Tableau_formation_R.xlsx", sheet = 1)


# load rasters

elevation = geodata::elevation_30s("CMR", path = path)
terra::plot(elevation)
names(elevation) = "elevation"

prec = worldclim_country(country = 'CMR', var = 'prec', path = path, res = 5)
terra::plot(prec)


# mean annual precipitation

prec_sum = sum(prec)
names(prec_sum) = "prec"
terra::plot(prec_sum)


elevation
prec_sum

# Country shapefiles
cmr0 = gadm(country='CMR', level = 0, path = "gadm")
cmr1 = gadm(country='CMR', level = 1, path = "gadm")
cmr2 = gadm(country='CMR', level = 2, path = "gadm")
cmr3 = gadm(country='CMR', level = 3, path = "gadm")

terra::plot(cmr0)
terra::plot(cmr1)
terra::plot(cmr2)
terra::plot(cmr3)


# Ntui

ntui = terra::subset(cmr3, subset = cmr3$NAME_3 == "Ntui")
terra::plot(ntui)


# rasters croped for Ntui

elevation_ntui = crop(elevation, ntui)
elevation_ntui = mask(elevation_ntui, ntui)
terra::plot(elevation_ntui)

prec_sum_ntui = crop(prec_sum, ntui)
prec_sum_ntui = mask(prec_sum_ntui, ntui)
terra::plot(prec_sum_ntui)


# convert geolocations from dataframe to shapefile

data_sf = vect(data, geom = c("_Point.GPS_longitude", "_Point.GPS_latitude"), crs ="+proj=longlat + datum = WGS84 + no_defs")
data_sf = project(data_sf, "EPSG:4326")

terra::plot(ntui)
terra::plot(data_sf, add = TRUE)


# plot using ggplot2

# with elevation

elevation_ntui_df = terra::as.data.frame(elevation_ntui, xy = TRUE)

ggplot() + theme_bw() +
  geom_raster(data = na.omit(elevation_ntui_df), aes(x = x, y = y, fill = elevation), alpha = 1) +
  geom_spatvector(data = data_sf, shape = 19, color = "black", alpha = 1, size = 3) +
  geom_spatvector(data = ntui, color = "black", fill = NA, linewidth = 1) +
  scale_fill_distiller(palette = "RdYlGn") +
  labs(fill = "Altitude (m.a.s.l.)") +
  theme(plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = margin(10, 10, 10, 10)) +
  annotation_scale(location = "tr", width_hint = 0.5, height = unit(0.25, "cm"), 
                   text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 10))

ggsave("Carte Ntui altitude.png", units = "cm", width = 18, height = 20, dpi = 320)


# with annual rainfall

prec_sum_ntui_df = terra::as.data.frame(prec_sum_ntui, xy = TRUE)

ggplot() + theme_bw() +
  geom_raster(data = na.omit(prec_sum_ntui_df), aes(x = x, y = y, fill = prec), alpha = 1) +
  geom_spatvector(data = data_sf, shape = 19, color = "black", alpha = 1, size = 3) +
  geom_spatvector(data = ntui, color = "black", fill = NA, linewidth = 1) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  labs(fill = "Pluviométrie (mm)") +
  theme(plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = margin(10, 10, 10, 10)) +
  annotation_scale(location = "tr", width_hint = 0.5, height = unit(0.25, "cm"), 
                   text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 10))

ggsave("Carte Ntui pluviométrie.png", units = "cm", width = 18, height = 20, dpi = 320)


# EXTRACTION OF SPATIAL DATA----------------------------------------------------

stacked = c(elevation_ntui, prec_sum_ntui)

terra::plot(stacked)

ext_spdata  = terra::extract(stacked, data_sf)

data = cbind(data, ext_spdata[, -c(1)])








