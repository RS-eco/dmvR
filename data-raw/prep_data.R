library(readr); library(dplyr)
amphi <- read_csv2("extdata/amphibians.csv")
birds <- read_csv2("extdata/ter_birds.csv") %>% select(-code)
colnames(birds) <- c("binomial", "presence", "origin", "seasonal", "family_nam", 
                     "shape_Area", "order_name")
birds$class_name <- "AVES"
birds$kingdom_na <- "ANIMALIA"
birds$phylum_nam <- "CHORDATA"
mam <- read_csv2("extdata/ter_mammals.csv")
rep <- read_csv2("extdata/gard_reptiles.csv") %>% select(-c(Value, FID_2, TID, Group))
colnames(rep) <- c("binomial", "shape_Area")
rep$class_name <- "REPTILIA"
rep$kingdom_na <- "ANIMALIA"
rep$phylum_nam <- "CHORDATA"
species_info <- plyr::rbind.fill(list(amphi, birds, mam, rep))
colnames(species_info)
write_csv(species_info, "data/species_info.csv.xz")

library(readr); library(dplyr)
amphi <- read_csv("extdata/amphibians_dist.csv.xz")
birds <- read_csv("extdata/ter_birds_dist.csv.xz")
mam <- read_csv("extdata/ter_mammals_dist.csv.xz")
rep <- read_csv("extdata/gard_reptiles_dist.csv.xz")
species_data <- dplyr::bind_rows(list(amphi, birds, mam, rep))
colnames(species_data)
species_data$presence <- 1

# Subset species data by extent of Bavaria
load("extdata/bavaria.rda")
r_bav <- raster::rasterize(bavaria, raster::raster(ncols=720, nrows=360))
df_bav <- as.data.frame(raster::rasterToPoints(r_bav))
deu <- raster::getData('GADM', country='DEU', level=1, path="extdata")
r_deu <- raster::rasterize(deu, raster::raster(ncols=720, nrows=360))
r_deu <- raster::crop(r_deu, deu)
plot(r_deu)
df_deu <- as.data.frame(raster::rasterToPoints(r_deu))

length(unique(species_data$species))
species_wide <- species_data %>% inner_join(df_deu) %>% 
  select(x, y, species, presence) %>% 
  group_by(x, y) %>% tidyr::spread(species, presence)
write_csv(species_wide, "data/species_data.csv.xz")

library(stars)
env1 <- read_ncdf(list.files("extdata", pattern=".nc", full.names=T)[1])
env2 <- read_ncdf(list.files("extdata", pattern=".nc", full.names=T)[2])
env3 <- read_ncdf(list.files("extdata", pattern=".nc", full.names=T)[3])

writeRaster(env, "data/tas_ewembi_deu_1981_2010.nc", format="CDF")

library(sf)
deu <- raster::getData('GADM', country='DEU', level=1, path="data")
deu <- st_as_sf(deu)
st_write(deu, "data/gadm36_DEU_adm1.shp")
#deu <- rgdal::readOGR("data/gadm36_DEU_adm1.shp")
