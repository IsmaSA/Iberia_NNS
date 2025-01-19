install.packages('pacman')
pacman::p_load(sf,dplyr,tidyr,xlsx,writexl,readxl,sp, ggplot2,terra,raster, rnaturalearth,rnaturalearthdata, readr,rgbif)

### Iberia non-native species
setwd("C:/Users/Propietario/Desktop/ELZA/Iberia")
df <- read_xlsx(path = "./Iberia.xlsx")

### Iberia non-native species  LINUX
setwd("/home/ismael-soto/Desktop/ELZA/Iberia")
list.files()

df <- read_xlsx(path = './Database/Iberia.xlsx')
head(df)
df = df %>% filter(Location %in% c('Spain', 'Portugal', 'Andorra', 'Gibraltar'))
country = unique(df$Location)

folders = c('Database', 'Codes', 'Plots')
for(f in folders){
  if(!file.exists(f)) {
    dir.create(f)
    cat('Folder:', f, 'created', '\n')
  }
}


names(df)
length(unique(df$New_names))
unique(df$Location)

### GAVIA ------------
gavia = read_csv('/home/ismael-soto/Downloads/GAVIA_main_data_table.csv')
gavia = gavia %>% filter(StatusCat =='Established')

setdiff(df$New_names[df$Location == 'Spain'], gavia$Binomial[gavia$CountryName == 'Spain'])
a= setdiff(gavia$Binomial[gavia$CountryName == 'SPAIN'],   df$New_names[df$Location == 'Spain']) %>% as.data.frame()%>% mutate(Country='Spain')
b= setdiff(gavia$Binomial[gavia$CountryName == 'PORTUGAL'],   df$New_names[df$Location == 'Portugal'])%>% as.data.frame()%>% mutate(Country='Portugal')
c= setdiff(gavia$Binomial[gavia$CountryName == 'ANDORRA'],   df$New_names[df$Location == 'Andorra'])%>% as.data.frame()%>% mutate(Country='Andorra')
d= setdiff(gavia$Binomial[gavia$CountryName == 'GIBRALTAR'],   df$New_names[df$Location == 'Gibraltar'])%>% as.data.frame()%>% mutate(Country='Gibraltar')

data = rbind(a,b,c,d)


remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))
library(tabulizer)
pdf_file <- "/home/ismael-soto/Downloads/TableS1.pdf"
tables <- extract_tables(pdf_file, output = "data.frame")


###  Get GBIF key ------------
df1= df %>% filter(is.na(GBIF_key) )
names = unique(df1$New_names)

keys= data.frame()
n='Mitrella nomadica'
for (n in names) {
  data <- rgbif::occ_search(scientificName = n, limit = 1)
  if (!is.null(data$data) && all(c('scientificName', 'species', 'speciesKey') %in% colnames(data$data))) {
    extracted_data <- data$data[, c('scientificName', 'species', 'speciesKey')]
    extracted_data$old_name <- n
    keys <- rbind(keys, extracted_data)
  } else {
    keys <- rbind(keys, data.frame(scientificName = n, 
                                   species = NA, 
                                   speciesKey = NA, 
                                   old_name = n, 
                                   stringsAsFactors = FALSE))
  }}
names(data$data)
writexl::write_xlsx(keys, 'extra_keys1.xlsx')

res <- data.frame()
errors <- data.frame()
h <- 1
for (name in names) {
  cat(h, "/", length(unique(names)), "\n")  
  h <- h + 1

  tryCatch({
    data <- name_backbone(name = name)
    
    if (!is.null(data)) {
      data1 <- data[, c("scientificName", "canonicalName", "matchType")]
      data1$old_name <- name
      
      res <- rbind(res, data1)
    } else {
      warning("No result for name: ", name)
    }
  }, error = function(e) {
    errors <- rbind(errors, data.frame(name = name, error_message = e$message))
    cat("Error with name:", name, "\n") 
  })
}

###  backbone GBIF ------------
df1 <- df[is.na(df$Class),]
sp <- unique(df$New_names)
n
res <- data.frame()
for(n in names){
  tryCatch({
data<- name_backbone(name=n)
  data <- occ_search(scientificName = n, limit = 1)
  data<- data[["data"]]
names(data)
data$old_name = n
data1<- data[,c("old_name","family","class","phylum")]
res<- rbind(res,data1)
}, error = function(e) {
    cat("Error with name:", n, "\n") 
  })
}
writexl::write_xlsx(res, 'res.xlsx')


###  GBIF extraction ------------
country <- unique(df$Location)[4]

for(country in unique(df$Location)){
  
  df1 <- df[df$Location==country, ] %>% filter(GBIF_key > 0)

  keys <- unique(df1$GBIF_key)
  if(country=="Spain"){
    con = "ES"
  } else if(country=="Portugal"){
    con = "PT"
  }else if(country=="Gibraltar"){
    con = "GI"
  }else if(country=="Andorra"){
    con = "AD"
  }
  
  x <- occ_download(
    pred_in("taxonKey", keys ),  
    pred("country", con),
    pred("occurrenceStatus","PRESENT"),
    pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN"))),
    pred("hasCoordinate", TRUE),
    user = "ismaelsoto",
    pwd = "Ismaputas123.",
    email = "isma-sa@hotmail.com"
  )
    status <- occ_download_meta(x)$status
    
    while (status != "SUCCEEDED") {
      Sys.sleep(10)  
      status <- occ_download_meta(x)$status  
      print(occ_download_meta(x)$status)
    }
    cat("Download data for", country, "\n")
    z <- occ_download_meta(x)
    z1 <- z %>% as.character()
    z2 <- z1[1]
    
    #download .zip
    dat <- occ_download_get(z2, overwrite=T) %>%
      occ_download_import()
    
    }
  
# read GBIF downloads
files <- list.files(pattern = ".zip")
target_file <- "occurrence.txt"
res<- data.frame()
i <- files[1]

for(i in files){
  cat(i)

  tryCatch({
    unzipped_files <- unzip(i, list = TRUE)

     unzip(i, files = target_file)
        
    occurrence_data <- data.table::fread(target_file , 
        select = c("species","acceptedTaxonKey","year", "occurrenceStatus","basisOfRecord","hasCoordinate",
                   "decimalLatitude", "decimalLongitude",
              "coordinateUncertaintyInMeters","coordinatePrecision","countryCode"))

    
    cols_need <- c("species","acceptedTaxonKey","year", "occurrenceStatus","basisOfRecord","hasCoordinate","decimalLatitude", "decimalLongitude",
                   "coordinateUncertaintyInMeters","coordinatePrecision","countryCode")
    occurrence_data1 <- occurrence_data[, ..cols_need]
    
    missing_columns <- setdiff(cols_need, names(occurrence_data1))
    for (col in missing_columns) {
      occurrence_data1[, (col) := NA]  
    }
    
    occurrence_data1$name <- i
    code =unique(occurrence_data1$countryCode)

    saveRDS(occurrence_data1, paste0('Iberia_', code,'.rds') )
    
  }, error = function(e) {
    print(paste("F en el chat:", i, "Error:", e$message))
  })
}


## Spatial analyses ----- 

marine <- st_read("./World_EEZ_v12_20231025/eez_v12.shp")
cities <- read_xlsx("worldcities.xlsx")
cities1 <- cities %>% filter(country %in% c("Andorra", "Gibraltar","Spain","Portugal")) %>%
  filter(capital %in% c("primary","admin")) %>% filter(!admin_name %in% c("Balearic Islands","Canary Islands","Azores"))
con <- "Spain"

for(con in c("Spain", "Portugal", "Gibraltar", "Andorra")){
  
  if(con=="Spain"){
    
points <- read_rds("Iberia_ES.rds")
code  <- "ESP"
  } else if(con=="Portugal"){
points <- read_rds("Iberia_PT.rds")
code  <- "PT"

  }else if(con=="Gibraltar"){
points <- read_rds("Iberia_GI.rds")

  }else if(con=="Andorra"){
points <- read_rds("Iberia_AD.rds")
code  <- "AND"
  }
  
  points_sf <- st_as_sf(points, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

  if (con=="Gibraltar") {
    world_states <- ne_states(returnclass = "sf")
    countries <- world_states[world_states$name == "Gibraltar", ]
  } else{
  countries<- geodata::gadm(
    country = c(code),level = 1, path = getwd() ) %>%
    sf::st_as_sf() %>%
    sf::st_cast("MULTIPOLYGON") %>%  st_transform(crs = st_crs(points_sf)) %>% 
    filter(!NAME_1 %in% c("Islas Baleares","Islas Canarias", "Ceuta y Melilla", "Azores", "Madeira"))
  }

  if(con !="Andorra"){
    mar <- marine %>% filter(TERRITORY1 ==con)
    mar <- st_transform(mar, crs = st_crs(countries))
  }
  
  countries_geom <- st_geometry(countries)
  
  if(con !="Andorra"){
  mar_geom <- st_geometry(mar)
  countries_geom <- st_sf(geometry = c(countries_geom, mar_geom))
  }
  
  cities2 <- cities1 %>% filter(country ==con) %>%
    st_as_sf(coords = c("lng", "lat"), crs = st_crs(countries_geom))
  
  plot(st_geometry(countries_geom))
  
  cat("points & countries shapefile for:", con)
  
if(country !="Gibraltar"){
points_sf <- st_intersection(points_sf, countries_geom) # st_join()
}

  points_main <- as_Spatial(points_sf$geometry)

  point_extent <- extent(points_main)  
  buffer <- 0.01  
  
  density_raster <- raster(
    xmn = xmin(point_extent) - buffer, 
    xmx = xmax(point_extent) + buffer, 
    ymn = ymin(point_extent) - buffer, 
    ymx = ymax(point_extent) + buffer, 
    resolution = 0.045045,  # Set finer resolution to capture point density
    crs = "+proj=longlat +datum=WGS84"
  )
  
  density_raster <- rasterize(
    points_sf,   # SpatialPoints data
    density_raster,    # New raster template
    fun = "count",     # Counts points in each cell
    background = 0     # Sets cells without points to 0
  )
  
  density_raster[density_raster == 0] <- NA
  
  density_df <- as.data.frame(density_raster, xy = TRUE)
  colnames(density_df) <- c("longitude", "latitude", "density")
  
p1<-  ggplot() +
  geom_raster(data = density_df, aes(x = longitude, y = latitude, fill = density), na.rm = TRUE) +
    geom_sf(data = countries_geom, color = "black", fill = "NA") + 
    scale_fill_gradientn(
      colors = c("#FFFF00", "#FFEA00", "#FFA500", "#FF7F00", "#FF0000"), 
      values = scales::rescale(c(0, 0.33, 0.66, 1)),  
      na.value = "transparent",  # Make NA values transparent
      guide = guide_colorbar(
        barwidth = 1, barheight = 10, title.position = "top", title.hjust = 0.5
      )
    )  + theme_void() +  labs(fill = "Density") +
  geom_sf(data = cities2, color = "black", size = 1.5) +  # Adjust size for city points
  geom_sf_text(data = cities2, aes(label = city), color = "black", size = 3, fontface = "bold", nudge_y = 0.008) 
  ggsave(filename = paste0("Density_", con,".svg"), plot = p1,
       width = 8, height = 6, units = "in", device = "svg")

}

marine <- st_read("./World_EEZ_v12_20231025/eez_v12.shp")
cities <- read_xlsx("worldcities.xlsx")
cities1 <- cities %>% filter(country %in% c("Andorra", "Gibraltar","Spain","Portugal")) %>%
  filter(capital %in% c("primary","admin")) %>% filter(admin_name %in% c("Canary Islands","Azores", 'Madeira'))

con <- "Canary Islands"

for(con in c("Madeira", "Azores", "Canary Islands")){
  
  if(con=="Canary Islands"){
    
    points <- read_rds("Iberia_ES.rds")
    code  <- "ESP"
  } else if(con=="Azores"){
    points <- read_rds("Iberia_PT.rds")
    code  <- "PT"
    
  }else if(con=="Madeira"){
    points <- read_rds("Iberia_PT.rds")
    code  <- "PT"
  }
  
points_sf <- st_as_sf(points, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
  
countries<- geodata::gadm(
      country = c(code),level = 1, path = getwd() ) %>%
      sf::st_as_sf() %>%
      sf::st_cast("MULTIPOLYGON") %>%  st_transform(crs = st_crs(points_sf)) %>% 
  filter(!NAME_1 %in% c( "Ceuta y Melilla")) %>%
      filter(NAME_1 %in% c("Islas Canarias", "Azores", "Madeira"))
  
if(code=="PT") {
  countries = countries[countries$NAME_1 == con,]
}

mar <- marine %>% filter(TERRITORY1 ==con)
mar <- st_transform(mar, crs = st_crs(countries))
 
countries_geom <- st_geometry(countries)
mar_geom <- st_geometry(mar)
countries_geom <- st_sf(geometry = c(countries_geom, mar_geom))
  
cities2 <- cities1 %>% filter(admin_name ==con) %>%
    st_as_sf(coords = c("lng", "lat"), crs = st_crs(countries_geom))
  
  plot(st_geometry(countries_geom))
  
  cat("points & countries shapefile for:", con)
  
  if(country !="Gibraltar"){
    points_sf <- st_intersection(points_sf, countries_geom) # st_join()
  }
  
  points_main <- as_Spatial(points_sf$geometry)
  
  point_extent <- extent(points_main)  
  buffer <- 0.01  
  
  density_raster <- raster(
    xmn = xmin(point_extent) - buffer, 
    xmx = xmax(point_extent) + buffer, 
    ymn = ymin(point_extent) - buffer, 
    ymx = ymax(point_extent) + buffer, 
    resolution = 0.045045,  # Set finer resolution to capture point density
    crs = "+proj=longlat +datum=WGS84"
  )
  
  density_raster <- rasterize(
    points_sf,   # SpatialPoints data
    density_raster,    # New raster template
    fun = "count",     # Counts points in each cell
    background = 0     # Sets cells without points to 0
  )
  
  density_raster[density_raster == 0] <- NA
  
  density_df <- as.data.frame(density_raster, xy = TRUE)
  colnames(density_df) <- c("longitude", "latitude", "density")
  
  p1<-  ggplot() +
    geom_raster(data = density_df, aes(x = longitude, y = latitude, fill = density), na.rm = TRUE) +
    geom_sf(data = countries_geom, color = "black", fill = "NA") + 
    scale_fill_gradientn(
      colors = c("#FFFF00", "#FFEA00", "#FFA500", "#FF7F00", "#FF0000"), 
      values = scales::rescale(c(0, 0.33, 0.66, 1)),  
      na.value = "transparent",  # Make NA values transparent
      guide = guide_colorbar(
        barwidth = 1, barheight = 10, title.position = "top", title.hjust = 0.5
      )
    )  + theme_void() +  labs(fill = "Density") +
    geom_sf(data = cities2, color = "black", size = 1.5) +  # Adjust size for city points
    geom_sf_text(data = cities2, aes(label = city), color = "black", size = 3, fontface = "bold", nudge_y = 0.008) 
  ggsave(filename = paste0("Islands_", con,".svg"), plot = p1,
         width = 8, height = 6, units = "in", device = "svg")
  
}

#### Islands  ------


#### Plots spatial resolution  ------

setwd("D:/temporary/Iberia")

ff = list.files(pattern = 'Density_')
f= ff[10]

file = read_xlsx(f)
con = 'Andorra'
code  <- "ESP"
code  <- "PT"
code  <- "AND"



if (con=="Gibraltar") {
  world_states <- ne_states(returnclass = "sf")
  countries <- world_states[world_states$name == "Gibraltar", ]
} else{
  countries<- geodata::gadm(
    country = c(code),level = 1, path = getwd() ) %>%
    sf::st_as_sf() %>%
    sf::st_cast("MULTIPOLYGON")  %>% 
    filter(!NAME_1 %in% c("Islas Baleares","Islas Canarias", "Ceuta y Melilla", "Azores", "Madeira"))
}

if(con !="Andorra"){
  mar <- marine %>% filter(TERRITORY1 ==con)
  mar <- st_transform(mar, crs = st_crs(countries))
}

countries_geom <- st_geometry(countries)

if(con !="Andorra"){
  mar_geom <- st_geometry(mar)
  countries_geom <- st_sf(geometry = c(countries_geom, mar_geom))
}

cities2 <- cities1 %>% filter(country ==con) %>% filter(!city =='Funchal') %>%
  st_as_sf(coords = c("lng", "lat"), crs = st_crs(countries_geom) )
           
           
           
p1 = ggplot() +
  geom_raster(data = file, aes(x = longitude, y = latitude, fill = density), na.rm = TRUE) +
  geom_sf(data = countries_geom, color = "black", fill = "NA") + 
  scale_fill_gradientn(
    colors = c("#FFFF00", "#FFEA00", "#FFA500", "#FF7F00", "#FF0000"), 
    values = scales::rescale(c(0, 0.1, 0.2, 0.4, 0.6, 0.8, 1)),  
    trans = "log",  # Log scale for color contrast
    labels = scales::label_number(accuracy = 1),  # Round off legend labels
    na.value = "transparent",  # Handle NA values
    guide = guide_colorbar(
      barwidth = 0.5, barheight = 15,  # Slimmer, longer color bar for elegance
      title.position = "top", title.hjust = 0.5,  # Center title on color bar
      frame.colour = "black", frame.linewidth = 0.5  # Frame the color bar
    )) +
  theme_void() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Centralized, bold title
    legend.position = "right",  # Move legend to right for better visibility
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title for emphasis
    legend.text = element_text(size = 10)  # Adjust legend text size for readability
  ) +
  labs(fill = "Density GBIF occ") +
  geom_sf(data = cities2, color = "black", size = 1.5) +  
  geom_sf_text(data = cities2, aes(label = city), color = "black", size = 3, fontface = "bold", nudge_y = 0.009) +
  annotation_scale(location = "br", width_hint = 0.3, line_width = 0.5, bar_cols = c("black", "white")) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering(fill = c("grey40", "white")))
p1
ggsave(filename = paste0("Density_", con,".svg"), plot = p1,
       width = 8, height = 6, units = "in", device = "svg")  


#### Number of species in countries  ------
setwd("C:/Users/Propietario/Downloads/Iberia")

marine <- st_read("./World_EEZ_v12_20231025/eez_v12.shp")
cities <- read_xlsx("worldcities.xlsx")
cities1 <- cities %>% filter(country %in% c("Andorra", "Gibraltar","Spain","Portugal")) %>%
  filter(capital %in% c("primary","admin")) %>% filter(!admin_name %in% c("Canary Islands","Azores"))

con <- "Spain"

for(con in c("Spain", "Portugal", "Gibraltar", "Andorra")){
  
  if(con=="Spain"){
    
    points <- read_rds("Iberia_ES.rds")
    code  <- "ESP"
  } else if(con=="Portugal"){
    points <- read_rds("Iberia_PT.rds")
    code  <- "PT"
    
  }else if(con=="Gibraltar"){
    points <- read_rds("Iberia_GI.rds")
    
  }else if(con=="Andorra"){
    points <- read_rds("Iberia_AD.rds")
    code  <- "AND"
  }
  
  points_sf <- st_as_sf(points, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
  
  if (con=="Gibraltar") {
    world_states <- ne_states(returnclass = "sf")
    countries <- world_states[world_states$name == "Gibraltar", ]
  } else{
    countries<- geodata::gadm(
      country = c(code),level = 1, path = getwd() ) %>%
      sf::st_as_sf() %>%
      sf::st_cast("MULTIPOLYGON") %>%  st_transform(crs = st_crs(points_sf)) %>% 
      filter(!NAME_1 %in% c("Islas Baleares","Islas Canarias", "Ceuta y Melilla", "Azores", "Madeira"))
  }
  
  if(con !="Andorra"){
    mar <- marine %>% filter(TERRITORY1 ==con)
    mar <- st_transform(mar, crs = st_crs(countries))
  }
  
  countries_geom <- st_geometry(countries)
  
  if(con !="Andorra"){
    mar_geom <- st_geometry(mar)
    countries_geom <- st_sf(geometry = c(countries_geom, mar_geom))
  }
  
  cities2 <- cities1 %>% filter(country ==con) %>%
    st_as_sf(coords = c("lng", "lat"), crs = st_crs(countries_geom))
  
  cat("points & countries shapefile for:", con)
  
  colnames(mar)[6] <- "COUNTRY"
  colnames(mar)[2] <- "COUNTRY"
  colnames(countries)[4] <- "COUNTRY"
  
  combined_shapefile <- rbind(countries[,c(4,12)], mar[,c(2,32)])
  combined_shapefile = combined_shapefile[!is.na(combined_shapefile$COUNTRY), ]
  plot(st_geometry(combined_shapefile))
  
  combined_shapefile <- combined_shapefile %>% st_cast("POLYGON")
  
  # now 16
  points_sf <- st_transform(points_sf, st_crs(combined_shapefile))
  joined_data <- st_join(combined_shapefile, points_sf[c(1:50),])

  species_count <- joined_data %>%
    group_by(COUNTRY) %>%  
    summarise(unique_species = n_distinct(species))
  
#points_main <- as_Spatial(points_sf$geometry)
  color_palette <-c("#FFFF00", "#FFEA00", "#FFA500", "#FF7F00", "#FF0000")
  cities2 <- cities2 %>% filter(!city =='Funchal')
  
  ggplot(data = species_count) +
    geom_sf(aes(fill = unique_species), color = "black") +
    scale_fill_gradientn(
      colors = color_palette,
      name = "Number of Species"
    ) +
    theme_void() +
    labs(fill = "Number of species") +
    geom_sf(data = cities2, color = "black", size = 1.5) +  # Add city points
    geom_sf_text(data = cities2, aes(label = city), color = "black", 
                 size = 3, fontface = "bold", nudge_y = 0.008) +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 8),
      legend.title = element_text(face = "bold", size = 8) )
  
  
  ggsave(filename = paste0("Density_", con,".svg"), plot = p1,
         width = 8, height = 6, units = "in", device = "svg")
  
}


### Islands area  ----


marine <- st_read("./World_EEZ_v12_20231025/eez_v12.shp")
cities <- read_xlsx("worldcities.xlsx")
cities1 <- cities %>% filter(country %in% c("Andorra", "Gibraltar","Spain","Portugal")) %>%
  filter(capital %in% c("primary","admin")) %>% 
  filter(admin_name %in% c("Canary Islands","Azores","Madeira","Funchal"))

con <- "Madeira"

for(con in c("Canary Islands","Azores","Madeira")){
  
  if(con=="Canary Islands"){
    
    points <- read_rds("Iberia_ES.rds")
    code  <- "ESP"
    c<- "Spain"
  } else if(con=="Azores"){
    points <- read_rds("Iberia_PT.rds")
    code  <- "PT"
    c<- "Portugal"
  }else if(con=="Madeira"){
    points <- read_rds("Iberia_PT.rds") 
    code  <- "PT"
    c<- "Portugal"}

  
  points_sf <- st_as_sf(points, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
  
  countries<- geodata::gadm(
      country = c(code),level = 1, path = getwd() ) %>%
      sf::st_as_sf() %>%
      sf::st_cast("MULTIPOLYGON") %>%  st_transform(crs = st_crs(points_sf)) %>% 
      filter(NAME_1 %in% c("Islas Canarias", "Azores", "Madeira"))
  
  mar <- marine %>% filter(TERRITORY1 == con )
  mar <- st_transform(mar, crs = st_crs(countries))
  
  countries_geom <- st_geometry(countries)
  
    mar_geom <- st_geometry(mar)
    countries_geom <- st_sf(geometry = c(countries_geom, mar_geom))
  
  cities2 <- cities1 %>% filter(country ==c) %>%
    st_as_sf(coords = c("lng", "lat"), crs = st_crs(countries_geom))
  
  cat("points & countries shapefile for:", con)
  
  colnames(mar)[6] <- "COUNTRY"
  colnames(mar)[2] <- "COUNTRY"
  colnames(countries)[4] <- "COUNTRY"
  
  combined_shapefile <- rbind(countries[,c(4,12)], mar[,c(2,32)])
  combined_shapefile = combined_shapefile[!is.na(combined_shapefile$COUNTRY), ]
  plot(st_geometry(combined_shapefile))
  
  combined_shapefile <- combined_shapefile %>% st_cast("POLYGON")
  
  # now 16
  points_sf <- st_transform(points_sf, st_crs(combined_shapefile))
  joined_data <- st_join(combined_shapefile, points_sf[c(1:50),])
  
  species_count <- joined_data %>%
    group_by(COUNTRY) %>%  
    summarise(unique_species = n_distinct(species))
  
  #points_main <- as_Spatial(points_sf$geometry)
  color_palette <-c("#FFFF00", "#FFEA00", "#FFA500", "#FF7F00", "#FF0000")
  cities2 <- cities2 %>% filter(!city =='Funchal')
  
 p1<-  ggplot(data = species_count) +
    geom_sf(aes(fill = unique_species), color = "black") +
    scale_fill_gradientn(
      colors = color_palette,
      name = "Number of Species"
    ) +
    theme_void() +
    labs(fill = "Number of species") +
    geom_sf(data = cities2, color = "black", size = 1.5) +  # Add city points
    geom_sf_text(data = cities2, aes(label = city), color = "black", 
                 size = 3, fontface = "bold", nudge_y = 0.008) +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 8),
      legend.title = element_text(face = "bold", size = 8) )
  
  
  ggsave(filename = paste0("area_Island", con,".svg"), plot = p1,
         width = 8, height = 6, units = "in", device = "svg")
  
}

### tetrapods from cesar -----
cesar <- read.csv("C:/Users/Propietario/Downloads/neobiota-064-001-s001.csv")


#### Taxonomic resolution --------- 
df<- df %>% filter(!Taxon %in% c("Acridotheres sp.","Ommatotriton sp.",""))

df_general <- df
df$Location[df$Location =="Madeira"] <- "Portugal"
df$Location[df$Location =="Canary Islands"] <- "Spain"
df$Location[df$Location =="Balearic Islands"] <- "Spain"
df$Location[df$Location =="Azores"] <- "Portugal"

# df is with each specific region

length(unique(df$New_names) )

# species per country

df %>% group_by(Location) %>% summarise(Species =n_distinct(Taxon))
df_general %>% group_by(Location) %>% summarise(Species =n_distinct(Taxon))


# Phyla ------
length(unique(df$Phylum) )
a<- df %>% group_by(Location,Phylum) %>% summarise(Species =n_distinct(Taxon))
a<- df_general %>% filter(Location %in%c("Azores","Madeira","Canary Islands","Balearic Islands")) %>% group_by(Location,Phylum) %>% summarise(Species =n_distinct(Taxon))

# Classes ------
length(unique(df$Class) )
b<- df %>% group_by(Location,Class) %>% summarise(Species =n_distinct(Taxon))

# Families ------
length(unique(df$Family) )
c<-df %>% group_by(Location,Family) %>% summarise(Species =n_distinct(Taxon))
c %>% group_by(Location) %>% summarise(n=n())

# Shared species composition ------
species_locations <- df_general %>%
  dplyr::select(Location, Taxon) %>%
  distinct()  

location_pairs <- expand.grid(Location1 = unique(species_locations$Location), 
                              Location2 = unique(species_locations$Location)) %>%
  filter(Location1 != Location2) 

shared_species <- location_pairs %>%
  rowwise() %>%
  mutate(
    Shared = length(intersect(
      species_locations$Taxon[species_locations$Location == Location1],
      species_locations$Taxon[species_locations$Location == Location2]
    )),
    Total1 = n_distinct(species_locations$Taxon[species_locations$Location == Location1]),
    Total2 = n_distinct(species_locations$Taxon[species_locations$Location == Location2]),
    Overlap_Share = Shared / min(Total1, Total2)  * 100
  ) %>%
  ungroup()
macaronesian_pairs <- expand.grid(Location1 = unique(macaronesian_data$Location), 
                                  Location2 = unique(macaronesian_data$Location)) %>%
  filter(Location1 != Location2)  # Remove self-comparisons

# Step 3: Calculate shared species and overlap
shared_species_macaronesian <- macaronesian_pairs %>%
  rowwise() %>%
  mutate(
    Shared = length(intersect(
      macaronesian_data$Taxon[macaronesian_data$Location == Location1],
      macaronesian_data$Taxon[macaronesian_data$Location == Location2]
    )),
    Total1 = n_distinct(macaronesian_data$Taxon[macaronesian_data$Location == Location1]),
    Total2 = n_distinct(macaronesian_data$Taxon[macaronesian_data$Location == Location2]),
    Overlap_Share = Shared / min(Total1, Total2)  # Share relative to smaller set
  ) %>%
  ungroup()

# Repeated but for macaronesian islnas
macaronesian_islands <- c("Madeira", "Azores", "Canary Islands")  
macaronesian_data= df_general %>%
  filter(Location %in% macaronesian_islands) %>%
  dplyr::select(Location, Taxon) %>%
  distinct()
macaronesian_pairs <- expand.grid(Location1 = unique(macaronesian_data$Location), 
                                  Location2 = unique(macaronesian_data$Location)) %>%
  filter(Location1 != Location2)  # Remove self-comparisons

shared_species_macaronesian <- macaronesian_pairs %>%
  rowwise() %>%
  mutate(
    Shared = length(intersect(
      macaronesian_data$Taxon[macaronesian_data$Location == Location1],
      macaronesian_data$Taxon[macaronesian_data$Location == Location2]
    )),
    Total1 = n_distinct(macaronesian_data$Taxon[macaronesian_data$Location == Location1]),
    Total2 = n_distinct(macaronesian_data$Taxon[macaronesian_data$Location == Location2]),
    Overlap_Share = Shared / min(Total1, Total2)*100  # Share relative to smaller set
  ) %>%
  ungroup()

# groups ------
unique(df$Group)
df$Group[df$Group =="Dinoflagellata"] <- "Microorganisms"
df$Group[df$Group =="Viruses"] <- "Microorganisms"
df$Group[df$Group =="Bacteria and protozoans"] <- "Microorganisms"
df$Group[df$Group =="Invertebrates (excl. Arthropods, Molluscs)"] <- "Other invertebrates"
df$Group[df$Group =="Arthropods p.p. (Myriapods, Diplopods etc.)"] <- "Other invertebrates"

df %>% group_by(Group) %>% summarise(n =n_distinct(Taxon)) %>% arrange(-n) %>% head(n=5)
group2<- df_general %>% group_by(Location, Group) %>% summarise(Species =n_distinct(Taxon))

groups <- df %>% group_by(Location, Group) %>% summarise(n =n_distinct(Taxon)) %>% arrange(-n)

install.packages("paletteer")
my_colors <- paletteer::paletteer_d("lisa::FridaKahlo")
print(my_colors)
location_colors <- c("Andorra" = "#D32F2F", "Gibraltar" = "#004D40", 
                     "Portugal" = "#000000", "Spain" = "#ff7f00")

groups <- groups %>% filter(!Group =="NA")
ggplot(groups, aes(x = reorder(Location, -n, sum), y = n, color = Location)) +
  geom_segment(aes(xend = Location, yend = 0), size = 1) +  # Line part of the lollipop
  geom_point(size = 3.5) +        
  facet_wrap(~ Group, scales = "fixed") +                                     # Separate panel for each Group
  scale_y_log10(labels = scales::comma) +                   # Use log scale if necessary and format y-axis
  scale_color_manual(values = location_colors) +            # Apply custom colors to locations
  labs(x = "",y = "Number of species") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10), # Rotate x-axis labels
    axis.text.y = element_text(size = 10),
    legend.position = "none",                                     # Remove legend if not needed
    panel.grid.major.x = element_blank(),                         # Customize grid for clarity
    panel.grid.minor.x = element_blank()
  )



country <- "Andorra"
for(country in c("Spain","Portugal","Gibraltar", "Andorra")) {
  
  df1 <- df %>% filter(Location ==country) 
  df1 <- df1[, c("Taxon", "Phylum", "Class","Family")]
  
  
  df2 = df1 %>% group_by(Phylum, Class, Family) %>% summarise(n=n_distinct(Taxon))
  
  DF = df2
  
  require(data.table)
  
  colNamesDF <- names(DF)
  
  if(is.data.table(DF)){
    DT <- copy(DF)
  } else {
    DT <- data.table(DF, stringsAsFactors = FALSE)
  }
  
  if(add_root){
    DT[, root := "Total"]  
  }
  
  colNamesDT <- names(DT)
  hierarchy_columns <- setdiff(colNamesDT, value_column)
  DT[, (hierarchy_columns) := lapply(.SD, as.factor), .SDcols = hierarchy_columns]
  
  if(is.null(value_column) && add_root){
    setcolorder(DT, c("root", colNamesDF))
  } else if(!is.null(value_column) && !add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c(setdiff(colNamesDF, value_column), "values"))
  } else if(!is.null(value_column) && add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c("root", setdiff(colNamesDF, value_column), "values"))
  }
  
  hierarchyList <- list()
  
  for(i in seq_along(hierarchy_columns)){
    current_columns <- colNamesDT[1:i]
    if(is.null(value_column)){
      currentDT <- unique(DT[, ..current_columns][, values := .N, by = current_columns], by = current_columns)
    } else {
      currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=current_columns, .SDcols = "values"]
    }
    setnames(currentDT, length(current_columns), "labels")
    hierarchyList[[i]] <- currentDT
  }
  
  hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
  
  parent_columns <- setdiff(names(hierarchyDT), c("labels", "values", value_column))
  hierarchyDT[, parents := apply(.SD, 1, function(x){fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parent_columns]
  hierarchyDT[, ids := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
  hierarchyDT[, c(parent_columns) := NULL]
  
  p1=plot_ly(
    data = hierarchyDT,
    ids = ~ids,
    labels = ~labels,
    parents = ~parents,
    values = ~values,
    type = 'sunburst',
    # textinfo = "label+percent root",
    branchvalues = 'total',
    insidetextfont = list(color = 'black', size = 16, family = 'Arial'), # Inside text color and font
    outsidetextfont = list(color = 'black', size = 16, family = 'Arial')  )
  
  
  htmlwidgets::saveWidget(widget = p1, file = paste0(country,'.html') )
  webshot2::webshot(paste0(country,'.html'), file = paste0(country,'.pdf'), selector = "body")
 
}
 
 
# Habitats----

unique(df$Habitat)
 
df$Habitat[df$Habitat =="TERRESTRIAL|HOST"] <- "TERRESTRIAL"
df$Habitat[df$Habitat =="MARINE|HOST"] <- "MARINE"
df$Habitat[df$Habitat =="FRESHWATER|HOST"] <- "FRESHWATER"
df$Habitat[df$Habitat =="MARINE|MARINE"] <- "MARINE"
df$Habitat[df$Habitat =="FRESHWATER|MARINE|MARINE"] <- "FRESHWATER|MARINE"
df$Habitat[df$Habitat =="TERRESTRIAL|FRESHWATER|MARINE|MARINE"] <- "TERRESTRIAL|FRESHWATER|MARINE"
df$Habitat[df$Habitat =="TERRESTRIAL|FRESHWATER|HOST"] <- "TERRESTRIAL|FRESHWATER"
df$Habitat[df$Habitat =="FRESHWATER|MARINE|HOST"] <- "FRESHWATER|MARINE"

 
df$Habitat[df$Habitat =="TERRESTRIAL"] <- "Terrestrial"
df$Habitat[df$Habitat =="MARINE"] <- "Marine"
df$Habitat[df$Habitat =="TERRESTRIAL|FRESHWATER"] <- "Terrestrial|Freshwater"
df$Habitat[df$Habitat =="FRESHWATER"] <- "Freshwater"
df$Habitat[df$Habitat =="FRESHWATER|MARINE"] <- "Freshwater|Marine"
df$Habitat[df$Habitat =="TERRESTRIAL|FRESHWATER|MARINE"] <- "Terrestrial|Freshwater|Marine"
df$Habitat[df$Habitat =="TERRESTRIAL|MARINE"] <- "Terrestrial|Marine"
 
 
habitats <- df %>% group_by(Habitat, Location) %>% summarise(n = n_distinct(Taxon))
 
 
library(ggplot2)

habitats <- habitats %>%
  mutate(Location = factor(Location, levels = c("Spain", "Portugal", "Gibraltar", "Andorra")))
ggplot(habitats, aes(x = reorder(Habitat, +n), y = n, fill = Habitat)) +
  geom_bar(stat = "identity") +                                       # Use actual values of `n` for bar height
  facet_wrap(~ Location, scales = "free") +                         # Separate panels by Location with independent y-axis
  labs(
       x = "Habitats",
       y = "Number of Species") +
  paletteer::scale_fill_paletteer_d("lisa::OskarSchlemmer")+
theme_bw() +
  theme(
    axis.text.x = element_text( size = 12),   
    axis.text.xy = element_text( size = 12),                # Rotate x-axis labels for readability
    legend.position = "none",
    strip.text = element_text(size = 12)               # Customize facet label text
  ) + coord_flip()

## Native range ---------

unique(df$Native_range)

df1 <- df[!df$Native_range =="NA", ]
df2 <- df1 %>% separate_rows(Native_range, sep = "_") %>% filter(!is.na(Native_range))



regions <- sf::st_read("C:/Users/Propietario/Downloads/Ecoregions2017.shp")
regions <- regions[,c(5,9,10,16)]
regions$REALM <- tolower(regions$REALM)
regions$REALM[regions$REALM=="oceania"] <- "australasia"
regions$REALM[regions$REALM=="palearctic"] <- "paleartic"
regions$REALM[regions$REALM=="nearctic"] <- "neartic"
regions<- regions %>% mutate(geometry = st_make_valid(geometry)) %>% 
  group_by(REALM) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_as_sf()

con = "Portugal"
plots <- list()
library(geosphere)

for(con in c("Spain", "Portugal", "Andorra", "Gibraltar")) {
  
df22 <- df2 %>% filter(Location ==con)  
df3 <- df22 %>% group_by(Native_range) %>% summarise(species_count = n_distinct(Taxon)) %>%
  mutate(percentage = species_count/ sum(species_count) * 100)

df4 <- regions %>% left_join(df3, by = c("REALM" = "Native_range"))

df4_robinson <- st_transform(df4, crs = "+proj=robin")

robinson_proj <- "+proj=robin"

# Coordinates for Madrid
madrid <- c(lon = -3.7038, lat = 40.4168)

# Define starting points for each arrow (USA, Brazil, Africa, China, Thailand, Australia)
start_points <- data.frame(
  region = c("neartic", "neotropic", "afrotropic", "paleartic", "indomalayan", "australasia"),
  lon = c(-100, -55, 20, 104, 101, 133),
  lat = c(40, -10, 0, 35, 15, -25)
)

start_points = start_points %>% left_join(df4[,c(1,4)], by=c('region'='REALM') )
start_points = start_points[!duplicated(start_points$region), ] 

# Create great circle arcs between each point and Madrid
arc_lines <- lapply(1:nrow(start_points), function(i) {
  gcIntermediate(
    c(start_points$lon[i], start_points$lat[i]),
    madrid,
    n = 50,  # Number of points to create a smooth curve
    addStartEnd = TRUE,
    sp = TRUE
  )
})

# Convert arcs to an sf object and assign species_count
arc_sf <- do.call(rbind, lapply(1:length(arc_lines), function(i) {
  st_as_sf(arc_lines[[i]], crs = 4326) %>%
    mutate(species_count = start_points$species_count[i],
           region = start_points$region[i])
}))
 

# Project everything to Robinson projection
df4_robinson <- st_transform(df4_robinson, crs = robinson_proj)
arc_sf <- st_transform(arc_sf, crs = robinson_proj)

realm_colors <- c(
  "afrotropic" =  "#87CEEB",
  "neartic" = "#F4D77B",
  "paleartic" = "#F4A460",
  "neotropic" = "#3CB371",
  "australasia" = "#CBA7C3FF",
  "indomalayan" = "tomato1")
realm_colors2 <- c(
  "afrotropic" =  "#4682B4",   # Darker version of "#87CEEB"
  "neartic" = "#E3BC52",       # Darker version of "#F4D77B"
  "paleartic" = "#E27B3A",     # Darker version of "#F4A460"
  "neotropic" = "#2E8B57",     # Darker version of "#3CB371"
  "australasia" = "#B183A7",   # Darker version of "#CBA7C3FF"
  "indomalayan" = "#CD3818"    # Darker version of "tomato1"
)

df4_robinson <- df4_robinson[!df4_robinson$REALM =="antarctica", ]
df4_robinson <- df4_robinson[!df4_robinson$REALM =="n/a", ]

df4_robinson <- df4_robinson %>%
  mutate( arrow_size = case_when(
      species_count >= 500 ~ 5,
      species_count >= 200 ~ 4,
      species_count >= 100 ~ 3,
      species_count >= 50 ~ 2,
      TRUE ~ 1), label = paste0("# species ≥ ", round(species_count / 100) * 100) )

arc_sf <- arc_sf %>% st_join(df4_robinson[,c(1,3,5)], by = c("region" = "REALM"))

#arc_sf<- arc_sf %>% 
#  mutate( arrow_size = case_when(
#   species_count >= 500 ~ 5,
#   species_count >= 200 ~ 4,
#   species_count >= 100 ~ 3,
#   species_count >= 50 ~ 2,
#   TRUE ~ 1), label = paste0("# species ≥ ", round(species_count / 100) * 100) )

p1<- ggplot(data = df4_robinson) +
  geom_sf(aes(fill = REALM), color = NA) +
  scale_fill_manual(values = realm_colors) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(fill = "Number of established non-native species") +
  geom_sf(data = arc_sf, 
          aes(linewidth = arrow_size, color = region),
          alpha = 0.7, 
          lineend = "round") + 
  scale_size_continuous(
    range = c(0.5, 3),  
    name = "Arrow Thickness"
  ) +
  scale_color_manual(values = realm_colors2, name = "Native realm") +
  guides(
    color = guide_legend(override.aes = list(size = 3)) ) +
  theme( legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_line(color = "gray90", linetype = "dashed"),
    panel.border = element_blank() )

p1
plots[[con]] <- p1
}



# Now continue with arranging the plots as before
plots[["Spain"]] <- plots[["Spain"]] + theme(legend.position = "none")
plots[["Portugal"]] <- plots[["Portugal"]] + theme(legend.position = "none")
plots[["Andorra"]] <- plots[["Andorra"]] + theme(legend.position = "none")
plots[["Gibraltar"]] <- plots[["Gibraltar"]] + theme(legend.position = "none")

# Arrange the plots with the shared legend
final_plot <- plot_grid(
  plot_grid(plots[["Spain"]], plots[["Portugal"]], plots[["Gibraltar"]], plots[["Andorra"]],
            ncol = 2))

final_plot
(plots[["Spain"]] + plots[["Portugal"]]) / (plots[["Gibraltar"]] + plots[["Andorra"]])





#### Temporal study --------- 
res <- data.frame()
n = 'Andorra'
for (n in c('Spain', 'Portugal', 'Andorra', 'Gibraltar')) {
  df1 <- df[df$Location == n, ]
  names <- unique(df1$New_names)
  
  if (n == 'Spain') {
    code <- 'ESP'
  } else if (n == 'Portugal') {
    code <- 'PRT'
  } else if (n == 'Andorra') {
    code <- 'AND'
  } else if (n == 'Gibraltar') {
    code <- 'GIB'
  }
  
  for (name in names) {
    tryCatch({
      s <- get_first_introductions(name)
      s1 <- s %>% filter(ISO3 == code) 
      
      if (nrow(s1) > 0) {
        res <- rbind(res, data.frame("Species" = name, Location = n,"first_record" = s1$year))
      }
    }, error = function(e) {
      message(sprintf("Error  '%s' in  '%s': %s", name, n, e$message))
    })
  }
}

write_xlsx(res, 'First_records_Iberia.xlsx')

res = read_xlsx('First_records_Iberia.xlsx')
res = res %>% filter(!is.na(first_record))
res = res[res$first_record >=1400 ,]

res_anual = res %>% group_by(first_record, Location) %>% summarise(n=n())
res_cum <- res %>%
  group_by(Location, first_record) %>% 
  summarise(n = n(), .groups = "drop") %>% arrange(Location, first_record) %>%  
  group_by(Location) %>% mutate(cumulative_records = cumsum(n))


res_anual <- res_anual %>%
  group_by(Location) %>%
  mutate(running_median = zoo::rollapply(n, width = 10, FUN = median, fill = NA, align = "center"))

background <- data.frame(
  xmin = seq(1500, 2100, by = 50),
  xmax = seq(1500, 2150, by = 50),
  fill = rep(c("grey70", "white"), length.out = length(seq(1600, 2100, by = 50))))


location_colors <- c(
  "Spain" = "#ff7f00",
  "Portugal" = "#33a02c", 
  "Andorra" = "#e31a1c", 
  "Gibraltar" = "#1f78b4")


p1 = ggplot(res_anual, aes(x = first_record, y = n, color = Location, group = Location)) +
  geom_rect(data = background, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
            inherit.aes = FALSE, alpha = 0.5) + ylim(0,25)+  xlim(1500, 2023) +
   theme_bw(base_size = 12) +
  geom_point(size = 2, alpha=0.35) + scale_fill_manual(values = c("grey70" = "lightgrey", "white" = "white"), guide = "none")+
  scale_color_manual(values = location_colors) +
  geom_line(aes(y = running_median, color = Location), size = 1.2, linetype = "solid") + 
labs(
  x = "Year", y = "Number of first records",
  color = "Location",  fill = "Location") +
  theme(
    axis.title = element_text(face = "bold"),
    legend.position = c(0.08, 0.8), # Place legend inside the plot
    legend.background = element_rect(fill = alpha("white", 0.4), color = "black", size = 0.5),
    axis.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")  ) 



p2= ggplot(res_cum, aes(x = first_record, y = cumulative_records, color = Location, group = Location)) +
  geom_rect(data = background, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
            inherit.aes = FALSE, alpha = 0.5) + 
  theme_bw(base_size = 12) +
  scale_fill_manual(values = c("grey70" = "lightgrey", "white" = "white"), guide = "none") +
  scale_color_manual(values = location_colors) +
  geom_line(aes(y = cumulative_records, color = Location), size = 1.2, linetype = "solid") + 
  labs(
    x = "Year", y = "Cumulative number of first records",
    color = "Location",  fill = "Location") +
  theme(
    axis.title = element_text(face = "bold"),
    legend.position = c(0.08, 0.8), # Place legend inside the plot
    legend.background = element_rect(fill = alpha("white", 0.4), color = "black", size = 0.5),
    axis.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")  ) + xlim(1500, 2023) 

p1+p2


#### Pathways  --------- 

setwd("C:/Users/Propietario/Desktop/ELZA/Iberia")
df <- read_xlsx(path = "./Iberia.xlsx")
unique(df$Pathway)  
unique(df$Group)

df$Group[df$Group =="Dinoflagellata"] <- "Microorganisms"
df$Group[df$Group =="Viruses"] <- "Microorganisms"
df$Group[df$Group =="Bacteria and protozoans"] <- "Microorganisms"
df$Group[df$Group =="Invertebrates (excl. Arthropods, Molluscs)"] <- "Other invertebrates"
df$Group[df$Group =="Arthropods p.p. (Myriapods, Diplopods etc.)"] <- "Other invertebrates"


df1= df %>% group_by(Group, Pathway_refined, Location) %>% summarise(n =n())
df1= df1 %>% filter(!Pathway_refined =="NA")
df1= df1 %>% filter(!Group =="NA")

plots <-list()
for(con in c("Spain","Portugal","Gibraltar", "Andorra")) {
  df2 <- df1[df1$Location ==con, ]
 p<- ggplot(data = df2, aes(axis1 = Group, axis2 = Pathway_refined, y = n)) +
    geom_alluvium(aes(fill = Group), curve_type = "cubic", alpha = 0.8, show.legend = FALSE) +
    geom_stratum(color = "black", fill = "grey80", size = 0.3) +
   geom_text_repel(stat = "stratum", aes(label = after_stat(stratum)), 
                   size = 3, nudge_x = 0, direction = "y",  point.padding = 0.2,
                   segment.size = 0.15, box.padding = 0.2, max.overlaps = Inf) +
   scale_fill_manual(values = c(
      "Algae" = "#66c2a5", "Amphibians" = "#fc8d62", "Birds" = "#8da0cb",
      "Bryophytes" = "#e78ac3", "Crustaceans" = "#a6d854", "Fishes" = "#ffd92f",
      "Fungi" = "#e5c494", "Insects" = "#b3b3b3", "Mammals" = "#1f78b4",
      "Microorganisms" = "#33a02c", "Molluscs" = "#fb9a99", "Reptiles" = "#e31a1c",
      "Vascular plants" = "#fdbf6f", "Other invertebrates" = "#cab2d6"
    )) +
    labs(
      x = NULL, y = "",
      fill = "Group"
    ) +
    theme_void(base_size = 12) +
    theme( panel.grid = element_blank()    )
 plots[[con]] <- p
}

final_plot <- (
  plots[["Spain"]] + 
    labs(tag = "a)") + 
    theme(plot.tag = element_text( size = 10)) +
    plots[["Portugal"]] + 
    labs(tag = "b)") + 
    theme(plot.tag = element_text( size = 10))
) / (plots[["Gibraltar"]] + 
    labs(tag = "c)") + 
    theme(plot.tag = element_text( size = 10)) +
    plots[["Andorra"]] + 
    labs(tag = "d)") + 
    theme(plot.tag = element_text( size = 10)))
final_plot


### national list ----- 

df = read_xlsx('C:/Users/Propietario/Desktop/ELZA/Iberia/Lista_no nativas.xlsx')
df = df[!is.na(df$Species), ]
df <- df %>% mutate(Species = str_extract(Species, "^[\\w'-]+\\s+[\\w'-]+"))

elza = read_xlsx("C:/Users/Propietario/Desktop/ELZA/GLOBAL NNS DATA FINAL.xlsx")

a= setdiff(unique(df$Species), unique(elza$New_names)) # not present in Elza

setdiff( unique(elza$New_names), unique(df$Species)) # not present in national lists


df1 = df %>% filter(Species %in% a)
write_xlsx(df1, 'Missing.Species.Elza.xlsx')

### From sisco ----- 

sisco1 <- read_xlsx("Sisco1.xlsx", sheet = "Metadata")
sisco1<- sisco1[sisco1$Status=="Established", ]
sisco2 <- read_xlsx("sisco2.xlsx", sheet = "Sps_list")
sisco2<- sisco2[sisco2$Status=="Established", ]

setdiff(sisco1$`Scientific name`, df$Taxon)
j <- setdiff(sisco2$`Scientific Name`, df$Taxon)%>% as.data.frame()

f <- elza %>% filter(Continent == 'Europe') %>% 
  # filter(Location == 'Germany') %>% 
  filter(grepl("FRESHWATER", Habitat, ignore.case = TRUE) & !grepl("sp\\.", New_names) )
length(unique(f$New_names))







df1 <- df %>% filter(is.na(GBIF_key))
dois <-data.frame()
for (n in unique(df1$Taxon)) {
  # Try to fetch data and handle potential errors or missing data explicitly
  result <- tryCatch({
    sp <- occ_data(scientificName = n, hasCoordinate = TRUE, occurrenceStatus = "PRESENT", limit = 3)
    sp <- sp[["data"]]
    if (nrow(sp) > 0) {
      key <- sp$acceptedTaxonKey[1]
      dois <- rbind(dois, data.frame("Species" = n, "DOI" = key))
      cat(n, "---> ", "key:", key, "\n")
    } else {
      cat(n, "---> ", "No data found", "\n")
      dois <- rbind(dois, data.frame("Species" = n, "DOI" = NA))
    }
  }, error = function(e) {
    cat(n, "---> ", "Error:", e$message, "\n")
    dois <- rbind(dois, data.frame("Species" = n, "DOI" = NA))
  })
  cat(counter, "/", length(unique(df1$Taxon)), "\n")
  counter <- counter + 1
}
