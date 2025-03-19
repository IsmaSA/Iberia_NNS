# Code to create Supplementary Material 

#### Figures:  --------------------

## Fig S1. Overlap species composition  ------- (Option 1)

unique(df$Country)
world <- ne_countries(scale = "medium", returnclass = "sf") 
es<- geodata::gadm(
    country = c("ES"),level = 1, path = getwd() ) %>%
    sf::st_as_sf() %>%
    sf::st_cast("MULTIPOLYGON")  %>% 
    filter(!NAME_1 %in% c("Islas Baleares","Islas Canarias", "Ceuta y Melilla", "Azores", "Madeira"))
pt<- geodata::gadm(
    country = c("PT"),level = 1, path = getwd() ) %>%
    sf::st_as_sf() %>%
    sf::st_cast("MULTIPOLYGON") %>% 
    filter(!NAME_1 %in% c("Islas Baleares","Islas Canarias", "Ceuta y Melilla", "Azores", "Madeira"))
ad<-  world[world$name=="Andorra",]
world_states <- ne_states(returnclass = "sf")
gb <- world_states[world_states$name == "Gibraltar", ]

plot(gb)
col <- c("Spain" = "#F56455FF", "Portugal" = "#206a0c", 
         "Andorra" = "#cfc90e",  "Gibraltar" = "#ccc6c6")

shared_species
c <- "Gibraltar"
plots = list()
for(c in unique(df$Country)){
  shared <- shared_species[shared_species$Location1==c,]
  shared <- shared[,c(-7)]
  if(c=="Spain"){
    shape <- es
  } else if(c=="Portugal"){
    shape <- pt
  } else if(c=="Andorra"){
    shape <- ad
  } else if(c=="Gibraltar"){
    shape <- gb
  }
  shape <- st_transform(shape, crs = st_crs(4326))
  combined_geometry <- st_union(shape)
  combined_shape <- st_sf(geometry = st_sfc(combined_geometry), crs = st_crs(shape))
  plot(combined_shape, col = NA, border = "black")
 
 if(c=="Spain"){
    size= 0.3
  } else if(c=="Portugal"){ size= 0.2}else if(c=="Andorra"){ 
    size= 0.01}else if(c=="Gibraltar"){ 
      size= 0.001}
grid <- st_make_grid(combined_shape, cellsize = size,  square = TRUE)
  grid_sf <- st_as_sf(grid)
  grid_sf <- grid_sf[st_intersects(grid_sf, combined_shape, sparse = FALSE), ]

  plot(grid_sf)
  grid_sf <- grid_sf %>%  mutate(grid_id = 1:n())
  
  total_grids <- nrow(grid_sf)

if(c %in% c('Andorra', 'Gibraltar')){
shared$proportion <- shared$Overlap1_to_2 / sum(shared$Overlap1_to_2)
shared$grid_count <- round(shared$proportion * total_grids)
shared$grid_count[which.max(shared$grid_count)] <- 
  total_grids - sum(shared$grid_count[-which.max(shared$grid_count)])
categories <- unlist(mapply(rep, shared$Location2, shared$grid_count))

  grid_sf$Country <- categories
} else{
  z = 100- sum(shared$Overlap1_to_2)
  shared =  rbind(shared, data.frame(Location1 = c, Location2 = c,Shared = 0, Total1 = 0, Total2 = 0, Overlap1_to_2 = z))
  shared$proportion <- shared$Overlap1_to_2 /100
  shared$grid_count <- round(shared$proportion * total_grids)
  shared$grid_count[which.max(shared$grid_count)] <- 
    total_grids - sum(shared$grid_count[-which.max(shared$grid_count)])

  categories <- unlist(mapply(rep, shared$Location2, shared$grid_count))
  grid_sf$Country <- categories
}
text_box_x <- st_bbox(es)$xmax - 1.7
text_box_y <- st_bbox(es)$ymax - 1.35
shared_text <- paste0(shared$Location2, ": ", round(shared$Overlap1_to_2, 2), "%")

  p1<- ggplot() +
    geom_sf(data = grid_sf, aes(fill = Country), color = "black") +
    geom_sf(data = combined_shape, fill = NA, color = "black") +  
    scale_fill_manual(values = col) + 
    theme_void() +
    labs(fill = "Country") +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12)  )
  p1
  plots[[c]] =p1
}
library(patchwork)
p=(plots[["Spain"]] + plots[["Portugal"]]) / 
  (plots[["Andorra"]] + plots[["Gibraltar"]]) + 
  plot_layout(heights = c(2, 0.8)) 

#Andri palette?
ggsave(plot= p, filename = "Shared.species.svg", device= "svg")

## Fig S1. Venn Diagram ------- (Option 1)

install.packages("ggVennDiagram")
install.packages("VennDiagram")
library(ggVennDiagram)
library(VennDiagram)

species_locations <- df %>%  dplyr::select(Country, Taxon) %>% distinct()

countries <- c("Spain", "Portugal", "Gibraltar", "Andorra")

species_list <- lapply(countries, function(country) {
  species_locations %>% filter(Country == country) %>%
 pull(Taxon) %>%  unique() } )

names(species_list) <- countries

location_colors <- c(
  "Spain" = "#ff7f00",
  "Portugal" = "#33a02c", 
  "Andorra" = "#e31a1c", 
  "Gibraltar" = "#1f78b4")

ggVennDiagram(species_list, 
              label_alpha = 1, 
              edge_lty = "solid",
              set_color = location_colors) +  # Use set_color instead of scale_fill_manual
  scale_fill_gradient(low = "white", high = "white") +  # Make fills transparent
  scale_color_manual(values = location_colors) +  # Apply colors to the borders
  theme_void()

ggsave(last_plot(), filename = "Venn.svg", device = "svg")



## Fig S2. Taxonomic groups ------------
head(df)

group_colors <- c(
  "Algae" = "#913003", "Amphibians" = "#7bc810", "Birds" = "#dae93d",
  "Bryophytes" = "#e78ac3", "Crustaceans" = "#a471ed", "Fishes" = "#8399e7",
  "Fungi" = "#b1d634", "Insects" = "#b3b3b3", "Mammals" = "#e5c494",
  "Microorganisms" = "#72c6b1", "Molluscs" = "#fb9a99", "Reptiles" = "#dd1a1a",
  "Vascular plants" = "#76b379", "Other invertebrates" = "#cab2d6")

colnames(df)[3] <-"Group"
df1= df %>% group_by(Country, Group) %>% summarise(n = n()) %>% mutate(
  Country = factor(Country, levels = c( "Spain", "Portugal","Andorra", "Gibraltar")) )

unique(df1$Group)
#install.packages("upstartr"); library(upstartr)

p<-ggplot(df1, aes(Group, n, fill = Group)) + geom_bar(stat = "identity",size= 0.2, color ="black")+
facet_wrap(vars(Country), scales = "free_x", strip.position = "top") +
  scale_fill_manual(values = group_colors) + coord_flip()+
  theme_bw() +  labs(x="", y="") +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none")
ggsave(p, filename = "FigS1.svg", width = 10, height = 5, dpi = 300)


#### Figure S3: Temporal focus ------------
# Data comes from temporal section

p = ggplot(res_anual, aes(x = year, y = n, color = ISO3, group = ISO3)) +
  ylim(0,30)+  xlim(1900,2025)+
  scale_x_continuous(breaks = c(1900,1920,1940,1960,1980,2000,2023), limits = c(1900, 2023))+
  theme_bw(base_size = 12) + coord_cartesian(clip = 'on')+
  geom_point(size = 2, alpha=0.35) + scale_fill_manual(values = c("grey70" = "lightgrey", "white" = "white"), guide = "none")+
  scale_color_manual(values = location_colors, name = "Country") +
  geom_line(aes(y = running_median, color = ISO3), size = 1.2, linetype = "solid") + 
  geom_line(data = res_comb_anual, aes(y = n), color = "black", linetype = "dashed", size = 1) + 
  labs(
    x = "Year", y = "Number of first records",
    color = "Location",  fill = "Location") +
  theme(
    axis.title = element_text(face = "bold"),
    panel.grid.minor.y = element_blank(),
    panel.grid.mayor.y = element_blank(),
    legend.position = c(0.08, 0.8), 
    legend.background = element_rect(fill = alpha("white", 0.4), color = "black", size = 0.5),
    axis.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  annotate("text", x = 1800, y = 20, label = "Industrial Revolution\n(1760–1840)", size = 3.5, color = "black") +
  annotate("text", x = 2014, y = 25, label = "EU Regulation\n1143/2014", size = 3.5, color = "black") +
  annotate("text", x = 1986, y = 20, label = "Spain and Portugal\n join EU\n (1986)", size = 3.5, color = "black") +
  geom_vline(xintercept = c(1800, 1986, 2014), linetype = "dashed", color = "grey50", size = 0.5, alpha=0.5) 

p


#### Figure S4: Spatial data (normalised) ------------

list.files(pattern = 'rds')

country <- c('Iberia', 'Andorra', 'Gibraltar')
c= 'Andorra'
marine <- st_read("./World_EEZ_v12_20231025/eez_v12.shp")
cities <- read_xlsx("worldcities.xlsx")
cities1 <- cities %>% filter(country %in% c("Andorra", "Gibraltar","Spain","Portugal")) %>%
  filter(capital %in% c("primary","admin")) %>% filter(!admin_name %in% c("Canary Islands","Azores"))
plots <- list()

for(c in country){  
  print(c)
  Sys.sleep(2)
  if(c =='Iberia'){
    points <- readRDS(paste0('./Database/Iberia_ES.rds') )
    points1 <- readRDS(paste0('./Database/Iberia_PT.rds') )
    ib = rbind(points,points1)
    points2 = read_xlsx("./Database/ibermis.xlsx") 
    colnames(points2)[4] = "species"
    colnames(points2)[12] = "decimalLatitude"
    colnames(points2)[13] = "decimalLongitude"
    points = rbind(ib[,c(1,7,8)], points2[,c(4,12,13)])
    code  <- c("PT", "ESP")
    
  } else if(c =='Andorra'){
    points <- readRDS(paste0('./Database/Iberia_AD.rds') )
    code  <- "AND"
  }else if(c =='Gibraltar'){ 
    points <- readRDS(paste0('./Database/Iberia_GI.rds') )
  }
  points_sf <- st_as_sf(points, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
  
  if (c=="Gibraltar") {
    world_states <- ne_states(returnclass = "sf")
    countries <- world_states[world_states$name == "Gibraltar", ]
  } else{
    countries<- geodata::gadm(
      country = c(code),level = 1, path = getwd() ) %>%
      sf::st_as_sf() %>%
      sf::st_cast("MULTIPOLYGON") %>%  st_transform(crs = st_crs(points_sf)) %>% 
      filter(!NAME_1 %in% c("Islas Baleares","Islas Canarias", "Ceuta y Melilla", "Azores", "Madeira"))
  }
  
  # Size of each boundary
  if (c=="Iberia") {
    spain = rvest::read_html('https://en.wikipedia.org/wiki/Autonomous_communities_of_Spain') %>% rvest::html_table() %>% .[[2]]
    spain = spain[,c(2,9)] 
    spain = spain %>% mutate(Area_km2 = str_replace(as.character(str_extract(`Area(km2)`, "^[0-9,]+")), ",", ".")) %>%
      mutate(Area_km2 = as.numeric(Area_km2))
    spain = spain[,c(1,3)] 
    pt = rvest::read_html('https://en.wikipedia.org/wiki/Districts_of_Portugal') %>% rvest::html_table() %>% .[[5]]
    pt = pt[,c(1,5)]
    pt = pt %>% mutate(`Area(km2)` = str_replace(as.character(str_extract(`Area(km2)`, "^[0-9,]+")), ",", "."))
    colnames(spain) = c('NAME_1', 'Area_km2')
    colnames(pt) = c('NAME_1', 'Area_km2')
    i <- rbind(spain,pt)
    setdiff(i$NAME_1, countries$NAME_1)
    i$NAME_1[i$NAME_1 == "Andalusia"] = 'Andalucía'
    i$NAME_1[i$NAME_1 == "Asturias"] = 'Principado de Asturias'
    i$NAME_1[i$NAME_1 == "Basque Country"] = 'País Vasco'
    i$NAME_1[i$NAME_1 == "Castile and León"] = 'Castilla y León'
    i$NAME_1[i$NAME_1 == "Catalonia"] = 'Cataluña'
    i$NAME_1[i$NAME_1 == "Murcia"] = 'Región de Murcia'
    i$NAME_1[i$NAME_1 == "Valencia"] = 'Comunidad Valenciana'
    i$NAME_1[i$NAME_1 == "Lisbon"] = 'Lisboa'
    i$NAME_1[i$NAME_1 == "Aragon"] = 'Aragón'
    i$NAME_1[i$NAME_1 == "Castilla–La Mancha"] = 'Castilla-La Mancha'
    i$NAME_1[i$NAME_1 == "Madrid"] = 'Comunidad de Madrid'
    i$NAME_1[i$NAME_1 == "Navarre"] = 'Comunidad Foral de Navarra'
    
    countries <- countries %>% left_join(i, by ="NAME_1")
    
  } else if(c=='Andorra'){
    area = rvest::read_html('https://en.wikipedia.org/wiki/Parishes_of_Andorra') %>% rvest::html_table() %>% .[[4]]
    area = area[-1,c(2,4)] %>% filter(as.numeric(Area) < 400)
    colnames(area) = c('NAME_1', 'Area_km2')
    setdiff(area$NAME_1, countries$NAME_1)
    area$NAME_1[area$NAME_1 == "Escaldes–Engordany"] = 'Escaldes-Engordany'
    area$NAME_1[area$NAME_1 == "La Massana[2]"] = 'La Massana'
    countries <- countries %>% left_join(area, by ="NAME_1")
  }else if(c=='Gibraltar'){
    area = '6.8' 
    countries$area_km2 = area
  }
  
  countries_geom <- st_geometry(countries)
  plot(st_geometry(countries_geom))
  
  if(c =="Iberia"){
    mar <- marine %>% filter(TERRITORY1 %in% c("Spain", "Portugal"))
    mar <- st_transform(mar, crs = st_crs(countries))
    mar <- mar %>% st_cast("POLYGON")
    mar_geom <- st_geometry(mar)
    area1 <- st_area(mar_geom)
    area1 <- as.numeric(area1) / 1e6
    mar$area_km2 =area1
  } else if(c =="Gibraltar"){ 
    mar <- marine %>% filter(TERRITORY1 %in% c("Gibraltar"))
    mar <- st_transform(mar, crs = st_crs(countries))
    mar_geom <- st_geometry(mar)
    mar_geom <- st_cast(mar, "MULTIPOLYGON") %>% st_geometry()
    plot(mar_geom)
    area1 <- st_area(mar_geom)
    area1 <- as.numeric(area1) / 1e6
    mar$area_km2 =area1
  }
  
  if(c !="Andorra"){
    mar_geom <- st_geometry(mar)
    mar_geom <- st_cast(mar, "MULTIPOLYGON") %>% st_geometry()
    countries_geom <- st_sf(geometry = c(st_geometry(countries_geom), mar_geom))
  }
  plot(countries_geom)
  
  if(c=="Gibraltar"){
    countries_geom <- countries_geom %>%filter(!st_is_empty(geometry)) %>% mutate(ID = row_number()) 
    points_joined <- st_join(points_sf, countries_geom)
    species_counts <- points_joined %>%
      group_by(ID) %>%
      summarise(species_count = n_distinct(species)) %>%
      st_drop_geometry()  
    combined_data <- countries_geom %>%  left_join(species_counts, by = "ID") %>% mutate(area_km2 = c('8.6',area1))
  }
  if(c=="Andorra"){
    countries_geom <- countries %>%filter(!st_is_empty(geometry)) %>% mutate(ID = row_number()) 
    points_joined <- st_join(points_sf, countries_geom)
    species_counts <- points_joined %>%
      group_by(ID, NAME_1) %>%
      summarise(species_count = n_distinct(species)) %>%
      st_drop_geometry()  
    combined_data <- countries_geom %>%  left_join(species_counts, by = "ID")
    colnames(combined_data)[12] = "area_km2"
  }
  if(c=="Iberia"){
    countries_geom <- countries_geom %>%filter(!st_is_empty(geometry)) %>% mutate(ID = row_number()) 
    points_joined <- st_join(points_sf, countries_geom)
    species_counts <- points_joined %>%
      group_by(ID) %>%
      summarise(species_count = n_distinct(species)) %>%
      st_drop_geometry()  
    combined_data <- countries_geom %>%  left_join(species_counts, by = "ID") 
    
    geom <- st_geometry(combined_data)
    area1 <- st_area(geom)
    area1 <- as.numeric(area1) / 1e6
    combined_data$area_km2 =area1
  }
  combined_data1 <- combined_data %>%
    mutate(area_km2 = as.numeric(area_km2), species_density = species_count / area_km2) 
  
  if(c =="Iberia"){
    cities2 <- cities1 %>% filter(iso2 %in% c("ES","PT")) %>%
      st_as_sf(coords = c("lng", "lat"), crs = st_crs(countries_geom))
    cities2 <- cities2 %>% filter(!city =='Funchal')%>% filter(city %in% c("Madrid","Lisbon"))
  } else if(c =="Andorra"){
    cities2 <- cities1 %>% filter(iso3 %in% c("AND")) %>%
      st_as_sf(coords = c("lng", "lat"), crs = st_crs(countries_geom))
  }else if(c =="Gibraltar"){
    cities2 <- cities1 %>% filter(iso2 %in% c("GI")) %>%
      st_as_sf(coords = c("lng", "lat"), crs = st_crs(countries_geom))
  }
  
  p1=ggplot(data = combined_data1) + 
    geom_sf(aes(fill = species_density), color = "black") +
    scale_fill_gradientn(
      colors = color_palette,
      name = "Number of species") +
    theme_void() +
    #labs(fill = "Number of species") +
    geom_sf(data = cities2, color = "black", size = 0.8) +  
    geom_sf_text(data = cities2, aes(label = city), color = "black", 
                 size = 3, fontface = "bold", nudge_y = 0.03) +
    theme(
      legend.position = "right",           
      legend.title = element_text(size = 10),  
      legend.text = element_text(size = 8),   
      legend.key.size = unit(0.4, "cm")      
    )+ guides(fill = guide_colorbar())
  p1
  plots[[c]] <- p1
}

plots[["Andorra"]]
plots[["Gibraltar"]]
plots[["Iberia"]]

library(patchwork)

final_plot <- wrap_plots(
  plots[["Andorra"]], 
  plots[["Iberia"]], 
  plots[["Gibraltar"]], 
  ncol = 1,  
  heights = c(0.8, 3, 0.5)  ) 

final_plot
ggsave(last_plot(), filename = "FigS4.Spatial.svg", device = "svg")  



#### Tables:  --------------------

#TableS1 and Table S2: Done manually

# Table S3 -----
setwd("/home/ismael-soto/Iberia_NNS")
list.files()

df <- readxl::read_xlsx(path = './Database/GBIF_backbone.xlsx')
head(df)
df = df %>% filter(Country %in% c('Spain', 'Portugal', 'Andorra', 'Gibraltar'))
df = df[!duplicated(df[,c('Country','LastSpeciesName')]), ]
names(df)
df1 = df[,c(1,2,14,16,17,3,5:8,10)]
writexl::write_xlsx(df1, 'TableS3.xlsx')


# Table S4 -----
es = readRDS('/home/ismael-soto/Desktop/ELZA/Iberia/Iberia_ES.rds')
pt = readRDS('/home/ismael-soto/Desktop/ELZA/Iberia/Iberia_PT.rds')
gi = readRDS('/home/ismael-soto/Desktop/ELZA/Iberia/Iberia_GI.rds')
ad = readRDS('/home/ismael-soto/Desktop/ELZA/Iberia/Iberia_AD.rds')

names(gi)
ib =rbind(es[,c(1,11)],pt[,c(1,11)],gi[,c(1,11)],ad[,c(1,11)])
a= ib %>% group_by(species) %>% summarise(n = n()) %>% arrange(-n)

names(df1)
colnames(df1)[6] = 'g'
a= df1 %>% group_by(LastSpeciesName, g) %>% summarise(n=n())



# split Spain data
setwd("C:/Users/Propietario/Downloads")
rds <- readRDS("Iberia_ES.rds")
rds<- rds[,c(1,7,8)]

chunk_size <- 270000
total_rows <- nrow(rds)
num_chunks <- ceiling(total_rows / chunk_size)

for (i in 1:num_chunks) {
  print(i)
  start_idx <- (i-1)*chunk_size + 1
  end_idx   <- min(i*chunk_size, total_rows)
  
  chunk_df <- rds[start_idx:end_idx, ]
  chunk_filename <- paste0("Iberia_ES", i, ".json")
  
  jsonlite::write_json(chunk_df, chunk_filename)
}
