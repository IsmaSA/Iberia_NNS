
### Code to use the clean database (LINUX) 

# Ismael Soto --- University of South Bohemia in Ceske Budejovice (USB)
Sys.time()

pacman::p_load(geodata, sf,dplyr,tidyr,xlsx,writexl,readxl,sp, ggplot2,stringr,terra,raster,stringr, rnaturalearth,rnaturalearthdata, readr,rgbif)

### Create the basic folders 
folders = c('Database', 'Codes', 'Plots')
for(f in folders){
  if(!file.exists(f)) {
    dir.create(f)
    cat('Folder:', f, 'created', '\n')
  }
}

### Iberia non-native species 
setwd("/home/ismael-soto/Desktop/ELZA/Iberia")

list.files()

df <- readxl::read_xlsx(path = './Database/GBIF_backbone.xlsx')
head(df)
df = df %>% filter(Country %in% c('Spain', 'Portugal', 'Andorra', 'Gibraltar'))
names(df)

df = df[!duplicated(df[,c('Country','LastSpeciesName')]), ]

# Check GBIF backbone: ----------
source(GBIF.FROV.r) # done in this code
GBIF = read_xlsx('/home/ismael-soto/Desktop/ELZA/Iberia/Database/GBIF_backbone.xlsx')
length(unique(GBIF$GBIF_key))

if(length(unique(df$GBIF_key)) ==1756){
  cat('Number of species match :)')} else{cat('check: there is a mismatch in sp names')}


# Check for missing information
nas <- sapply(df[, c("Family", "Class", "Phylum",'Taxonomic group','Pathway','Native_range')], function(column) {
  sum(is.na(column))
})
nas
table(df$Phylum)
table(df$Class)
table(df$Family)
table(df$'Taxonomic group')

# check if there is similar names: 
a=similar(df$Phylum)
a=similar(df$Class)
a=similar(df$Family)

b <- a$term_groups[duplicated(a$term_groups$Cluster), ]

df$Family[df$Family =='Apiceae'] ='Apiaceae'


# Overall
cat('Total species:', length(unique(df$LastSpeciesName)) ) # 1,727 sp
cat('Total species:', length(unique(df$Taxon)) ) # 1,831 sp
df %>% group_by(Country) %>% summarise(Species = n_distinct(LastSpeciesName))

# Phylum:
cat('Total Phylum:', length(unique(df$Phylum)) ) # 19 
df %>% group_by(Country) %>% summarise(Phylum = n_distinct(Phylum))

# Class:
cat('Total Class:', length(unique(df$Class)) ) # 52
df %>% group_by(Country) %>% summarise(Class = n_distinct(Class))

# Family:
cat('Total Family:', length(unique(df$Family)) ) # 398
df %>% group_by(Country) %>% summarise(Family = n_distinct(Family))


# Shared species composition
species_locations <- df %>% dplyr::select(Country, LastSpeciesName) %>%distinct()  

location_pairs <- expand.grid(Location1 = unique(species_locations$Country), 
                              Location2 = unique(species_locations$Country)) %>%
  filter(Location1 != Location2) 

shared_species <- location_pairs %>% rowwise() %>%
  mutate( Shared = length(intersect(
      species_locations$LastSpeciesName[species_locations$Country == Location1],
      species_locations$LastSpeciesName[species_locations$Country == Location2] )),
   Total1 = n_distinct(species_locations$LastSpeciesName[species_locations$Country == Location1]),
    Total2 = n_distinct(species_locations$LastSpeciesName[species_locations$Country == Location2]),
    Overlap_Share = Shared / min(Total1, Total2)  * 100 ) %>% ungroup()

## 2.0 (no overlap)
shared_species <- location_pairs %>% rowwise() %>% mutate(
    Shared = length(intersect(
      species_locations$LastSpeciesName[species_locations$Country == Location1],
      species_locations$LastSpeciesName[species_locations$Country == Location2]
    )),
    Total1 = n_distinct(species_locations$LastSpeciesName[species_locations$Country == Location1]),
    Total2 = n_distinct(species_locations$LastSpeciesName[species_locations$Country == Location2]),
    Overlap1_to_2 = (Shared / Total1) * 100,  # % of Location1's species found in Location2
    Overlap2_to_1 = (Shared / Total2) * 100   # % of Location2's species found in Location1
  ) %>% ungroup()


# Taxonomic composition (Groups)-----
table(df$'Taxonomic group')
unique(df$'Taxonomic group')

df$'Taxonomic group'[df$'Taxonomic group' =="mammals"] <- "Mammals"
df$'Taxonomic group'[df$'Taxonomic group' =="Amphibia"] <- "Amphibians"
df$'Taxonomic group'[df$'Taxonomic group' =="Insects"] <- "Insects"
df$'Taxonomic group'[df$'Taxonomic group' =="Aves"] <- "Birds"


df %>% group_by(df$'Taxonomic group') %>% summarise(Species = n_distinct(LastSpeciesName)) %>% arrange(-Species)
df %>% group_by(Country) %>% summarise(Species = n_distinct(LastSpeciesName)) %>% arrange(-Species)
a= df %>% group_by(Country,df$'Taxonomic group') %>% summarise(Species = n_distinct(LastSpeciesName)) %>% arrange(-Species)


# Taxonomic composition (Phylum, class & families)
unique(df$Phylum)
df %>% group_by(Phylum) %>% summarise(Species = n_distinct(LastSpeciesName)) %>% arrange(-Species)
a=df %>% group_by(Country,Phylum) %>% summarise(Species = n_distinct(LastSpeciesName)) %>% arrange(-Species)

df %>% group_by(Class) %>% summarise(Species = n_distinct(LastSpeciesName)) %>% arrange(-Species)
a=df %>% group_by(Country,Class) %>% summarise(Species = n_distinct(LastSpeciesName)) %>% arrange(-Species)

df %>% group_by(Family) %>% summarise(Species = n_distinct(LastSpeciesName)) %>% arrange(-Species)
a=df %>% group_by(Country,Family) %>% summarise(Species = n_distinct(LastSpeciesName)) %>% arrange(-Species)


## Figure 1  ------
df
pacman::p_load(plotly, viridis,webshot2, htmlwidgets)
add_root <- TRUE
value_column <- "n"
country <- "Spain"
pp <- list()
for(country in c("Spain","Portugal","Gibraltar", "Andorra")) {
  
  df1 <- df %>% filter(Country ==country) 
  df1 <- df1[, c("LastSpeciesName", "Phylum", "Class","Family")]
  
  df2 = df1 %>% group_by(Phylum, Class, Family) %>% summarise(n=n_distinct(LastSpeciesName))
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
unique(df2$Phylum)  

cols <- c(
  "Annelida" = "#E41A1C",       
  "Arthropoda" = "#b89437",     
  "Ascomycota" = "#4aaf72",     
  "Bryophyta" = "#984EA3",      
  "Bryozoa" = "#FF7F00",        
  "Cercozoa" = "#FFFF33",       
  "Chlorophyta" = "#A65628",    
  "Chordata" = "#8d81f7",       
  "Chytridiomycota" = "#999999", 
  "Cnidaria" = "#66C2A5",       
  "Ctenophora" = "#FC8D62",     
  "Mollusca" = "#8DA0CB",      
  "Myzozoa" = "#E78AC3",       
  "Nematoda" = "#A6D854",       
  "Ochrophyta" = "#FFD92F",     
  "Oomycota" = "#E5C494",      
  "Platyhelminthes" = "#B3B3B3", 
  "Rhodophyta" = "#CAB2D6",     
  "Tracheophyta" = "#14a34e"   
)

hierarchyDT$phylum <- sapply(strsplit(hierarchyDT$ids, " - "), function(x) {
  if (length(x) >= 2) x[2] else NA_character_ 
})
hierarchyDT$color <- cols[hierarchyDT$phylum]

p1 <- plot_ly(
  data = hierarchyDT,
  ids = ~ids,
  labels = ~labels,
  parents = ~parents,
  values = ~values,
  type = 'sunburst',
  branchvalues = 'total',
  marker = list(
    colors = ~color,  # Use assigned colors
    line = list(color = "white", width = 1)
  ),
  insidetextfont = list(color = 'black', size = 16, family = 'Arial'),
  outsidetextfont = list(color = 'black', size = 16, family = 'Arial')
)
p1
  pp[[country]] <- p1

# Run this with using windows 
htmlwidgets::saveWidget(widget = p1, 
file = file.path('/home/ismael-soto/Documents/GitHub/Iberia_NNS/Plots', paste0(country, '.html')), selfcontained = FALSE)

webshot2::webshot(
  file.path('/home/ismael-soto/Documents/GitHub/Iberia_NNS/Plots', paste0(country, '.html')), 
  file = file.path('/home/ismael-soto/Documents/GitHub/Iberia_NNS/Plots', paste0(country, '.pdf')),  
  selector = "body",
  browser = "/usr/bin/chromium-browser",  # or full path: "/usr/bin/chromium-browser"
)
 
 cat(country)
}
options(browser = "/snap/bin/chromium")


### Pathways of introduction -----

unique(df$Pathway)
table(df$Pathway)
df %>% filter(Pathway != "NA") %>% distinct(LastSpeciesName)  %>% nrow() 
df %>% filter(grepl("_", Pathway)) %>% distinct(LastSpeciesName)  %>% nrow() 

df1 = df %>% separate_rows(Pathway, sep= '_') %>% filter(!is.na(Pathway))
df1 = df1[!df1$Pathway == 'NA',]
table(df1$Pathway)

a= df1 %>% group_by(Country,Pathway) %>% summarise(Species= n_distinct(LastSpeciesName)) %>% arrange(-Species)

### Figure 3: -----
pacman::p_load(ggalluvial, ggrepel, patchwork)
group_colors <- c(
  "Algae" = "#913003", "Amphibians" = "#7bc810", "Birds" = "#dae93d",
  "Bryophytes" = "#e78ac3", "Crustaceans" = "#a471ed", "Fishes" = "#8399e7",
  "Fungi" = "#b1d634", "Insects" = "#b3b3b3", "Mammals" = "#e5c494",
  "Microorganisms" = "#72c6b1", "Molluscs" = "#fb9a99", "Reptiles" = "#dd1a1a",
  "Vascular plants" = "#76b379", "Other invertebrates" = "#cab2d6")
path <- "#cccccc"

plots <-list()
# I also need to run this plot in windows:
con = 'Spain'
for(con in c("Spain","Portugal","Gibraltar", "Andorra")) {
 colnames(df1)[3] = 'Group'
 df2 <- df1[df1$Country ==con, ]
 df2= df2 %>% group_by(Group,Pathway) %>% summarise(n= n_distinct(LastSpeciesName)) %>% arrange(-n)

 nodes <- data.frame(name = unique(c(df2$Group, df2$Pathway)))

df2 <- df2 %>%
  mutate(
    source = match(Group, nodes$name) - 1,
    target = match(Pathway, nodes$name) - 1,
    color = group_colors[Group])

node_colors <- c(
  group_colors[unique(df2$Group)],
  rep("#cccccc", length(unique(df2$Pathway))))

p <- plot_ly(
  type = "sankey",
  node = list(label = nodes$name, color = node_colors),  link = list(
    source = df2$source,
    target = df2$target,value = df2$n,
    color = df2$color  ))
 plots[[con]] <- p
 htmlwidgets::saveWidget(widget = p, file = file.path('./Plots', paste0(country, '_path.html')), selfcontained = FALSE)
webshot2::webshot(
  file.path('./Plots', paste0(country, '_path.html')), 
  file = file.path('./Plots', paste0(country, '_path.pdf')),  selector = "body")
}


### Native range ------
df

df1 <- df[!df$Native_range =="NA", ]
df2 <- df1 %>% separate_rows(Native_range, sep = "_") %>% filter(!is.na(Native_range))

table(df2$Native_range)
df2$Native_range[df2$Native_range=='neot'] ='neotropic'
df2$Native_range[df2$Native_range=='afrotorpic'] ='afrotropic'

df2 %>% group_by(Native_range) %>% summarise(n= n_distinct(LastSpeciesName)) %>% arrange(-n)
a= df2 %>% group_by(Country, Native_range) %>% summarise(n= n_distinct(LastSpeciesName)) %>% arrange(-n)

range(a$n)

# better to fix in Inkscape

### Temporal trends ------
### Temporal trends ------
iberia

list.files()
df1 = read_xlsx("./Database/All.First.records.xlsx")
df1 <- df1[df1$ISO3 %in% c("ESP", "PRT", "AND","GIB"), ]
df1 <- df1[df1$Native=="FALSE", ]
df1 = df1 %>% filter(!is.na(df1$year))
df1 = df1 %>% filter(year < 2024 & year >0)
df1 <- df1[df1$usageKey %in% iberia$usageKey, ] # Use the keys from GIATAR (potentially same as GBIF)
df1 <- df1[!df1$Source=="Not dated", ]

df1$ISO3[df1$ISO3 =="ESP"] ="Spain"
df1$ISO3[df1$ISO3 =="AND"] ="Andorra"
df1$ISO3[df1$ISO3 =="PRT"] ="Portugal"
df1$ISO3[df1$ISO3 =="GIB"] ="Gibraltar"

table(df1$ISO3)
table(df1$year)

res_anual = df1 %>% group_by(year, ISO3) %>% summarise(n=n())
res_cum <- df1 %>%
  group_by(ISO3, year) %>% 
  summarise(n = n(), .groups = "drop") %>% arrange(ISO3, year) %>%  
  group_by(ISO3) %>% mutate(cumulative_records = cumsum(n))

res_anual <- res_anual %>%
  group_by(ISO3) %>%
  mutate(running_median = zoo::rollapply(n, width = 10, FUN = median, fill = NA, align = "center"))

res_comb_anual <- res_anual %>% group_by(year) %>% summarise(n=sum(running_median)) %>% mutate(ISO3="comb")

res_comb_cum <-  df1 %>%
  group_by(year) %>% 
  summarise(n = n(), .groups = "drop") %>% arrange(year) %>%  
  mutate(cumulative_records = cumsum(n))%>% mutate(ISO3="comb")


location_colors <- c(
  "Spain" = "#ff7f00",
  "Portugal" = "#33a02c", 
  "Andorra" = "#e31a1c", 
  "Gibraltar" = "#1f78b4")
location_colors = col

p1 = ggplot(res_anual, aes(x = year, y = n, color = ISO3, group = ISO3)) +
  ylim(0,30)+  xlim(1500,2025)+
  scale_x_continuous(breaks = c(1500,1600,1700,1800,1900,2000,2023), limits = c(1500, 2023))+
  theme_bw(base_size = 12) + coord_cartesian(clip = 'on')+
  geom_point(size = 2, alpha=0.35) + scale_fill_manual(values = c("grey70" = "lightgrey", "white" = "white"), guide = "none")+
  scale_color_manual(values = location_colors) +
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
  # Add curved arrows
  geom_vline(xintercept = c(1800, 1986, 2014), linetype = "dashed", color = "grey50", size = 0.5, alpha=0.5) 
# geom_curve(x = 1800, y = 19, xend = 1760, yend = 15, 
#          curvature = 0.2, arrow = arrow(length = unit(0.2, "cm")), color = "black") +
#geom_curve(x = 2014, y = 24, xend = 2000, yend = 20, 
#           curvature = 0.2, arrow = arrow(length = unit(0.2, "cm")), color = "black") +
# geom_curve(x = 1986, y = 19, xend = 1975, yend = 15, 
#            curvature = -0.2, arrow = arrow(length = unit(0.2, "cm")), color = "black")

p1

p2= ggplot(res_cum, aes(x = year, y = cumulative_records, color = ISO3, group = ISO3)) +
  scale_x_continuous(breaks = c(1500,1600,1700,1800,1900,2000,2023), limits = c(1500, 2023))+
  theme_bw(base_size = 12) + coord_cartesian(clip = 'on')+
  scale_fill_manual(values = c("grey70" = "lightgrey", "white" = "white"), guide = "none") +
  scale_color_manual(values = location_colors) +
  geom_line(aes(y = cumulative_records, color = ISO3), size = 1.2, linetype = "solid") + 
  geom_line(data = res_comb_cum, aes(y = cumulative_records), color = "black", linetype = "dashed", size = 1) + 
  labs(
    x = "Year", y = "Cumulative number of first records",
    color = "Location",  fill = "Location") +
  theme(
    axis.title = element_text(face = "bold"),
    panel.grid.minor.y = element_blank(),
    panel.grid.mayor.y = element_blank(),
    legend.position = c(0.08, 0.8), # Place legend inside the plot
    legend.background = element_rect(fill = alpha("white", 0.4), color = "black", size = 0.5),
    axis.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) + 
  xlim(1500,2025)+
  scale_x_continuous(breaks = c(1500,1600,1700, 1800,1900,2000,2023), limits = c(1500, 2023))+
  annotate("text", x = 1800, y = 300, label = "Industrial Revolution\n(1760–1840)", size = 3.5, color = "black") +
  annotate("text", x = 2014, y = 300, label = "EU Regulation\n1143/2014", size = 3.5, color = "black") +
  annotate("text", x = 1986, y = 300, label = "Spain and Portugal\njoin EU", size = 3.5, color = "black") +
  # Add curved arrows
  geom_vline(xintercept = c(1800, 1986, 2014), linetype = "dashed", color = "grey50", size = 0.5, alpha=0.5) 
#  geom_curve(x = 1800, y = 19, xend = 1760, yend = 15, 
#        curvature = 0.2, arrow = arrow(length = unit(0.2, "cm")), color = "black") +
#geom_curve(x = 2014, y = 24, xend = 2000, yend = 20, 
#         curvature = 0.2, arrow = arrow(length = unit(0.2, "cm")), color = "black") +
#geom_curve(x = 1986, y = 19, xend = 1975, yend = 15, 
#          curvature = -0.2, arrow = arrow(length = unit(0.2, "cm")), color = "black")
p2
library(patchwork)
p=p1+p2
p
ggsave(plot=p, filename = "temporal.svg", device = "svg")


### Spatial GBIF  -----
# The extraction occ are running in the FROV PC (see GBIF.FROV.r)
getwd()
files = list.files(path = "./Database",  pattern = "Iberia_") 
f= files[1]

marine <- st_read("./World_EEZ_v12_20231025/eez_v12.shp")
cities <- read_xlsx("worldcities.xlsx")
cities1 <- cities %>% filter(country %in% c("Andorra", "Gibraltar","Spain","Portugal")) %>%
  filter(capital %in% c("primary")) %>% filter(!admin_name %in% c("Balearic Islands","Canary Islands","Azores","Funchal","Madeira"))

points <- read_rds(paste0("./Database/Iberia_ES.rds") )
points1 <- read_rds(paste0("./Database/Iberia_PT.rds") )
ib = rbind(points,points1)

points2 = read_xlsx("./Database/ibermis.xlsx") 
table(points2$country)
colnames(points2)[4] = "species"
colnames(points2)[12] = "decimalLatitude"
colnames(points2)[13] = "decimalLongitude"

ib = rbind(ib[,c(1,7,8)], points2[,c(4,12,13)])

points_sf <- st_as_sf(ib, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

countries<- geodata::gadm(
  country = c("ESP","PT"),level = 1, path = getwd() ) %>%
  sf::st_as_sf() %>%
  sf::st_cast("MULTIPOLYGON") %>%  st_transform(crs = st_crs(points_sf)) %>% 
  filter(!NAME_1 %in% c("Islas Baleares","Islas Canarias", "Ceuta y Melilla", "Azores", "Madeira"))

mar <- marine %>% filter(TERRITORY1 %in% c("Spain","Portugal"))
mar <- st_transform(mar, crs = st_crs(countries))

countries_geom <- st_geometry(countries)

mar_geom <- st_geometry(mar)
countries_geom <- st_sf(geometry = c(countries_geom, mar_geom))

cities2 <- cities1 %>% filter(country %in% c("Spain","Portugal")) %>%
  st_as_sf(coords = c("lng", "lat"), crs = st_crs(countries_geom))

plot(st_geometry(countries_geom))

points_main <- as_Spatial(points_sf$geometry)

point_extent <- extent(points_main)  
buffer <- 0.01  

density_raster <- raster(
  xmn = xmin(point_extent) - buffer, 
  xmx = xmax(point_extent) + buffer, 
  ymn = ymin(point_extent) - buffer, 
  ymx = ymax(point_extent) + buffer, 
  resolution = 0.045045,  # Set finer resolution to capture point density
  crs = "+proj=longlat +datum=WGS84")

density_raster <- rasterize(
  points_sf,   # SpatialPoints data
  density_raster,    # New raster template
  fun = "count",     # Counts points in each cell
  background = 0     # Sets cells without points to 0
)

density_raster[density_raster == 0] <- NA

density_df <- as.data.frame(density_raster, xy = TRUE)
names(density_df)
density_df = density_df[,c(1,2,4)]
colnames(density_df) <- c("longitude", "latitude", "density")

raster_points <- rasterToPoints(density_raster)

df <- as.data.frame(raster_points)
names(df) <- c("longitude", "latitude", "value")

library(scales)
countries_geom <- sf::st_simplify(countries_geom, preserveTopology = TRUE, dTolerance = 1000)

p1= ggplot() +xlim(-10, 5.6) + ylim(35, 45) +
  geom_segment(data = df,aes(x = longitude, y = latitude,
                             xend = longitude,
                             yend = latitude + (value/max(value, na.rm = TRUE)) * 2,
                             color = value),
               alpha = 0.8,
               size = 0.5) + # color_palette+ 
scale_color_gradientn(colors = c("#FFF5E1", "#FFE0A3", "#FFCC66", "#FF9933", "#FF6600", "#CC3300"),
                     trans = "sqrt") +
  geom_sf(data = countries_geom, color = "black", fill = "grey70") + 
  theme_void()+
  theme(
    legend.position = "right",          
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),   
    legend.key.size = unit(0.4, "cm")      
  )+ 
  geom_sf(data = cities2, color = "tan2",font="bold", size = 1) +  
  geom_sf_text(data = cities2, aes(label = city), font="bold", color = "tan2", size = 3, fontface = "bold", nudge_y = -0.2) 
p1
ggsave(last_plot(), device = "svg", filename="a.svg", dpi =10)


## Andorra
points <- read_rds(paste0("./Database/Iberia_AD.rds") )
points_sf <- st_as_sf(points, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

countries<- geodata::gadm(
  country = c("AND"),level = 1, path = getwd() ) %>%
  sf::st_as_sf() %>%
  sf::st_cast("MULTIPOLYGON") %>%  st_transform(crs = st_crs(points_sf)) %>% 
  filter(!NAME_1 %in% c("Islas Baleares","Islas Canarias", "Ceuta y Melilla", "Azores", "Madeira"))

countries_geom <- st_geometry(countries)
countries_geom <- st_sf(geometry = c(countries_geom))

cities2 <- cities1 %>% filter(country %in% c("Andorra")) %>%
  st_as_sf(coords = c("lng", "lat"), crs = st_crs(countries_geom))

plot(st_geometry(countries_geom))

points_main <- as_Spatial(points_sf$geometry)
point_extent <- extent(points_main)  
buffer <- 0.01  

density_raster <- raster(
  xmn = xmin(point_extent) - buffer, 
  xmx = xmax(point_extent) + buffer, 
  ymn = ymin(point_extent) - buffer, 
  ymx = ymax(point_extent) + buffer, 
  resolution = 0.005,  
  crs = "+proj=longlat +datum=WGS84")

density_raster <- rasterize(
  points_sf,  
  density_raster,   
  fun = "count", background = 0)

density_raster[density_raster == 0] <- NA
density_df <- as.data.frame(density_raster, xy = TRUE)
names(density_df)
density_df = density_df[,c(1,2,13)]
colnames(density_df) <- c("longitude", "latitude", "density")

raster_points <- rasterToPoints(density_raster)
df <- as.data.frame(raster_points)
names(df) <- c("longitude", "latitude", "value")

#library(scales)
p5= ggplot() +#xlim(1.4, 1.8) + ylim(42.4, 42.7) +
  geom_segment(data = df,aes(x = longitude, y = latitude,
                             xend = longitude,
                             yend = latitude + (value/max(value, na.rm = TRUE)) * 0.1,
                             color = value),
               alpha = 0.8,
               size = 0.5) + # color_palette+ 
  scale_color_viridis_c(option = "C", direction = 1, end = 0.9,  # Customize the viridis options as needed
                        name = "Number of GBIF occurrences",
                        breaks = c(0, 2500, 5000, 7500, 10000,12000,15000)) +
  geom_sf(data = countries_geom, color = "black", fill = "NA") + 
  theme_void()+
  geom_sf(data = cities2, color = "tan2", size = 1) +  
  geom_sf_text(data = cities2, aes(label = city), color = "tan2", size = 3, fontface = "bold", nudge_y = 0.009) 
p5

ggsave(last_plot(), device = "svg", filename="b.svg", dpi =300)

## Gibraltar
points <- read_rds(paste0("./Database/Iberia_GI.rds") )
points_sf <- st_as_sf(points, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

world_states <- ne_states(returnclass = "sf")
countries <- world_states[world_states$name == "Gibraltar", ]

mar <- marine %>% filter(TERRITORY1 %in% c("Gibraltar"))
mar <- st_transform(mar, crs = st_crs(countries))

countries_geom <- st_geometry(countries)

mar_geom <- st_geometry(mar)
countries_geom <- st_sf(geometry = c(countries_geom, mar_geom))

cities2 <- cities1 %>% filter(country %in% c("Gibraltar")) %>%
  st_as_sf(coords = c("lng", "lat"), crs = st_crs(countries_geom))

plot(st_geometry(countries_geom))

points_main <- as_Spatial(points_sf$geometry)
point_extent <- extent(points_main)  
buffer <- 0.01  

density_raster <- raster(
  xmn = xmin(point_extent) - buffer, 
  xmx = xmax(point_extent) + buffer, 
  ymn = ymin(point_extent) - buffer, 
  ymx = ymax(point_extent) + buffer, 
  resolution = 0.005,  # Set finer resolution to capture point density
  crs = "+proj=longlat +datum=WGS84")

density_raster <- rasterize(
  points_sf,   # SpatialPoints data
  density_raster,    # New raster template
  fun = "count",     # Counts points in each cell
  background = 0     # Sets cells without points to 0
)

density_raster[density_raster == 0] <- NA
density_df <- as.data.frame(density_raster, xy = TRUE)
names(density_df)
density_df = density_df[,c(1,2,13)]
colnames(density_df) <- c("longitude", "latitude", "density")

raster_points <- rasterToPoints(density_raster)
df <- as.data.frame(raster_points)
names(df) <- c("longitude", "latitude", "value")

#library(scales)
p3= ggplot() +#xlim(1.4, 1.8) + ylim(42.4, 42.7) +
  geom_segment(data = df,aes(x = longitude, y = latitude,
                             xend = longitude,
                             yend = latitude + (value/max(value, na.rm = TRUE)) * 0.1,
                             color = value),
               alpha = 0.8,
               size = 0.5) + # color_palette+ 
  scale_color_viridis_c(option = "C", direction = 1, end = 0.9,  # Customize the viridis options as needed
                        name = "Number of GBIF occurrences",
                        breaks = c(0, 2500, 5000, 7500, 10000,12000,15000)) +
  geom_sf(data = countries_geom, color = "black", fill = "NA") + 
  theme_void()+ 
  geom_sf(data = cities2, color = "tan2", size = 1) +  
  geom_sf_text(data = cities2, aes(label = city), color = "tan2", size = 3, fontface = "bold", nudge_y = 0.009) 

ggsave(last_plot(), device = "svg", filename="c.svg", dpi =300)


#  b) Species per administratitve area

marine <- st_read("./World_EEZ_v12_20231025/eez_v12.shp")
cities <- read_xlsx("worldcities.xlsx")
cities1 <- cities %>% filter(country %in% c("Andorra", "Gibraltar","Spain","Portugal")) %>%
  filter(capital %in% c("primary","admin")) %>% filter(!admin_name %in% c("Canary Islands","Azores"))

con <- "Andorra"

for(con in c("Iberia", "Gibraltar", "Andorra")){
  
  if(con=="Iberia"){
    points <- read_rds(paste0("./Database/Iberia_ES.rds") )
    points1 <- read_rds(paste0("./Database/Iberia_PT.rds") )
    ib = rbind(points,points1)
    points2 = read_xlsx("./Database/ibermis.xlsx") 
    colnames(points2)[4] = "species"
    colnames(points2)[12] = "decimalLatitude"
    colnames(points2)[13] = "decimalLongitude"
    points = rbind(ib[,c(1,7,8)], points2[,c(4,12,13)])
    code  <- c("PT", "ESP")
  } else if(con=="Gibraltar"){
    points <- read_rds("./Database/Iberia_GI.rds")
    
  }else if(con=="Andorra"){
    points <- read_rds("./Database/Iberia_AD.rds")
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
  
  countries_geom <- st_geometry(countries)
  plot(st_geometry(countries_geom))
  
  cat("points & countries shapefile for:", con)
  
  if(con =="Iberia"){
    cities2 <- cities1 %>% filter(iso2 %in% c("ES","PT")) %>%
      st_as_sf(coords = c("lng", "lat"), crs = st_crs(countries_geom))
  } else if(con =="Andorra"){
    cities2 <- cities1 %>% filter(iso3 %in% c("AND")) %>%
      st_as_sf(coords = c("lng", "lat"), crs = st_crs(countries_geom))
  }else if(con =="Gibraltar"){
    cities2 <- cities1 %>% filter(iso2 %in% c("GI")) %>%
      st_as_sf(coords = c("lng", "lat"), crs = st_crs(countries_geom))
  }
  
  if(con == "Andorra"){
    points_joined <- st_join(points_sf, countries)
    species_counts <- points_joined %>%
      group_by(NAME_1) %>%
      summarise(species_count = n_distinct(species)) %>%
      st_drop_geometry()  
    
    combined_data <- countries %>%
      left_join(species_counts, by = "NAME_1")
  }
  
  
  if(con =="Iberia"){
    countries_geom <- countries_geom %>% st_cast("MULTIPOLYGON")
    mar <- marine %>% filter(TERRITORY1 %in% c("Spain", "Portugal"))
    mar <- st_transform(mar, crs = st_crs(countries))
    mar <- mar %>% st_cast("POLYGON")
  } else if(con =="Gibraltar"){ 
    mar <- marine %>% filter(TERRITORY1 %in% c("Gibraltar"))
    mar <- st_transform(mar, crs = st_crs(countries))
  }
  
  if(con !="Andorra"){
    mar_geom <- st_geometry(mar)
    mar_geom <- st_cast(mar, "MULTIPOLYGON") %>% st_geometry()
    countries_geom <- st_sf(geometry = c(st_geometry(countries_geom), mar_geom))
  }
  
  plot(st_geometry(countries_geom))
  
  cat("points & countries shapefile for:", con)
  
  if(con=="Gibraltar"){
    countries_geom <- countries_geom %>%filter(!st_is_empty(geometry)) %>% mutate(ID = row_number()) 
    points_joined <- st_join(points_sf, countries_geom)
    species_counts <- points_joined %>%
      group_by(ID) %>%
      summarise(species_count = n_distinct(species)) %>%
      st_drop_geometry()  
    
    combined_data <- countries_geom %>%
      left_join(species_counts, by = "ID")
  }
  
  if(con=="Andorra"){
    combined_shapefile = countries
    joined_data <- st_join(points_sf, combined_shapefile)
    species_count <- joined_data %>%
      group_by(NAME_1) %>%
      summarise(
        species_count = n_distinct(species), # Count unique species
        geometry = st_union(geometry)       # Combine geometries for each NAME_1
      ) %>%
      st_as_sf()
  }
  
  if(con=="Iberia"){
    countries_geom <- countries_geom %>%filter(!st_is_empty(geometry)) %>% mutate(ID = row_number()) 
    points_joined <- st_join(points_sf, countries_geom)
    species_counts <- points_joined %>%
      group_by(ID) %>%
      summarise(species_count = n_distinct(species)) %>%
      st_drop_geometry()  
    
    combined_data <- countries_geom %>%
      left_join(species_counts, by = "ID")
  }
  
  #points_main <- as_Spatial(points_sf$geometry)
  color_palette <-c("#FFFF00", "#FFEA00", "#FFA500", "#FF7F00", "#FF0000")
  cities2 <- cities2 %>% filter(!city =='Funchal')
  
  p6=ggplot(data = combined_data) + 
    geom_sf(aes(fill = species_count), color = "black") +
    #  scale_fill_gradientn(
    # colors = color_palette,
    # name = "Number of Species"
    #) +
    scale_fill_viridis_c(option = "C", direction = 1, end = 0.9,  # Customize the viridis options as needed
                         name = "Number of species",
                         breaks = seq(0, 1200, by = 200),  # Ensure evenly spaced breaks
                         labels = seq(0, 1200, by = 200)    # Explicitly set corresponding labels
    ) +
    theme_void() +
    #labs(fill = "Number of species") +
    geom_sf(data = cities2, color = "tan2", size = 0.8) +  # Add city points
    geom_sf_text(data = cities2, aes(label = city), color = "tan2", 
                 size = 3, fontface = "bold", nudge_y = 0.03) +
    theme(
      legend.position = "right",           # Keep legend on the right
      legend.title = element_text(size = 10),  # Smaller title
      legend.text = element_text(size = 8),   # Smaller text
      legend.key.size = unit(0.4, "cm")       # Smaller legend key boxes
    )
  p4
  ggsave(filename = paste0("Sp_", con,".svg"), plot = p1,
         width = 8, height = 6, units = "in", device = "svg")
}

p <- p1 / p2 + plot_layout(heights = c(1, 1)) # Iberia
p <- p3 / p4 + plot_layout(heights = c(1, 1)) # Gibraltar
p <- p5 / p6 + plot_layout(heights = c(1, 1)) # Andorra

ggsave(filename = "Spatial2.svg",
  plot = p, width = 2.76,height = 3.9,units = "in",dpi = 300)







## Fig S3. Spatial data normalised  -------


