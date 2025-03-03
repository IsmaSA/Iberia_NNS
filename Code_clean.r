
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


# Check GBIF backbone: ----------
source(GBIF.FROV.r) # done in this code
setdiff(df$Taxon, res$name)

GBIF = read_xlsx('/home/ismael-soto/Desktop/ELZA/Iberia/Database/GBIF_backbone.xlsx', sheet = 2)

length(unique(GBIF$GBIF_key))

if(length(unique(df$GBIF_key)) ==1672){
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


# Overall
cat('Total species:', length(unique(df$LastSpeciesName)) ) # 1,662 sp
cat('Total species:', length(unique(df$Taxon)) ) # 1,713 sp
df %>% group_by(Country) %>% summarise(Species = n_distinct(LastSpeciesName))

# Phylum:
cat('Total Phylum:', length(unique(df$Phylum)) ) # 19 
df %>% group_by(Country) %>% summarise(Phylum = n_distinct(Phylum))

# Class:
cat('Total Class:', length(unique(df$Class)) ) # 52
df %>% group_by(Country) %>% summarise(Class = n_distinct(Class))

# Family:
cat('Total Family:', length(unique(df$Family)) ) # 598
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
  file = file.path('/home/ismael-soto/Documents/GitHub/Iberia_NNS/Plots', paste0(country, '.pdf')),  selector = "body")
 
 cat(country)
}
options(browser = "/snap/bin/chromium")


### Figure 2: -----
country = unique(df.hab$Location)
c = 'Gibraltar'
col <- c("FRESHWATER" = "#35e521", "MARINE" = "#2616bb", 
         "TERRESTRIAL" = "#af9b26")
plots= list()
for(c in country) {
  df.hab1 <- df.hab[df.hab$Location ==c, ]

  shape <- ne_states(returnclass = "sf")
  shape <- shape[shape$admin == c, ]
  
  if(c =="Spain"){
  shape <- shape[!shape$name %in% c("Ceuta","Melilla","Santa Cruz de Tenerife","Las Palmas","Baleares") ,  ]
  } else if(c =="Portugal"){
    shape <- shape[!shape$name %in% c("Azores","Madeira") ,  ]
  } 
  
  #shape <- ne_countries(scale = "medium", country = c, returnclass = "sf")
  shape <- st_transform(shape, crs = st_crs(4326))
  plot(shape)
  
  combined_geometry <- st_union(shape)
  combined_shape <- st_sf(geometry = st_sfc(combined_geometry), crs = st_crs(shape))
  plot(combined_shape, col = NA, border = "black")
  
  
  # if (c == "Spain") {
  #  mainland_bbox <- st_bbox(c(xmin = -9.5, ymin = 35.0, xmax = 3.3, ymax = 44.0), crs = st_crs(shape))
  #  shape <- st_crop(shape, mainland_bbox)
  #} else if (c == "Portugal") {
  #  mainland_bbox <- st_bbox(c(xmin = -9.5, ymin = 36.5, xmax = -6.2, ymax = 42.2), crs = st_crs(shape))
  # shape <- st_crop(shape, mainland_bbox)
  #}
  
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
  df.hab1$proportion <- df.hab1$n / sum(df.hab1$n)
  df.hab1$grid_count <- round(df.hab1$proportion * total_grids)
  df.hab1$grid_count[which.max(df.hab1$grid_count)] <- 
    total_grids - sum(df.hab1$grid_count[-which.max(df.hab1$grid_count)])
  
  categories <- unlist(mapply(rep, df.hab1$Habitat, df.hab1$grid_count))
  grid_sf$Habitat <- categories
  
  p1<- ggplot() +
    geom_sf(data = grid_sf, aes(fill = Habitat), color = "black") +
    geom_sf(data = combined_shape, fill = NA, color = "black") +  
    scale_fill_manual(values = col) + 
    theme_void() +
    labs(fill = "Habitat") +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12)  )
  p1
  plots[[c]] =p1
  }

library(patchwork)
(plots[["Spain"]] + plots[["Portugal"]]) / 
  (plots[["Andorra"]] + plots[["Gibraltar"]]) + 
  plot_layout(heights = c(2, 1)) 
ggsave(plot= last_plot(), filename = "habitat.svg", device= "svg")

### Pathways of introduction -----

unique(df$Pathway_refined)
table(df$Pathway_refined)

df %>% filter(grepl("_", Pathway_refined)) %>%  distinct(New_names)  %>% nrow() 

df1 = df %>% separate_rows(Pathway_refined, sep= '_') %>% filter(!is.na(Pathway_refined))
df1 = df1[!df1$Pathway_refined == 'NA',]
table(df1$Pathway_refined)

a= df1 %>% group_by(Location,Pathway_refined) %>% summarise(Species= n_distinct(New_names)) %>% arrange(-Species)

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
con = 'Spain'
for(con in c("Spain","Portugal","Gibraltar", "Andorra")) {
 
 df2 <- df1[df1$Location ==con, ]
 df2= df2 %>% group_by(Group,Pathway_refined) %>% summarise(n= n_distinct(New_names)) %>% arrange(-n)

 nodes <- data.frame(name = unique(c(df2$Group, df2$Pathway_refined)))

df2 <- df2 %>%
  mutate(
    source = match(Group, nodes$name) - 1,
    target = match(Pathway_refined, nodes$name) - 1,
    color = group_colors[Group])

node_colors <- c(
  group_colors[unique(df2$Group)],
  rep("#cccccc", length(unique(df2$Pathway_refined))))

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
a= df2 %>% group_by(Location, Native_range) %>% summarise(n= n_distinct(New_names)) %>% arrange(-n)


## Figure 2
range(a$n)
# better to fix in Inkscape
b <- df[df$Group=="Fishes", ]

### Temporal trends ------
list.files()
df1 = read_xlsx("./Database/All.First.records.xlsx")
df1 <- df1[df1$ISO3 %in% c("ESP", "PRT", "AND","GIB"), ]
df1 <- df1[df1$Native=="FALSE", ]
df1 <- df1[!df1$Source=="Not dated", ]
df1 = df1 %>% filter(!is.na(df1$year))
df1 = df1 %>% filter(year < 2024 & year >0)
df1 <- df1[df1$usageKey %in% df$GIATAR_key, ]

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

background <- data.frame(
  xmin = seq(1500, 2050, by = 50),
  xmax = seq(1550, 2100, by = 50),
  fill = rep(c("grey70", "white"), length.out = length(seq(1500, 2050, by = 50))))

location_colors <- c(
  "Spain" = "#ff7f00",
  "Portugal" = "#33a02c", 
  "Andorra" = "#e31a1c", 
  "Gibraltar" = "#1f78b4")


p1 = ggplot(res_anual, aes(x = year, y = n, color = ISO3, group = ISO3)) +
  geom_rect(data = background, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
            inherit.aes = FALSE, alpha = 0.5) + ylim(0,30)+  xlim(1700,2025)+
  scale_x_continuous(breaks = c(1700,1800,1900,2000,2023), limits = c(1700, 2023))+
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
    legend.position = c(0.08, 0.8), # Place legend inside the plot
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
  geom_rect(data = background, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
            inherit.aes = FALSE, alpha = 0.5) +  scale_x_continuous(breaks = c(1700,1800,1900,2000,2023), limits = c(1700, 2023))+
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
    legend.position = c(0.08, 0.8), # Place legend inside the plot
    legend.background = element_rect(fill = alpha("white", 0.4), color = "black", size = 0.5),
    axis.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) + 
  xlim(1700,2025)+
  scale_x_continuous(breaks = c(1700,1800,1900,2000,2023), limits = c(1700, 2023))+
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

library(patchwork)
p1+p2
ggsave(plot=last_plot(), filename = "temporal.svg", device = "svg")

### Spatial GBIF  -----
# The extraction occ are running in the FROV PC (see GBIF.FROV.r)
getwd()
files = list.files(path = "./Database",  pattern = "Iberia_") 
f= files[4]

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
  scale_color_viridis_c(option = "C", direction = 1, end = 0.9,  # Customize the viridis options as needed
                        name = "Number of GBIF \n occurrences",
                        breaks = c(0, 2500, 5000, 7500, 10000,12000,15000)) +
  geom_sf(data = countries_geom, color = "black", fill = "NA") + 
  theme_void()+
  theme(
    legend.position = "right",           # Keep legend on the right
    legend.title = element_text(size = 10),  # Smaller title
    legend.text = element_text(size = 8),   # Smaller text
    legend.key.size = unit(0.4, "cm")       # Smaller legend key boxes
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



### Supplementary Figure (groups)  -----

# Fig S1. Venn Diagram  -------
install.packages("ggVennDiagram")
install.packages("VennDiagram")
library(ggVennDiagram)
library(VennDiagram)

species_locations <- df %>%  dplyr::select(Location, Taxon) %>% distinct()

countries <- c("Spain", "Portugal", "Gibraltar", "Andorra")

species_list <- lapply(countries, function(country) {
  species_locations %>% filter(Location == country) %>%
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



## Fig S2. Grous  -------

head(df)
df$Group[df$Group =="Dinoflagellata"] <- "Microorganisms"
df$Group[df$Group =="Viruses"] <- "Microorganisms"
df$Group[df$Group =="Bacteria and protozoans"] <- "Microorganisms"
df$Group[df$Group =="Microorganism"] <- "Microorganisms"
df$Group[df$Group =="Microorganism"] <- "Microorganisms"
df$Group[df$Group =="mammals"] <- "Mammals"
df$Group[df$Group =="Invertebrates (excl. Arthropods, Molluscs)"] <- "Other invertebrates"
df$Group[df$Group =="Arthropods p.p. (Myriapods, Diplopods etc.)"] <- "Other invertebrates"

group_colors <- c(
  "Algae" = "#913003", "Amphibians" = "#7bc810", "Birds" = "#dae93d",
  "Bryophytes" = "#e78ac3", "Crustaceans" = "#a471ed", "Fishes" = "#8399e7",
  "Fungi" = "#b1d634", "Insects" = "#b3b3b3", "Mammals" = "#e5c494",
  "Microorganisms" = "#72c6b1", "Molluscs" = "#fb9a99", "Reptiles" = "#dd1a1a",
  "Vascular plants" = "#76b379", "Other invertebrates" = "#cab2d6")

df1= df %>% group_by(Location, Group) %>% summarise(n = n()) %>% mutate(
  Location = factor(Location, levels = c("Spain", "Portugal", "Andorra", "Gibraltar")) )

unique(df1$Group)

ggplot(df1, aes(Group, n, fill = Group)) + geom_bar(stat = "identity",size= 0.2, color ="black")+
  facet_wrap(vars(Location), scales = "free_y") +
  scale_fill_manual(values = group_colors) +
  theme_bw() +  labs(x="", y="")+scale_x_reordered() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none")


## Fig S3. Spatial data normalised  -------
list.files(pattern = 'rds')

country <- c('Iberia', 'Andorra', 'Gibraltar')
c= 'Iberia'
marine <- st_read("./World_EEZ_v12_20231025/eez_v12.shp")
cities <- read_xlsx("worldcities.xlsx")
cities1 <- cities %>% filter(country %in% c("Andorra", "Gibraltar","Spain","Portugal")) %>%
  filter(capital %in% c("primary","admin")) %>% filter(!admin_name %in% c("Canary Islands","Azores"))
plots <- list()

for(c in country){  
  print(c)
  Sys.sleep(2)
  if(c =='Iberia'){
  points <- read_rds(paste0('./Database/Iberia_ES.rds') )
  points1 <- read_rds(paste0('./Database/Iberia_PT.rds') )
  ib = rbind(points,points1)
  points2 = read_xlsx("./Database/ibermis.xlsx") 
  colnames(points2)[4] = "species"
  colnames(points2)[12] = "decimalLatitude"
  colnames(points2)[13] = "decimalLongitude"
  points = rbind(ib[,c(1,7,8)], points2[,c(4,12,13)])
  code  <- c("PT", "ESP")

  } else if(c =='Andorra'){
  points <- read_rds(paste0('./Database/Iberia_AD.rds') )
   code  <- "AND"
  }else if(c =='Gibraltar'){ 
  points <- read_rds(paste0('./Database/Iberia_GI.rds') )
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
    scale_fill_viridis_c(option = "C", direction = 1, end = 0.9,  
                         name = "Species per area (km2)",
                        limits = c(0, 3.2), 
                         breaks = seq(0, 3.2, by = 0.5),  
                         labels = seq(0, 3.2, by = 0.5),
                         oob = scales::squish     
    ) +
    theme_void() +
    #labs(fill = "Number of species") +
    geom_sf(data = cities2, color = "tan2", size = 0.8) +  # Add city points
    geom_sf_text(data = cities2, aes(label = city), color = "tan2", 
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
ggsave(last_plot(), filename = "Spatial.correction.svg", device = "svg")  

