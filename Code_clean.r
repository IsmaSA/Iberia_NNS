
### Code to use the clean database (LINUX) 

# Ismael Soto --- University of South Bohemia in Ceske Budejovice (USB)
Sys.time()

pacman::p_load(sf,dplyr,tidyr,xlsx,writexl,readxl,sp, ggplot2,stringr,terra,raster,stringr, rnaturalearth,rnaturalearthdata, readr,rgbif)

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

df <- readxl::read_xlsx(path = './Database/Iberia.xlsx')
head(df)
df = df %>% filter(Location %in% c('Spain', 'Portugal', 'Andorra', 'Gibraltar'))
names(df)
df= df[!duplicated(df[,c('Location','GBIF_key')]), ]
df= df[df$GBIF_key != '0', ]
df= df[!df$New_names %in% c('Ommatotriton sp.','Acridotheres sp.'), ]


# Check for missing information
nas <- sapply(df[, c("Family", "Class", "Phylum",'Group','Pathway_refined','Native_range')], function(column) {
  sum(is.na(column))
})
nas
table(df$Phylum)
table(df$Class)
table(df$Family)
table(df$Group)


# Overall
cat('Total species:', length(unique(df$New_names)) ) # 2,505 sp
df %>% group_by(Location) %>% summarise(Species = n_distinct(New_names))

# Phylum:
cat('Total Phylum:', length(unique(df$Phylum)) ) # 24 
df %>% group_by(Location) %>% summarise(Phylum = n_distinct(Phylum))

# Class:
cat('Total Class:', length(unique(df$Class)) ) # 24 
df %>% group_by(Location) %>% summarise(Class = n_distinct(Class))

# Family:
cat('Total Family:', length(unique(df$Family)) ) # 24 
df %>% group_by(Location) %>% summarise(Family = n_distinct(Family))


# Shared species composition
species_locations <- df %>% dplyr::select(Location, Taxon) %>%distinct()  

location_pairs <- expand.grid(Location1 = unique(species_locations$Location), 
                              Location2 = unique(species_locations$Location)) %>%
  filter(Location1 != Location2) 

shared_species <- location_pairs %>% rowwise() %>%
  mutate( Shared = length(intersect(
      species_locations$Taxon[species_locations$Location == Location1],
      species_locations$Taxon[species_locations$Location == Location2] )),
   Total1 = n_distinct(species_locations$Taxon[species_locations$Location == Location1]),
    Total2 = n_distinct(species_locations$Taxon[species_locations$Location == Location2]),
    Overlap_Share = Shared / min(Total1, Total2)  * 100 ) %>% ungroup()


# Taxonomic composition (Groups)-----
table(df$Group)
unique(df$Group)

df$Group[df$Group =="Dinoflagellata"] <- "Microorganisms"
df$Group[df$Group =="Viruses"] <- "Microorganisms"
df$Group[df$Group =="Bacteria and protozoans"] <- "Microorganisms"
df$Group[df$Group =="Microorganism"] <- "Microorganisms"
df$Group[df$Group =="Microorganism"] <- "Microorganisms"
df$Group[df$Group =="mammals"] <- "Mammals"
df$Group[df$Group =="Invertebrates (excl. Arthropods, Molluscs)"] <- "Other invertebrates"
df$Group[df$Group =="Arthropods p.p. (Myriapods, Diplopods etc.)"] <- "Other invertebrates"

df %>% group_by(Group) %>% summarise(Species = n_distinct(New_names)) %>% arrange(-Species)
df %>% group_by(Location,Group) %>% summarise(Species = n_distinct(New_names)) %>% arrange(-Species)


# Taxonomic composition (Phylum, class & families)
unique(df$Phylum)
df %>% group_by(Phylum) %>% summarise(Species = n_distinct(New_names)) %>% arrange(-Species)
a=df %>% group_by(Location,Phylum) %>% summarise(Species = n_distinct(New_names)) %>% arrange(-Species)

df %>% group_by(Class) %>% summarise(Species = n_distinct(New_names)) %>% arrange(-Species)
a=df %>% group_by(Location,Class) %>% summarise(Species = n_distinct(New_names)) %>% arrange(-Species)

df %>% group_by(Family) %>% summarise(Species = n_distinct(New_names)) %>% arrange(-Species)
a=df %>% group_by(Location,Family) %>% summarise(Species = n_distinct(New_names)) %>% arrange(-Species)


## Figure 1  ------
df

pacman::p_load(plotly, viridis,webshot2, htmlwidgets)
add_root <- TRUE
value_column <- "n"
country <- "Andorra"
pp <- list()
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
  
  pp[[country]] <- p1

# Run this with windows - change to chrome to brave browser
htmlwidgets::saveWidget(widget = p1, file = file.path('./Plots', paste0(country, '.html')), selfcontained = FALSE)
webshot2::webshot(
  file.path('./Plots', paste0(country, '.html')), 
  file = file.path('./Plots', paste0(country, '.pdf')),  selector = "body")
 
 cat(country)
}


### Habitats -----
unique(df$Habitat)

df$Habitat[df$Habitat =='MARINE|MARINE'] = 'MARINE'
df$Habitat[df$Habitat =='FRESHWATER-MARINE'] = 'FRESHWATER|MARINE'
df$Habitat[df$Habitat =='FRESHWATER MARINE'] = 'FRESHWATER|MARINE'
df$Habitat[df$Habitat =='FRESHWATER|MARINE|MARINE'] = 'FRESHWATER|MARINE'
df$Habitat[df$Habitat =='TERRESTRIAL|FRESHWATER|MARINE|MARINE'] = 'TERRESTRIAL|FRESHWATER|MARINE'
unique(df$Habitat)


df.hab <- df %>%
  mutate(Habitat = str_replace_all(Habitat, "\\s+", ""), 
         Habitat = str_replace_all(Habitat, "-", "|"),
         Habitat = str_replace_all(Habitat, "FRESHWATER-MARINE", "FRESHWATER|MARINE"))    
unique(df.hab$Habitat)

df.hab <- df.hab %>% separate_rows(Habitat, sep = "\\|") %>%  filter(Habitat != "")  
df.hab <- df.hab[!df.hab$Habitat=='HOST', ] 
unique(df.hab$Habitat)
table(df.hab$Habitat)

df.hab <- df.hab %>% group_by(Location, Habitat) %>% summarise(n = n())
str(df.hab)

df.hab.text <- df.hab %>% group_by(Location, Habitat) %>% summarise(n = n())


### Figure 2: -----
country = unique(df.hab$Location)
c = 'Gibraltar'
col <- c("FRESHWATER" = "#0d7ff1", "MARINE" = "#2616bb", 
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
    size= 0.5
  } else if(c=="Portugal"){ size= 0.4}else if(c=="Andorra"){ 
    size= 0.05}else if(c=="Gibraltar"){ 
      size= 0.005}
  
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
    geom_sf(data = combined_shape, fill = NA, color = "black") +  
    geom_sf(data = grid_sf, aes(fill = Habitat), color = "black") +
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

(plots[["Spain"]] + plots[["Portugal"]]) / 
  (plots[["Andorra"]] + plots[["Gibraltar"]]) + 
  plot_layout(heights = c(2, 1)) 


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
df = read_xlsx("All.First.records.xlsx")
df1 <- df[df$ISO3 %in% c("ESP", "PRT", "AND","GIB"), ]
df1 <- df1[df1$Native=="FALSE", ]
df1 <- df1[!df1$Source=="Not dated", ]
df1 = df1 %>% filter(!is.na(df1$year))
df1 = df1 %>% filter(year < 2024 & year >0)

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

background <- data.frame(
  xmin = seq(1500, 2050, by = 50),
  xmax = seq(1550, 2100, by = 50),
  fill = rep(c("grey70", "white"), length.out = length(seq(1500, 2050, by = 50)))
)


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
  annotate("text", x = 1986, y = 20, label = "Spain and Portugal\nJoin EU", size = 3.5, color = "black") +
  # Add curved arrows
  geom_vline(xintercept = c(1800, 1986, 2014), linetype = "dashed", color = "grey50", size = 0.5, alpha=0.5) +
  geom_curve(x = 1800, y = 19, xend = 1760, yend = 15, 
             curvature = 0.2, arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  geom_curve(x = 2014, y = 24, xend = 2000, yend = 20, 
             curvature = 0.2, arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  geom_curve(x = 1986, y = 19, xend = 1975, yend = 15, 
             curvature = -0.2, arrow = arrow(length = unit(0.2, "cm")), color = "black")

p1

p2= ggplot(res_cum, aes(x = year, y = cumulative_records, color = ISO3, group = ISO3)) +
  geom_rect(data = background, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
            inherit.aes = FALSE, alpha = 0.5) +  scale_x_continuous(breaks = c(1700,1800,1900,2000,2023), limits = c(1700, 2023))+
  theme_bw(base_size = 12) + coord_cartesian(clip = 'on')+
  scale_fill_manual(values = c("grey70" = "lightgrey", "white" = "white"), guide = "none") +
  scale_color_manual(values = location_colors) +
  geom_line(aes(y = cumulative_records, color = ISO3), size = 1.2, linetype = "solid") + 
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
  annotate("text", x = 1986, y = 300, label = "Spain and Portugal\nJoin EU", size = 3.5, color = "black") +
  # Add curved arrows
  geom_vline(xintercept = c(1800, 1986, 2014), linetype = "dashed", color = "grey50", size = 0.5, alpha=0.5) +
  geom_curve(x = 1800, y = 19, xend = 1760, yend = 15, 
             curvature = 0.2, arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  geom_curve(x = 2014, y = 24, xend = 2000, yend = 20, 
             curvature = 0.2, arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  geom_curve(x = 1986, y = 19, xend = 1975, yend = 15, 
             curvature = -0.2, arrow = arrow(length = unit(0.2, "cm")), color = "black")

library(patchwork)
p1+p2
