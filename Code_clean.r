
### Code to use the clean database (LINUX) 

# Ismael Soto --- University of South Bohemia in Ceske Budejovice (USB)
Sys.time()


pacman::p_load(sf,dplyr,tidyr,xlsx,writexl,readxl,sp, ggplot2,terra,raster, rnaturalearth,rnaturalearthdata, readr,rgbif)

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
 
### Pathways of introduction -----

unique(df$Pathway_refined)
table(df$Pathway_refined)

df %>% filter(grepl("_", Pathway_refined)) %>%  distinct(New_names)  %>% nrow() 

df1 = df %>% separate_rows(Pathway_refined, sep= '_') %>% filter(!is.na(Pathway_refined))
df1 = df1[!df1$Pathway_refined == 'NA',]
table(df1$Pathway_refined)

a= df1 %>% group_by(Location,Pathway_refined) %>% summarise(Species= n_distinct(New_names)) %>% arrange(-Species)

### Figure 2: -----
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
# better to fix in Inkscape


### Temporal trends ------


