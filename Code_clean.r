
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
cat('Total species:', length(unique(df$New_names)) ) # 2,686 sp
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


# Taxonomic composition (Groups)
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
