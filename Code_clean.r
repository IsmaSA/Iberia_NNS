
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


cat('Total species:', length(unique(df1$New_names)) ) # 2,639 sp

df %>% group_by(Location) %>% summarise(Species = n_distinct(New_names))


