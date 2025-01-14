
# Code to use the clean database (LINUX) 

## Ismael Soto  - University of South Bohemia in Ceske Budejovice (USB)
Sys.time()
ls()
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
df1= df[!duplicated(df[,c('Location','GBIF_key')]), ]
country = unique(df$Location)

cat('Total species:', length(unique(df1$New_names)) )


