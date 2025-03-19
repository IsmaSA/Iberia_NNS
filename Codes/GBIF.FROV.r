
### Code to download GBIF occ PC FROV

pacman::p_load(sf,dplyr,tidyr,xlsx,writexl,readxl,sp, CoordinateCleaner, ggplot2,terra,raster, rnaturalearth,rnaturalearthdata, readr,rgbif)

setwd("/home/ismael-soto/Desktop/ELZA/Iberia")
list.files()
df <- readxl::read_xlsx(path = './Database/ListNNS.Iberia.xlsx', sheet = 2)
head(df)
df = df %>% filter(Country %in% c('Spain', 'Portugal', 'Andorra', 'Gibraltar'))
names(df)
unique(df$'Ismael Notes')
df= df[!df$'Ismael Notes' %in% c('Remove','remove','Removed (Cesar)','Remove (crypto)','Unsure what to do'),]




# GBIF backbone  -----

res <- data.frame()
errors <- data.frame()
h <- 1
name <- "Eichhornia crassipes"
name <- "Pontederia crassipes"
i=1
# Use accepted usage key
for (i in 1:nrow(df)) {
  cat(h, "/", nrow(df), "\n")  
  h <- h + 1

df1 = df[i,]
name =  df1$Taxon
  
  tryCatch({
    data <- name_backbone(name = name)
    
    if (!is.null(data)) {
df1$CanonicalName <- data$canonicalName

if ("acceptedUsageKey" %in% colnames(data)) {
  df1$GBIF_key <- data$acceptedUsageKey
} else if ("usageKey" %in% colnames(data)) {
  df1$GBIF_key <- data$usageKey
} else {
  df1$GBIF_key <- NA
}
df1$tatus = data$status
df1$ScientificName <- data$scientificName
      
data1= occ_search(taxonKey= df1$GBIF_key , limit=1)[['data']]

df1$LastSpeciesName = data1$species

      res <- rbind(res, df1)
    } else {
      warning("No result for name: ", name)
    }
  }, error = function(e) {
    errors <- rbind(errors, data.frame(name = name, error_message = e$message))
    cat("Error with name:", name, "\n") 
  })
}
names(res)
unique(res$Taxon)

write_xlsx(res, path = '/home/ismael-soto/Desktop/ELZA/Iberia/Database/GBIF_backbone.xlsx')

# Four particular cases
Cereus peruvianus
Dohrniphora papuana 
Prodiplosis vaccinii 
Wlassiscia pannonica

name <- ""

data= occ_search(scientificName = name, limit = 3)
data
data= data[['data']]



###  GBIF extraction ------------
country <- unique(df$Location)[3]

for(country in unique(df$Location)){
  
  df1 <- df[df$Location==country, ] %>% filter(GBIF_key > 0)

  keys <- unique(df1$GBIF_key)
  keys <- as.numeric(keys)
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
    user = "***",
    pwd = "*******",
    email = "***@****"
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

getwd()

names(df1)
df1 = df[,c(1,5,11,17,7,10,15,14,13,16,18)]
write_xlsx(df1, path = '/home/ismael-soto/Desktop/ELZA/Iberia/Database/ListNNS.Iberia.xlsx')

### read GBIF downloads  -----
setwd("/home/ismael-soto/Desktop/ELZA/Iberia/Database")

files <- list.files(pattern = ".zip")
target_file <- "occurrence.txt"
res<- data.frame()
i <- files[4]

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
print(code)

if(!code %in% c("AD","GI")){ # Andorra and Gibraltar just check zero, equal
flags<- CoordinateCleaner::clean_coordinates(x = occurrence_data1,
                                             lon = "decimalLongitude",
                                             lat = "decimalLatitude",
                                             countries = "countryCode",
                                             species = "species",
                                             tests = c("capitals", "centroids", "equal","gbif",
                                                       "zeros")) 

occurrence_data1 <- occurrence_data1[flags$.summary,]
}

saveRDS(occurrence_data1, paste0('Iberia_', code,'.rds') )
    
  }, error = function(e) {
    print(paste("F en el chat:", i, "Erro:", e$message))
  })
}


