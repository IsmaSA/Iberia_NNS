
### Code to download GBIF occ PC FROV

pacman::p_load(sf,dplyr,tidyr,xlsx,writexl,readxl,sp, CoordinateCleaner, ggplot2,terra,raster, rnaturalearth,rnaturalearthdata, readr,rgbif)

setwd("/home/ismael-soto/Desktop/ELZA/Iberia")
list.files()
df <- readxl::read_xlsx(path = './Database/Iberia.xlsx')
head(df)
df = df %>% filter(Location %in% c('Spain', 'Portugal', 'Andorra', 'Gibraltar'))
names(df)
df= df[!duplicated(df[,c('Location','GBIF_key')]), ]
df= df[df$GBIF_key != '0', ]
df= df[!df$New_names %in% c('Ommatotriton sp.','Acridotheres sp.'), ]


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




# spocc  -----
install.packages("terra", dependencies = TRUE)
install.packages("spocc", dependencies = TRUE)
install.packages("remotes")
remotes::install_github("ropensci/spocc")
library("spocc")

cawr<-'Campylorhynchus brunneicapillus' ##California cactus wren
cawr_obs<-occ(query = cawr, from=c('inat','ebird','vertnet','idigbio','obis'), limit = 50000, has_coords = TRUE)
obs<-occ2df(cawr_obs)