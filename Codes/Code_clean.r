rm(list = ls())

# Summary of the database

pacman::p_load(sf,dplyr,tidyr,xlsx,writexl,readxl,sp, ggplot2,terra,raster, rnaturalearth,
rnaturalearthdata, readr, data.table,tmap, rworldmap, gbif.range)

#df <- readxl::read_xlsx("./UpdateResults/ListNNS.Iberia.clean.xlsx", sheet=2)
df <- readxl::read_xlsx("./Submission/Diversity&Distributions/Submission 2/ListNNS.Iberia.v.2.xlsx", sheet=2)

df = df[df$canonicalName !='Austropotamobius fulcisianus*',]

colSums(is.na(df))

unique(df$canonicalName) #1273 species
length(unique(df$canonicalName))

# Species per country
df %>% group_by(Country) %>% summarise(n_species =n_distinct(canonicalName) ) %>% arrange(-n_species)


#Phylum:
length(unique(df$Phylum))
df %>% group_by(Country, Phylum) %>% summarise(n_species =n_distinct(canonicalName) ) %>% arrange(-n_species) %>%
group_by(Country) %>% summarise(n_phyla = n_distinct(Phylum)) %>% arrange(-n_phyla)

# Classes
length(unique(df$Class))
df %>% group_by(Country, Class) %>% summarise(n_species =n_distinct(canonicalName) ) %>% arrange(-n_species) %>%
group_by(Country) %>% summarise(n_phyla = n_distinct(Class)) %>% arrange(-n_phyla)

# family
length(unique(df$Family))
df %>% group_by(Country, Family) %>% summarise(n_species =n_distinct(canonicalName) ) %>% arrange(-n_species) %>%
group_by(Country) %>% summarise(n_phyla = n_distinct(Family)) %>% arrange(-n_phyla)


# Shared species composition
species_locations <- df %>% dplyr::select(Country, canonicalName) %>%distinct()  

location_pairs <- expand.grid(Location1 = unique(species_locations$Country), 
                              Location2 = unique(species_locations$Country)) %>%
  filter(Location1 != Location2) 

shared_species <- location_pairs %>% rowwise() %>%
  mutate( Shared = length(intersect(
      species_locations$canonicalName[species_locations$Country == Location1],
      species_locations$canonicalName[species_locations$Country == Location2] )),
   Total1 = n_distinct(species_locations$canonicalName[species_locations$Country == Location1]),
    Total2 = n_distinct(species_locations$canonicalName[species_locations$Country == Location2]),
    Overlap_Share = Shared / min(Total1, Total2)  * 100 ) %>% ungroup()

spp_andorra <- unique(df$canonicalName[df$Country == "Andorra"])
spp_gibraltar <- unique(df$canonicalName[df$Country == "Gibraltar"])
shared_spp <- intersect(spp_andorra, spp_gibraltar)

## 2.0 (no overlap)
shared_species <- location_pairs %>% rowwise() %>% mutate(
    Shared = length(intersect(
      species_locations$canonicalName[species_locations$Country == Location1],
      species_locations$canonicalName[species_locations$Country == Location2]
    )),
    Total1 = n_distinct(species_locations$canonicalName[species_locations$Country == Location1]),
    Total2 = n_distinct(species_locations$canonicalName[species_locations$Country == Location2]),
    Overlap1_to_2 = (Shared / Total1) * 100,  # % of Location1's species found in Location2
    Overlap2_to_1 = (Shared / Total2) * 100   # % of Location2's species found in Location1
  ) %>% ungroup()




### Taxonomic composition: 
df %>% group_by(Group) %>% summarise(n_species =n_distinct(canonicalName) ) %>% arrange(-n_species)
a<- df %>% group_by(Group, Country) %>% summarise(n_species =n_distinct(canonicalName) ) %>% arrange(-n_species)
top3_groups <- a %>%
  group_by(Country) %>% slice_max(order_by = n_species, n = 3, with_ties = FALSE) %>%  ungroup()

# Taxonomic rank (Phylum, class & families)
unique(df$Phylum)
df %>% group_by(Phylum) %>% summarise(Species = n_distinct(canonicalName)) %>% arrange(-Species)
a=df %>% group_by(Country, Phylum) %>% summarise(Species = n_distinct(canonicalName)) %>% arrange(-Species) 

df %>% group_by(Class) %>% summarise(Species = n_distinct(canonicalName)) %>% arrange(-Species)
a=df %>% group_by(Country,Class) %>% summarise(Species = n_distinct(canonicalName)) %>% arrange(-Species) %>% 
slice_max(order_by = Species, n = 3, with_ties = FALSE) %>%  ungroup()

df %>% group_by(Family) %>% summarise(Species = n_distinct(canonicalName)) %>% arrange(-Species)
a=df %>% group_by(Country,Family) %>% summarise(Species = n_distinct(canonicalName)) %>% arrange(-Species) %>% 
slice_max(order_by = Species, n = 3, with_ties = FALSE) %>%  ungroup()



#####   Pathways of introduction  ---------------------------------------------
unique(df$Pathway)
table(df$Pathway)
df %>% filter(Pathway != "Na") %>% distinct(canonicalName)  %>% nrow() 
df %>% filter(grepl("_", Pathway)) %>% distinct(canonicalName)  %>% nrow() 

df1 = df %>% separate_rows(Pathway, sep= '_') %>% filter(!is.na(Pathway)) %>% filter(Pathway != "Na") %>% filter(Pathway != "NA")
table(df1$Pathway)

a= df1 %>% group_by(Country,Pathway) %>% summarise(Species= n_distinct(canonicalName)) %>% arrange(-Species)

df1 %>%
  group_by(canonicalName) %>%
  summarise(n_pathways = n_distinct(Pathway), .groups = "drop") %>%
  filter(n_pathways > 1) %>% nrow()

### Native range ------

df1 <- df[!df$Native_range =="NA", ]
df1 <- df1[!df1$Native_range =="Na", ]
df2 <- df1 %>% separate_rows(Native_range, sep = "_") %>% filter(!is.na(Native_range))

table(df2$Native_range)

df2 %>% group_by(Native_range) %>% summarise(n= n_distinct(canonicalName)) %>% arrange(-n)
a= df2 %>% group_by(Country, Native_range) %>% summarise(n= n_distinct(canonicalName)) %>% arrange(-n)

range(a$n)
a=df2 %>% group_by(Country,Native_range) %>% summarise(n= n_distinct(canonicalName)) %>% arrange(-n) %>% 
slice_max(order_by = n, n = 3, with_ties = FALSE) %>%  ungroup()



#  Temporal trends of first-records  -----------------------------------

df1 = read_xlsx("./Database/All.First.records.xlsx")
df1 <- df1[df1$ISO3 %in% c("ESP", "PRT", "AND","GIB"), ]
df1 <- df1[df1$Native=="FALSE", ]
df1 = df1 %>% filter(!is.na(df1$year))
df1 = df1 %>% filter(year < 2024 & year >0)
df1 <- df1[df1$usageKey %in% df$gbif_key, ] # Use the keys from GIATAR (potentially same as GBIF)
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
    color = "Location",  fill = "Location")
p1

p2= ggplot(res_cum, aes(x = year, y = cumulative_records, color = ISO3, group = ISO3)) +
  scale_x_continuous(breaks = c(1500,1600,1700,1800,1900,2000,2023), limits = c(1500, 2023))+
  theme_bw(base_size = 12) + coord_cartesian(clip = 'on')+
  scale_fill_manual(values = c("grey70" = "lightgrey", "white" = "white"), guide = "none") +
  scale_color_manual(values = location_colors) +
  geom_line(aes(y = cumulative_records, color = ISO3), size = 1.2, linetype = "solid") + 
  geom_line(data = res_comb_cum, aes(y = cumulative_records), color = "black", linetype = "dashed", size = 1)
  p2
