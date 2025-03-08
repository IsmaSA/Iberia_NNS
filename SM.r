# Code to create Supplementary Material 

#### Figures:  --------------------
## Fig S2. Group  -------

head(df)

group_colors <- c(
  "Algae" = "#913003", "Amphibians" = "#7bc810", "Birds" = "#dae93d",
  "Bryophytes" = "#e78ac3", "Crustaceans" = "#a471ed", "Fishes" = "#8399e7",
  "Fungi" = "#b1d634", "Insects" = "#b3b3b3", "Mammals" = "#e5c494",
  "Microorganisms" = "#72c6b1", "Molluscs" = "#fb9a99", "Reptiles" = "#dd1a1a",
  "Vascular plants" = "#76b379", "Other invertebrates" = "#cab2d6")

colnames(df)[3] <-"Group"
df1= df %>% group_by(Country, Group) %>% summarise(n = n()) %>% mutate(
  Country = factor(Country, levels = c( "Spain", "Portugal","Andorra", "Gibraltar")) )

unique(df1$Group)
#install.packages("upstartr"); library(upstartr)

p<-ggplot(df1, aes(Group, n, fill = Group)) + geom_bar(stat = "identity",size= 0.2, color ="black")+
facet_wrap(vars(Country), scales = "free_x", strip.position = "top") +
  scale_fill_manual(values = group_colors) + coord_flip()+
  theme_bw() +  labs(x="", y="") +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none")
ggsave(p, filename = "FigS1.svg", width = 10, height = 5, dpi = 300)


#### Figure S3: Temporal focus ------------
# Data comes from temporal section

p = ggplot(res_anual, aes(x = year, y = n, color = ISO3, group = ISO3)) +
  ylim(0,30)+  xlim(1900,2025)+
  scale_x_continuous(breaks = c(1900,1920,1940,1960,1980,2000,2023), limits = c(1900, 2023))+
  theme_bw(base_size = 12) + coord_cartesian(clip = 'on')+
  geom_point(size = 2, alpha=0.35) + scale_fill_manual(values = c("grey70" = "lightgrey", "white" = "white"), guide = "none")+
  scale_color_manual(values = location_colors, name = "Country") +
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
  geom_vline(xintercept = c(1800, 1986, 2014), linetype = "dashed", color = "grey50", size = 0.5, alpha=0.5) 

p



# This is for Table S3: 
most_representative <- df %>%
  group_by(Class, Family, Taxon, Group) %>%
  summarise(Loc = n_distinct(Location), .groups = "drop") %>%
  arrange(Class, desc(Loc)) %>%
  group_by(Class, Family) 

# --- 

