# Code to create Supplementary Material 

# This is for Table S3: 
most_representative <- df %>%
  group_by(Class, Family, Taxon, Group) %>%
  summarise(Loc = n_distinct(Location), .groups = "drop") %>%
  arrange(Class, desc(Loc)) %>%
  group_by(Class, Family) 

# --- 

