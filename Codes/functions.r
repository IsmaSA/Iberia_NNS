
pacman::p_load(stringdist)


# Custom functions:

similar <- function(x, max_distance = 1, method = "lv") {
  
  original_na <- is.na(x)
  x_clean <- x[!original_na]
  
  if (length(x_clean) == 0) return(x)  
  
  freq_table <- table(x_clean)
  unique_terms <- names(freq_table)
  
  if (length(unique_terms) == 1) return(x) 
  
  dist <- stringdist::stringdistmatrix(
    unique_terms, 
    unique_terms, 
    method = method
  )
  
  clust <- stats::hclust(stats::as.dist(dist), method = "complete")
  clusters <- stats::cutree(clust, h = max_distance)
  
  group_report <- lapply(unique(clusters), function(cl) {
    members <- unique_terms[clusters == cl]
    canonical <- names(which.max(freq_table[members]))
    counts <- as.integer(freq_table[members])
    
    data.frame(
      Cluster = cl,
      Canonical_Term = canonical,
      Variant_Term = members,
      Count = counts,
      Distance = sapply(members, function(m) stringdist::stringdist(canonical, m, method = method))  ) })
  
  group_df <- do.call(rbind, group_report)
  term_mapping <- setNames(group_df$Canonical_Term, group_df$Variant_Term)
  
  result <- x
  mapped_values <- term_mapping[as.character(x_clean)]
  result[!original_na] <- ifelse(is.na(mapped_values), x_clean, unname(mapped_values))
  
  list(
    cleaned_vector = result,
    term_groups = group_df[order(group_df$Cluster, -group_df$Count), ] )
}
