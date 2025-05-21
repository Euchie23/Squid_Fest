pca_kmeans_cluster_explorer <- function(data_list, remove.vars = FALSE) {
  
  # Extract main dataset from input list
  dataset_with_numerical_values <- as.data.frame(data_list$dataset_with_numerical_values)
  
  # Determine whether data is for trace metals or organic compounds
  if (grepl("Metal", colnames(dataset_with_numerical_values)[16])) {
    number_range <- 16:25  # column range for metals
    vars <- c('Metal_H', "Metal_I")  # variables to remove (e.g., high % zero or low variance)
    output_folder <- "Trace_metals"
  } else {
    number_range <- 16:19  # column range for organics
    vars <- c("Organic_C")
    output_folder <- "Organic_compounds"
  }
  
  # Set path where results will be saved
  output_folder <- file.path(
    "/Users/mrnobody/Documents/GitHub/Squid_Fest/Squid_Concentration_Analysis/3-Data_Mining/Data_mining_plots/pca_kmeans_clusters/",
    output_folder
  )
  
  # Define all tissues and years of interest
  tissues <- c("liver", "stomach", "muscle", "inksac")
  years <- c('2019', '2020', '2021')
  
  # Create output folder if it doesn't exist
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  
  # Loop through each tissue type
  for (tissue in tissues) {
    
    # Create folder for tissue-specific results
    tissue_folder <- file.path(output_folder, tissue)
    dir.create(tissue_folder, recursive = TRUE, showWarnings = FALSE)
    
    # Subset dataset to relevant columns (pollutants + metadata)
    subsetted_dataset <- dataset_with_numerical_values[, c(number_range, 3, 4, 5, 6)]
    
    # Optionally remove columns (e.g., high zero %, low variance)
    if (remove.vars) {
      subsetted_dataset <- subsetted_dataset %>% select(!all_of(vars))
    }
    
    # ✅ Define pollutant names dynamically (all numeric columns except metadata)
    pollutants <- colnames(subsetted_dataset)[!colnames(subsetted_dataset) %in% c("Year", "Area", "ID_num", "Tissue")]
    
    # Subset by current tissue type
    subsetted_tissue_dataset <- subsetted_dataset %>% filter(Tissue == tissue)
    
    # Set colors for different years
    year_colors <- c("2019" = "red", "2020" = "green", "2021" = "blue")
    year_vector_all <- as.character(subsetted_tissue_dataset$Year)
    
    # Prepare data for PCA by removing metadata and converting to numeric
    pca_data_all_years <- subsetted_tissue_dataset %>%
      select(-Year, -Area, -ID_num, -Tissue) %>%
      mutate(across(everything(), as.numeric))
    
    # Run PCA
    pca_all <- PCA(pca_data_all_years, graph = FALSE)
    point_colors_all <- year_colors[year_vector_all]
    
    # PCA biplot with ellipses and variable labels
    biplot_all <- fviz_pca_biplot(pca_all, col.ind = subsetted_tissue_dataset$Year,
                                  palette = point_colors_all,
                                  addEllipses = TRUE, label = "var",
                                  col.var = "black", repel = TRUE,
                                  legend.title = "Years",
                                  title = paste("PCA -", tissue, "- All Years"))
    
    # Perform K-means clustering on PCA results (first 2 PCs)
    kmeans_all <- kmeans(pca_all$ind$coord[, 1:2], centers = 3)
    cluster_df_all <- subsetted_tissue_dataset
    cluster_df_all$Cluster <- as.factor(kmeans_all$cluster)
    
    # PCA plot colored by cluster membership
    kmeans_biplot_all <- fviz_pca_ind(pca_all, geom.ind = "point", pointshape = 21,
                                      fill.ind = cluster_df_all$Cluster,
                                      col.ind = "black", addEllipses = TRUE,
                                      ellipse.type = "norm", label = "none",
                                      palette = "jco",
                                      title = paste("K-means Clusters -", tissue, "- All Years"))
    
    # Reshape data to long format for plotting
    cluster_long_all <- cluster_df_all %>%
      select(Cluster, all_of(pollutants)) %>%
      pivot_longer(-Cluster, names_to = "variable", values_to = "value")
    
    # Boxplots + jitter plots by cluster for each pollutant
    boxplot_all <- ggplot(cluster_long_all, aes(x = Cluster, y = value, fill = Cluster)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.7) +  # cleaner boxplot, remove outliers
      geom_jitter(shape = 21, width = 0.2, size = 1.5, alpha = 0.6, color = "black") +
      facet_wrap(~ variable, scales = "free_y") +
      theme_minimal() +
      labs(title = paste("Pollutant Distributions by Cluster -", tissue, "- All Years")) +
      scale_fill_brewer(palette = "Set1")
    
    # Save plots for all years
    png(file.path(tissue_folder, paste0(tissue, "_All_Years_PCA_KMeans_Boxplot.png")), width = 1800, height = 900)
    grid.arrange(biplot_all, kmeans_biplot_all, boxplot_all, ncol = 3)
    dev.off()
    
    # --- Loop through individual years ---
    for (year in years) {
      subset_year <- subsetted_tissue_dataset %>% filter(Year == year)
      if (nrow(subset_year) == 0) next  # skip if no data for that year
      
      # Prepare PCA data for year
      pca_year_data <- subset_year %>%
        select(-Year, -Area, -ID_num, -Tissue) %>%
        mutate(across(everything(), as.numeric))
      
      # Run PCA for current year
      pca_year <- PCA(pca_year_data, graph = FALSE)
      kmeans_year <- kmeans(pca_year$ind$coord[, 1:2], centers = 3)
      subset_year$Cluster <- as.factor(kmeans_year$cluster)
      
      # PCA biplot for year
      pca_biplot <- fviz_pca_biplot(pca_year, col.ind = year_colors[year],
                                    addEllipses = TRUE, label = "var",
                                    col.var = "black", repel = TRUE,
                                    title = paste("PCA -", tissue, year))
      
      # K-means plot for year
      kmeans_biplot <- fviz_pca_ind(pca_year, geom.ind = "point", pointshape = 21,
                                    fill.ind = subset_year$Cluster,
                                    col.ind = "black", addEllipses = TRUE,
                                    ellipse.type = "norm", label = "none",
                                    palette = "jco",
                                    title = paste("K-means Clusters -", tissue, year))
      
      # Prepare data for boxplot
      cluster_long <- subset_year %>%
        select(Cluster, all_of(pollutants)) %>%
        pivot_longer(-Cluster, names_to = "variable", values_to = "value")
      
      # Boxplots + jitter for each cluster and pollutant
      boxplot_year <- ggplot(cluster_long, aes(x = Cluster, y = value, fill = Cluster)) +
        geom_boxplot(outlier.shape = NA, alpha = 0.7) +
        geom_jitter(shape = 21, width = 0.2, size = 1.5, alpha = 0.6, color = "black") +
        facet_wrap(~ variable, scales = "free_y") +
        theme_minimal() +
        labs(title = paste("Pollutant Distributions by Cluster -", tissue, year)) +
        scale_fill_brewer(palette = "Set1")
      
      # Save plots for current year
      png(file.path(tissue_folder, paste0(tissue, "_", year, "_PCA_KMeans_Boxplot.png")), width = 1800, height = 900)
      grid.arrange(pca_biplot, kmeans_biplot, boxplot_year, ncol = 3)
      dev.off()
    }
  }
}