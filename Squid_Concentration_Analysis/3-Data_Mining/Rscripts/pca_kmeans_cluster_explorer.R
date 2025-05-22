#LOADING LIBRARIES----
library(dplyr) # for data manipulation
library(FactoMineR) #for providing functions for multivariate data analysis, especially Principal Component Analysis (PCA) and similar methods.
library(factoextra) # for enhancing visualization of multivariate analyses (like PCA) from FactoMineR with intuitive and publication-ready plots.
library(ggplot2) # for  data visualization
library(gridExtra) # for multiple ggplot2 plots to be arranged in a grid layout for combined display or export.
library(grid) # for the grid graphics system in R, which provides low-level functions for creating and controlling complex graphic layouts
library(RColorBrewer)  #  for Color palettes
library(stringr) # For String Manipulation


#Data Processor Function before doing analysis
#It is recommended that the 'Keep_LOQ_values' argument remain as FALSE to remove all values below blank control (BB) or LOQ (BLOQ) to get a more accurate figure for the number of outliers within the dataset for the data distribution otherwise it will also classify values that are BB or BLOQ  as outliers leading to couble counting.
process_dataset <- function(data, keep_LOQ_values=FALSE) {
  # Check if the dataset is Trace Metals (Ag is in the 16th column)
  if (grepl("Metal", colnames(data)[16])) {
    
    # Processing for Trace Metals dataset
    data <- data %>%
      relocate(1:15, colnames(data)[16:25]) %>%
      mutate(across(4, ~ paste0(., ".")))  # Add "." to column 4
    
    # FUNCTIONS TO HELP MODIFY ID NUMBERS FOR FURTHER ANALYSIS (TM)
    ID_num_modifier <- function(x){#padding 0 to first two digits
      if (str_detect(substring(x[4],1,2),"\\.") == TRUE){ 
        modified_id=paste('0',substring(x[4],1,))
        modified_id<- gsub(" ", "",  modified_id)
        x[4] <- gsub(substring(x[4],1,), modified_id,  x[4])
      }
      return(x)
    }
    
    
    # Apply the 'ID_num_modifier' function and remove "." from the 'Area' column for data version 1 with numerical values as concentrationentrations
    data1 <- as.data.frame(t(apply(data, MARGIN = 1, ID_num_modifier)))
    data1$ID_num <- gsub("\\.", "", data1$ID_num)
    
    # Apply the 'ID_num_modifier' function and remove "." from the 'Area' column for data version 2 with classification groups (categorical values) representing concentration values
    data2 <- as.data.frame(t(apply(data, MARGIN = 1, ID_num_modifier)))
    data2$ID_num <- gsub("\\.", "", data2$ID_num)
    
    if(keep_LOQ_values == FALSE){
      cat("The LOQ values were replaced with 0.\n")
      # Replace "BLOQ" values with 0 in the relevant columns (16:25)
      data1[, 16:25] <- lapply(data1[, 16:25], gsub, pattern = ".*BLOQ.*", replacement = 0)
      data1[,c(16:25)] <- lapply(data1[,c(16:25)], gsub, pattern = ".*BB.*", replacement = 0)
      data1[,c(16:25)] <- lapply(data1[,c(16:25)], gsub, pattern = "^0$", replacement = 0)
      data1[,c(16:25)]  <- lapply(data1[,c(16:25)] , gsub, pattern = "N/A", replacement = 0)
      
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = ".*BLOQ.*", replacement = 'BB+BLOQ')
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = ".*BB.*", replacement = 'BB+BLOQ')
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = "^0$", replacement = 'BLOD')
      data2[,c(16:25)]  <- lapply(data2[,c(16:25)] , gsub, pattern = "N/A", replacement = 'BLOD')
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = "[0-9.]+", replacement = "Detected")
      return(list(dataset_with_numerical_values=data1, dataset_with_categorical_values=data2))
      
    }else{
      # Removing "BLOQ" and "BB" values in the relevant columns (16:25)
      data1[, 16:25] <- lapply(data1[, 16:25], gsub, pattern = " BLOQ", replacement = "")
      data1[,c(16:25)] <- lapply(data1[,c(16:25)], gsub, pattern = " BB", replacement = "")
      data1[,c(16:25)] <- lapply(data1[,c(16:25)], gsub, pattern = "^0$", replacement = 0)
      data1[,c(16:25)]  <- lapply(data1[,c(16:25)] , gsub, pattern = "N/A", replacement = 0)
      
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = ".*BLOQ.*", replacement = 'BB+BLOQ')
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = ".*BB.*", replacement = 'BB+BLOQ')
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = "^0$", replacement = 'BLOD')
      data2[,c(16:25)]  <- lapply(data2[,c(16:25)] , gsub, pattern = "N/A", replacement = 'BLOD')
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = "[0-9.]+", replacement = "Detected")
      
      process_with_user_input <- function() {
        # Ask the user if they want to keep LOQ values
        keep_loq <- readline(prompt = "Do you want to keep the original LOQ values? (yes/no): ")
        
        if (tolower(keep_loq) == "yes") {
          cat("You chose to keep LOQ values.\n")
          return("Keep LOQ") # Return a clear message indicating the choice
        } else if (tolower(keep_loq) == "no") {
          multiplier <- NA
          while (is.na(multiplier)) {
            user_input <- readline(prompt = "What number do you want to multiply the LOQ values by? ")
            multiplier <- suppressWarnings(as.numeric(user_input))
            
            if (is.na(multiplier)) {
              cat("Invalid input. Please enter a numeric value.\n")
            }
          }
          
          cat("You chose a multiplier of ", multiplier, ".\n")
          return(multiplier) # Return the chosen multiplier
        } else {
          cat("Invalid input. Please answer with 'yes' or 'no'.\n")
          return(NULL) # Return NULL for invalid input
        }
      }
      
      # To get user input for 
      user_choice <- process_with_user_input()
      
      # Modify "BLOQ" and "BB" values with userinput in the relevant columns (16:25)
      process_data <- function(data, multiplier) {
        # Validate user input
        if (is.na(multiplier)) {
          stop("Invalid multiplier. Please enter a numeric value.")
        }
        
        data1[, 16:25] <- lapply(16:25, function(index) {
          column <- data[[index]]  # Extract the column
          # Identify which values contain "BLOQ" or "BB"
          bloq_bb_mask <- grepl(" BLOQ| BB", column)
          # Remove "BLOQ" and "BB" text and convert to numeric
          cleaned_column <- suppressWarnings(as.numeric(gsub(" BLOQ| BB", "", column)))
          
          # Multiply only the masked values by the multiplier
          if (is.numeric(multiplier)) {
            cleaned_column[bloq_bb_mask] <- cleaned_column[bloq_bb_mask] * multiplier
          }
          
          # Return the updated column
          return(cleaned_column)
        })
        
        # Return the updated dataset
        return(list(dataset_with_numerical_values=data1, dataset_with_categorical_values=data2))
      }
      data1 <-process_data(data, user_choice)
      #return(list(dataset_with_numerical_values=data1, dataset_with_categorical_values=data2))
    }
    
  } else {
    # Processing for Organic Compounds dataset
    data <- data %>%
      relocate(Area, .after = ID_num) %>%
      mutate(across(4, ~ paste0(., ".")))  # Add "." to column 4
    
    # Convert column 6 to lowercase
    data[, 6] <- tolower(data[, 6])
    
    
    # FUNCTIONS TO HELP MODIFY ID NUMBERS FOR FURTHER ANALYSIS (TM)
    ID_num_modifier <- function(x){#padding 0 to first two digits
      if (str_detect(substring(x[4],1,2),"\\.") == TRUE){ 
        sk=paste('0',substring(x[4],1,))
        sk<- gsub(" ", "",  sk)
        x[4] <- gsub(substring(x[4],1,), sk,  x[4])
      }
      return(x)
    }
    
    # Apply the 'ID_num_modifier' function and remove "." from the 'Area' column for data version 1 with numerical values as concentrations
    data1 <- as.data.frame(t(apply(data, MARGIN = 1, ID_num_modifier)))
    data1$ID_num <- gsub("\\.", "", data1$ID_num)
    
    # Apply the 'ID_num_modifier' function and remove "." from the 'Area' column for data version 2 with classification groups representing concentration values
    data2 <- as.data.frame(t(apply(data, MARGIN = 1, ID_num_modifier)))
    data2$ID_num <- gsub("\\.", "", data2$ID_num)
    
    if(keep_LOQ_values == FALSE){
      cat("The LOQ values were replaced with 0.\n")
      # Replace "BLOQ", "N/A", and "0" values with 0 in the relevant columns (16:19)
      data1[, 16:19] <- lapply(data1[, 16:19], gsub, pattern = ".*BLOQ.*", replacement = 0)
      data1[, 16:19] <- lapply(data1[, 16:19], gsub, pattern = "N/A", replacement = 0)
      data1[, 16:19] <- lapply(data1[, 16:19], gsub, pattern = "^0$", replacement = 0)
      
      data2[,c(16:19)] <- lapply(data2[,c(16:19)], gsub, pattern = ".*BLOQ.*", replacement = 'BLOQ')
      data2[,c(16:19)] <- lapply(data2[,c(16:19)], gsub, pattern = "^0$", replacement = 'BLOD')
      data2[,c(16:19)]  <- lapply(data2[,c(16:19)] , gsub, pattern = "N/A", replacement = 'BLOD')
      data2[,c(16:19)] <- lapply(data2[,c(16:19)], gsub, pattern = "[0-9.]+", replacement = "Detected")
      
      return(list(dataset_with_numerical_values=data1, dataset_with_categorical_values=data2))
      
    }else{
      # Removing "BLOQ" and  values in the relevant columns (16:19)
      data1[, 16:19] <- lapply(data1[, 16:19], gsub, pattern = " BLOQ", replacement = "")
      data1[, 16:19] <- lapply(data1[, 16:19], gsub, pattern = "N/A", replacement = 0)
      data1[, 16:19] <- lapply(data1[, 16:19], gsub, pattern = "^0$", replacement = 0)
      
      data2[,c(16:19)] <- lapply(data2[,c(16:19)], gsub, pattern = ".*BLOQ.*", replacement = 'BLOQ')
      data2[,c(16:19)] <- lapply(data2[,c(16:19)], gsub, pattern = "^0$", replacement = 'BLOD')
      data2[,c(16:19)]  <- lapply(data2[,c(16:19)] , gsub, pattern = "N/A", replacement = 'BLOD')
      data2[,c(16:19)] <- lapply(data2[,c(16:19)], gsub, pattern = "[0-9.]+", replacement = "Detected")
      
      process_with_user_input <- function() {
        # Ask the user if they want to keep LOQ values
        keep_loq <- readline(prompt = "Do you want to keep LOQ values? (yes/no): ")
        
        if (tolower(keep_loq) == "yes") {
          cat("You chose to keep LOQ values.\n")
          return("Keep LOQ") # Return a clear message indicating the choice
        } else if (tolower(keep_loq) == "no") {
          multiplier <- NA
          
          while (is.na(multiplier)) {
            user_input <- readline(prompt = "What number do you want to multiply the LOQ values by? ")
            multiplier <- suppressWarnings(as.numeric(user_input))
            
            if (is.na(multiplier)) {
              cat("Invalid input. Please enter a numeric value.\n")
            }
          }
          
          cat("You chose a multiplier of ", multiplier, ".\n")
          return(multiplier) # Return the chosen multiplier
        } else {
          cat("Invalid input. Please answer with 'yes' or 'no'.\n")
          return(NULL) # Return NULL for invalid input
        }
      }
      
      # To get user input for 
      user_choice <- process_with_user_input()
      
      # Modify "BLOQ" and "BB" values with userinput in the relevant columns (16:25)
      process_data <- function(data, multiplier) {
        # Validate user input
        if (is.na(multiplier)) {
          stop("Invalid multiplier. Please enter a numeric value.")
        }
        data1[, 16:19] <- lapply(16:19, function(index) {
          column <- data[[index]]  # Extract the column
          # Identify which values contain "BLOQ" or "BB"
          bloq_bb_mask <- grepl(" BLOQ| BB", column)
          # Remove "BLOQ" and "BB" text and convert to numeric
          cleaned_column <- suppressWarnings(as.numeric(gsub(" BLOQ| BB", "", column)))
          
          # Multiply only the masked values by the multiplier
          if (is.numeric(multiplier)) {
            cleaned_column[bloq_bb_mask] <- cleaned_column[bloq_bb_mask] * multiplier
          }
          
          # Return the updated column
          return(cleaned_column)
        })
        
        # Return the updated dataset
        #return(list(dataset_with_numerical_values=data1, dataset_with_categorical_values=data2))
      }
      data1 <-process_data(data, user_choice)
      return(list(dataset_with_numerical_values=data1, dataset_with_categorical_values=data2))
    }
  }
}


# Data Processing for Trace Metals dataset
tracemetals_data <- read.csv("Squid_Concentration_Analysis/3-Data_Mining/Datasets/preprocessed_data/Final_TMresults_mgkg.csv", header = TRUE)
datasets_for_trace_metals <- process_dataset(tracemetals_data, keep_LOQ_values = FALSE) 


# Data Processing for Organic Compounds dataset
organiccompounds_data <- read.csv("Squid_Concentration_Analysis/3-Data_Mining/Datasets/preprocessed_data/Final_OCresults_mgkg.csv", header = TRUE)
datasets_for_organic_compounds <- process_dataset(organiccompounds_data, keep_LOQ_values = FALSE)

# Main function: analyze_tissue_pollutants_with_pca_kmeans
# This function performs dimensionality reduction and unsupervised clustering on pollutant concentration data in squid tissues.It processes each tissue type (e.g., liver, stomach, muscle, ink sac) and conducts Principal Component Analysis (PCA) to identify major variation patterns in pollutant profiles (Trace Metals or Organic Compounds).K-means clustering is then applied to group samples based on similarity in these profiles.For each tissue and each year (e.g., 2019, 2020, 2021), as well as across all years combined. Lastly, Kruskal-Wallis test is used here to statistically evaluate whether the distribution of each metadata variable (e.g.,gender, mantle length, Month_of_Capture etc) differs significantly between clusters identified by k-means clustering.
# the function generates and saves side-by-side plots that include:
#   - Standard PCA biplots (showing variance and variable loadings)
#   - K-means PCA biplots (colored by clusters)
#   - Boxplots showing pollutant distributions across clusters
#   - Pvalues from Kruskal Wallis test between metadata vs clusters

# These plots are saved in organized folders by pollutant type and tissue.
# The function also provides numerical summaries for each cluster, helping interpret which pollutants dominate each group. This approach supports identifying contamination patterns, comparing temporal pollutant profiles, and discovering distinct sample groupings based on chemical makeup.

# Full Function: PCA + KMeans + Metadata Kruskal-Wallis with p-values printed on plots

# Full Function: PCA + KMeans + Metadata Kruskal-Wallis with p-values on plots
pca_kmeans_cluster_explorer <- function(data_list, remove.vars = FALSE) {
  
  dataset_with_numerical_values <- as.data.frame(data_list$dataset_with_numerical_values)
  
  
  #Changing metadata colums from dataset into numeric or factor
  dataset_with_numerical_values[,c(7,8,9,13,14)] =as.numeric(unlist(dataset_with_numerical_values[,c(7,8,9,13,14)]))
  dataset_with_numerical_values[,12]<- cut(as.numeric(dataset_with_numerical_values[,12]), breaks = 4, labels =c(3,4,5,6))
  dataset_with_numerical_values[,c(10,11,15)]<- as.character(dataset_with_numerical_values[,c(10,11,15)])
  
  
  # Determine compound type and set variables and output folder
  if (grepl("Metal", colnames(dataset_with_numerical_values)[16])) {
    number_range <- 16:25
    vars <- c('Metal_H', "Metal_I")
    output_folder <- "Trace_metals"
  } else {
    number_range <- 16:19
    vars <- c("Organic_C")
    output_folder <- "Organic_compounds"
  }
  
  output_folder <- file.path("Squid_Concentration_Analysis/3-Data_Mining/Data_mining_plots/pca_kmeans_clusters/", output_folder)
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  
  # Loop through each tissue and year
  tissues <- c("liver", "stomach", "muscle", "inksac")
  years <- c('2019', '2020', '2021')
  
  for (tissue in tissues) {
    
    tissue_folder <- file.path(output_folder, tissue)
    dir.create(tissue_folder, recursive = TRUE, showWarnings = FALSE)
    
    # Select variables and remove unwanted ones if needed
    subsetted_dataset <- dataset_with_numerical_values[, c(number_range, 3, 4, 5, 6)]
    if (remove.vars) {
      subsetted_dataset <- subsetted_dataset %>% select(!all_of(vars))
    }
    
    pollutants <- colnames(subsetted_dataset)[!colnames(subsetted_dataset) %in% c("Year", "Area", "ID_num", "Tissue")]
    
    #For metadata evaluation with kw test
    metadata <- dataset_with_numerical_values[,c(7:15)]
    metadata_vars <- colnames(metadata)
    
    #subsetted for PCA and K-Means analysis
    subsetted_tissue_dataset <- subsetted_dataset %>% filter(Tissue == tissue)
    
    #subsetted for kw test
    subsetted_tissue_dataset_kw <- dataset_with_numerical_values %>% filter(Tissue == tissue)
    year_colors <- c("2019" = "red", "2020" = "green", "2021" = "blue")
    year_vector_all <- as.character(subsetted_tissue_dataset$Year)
    
    # --- PCA and Clustering for All Years ---
    pca_data_all_years <- subsetted_tissue_dataset %>%
      select(-Year, -Area, -ID_num, -Tissue) %>%
      mutate(across(everything(), as.numeric))
    
    pca_all <- PCA(pca_data_all_years, graph = FALSE)
    kmeans_all <- kmeans(pca_all$ind$coord[, 1:2], centers = 3)
    cluster_df_all <- subsetted_tissue_dataset_kw
    cluster_df_all$Cluster <- as.factor(kmeans_all$cluster)
    
    # PCA + KMeans plots
    biplot_all <- fviz_pca_biplot(pca_all, col.ind = year_vector_all,
                                  palette = year_colors,
                                  addEllipses = TRUE, label = "var",
                                  col.var = "black", repel = TRUE,
                                  legend.title = "Years",
                                  title = paste("PCA -", tissue, "- All Years"))
    
    kmeans_biplot_all <- fviz_pca_ind(pca_all, geom.ind = "point", pointshape = 21,
                                      fill.ind = cluster_df_all$Cluster,
                                      col.ind = "black", addEllipses = TRUE,
                                      ellipse.type = "norm", label = "none",
                                      palette = "jco",
                                      title = paste("K-means Clusters -", tissue, "- All Years"))
    
    # Boxplots for pollutant variables by cluster
    cluster_long_all <- cluster_df_all %>%
      select(Cluster, all_of(pollutants)) %>%
      pivot_longer(-Cluster, names_to = "variable", values_to = "value")
    
    boxplot_all <- ggplot(cluster_long_all, aes(x = Cluster, y = value, fill = Cluster)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.7) +
      geom_jitter(shape = 21, width = 0.2, size = 1.5, alpha = 0.6, color = "black") +
      facet_wrap(~ variable, scales = "free_y") +
      theme_minimal() +
      labs(title = paste("Pollutant Distributions by Cluster -", tissue, "- All Years")) +
      scale_fill_brewer(palette = "Set1")
    
    # --- Kruskal-Wallis Test (All Years) ---
    kw_df_all <- data.frame()
    cat("\nMetadata Association Tests -", tissue, "(All Years):\n")
    for (var in metadata_vars) {
      if (length(unique(cluster_df_all[[var]])) > 1) {
        test_result <- kruskal.test(as.formula(paste(var, "~ Cluster")), data = cluster_df_all)
        kw_df_all <- rbind(kw_df_all, data.frame(Variable = var, PValue = test_result$p.value))
        cat(sprintf("%s: p = %.4f\n", var, test_result$p.value))
      }
    }
    
    # Add significance stars and sort
    kw_df_all <- kw_df_all %>%
      mutate(Significance = case_when(
        PValue < 0.001 ~ "***",
        PValue < 0.01  ~ "**",
        PValue < 0.05  ~ "*",
        TRUE ~ ""
      )) %>%
      arrange(PValue) %>%
      mutate(Label = sprintf("%s: p = %.4f %s", Variable, PValue, Significance))
    
    # Plot text results
    kw_text_plot_all <- ggplot() +
      annotate("text", x = 0, y = seq_along(kw_df_all$Label)*0.5, label = kw_df_all$Label, hjust = 0, size = 6) +
      theme_void() +
      labs(title = "Kruskal-Wallis P-values") +
      theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
    
    # Save PNG
    png(file.path(tissue_folder, paste0(tissue, "_All_Years_PCA_KMeans_Boxplot.png")), width = 1800, height = 1200)
    grid.arrange(biplot_all, kmeans_biplot_all, boxplot_all, kw_text_plot_all, ncol = 2)
    dev.off()
    
    # --- Loop through each individual year ---
    for (year in years) {
      #subsetted for PCA and K-Means analysis
      subset_year <- subsetted_tissue_dataset %>% filter(Year == year)
      #subsetted for kw test
      subset_year_kw <- subsetted_tissue_dataset_kw %>% filter(Year == year)
      if (nrow(subset_year) == 0) next
      
      # PCA & KMeans
      pca_year_data <- subset_year %>%
        select(-Year, -Area, -ID_num, -Tissue) %>%
        mutate(across(everything(), as.numeric))
      
      pca_year <- PCA(pca_year_data, graph = FALSE)
      kmeans_year <- kmeans(pca_year$ind$coord[, 1:2], centers = 3)
      cluster_df <- subset_year_kw
      cluster_df$Cluster <- as.factor(kmeans_year$cluster)
      
      pca_biplot <- fviz_pca_biplot(pca_year, col.ind = year_colors[year],
                                    addEllipses = TRUE, label = "var",
                                    col.var = "black", repel = TRUE,
                                    title = paste("PCA -", tissue, year))
      
      kmeans_biplot <- fviz_pca_ind(pca_year, geom.ind = "point", pointshape = 21,
                                    fill.ind = cluster_df$Cluster,
                                    col.ind = "black", addEllipses = TRUE,
                                    ellipse.type = "norm", label = "none",
                                    palette = "jco",
                                    title = paste("K-means Clusters -", tissue, year))
      
      # Boxplots
      cluster_long <- cluster_df %>%
        select(Cluster, all_of(pollutants)) %>%
        pivot_longer(-Cluster, names_to = "variable", values_to = "value")
      
      boxplot_year <- ggplot(cluster_long, aes(x = Cluster, y = value, fill = Cluster)) +
        geom_boxplot(outlier.shape = NA, alpha = 0.7) +
        geom_jitter(shape = 21, width = 0.2, size = 1.5, alpha = 0.6, color = "black") +
        facet_wrap(~ variable, scales = "free_y") +
        theme_minimal() +
        labs(title = paste("Pollutant Distributions by Cluster -", tissue, year)) +
        scale_fill_brewer(palette = "Set1")
      
      # Kruskal-Wallis Test (Per Year)
      kw_df <- data.frame()
      cat("\nMetadata Association Tests -", tissue, year, ":\n")
      for (var in metadata_vars) {
        if (length(unique(cluster_df[[var]])) > 1) {
          test_result <- kruskal.test(as.formula(paste(var, "~ Cluster")), data = cluster_df)
          kw_df <- rbind(kw_df, data.frame(Variable = var, PValue = test_result$p.value))
          cat(sprintf("%s: p = %.4f\n", var, test_result$p.value))
        }
      }
      
      kw_df <- kw_df %>%
        mutate(Significance = case_when(
          PValue < 0.001 ~ "***",
          PValue < 0.01  ~ "**",
          PValue < 0.05  ~ "*",
          TRUE ~ ""
        )) %>%
        arrange(PValue) %>%
        mutate(Label = sprintf("%s: p = %.4f %s", Variable, PValue, Significance))
      
      kw_text_plot <- ggplot() +
        annotate("text", x = 0, y = seq_along(kw_df$Label)*0.5, label = kw_df$Label, hjust = 0, size = 6) +
        theme_void() +
        labs(title = "Kruskal-Wallis P-values") +
        theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
      
      png(file.path(tissue_folder, paste0(tissue, "_", year, "_PCA_KMeans_Boxplot.png")), width = 1800, height = 1200)
      grid.arrange(pca_biplot, kmeans_biplot, boxplot_year, kw_text_plot, ncol = 2)
      dev.off()
    }
  }
}


# Run the function (example)
pca_kmeans_cluster_explorer(datasets_for_trace_metals, remove.vars = TRUE)
