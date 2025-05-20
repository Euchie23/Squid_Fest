#LOADING LIBRARIES----
library(dplyr) # for data manipulation
library(FactoMineR) #for providing functions for multivariate data analysis, especially Principal Component Analysis (PCA) and similar methods.
library(factoextra) # for enhancing visualization of multivariate analyses (like PCA) from FactoMineR with intuitive and publication-ready plots.
library(gridExtra) # for multiple ggplot2 plots to be arranged in a grid layout for combined display or export.

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
# This function performs dimensionality reduction and unsupervised clustering on pollutant concentration data in squid tissues. It processes each tissue (e.g., liver, stomach, muscle, ink sac) and runs a Principal Component Analysis (PCA) to visualize the major patterns in the each pollutant data (Trace Metals and Organic Compounds). It then applies K-means clustering to group the samples into clusters based on similarity in pollutant profiles. For each tissue and year (e.g., 2019, 2020, 2021), the function creates side-by-side plots comparing the PCA biplots (which show variance and variable loadings) and the K-means cluster plots (which group samples based on structure in the data). It also optionally performs this analysis across all years combined. The function automatically saves these visualizations to well-organized folders, and it summarizes each cluster’s pollutant composition numerically to help with interpretation. This helps identify potential pollutant patterns or contamination profiles within and across years, and to explore which pollutants define each cluster.

pca_kmeans_tissue <- function(data_list, remove.vars = FALSE) {
  
  dataset_with_numerical_values <- as.data.frame(data_list$dataset_with_numerical_values)
  
  # Determine pollutant range and variables
  if (grepl("Metal", colnames(dataset_with_numerical_values)[16])) {
    number_range <- 16:25
    pca_vars <- colnames(dataset_with_numerical_values)[number_range]
    vars <- c('Metal_H', "Metal_I", "Metal_F", "Metal_J")
    output_folder <- file.path("/Users/mrnobody/Documents/GitHub/Squid_Fest/Squid_Concentration_Analysis/3-Data_Mining/Data_mining_plots/biplots_pca_tissue/Trace_metals")
  } else {
    number_range <- 16:19
    pca_vars <- colnames(dataset_with_numerical_values)[number_range]
    vars <- c("Organic_C")
    output_folder <- file.path("/Users/mrnobody/Documents/GitHub/Squid_Fest/Squid_Concentration_Analysis/3-Data_Mining/Data_mining_plots/biplots_pca_tissue/Organic_compounds")
  }
  
  # Subset data
  if (!remove.vars) {
    subsetted_dataset <- dataset_with_numerical_values[,c(number_range, 3,4,5,6)]
  } else {
    subsetted_dataset <- dataset_with_numerical_values[, c(number_range, 3,4,5,6)] %>%
      select(!all_of(vars))
  }
  
  tissues <- c("liver", "stomach", "muscle", "inksac")
  years <- c('2019', '2020', '2021')
  
  # Create parent folder if not exists
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  
  for (tissue in tissues) {
    
    # Create tissue subfolder inside parent folder
    tissue_folder <- file.path(output_folder, tissue)
    dir.create(tissue_folder, recursive = TRUE, showWarnings = FALSE)
    
    # Define consistent color mapping for years
    year_colors <- c("2019" = "red", "2020" = "green", "2021" = "blue")
    
    # Subset for current tissue all years
    subsetted_tissue_dataset <- subsetted_dataset %>% filter(Tissue == tissue)
    
    # ======= ALL YEARS PCA and K-means =========
    if (nrow(subsetted_tissue_dataset) > 0) {
      
      # Prepare data for PCA - all years combined
      pca_data_all_years <- subsetted_tissue_dataset %>%
        select(-Year, -Area, -ID_num, -Tissue) %>%
        mutate(across(everything(), as.numeric))
      
      pca_all_years <- PCA(pca_data_all_years, graph = FALSE)
      pca_coords_all_years <- as.data.frame(pca_all_years$ind$coord)
      
      # Run K-means clustering on first 2 PC dimensions
      set.seed(123)
      kmeans_all_years <- kmeans(pca_coords_all_years[,1:2], centers = 3)
      pca_coords_all_years$cluster <- as.factor(kmeans_all_years$cluster)
      
      # PCA biplot for all years
      biplot_all_years <- fviz_pca_biplot(pca_all_years, col.ind = subsetted_tissue_dataset$Year,
                                          palette = year_colors,
                                          addEllipses = TRUE, label = "var",
                                          col.var = "black", repel = TRUE,
                                          legend.title = "Year",
                                          title = paste("PCA - Biplot", tissue, "All Years"))
      
      # Scree plots
      scree_pc1 <- fviz_contrib(pca_all_years, choice = "var", axes = 1, top = 10,
                                title = paste("PC1 Contributions -", tissue, "All Years"))
      scree_pc2 <- fviz_contrib(pca_all_years, choice = "var", axes = 2, top = 10,
                                title = paste("PC2 Contributions -", tissue, "All Years"))
      
      # K-means clusters plot for all years
      biplot_kmeans_all <- fviz_pca_ind(pca_all_years,
                                        geom.ind = "point",
                                        col.ind = pca_coords_all_years$cluster,
                                        palette = "jco",
                                        addEllipses = TRUE,
                                        legend.title = "Cluster",
                                        title = paste("K-means Clusters -", tissue, "All Years"))
      
      # Save combined plot for all years
      combined_all_years_path <- file.path(tissue_folder, paste0(tissue, "_AllYears_PCA_and_KMeans.png"))
      
      png(combined_all_years_path, width = 1800, height = 600)
      grid.arrange(biplot_all_years, scree_pc1, scree_pc2, biplot_kmeans_all, ncol = 4)
      dev.off()
    }
    
    # =========== LOOP OVER YEARS ==============
    for (year in years) {
      
      # Subset for this tissue and year
      subset_year <- subsetted_tissue_dataset %>% filter(Year == year)
      
      # Some tissues might not have data for all years
      if (nrow(subset_year) == 0) next 
      
      # Prepare data for PCA - per year
      pca_data_year <- subset_year %>%
        select(-Year, -Area, -ID_num, -Tissue) %>%
        mutate(across(everything(), as.numeric))
      
      pca_year <- PCA(pca_data_year, graph = FALSE)
      
      # Run K-means clustering on first 2 PC dimensions for this year
      pca_coords_year <- as.data.frame(pca_year$ind$coord)
      set.seed(123)
      kmeans_year <- kmeans(pca_coords_year[,1:2], centers = 3)
      pca_coords_year$cluster <- as.factor(kmeans_year$cluster)
      
      # PCA biplot for year
      biplot_year <- fviz_pca_biplot(pca_year, col.ind = year_colors[as.character(year)],
                                     addEllipses = TRUE, label = "var",
                                     col.var = "black", repel = TRUE,
                                     title = paste("PCA - Biplot", tissue, year))
      
      # Scree plots for year
      scree_year_pc1 <- fviz_contrib(pca_year, choice = "var", axes = 1, top = 10,
                                     title = paste("PC1 Contributions -", tissue, year))
      scree_year_pc2 <- fviz_contrib(pca_year, choice = "var", axes = 2, top = 10,
                                     title = paste("PC2 Contributions -", tissue, year))
      
      # K-means cluster plot for year
      biplot_kmeans_year <- fviz_pca_ind(pca_year,
                                         geom.ind = "point",
                                         col.ind = pca_coords_year$cluster,
                                         palette = "jco",
                                         addEllipses = TRUE,
                                         legend.title = "Cluster",
                                         title = paste("K-means Clusters -", tissue, year))
      
      # Save combined plot for this year
      combined_year_path <- file.path(tissue_folder, paste0(tissue, "_", year, "_PCA_and_KMeans.png"))
      
      png(combined_year_path, width = 1800, height = 600)
      grid.arrange(biplot_year, scree_year_pc1, scree_year_pc2, biplot_kmeans_year, ncol = 4)
      dev.off()
      
      # ==== CLUSTER ANALYSIS & SUMMARY ====
      
      # Add cluster labels to subset_year
      subset_year$cluster <- kmeans_year$cluster
      
      #forcing columsn to numric data
    numeric_data <- subset_year[, pca_vars] %>%
        mutate(across(everything(), as.numeric))
      
      # Print cluster means of original variables
      cluster_summary <- aggregate(numeric_data, 
                                   by = list(Cluster = subset_year$cluster), 
                                   FUN = mean)
      message("Cluster summary for tissue: ", tissue, ", year: ", year)
      print(cluster_summary)
      
      # Optional: Save or plot boxplots for important variables here if you want
      
    } # end per year loop
    
  } # end tissue loop
  
  message("All PCA and K-means plots saved to folder: ", normalizePath(output_folder))
}

pca_kmeans_tissue(datasets_for_trace_metals, remove.vars = FALSE)
