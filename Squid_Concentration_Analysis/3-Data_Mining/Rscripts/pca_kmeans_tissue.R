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
# This function performs dimensionality reduction and unsupervised clustering on pollutant concentration data in squid tissues.It processes each tissue type (e.g., liver, stomach, muscle, ink sac) and conducts Principal Component Analysis (PCA) to identify major variation patterns in pollutant profiles (Trace Metals or Organic Compounds).K-means clustering is then applied to group samples based on similarity in these profiles.For each tissue and each year (e.g., 2019, 2020, 2021), as well as across all years combined,
# the function generates and saves side-by-side plots that include:
#   - Standard PCA biplots (showing variance and variable loadings)
#   - K-means PCA biplots (colored by clusters)
#   - Boxplots showing pollutant distributions across clusters

# These plots are saved in organized folders by pollutant type and tissue.
# The function also provides numerical summaries for each cluster, helping interpret which pollutants dominate each group. This approach supports identifying contamination patterns, comparing temporal pollutant profiles, and discovering distinct sample groupings based on chemical makeup.


pca_kmeans_cluster_explorer <- function(data_list, remove.vars = FALSE) {
  
  # Extract numeric dataset from list
  dataset_with_numerical_values <- as.data.frame(data_list$dataset_with_numerical_values)
  
  #Changing metadata colums from dataset into numeric or factor
  dataset_with_numerical_values[,c(7,8,9,13,14)] =as.numeric(unlist(dataset_with_numerical_values[,c(7,8,9,13,14)]))
  dataset_with_numerical_values[,12]<- cut(as.numeric(dataset_with_numerical_values[,12]), breaks = 4, labels =c(3,4,5,6))
  dataset_with_numerical_values[,c(10,11,15)]<- as.character(dataset_with_numerical_values[,c(10,11,15)])
  metadata <- dataset_with_numerical_values[,c(3:15)]
  
  # Define trace metal or organic compound setup
  if (grepl("Metal", colnames(dataset_with_numerical_values)[16])) {
    number_range <- 16:25
    vars_to_remove <- c('Metal_H', 'Metal_I')  # Example: zero-heavy or low-variance
    output_folder <- "Trace_metals"
  } else {
    number_range <- 16:19
    vars_to_remove <- c("Organic_C")
    output_folder <- "Organic_compounds"
  }
  
  # Create output folder
  output_folder <- file.path("Squid_Concentration_Analysis/3-Data_Mining/Data_mining_plots/pca_kmeans_clusters/", output_folder)
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  
  tissues <- c("liver", "stomach", "muscle", "inksac")
  years <- c('2019', '2020', '2021')
  
  for (tissue in tissues) {
    
    tissue_folder <- file.path(output_folder, tissue)
    dir.create(tissue_folder, recursive = TRUE, showWarnings = FALSE)
    
    # Filter dataset and optionally remove zero-heavy columns
    data_numeric <- dataset_with_numerical_values[, c(number_range, 3,4,5,6)]
    if (remove.vars) {
      data_numeric <- data_numeric %>% select(!all_of(vars_to_remove))
    }
    
    # Identify pollutant names after filtering
    pollutant_vars <- colnames(data_numeric)[!colnames(data_numeric) %in% c("Year", "Area", "ID_num", "Tissue")]
    
    # Subset data by tissue
    data_tissue <- data_numeric %>% filter(Tissue == tissue)
    
    # PCA input: just pollutant data
    pca_input <- data_tissue %>%
      select(-Year, -Area, -ID_num, -Tissue) %>%
      mutate(across(everything(), as.numeric))
    
    # Run PCA
    pca_all <- PCA(pca_input, graph = FALSE)
    
    # K-means on first 2 PCs
    kmeans_result <- kmeans(pca_all$ind$coord[, 1:2], centers = 3)
    data_tissue$Cluster <- as.factor(kmeans_result$cluster)
    
    # Combine with metadata if available
    if (!is.null(metadata)) {
      data_tissue <- left_join(data_tissue, metadata, by =  c("ID_num", "Area", "Tissue", "Year"))
    }
    
    # PCA biplot and cluster plot
    biplot_all <- fviz_pca_biplot(pca_all, col.ind = data_tissue$Cluster, label = "var",
                                  addEllipses = TRUE, repel = TRUE,
                                  col.var = "black", palette = "jco",
                                  title = paste("PCA Biplot -", tissue, "(All Years)"))
    
    cluster_plot <- fviz_pca_ind(pca_all, geom.ind = "point", pointshape = 21,
                                 fill.ind = data_tissue$Cluster, col.ind = "black",
                                 addEllipses = TRUE, ellipse.type = "norm",
                                 palette = "jco",
                                 title = paste("K-means Clustering -", tissue))
    
    # Long format for pollutant boxplots
    cluster_long <- data_tissue %>%
      select(Cluster, all_of(pollutant_vars)) %>%
      pivot_longer(-Cluster, names_to = "variable", values_to = "value")
    
    boxplot <- ggplot(cluster_long, aes(x = Cluster, y = value, fill = Cluster)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.2, alpha = 0.5, size = 0.7) +  # Jitter to show data points
      facet_wrap(~ variable, scales = "free_y", ncol = 3) +
      labs(title = paste("Pollutant Distributions by Cluster -", tissue)) +
      theme_minimal() + scale_fill_brewer(palette = "Set1")
    
    # Save final combined plot
    png(file.path(tissue_folder, paste0(tissue, "_PCA_KMeans_Boxplot.png")), width = 2400, height = 900)
    grid.arrange(biplot_all, cluster_plot, boxplot, ncol = 1)
    dev.off()
    
    # Kruskal-Wallis Test for each metadata variable
    if (!is.null(metadata)) {
      metadata_vars <- colnames(metadata)[!colnames(metadata) %in% c("ID_num")]
      cat("\nKruskal-Wallis Results -", tissue, "\n")
      for (var in metadata_vars) {
        if (is.numeric(data_tissue[[var]]) || is.integer(data_tissue[[var]])) {
          result <- kruskal.test(data_tissue[[var]] ~ data_tissue$Cluster)
          cat(var, ": p-value =", result$p.value, "\n")
        }
      }
    }
  }
}

# Run the function (example)
pca_kmeans_cluster_explorer(datasets_for_trace_metals, remove.vars = TRUE)
