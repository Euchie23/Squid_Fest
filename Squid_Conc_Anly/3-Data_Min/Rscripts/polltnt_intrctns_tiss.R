#LOADING LIBRARIES----
library(dplyr) # for data manipulation
library(fs) #for providing a modern, consistent, and cross-platform interface for working with the file system (e.g., creating, deleting, or navigating files and folders)
library(GGally) # for creating easy, multi-variable correlation and scatterplot matrices.
library(ggplot2) # for  data visualization
library(gridExtra) # for multiple ggplot2 plots to be arranged in a grid layout for combined display or export.
library(pheatmap) # for creating customizable heatmaps with clustering and annotations.
library(reshape2)  #  for transforming data between wide and long formats for easier analysis and visualization.
library(stringr) # For String Manipulation

#Data Processor Function before doing analysis
#It is recommended that the 'Keep_LOQ_values' argument remain as FALSE to remove all values below blank control (BB) or LOQ (BLOQ) to get a more accurate figure for the number of outliers within the dataset for the data distribution otherwise it will also classify values that are BB or BLOQ  as outliers leading to couble counting.
process_dataset<- function(data, keep_LOQ_values=FALSE) {
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
      
      
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = ".*BLOQ.*", replacement = 'BLOQ')
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = ".*BB.*", replacement = 'BLOQ')
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = "^0$", replacement = 'BLOD')
      data2[,c(16:25)]  <- lapply(data2[,c(16:25)] , gsub, pattern = "N/A", replacement = 'BLOD')
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = "[0-9.]+", replacement = "Detected")
      
      # Get the column names for columns 17 to 25
      cols_to_reorder <- names(data1)[16:25]
      
      # Sort the column names alphabetically
      sorted_cols <- sort(cols_to_reorder)
      
      # Reorder the columns based on the sorted names
      data1 <- data1[, c(1:15, match(sorted_cols, names(data1)))]
      data2 <- data2[, c(1:15, match(sorted_cols, names(data2)))]
      
      return(list(dataset_with_numerical_values=data1, dataset_with_categorical_values=data2))
      
    }else{
      # Removing "BLOQ" and "BB" values in the relevant columns (16:25)
      data1[, 16:25] <- lapply(data1[, 16:25], gsub, pattern = " BLOQ", replacement = "")
      data1[,c(16:25)] <- lapply(data1[,c(16:25)], gsub, pattern = " BB", replacement = "")
      data1[,c(16:25)] <- lapply(data1[,c(16:25)], gsub, pattern = "^0$", replacement = 0)
      data1[,c(16:25)]  <- lapply(data1[,c(16:25)] , gsub, pattern = "N/A", replacement = 0)
      
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = ".*BLOQ.*", replacement = 'BLOQ')
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = ".*BB.*", replacement = 'BLOQ')
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
        
        # Get the column names for columns 17 to 25
        cols_to_reorder <- names(data1)[16:25]
        
        # Sort the column names alphabetically
        sorted_cols <- sort(cols_to_reorder)
        
        # Reorder the columns based on the sorted names
        data1 <- data1[, c(1:15, match(sorted_cols, names(data1)))]
        data2 <- data2[, c(1:15, match(sorted_cols, names(data2)))]
        
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
      
      # Get the column names for columns 17 to 25
      cols_to_reorder <- names(data1)[16:25]
      
      # Sort the column names alphabetically
      sorted_cols <- sort(cols_to_reorder)
      
      # Reorder the columns based on the sorted names
      data1 <- data1[, c(1:15, match(sorted_cols, names(data1)))]
      data2 <- data2[, c(1:15, match(sorted_cols, names(data2)))]
      
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
        
        # Get the column names for columns 17 to 25
        cols_to_reorder <- names(data1)[16:25]
        
        # Sort the column names alphabetically
        sorted_cols <- sort(cols_to_reorder)
        
        # Reorder the columns based on the sorted names
        data1 <- data1[, c(1:15, match(sorted_cols, names(data1)))]
        data2 <- data2[, c(1:15, match(sorted_cols, names(data2)))]
        
        # # Return the updated dataset
        return(list(dataset_with_numerical_values=data1, dataset_with_categorical_values=data2))
        
      }
      data1 <-process_data(data, user_choice)
    }
  }
}


# Data Processing for Trace Metals dataset
tracemetals_data <- read.csv("Squid_Conc_Anly/3-Data_Min/Data/Prepd_Data/Fnl_Mres_mgkg.csv", header = TRUE)
datasets_for_trace_metals <- process_dataset(tracemetals_data, keep_LOQ_values = FALSE) 


# Data Processing for Organic Compounds dataset
organiccompounds_data <- read.csv("Squid_Conc_Anly/3-Data_Min/Data/Prepd_Data/Fnl_Ores_mgkg.csv", header = TRUE)
datasets_for_organic_compounds <- process_dataset(organiccompounds_data, keep_LOQ_values = FALSE)


# Extract the 'wide' datasets from each list
tm_data <- datasets_for_trace_metals$dataset_with_numerical_values
oc_data <- datasets_for_organic_compounds$dataset_with_numerical_values[,c(3:6,16:19)]

# Perform an inner join on the two datasets with common identifiers to check for polllutant interactions
combined_data <- left_join(
  tm_data,
  oc_data,
  by = c("Year", "ID_num", "Area", "Tissue")  # Optional: to distinguish same-named columns
)


explore_pollutant_interactions <- function(df, metal_cols, organic_cols) {
  
  # Changing concentration columns into numeric (flattening with unlist can cause issues)
  # Instead, convert each column individually:
  cols_to_convert <- c(16:29)
  df[, cols_to_convert] <- lapply(df[, cols_to_convert], as.numeric)
  
  # Desired tissue order
  tissue_order <- c("liver", "stomach", "muscle", "inksac")
  
  # Convert to factor to enforce order
  df$Tissue <- factor(df$Tissue, levels = tissue_order)
  
  tissues <- levels(df$Tissue)
  
  for (tissue in tissues) {
    
    output_folder <- file.path(
      "Squid_Conc_Anly/3-Data_Min/Outputs/polltnt_intrctns/"
    )
    dir_create(output_folder)
    
    df_tissue <- df %>% filter(Tissue == tissue)
    
    # Focus on numeric pollutant data
    df_focus <- df_tissue %>% select(all_of(c(metal_cols, organic_cols))) %>% select(where(is.numeric))
    
    min_nonzero_prop <- 0.10
    nonzero_prop <- colMeans(df_focus != 0, na.rm = TRUE)
    print(paste("Non-zero proportions for", tissue))
    print(nonzero_prop)
    
    # Keep columns with ≥ 10% non-zero values
    df_focus <- df_focus[, nonzero_prop >= min_nonzero_prop]
    
    # Remove constant columns (zero variance)
    df_focus <- df_focus[, sapply(df_focus, function(x) sd(x, na.rm = TRUE) > 0)]
    
    # Check if enough data remains
    if (nrow(df_focus) < 2 || ncol(df_focus) < 2) {
      cat("⚠️ Skipping tissue", tissue, "- not enough valid data.\n")
      next
    }
    
    # Correlation heatmap
    cor_matrix_focus <- cor(df_focus, use = "pairwise.complete.obs",method = "spearman")
    
    heatmap_path <- file.path(output_folder, paste0("htmp_", tissue, ".png"))
    pheatmap(cor_matrix_focus,
             cluster_rows = TRUE,
             cluster_cols = TRUE,
             display_numbers = TRUE,
             fontsize_number = 8,
             main = paste("Correlation in", tissue),
             filename = heatmap_path)
    cat("✅ Heatmap saved for", tissue, "→", heatmap_path, "\n")
    
    # Scatterplot matrix with error handling
    scatterplot_path <- file.path(output_folder, paste0("scttrplt_", tissue, ".pdf"))
    
    tryCatch({
      pdf(scatterplot_path, width = 10, height = 10)
      print(ggpairs(df_focus, title = paste("Scatterplot Matrix -", tissue)))
      dev.off()
      cat("✅ Scatterplot matrix saved for", tissue, "→", scatterplot_path, "\n")
    }, error = function(e) {
      cat("❌ Error creating scatterplot for", tissue, ":", conditionMessage(e), "\n")
      # Close PDF device if still open
      if (dev.cur() != 1) dev.off()
      # Optionally remove corrupt file
      if (file.exists(scatterplot_path)) file.remove(scatterplot_path)
    })
  }
}

metal_cols <- c("Metal_A", "Metal_B", "Metal_C","Metal_D", "Metal_E", "Metal_F","Metal_G", "Metal_H", "Metal_I", "Metal_J")          # Replace with your actual column names
organic_cols <- c("Organic_A","Organic_B","Organic_C","Organic_D")     # Replace with your actual column names

explore_pollutant_interactions(combined_data, metal_cols, organic_cols)
