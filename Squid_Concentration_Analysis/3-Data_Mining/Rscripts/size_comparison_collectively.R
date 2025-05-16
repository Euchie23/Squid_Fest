#LOADING LIBRARIES----
library(ggplot2)  # For plotting graphs
library(grid) # for creating, modifying, and arranging graphical objects ("grobs") like text, lines, rectangles, and complex layouts.
library(dplyr) # for data manipulation
library(tidyr) # For a collection of R packages used for data manipulation and visualization.
library(stringr) # For String Manipulation
library(tibble) # For creating tibbles



#Data Processor Function before doing analysis
#It is recommended that the 'Keep_LOQ_values' argument remain as FALSE to remove all values below blank control (BB) or LOQ (BLOQ) to get a more precise figure for the number of outliers within the dataset if using data for detection summary otherwise it will also classify values that are BB or BLOQ  as outliers leading to double counting.
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



# Helper function to calculate and store coefficients after performing spearmans correlation to assess the strength and direction of the monotonic relationship between sizes and concentrations and modifies concentrations for plotting thse coefficients on their respective graphs
get_coefficients <- function (dataset_subsetted_by_tissue){
  
  # Extracting unique factor levels for sizes, tissues, and years
  sizes <- levels(factor(dataset_subsetted_by_tissue$size)) # Size categories
  tissues <- levels(factor(dataset_subsetted_by_tissue$Tissue)) # Tissue categories
  years <- levels(factor(dataset_subsetted_by_tissue$Year)) # Year categories
  
 
  # Initialize variable to control loop iteration based on tissue type
  s<-1
  if(unique(dataset_subsetted_by_tissue$Tissue)=='inksac'){
    index <- 3 # If tissue is 'inksac', loop will run for 3 iterations
  }else{
    index <- 4 # Otherwise, loop runs for 4 iterations
  }
  # Create an empty data frame to store the results of the coefficients. final coefficient_dataframe_final where coefficient_resultsis accumulated
  coefficient_dataframe_final <- data.frame(matrix(ncol=7, nrow = 0))
  colnames(coefficient_dataframe_final) <- c('Tissue','Year','concentrations', 'Values','rho', 'pvalues', 'size')
  
  # Loop through the years and calculate coefficients
  while(s != index) {
    
    # Temporary data frame to store coefficients for the current iteration
    coefficient_results_1<- data.frame(matrix(ncol=7, nrow = 0))
    colnames(coefficient_results_1) <- colnames(coefficient_dataframe_final)
    
    # Create an empty data frame for storing results for the current year and size
    coefficient_dataframe_final <- data.frame(matrix(ncol=7, nrow = 0)) 
    colnames(coefficient_dataframe_final) <- colnames(coefficient_dataframe_final)
    
    # Filter data by the year (s-th year in the loop)
    tissues_per_year <- filter(as.data.frame(dataset_subsetted_by_tissue), Year == years[s])
    
    # If there is data for the selected year, process by sizes (small, medium, large)
    if(nrow(tissues_per_year)!=0){
      for (i in 1:length(sizes)){
        sizes_per_tissue_per_year <- filter(tissues_per_year, size == sizes[i]) # Filter by current size
        
        #Changing values to numeric to be manipulated and plotted
        Values_numeric <- suppressWarnings(as.numeric(as.character(sizes_per_tissue_per_year$Values)))
        
        # Handle xmin and xmax
        xmin <- ifelse(any(Values_numeric != 0, na.rm = TRUE), min(Values_numeric, na.rm = TRUE), 0)
        xmax <- ifelse(any(Values_numeric != 0, na.rm = TRUE), max(Values_numeric, na.rm = TRUE), 1)
        
        mid_x_axis <- (xmax + xmin)/2
        
        if(mid_x_axis >= 100){
          midpoint_x_axis <- floor(mid_x_axis/ 100) * 100
        }else if (mid_x_axis >= 10){
          midpoint_x_axis <- floor(mid_x_axis / 10) * 10
        }else{
          midpoint_x_axis <- mid_x_axis
        }
        
        # Handle ymax
        concentrations <- sizes_per_tissue_per_year$concentrations
        ycoord <- ifelse(any(!is.na(concentrations)), max(concentrations, na.rm = TRUE), 1)
        
        # If there are not enough data points or all concentrations are the same, set default values
        if(nrow(sizes_per_tissue_per_year)<2|all(sizes_per_tissue_per_year[-1,'concentrations'] == sizes_per_tissue_per_year[1,'concentrations']|max(sizes_per_tissue_per_year$concentrations)== 0)==TRUE){ 
    
          # Set default values if data is insufficient or all concentrations are the same
          coefficient_results_1[1,1] <- unique(tissues_per_year$Tissue)
          coefficient_results_1[1,2] <- years[s]
          coefficient_results_1[1,3] <- 0 # Set concentration to 0
          coefficient_results_1[1,4] <- 0 # Set variable to 0
          coefficient_results_1[1,5] <- 0 # Set rho to 0
          coefficient_results_1[1,6] <- 0 # Set p-value to 0
          coefficient_results_1[1,7] <- sizes[i]
          
          coefficient_dataframe_final <- rbind(coefficient_dataframe_final, coefficient_results_1) # Append the current coefficient to the results
      
        # If the maximum concentration in the size dataframe is not 0 then go to perform the correlation test for large sized squids.
        }else{

          # Perform correlation test for different size categories (small, medium, large)
          coefficient_results <- cor.test(as.numeric(as.factor(sizes_per_tissue_per_year$Values)), as.numeric(sizes_per_tissue_per_year$concentrations), method = "spearman", exact = FALSE)
          
          # Store the results for correlation coefficient and p-value
          coefficient_results_1[1, 1] <- unique(sizes_per_tissue_per_year$Tissue)
          coefficient_results_1[1, 2] <- years[s]
          #Y coordinate for rho coefficient and p-value
          if (str_detect(sizes[i], 'large')== TRUE){
            coefficient_results_1[1, 3] <- signif((ycoord/ 0.82), 3) 
          }else if (str_detect(sizes[i], 'medium')== TRUE){
            coefficient_results_1[1, 3] <- signif((ycoord/ 0.88), 3)
          }else{
            coefficient_results_1[1, 3] <- signif((ycoord/ 0.94), 3) 
          }
          #X coordinate for rho coefficient and p-value
         if (unique(sizes_per_tissue_per_year$vars) == 'Month_of_Capture'){
            coefficient_results_1[1, 4] <- midpoint_x_axis * 0.4
          }else{
            coefficient_results_1[1, 4] <- midpoint_x_axis 
          }
          
          # Store the rho and p-values from the correlation test
          coefficient_results_1[1, 5] <- coefficient_results$estimate
          coefficient_results_1[1, 6] <- coefficient_results$p.value
          coefficient_results_1[1, 7] <- unique(sizes_per_tissue_per_year$size)
          # Append the calculated coefficients to the results
          coefficient_dataframe_final <- rbind(coefficient_dataframe_final, coefficient_results_1)
        }
      }
    }       
          
    # Increment the year index
    s<-s+1
    
 
    # Append the results from the current year to the final dataframe
    coefficient_dataframe_final <- rbind(coefficient_dataframe_final, coefficient_dataframe_final)
    coefficient_dataframe_final$Tissue <- as.character(coefficient_dataframe_final$Tissue)  # Convert Tissue column to character type
    coefficient_dataframe_final$Year <- as.character(coefficient_dataframe_final$Year) # Convert Year column to character type
   
  }
  # Return the final dataframe with the coefficients
  return (coefficient_dataframe_final)
}




# Helper function to modify the coefficients by cleaning and formatting rho and p-values
coefficient_results_modification <- function(coefficients_accumulated) {

  # Round 'rho' values to 4 decimal places
  coefficients_accumulated$rho <- signif(coefficients_accumulated$rho, 4)
  
  # Round 'pvalues' to the default number of significant digits
  coefficients_accumulated$pvalues <- signif(coefficients_accumulated$pvalues)
  
  # Replace rho and pvalues with NA for non-significant results (p-values > 0.05 or p-value == 0)
  coefficients_accumulated$rho[coefficients_accumulated$pvalues > 0.05 | coefficients_accumulated$pvalues == 0] <- NA
  coefficients_accumulated$pvalues[coefficients_accumulated$pvalues > 0.05 | coefficients_accumulated$pvalues == 0] <- NA
  
  # Add labels "r=" and "p-val=" to 'rho' and 'pvalues'
  coefficients_accumulated$rho <- paste0("r=", coefficients_accumulated$rho)
  coefficients_accumulated$pvalues <- paste0("p-val=", coefficients_accumulated$pvalues)
  
  # Remove duplicated rho and pvalues and set them to NA
  coefficients_accumulated$pvalues[duplicated(coefficients_accumulated$pvalues)] <- NA
  coefficients_accumulated$rho[duplicated(coefficients_accumulated$rho)] <- NA
  
  # Clean up rows where 'rho' or 'pvalues' are NA due to "NA" string
  coefficients_accumulated$rho <- gsub(".*NA.*", NA, coefficients_accumulated$rho)
  coefficients_accumulated$pvalues <- gsub(".*NA.*", NA, coefficients_accumulated$pvalues)
  
  # Replace NA or NaN with 0 for 'rho' and 'pvalues'
  coefficients_accumulated$rho[is.na(coefficients_accumulated$rho)] <- 0
  coefficients_accumulated$pvalues[is.na(coefficients_accumulated$pvalues)] <- 0
  coefficients_accumulated$pvalues[is.nan(coefficients_accumulated$pvalues)] <- 0
  
  # Subset the data to include only rows where pvalues are not zero (non-significant results excluded)
  modified_coefficients <- subset(coefficients_accumulated, pvalues != 0)
  
  # Return the modified coefficients data frame
  return(modified_coefficients)
}


# Main function: data_mining_using_mantle_length_for_concentrations
# This function performs a series of data processing steps, including statistical analysis, and plotting, to investigate the relationship between various environmental concentrations and biological variables (such as tissue types and year). The analysis uses mantle length as a categorical variable to group data into small, medium, and large categories, which are then used to generate plots of pollutant concentrations versus different variables (e.g.,distance to land or month of capture). The function includes options for handling, zero values, and performing statistical modeling (e.g., linear regression) to assess the strength of these relationships. Plots are generated using `ggplot2`, and results are returned in a list.
comparing_sizes_collectively <- function (data_list, remove.zeroes = FALSE){
  
  dataset_with_numerical_values <- data_list$dataset_with_numerical_values
  
  # Step 1: Define the Mantle lengths and categorize them into small, medium, large
  Mantle_lengths <- sort(unique(dataset_with_numerical_values$Mantle_length_mm))
  small <- Mantle_lengths[1:(1/3 * length(Mantle_lengths))]
  medium <- Mantle_lengths[(1/3 * length(Mantle_lengths) + 1):(2/3 * length(Mantle_lengths))]
  large <- Mantle_lengths[(2/3 * length(Mantle_lengths) + 1):length(Mantle_lengths)]
  
  # Step 2: Initialize empty lists for storing results
  list3 <- list()
  list3names <- c()
  
  # Step 3: Determine pollutant range and subset the dataset based on its presence
  if (grepl("Metal", colnames(dataset_with_numerical_values)[16])) {
    # Trace metals subset
    range <- colnames(dataset_with_numerical_values[16:25])
    cols_to_sort <- c("Fe", "Co", "Ni","Cu", "Zn", "Ag","Cd", "Hg", "Tl", "Pb")
    # Sort them alphabetically
    sorted_cols <- sort(cols_to_sort)
    # Reorder the dataset with sorted subset
    dataset_with_numerical_values <- dataset_with_numerical_values %>%
      select(any_of(setdiff(names(.), cols_to_sort)), all_of(sorted_cols))
    range_name <- 'Metal_A:Metal_J'
    number_range <- 17:26
  } else {
    # Organic compounds subset
    range <- colnames(dataset_with_numerical_values[16:19])
    range_name <- 'Organic_A:Organic_D'
    number_range <- 17:20
  }
  
  # Step 4: Add 'size' column based on Mantle length categories
  dataset_with_numerical_values <- dataset_with_numerical_values %>%
    arrange(Mantle_length_mm) %>%
    mutate(size = case_when(
      Mantle_length_mm %in% small ~ paste('small', '(', min(small), 'mm-', max(small), 'mm', ')'),
      Mantle_length_mm %in% medium ~ paste('medium', '(', min(medium), 'mm-', max(medium), 'mm', ')'),
      Mantle_length_mm %in% large ~ paste('large', '(', min(large), 'mm-', max(large), 'mm', ')'),
      TRUE ~ NA_character_
    )) %>%
    relocate(size, .after = dtfl_km)
  
  # Step 5: Prepare and subsetting dataset for further analysis
  final_coefficients_accumulated <- data.frame(matrix(ncol = 8, nrow = 0))# making empty datasets to store results
  
  subsetted_dataset_with_variables <- dataset_with_numerical_values[, c(8, 9, 13)]#Subsetting 
  
  subsetted_dataset_with_numerical_values <- dataset_with_numerical_values[,c(number_range, 3, 6, 7:13)]# subsetting needed columns for futher analysis 
  
  
  # Step 6: Loop through variables and calculate coefficients
  for (h in 1:3) {
    variable <- colnames(subsetted_dataset_with_variables)[h]
    print(variable)
    
    coefficients_accumulated <- data.frame(matrix(ncol = 8, nrow = 0))
    pvalues_accumulated <- data.frame(matrix(ncol = 8, nrow = 0))
    
    subsetted_dataset_with_numerical_values2 <- subsetted_dataset_with_numerical_values #saving subsetted dataset to a new dataset
    tissues <- levels(factor(subsetted_dataset_with_numerical_values2$Tissue))#creating a vector of tissues for later data processing
    sizes <- levels(factor(subsetted_dataset_with_numerical_values2$size))#creating a vector of sizes for later data processing
    
    # Step 7: Long format transformation of the dataset
    if (remove.zeroes == FALSE) {
      long_dataset <- subsetted_dataset_with_numerical_values2 %>%
        pivot_longer(all_of(range), names_to = "pollutants", values_to = "concentrations") %>%
        pivot_longer(!!rlang::sym(paste0(variable)), names_to = "vars", values_to = "Values") %>%
        mutate(concentrations = as.numeric(concentrations),Values = as.numeric(Values))
    } else {
      long_dataset <- subsetted_dataset_with_numerical_values2 %>%
        pivot_longer(all_of(range), names_to = "pollutants", values_to = "concentrations") %>%
        pivot_longer(!!rlang::sym(paste0(variable)), names_to = "vars", values_to = "Values") %>%
        filter(concentrations != 0) %>%
        mutate(
          concentrations = as.numeric(concentrations),
          Values = as.numeric(Values)
        )
    }
    
      # Step 8: Loop through tissues and calculate coefficients
      for (i in 1:length(tissues)) {
        long_dataset_subsetted_by_tissue <- long_dataset %>%
          group_by(Year) %>%
          filter(Tissue == tissues[i])
        if (nrow(long_dataset_subsetted_by_tissue) > 0) {
          coefficients <- get_coefficients(long_dataset_subsetted_by_tissue)
          coefficients_accumulated <- rbind(coefficients_accumulated, coefficients)
        }
      }
      
      # Step 9: Calculate p-values
      pvalues <- coefficient_results_modification(coefficients_accumulated)
      pvalues_accumulated <- rbind(pvalues_accumulated, pvalues)
      cat("\n----------\n")
      print(pvalues)
      
      # Step 10: Calculating X and Y axis scales
      if (variable != "Month_of_Capture") { 
      x_limits <- long_dataset %>%
        mutate(Values_numeric = suppressWarnings(as.numeric(as.character(Values)))) %>%
        group_by(Year) %>%
        summarise(
          x_axis_lower_limit = min(Values_numeric, na.rm = TRUE),
          x_axis_upper_limit = max(Values_numeric, na.rm = TRUE),
          .groups = "drop"
        )
      
      original_year_list <- c("2019", "2020", "2021")
      

      # Retaining only years that still exist in the dataset
      year_list <- intersect(original_year_list, unique(long_dataset$Year))
      
      # Building x scale dataframe for valid pollutants only
      df_scales_x <- data.frame(
        Year = year_list,
        xmin = NA_real_,
        xmax = NA_real_
      ) %>%
        left_join(x_limits, by = "Year") %>%
        mutate(
          xmin = coalesce(x_axis_lower_limit, NA_real_),
          xmax = coalesce(x_axis_upper_limit, NA_real_),
          range_size = xmax - xmin
        ) %>%
        filter(!is.na(xmin) & !is.na(xmax)) %>%
        select(Year, xmin, xmax, range_size)
      
      # Spliting and generating axis scales
      df_scales_x <- split(df_scales_x, df_scales_x$Year)
      
      scales_x <- lapply(df_scales_x, function(x) {
        
        # Checking if the column 'Values' in the full dataset is numeric
        if (is.numeric(long_dataset$Values)) {
          if (x$range_size < 5) {
            scale_x_continuous(
              limits = c(x$xmin, x$xmax),
              breaks = seq(x$xmin, x$xmax, length.out = 5)
            )
          } else {
            scale_x_continuous(limits = c(x$xmin, x$xmax))
          }
        } else {
          scale_x_discrete()
        }
      })
    } else {
      scales_x <- NULL
    }
    


    # Y_Axis
    # Generating y-axis limits by pollutant
    y_limits <- long_dataset %>%
      group_by(Tissue) %>%
      summarise(
        y_axis_lower_limit = min(concentrations, na.rm = TRUE),
        y_axis_upper_limit = if (any(unique(Tissue) %in% pvalues$Tissue)) {
          max(concentrations, na.rm = TRUE) / 0.80
        } else {
          max(concentrations, na.rm = TRUE)
        },
        .groups = "drop"
      )
    
    # Defining full possible list
    original_tissue_list <- c("liver", "stomach", "muscle", "inksac")
    original_breaks <- c(5, 5, 5, 5)
    
    # Retaining only tissues that still exist in the dataset
    tissue_list <- intersect(original_tissue_list, unique(long_dataset$Tissue))
    n <- original_breaks[match(tissue_list, original_tissue_list)]  # align y axis breaks
    
    # Building df_scales_y safely
    df_scales_y <- data.frame(
      Tissue = tissue_list,
      ymin = NA_real_,
      ymax = NA_real_,
      n = n
    ) %>%
      left_join(y_limits, by = "Tissue") %>%
      mutate(
        ymin = coalesce(y_axis_lower_limit, NA_real_),
        ymax = coalesce(y_axis_upper_limit, NA_real_)
      ) %>%
      filter(!is.na(ymin) & !is.na(ymax)) %>%  # Keep only those with limits
      select(Tissue, ymin, ymax, n)
    
    # Building scales
    df_scales_y <- split(df_scales_y, df_scales_y$Tissue)
    
    scales_y <- lapply(df_scales_y, function(x) {
      scale_y_continuous(limits = c(x$ymin, x$ymax), n.breaks = x$n)
    })
    list3<-append(list(pvalues_accumulated),list3, 0)
    name3 <- paste(variable,'_',tissues[i],"_coeffcient_results",sep = "")
    list3names <- append(list3names,name3)

      
      # Step 11: Plotting
      if (unique(long_dataset$vars) == "Month_of_Capture") {
        long_dataset$Values <- cut(long_dataset$Values, breaks = 4, labels = c(3, 4, 5, 6))
      }
      
      reordered_tissues <- c("liver", "stomach", "muscle", "inksac")
      Colors <- setNames(c('#F8766D', '#7CAE00', '#00A9FF'), sizes)
      
      # Generate plot based on variable
      if (variable == 'dta_km') {
        dta_km <- ggplot(long_dataset, aes(Values, concentrations, colour = size, group = size)) +
          scale_colour_manual(values = Colors) +
          geom_smooth(method = "lm", se = FALSE) +
          labs(
            title = paste('<B>', range_name, '::', '</B>', variable, "Vs Conc mg/kg using size", sep = ""),
            y = "Concentration mg/kg", 
            x = paste(variable)
          ) +
          theme(
            plot.title = ggtext::element_markdown(),
            strip.text = element_text(size = 12, face = "bold"),
            legend.title = element_text(face = "bold", size = 14)
          ) +
          facet_grid(factor(Tissue, levels = reordered_tissues) ~ Year, scales = "free", drop = FALSE) +
          ggh4x::facetted_pos_scales(
            x = scales_x,
            y = scales_y
          )+ 
          geom_point(aes(shape = size, color = size), size = 2) +
          {if (nrow(pvalues_accumulated) != 0) geom_text(pvalues_accumulated, mapping = aes(label = paste(rho, pvalues, sep = ",")), hjust = 0, size = 3.5, fontface = "italic", position = position_dodge(width = .1), check_overlap = FALSE)}
      } else if (variable == 'dtfl_km') {
        dtfl_km <- ggplot(long_dataset, aes(Values, concentrations, colour = size, group = size)) +
          scale_colour_manual(values = Colors) +
          geom_smooth(method = "lm", se = FALSE) +
          labs(
            title = paste('<B>', range_name, '::', '</B>', variable, "Vs Conc mg/kg using size", sep = ""),
            y = "Concentration mg/kg", 
            x = paste(variable)
          ) +
          theme(
            plot.title = ggtext::element_markdown(),
            strip.text = element_text(size = 12, face = "bold"),
            legend.title = element_text(face = "bold", size = 14)
          ) +
          facet_grid(factor(Tissue, levels = reordered_tissues) ~ Year, scales = "free", drop = FALSE) +
          ggh4x::facetted_pos_scales(
            x = scales_x,
            y = scales_y
          )+ 
          geom_point(aes(shape = size, color = size), size = 2) +
          {if (nrow(pvalues_accumulated) != 0) geom_text(pvalues_accumulated, mapping = aes(label = paste(rho, pvalues, sep = ",")), hjust = 0, size = 3.5, fontface = "italic", position = position_dodge(width = .1), check_overlap = FALSE)}
      } else {
        Month_of_Capture <- ggplot(long_dataset, aes(Values, concentrations, colour = size, group = size)) +
          scale_colour_manual(values = Colors) +
          geom_smooth(method = "lm", se = FALSE) +
          labs(
            title = paste('<B>', range_name, '::', '</B>', variable, "Vs Conc mg/kg using size", sep = ""),
            y = "Concentration mg/kg", 
            x = paste(variable)
          ) +
          theme(
            plot.title = ggtext::element_markdown(),
            strip.text = element_text(size = 12, face = "bold"),
            legend.title = element_text(face = "bold", size = 14)
          ) +
          facet_grid(factor(Tissue, levels = reordered_tissues) ~ Year, scales = "free", drop = FALSE) +
          ggh4x::facetted_pos_scales(
            x = scales_x,
            y = scales_y
          )+ 
          geom_point(aes(shape = size, color = size), size = 2) +
          {if (nrow(pvalues_accumulated) != 0) geom_text(pvalues_accumulated, mapping = aes(label = paste(rho, pvalues, sep = ",")), hjust = 0, size = 3.5, fontface = "italic", position = position_dodge(width = .1), check_overlap = FALSE)}
      }
    }
  names(list3)<-list3names
  
  final_coefficients_accumulated <- rbind(final_coefficients_accumulated, coefficients_accumulated)
  
  return(list(dta_km = dta_km, dtfl_km = dtfl_km, Month_of_Capture = Month_of_Capture, final_coefficients_accumulated = list3))
}


#Calling Main Function. All arguments except remove.zeroes (default is set at False) are required and user has to choose between datasets_for_organic_compounds or datasets_for_trace_metals. This function shows how pollution levels collectively change in different tissues of the squids depending on the aforementioned three variables. Users can also remove all zeroes and focus on only the detected concentrations or keep them. The results are saved in size_comparison_results list.
collective_size_comparison_results <- comparing_sizes_collectively (datasets_for_trace_metals, remove.zeroes = FALSE)

#Below code saves multiple plots into individual PNG files. It loops through the list of plots and and for each plot it extracts its name, and the plot then saves it in one .png file using grid.draw()
save_graphs <- function(graph_list) {
  

  # Extract pollutant types from the first available plot
  plot_data <- graph_list[["dta_km"]][[1]]
  pollutant_types <- unique(plot_data$pollutants)


  # Define the folder where you want to save the PNG files 
  if (any(grepl("Metal", pollutant_types))) {
    output_folder <- file.path("/Users/mrnobody/Documents/GitHub/Squid_Fest/Squid_Concentration_Analysis/3-Data_Mining/Data_mining_plots/size_comparison_collectively/trace_metals")
  }else{
    output_folder <- file.path("/Users/mrnobody/Documents/GitHub/Squid_Fest/Squid_Concentration_Analysis/3-Data_Mining/Data_mining_plots/size_comparison_collectively/Organic_compounds")  
  }
  
  # Create the folder if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  
  # Loop through actual ggplot objects in graph_list
  for (plot_name in names(graph_list)) {
    plot_object <- graph_list[[plot_name]]
    
    # In case it's wrapped in a list like graph_list$X$graph
    if (is.list(plot_object) && inherits(plot_object[[1]], "gg")) {
      plot_object <- plot_object[[1]]
    }
    
    if (!inherits(plot_object, "gg")) {
      cat("⚠️ Skipping", plot_name, "- not a ggplot object.\n")
      next
    }
    
    # Use tryCatch to handle all errors
    tryCatch({
      # Create output file path
      output_path <- file.path(output_folder, paste0(plot_name, ".png"))
      
      # Save PNG
      png(output_path, width = 1400, height = 800)
      
      #Converts the ggplot object into a "grob" (graphical object) then draws it on a PNG to use all available space.
      grid.draw(ggplotGrob(plot_object))
      
      dev.off()
      cat("Saved:", output_path, "\n")
      
    }, error = function(e) {
      cat("⚠️ Error in", plot_name, ":", e$message, "\n")
    })
  }
}

#Calling save_graphs function:
save_graphs(collective_size_comparison_results)
