#LOADING LIBRARIES----
library(ggplot2)  # For plotting graphs
library(grid) # for creating, modifying, and arranging graphical objects ("grobs") like text, lines, rectangles, and complex layouts.
library(dplyr) # for data manipulation
library(tidyr) # For a collection of R packages used for data manipulation and visualization.
library(stringr) # For String Manipulation
library(tibble) # For creating tibbles



#Data Processor Function before doing analysis
#It is recommended that the 'Keep_LOQ_values' argument remain as FALSE to remove all values below blank control (BB) or LOQ (BLOQ) to get a more precise figure for the number of outliers within the dataset if using data for detection summary, otherwise it will also classify values that are BB or BLOQ as outliers leading to double counting
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
      
      
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = ".*BLOQ.*", replacement = 'BB+BLOQ')
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = ".*BB.*", replacement = 'BB+BLOQ')
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
      print(data1)
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


#generating the markdown for reading images:
#OCicons for Organic Compounds
OCicons <- data.frame(pollutants=c("Organic_A","Organic_B","Organic_C","Organic_D"), icons=c("https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=15168&format=png","https://img.icons8.com/?size=38&id=15168&format=png"))
urls <-OCicons$icons
names(urls) <- OCicons$pollutants

#TMicons for Trace Metals:
TMicons <- data.frame(pollutants=c("Metal_F","Metal_G","Metal_B","Metal_D","Metal_A","Metal_H","Metal_C","Metal_J","Metal_I","Metal_E"), icons=c("https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png"))
urlz <-TMicons$icons
names(urlz) <- TMicons$pollutants


#Helper function for creating a clean, minimalistic theme for the plot using theme_minimal(), and then customizes the style (like text sizes, fonts, colors, backgrounds, etc.).

theme_icons <- function(base_size = 10,
                        title_size = 20,
                        ...){
  # CUSTOM THEME:
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      
      # axis
      axis.text = element_text( size = 10 ),
      axis.text.x = element_text( size = 10),
      axis.title = element_text( size = 16, face = "bold" ),
      
      # title
      plot.title = element_text(size = title_size),
      plot.title.position = "plot",
      
      #panel
      panel.background = element_rect(fill="white"),
      panel.ontop = FALSE,
      #legend
      legend.text = element_text(size = 15),
      legend.title = element_text(face = "bold", size = 16),
      
      #strip
      strip.text = element_text(size = 10),
      strip.background =element_rect(fill="lightgray"),
      strip.text.x = element_markdown(size = 20),
      ... 
    )
}


#Helper function to activate the icons for organic compound pollutants in final graph,  It first checks if the icons exist and if they do then they are posted in the graph: 
markdown_function_for_OC_icons <- function(x) {
  # Use file.path() for a safe file path
  icon_path <- file.path("Squid_Concentration_Analysis/3-Data_Mining/OCicons", paste0(x[1], ".png"))
  
  # Construct the HTML string
  y <- paste0(" <img src='", icon_path, "' width='17'/>")
  
  return(y)
}



#Helper function to activate the icons for trace metal pollutants in final graph, It first checks if the icons exist and if they do then they are loaded unto the graph: 
markdown_function_for_TM_icons <- function(x) {
  # Create file paths correctly
  file1 <- file.path("Squid_Concentration_Analysis/3-Data_Mining/TMicons", paste0(x[1], ".png"))
  file2 <- file.path("Squid_Concentration_Analysis/3-Data_Mining/TMicons", paste0(x[1], "1.png"))
  file3 <- file.path("Squid_Concentration_Analysis/3-Data_Mining/TMicons", paste0(x[1], "2.png"))
  
  # Check which files exist and build the HTML string accordingly
  if (file.exists(file1) & file.exists(file2) & file.exists(file3)) {
    y <- paste0(" ", "<img src='", file1, "' width='17'/> ",
                "<img src='", file2, "' width='17'/> ",
                "<img src='", file3, "' width='17'/>")
  } else if (file.exists(file1) & file.exists(file2)) {
    y <- paste0(" ", "<img src='", file1, "' width='17'/> ",
                "<img src='", file2, "' width='17'/>")
  } else {
    y <- paste0(" ", "<img src='", file1, "' width='17'/>")
  }
  
  return(y)
} 



# Helper function to calculate and store coefficients after performing spearmans correlation to assess the strength and direction of the monotonic relationship between sizes and concentrations and modifies concentrations for plotting thse coefficients on their respective graphs.
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
  # Create an empty data frame to store the results of the coefficients. final coefficient_results_final where coefficient_results_1 is accumulated
  coefficient_results_final <- data.frame(matrix(ncol=8, nrow = 0))
  colnames(coefficient_results_final) <- c('Tissue', 'Year','pollutant','concentrations', 'Values','rho', 'pvalues', 'size')
  
  # Loop through the years and calculate coefficients
  while(s != index) {
    
    # Temporary data frame to store coefficients for the current iteration
    coefficient_results_1 <- data.frame(matrix(ncol=8, nrow = 0))
    colnames(coefficient_results_1) <- colnames(coefficient_results_final)
    
    # Create an empty data frame for storing results for the current year and size
    coefficient_dataframe_final <- data.frame(matrix(ncol=8, nrow = 0)) 
    colnames(coefficient_dataframe_final) <- colnames(coefficient_results_final)
    
    # Filter data by the year (s-th year in the loop)
    tissues_by_year <- filter(as.data.frame(dataset_subsetted_by_tissue), Year == years[s])
    
    # If there is data for the selected year, process by sizes (small, medium, large)
    if(nrow(tissues_by_year)!=0){
      for (i in 1:length(sizes)){
        sizes_per_tissue_per_year <- filter(tissues_by_year, size == sizes[i]) # Filter by current size
        
        #Changing values to numeric to be manipulated and plotted
        Values_numeric <- suppressWarnings(as.numeric(as.character(tissues_by_year$Values)))
        
        # Handle xmin and xmax
        xmin <- ifelse(any(Values_numeric != 0, na.rm = TRUE), min(Values_numeric, na.rm = TRUE), 0)
        xmax <- ifelse(any(Values_numeric != 0, na.rm = TRUE), max(Values_numeric, na.rm = TRUE), 1)
        
        mid_x_axis <- (xmax + xmin)/2
        
      if(unique(tissues_by_year$vars) == 'Month_of_Capture'){
        if(mid_x_axis >= 100){
          midpoint_x_axis <- floor(mid_x_axis/ 100) * 100
        }else if (mid_x_axis >= 10){
          midpoint_x_axis <- floor(mid_x_axis / 10) * 10
        }else{
          midpoint_x_axis <- mid_x_axis
        }
      }else{
        midpoint_x_axis <- mid_x_axis
      }
        # Handle ymax
        concentrations <- sizes_per_tissue_per_year$concentrations
        ycoord <- ifelse(any(!is.na(concentrations)), max(concentrations, na.rm = TRUE), 1)

        
        # If there are not enough data points or all concentrations are the same, set default values
        if(nrow(sizes_per_tissue_per_year)<2|all(sizes_per_tissue_per_year[-1,'concentrations'] == sizes_per_tissue_per_year[1,'concentrations']|max(sizes_per_tissue_per_year$concentrations)== 0)==TRUE){ 
          
          # Set default values if data is insufficient or all concentrations are the same
          coefficient_results_1[1,1] <- unique(tissues_by_year$Tissue)
          coefficient_results_1[1,2] <- years[s]
          coefficient_results_1[1,3] <- unique(tissues_by_year$pollutants)
          coefficient_results_1[1,4] <- 0 # Set concentrationsto 0
          coefficient_results_1[1,5] <- 0 # Set values to 0
          coefficient_results_1[1,6] <- 0 # Set rho to 0
          coefficient_results_1[1,7] <- 0 # Set p-values to 0
          coefficient_results_1[1,8] <- sizes[i]
          
          coefficient_dataframe_final <- rbind(coefficient_dataframe_final, coefficient_results_1) # Append the current coefficient to the results
          
          # If the maximum concentration in the size_per_tissue_per_year dataframe is not 0 then go to perform the correlation test for squids based on their size.
        }else{
          
          # Perform correlation test for different size categories (small, medium, large)
          coefficient_results <- cor.test(as.numeric(as.factor(sizes_per_tissue_per_year$Values)), as.numeric(sizes_per_tissue_per_year$concentrations), method = "spearman", exact = FALSE)
          
          # Store the results for correlation coefficient and p-value
          coefficient_results_1[1, 1] <- unique(sizes_per_tissue_per_year$Tissue)
          coefficient_results_1[1, 2] <- years[s]
          coefficient_results_1[1, 3] <- unique(sizes_per_tissue_per_year$pollutants)
          #Y coordinate for rho coefficient and p-value
          if (str_detect(sizes[i], 'large')== TRUE){
            coefficient_results_1[1, 4] <- signif((ycoord/ 0.82), 3) 
          }else if (str_detect(sizes[i], 'medium')== TRUE){
            coefficient_results_1[1, 4] <- signif((ycoord/ 0.88), 3)
          }else{
            coefficient_results_1[1, 4] <- signif((ycoord/ 0.94), 3) 
          }
          #X coordinate for rho coefficient and p-value
          if (unique(sizes_per_tissue_per_year$vars) == 'Month_of_Capture'){
            coefficient_results_1[1, 5] <- midpoint_x_axis* 0.2 
          }else{
            coefficient_results_1[1, 5] <- midpoint_x_axis*0.95 
          }
          # Store the rho and p-values from the correlation test
          coefficient_results_1[1, 6] <- coefficient_results$estimate
          coefficient_results_1[1, 7] <- round(coefficient_results$p.value, 4)
          coefficient_results_1[1, 8] <- unique(sizes_per_tissue_per_year$size)
          # Append the calculated coefficients to the results
          coefficient_dataframe_final <- rbind(coefficient_dataframe_final, coefficient_results_1)
        }
      }
    }       
    
    # Increment the year index
    s<-s+1
    
    
    # Append the results from the current year to the final dataframe
    coefficient_results_final <- rbind(coefficient_results_final, coefficient_dataframe_final)
    coefficient_results_final$Tissue <- as.character(coefficient_results_final$Tissue)  # Convert Tissue column to character type
    coefficient_results_final$Year <- as.character(coefficient_results_final$Year) # Convert Year column to character type
    
  }
  # Return the final dataframe with the coefficients
  return (coefficient_results_final)
}



# Helper function to modify the coefficients by cleaning and formatting rho and p-values.
coefficient_results_modification <- function(coefficients_accumulated) {
  
  # Round 'rho' values to 4 decimal places
  coefficients_accumulated$rho <- signif(coefficients_accumulated$rho, 4)
  
  # Round 'pvalues' to the default number of significant digits
  coefficients_accumulated$pvalues <- signif(coefficients_accumulated$pvalues)
  
  # Replace rho and pvalues with NA for non-significant results (p-values > 0.05 or p-value == 0)
  coefficients_accumulated$rho[coefficients_accumulated$pvalues > 0.05 | coefficients_accumulated$pvalues == 0| is.nan(coefficients_accumulated$pvalues)] <- NA
  coefficients_accumulated$pvalues[coefficients_accumulated$pvalues > 0.05 | coefficients_accumulated$pvalues == 0|is.nan(coefficients_accumulated$pvalues)] <- NA
  
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
  coefficients_accumulated$rho[is.nan(coefficients_accumulated$rho)] <- 0
  coefficients_accumulated$pvalues[is.na(coefficients_accumulated$pvalues)] <- 0
  coefficients_accumulated$pvalues[is.nan(coefficients_accumulated$pvalues)] <- 0
  
  # Subset the data to include only rows where pvalues are not zero (non-significant results excluded)
  modified_coefficients <- subset(coefficients_accumulated, pvalues != 0)
  
  # Return the modified coefficients data frame
  return(modified_coefficients)
}



# Main function: comparing_sizes_per_variable
# This function performs a series of data processing steps, including statistical analysis, and plotting, to investigate the relationship between various environmental concentrations and biological variables (such as tissue types and year). The analysis uses mantle length as a categorical variable to group data into small, medium, and large categories, which are then used to visualize how pollution levels change in different tissues of the squid depending on things like how far from land they were found or what month they were caught — and also comparing if larger, medium, or smaller squids have different concentration patterns, using regression lines to show how strong those patterns are. The function includes options for handling zero values, and performing statistical modeling (e.g., linear regression) to assess the strength of these relationships. Plots are generated using `ggplot2`, and results are returned in a list.
comparing_sizes_per_pollutant <- function (data_list, variable, remove.zeroes = FALSE){
  
  dataset_with_numerical_values <- data_list$dataset_with_numerical_values
  
  # Step 1: Define the Mantle lengths and categorize them into small, medium, large
  Mantle_lengths <- sort(unique(dataset_with_numerical_values$Mantle_length_mm))
  small <- Mantle_lengths[1:(1/3 * length(Mantle_lengths))]
  medium <- Mantle_lengths[(1/3 * length(Mantle_lengths) + 1):(2/3 * length(Mantle_lengths))]
  large <- Mantle_lengths[(2/3 * length(Mantle_lengths) + 1):length(Mantle_lengths)]
  
  # Step 2: Initialize empty lists for storing results
  list0 <- list()
  list3 <- list()
  list0names <- c()
  list3names <- c()
  
 
  # Step 3: Determine pollutant range and subset the dataset based on its presence
  if (grepl("Metal", colnames(dataset_with_numerical_values)[16])) {
    # Trace metals subset
    range <- colnames(dataset_with_numerical_values[16:25])
    icons_tm <- apply(TMicons, 1, markdown_function_for_TM_icons)
    icons_markdown <- icons_tm
    
    cols_to_sort <- c("Metal_A", "Metal_B", "Metal_C","Metal_D", "Metal_E", "Metal_F","Metal_G", "Metal_H", "Metal_I", "Metal_J")
    # Sort them alphabetically
    sorted_cols <- sort(cols_to_sort)
    # Reorder the dataset with sorted subset
    dataset_with_numerical_values <- dataset_with_numerical_values %>%
      select(any_of(setdiff(names(.), cols_to_sort)), all_of(sorted_cols))
    range_name <- 'Metal_A:Metal_J'
    number_range <- 17:26
  } else {
    # Organic compounds subset
    icons_oc <- apply(OCicons, 1, markdown_function_for_OC_icons)
    icons_markdown <- icons_oc
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
  
  subsetted_dataset_with_numerical_values <- dataset_with_numerical_values[,c(3, 6, 7:13, number_range)]# subsetting needed columns for futher analysis 
  
  
  # Step 6: Loop through variables and calculate coefficients
  for (h in 1:length(number_range)) {
   pollutant <- colnames(dataset_with_numerical_values[,c(number_range)])[h]
    coefficients_accumulated <- data.frame(matrix(ncol = 8, nrow = 0))
    pvalues_accumulated <- data.frame(matrix(ncol = 8, nrow = 0))
    
    # subsetted_dataset_with_numerical_values2 <- subsetted_dataset_with_numerical_values #saving subsetted dataset to a new dataset
    tissues <- levels(factor(subsetted_dataset_with_numerical_values$Tissue))#creating a vector of tissues for later data processing
    sizes <- levels(factor(subsetted_dataset_with_numerical_values$size))#creating a vector of sizes for later data processing
    
    if (!(variable %in% c("Month_of_Capture", "dta_km", "dtfl_km"))){
      stop("Please use one of the below variables, copy and paste if need be:\n1)Month_of_Capture\n2)dta_km\n3)dtfl_km")# This piece of code accounts for if any other variable (x variable) is chosen other than these three or in the even that they were typed wrong. It send out a messsage to the user.
    }

    # Step 7: Long format transformation of the dataset
    if (remove.zeroes == FALSE) {
      long_dataset <- subsetted_dataset_with_numerical_values %>%
        pivot_longer(paste(pollutant), names_to = "pollutants", values_to = "concentrations") %>%
        pivot_longer(!!rlang::sym(paste(variable)), names_to = "vars", values_to = "Values") %>%
        mutate(concentrations = as.numeric(concentrations),Values = as.numeric(Values))
    } else {
      long_dataset <- subsetted_dataset_with_numerical_values %>%
        pivot_longer(paste(pollutant), names_to = "pollutants", values_to = "concentrations") %>%
        pivot_longer(!!rlang::sym(paste0(variable)), names_to = "vars", values_to = "Values") %>%
        filter(concentrations != 0) %>%
        mutate(
          concentrations = as.numeric(concentrations),
          Values = as.numeric(Values)
        )
    }
    if (nrow(long_dataset) != 0) {
      # Step 8: Loop through tissues and calculate and modify coefficients
      for (i in 1:length(tissues)) {
        long_dataset_subsetted_by_tissue <- long_dataset %>%
          group_by(Year) %>%
          filter(Tissue == tissues[i])
        if (nrow(long_dataset_subsetted_by_tissue) > 0) {
          coefficients <- get_coefficients(long_dataset_subsetted_by_tissue)
          coefficients_accumulated <- rbind(coefficients_accumulated, coefficients)
        }
      }
      #modification of pvalue dataset
      pvalues <- coefficient_results_modification(coefficients_accumulated)
      pvalues_accumulated <- rbind(pvalues_accumulated, pvalues)
      
      # Step 9: Calculating x and y scales for plotting
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
        
        # Splitting and generating axis scales
        df_scales_x <- split(df_scales_x, df_scales_x$Year)
        
        scales_x <- lapply(df_scales_x, function(x) {
          # Checking if the column 'Values' in full dataset is numeric
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
      
      
      #Ensuring that Tissue is a factor with the correct order for 'y' axis scales
      reordered_tissues <- c("liver", "stomach", "muscle", "inksac")
      long_dataset$Tissue <- factor(long_dataset$Tissue, levels = reordered_tissues)
      
      # Computing y-axis limits per Tissue-Year
      y_limits <- long_dataset %>%
        group_by(Tissue, Year) %>%
        summarise(
          y_axis_lower_limit = if (n() == 1) 0 else min(concentrations, na.rm = TRUE),
          y_axis_upper_limit = if (any(unique(Tissue) %in% pvalues_accumulated$Tissue)) {
            max(concentrations, na.rm = TRUE) / 0.80
          } else {
            max(concentrations, na.rm = TRUE)
          },
          .groups = "drop"
        )
      
      # Setup tissue list and breaks
      original_tissue_list <- c("liver", "stomach", "muscle", "inksac")
      original_breaks <- c(5, 5, 5, 5)
      
      # Joining limits and define scales
      df_scales_y <- y_limits %>%
        mutate(
          n = original_breaks[match(Tissue, original_tissue_list)],
          ymin = y_axis_lower_limit,
          ymax = y_axis_upper_limit
        ) %>%
        filter(!is.na(ymin) & !is.na(ymax)) %>%
        select(Tissue, Year, ymin, ymax, n) %>%
        
        # Capitalizing Tissue in the TissueYear column
        mutate(TissueYear = paste(stringr::str_to_title(Tissue), Year, sep = "-"))
      
      
      # Ensure TissueYear is a factor with the correct order
      df_scales_y <- df_scales_y %>%
        mutate(TissueYear = factor(TissueYear, levels = TissueYear))
      
      # Creating named list of y scales for ggh4x
      df_scales_y_list <- split(df_scales_y, df_scales_y$TissueYear)
      
      scales_y <- lapply(df_scales_y_list, function(x) {
        scale_y_continuous(limits = c(x$ymin, x$ymax), n.breaks = x$n)
      })
      
      
      # Assigning names for y scales
      names(scales_y) <- names(df_scales_y_list)
      
      
      # Filtering out unused scales
      facet_panels_with_data <- unique(with(long_dataset, paste(stringr::str_to_title(Tissue), Year, sep = "-")))  # match "Tissue-Year" format
      scales_y <- scales_y[names(scales_y) %in% facet_panels_with_data]
      
      
      
      #Saving pvalue accumulated data in list
      list3<-append(list(pvalues_accumulated),list3, 0)
      name3 <- paste(variable,'_',tissues[i],"_coeffcient_results",sep = "")
      list3names <- append(list3names,name3)
    
      
     
      # Step 10: preprocessing of datasets before plotting
      # Defining correct order
      reordered_tissues_upper <- stringr::str_to_title(c("liver", "stomach", "muscle", "inksac"))
      years <- c(2019, 2020, 2021)
      
      # Create the correct facet order as a character vector
      facet_levels <- as.vector(t(outer(reordered_tissues_upper, years, paste, sep = "-")))
      
      # Update your dataset
      long_dataset <- long_dataset %>%
        mutate(
          Tissue = stringr::str_to_title(as.character(Tissue)),  # Capitalize tissue
          Tissue = factor(Tissue, levels = reordered_tissues_upper),   # Apply factor levels
          Year = as.integer(Year),
          TissueYear = paste(Tissue, Year, sep = "-"),
          TissueYear = factor(TissueYear, levels = facet_levels) # Set correct facet order
        )
      
      
      # Format pvalues data to match facet structure: capitalize tissue, order factors, create TissueYear key
      if (nrow(pvalues_accumulated)!=0){
      pvalues_accumulated <- pvalues_accumulated %>%
        mutate(
          Tissue = factor(stringr::str_to_title(as.character(Tissue)), levels = reordered_tissues_upper),
          Year = as.integer(Year),
          TissueYear = factor(
            paste(Tissue, Year, sep = "-"),
            levels = levels(long_dataset$TissueYear)
          )
        )
      }
      
      cat("\n----------\n")
      print(pollutant)
      print(pvalues_accumulated)


      # Step 11: Plotting
      if (unique(long_dataset$vars) == "Month_of_Capture") {
        long_dataset$Values <- cut(long_dataset$Values, breaks = 4, labels = c(3, 4, 5, 6))
      }

      # Ensuring that Tissue is a factor with the correct order for plotting 
      long_dataset$Tissue <- factor(long_dataset$Tissue, levels = reordered_tissues)
      Colors <- setNames(c('#F8766D', '#7CAE00', '#00A9FF'), sizes)
      
      #Plotting graphs per pollutant
        graph <- long_dataset %>% ggplot(aes(Values, concentrations, colour = size, group = size)) +
          scale_colour_manual(values = Colors) +
          geom_smooth(method = "lm", se = FALSE) +
          labs(
            title = paste(icons_markdown[h],'<B>', pollutant, '::', '</B>', variable, "Vs Conc mg/kg using size", sep = ""),
            y = "Concentration mg/kg", 
            x = paste(variable)
          ) +
          theme(
            plot.title = ggtext::element_markdown(),
            strip.text = element_text(size = 12, face = "bold"),
            legend.title = element_text(face = "bold", size = 14)
          ) +
          facet_wrap(~ TissueYear, scales = "free", nrow = 4, ncol = 3, strip.position = "top") +
          ggh4x::facetted_pos_scales(
            x = scales_x,
            y = scales_y
          )+ 
          geom_point(aes(shape = size, color = size), size = 2) +
          {if (nrow(pvalues_accumulated) != 0) geom_text(pvalues_accumulated, mapping = aes(label = paste(rho, pvalues, sep = ",")), hjust = 0, size = 3.5, fontface = "italic", position = position_dodge(width = .1), check_overlap = FALSE)}
        
        #Appending graphs to list 
        list0 <- append(list(graph), list0, 0)
        name0 <- paste(pollutant, "_plot_", variable, sep = "")
        list0names <- append(list0names, name0)
    }
  }
  
  #Combining rows from with coefficients to add to list later
  final_coefficients_accumulated <- rbind(final_coefficients_accumulated, coefficients_accumulated)
  
  #If a list is not empty, it assigns names to its elements using the corresponding listXnames vector.
  if (length(list0) > 0) names(list0) <- list0names  

  
  
  return(list(graphs = list0, final_coefficients_accumulated = final_coefficients_accumulated))
}


#Calling Main Function. All arguments except remove.zeroes (default is set at False) are required and user has to choose between datasets_for_organic_compounds or datasets_for_trace_metals. The user also has the option of choosing between three variables mainly Month_of_Capture, dta_km(distance to Argentina_km) and dtfl_km(distance to the Falkland Islands_km) to show how pollution levels change in different tissues of the squid depending on the aforementioned three variables. Users can also remove all zeroes and focus on only the detected concentrations or keep them. The results are saved in size_comparison_results list. Variable can only be either 'dta_km','dtfl_km' or 'Month_of_Capture'
size_comparison_results_per_pollutant <- comparing_sizes_per_pollutant (datasets_for_trace_metals, variable='Month_of_Capture', remove.zeroes = FALSE) 
#Below code saves multiple plots into individual PNG files. It loops through the list of plots and and for each plot it extracts its name, and the plot then saves it in one .png file using grid.draw()
save_graphs <- function(graph_list) {
  
# Extract pollutant types from the first available plot  
plot_name <- names(graph_list[["graphs"]])[[1]]
variable_name <- sub(".*_plot_", "", plot_name)  
  
  
# Define the folder where you want to save the PNG files 
if (grepl("Metal", names(graph_list[["graphs"]])[[1]])) {
output_folder <- file.path("/Users/mrnobody/Documents/GitHub/Squid_Fest/Squid_Concentration_Analysis/3-Data_Mining/Data_mining_plots/size_comparison_per_pollutant/Trace_metals",variable_name)
}else{
output_folder <- file.path("/Users/mrnobody/Documents/GitHub/Squid_Fest/Squid_Concentration_Analysis/3-Data_Mining/Data_mining_plots/size_comparison_per_pollutant/Organic_compounds",variable_name)  
}

# Create the folder if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Loop through each plot
for (i in seq_along(graph_list$graphs)) {
  
  plot_name <- names(graph_list$graphs)[i]
  plot_object <- graph_list$graphs[[i]]

  # Debugging info
  cat("\n----------\n")
  cat("Checking:", plot_name, "\n")
  print(class(plot_object))
  print(length(plot_object))
  
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
save_graphs(size_comparison_results_per_pollutant)
