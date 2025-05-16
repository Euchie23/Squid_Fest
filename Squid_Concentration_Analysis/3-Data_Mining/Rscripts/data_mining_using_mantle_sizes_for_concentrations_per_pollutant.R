#LOADING LIBRARIES----
library(ggplot2)  # For plotting graphs
library(dplyr) # for data manipulation
library(tidyr) # For a collection of R packages used for data manipulation and visualization.
library(stringr) # For String Manipulation
library(tibble) # For creating tibbles



#Data Processor Function before doing analysis
#IT is recommended that the 'Keep_LOQ_values' argument remain as FALSE to remove all values below blank control (BB) or LOQ (BLOQ) to get a more accurate figure for the number of outliers within the dataset for the data distribution otherwise it will also classify values that are BB or BLOQ  as outliers leading to couble counting.
process_dataset <- function(data, keep_LOQ_values=FALSE) {
  # Check if the dataset is Heavy Metals (Ag is in the 16th column)
  if (grepl("Fe|Ag", colnames(data)[16])) {
    
    # Processing for Heavy Metals dataset
    data <- data %>%
      relocate(1:15, colnames(data)[16:25]) %>%
      mutate(across(4, ~ paste0(., ".")))  # Add "." to column 4
    
    # FUNCTIONS TO HELP MODIFY ID NUMBERS FOR FURTHER ANALYSIS (HM)
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
    
    
    # FUNCTIONS TO HELP MODIFY ID NUMBERS FOR FURTHER ANALYSIS (HM)
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


# Data Processing for Heavy Metals dataset
heavymetals_data <- read.csv("Datasets/Results/Final_HMresults_mgkg.csv", header = TRUE)
datasets_for_heavy_metals <- process_dataset(heavymetals_data, keep_LOQ_values = FALSE) 


# Data Processing for Organic Compounds dataset
organiccompounds_data <- read.csv("Datasets/Results/Final_OCresults_mgkg.csv", header = TRUE)
datasets_for_organic_compounds <- process_dataset(organiccompounds_data, keep_LOQ_values = FALSE)


#generating the markdown for reading images:
#OCicons for Organic Compounds
OCiconz <- data.frame(pollutants=c("Adipic_acid","Caprolactam","Chlorpyrifos","Ibuprofen"), icons=c("https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=15168&format=png","https://img.icons8.com/?size=38&id=15168&format=png"))
urls <-OCiconz$icons
names(urls) <- OCiconz$pollutants

#HMicons for Heavy Metals:
HMiconz <- data.frame(pollutants=c("Ag","Cd","Co","Cu","Fe","Hg","Ni","Pb","Tl","Zn"), icons=c("https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png"))
urlz <-HMiconz$icons
names(urlz) <- HMiconz$pollutants


#Helper function for prepping markdown for ggplot. This is used to turn the url text into an image.
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
  icon_path <- file.path("OCicons", paste0(x[1], ".png"))
  
  # Construct the HTML string
  y <- paste0(" <img src='", icon_path, "' width='17'/>")
  
  return(y)
}



#Helper function to activate the icons for heavy metal pollutants in final graph, It first checks if the icons exist and if they do then they are posted in the graph: 
markdown_function_for_HM_icons <- function(x) {
  # Create file paths correctly
  file1 <- file.path("HMicons", paste0(x[1], ".png"))
  file2 <- file.path("HMicons", paste0(x[1], "1.png"))
  file3 <- file.path("HMicons", paste0(x[1], "2.png"))
  
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



# Helper function to adjust the variable based on the year and other factors
adjust_variable <- function(subset_by_year, size, year) {
  if (unique(subset_by_year$vars) == 'dtfl_km' & unique(subset_by_year$Year) == '2019') {
    return(max(subset_by_year$Values) * 0.75)
  } else if (unique(subset_by_year$vars) == 'dtfl_km' & unique(subset_by_year$Year) == '2020') {
    return(max(subset_by_year$Values) * 0.85)
  } else if (unique(subset_by_year$vars) == 'dtfl_km' & unique(subset_by_year$Year) == '2021') {
    return(max(subset_by_year$Values) * 0.5)
  } else if (unique(subset_by_year$vars) == 'dta_km' & unique(subset_by_year$Year) == '2019') {
    return(min(subset_by_year$Values))
  } else if (unique(subset_by_year$vars) == 'dta_km' & unique(subset_by_year$Year) == '2020') {
    return(min(subset_by_year$Values))
  } else if (unique(subset_by_year$vars) == 'dta_km' & unique(subset_by_year$Year) == '2021') {
    return(min(subset_by_year$Values))
  } else {
    return(max(as.numeric(as.factor(subset_by_year$Values))) * 0.2)
  }
}



# Helper function to calculate and store coefficients for different tissue sizes, filtering out outliers and using minimun outlier concentration to set upper limit for y axis.
get_coefficients <- function (dataset_subsetted_by_tissue, axis_modification = axis_modification_multiplier){
  
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
  # Create an empty data frame to store the results of the coefficients. final coefficient_dataframe_final where coefficient1 is accumulated
  coefficient_dataframe_final <- data.frame(matrix(ncol=9, nrow = 0))
  colnames(coefficient_dataframe_final) <- c('Tissue', 'Year','pollutant','concentrations', 'Values','rho', 'pvalues', 'y_axis_upper_limit', 'size')
  
  # Loop through the years and calculate coefficients
  while(s != index) {
    
    # Temporary data frame to store coefficients for the current iteration
    coefficient1 <- data.frame(matrix(ncol=9, nrow = 0))
    colnames(coefficient1) <- colnames(coefficient_dataframe_final)
    
    # Create an empty data frame for storing results for the current year and size
    coefficient_dataframe1 <- data.frame(matrix(ncol=9, nrow = 0)) 
    colnames(coefficient_dataframe1) <- colnames(coefficient_dataframe_final)
    
    # Identify outliers: concentrations above the 90th percentile
    outlier  <-which(dataset_subsetted_by_tissue$concentrations > quantile(dataset_subsetted_by_tissue$concentrations, 0.95))  # Find concentrations greater than the 'quantile_level_for_outliers' inserted when running the parent function.
    outliers <- dataset_subsetted_by_tissue$concentrations[c(outlier)] # Extract the outlier concentrations
    
    # If there are outliers, remove them and calculate the upper limit for the y-axis
    if(length(outliers)!=0){
      #pollutant <- dataset_subsetted_by_tissue[-c(outlier),] # Remove outliers from the data
      minimum_outlier <- min(outliers) # Find the minimum value among outliers
      y_upper_limit <-minimum_outlier*axis_modification # Set the upper limit for the y-axis based on the smallest outlier
    }else{
      y_upper_limit <-0 # If no outliers, set y_upper_limit to 0
    }
    
    # Filter data by the year (s-th year in the loop)
    subset_by_year <- filter(as.data.frame(dataset_subsetted_by_tissue), Year == years[s])
    
    # If there is data for the selected year, process by sizes (small, medium, large)
    if(nrow(subset_by_year)!=0){
      for (i in 1:length(sizes)){
        size_dataframe <- filter(subset_by_year, size == sizes[i]) # Filter by current size
        
        # If there are not enough data points or all concentrations are the same, set default values
        if(nrow(size_dataframe)<2|all(size_dataframe[-1,'concentrations'] == size_dataframe[1,'concentrations']|max(size_dataframe$concentrations)== 0)==TRUE){ 
          
          # Set default values if data is insufficient or all concentrations are the same
          coefficient1[1,1] <- unique(subset_by_year$Tissue)
          coefficient1[1,2] <- years[s]
          coefficient1[1,3] <- unique(subset_by_year$pollutants)
          coefficient1[1,4] <- 0 # Set variable to 0
          coefficient1[1,5] <- 0 # Set rho to 0
          coefficient1[1,6] <- 0 # Set p-value to 0
          coefficient1[1,7] <- 0
          coefficient1[1,8] <- ifelse(max(size_dataframe$concentrations)|length(size_dataframe$concentrations) == 0, 0, max(size_dataframe$concentrations)) # Max concentration for y-axis
          coefficient1[1,9] <- sizes[i]
          
          coefficient_dataframe1 <- rbind(coefficient_dataframe1, coefficient1) # Append the current coefficient to the results
          
          # If the maximum concentration in the size dataframe is not 0 then go to perform the correlation test for large sized squids.
        }else{
          
          # Perform correlation test for different size categories (small, medium, large)
          coefficient0 <- cor.test(as.numeric(as.factor(size_dataframe$Values)), as.numeric(size_dataframe$concentrations), method = "spearman", exact = FALSE)
          
          # Store the results for correlation coefficient and p-value
          coefficient1[1, 1] <- unique(size_dataframe$Tissue)
          coefficient1[1, 2] <- years[s]
          coefficient1[1, 3] <- unique(size_dataframe$pollutants)
          if (str_detect(sizes[i], "large")== TRUE){
            coefficient1[1, 4] <- signif((y_upper_limit * 0.95), 3)  #X-coordinate for rho coefficient and p-value
          }else if (str_detect(sizes[i], "medium")== TRUE){
            coefficient1[1, 4] <- signif((y_upper_limit * 0.85), 3)
          }else{
            coefficient1[1, 4] <- signif((y_upper_limit * 0.75), 3) 
          }
          
          
          # Adjust the variable depending on the year and the variable type
          coefficient1[1, 5] <- adjust_variable(subset_by_year, sizes[i], years[s])
          
          # Store the rho and p-values from the correlation test
          coefficient1[1, 6] <- coefficient0$estimate
          coefficient1[1, 7] <- coefficient0$p.value
          coefficient1[1, 8] <- y_upper_limit  # Store the calculated yscale
          coefficient1[1, 9] <- unique(size_dataframe$size)
          # Append the calculated coefficients to the results
          coefficient_dataframe1 <- rbind(coefficient_dataframe1, coefficient1)
        }
      }
    }       
    
    # Increment the year index
    s<-s+1
    
    
    # Append the results from the current year to the final dataframe
    coefficient_dataframe_final <- rbind(coefficient_dataframe_final, coefficient_dataframe1)
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




# Main function: data_mining_using_mantle_length_for_concentrations
# This function performs a series of data processing steps, including statistical analysis, and plotting, to investigate the relationship between various environmental concentrations and biological variables (such as tissue types and year). The analysis uses mantle length as a categorical variable to group data into small, medium, and large categories, which are then used to generate plots of pollutant concentrations versus different variables (e.g.,distance traveled or size). The function includes options for handling outliers, zero values, and performing statistical modeling (e.g., linear regression) to assess the strength of these relationships. Plots are generated using `ggplot2`, and results are returned in a list.

data_mining_using_mantle_length_for_concentrations_per_pollutant <- function (data_list, axis_modification_multiplier, variable, remove.zeroes = FALSE){
  
  dataset_with_numerical_values <- data_list$dataset_with_numerical_values
  
  # Step 1: Define the Mantle lengths and categorize them into small, medium, large
  Mantle_lengths <- sort(unique(dataset_with_numerical_values$Mantle_length_mm))
  small <- Mantle_lengths[1:(1/3 * length(Mantle_lengths))]
  medium <- Mantle_lengths[(1/3 * length(Mantle_lengths) + 1):(2/3 * length(Mantle_lengths))]
  large <- Mantle_lengths[(2/3 * length(Mantle_lengths) + 1):length(Mantle_lengths)]
  
  # Step 2: Initialize empty lists for storing results
  list0 <- list()
  # list1 <- list()
  # list2 <- list()
  list0names <- c()
  # list1names <- c()
  # list2names <- c()
  
  # Step 3: Determine pollutant range and subset the dataset based on its presence
  if (grepl("Fe|Ag", colnames(dataset_with_numerical_values)[16])) {
    # Heavy metals subset
    range <- colnames(dataset_with_numerical_values[16:25])
    icons_hm <- apply(HMiconz, 1, markdown_function_for_HM_icons)
    icons_markdown <- icons_hm
    range_name <- 'Fe:Pb'
    number_range <- 17:26
  } else {
    # Organic compounds subset
    icons_oc <- apply(OCiconz, 1, markdown_function_for_OC_icons)
    icons_markdown <- icons_oc
    range <- colnames(dataset_with_numerical_values[16:19])
    range_name <- 'Adipic_acid:Ibuprofen'
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
  final_coefficients_accumulated <- data.frame(matrix(ncol = 9, nrow = 0))# making empty datasets to store results
  
  subsetted_dataset_with_variables <- dataset_with_numerical_values[, c(8, 9, 13)]#Subsetting 
  
  subsetted_dataset_with_numerical_values <- dataset_with_numerical_values[,c(3, 6, 7:13, number_range)]# subsetting needed columns for futher analysis 
  
  
  # Step 6: Loop through variables and calculate coefficients
  for (h in 1:length(number_range)) {
   pollutant <- colnames(dataset_with_numerical_values[,c(number_range)])[h]
    print(pollutant)
    
    coefficients_accumulated <- data.frame(matrix(ncol = 9, nrow = 0))
    pvalues_accumulated <- data.frame(matrix(ncol = 9, nrow = 0))
    
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
      # Step 8: Loop through tissues and calculate coefficients
      for (i in 1:length(tissues)) {
        long_dataset_subsetted_by_tissue <- long_dataset %>%
          group_by(Year) %>%
          filter(Tissue == tissues[i])
        if (nrow(long_dataset_subsetted_by_tissue) > 0) {
          coefficients <- get_coefficients(long_dataset_subsetted_by_tissue, axis_modification_multiplier)
          coefficients_accumulated <- rbind(coefficients_accumulated, coefficients)
        }
      }
      
      # Step 9: Calculate p-values
      pvalues <- coefficient_results_modification(coefficients_accumulated)
      pvalues_accumulated <- rbind(pvalues_accumulated, pvalues)
      print(pvalues_accumulated)
      
      # Step 10: Modify dataset with accumulated coefficients
      coefficients_accumulated_modified <- coefficients_accumulated[, -c(4:7)] %>%
        distinct(Tissue, Year, y_axis_upper_limit, size)
      
      long_dataset2 <- long_dataset %>%
        left_join(coefficients_accumulated_modified, by = c('Year' = 'Year', 'Tissue' = 'Tissue', 'size' = 'size'))
      
      # Function to replace NA values
      replacing_na <- function(x) {
        if (is.na(x[10])) {
          x[10] <- x[7]
        }
        return(x)
      }
      
      # Apply the function to replace NAs
      long_dataset_modified <- data.frame(t(apply(long_dataset2, 1, replacing_na)))
      colnames(long_dataset_modified) <- colnames(long_dataset2)
      long_dataset_modified$y_axis_upper_limit <- as.numeric(long_dataset_modified$y_axis_upper_limit)
      
      # Filter the modified dataset for the highest y_axis_upper_limit per tissue
      long_dataset_modified1 <- long_dataset_modified %>%
        group_by(Tissue) %>%
        filter(y_axis_upper_limit == max(y_axis_upper_limit)) %>%
        distinct(Tissue, y_axis_upper_limit)
      
      # Ensuring that Tissue is a factor with the correct order for 'y' axis scales
      reordered_tissues <- c("liver", "stomach", "muscle", "inksac")
      long_dataset_modified1$Tissue <- factor(long_dataset_modified1$Tissue, levels = c("liver", "stomach", "muscle", "inksac"))
      
      # Create scale data for plotting
      df_scales <- data.frame(
        Tissue = factor(c("liver", "stomach", "muscle", "inksac"), levels = reordered_tissues),  # Ensure order
        ymin = c(0, 0, 0, 0),
        ymax = c(NA, NA, NA, NA),
        n = c(5, 5, 5, 5)
      )
      
      df_scales <- df_scales %>%
        left_join(long_dataset_modified1, by = "Tissue") %>%
        mutate(ymax = coalesce(y_axis_upper_limit, 0)) %>%
        select(Tissue, ymin, ymax, n)
      
      
      # Generate scale data for plotting
      df_scales <- split(df_scales, df_scales$Tissue)
      scales <- lapply(df_scales, function(x) {
        scale_y_continuous(limits = c(x$ymin, x$ymax), n.breaks = x$n)
      })
    
      # Step 11: Plotting
      if (unique(long_dataset$vars) == "Month_of_Capture") {
        long_dataset$Values <- cut(long_dataset$Values, breaks = 4, labels = c(3, 4, 5, 6))
      }
      
      # Ensuring that Tissue is a factor with the correct order for plotting 
      long_dataset$Tissue <- factor(long_dataset$Tissue, levels = c("liver", "stomach", "muscle", "inksac"))
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
          facet_grid(factor(Tissue, levels = reordered_tissues) ~ Year, scales = "free", drop = FALSE) +
          ggh4x::facetted_pos_scales(y = scales) +
          geom_point(aes(shape = size, color = size), size = 2) +
          {if (nrow(pvalues_accumulated) != 0) geom_text(pvalues_accumulated, mapping = aes(label = paste(rho, pvalues, sep = ",")), hjust = -0.7, size = 3.5, fontface = "italic", position = position_dodge(width = .1), check_overlap = FALSE)}
        
        #Appending graphs to list 
        list0 <- append(list(graph), list0, 0)
        name0 <- paste(pollutant, "plot", variable, sep = "")
        list0names <- append(list0names, name0)
    }
  }
  
  #Combining rows from with coefficients to add to list later
  final_coefficients_accumulated <- rbind(final_coefficients_accumulated, coefficients_accumulated)
  
  #If a list is not empty, it assigns names to its elements using the corresponding listXnames vector.
  if (length(list0) > 0) names(list0) <- list0names  

  
  
  return(list(graphs = list0, final_coefficients_accumulated = final_coefficients_accumulated))
}



#Calling Main Function 
elements_from_data_mining_using_mantle_length_per_pollutant <- data_mining_using_mantle_length_for_concentrations_per_pollutant (datasets_for_organic_compounds_data_distribution, axis_modification_multiplier = 0.80, variable='Month_of_Capture', remove.zeroes = TRUE)
