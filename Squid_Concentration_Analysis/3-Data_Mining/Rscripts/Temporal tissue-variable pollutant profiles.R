#LOADING LIBRARIES----
library(ggplot2)  # For plotting graphs
library(ggtext) # for extending ggplot2 themes to support Markdown/HTML in text elements like titles and labels.
library(grid) # for creating, modifying, and arranging graphical objects ("grobs") like text, lines, rectangles, and complex layouts. 
library(dplyr) # for data manipulation
library(tidyr) # for a collection of R packages used for data manipulation and visualization.
library(stringr) # For String Manipulation
library(tibble) # For creating tibbles
library(tools) # for capitalizing the first letter in a string


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
datasets_for_Trace_metals <- process_dataset(tracemetals_data, keep_LOQ_values = FALSE) 


# Data Processing for Organic Compounds dataset
organiccompounds_data <- read.csv("Squid_Concentration_Analysis/3-Data_Mining/Datasets/preprocessed_data/Final_OCresults_mgkg.csv", header = TRUE)
datasets_for_organic_compounds <- process_dataset(organiccompounds_data, keep_LOQ_values = FALSE)

#generating the markdown for reading images:
#OCicons for Organic Compounds
OCiconz <- data.frame(pollutants=c("Organic_A","Organic_B","Organic_C","Organic_D"), icons=c("https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=15168&format=png","https://img.icons8.com/?size=38&id=15168&format=png"))
urls <-OCiconz$icons
names(urls) <- OCiconz$pollutants

#TMicons for Trace Metals:
TMiconz <- data.frame(pollutants=c("Ag","Cd","Co","Cu","Fe","Hg","Ni","Pb","Tl","Zn"), icons=c("https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png"))
urlz <-TMiconz$icons
names(urlz) <- TMiconz$pollutants


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
  y <- paste0(x[1]," <img src='", icon_path, "' width='17'/>")
  
  return(y)
}



#Helper function to activate the icons for Trace metal pollutants in final graph, It first checks if the icons exist and if they do then they are loaded unto the graph: 
markdown_function_for_TM_icons <- function(x) {
  # Create file paths correctly
  file1 <- file.path("TMicons", paste0(x[1], ".png"))
  file2 <- file.path("TMicons", paste0(x[1], "1.png"))
  file3 <- file.path("TMicons", paste0(x[1], "2.png"))
  
  # Check which files exist and build the HTML string accordingly
  if (file.exists(file1) & file.exists(file2) & file.exists(file3)) {
    y <- paste0(x[1], "<img src='", file1, "' width='17'/> ",
                "<img src='", file2, "' width='17'/> ",
                "<img src='", file3, "' width='17'/>")
  } else if (file.exists(file1) & file.exists(file2)) {
    y <- paste0(x[1], "<img src='", file1, "' width='17'/> ",
                "<img src='", file2, "' width='17'/>")
  } else {
    y <- paste0(x[1], "<img src='", file1, "' width='17'/>")
  }
  
  return(y)
}

# Helper function to calculate and store coefficients after performing spearmans correlation to assess the strength and direction of the monotonic relationship between year and concentrations. 
get_coefficients <- function (subsetted_tissue_dataset){
  #print(subsetted_tissue_dataset[,c(7:14)])
  # Extracting unique factor levels for sizes, tissues, and years
  pollutants <- levels(factor(subsetted_tissue_dataset$pollutants)) # pollutant categories
  tissues <- levels(factor(subsetted_tissue_dataset$Tissue)) # Tissue categories
  years <- levels(factor(subsetted_tissue_dataset$Year, levels = c("2019", "2020", "2021"))) # Year categories
  
  # Initialize variable to control loop iteration based on tissue type
  s<-1
  # Create an empty data frame to store the results of the coefficients. final coefficient_results_final where coefficient_results_1 is accumulated
  coefficient_results_final <- data.frame(matrix(ncol=7, nrow = 0))
  colnames(coefficient_results_final) <- c('Tissue', 'Year','pollutants','concentrations', 'Values','rho', 'pvalues')
  
  # Loop through the years and calculate coefficients
  while(s != 4) {
    # Temporary data frame to store coefficients for the current iteration
    coefficient_results_1 <- data.frame(matrix(ncol=7, nrow = 0))
    colnames(coefficient_results_1) <- colnames(coefficient_results_final)
    
    # Create an empty data frame for storing results for the current year and size
    coefficient_dataframe_final <- data.frame(matrix(ncol=7, nrow = 0)) 
    colnames(coefficient_dataframe_final) <- colnames(coefficient_results_final)
    
    # Filter data by the year (s-th year in the loop).........
    tissues_per_year <- filter(as.data.frame(subsetted_tissue_dataset), Year == years[s])
    
    #for loop to iterate over pollutants
    for (h in 1:length(pollutants)){
      
      # Filtering the subsetted_tissue_dataset dataframe by pollutants
      pollutants_per_tissue <- filter(as.data.frame(subsetted_tissue_dataset), pollutants == pollutants[h])
      
      #Filtering the tissues_per_year dataframe by pollutants
      pollutants_per_tissue_per_year <- filter(as.data.frame(tissues_per_year), pollutants == pollutants[h])
      
      #Changing values to numeric to be manipulated and plotted
      Values_numeric <- suppressWarnings(as.numeric(as.character(pollutants_per_tissue$Values)))
      
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
      concentrations <- pollutants_per_tissue$concentrations
      ycoord <- ifelse(any(!is.na(concentrations)), max(concentrations, na.rm = TRUE), 1)
      
      
      # If there are not enough data points or all concentrations are the same, set default values
      if(nrow(pollutants_per_tissue_per_year)<2|all(pollutants_per_tissue_per_year[-1,'concentrations'] == pollutants_per_tissue_per_year[1,'concentrations']|max(pollutants_per_tissue_per_year$concentrations)== 0)==TRUE){ 
        # Set default values if data is insufficient or all concentrations are the same
        coefficient_results_1[1,1] <- unique(pollutants_per_tissue$Tissue)
        coefficient_results_1[1,2] <- years[s]
        coefficient_results_1[1,3] <- pollutants[h]
        coefficient_results_1[1,4] <- 0 # Set concentration to 0
        coefficient_results_1[1,5] <- 0 # Set values to 0
        coefficient_results_1[1,6] <- 0 # Set Rho to 0
        coefficient_results_1[1,7] <- 0# Set p-value to 0
        
        coefficient_dataframe_final <- rbind(coefficient_dataframe_final, coefficient_results_1) # Append the current coefficient to the results
      }else{
        
        # Perform correlation test for values (of variables) per tissue per year vs concentrations per tissue per year
        coefficient_results <- cor.test(as.numeric(as.factor(pollutants_per_tissue_per_year$Values)), as.numeric(pollutants_per_tissue_per_year$concentrations), method="spearman", exact = FALSE)
        
        # Store the results for correlation coefficient and p-value
        coefficient_results_1[1, 1] <- unique(pollutants_per_tissue$Tissue)
        coefficient_results_1[1, 2] <- years[s]
        coefficient_results_1[1, 3] <- pollutants[h]
        #Y coordinate for rho coefficient and p-value
        if (str_detect(years[s], '2019')== TRUE){
          coefficient_results_1[1, 4] <- signif((ycoord/ 0.82), 3) 
        }else if (str_detect(years[s], '2020')== TRUE){
          coefficient_results_1[1, 4] <- signif((ycoord/ 0.88), 3)
        }else{
          coefficient_results_1[1, 4] <- signif((ycoord/ 0.94), 3) 
        }
        #X coordinate for rho coefficient and p-value
        if (unique(pollutants_per_tissue_per_year$vars) == 'Gender'){
          coefficient_results_1[1, 5] <- -0.45
        }else if (unique(pollutants_per_tissue_per_year$vars) == 'Month_of_Capture'){
          coefficient_results_1[1, 5] <- midpoint_x_axis * 0.4
        }else if (unique(pollutants_per_tissue_per_year$vars) == 'Maturity_level'){
          coefficient_results_1[1, 5] <- midpoint_x_axis * 0.4
        }else{
          coefficient_results_1[1, 5] <- midpoint_x_axis 
        }
        
        # Store the rho and p-values from the correlation test
        coefficient_results_1[1, 6] <- coefficient_results$estimate
        coefficient_results_1[1, 7] <- coefficient_results$p.value
        
        # Append the calculated coefficients to the results
        coefficient_dataframe_final <- rbind(coefficient_dataframe_final, coefficient_results_1)
        
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
  
  #print(coefficients_accumulated)
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