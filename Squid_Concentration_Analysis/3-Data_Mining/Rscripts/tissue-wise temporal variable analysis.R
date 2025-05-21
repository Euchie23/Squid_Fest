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
  icon_path <- file.path("Squid_Concentration_Analysis/3-Data_Mining/OCicons", paste0(x[1], ".png"))
  
  # Construct the HTML string
  y <- paste0(x[1]," <img src='", icon_path, "' width='17'/>")
  
  return(y)
}



#Helper function to activate the icons for Trace metal pollutants in final graph, It first checks if the icons exist and if they do then they are loaded unto the graph: 
markdown_function_for_TM_icons <- function(x) {
  # Create file paths correctly
  file1 <- file.path("Squid_Concentration_Analysis/3-Data_Mining/TMicons", paste0(x[1], ".png"))
  file2 <- file.path("Squid_Concentration_Analysis/3-Data_Mining/TMicons", paste0(x[1], "1.png"))
  file3 <- file.path("Squid_Concentration_Analysis/3-Data_Mining/TMicons", paste0(x[1], "2.png"))

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

# Main function: comparing_years_per_tissue_per_variable
#This function helps explore how different environmental and biological factors, refered to as variables in the analysis, (like distance to land, latitude, and month of capture) relate to pollutant levels in squid tissues. It focuses on how these relationships look within each tissue — like liver, stomach, muscle, and ink sac.For each factor (such as distance to land), the function creates separate sets of graphs for each tissue. Within each graph, you’ll see small panels (called facets), where each panel shows the behavior of a specific pollutant (like a trace metal or an organic compound).The data in each panel is grouped by year (e.g., 2019, 2020, 2021), so it’s easy to spot changes over time. The graphs also include regression lines, which help show whether the factor and the pollutant are positively related, negatively related, or not strongly related.The function handles zero values and applies statistical models (like linear regression) to assess the strength of these patterns. It returns both the plots and the statistical output in a list. This approach helps identify whether environmental or biological factors consistently affect pollutant levels within each tissue across different years.
comparing_years_per_tissue_per_variable <- function (data_list, remove.zeroes = FALSE){
  
  # Extract numeric dataset from list
  dataset_with_numerical_values <- as.data.frame(data_list$dataset_with_numerical_values)
  
  #Changing metadata colums from dataset into numeric or factor
  dataset_with_numerical_values[,c(7,8,9,13,14)] =as.numeric(unlist(dataset_with_numerical_values[,c(7,8,9,13,14)]))
  dataset_with_numerical_values[,12]<- cut(as.numeric(dataset_with_numerical_values[,12]), breaks = 4, labels =c(3,4,5,6))
  dataset_with_numerical_values[,15]<- as.character(dataset_with_numerical_values[,15])
  

  # Step 1: Initialize empty lists for storing results
  list0 <- list()
  list1 <- list()
  list2 <- list()
  list3 <- list()
  list4 <- list()
  list0names <- c()
  list1names <- c()
  list2names <- c()
  list3names <- c()
  list4names <- c()
  
  # Step 2: Determine pollutant range and subset the dataset based on its presence
  if (grepl("Metal", colnames(dataset_with_numerical_values)[16])) {
    # Trace metals subset
    icons_tm <- apply(TMicons, 1, markdown_function_for_TM_icons)
    names(icons_tm) <- names(urlz)
    icons_markdown <- icons_tm
    number_range <- 16:25
    cols_to_sort <- c("Metal_A", "Metal_B", "Metal_C","Metal_D", "Metal_E", "Metal_F","Metal_G", "Metal_H", "Metal_I", "Metal_J")
    # Sort them alphabetically
    sorted_cols <- sort(cols_to_sort)
    # Reorder the dataset with sorted subset
    dataset_with_numerical_values <- dataset_with_numerical_values %>%
      select(any_of(setdiff(names(.), cols_to_sort)), all_of(sorted_cols))
    range <- colnames(dataset_with_numerical_values[16:25])
  } else {
    # Organic compounds subset
    icons_oc <- apply(OCicons, 1, markdown_function_for_OC_icons)
    names(icons_oc) <- names(urls)
    icons_markdown <- icons_oc
    number_range <- 16:19
    # Define columns to sort alphabetically
    cols_to_sort <- c("Organic_A","Organic_B","Organic_C","Organic_D")
    # Sort them alphabetically
    sorted_cols <- sort(cols_to_sort)
    # Reorder the dataset with sorted subset
    dataset_with_numerical_values <- dataset_with_numerical_values %>%
      select(any_of(setdiff(names(.), cols_to_sort)), all_of(sorted_cols))
    range <- colnames(dataset_with_numerical_values[16:19])
  }
  

  # Step 3: Loop through variables and calculate coefficients
  for (h in 1:4) {
    cat("\n")
    cat("\n")

    subsetted_dataset_with_numerical_values <- dataset_with_numerical_values[,c(7:9,11,13,15,12,14,number_range,6,3)]# subsetting needed columns for futher analysis (arranged in alphabetical order)
    
    
    tissues <- levels(factor(subsetted_dataset_with_numerical_values$Tissue, levels = c("liver", "stomach", "muscle", "inksac")))#creating a vector of tissues for later data processing
    print(tissues[h])
    
    # coefficients_accumulated <- data.frame(matrix(ncol = 8, nrow = 0))
     pvalues_accumulated <- data.frame(matrix(ncol = 7, nrow = 0))
    
    # subsetted_dataset_with_numerical_values2 <- subsetted_dataset_with_numerical_values #saving subsetted dataset to a new dataset

    for(i in 1:8) {
      cat("\n")
      variable <- colnames(subsetted_dataset_with_numerical_values)[i]
      print(variable)
  
    # Step 4: Long format transformation of the dataset
    if (remove.zeroes == FALSE) {
      long_dataset <- subsetted_dataset_with_numerical_values %>%
        pivot_longer(all_of(range), names_to = "pollutants", values_to = "concentrations") %>%
        pivot_longer(!!rlang::sym(paste(variable)), names_to = "vars", values_to = "Values")%>% 
        mutate(
          concentrations = as.numeric(concentrations)
        )
    } else {
      long_dataset <- subsetted_dataset_with_numerical_values %>%
        pivot_longer(all_of(range), names_to = "pollutants", values_to = "concentrations") %>%
        pivot_longer(!!rlang::sym(paste(variable)), names_to = "vars", values_to = "Values") %>%
        filter(concentrations != 0) %>% 
        mutate(
          concentrations = as.numeric(concentrations)
        )
    }
    
      # Step 5: Subsetting long dataset by tissues and finding the coefficients
        long_subsetted_tissue_dataset <- long_dataset %>%
          group_by(pollutants) %>%
          filter(Tissue == tissues[h])
          coefficients <- get_coefficients(long_subsetted_tissue_dataset)
     
       # Step 6: Calculate p-values
      pvalues <- coefficient_results_modification(coefficients)
      pvalues_accumulated <- rbind(pvalues_accumulated, pvalues)
      print(pvalues)
      
      
      # Step 7: Computing X and Y axis limits for each pollutant (facet) per tissue, per year, for plotting
      if (variable != "Gender") { 
        # Create x upper and lower limits
        # 1. Compute raw x-axis limits from dataset
        x_limits <- long_subsetted_tissue_dataset %>%
          mutate(Values_numeric = suppressWarnings(as.numeric(as.character(Values)))) %>%
          group_by(pollutants) %>%
          summarise(
            x_axis_lower_limit = min(Values_numeric, na.rm = TRUE),
            x_axis_upper_limit = max(Values_numeric, na.rm = TRUE),
            .groups = "drop"
          )
        
        # 2. Define your full possible pollutant list
        if (grepl("Metal", colnames(dataset_with_numerical_values)[16])) {
          original_pollutant_list <- c("Metal_A", "Metal_B", "Metal_C","Metal_D", "Metal_E", "Metal_F","Metal_G", "Metal_H", "Metal_I", "Metal_J")
        } else {
          original_pollutant_list <- c("Organic_A","Organic_B","Organic_C","Organic_D")
        }
        
        # 3. Retain only pollutants that still exist in the dataset
        pollutant_list <- intersect(original_pollutant_list, unique(long_subsetted_tissue_dataset$pollutants))
        
        # 4. Build x scale dataframe for valid pollutants only
        df_scales_x <- data.frame(
          pollutants = pollutant_list,
          xmin = NA_real_,
          xmax = NA_real_
        ) %>%
          left_join(x_limits, by = "pollutants") %>%
          mutate(
            xmin = coalesce(x_axis_lower_limit, NA_real_),
            xmax = coalesce(x_axis_upper_limit, NA_real_),
            range_size = xmax - xmin
          ) %>%
          filter(!is.na(xmin) & !is.na(xmax)) %>%
          select(pollutants, xmin, xmax, range_size)
        
        # 5. Split and generate axis scales
        df_scales_x <- split(df_scales_x, df_scales_x$pollutants)
        
        scales_x <- lapply(df_scales_x, function(x) {
          # Check if the column 'Values' in your full dataset is numeric
          if (is.numeric(long_subsetted_tissue_dataset$Values)) {
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
      
    
      # Y_Axis---- 
      # Create x upper and lower limits
      # 1. Generate y-axis limits by pollutant
      y_limits <- long_subsetted_tissue_dataset %>%
        group_by(pollutants) %>%
        summarise(
          y_axis_lower_limit = min(concentrations, na.rm = TRUE),
          y_axis_upper_limit = if (any(unique(pollutants) %in% pvalues$pollutants)) {
            max(concentrations, na.rm = TRUE) / 0.80
          } else {
            max(concentrations, na.rm = TRUE)
          },
          .groups = "drop"
        )
      
      # 2. Define your full possible list
      if (grepl("Metal", colnames(dataset_with_numerical_values)[16])) {
        original_pollutant_list <- c("Metal_A", "Metal_B", "Metal_C","Metal_D", "Metal_E", "Metal_F","Metal_G", "Metal_H", "Metal_I", "Metal_J")
        original_n <- c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5)
      } else {
        original_pollutant_list <- c("Organic_A","Organic_B","Organic_C","Organic_D")
        original_n <- c(5, 5, 5, 5)
      }
      
      # 3. Retain only pollutants that still exist in the dataset
      pollutant_list <- intersect(original_pollutant_list, unique(long_subsetted_tissue_dataset$pollutants))
      n <- original_n[match(pollutant_list, original_pollutant_list)]  # align n
      
      # 4. Build df_scales_y safely
      df_scales_y <- data.frame(
        pollutants = pollutant_list,
        ymin = NA_real_,
        ymax = NA_real_,
        n = n
      ) %>%
        left_join(y_limits, by = "pollutants") %>%
        mutate(
          ymin = coalesce(y_axis_lower_limit, NA_real_),
          ymax = coalesce(y_axis_upper_limit, NA_real_)
        ) %>%
        filter(!is.na(ymin) & !is.na(ymax)) %>%  # Keep only those with limits
        select(pollutants, ymin, ymax, n)
      
      # 5. Build scales
      df_scales_y <- split(df_scales_y, df_scales_y$pollutants)
      
      scales_y <- lapply(df_scales_y, function(x) {
        scale_y_continuous(limits = c(x$ymin, x$ymax), n.breaks = x$n)
      })
      list4<-append(list(pvalues_accumulated),list4, 0)
      name4 <- paste(variable,'_',tissues[h],"_coeffcient_results",sep = "")
      list4names <- append(list4names,name4)
      
      
      # Step 8: plotting data
      plotting_dataset <- long_subsetted_tissue_dataset 
      if(tissues[h]=='liver'){
        if(unique(plotting_dataset$vars)=='Latitude'){
          plt <-plotting_dataset %>% ggplot(aes(Values, concentrations, colour = Year, group=Year)) +
            geom_smooth(method=lm, se=FALSE)+
            labs(title = paste(tools::toTitleCase(tissues[h]),variable,"Vs Concentrations mg/kg",sep =" "),
                 y = "Concentration mg/kg", x = paste(variable))+
            facet_wrap(vars(pollutants), labeller = as_labeller(icons_markdown), scales ="free", ncol=3, drop = TRUE) +
            ggh4x::facetted_pos_scales(
              x = scales_x,
              y = scales_y
            )+ 
            theme_icons()+
            geom_point(aes(shape = Year, color = Year), size = 2)+
            scale_color_manual(
              values = c("2019" = "red", "2020" = "green", "2021" = "blue"),
              drop = FALSE
            ) +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels
              plot.margin = margin(20, 20, 20, 20)  # Add margins to give labels more space
            )+
            {if(nrow(pvalues)!=0) geom_text(pvalues, mapping=aes(label=paste(rho, pvalues, sep= ",")),hjust =0, size = 3.5, fontface="italic", position = position_dodge(width = .1), check_overlap = FALSE)}
          list0<-append(list(plt),list0, 0)
          name0 <- paste(variable,"_plots_", tissues[h], sep = "")
          list0names <- append(list0names,name0)
        }else{
          #print(str(plotting_dataset))
          plt <-plotting_dataset %>% ggplot(aes(Values, concentrations, colour = Year, group=Year)) +
            geom_smooth(method=lm, se=FALSE)+
            labs(title = paste(tools::toTitleCase(tissues[h]),variable,"Vs Concentrations mg/kg",sep =" "),
                 y = "Concentration mg/kg", x = paste(variable))+
            facet_wrap(vars(pollutants), labeller = as_labeller(icons_markdown), scales ="free", ncol=3, drop = TRUE) +
            ggh4x::facetted_pos_scales(
              x = scales_x,
              y = scales_y
            )+ 
            theme_icons()+
            geom_point(aes(shape = Year, color = Year), size = 2)+
            scale_color_manual(
              values = c("2019" = "red", "2020" = "green", "2021" = "blue"),
              drop = FALSE
            ) +
            # Conditional replacement of 0/1 labels for Gender
            {if (unique(plotting_dataset$vars) == 'Gender')
              scale_x_continuous(
                limits = c(-0.5, 1.5),  # Adjust the x limits to make it more centered
                breaks = c(0, 1), # Place breaks at 0 and 1
                labels = c("Females", "Males")
              ) } +
            # Optional p-value text
            {if(nrow(pvalues)!=0) geom_text(pvalues, mapping=aes(label=paste(rho, pvalues, sep= ",")),hjust =0, size = 3.5, fontface="italic", position = position_dodge(width = .1), check_overlap = FALSE)}
          list0<-append(list(plt),list0, 0)
          name0 <- paste(variable,"_plots_", tissues[h], sep = "")
          list0names <- append(list0names,name0)
        }
      }else if(tissues[h]=='stomach'){
        if(unique(plotting_dataset$vars)=='Latitude'){
          plt <-plotting_dataset %>% ggplot(aes(Values, concentrations, colour = Year, group=Year)) +
            geom_smooth(method=lm, se=FALSE)+
            labs(title = paste(tools::toTitleCase(tissues[h]),variable,"Vs Concentrations mg/kg",sep =" "),
                 y = "Concentration mg/kg", x = paste(variable))+
            facet_wrap(vars(pollutants), labeller = as_labeller(icons_markdown), scales ="free", ncol=3, drop = TRUE) +
            ggh4x::facetted_pos_scales(
              x = scales_x,
              y = scales_y
            )+ 
            theme_icons()+
            geom_point(aes(shape = Year, color = Year), size = 2)+
            scale_color_manual(
              values = c("2019" = "red", "2020" = "green", "2021" = "blue"),
              drop = FALSE
            ) +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels
              plot.margin = margin(20, 20, 20, 20)  # Add margins to give labels more space
            )+
            {if(nrow(pvalues)!=0) geom_text(pvalues, mapping=aes(label=paste(rho, pvalues, sep= ",")),hjust =0, size = 3.5, fontface="italic", position = position_dodge(width = .1), check_overlap = FALSE)}
          list1<-append(list(plt),list1, 0)
          name1 <- paste(variable,"_plots_", tissues[h], sep = "")
          list1names <- append(list1names,name1)
        }else{
          #print(str(plotting_dataset))
          plt <-plotting_dataset %>% ggplot(aes(Values, concentrations, colour = Year, group=Year)) +
            geom_smooth(method=lm, se=FALSE)+
            labs(title = paste(tools::toTitleCase(tissues[h]),variable,"Vs Concentrations mg/kg",sep =" "),
                 y = "Concentration mg/kg", x = paste(variable))+
            facet_wrap(vars(pollutants), labeller = as_labeller(icons_markdown), scales ="free", ncol=3, drop = TRUE) +
            ggh4x::facetted_pos_scales(
              x = scales_x,
              y = scales_y
            )+ 
            theme_icons()+
            geom_point(aes(shape = Year, color = Year), size = 2)+
            scale_color_manual(
              values = c("2019" = "red", "2020" = "green", "2021" = "blue"),
              drop = FALSE
            ) +
            # Conditional replacement of 0/1 labels for Gender
            {if (unique(plotting_dataset$vars) == 'Gender')
              scale_x_continuous(
                limits = c(-0.5, 1.5),  # Adjust the x limits to make it more centered
                breaks = c(0, 1), # Place breaks at 0 and 1
                labels = c("Females", "Males")
              ) } +
            # Optional p-value text
            {if(nrow(pvalues)!=0) geom_text(pvalues, mapping=aes(label=paste(rho, pvalues, sep= ",")),hjust =0, size = 3.5, fontface="italic", position = position_dodge(width = .1), check_overlap = FALSE)}
          list1<-append(list(plt),list1, 0)
          name1 <- paste(variable,"_plots_", tissues[h], sep = "")
          list1names <- append(list1names,name1)
        }
      }else if(tissues[h]=='muscle'){
        if(unique(plotting_dataset$vars)=='Latitude'){
          plt <-plotting_dataset %>% ggplot(aes(Values, concentrations, colour = Year, group=Year)) +
            geom_smooth(method=lm, se=FALSE)+
            labs(title = paste(tools::toTitleCase(tissues[h]),variable,"Vs Concentrations mg/kg",sep =","),
                 y = "Concentration mg/kg", x = paste(variable))+
            facet_wrap(vars(pollutants), labeller = as_labeller(icons_markdown), scales ="free", ncol=3, drop = TRUE) +
            ggh4x::facetted_pos_scales(
              x = scales_x,
              y = scales_y
            )+ 
            theme_icons()+
            geom_point(aes(shape = Year, color = Year), size = 2)+
            scale_color_manual(
              values = c("2019" = "red", "2020" = "green", "2021" = "blue"),
              drop = FALSE
            ) +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels
              plot.margin = margin(20, 20, 20, 20)  # Add margins to give labels more space
            )+
            {if(nrow(pvalues)!=0) geom_text(pvalues, mapping=aes(label=paste(rho, pvalues, sep= ",")),hjust =0, size = 3.5, fontface="italic", position = position_dodge(width = .1), check_overlap = FALSE)} 
          list2<-append(list(plt),list2, 0)
          name2 <- paste(variable,"_plots_", tissues[h], sep = "")
          list2names <- append(list2names,name2)
        }else{
          #print(str(plotting_dataset))
          plt <-plotting_dataset %>% ggplot(aes(Values, concentrations, colour = Year, group=Year)) +
            geom_smooth(method=lm, se=FALSE)+
            labs(title = paste(tools::toTitleCase(tissues[h]),variable,"Vs Concentrations mg/kg",sep =" "),
                 y = "Concentration mg/kg", x = paste(variable))+
            facet_wrap(vars(pollutants), labeller = as_labeller(icons_markdown), scales ="free", ncol=2, drop = TRUE) +
            ggh4x::facetted_pos_scales(
              x = scales_x,
              y = scales_y
            )+ 
            theme_icons()+
            geom_point(aes(shape = Year, color = Year), size = 2)+
            scale_color_manual(
              values = c("2019" = "red", "2020" = "green", "2021" = "blue"),
              drop = FALSE
            ) +
            # Conditional replacement of 0/1 labels for Gender
            {if (unique(plotting_dataset$vars) == 'Gender')
              scale_x_continuous(
                limits = c(-0.5, 1.5),  # Adjust the x limits to make it more centered
                breaks = c(0, 1), # Place breaks at 0 and 1
                labels = c("Females", "Males")
              ) } +
            # Optional p-value text
            {if(nrow(pvalues)!=0) geom_text(pvalues, mapping=aes(label=paste(rho, pvalues, sep= ",")),hjust =0, size = 3.5, fontface="italic", position = position_dodge(width = .1), check_overlap = FALSE)}
          list2<-append(list(plt),list2, 0)
          name2 <- paste(variable,"_plots_", tissues[h], sep = "")
          list2names <- append(list2names,name2)
        }
      }else{
        if(unique(plotting_dataset$vars)=='Latitude'){
          plt <-plotting_dataset %>% ggplot(aes(Values, concentrations, colour = Year, group=Year)) +
            geom_smooth(method=lm, se=FALSE)+
            labs(title = paste(tools::toTitleCase(tissues[h]),variable,"Vs Concentrations mg/kg",sep =" "),
                 y = "Concentration mg/kg", x = paste(variable))+
            facet_wrap(vars(pollutants), labeller = as_labeller(icons_markdown), scales ="free", ncol=3, drop = TRUE) +
            ggh4x::facetted_pos_scales(
              x = scales_x,
              y = scales_y
            )+ 
            theme_icons()+
            geom_point(aes(shape = Year, color = Year), size = 2)+
            scale_color_manual(
              values = c("2019" = "red", "2020" = "green", "2021" = "blue"),
              drop = FALSE
            ) +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels
              plot.margin = margin(20, 20, 20, 20)  # Add margins to give labels more space
            )+
            geom_text(pvalues, mapping=aes(label=paste(rho, pvalues, sep = ",")),hjust =0, size = 3.5, fontface="italic", position = position_dodge(width = .1), check_overlap = FALSE) 
          list3<-append(list(plt),list3, 0)
          name3 <- paste(variable,"_plots_", tissues[h], sep = "")
          list3names <- append(list3names,name3)
        }else{
          #print(str(plotting_dataset))
          plt <-plotting_dataset %>% ggplot(aes(Values, concentrations, colour = Year, group=Year)) +
            geom_smooth(method=lm, se=FALSE)+
            labs(title = paste(tools::toTitleCase(tissues[h]),variable,"Vs Concentrations mg/kg",sep =" "),
                 y = "Concentration mg/kg", x = paste(variable))+
            facet_wrap(vars(pollutants), labeller = as_labeller(icons_markdown), scales ="free", ncol=3, drop = TRUE) +
            ggh4x::facetted_pos_scales(
              x = scales_x,
              y = scales_y
            )+ 
            theme_icons()+
            geom_point(aes(shape = Year, color = Year), size = 2)+
            scale_color_manual(
              values = c("2019" = "red", "2020" = "green", "2021" = "blue"),
              drop = FALSE
            ) +
            # Conditional replacement of 0/1 labels for Gender
            {if (unique(plotting_dataset$vars) == 'Gender')
              scale_x_continuous(
                limits = c(-0.5, 1.5),  # Adjust the x limits to make it more centered
                breaks = c(0, 1), # Place breaks at 0 and 1
                labels = c("Females", "Males")
              ) } +
            # Optional p-value text
            {if(nrow(pvalues)!=0) geom_text(pvalues, mapping=aes(label=paste(rho, pvalues, sep= ",")),hjust =0, size = 3.5, fontface="italic", position = position_dodge(width = .1), check_overlap = FALSE)} 
          list3<-append(list(plt),list3, 0)
          name3 <- paste(variable,"_plots_", tissues[h], sep = "")
          list3names <- append(list3names,name3)
        }
      }
      names(list0)<-list0names
      names(list1)<-list1names
      names(list2)<-list2names
      names(list3)<-list3names
      names(list4)<-list4names
    }
    }
    return(list (liver=list0, stomach=list1, muscle=list2, inksac=list3, final_coefficients_results=list4 ))
}


#Calling Main Function. All arguments except remove.zeroes (default is set at False) are required. Users can also remove all zeroes and focus on only the detected concentrations or keep them.The user also has to choose between datasets_for_organic_compounds or datasets_for_trace_metals.The results are saved in temporal_comparison_results_per_tissue.
tissue_wise_temporal_variable_analysis <- comparing_years_per_tissue_per_variable(datasets_for_organic_compounds,remove.zeroes = FALSE)

#Below code saves multiple plots into individual PNG files. It loops through the list of plots and and for each plot it extracts the tissue name, and the plots for that tissue then saves them in their respective tissue folders.
save_graphs <- function(graph_list) {
  # Extract pollutant types from the first available plot
  plot_obj <- graph_list[["liver"]][[1]]
  plot_data <- plot_obj$data
  pollutant_types <- unique(plot_data$pollutants)
  
  tissues <- c('liver', 'stomach', 'muscle', 'inksac')  # Define order of tissues
  
  # Iterate through tissues in the specified order
  for (tissue_name in tissues) {
    
    # Check if the tissue exists in the graph list
    if (!(tissue_name %in% names(graph_list))) {
      next  # Skip if the tissue does not have any plots
    }
    
    # Define the folder where you want to save the PNG files for each tissue
    if (any(grepl("Metal", pollutant_types))) {
      output_folder <- file.path("/Users/mrnobody/Documents/GitHub/Squid_Fest/Squid_Concentration_Analysis/3-Data_Mining/Data_mining_plots/tissue-wise_temporal_variable_analysis/Trace_metals", tissue_name)
    } else {
      output_folder <- file.path("/Users/mrnobody/Documents/GitHub/Squid_Fest/Squid_Concentration_Analysis/3-Data_Mining/Data_mining_plots/tissue-wise_temporal_variable_analysis/Organic_compounds", tissue_name)
    }
    
    # Create the output folder if it doesn't exist
    if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
    
    tissue_sublist <- graph_list[[tissue_name]]
    
    # Loop through each plot for the current tissue
    for (i in seq_along(tissue_sublist)) {
      plot_name <- names(tissue_sublist)[i]
      plot_object <- tissue_sublist[[i]]
      
      cat("\n----------\n")
      cat("Checking:", tissue_name, "_", plot_name, "\n")
      
      # Construct output path for each plot
      output_path <- file.path(output_folder, paste0(tissue_name, "_", plot_name, ".png"))
      
      # Ensure any open devices are closed before starting new plot saving
      if (!is.null(dev.list())) {
        dev.off()  # Close all open devices
      }
      
      # Try saving the plot
      tryCatch({
        png(output_path, width = 1400, height = 800)
        grid::grid.draw(ggplotGrob(plot_object))
        cat("Saved:", output_path, "\n")
        dev.off()  # Ensure device is closed
      }, error = function(e) {
        cat("⚠️ Error in", plot_name, ":", e$message, "\n")
        if (!is.null(dev.list()) && length(dev.list()) > 0) dev.off()  # Force-close if error left device open
      })
    }
  }
}
#Calling save_graphs function:
save_graphs(tissue_wise_temporal_variable_analysis)
