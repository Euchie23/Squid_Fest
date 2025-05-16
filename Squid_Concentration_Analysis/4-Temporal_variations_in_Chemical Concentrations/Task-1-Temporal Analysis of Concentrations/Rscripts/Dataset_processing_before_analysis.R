
#Data Processor Function before doing analysis
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
        sk=paste('0',substring(x[4],1,))
        sk<- gsub(" ", "",  sk)
        x[4] <- gsub(substring(x[4],1,), sk,  x[4])
      }
      return(x)
    }
    
    # Apply the 'ID_num_modifier' function and remove "." from the 'Area' column
    data <- as.data.frame(t(apply(data, MARGIN = 1, ID_num_modifier)))
    data$ID_num <- gsub("\\.", "", data$ID_num)
    
    if(keep_LOQ_values == FALSE){
      cat("The LOQ values were replaced with 0.\n")
    # Replace "BLOQ" values with 0 in the relevant columns (16:25)
    data[, 16:25] <- lapply(data[, 16:25], gsub, pattern = ".*BLOQ.*", replacement = 0)
    data[,c(16:25)] <- lapply(data[,c(16:25)], gsub, pattern = ".*BB.*", replacement = 0)
    data[,c(16:25)] <- lapply(data[,c(16:25)], gsub, pattern = "^0$", replacement = 0)
    data[,c(16:25)]  <- lapply(data[,c(16:25)] , gsub, pattern = "N/A", replacement = 0)
    return(data)
    
    }else{
      # Removing "BLOQ" and "BB" values in the relevant columns (16:25)
      data[, 16:25] <- lapply(data[, 16:25], gsub, pattern = " BLOQ", replacement = "")
      data[,c(16:25)] <- lapply(data[,c(16:25)], gsub, pattern = " BB", replacement = "")
      data[,c(16:25)] <- lapply(data[,c(16:25)], gsub, pattern = "^0$", replacement = 0)
      data[,c(16:25)]  <- lapply(data[,c(16:25)] , gsub, pattern = "N/A", replacement = 0)
      
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
          
          # Process the data
          data[, 16:25] <- lapply(data[, 16:25], function(column) {
            # Replace " BLOQ" and " BB" with empty strings
            cleaned_column <- gsub(" BLOQ", "", column)
            cleaned_column <- gsub(" BB", "", cleaned_column)
            
            # Convert to numeric
            numeric_column <- as.numeric(cleaned_column)
            
            # Multiply by the user input
            if (is.numeric(multiplier)){
              result_column <- numeric_column * multiplier
              return(result_column)
            }else{
              return (numeric_column)
            }
            
          })
          
          # Return the updated dataset
          return(data)
        }
        data <-process_data(data, user_choice)
        return(data)
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
    
    # Apply the 'dcc' function and remove "." from the 'Area' column
    data <- as.data.frame(t(apply(data, MARGIN = 1, ID_num_modifier)))
    data$ID_num <- gsub("\\.", "", data$ID_num)
    
    if(keep_LOQ_values == FALSE){
      cat("The LOQ values were replaced with 0.\n")
      # Replace "BLOQ", "N/A", and "0" values with 0 in the relevant columns (16:19)
      data[, 16:19] <- lapply(data[, 16:19], gsub, pattern = ".*BLOQ.*", replacement = 0)
      data[, 16:19] <- lapply(data[, 16:19], gsub, pattern = "N/A", replacement = 0)
      data[, 16:19] <- lapply(data[, 16:19], gsub, pattern = "^0$", replacement = 0)
      return(data)
      
    }else{
      # Removing "BLOQ" and  values in the relevant columns (16:19)
      data[, 16:19] <- lapply(data[, 16:19], gsub, pattern = " BLOQ", replacement = "")
      data[, 16:19] <- lapply(data[, 16:19], gsub, pattern = "N/A", replacement = 0)
      data[, 16:19] <- lapply(data[, 16:19], gsub, pattern = "^0$", replacement = 0)
      
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
        
        # Process the data
        data[, 16:19] <- lapply(data[, 16:19], function(column) {
          # Replace " BLOQ" and " BB" with empty strings
          cleaned_column <- gsub(" BLOQ", "", column)
          #cleaned_column <- gsub(" BB", "", cleaned_column)
          
          # Convert to numeric
          numeric_column <- as.numeric(cleaned_column)
          
          # Multiply by the user input
          if (is.numeric(multiplier)){
            result_column <- numeric_column * multiplier
            return(result_column)
          }else{
            return (numeric_column)
          }
          
        })
        
        # Return the updated dataset
        return(data)
      }
      data <-process_data(data, user_choice)
      return(data)
    }
  }
} 


# NOTE FOR THE BELOW FUNCTIONS:
#If you choose FALSE for "keep_LOQ_values", it will be replaced with 0, then removed later in the data analysis based on the user's input, when asked if you want to to keep the original LOQ values if you choose "Yes" it keeps the original values, if no it asks for a multiplier. This is because some researchers use half or maybe quarter of the LOQ values when running the analysis.

# Data Processing for Heavy Metals dataset
heavymetals_data <- read.csv("Final_Results_From_Task 1/Final_HMresults_mgkg.csv", header = TRUE)
processed_hm_data <- process_dataset(heavymetals_data, keep_LOQ_values = TRUE) 

# Data Processing for Organic Compounds dataset
oc_data <- read.csv("Final_Results_From_Task 1/Final_OCresults_mgkg.csv", header = TRUE)
processed_oc_data <- process_dataset(oc_data, keep_LOQ_values = TRUE)