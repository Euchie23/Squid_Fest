#LOADING LIBRARIES----
library(dplyr) #For data manipulation
library(dunn.test) #For the "dunn.test()" function. Used if there are <3 but >1 groups to compare.
library(FSA) #For the "dunnTest()" function. Used if there are >=3 groups to compare.
library(ggplot2)  #For plotting graphs
library(grid) #For used for "textGrob" to create graphical text objects
library(gridExtra) #For customizing the appearance of tables using "Tablegrob"
library(gtable) #For adding new rows to a gtable.
library(stringr) #For String Manipulation
library(tibble) #For creating tibbles
library(tidyr) #For a collection of R packages used for data manipulation and visualization.





#Data Processor Function before doing analysis
#It is recommended that the 'Keep_LOQ_values' argument remain as FALSE to remove all values below blank control (BB) or LOQ (BLOQ) to get a more accurate figure for the number of outliers within the dataset, particularly for the data distribution otherwise it will also classify values that are BB or BLOQ  as outliers leading to double counting.
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
organiccompounds_data <- read.csv("Squid_Conc_Anly/3-Data_Min/Data/Prepd_Data/Fnl_0res_mgkg.csv", header = TRUE)
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


#Helper function to activate the icons for organic compound pollutants in final graph,  It first checks if the icons exist and if they do then they are loaded unto the graph: 
markdown_function_for_OC_icons <- function(x) {
  # Use file.path() for a safe file path
  icon_path <- file.path("Squid_Conc_Anly/3-Data_Min/OCicons", paste0(x[1], ".png"))
  
  # Construct the HTML string
  y <- paste0(" <img src='", icon_path, "' width='17'/>")
  
  return(y)
}



#Helper function to activate the icons for trace metal pollutants in final graph, It first checks if the icons exist and if they do then they are loaded unto the graph: 
markdown_function_for_TM_icons <- function(x) {
  # Create file paths correctly
  file1 <- file.path("Squid_Conc_Anly/3-Data_Min/TMicons", paste0(x[1], ".png"))
  file2 <- file.path("Squid_Conc_Anly/3-Data_Min/TMicons", paste0(x[1], "1.png"))
  file3 <- file.path("Squid_Conc_Anly/3-Data_Min/TMicons", paste0(x[1], "2.png"))
  
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



# Helper function to calculate and store y_axis_upper_limit for the different tissues affected by each pollutant.
get_y_axis_upper_limit <- function (dataset){
  
  # Create an empty data frame to store the results of the coefficients. final  y_axis_dataframe_final where coefficient1 is accumulated
  y_axis_dataframe_final <- data.frame(matrix(ncol=3, nrow = 0))
  colnames( y_axis_dataframe_final) <- c('Tissue','pollutant','y_axis_upper_limit')
  
  # Temporary data frame to store coefficients for the current iteration
  y_axis_dataframe <- data.frame(matrix(ncol=3, nrow = 0))
  colnames( y_axis_dataframe) <- colnames( y_axis_dataframe_final)
  
  
  #changing class of concentration values from character to numeric
  dataset$concentrations <- as.numeric(dataset$concentrations)
  
  
  #changing class of concentration values from character to numeric
  dataset$Tissue <- as.character(dataset$Tissue)
  
  # Handle y_upper_limit
  concentrations <- dataset$concentrations
  ycoord <- ifelse(any(!is.na(concentrations)), max(concentrations, na.rm = TRUE), 1)
  
    y_upper_limit <-ycoord # Set the upper limit for the y-axis based on the smallest outlier
    y_axis_dataframe[1,1] <- unique(dataset$Tissue)
    y_axis_dataframe[1,2] <- unique(dataset$pollutants)
    y_axis_dataframe[1,3] <- y_upper_limit
    y_axis_dataframe_final <- rbind( y_axis_dataframe_final,y_axis_dataframe)
  # Return the final dataframe with the coefficients
  return ( y_axis_dataframe_final)
}


# Main Function: temporal_comparisons_between_genders
# This function performs a series of data processing steps, including statistical analysis, and plotting, to investigate the relationship between various environmental concentrations and biological variables (such as tissue types and year). The analysis uses gender as a categorical variable, which is then used to find out whether gender has an effect on concentration levels. This function generates boxplots and tables of statistical comparison results to show how pollutant concentration values significantly differ between genders between and within years and includes options for y axis modification, zero values, and performing inferential statistics (e.g., kruskal wallis, ANOVA, student-t test etc..) to assess the comparisons and make clear plots. These plots are generated using `ggplot2`, and results are returned in a list.
temporal_comparisons_between_genders <- function (data_list, variable, remove.zeroes = FALSE){
  
  dataset_with_numerical_values <- data_list$dataset_with_numerical_values

# Step 1: Initialize empty lists for storing results----
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
# Step 2: Determine pollutant range and subset the dataset based on its presence----
if (grepl("Metal", colnames(dataset_with_numerical_values)[16])) {
  # Trace metals subset
  range <- colnames(dataset_with_numerical_values[16:25])
  icons_tm <- apply(TMicons, 1, markdown_function_for_TM_icons)
  icons_markdown <- icons_tm
  range_name <- 'Metal_A:Metal_B'
  number_range <- 16:25
} else {
  # Organic compounds subset
  icons_oc <- apply(OCicons, 1, markdown_function_for_OC_icons)
  icons_markdown <- icons_oc
  range <- colnames(dataset_with_numerical_values[16:19])
  range_name <- 'Organic_A:Organic_B'
  number_range <- 16:19
}


# Re-categorizing gender classifications into words instead of numbers
dataset_with_numerical_values$Gender[dataset_with_numerical_values$Gender=="0"]<-"Female"
dataset_with_numerical_values$Gender[dataset_with_numerical_values$Gender=="1"]<-"Male"


# streamlining dataframe for efficiency change back to 26 from 30 for Trace metals
subsetted_datatset1 <-dataset_with_numerical_values[,c(number_range,3, 6, 7)]

# Extracting unique factor levels for sizes, tissues, and years
years <- levels(factor(subsetted_datatset1$Year)) # Size categories
tissues <- levels(factor(subsetted_datatset1$Tissue, levels = c("liver", "stomach", "muscle", "inksac"))) # Tissue categories
genders <- levels(factor(subsetted_datatset1$Gender)) # Gender categories


#Creating index for while loop
h<-1

#Initializing while loop
while(h != length(number_range)+1){
  
  #Creating dynamic variable to indicate pollutant being processed
  pollutant <-colnames(subsetted_datatset1)[h] 
  
  #Initializing empty dataframe for statistical processing later
  y_axis_upper_limit_accumulated <- data.frame(matrix(ncol=3, nrow = 0)) #for y axis upper limit when plotting.
  
  results_gender_between_years <- data.frame(matrix(ncol=9, nrow = 0))
  results_gender_within_years <- data.frame(matrix(ncol=10, nrow = 0))
  normality_test_results_accumulated <- data.frame(matrix(ncol=4, nrow = 0)) 
  colnames(normality_test_results_accumulated) <- c('Gender', 'Tissue', 'Year', 'results')
  colnames(results_gender_within_years) <- c('Tissue ','Year ','Normality_test ','Test_stat ', 'Degrees of Freedom ','pvalues ','Mean_Female\nsymbol = (●) ','Mean_Male\n symbol = (●) ','Median_Female\nsymbol = (——) ', 'Median_Male\nsymbol = (——) ') 
  colnames(results_gender_between_years) <- c('Gender','Tissue','Norm_test\n(Shapiro_Wilks)','Chi_sq/F_stat','DegFree', 'P_val','Mean_sq','Sum_of_Squares', 'group_diff\n(TukeyHSD/Dunns_test)')
  accumulated_results_GBY <- data.frame(matrix(ncol=9, nrow = 0), check.names = FALSE)
  accumulated_results_GWY <- data.frame(matrix(ncol=10, nrow = 0), check.names = FALSE)
  colnames(accumulated_results_GBY) <- colnames(results_gender_between_years)
  colnames(accumulated_results_GWY) <- colnames(results_gender_within_years)
  
  #Tissue order set to be applied to Tissue column for plotting later
  custom_order <- c("liver", "stomach", "muscle", "inksac")
  
  # Step 3: Sub-setting data for each pollutant for further statistical processing later----
  if(remove.zeroes==FALSE){
    subsetted_datatset_long<- subsetted_datatset1 %>% pivot_longer(paste(pollutant), names_to = "pollutants", values_to = "concentrations")%>%pivot_longer (Gender, names_to = "vars", values_to = "Gender")%>%
      mutate(concentrations = as.numeric(concentrations),
             Tissue = factor(Tissue, levels = custom_order),
             Year = factor(Year),
             Gender = factor(Gender)
      )
  }else{
    subsetted_datatset_long<- subsetted_datatset1 %>%  pivot_longer(paste(pollutant), names_to = "pollutants", values_to = "concentrations")%>%pivot_longer (Gender, names_to = "vars", values_to = "Gender")%>%
      mutate(concentrations = as.numeric(concentrations),
             Tissue = factor(Tissue, levels = custom_order),
             Year = factor(Year),
             Gender = factor(Gender)
      ) %>%subset(concentrations !=0)
  }
  
  
  # Step 4: Nested for-loops through subsetted datasets per pollutants for in-depth statistical analysis to find group differences for gender between years (GBY)----
  for(i in 1:length(tissues)) { #sub-setting per tissue for each pollutant
    normality_test_result <- data.frame(matrix(ncol=4, nrow = 0)) 
    colnames(normality_test_result) <- c('Gender', 'Tissue', 'Year', 'results')
    subsetted_datatset_long_by_tissue<-subsetted_datatset_long %>% group_by(pollutants) %>% subset(Tissue == tissues[i])
    if(nrow(subsetted_datatset_long_by_tissue)==0){
      results_gender_between_years[1,1] <- "Both Genders"
      results_gender_between_years[1,2] <- tissues[i]
      results_gender_between_years[1,3] <- 'Not enough observations'
      results_gender_between_years[1,4] <- NA
      results_gender_between_years[1,5] <- NA
      results_gender_between_years[1,6] <- NA
      results_gender_between_years[1,7] <- NA
      results_gender_between_years[1,8] <-NA
      results_gender_between_years[1,9] <-NA
      accumulated_results_GBY <- rbind(accumulated_results_GBY, results_gender_between_years)
    }else{
     
      #Function used to get y_axis_upper_limit from subsetted_datatset_long_by_tissue
       y_axis_upper_limit <- get_y_axis_upper_limit(subsetted_datatset_long_by_tissue)
       
       # Results from the get_y_axis_upper_limit function are accumulated here to be used later for plotting
      y_axis_upper_limit_accumulated <- rbind(y_axis_upper_limit_accumulated, y_axis_upper_limit)
    
      
      for(j in 1:length(genders)){#sub-setting per gender for each pollutant after sub-setting by tissue
        normality_test_results1 <- data.frame(matrix(ncol=4, nrow = 0)) 
        colnames(normality_test_results1) <- c('Gender', 'Tissue', 'Year', 'results')
        subsetted_tissue_datatset_by_gender<-as.data.frame(subsetted_datatset_long_by_tissue %>% group_by(pollutants) %>% subset(Gender == genders[j]))
        
        for(l in 1:length(years)){ #sub-setting per year for each pollutant after sub-setting by tissue and gender.
          subsetted_tissue_and_gender_dataset_per_year<-as.data.frame(subsetted_tissue_datatset_by_gender %>% group_by(pollutants) %>% subset(Year == years[l]))
          
# Below condition used to run normality test for dataset after sub-setting per year for each pollutant after sub-setting by tissue and gender.      
          if(nrow(subsetted_tissue_and_gender_dataset_per_year)>3 & all(subsetted_tissue_and_gender_dataset_per_year[-1,'concentrations'] == subsetted_tissue_and_gender_dataset_per_year[1,'concentrations'])==FALSE){
            shapiro_wilks_test_result <-(shapiro.test(subsetted_tissue_and_gender_dataset_per_year$concentrations)$p.value)
            normality_test_result[1,1] <- genders[j]
            normality_test_result[1,2] <- tissues[i]
            normality_test_result[1,3] <- years[l]
            if(shapiro_wilks_test_result<0.05){
              normality_test_result[1,4] <- 'fail' 
            }else{
              normality_test_result[1,4] <- 'pass' 
            }
            normality_test_results1 <- rbind(normality_test_results1, normality_test_result)
          }else{
            normality_test_result[1,1] <- genders[j]
            normality_test_result[1,2] <- tissues[i]
            normality_test_result[1,3] <- years[l]
            normality_test_result[1,4] <- '<3 Obs' 
            normality_test_results1 <- rbind(normality_test_results1, normality_test_result)
          }
        }
        normality_test_results_accumulated <- rbind(normality_test_results_accumulated, normality_test_results1)
        
        # Below condition used to run the kruskal Wallis hypothesis test for group differences between gender temporally using the subsetted_tissue_datatset_by_gender. 
        if(nrow(normality_test_results1)!=0 & 'fail' %in% normality_test_results1[,'results']==TRUE & length(levels(factor(subsetted_tissue_datatset_by_gender[,'Year'])))>1){
          shapiro_wilks_test_status<-'Fail'
          kw.model <- kruskal.test(concentrations ~ Year, data = subsetted_tissue_datatset_by_gender)
          kw_pval <- signif((kw.model$p.value),1)
          results_gender_between_years[1,1] <- genders[j]
          results_gender_between_years[1,2] <- tissues[i]
          results_gender_between_years[1,3] <- shapiro_wilks_test_status
          results_gender_between_years[1,4] <- paste('chisq=', signif(kw.model$statistic[[1]],3), sep = "")
          results_gender_between_years[1,5] <- kw.model$parameter[[1]]
          results_gender_between_years[1,6] <- kw.model$p.value
          results_gender_between_years[1,7] <- NA
          results_gender_between_years[1,8] <- NA
          
          #below conditon checks results of kruskal wallis test to know whether to run post-hoc dunns test for more detailed information on which groups differ.
          if(is.nan(kw_pval)==FALSE|kw_pval!=0 & kw_pval< 0.05){
            if(length(levels(factor((subsetted_tissue_datatset_by_gender$Year)))) < 3){
              shapiro_wilks_test_status <- data.frame(matrix(ncol=4, nrow = 0))
              colnames(shapiro_wilks_test_status) <- c('Comparison', 'Z','P.unadj','P.adj')
              post_hoc_test_coefficients<-dunn.test (subsetted_tissue_datatset_by_gender$concentrations, subsetted_tissue_datatset_by_gender$Year, method=p.adjustment.methods, kw=TRUE)
              shapiro_wilks_test_status[1,1] <- post_hoc_test_coefficients$comparisons
              shapiro_wilks_test_status[1,2] <- post_hoc_test_coefficients$Z
              shapiro_wilks_test_status[1,3] <- post_hoc_test_coefficients$P
              shapiro_wilks_test_status[1,4] <- post_hoc_test_coefficients$P.adjusted
              post_hoc_test_coefficients2 <- shapiro_wilks_test_status
            }else{
              post_hoc_test_coefficients <-dunnTest(concentrations ~ Year,data=subsetted_tissue_datatset_by_gender,method="bh")
              post_hoc_test_coefficients2 <- post_hoc_test_coefficients$res
            }
            post_hoc_test_coefficients2[['P.adj']] <- signif(as.numeric(post_hoc_test_coefficients2[['P.adj']]), 2)
            post_hoc_test_coefficients3 <- replace(post_hoc_test_coefficients2, is.na(post_hoc_test_coefficients2), 1)
            post_hoc_test_coefficients4 <- data.frame(post_hoc_test_coefficients3, pv=format(post_hoc_test_coefficients3$P.adj, scientific = FALSE))
            
            # Below function used to modify string for for kruskal wallis group difference result to place in table later.
            comparisons_for_kw_test <- function(x){
              vector<-c()
              if(x[5]<0.01){
                vector<-append(vector, paste(x[1],'<0.01', sep = ""))
              }else if(x[5]>0.01 & x[4]<0.05){
                vector<-append(vector, paste(x[1],'=',x[4], sep = ""))
              }else{
                vector<-append(vector, paste(x[1],'=n.s', sep = ""))
              }
              return(vector)
            }
            comparisons_results_kw <-apply(post_hoc_test_coefficients4, 1, comparisons_for_kw_test)
            results_gender_between_years[1,9] <-paste(comparisons_results_kw, collapse = "::") 
          }else{
            results_gender_between_years[1,9] <-paste("No difference")
          }
          accumulated_results_GBY <- rbind(accumulated_results_GBY, results_gender_between_years)
          
          # Below condition used to run the anova hypothesis test for group differences between gender temporally using the subsetted_tissue_datatset_by_gender.
        }else if (nrow(normality_test_results1)!=0 & 'pass' %in% normality_test_results1[,'results']==TRUE & 'fail' %in% normality_test_results1[,'results']==FALSE & length(levels(factor(subsetted_tissue_datatset_by_gender[,'Year'])))>1){
          shapiro_wilks_test_status<-'pass'
          aov.model <- aov(concentrations ~ Year, data = subsetted_tissue_datatset_by_gender)
          anova_pval <- signif((anova(aov.model)[1,'Pr(>F)']),3)
          results_gender_between_years[1,1] <- genders[j]
          results_gender_between_years[1,2] <- tissues[i]
          results_gender_between_years[1,3] <- shapiro_wilks_test_status
          results_gender_between_years[1,4] <- paste('f=', signif(anova(aov.model)[1,'F value'],3), sep = "")
          results_gender_between_years[1,5] <- anova(aov.model)[1,'Df']
          results_gender_between_years[1,6] <- anova(aov.model)[1,'Pr(>F)']
          results_gender_between_years[1,7] <- anova(aov.model)[1,'Mean Sq']
          results_gender_between_years[1,8] <- anova(aov.model)[1,'Sum Sq']
         
          #below conditon checks results of Anova test to know whether to run post-hoc Tukey's HSD test for more detailed information on which groups differ.
          if(is.nan(anova_pval)==FALSE & anova_pval< 0.05){
            post_hoc_test_coefficients <-TukeyHSD(aov.model)
            post_hoc_test_coefficients2 <- data.frame(compare = row.names(post_hoc_test_coefficients$Year), post_hoc_test_coefficients$Year)
            post_hoc_test_coefficients2$p.adj <- signif(as.numeric(post_hoc_test_coefficients2$p.adj), 2)
            post_hoc_test_coefficients3 <- replace(post_hoc_test_coefficients2, is.na(post_hoc_test_coefficients2), 1)
            
            # Below function used to modify string for Anova group difference result to place in table later.
            comparisons_for_aov_test <- function(x){
              vector<-c()
              if(x[5]<0.01){
                vector<-append(vector, paste(x[1],'<0.01', sep = ""))
              }else if(x[5]<0.05){
                vector<-append(vector, paste(x[1],'=',x[5], sep = ""))
              }else{
                vector<-append(vector, paste(x[1],'=n.s', sep = ""))
              }
              return(vector)
            }
            comparisons_results_aov <-apply(post_hoc_test_coefficients3, 1, comparisons_for_aov_test)
            results_gender_between_years[1,9] <-paste(comparisons_results_aov, collapse = "::") 
          }else{
            results_gender_between_years[1,9] <-paste("No difference")
          }
          accumulated_results_GBY <- rbind(accumulated_results_GBY, results_gender_between_years)
          
          # Below condition used to create group difference entry for  subsetted_tissue_datatset_by_gender with less than data entries.
        }else if ('pass' %in% normality_test_results1[,'results']==TRUE|'fail' %in% normality_test_results1[,'results']==TRUE & length(levels(factor(subsetted_tissue_datatset_by_gender[,'Year'])))<2){
          results_gender_between_years[1,1] <- genders[j]
          results_gender_between_years[1,2] <- tissues[i]
          results_gender_between_years[1,3] <- 'Few observations & no difference in Years'
          results_gender_between_years[1,4] <- NA
          results_gender_between_years[1,5] <- NA
          results_gender_between_years[1,6] <- NA
          results_gender_between_years[1,7] <- NA
          results_gender_between_years[1,8] <- NA
          results_gender_between_years[1,9] <- NA
          accumulated_results_GBY <- rbind(accumulated_results_GBY, results_gender_between_years)
        }else{
          results_gender_between_years[1,1] <- genders[j]
          results_gender_between_years[1,2] <- tissues[i]
          results_gender_between_years[1,3] <- "Not enough observations"
          results_gender_between_years[1,4] <- NA
          results_gender_between_years[1,5] <- NA
          results_gender_between_years[1,6] <- NA
          results_gender_between_years[1,7] <- NA
          results_gender_between_years[1,8] <-NA
          results_gender_between_years[1,9] <-NA
          accumulated_results_GBY <- rbind(accumulated_results_GBY, results_gender_between_years)
        }
      }
    }
    #filtering the acucmulated normality test results per tissue.
    filtered_normality_test_results_by_tissues <- filter(normality_test_results_accumulated, Tissue== tissues[i])
    cat("\n")
    cat("\n----------\n")
    print(pollutant)
    print(filtered_normality_test_results_by_tissues)
    
    # Step 5: for-loop through subsetted datasets per pollutants for in-depth statistical analysis to find group differences for gender within years (GWY)----
    for(k in 1:length(years)){
      if(nrow(subsetted_datatset_long_by_tissue)==0){
        results_gender_within_years[1,1] <- tissues[i]
        results_gender_within_years[1,2] <- years[k]
        results_gender_within_years[1,3] <- 'Not enough Observations'
        results_gender_within_years[1,4] <- NA
        results_gender_within_years[1,5] <- NA
        results_gender_within_years[1,6] <- NA
        results_gender_within_years[1,7] <- NA
        results_gender_within_years[1,8] <-NA
        results_gender_within_years[1,9] <- NA
        results_gender_within_years[1,10] <-NA
        accumulated_results_GWY <- rbind(accumulated_results_GWY, results_gender_within_years)
      }else{
        subsetted_tissue_and_gender_dataset_per_year<-as.data.frame(subsetted_datatset_long_by_tissue %>% group_by(pollutants) %>% subset(Year == years[k]))
        subsetted_tissue_and_gender_dataset_per_year_females<-as.data.frame(subsetted_tissue_and_gender_dataset_per_year %>% group_by(pollutants) %>% subset(Gender == genders[1]))
        subsetted_tissue_and_gender_dataset_per_year_males<-as.data.frame(subsetted_tissue_and_gender_dataset_per_year %>% group_by(pollutants) %>% subset(Gender == genders[2]))
        
        #Below condition activates if all values (has to be more than 3) within the Gender and concentration columns of the subsetted_tissue_and_gender_dataset_per_year are not the same (indicated with false) then they go on to the next step but need to wait until the second condition is passed.
        if(all(subsetted_tissue_and_gender_dataset_per_year[-1,'Gender'] == subsetted_tissue_and_gender_dataset_per_year[1,'Gender'])==FALSE & nrow(subsetted_tissue_and_gender_dataset_per_year)>3 & all(subsetted_tissue_and_gender_dataset_per_year[-1,'concentrations'] == subsetted_tissue_and_gender_dataset_per_year[1,'concentrations'])==FALSE){
          
          #Below if condition is the second condition, which activates to run tests for the normally distributed data from subsetted_tissue_and_gender_dataset_per_year
          if(nrow(filtered_normality_test_results_by_tissues)!=0 & 'pass' %in% filtered_normality_test_results_by_tissues[,'results']==TRUE & 'fail' %in% filtered_normality_test_results_by_tissues[,'results']==FALSE){
         
            shapiro_wilks_test_status <- 'Pass'
            if (years[k]=='2019'){#2019 is subsetted because only females were caught in 2019 so we will be using it to tests for differences within years
              subsetted_tissue_datatset_by_year_for_2019<-as.data.frame(subsetted_datatset_long_by_tissue%>% group_by(pollutants) %>% subset(Year == years[k])) 
              subsetted_tissue_datatset_by_year_for_2019_females<-as.data.frame(subsetted_tissue_datatset_by_year_for_2019 %>% group_by(pollutants) %>% subset(Gender == genders[1])) 
              results_gender_within_years[1,1] <- tissues[i]
              results_gender_within_years[1,2] <- years[k]
              results_gender_within_years[1,3] <- shapiro_wilks_test_status
              results_gender_within_years[1,4] <- NA
              results_gender_within_years[1,5] <- NA
              results_gender_within_years[1,6] <- NA
              results_gender_within_years[1,7] <- mean(subsetted_tissue_datatset_by_year_for_2019_females$concentrations)
              results_gender_within_years[1,8] <-NA
              results_gender_within_years[1,9] <-median(subsetted_tissue_datatset_by_year_for_2019_females$concentrations)
              results_gender_within_years[1,10] <-NA
              accumulated_results_GWY <- rbind(accumulated_results_GWY, results_gender_within_years)
            }else{#This focuses on the other 2 years 2020 and 2021 since they had both male and femal squids
              subsetted_tissue_datatset_by_year<-as.data.frame(subsetted_datatset_long_by_tissue %>% group_by(pollutants) %>% subset(Year == years[k])) 
              subsetted_tissue_datatset_by_year_for_females<-as.data.frame(subsetted_tissue_datatset_by_year %>% group_by(pollutants) %>% subset(Gender == genders[1]))
              subsetted_tissue_datatset_by_year_for_males<-as.data.frame(subsetted_tissue_datatset_by_year %>% group_by(pollutants) %>% subset(Gender == genders[2]))
       
               #Below if condition activates if there aren't enough factors/ data to run the hypothesis testing if there are enough factors and data then it runs a t.test to determine if there is a significant  difference between the means of the male and female groups.
              if(nrow(subsetted_tissue_datatset_by_year)<2|length(which(subsetted_tissue_datatset_by_year$Gender == genders[1]))<2|length(which(subsetted_tissue_datatset_by_year$Gender == genders[2]))<2|all(subsetted_tissue_datatset_by_year[-1,'concentrations'] == subsetted_tissue_datatset_by_year[1,'concentrations'])){
                results_gender_within_years[1,1] <- tissues[i]
                results_gender_within_years[1,2] <- years[k]
                results_gender_within_years[1,3] <- shapiro_wilks_test_status
                results_gender_within_years[1,4] <- NA
                results_gender_within_years[1,5] <- NA
                results_gender_within_years[1,6] <- NA
                results_gender_within_years[1,7] <- mean(subsetted_tissue_datatset_by_year_for_females$concentrations)
                results_gender_within_years[1,8] <-mean(subsetted_tissue_datatset_by_year_for_males$concentrations)
                results_gender_within_years[1,9] <- median(subsetted_tissue_datatset_by_year_for_females$concentrations)
                results_gender_within_years[1,10] <-median(subsetted_tissue_datatset_by_year_for_males$concentrations)
                accumulated_results_GWY <- rbind(accumulated_results_GWY, results_gender_within_years)
              }else{
                tt.model <- t.test(concentrations ~ Gender, data = subsetted_tissue_datatset_by_year)
                results_gender_within_years[1,1] <- tissues[i]
                results_gender_within_years[1,2] <- years[k]
                results_gender_within_years[1,3] <- shapiro_wilks_test_status
                results_gender_within_years[1,4] <- paste('t=', signif((tt.model$statistic[["t"]]),3), sep = "")
                results_gender_within_years[1,5] <- tt.model$parameter[["df"]]
                results_gender_within_years[1,6] <- signif((tt.model$p.value),3)
                results_gender_within_years[1,7] <- tt.model$estimate[[1]]
                results_gender_within_years[1,8] <-tt.model$estimate[[2]]
                results_gender_within_years[1,9] <- median(subsetted_tissue_datatset_by_year_for_females$concentrations)
                results_gender_within_years[1,10] <-median(subsetted_tissue_datatset_by_year_for_males$concentrations)
                accumulated_results_GWY <- rbind(accumulated_results_GWY, results_gender_within_years)
              }
            }
            #Below else if condition checks the normality test results and activates to run tests for non-normally distributed data from the subsetted_tissue_and_gender_dataset_per_year
          }else if (nrow(filtered_normality_test_results_by_tissues)!=0 & 'fail' %in% filtered_normality_test_results_by_tissues[,'results']==TRUE){
            shapiro_wilks_test_status <- 'fail'
            if (years[k]=='2019'){
              subsetted_tissue_datatset_by_year_for_2019<-as.data.frame(subsetted_datatset_long_by_tissue %>% group_by(pollutants) %>% subset(Year == years[k])) 
              subsetted_tissue_datatset_by_year_for_2019_females<-as.data.frame(subsetted_tissue_datatset_by_year_for_2019 %>% group_by(pollutants) %>% subset(Gender == genders[1])) 
              results_gender_within_years[1,1] <- tissues[i]
              results_gender_within_years[1,2] <- years[k]
              results_gender_within_years[1,3] <- shapiro_wilks_test_status
              results_gender_within_years[1,4] <- NA
              results_gender_within_years[1,5] <- NA
              results_gender_within_years[1,6] <- NA
              results_gender_within_years[1,7] <- mean(subsetted_tissue_datatset_by_year_for_2019_females$concentrations)
              results_gender_within_years[1,8] <-NA
              results_gender_within_years[1,9] <-median(subsetted_tissue_datatset_by_year_for_2019_females$concentrations)
              results_gender_within_years[1,10] <-NA
              accumulated_results_GWY <- rbind(accumulated_results_GWY, results_gender_within_years)
            }else{
              subsetted_tissue_datatset_by_year<-as.data.frame(subsetted_datatset_long_by_tissue %>% group_by(pollutants) %>% subset(Year == years[k])) 
              subsetted_tissue_datatset_by_year_for_females<-as.data.frame(subsetted_tissue_datatset_by_year %>% group_by(pollutants) %>% subset(Gender == genders[1]))
              subsetted_tissue_datatset_by_year_for_males<-as.data.frame(subsetted_tissue_datatset_by_year %>% group_by(pollutants) %>% subset(Gender == genders[2]))
              
              #Below if condition activates if there aren't enough factors/ data to run the hypothesis testing if there are enough factors and data then it runs a wilcox.test(Mann-Whitney U) to check whether males and females differ in their distributions.
              if(nrow(subsetted_tissue_datatset_by_year)<2|length(which(subsetted_tissue_datatset_by_year$Gender == genders[1]))<2|length(which(subsetted_tissue_datatset_by_year$Gender == genders[2]))<2|all(subsetted_tissue_datatset_by_year[-1,'concentrations'] == subsetted_tissue_datatset_by_year[1,'concentrations'])){
                results_gender_within_years[1,1] <- tissues[i]
                results_gender_within_years[1,2] <- years[k]
                results_gender_within_years[1,3] <- shapiro_wilks_test_status
                results_gender_within_years[1,4] <- NA
                results_gender_within_years[1,5] <- NA
                results_gender_within_years[1,6] <- NA
                results_gender_within_years[1,7] <- mean(subsetted_tissue_datatset_by_year_for_females$concentrations)
                results_gender_within_years[1,8] <-mean(subsetted_tissue_datatset_by_year_for_males$concentrations)
                results_gender_within_years[1,9] <- median(subsetted_tissue_datatset_by_year_for_females$concentrations)
                results_gender_within_years[1,10] <-median(subsetted_tissue_datatset_by_year_for_males$concentrations)
                accumulated_results_GWY <- rbind(accumulated_results_GWY, results_gender_within_years)
              }else{
                mu.model <-wilcox.test(concentrations ~ Gender, data = subsetted_tissue_datatset_by_year, exact = FALSE)
                results_gender_within_years[1,1] <- tissues[i]
                results_gender_within_years[1,2] <- years[k]
                results_gender_within_years[1,3] <- shapiro_wilks_test_status
                results_gender_within_years[1,4] <- paste('W=', signif(mu.model$statistic[["W"]],3), sep = "")
                results_gender_within_years[1,5] <- NA
                results_gender_within_years[1,6] <- signif((mu.model$p.value),3)
                results_gender_within_years[1,7] <- mean(subsetted_tissue_datatset_by_year_for_females$concentrations)
                results_gender_within_years[1,8] <-mean(subsetted_tissue_datatset_by_year_for_males$concentrations)
                results_gender_within_years[1,9] <- median(subsetted_tissue_datatset_by_year_for_females$concentrations)
                results_gender_within_years[1,10] <-median(subsetted_tissue_datatset_by_year_for_males$concentrations)
                accumulated_results_GWY <- rbind(accumulated_results_GWY, results_gender_within_years)
              }
            }  
            #Below else condition does not check the normality test results and only activates when there is no data in subsetted_tissue_and_gender_dataset_per_year dataset. Hence no stats were performed. It records the tissue and year so that it will still be included in the data visualization later on. This is mainly for inksac in 2019.. since this tissue was not tested for that year.
          }else{
            results_gender_within_years[1,1] <- tissues[i]
            results_gender_within_years[1,2] <- years[k]
            results_gender_within_years[1,3] <- 'Not enough Observations'
            results_gender_within_years[1,4] <- NA
            results_gender_within_years[1,5] <- NA
            results_gender_within_years[1,6] <- NA
            results_gender_within_years[1,7] <- NA
            results_gender_within_years[1,8] <- NA
            results_gender_within_years[1,9] <- NA
            results_gender_within_years[1,10] <-NA
            accumulated_results_GWY <- rbind(accumulated_results_GWY, results_gender_within_years)
          }
          #Below else if condition activates if all values (has to be more than 3 values) for the Gender column within the subsetted_tissue_and_gender_dataset_per_year are the same (indicated with True. This is mainly geared at 2019 since only females were caught in that year). The values in concentration columns, however, are not the same (indicated with false) then it goes on to perform the stats and details for that subsetted dataset.
        }else if (all(subsetted_tissue_and_gender_dataset_per_year[-1,'Gender'] == subsetted_tissue_and_gender_dataset_per_year[1,'Gender'])==TRUE & nrow(subsetted_tissue_and_gender_dataset_per_year)>3 & all(subsetted_tissue_and_gender_dataset_per_year[-1,'concentrations'] == subsetted_tissue_and_gender_dataset_per_year[1,'concentrations'])==FALSE){
          results_gender_within_years[1,1] <- tissues[i]
          results_gender_within_years[1,2] <- years[k]
          results_gender_within_years[1,3] <- paste('Only Females')
          results_gender_within_years[1,4] <- "Only one group present"
          results_gender_within_years[1,5] <- NA
          results_gender_within_years[1,6] <- NA
          results_gender_within_years[1,7] <- mean(subsetted_tissue_and_gender_dataset_per_year_females$concentrations)
          results_gender_within_years[1,8] <-NA
          results_gender_within_years[1,9] <- median(subsetted_tissue_and_gender_dataset_per_year_females$concentrations)
          results_gender_within_years[1,10] <-NA
          accumulated_results_GWY <- rbind(accumulated_results_GWY, results_gender_within_years)
          #Below else if condition activates if there are less than 3 values within the subsetted_tissue_and_gender_dataset_per_year. It then goes on to perform the stats and record details for that limited subsetted dataset.
        }else{
          results_gender_within_years[1,1] <- tissues[i]
          results_gender_within_years[1,2] <- years[k]
          results_gender_within_years[1,3] <- 'Not enough observations'
          results_gender_within_years[1,4] <- NA
          results_gender_within_years[1,5] <- NA
          results_gender_within_years[1,6] <- NA
          results_gender_within_years[1,7] <- mean(subsetted_tissue_and_gender_dataset_per_year_females$concentrations)
          results_gender_within_years[1,8] <-mean(subsetted_tissue_and_gender_dataset_per_year_males$concentrations)
          results_gender_within_years[1,9] <- median(subsetted_tissue_and_gender_dataset_per_year_females$concentrations)
          results_gender_within_years[1,10] <-median(subsetted_tissue_and_gender_dataset_per_year_males$concentrations)
          accumulated_results_GWY <- rbind(accumulated_results_GWY, results_gender_within_years)
        }
      }
    }
  }
  
  #Below code save results Shapiro-Wilks normality test into list3 for viewing and double checking afterward. It dynamically saves the name of each dataset according to their respective pollutant.
  list3<-append(list(normality_test_results_accumulated),list3, 0)
  name3 <- paste(pollutant,"_Shapiro_Wilks_test_results", sep = "")
  list3names <- append(list3names,name3)
  
  
  # Step 6: Creating tables for data visualization using accumulated_results_GBY and accumulated_results_GWY datasets----
  
  #Below code creates tables (created as ) from the accumulated_results_GBY data for each pollutant and then saves the table to list1 with their respective name for viewing and double checking afterward.
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)),
                       base_size = 12,
                       padding = unit(c(2, 3), "mm"))
  cols1 <- c('Gender ','Tissue ','comments ','Test_statistics ','Degrees of Freedom', 'pvalues ','Mean_sq ','Sum of Squares', 'Group_differences ')
  colnames(accumulated_results_GBY) <- sapply(cols1, function(x) paste(strwrap(x, width = 12),  collapse="\n"))
  tbl1 <- tableGrob(accumulated_results_GBY, rows=NULL, theme=tt)
  title <- textGrob("Gender Between Years",gp=gpar(fontsize=15,fontface='bold'))
  padding <- unit(1.3,"mm")
  table <- gtable_add_rows(tbl1,heights = grobHeight(title) + padding,pos = 0)
  table <- gtable_add_grob(table, title, 1, 1, 1,-1, ncol(table))
  
  #Saving tables to list1
  list1<-append(list(table),list1, 0)
  name1 <- paste(pollutant,"_gender_between_years", sep = "")
  list1names <- append(list1names,name1)
  
  #Below code creates tables from the accumulated_results_GWY data for each pollutant and then saves the table to list2 with their respective name for viewing and double checking afterward.
  tt1 <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)),
                        base_size = 11,
                        padding = unit(c(1, 2), "mm"))
  cols2 <- c('Tissue ','Year ','comments ','Test_statistics ', 'Degrees of Freedom ','pvalues ','Mean_Female symbol=(●) ','Mean_Male symbol=(●) ','Median_Female symbol=(——) ', 'Median_Male symbol=(——) ')
  colnames(accumulated_results_GWY) <- sapply(cols2, function(x) paste(strwrap(x, width = 12),  collapse="\n"))
  tbl2 <- tableGrob(accumulated_results_GWY, rows=NULL, theme=tt1)
  title2 <- textGrob("Gender Within Years",gp=gpar(fontsize=15,fontface='bold'))
  padding <- unit(1.3,"mm")
  table2 <- gtable_add_rows(tbl2,heights = grobHeight(title2) + padding,pos = 0)
  table2 <- gtable_add_grob(table2, title2, 1, 1, 1,-1, ncol(table2))
  
  #Saving tables to list1
  list2<-append(list(table2),list2, 0)
  name2 <- paste(pollutant,"_gender_within_years", sep = "")
  list2names <- append(list2names,name2)
  
  #________________________________________________________________________
  #Step 7: Below code sets different y-axis ranges for each tissue in a faceted plot. It joins saved y-axis limits with a base template, makes sure the values are numbers and tissues are in a set order, then builds custom y-axis scales for each tissue using scale_y_continuous()----
  if(nrow(y_axis_upper_limit_accumulated)!=0){
    y_axis_upper_limit_accumulated_copy <- y_axis_upper_limit_accumulated
  }else{
    Tissue <- tissues[i]
    pollutants <- pollutant
    y_axis_upper_limit <- 0
    y_upper_limit_dataframe <- data.frame(Tissue, pollutants, y_axis_upper_limit)
    y_axis_upper_limit_accumulated_copy <- y_upper_limit_dataframe 
  }
  df_scales <- data.frame(
    Tissue = c("liver", "stomach", "muscle","inksac"),
    ymin = c(0, 0, 0, 0),
    ymax = c(NA),
    n = c(5, 5, 5,5))
  
  df_scales %<>% inner_join(y_axis_upper_limit_accumulated_copy, by= "Tissue") %>%
    mutate(ymax = coalesce(y_axis_upper_limit)) %>%select(Tissue, ymin, ymax, n)
  
  custom_order <- c("liver", "stomach", "muscle", "inksac")
  df_scales <- df_scales %>%
    mutate(
      ymin = as.numeric(ymin),
      ymax = as.numeric(ymax),
      n = as.numeric(n),
      Tissue = factor(Tissue, levels = custom_order)
    )
  
  df_scales <- split(df_scales, df_scales$Tissue)
  scales <- lapply(df_scales, function(x) {
    scale_y_continuous(
      limits = c(as.numeric(x$ymin[1]), as.numeric(x$ymax[1])),
      n.breaks = as.numeric(x$n[1])
    )
  })
  
#Saves full dataset (long format) to list 4.
  list4<-append(list(subsetted_datatset_long),list4, 0)
  name4 <- paste(pollutant,"_datasets_for_plotting", sep = "")
  list4names <- append(list4names,name4)
  
  # Step 8: Below code creates customized boxplots for pollutant concentrations across different tissues, years, and genders. It calculates boxplot stats (min, 25th percentile, median, mean, 75th percentile, max) for each group (Tissue, Year, Gender).Then it plots boxplots using those summary stats (stat = 'identity') and overlays the group means as points.It uses facet_wrap() to show one plot per tissue with individual y-axis scales (free_y), and ggh4x::facetted_pos_scales() to apply custom axis settings from the scales list.----
  boxplots <-subsetted_datatset_long %>%
    dplyr::group_by(Tissue, Year, Gender) %>%
    dplyr::summarize(ymin = quantile(concentrations, 0, na.rm = TRUE),
              lower = quantile(concentrations, 0.25, na.rm = TRUE),
              median = median(concentrations, na.rm = TRUE),
              mean = mean(concentrations, na.rm = TRUE),
              upper = quantile(concentrations, 0.75, na.rm = TRUE),
              ymax = quantile(concentrations, 1, na.rm = TRUE)) %>%
    ggplot(aes(x = Year, fill = Gender)) +
    geom_boxplot(stat = 'identity',
                 aes(ymin = ymin, lower = lower, middle = median, upper = upper,
                     ymax = ymax)) +
    geom_point(aes(y = mean, group = Gender),
               position = position_dodge(width = 0.9)) +
    labs(title = paste(icons_markdown[h], '<B>',pollutant,'::','</B>',"Boxplots",sep =" "),
         y = "Concentrations", x = "Years")+
    theme(
      plot.title = ggtext::element_markdown(),
      strip.text = element_text(size = 12, face = "bold"),
      legend.title = element_text(face = "bold", size = 14)
    )+
    facet_wrap(vars(Tissue), scales ="free_y", ncol=4, drop = FALSE) +
    guides(colour="none")+
    ggh4x::facetted_pos_scales(y = scales)
  
  
  #Saves boxplots to list0 for viewing later.
  list0<-append(list(boxplots),list0, 0)
  name0 <- paste(pollutant,"_plts", sep = "")
  list0names <- append(list0names,name0)
  h<-h+1
}
#assigning names to multiple lists using pre-defined vectors of names
names(list0)<-list0names
names(list1)<-list1names
names(list2)<-list2names
names(list3)<-list3names
names(list4)<-list4names
return(list (boxplots_for_pollutants=list0, gender_between_years_tables=list1, gender_within_years_tables=list2, Shapiro_Wilks_test_results=list3, datasets_for_plotting=list4))
}


#Calling Main Function. All arguments except remove.zeroes (default is set at False) are required and user has to choose between datasets_for_organic_compounds or datasets_for_trace_metals for data processing,  also choose a multiplier to help adjust the y_axis since some plots may not show results clearly due to the compression effect or scale distortion of the outliers. For example: The 0.95 means that the upper limit of the y axis will be set at 0.95 * the minimum outlier from the upper quantile for each pollutant resulting in different y axis scales for each pollutant based on their minimum upper outliers. This can be changed as user sees fit. Users also have the option to remove all zeroes and focus on only the detected concentrations or keep them. The results are saved in temporal_gender_comparisons_results list.
gender_temporal_comparisons_results <- temporal_comparisons_between_genders(datasets_for_organic_compounds,remove.zeroes = FALSE)



#Below code saves multiple plots (each with accompanying statistical tables) into individual PNG files. It loops through the list of plots and associated tables. For each plot it extracts its name, the plot, and two result tables and saves all three (plot + two tables) in one .png file using grid.arrange().
save_graphs <- function(graph_list) {
  
  
  # Define the folder where you want to save the PNG files 
    output_folder <- file.path("/Users/mrnobody/Documents/GitHub/Squid_Fest/Squid_Conc_Anly/3-Data_Min/Outputs/temp_gender_comprsns")
  
  # Create the folder if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  # Loop through each plot
  for (i in seq_along(graph_list$boxplots_for_pollutants)) {
    
    plot_name <- names(graph_list$boxplots_for_pollutants)[i]
    plot_object <- graph_list$boxplots_for_pollutants[[i]]
    gender_within_table <- graph_list$gender_within_years_tables[[i]]
    gender_between_table <- graph_list$gender_between_years_tables[[i]]
    
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
      
      #Arranges graphs, with tables on one PNG
      grid.arrange(
        plot_object,
        gender_within_table,
        gender_between_table,
        nrow = 3,
        heights = c(2, 1.5, 2)
      )
      
      dev.off()
      cat("Saved:", output_path, "\n")
      
    }, error = function(e) {
      cat("⚠️ Error in", plot_name, ":", e$message, "\n")
    })
  }
}

#Calling save_graphs function:
save_graphs(gender_temporal_comparisons_results)
