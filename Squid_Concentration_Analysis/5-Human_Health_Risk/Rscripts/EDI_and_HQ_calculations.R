#LOADING LIBRARIES----
library(dplyr) # for data manipulation
library(ggplot2)  # For plotting graphs
library(grid) # for creating, modifying, and arranging graphical objects ("grobs") like text, lines, rectangles, and complex layouts.
library(gridExtra) #for arranging multiple plots or grid-based objects
library(tibble) # for modern, user-friendly reimagining of data frames with better printing and subsetting features.
library(tidyr) # For a collection of R packages used for data manipulation and visualization.
library(stringr) # For String Manipulation
library(openxlsx) # for  reading, writing, and editing Excel files directly from R.



#Data Processor Function before doing analysis
#IT is recommended that the 'Keep_LOQ_values' argument remain as FALSE to remove all values below blank control (BB) or LOQ (BLOQ) to get a more accurate figure for the number of outliers within the dataset for the detection summary otherwise it will also classify values that are BB or BLOQ  as outliers leading to double counting.
process_dataset_for_detection_summary <- function(data, keep_LOQ_values=FALSE) {
  # Check if the dataset is Trace Metals.
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
        return(list(dataset_with_numerical_values=data1, dataset_with_categorical_values=data2))
      }
      data1 <-process_data(data, user_choice)
      #return(list(dataset_with_numerical_values=data1, dataset_with_categorical_values=data2))
    }
  }
}


# Data Processing for Trace Metals dataset
tracemetals_data <- read.csv("Squid_Concentration_Analysis/1-Data_Preprocessing/Final_Results_For_Analysis/Final_TMresults_mgkg.csv", header = TRUE)
datasets_for_trace_metals <- process_dataset_for_detection_summary(tracemetals_data, keep_LOQ_values = FALSE) 


# Data Processing for Organic Compounds dataset
organiccompounds_data <- read.csv("Squid_Concentration_Analysis/1-Data_Preprocessing/Final_Results_For_Analysis/Final_OCresults_mgkg.csv", header = TRUE)
datasets_for_organic_compounds <- process_dataset_for_detection_summary(organiccompounds_data, keep_LOQ_values = FALSE)

# Helper function to calculate and store y_axis_upper_limit for the different tissues affected by each pollutant.
get_y_axis_upper_limit <- function (dataset){
  
  # Create an empty data frame to store the results of the coefficients. final  y_axis_dataframe_final where coefficient1 is accumulated
  y_axis_dataframe_final <- data.frame(matrix(ncol=2, nrow = 0))
  colnames( y_axis_dataframe_final) <- c('Year','y_axis_upper_limit')
  
  # Temporary data frame to store coefficients for the current iteration
  y_axis_dataframe <- data.frame(matrix(ncol=2, nrow = 0))
  colnames( y_axis_dataframe) <- colnames( y_axis_dataframe_final)

  
  # Handle y_upper_limit
  concentrations <- dataset$concTai
  y_scales <- ifelse(any(!is.na(concentrations)), max(concentrations, na.rm = TRUE), 1)
  
  y_upper_limit <-y_scales # Set the upper limit for the y-axis based on the smallest outlier
  y_axis_dataframe[1,1] <- unique(dataset$Year)
  y_axis_dataframe[1,2] <- y_upper_limit
  y_axis_dataframe_final <- rbind( y_axis_dataframe_final,y_axis_dataframe)
  # Return the final dataframe with the coefficients
  return ( y_axis_dataframe_final)
}


#' === MAIN FUNCTION ===
# This function calculates Estimated Daily Intake (EDI) and Hazard Quotient (HQ) values for various pollutants (either trace metals or organic compounds) found in squid tissue.The goal is to estimate potential human health risks from consuming contaminated squid by comparing observed pollutant concentrations to established safety thresholds.The function first determines whether the dataset includes trace metals or organic pollutants. Based on this, it sets the correct concentration columns and joins reference data, including oral reference doses and recommended levels and the average weight and seafood consumption of the general population of two consumers of these squid species .It then reshapes the data into a long format and filters it to focus on muscle tissue only, which is most relevant for human consumption. Optionally, it can remove zero values if needed.For each year in the dataset, and for each pollutant, the function calculates:
# - Mean and 95th percentile concentrations
# - Corresponding EDI values for two countries (Argentina and Taiwan)
# - HQ values for both countries, which are ratios of EDI to oral reference doses
#
# These metrics are returned in a combined summary table that can be used to assess how pollutant exposure changes over time and whether it exceeds safety levels.
EDI_and_HQ_calculation <- function(data_list, remove.zeroes=FALSE){
  
  dataset_with_numerical_values <- data_list$dataset_with_numerical_values
  
  # Initialize empty lists for storing results----
  list0 <- list()
  list1 <- list()
  list2 <- list()
  list3 <- list()
  list0names <- c()
  list1names <- c()
  list2names <- c()
  list3names <- c()
  # Determine pollutant range and subset the dataset based on its presence----
  if (grepl("Metal", colnames(dataset_with_numerical_values)[16])) {
    # Trace metals subset
    range <- colnames(dataset_with_numerical_values[16:25])
    number_range <- 16:25
    
    #changing concentration data to numeric
    dataset_with_numerical_values[, number_range] <- lapply(dataset_with_numerical_values[, number_range], as.numeric)
    
    #recommended levels for trace metals in mg/kg accumulated from different datasets:
    recommended_levels <- data.frame(pollutants=c("Metal_F","Metal_G","Metal_B","Metal_D","Metal_A","Metal_H","Metal_C","Metal_J","Metal_I","Metal_E"), 
                                     lower_recommended_levels=c(0.01,2,0.0016,30,100,0.1,0.05,8,0.45,30), upper_recommended_levels=c(rep(NA, 10)), 
                                     levels=c('Grasso et al. 2021: 0.01mg/kg','FAO/WHO: <0.05-2mg/kg','EFSA: 0.0016mg/kg','ANVISA: 30mg/kg', 'FAO/WHO: 100mg/kg','Brodziak-Dopierała et al. 2023: 0.1mg/kg','FAO/WHO: <0.05-2mg/kg','FAO/WHO: <0.5-8mg/kg','Makridis and Amberger, 1996; LaCoste et al. 2001:\n <0.45-2.28mg/kg (permissible range for animal feed)','FAO/WHO: <30-100mg/kg'),oral_reference_dosage=c(0.005, 0.01, 0.03, 0.04, 0.7, 0.1, 0.003, 0.04,0.00001, 0.3))
  } else {
    # Organic compounds subset
    range <- colnames(dataset_with_numerical_values[16:19])
    number_range <- 16:19
    
    #changing concentration data to numeric
    dataset_with_numerical_values[, number_range] <- lapply(dataset_with_numerical_values[, number_range], as.numeric)
    
    #recommended levels for organic compounds in mg/kg accumulated from different datasets:
    recommended_levels <- data.frame(pollutants=c("Organic_A","Organic_B","Organic_C","Organic_D"), lower_recommended_levels=c(470, 50,0.01,40), upper_recommended_levels=c(NA, NA,NA,NA), levels=c('EPA: 470mg/kg/day','EPA: 50mg/kg/day', 'FAO/WHO: 0.01mg/kg/day','The Mayo Clinic: 40mg/kg/day'), oral_reference_dosage=c(2,0.5,0.01,1200))
  }
  
  #subsetting dataset and getting needed vectors for further analysis
  subsetted_dataset <-dataset_with_numerical_values[,c(number_range,3, 4, 5, 6, 7)]
  years <- levels(factor(dataset_with_numerical_values[,3]))
  pollutants <- colnames(dataset_with_numerical_values)[number_range]
  subsetted_dataframe <-subsetted_dataset
  
  #making empty datasets for further analysis and savibg data
  y_scales <- data.frame(matrix(ncol=3, nrow = 0))
  tissues <- levels(factor(dataset_with_numerical_values[,6]))
  results_dataframe_0 <- data.frame(matrix(ncol=15, nrow = 0)) 
  results_dataframe_1 <- data.frame(matrix(ncol=15, nrow = 0))
  colnames(results_dataframe_0) <- c('Year','pollutant','oral_reference_dosage','ConcentrationArg (mean)','ConcentrationTai (mean)','Argentina EDI(mean)','Taiwan EDI(mean)', 'Argentina HQ (mean)','Taiwan HQ (mean)','ConcentrationArg (95th percentile)', 'ConcentrationTai (95th percentile)','Argentina EDI (95th percentile)','Taiwan EDI (95th percentile)', 'Argentina HQ (95th percentile)', 'Taiwan HQ (95th percentile)')
  colnames(results_dataframe_1) <- c('Year','pollutant','oral_reference_dosage','ConcentrationArg (mean)','ConcentrationTai (mean)','Argentina EDI(mean)','Taiwan EDI(mean)', 'Argentina HQ (mean)','Taiwan HQ (mean)','ConcentrationArg (95th percentile)', 'ConcentrationTai (95th percentile)','Argentina EDI (95th percentile)','Taiwan EDI (95th percentile)', 'Argentina HQ (95th percentile)', 'Taiwan HQ (95th percentile)')
  full_results_dataframe <- data.frame(matrix(ncol=15, nrow = 0), check.names = FALSE)
  full_results_dataframe1 <- data.frame(matrix(ncol=15, nrow = 0), check.names = FALSE)
  colnames(full_results_dataframe) <- colnames(results_dataframe_0)
  colnames(full_results_dataframe1) <- colnames(results_dataframe_0)
  
  #Changing subsetted data into long format for statistical analysis
  if(remove.zeroes==FALSE){
    long_subsetted_dataframe<- subsetted_dataframe %>% pivot_longer(all_of(range), names_to = "pollutant", values_to = "concTai")%>%subset(Tissue == 'muscle')%>%mutate(concArg=NA)%>%mutate(EDIgenpopArg=NA)%>%mutate(EDIgenpopTai=NA)
  }else{
    long_subsetted_dataframe<- subsetted_dataframe %>% pivot_longer(all_of(range), names_to = "pollutant", values_to = "concTai")%>%subset(concTai !=0)%>%subset(Tissue == 'muscle')%>%mutate(EDIgenpopArg=NA)%>%mutate(EDIgenpopTai=NA)
  }
  
  #further subsetting on long_subsetted dataframe by pollutants
  long_subsetted_dataframe1 <-long_subsetted_dataframe%>% left_join(recommended_levels, by=c('pollutant'='pollutants'))
  
  #Target Hazard Quotient 1 function for THQ calculation 
  THQcalc <- function(x){
    for (i in 1:nrow(x)){
      if(x[i,'Tissue']!= 'muscle'){
        x[i,'concArg'] <- NA
      }else{
        x[i,'concArg'] <- x[i,'concTai']
      }
    }
    return(x)
  }
  long_subsetted_dataframe2 <-THQcalc(long_subsetted_dataframe1)
  
  #Using for loop to full in empty dataframe 1 (Hazard Qutotient per pollutant per country) by conducting descriptive stats and EDI calculations for Argentina and Taiwan.
  for(h in 1:length(years)){
    subsetted_yearly_dataframe<-long_subsetted_dataframe2 %>% group_by(pollutant) %>% subset(Year == years[h])
    Total <- c()
    for (i in 1:length(pollutants)){
      subsetted_pollutant_dataframe <- as.data.frame(filter(subsetted_yearly_dataframe, pollutant== pollutants[i]))
      if(nrow(subsetted_pollutant_dataframe)!=0){
        results_dataframe_0[1,1] <- years[h]
        results_dataframe_0[1,2] <- pollutants[i]
        results_dataframe_0[1,3] <- unique(subsetted_pollutant_dataframe[,'oral_reference_dosage'])
        results_dataframe_0[1,4] <- signif(mean(subsetted_pollutant_dataframe$concArg, na.rm=TRUE),3)
        results_dataframe_0[1,5] <- signif(mean(subsetted_pollutant_dataframe$concTai, na.rm=TRUE),3)
        results_dataframe_0[1,6] <- signif((6.78*results_dataframe_0[1,4])/65, 3)
        results_dataframe_0[1,7] <- signif((29.76*results_dataframe_0[1,5])/65, 3)
        results_dataframe_0[1,8] <- format(signif(results_dataframe_0[1,6]/unique(subsetted_pollutant_dataframe[,'oral_reference_dosage']),3), scientific=FALSE)
        results_dataframe_0[1,9] <- format(signif(results_dataframe_0[1,7]/unique(subsetted_pollutant_dataframe[,'oral_reference_dosage']),3), scientific=FALSE)
        results_dataframe_0[1,10] <- signif(quantile(subsetted_pollutant_dataframe$concArg, probs = .95, na.rm=TRUE), 3)
        results_dataframe_0[1,11] <-signif(quantile(subsetted_pollutant_dataframe$concTai, probs = .95, na.rm=TRUE), 3)
        results_dataframe_0[1,12] <- signif((6.78*results_dataframe_0[1,10])/65, 3)
        results_dataframe_0[1,13] <-signif((29.76*results_dataframe_0[1,11])/65, 3)
        results_dataframe_0[1,14] <-format(signif(results_dataframe_0[1,12]/unique(subsetted_pollutant_dataframe[,'oral_reference_dosage']),3), scientific=FALSE)
        results_dataframe_0[1,15] <-format(signif(results_dataframe_0[1,13]/unique(subsetted_pollutant_dataframe[,'oral_reference_dosage']),3), scientific=FALSE)
        full_results_dataframe <-rbind(full_results_dataframe, results_dataframe_0)
      }else if (nrow(subsetted_pollutant_dataframe)==0){
        results_dataframe_0[1,1] <- years[h]
        results_dataframe_0[1,2] <- pollutants[i]
        results_dataframe_0[1,3] <- recommended_levels[i,'oral_reference_dosage']
        results_dataframe_0[1,4] <- 0
        results_dataframe_0[1,5] <- 0
        results_dataframe_0[1,6] <- 0
        results_dataframe_0[1,7] <- 0
        results_dataframe_0[1,8] <- 0
        results_dataframe_0[1,9] <- 0
        results_dataframe_0[1,10] <-0
        results_dataframe_0[1,11] <-0
        results_dataframe_0[1,12] <-0
        results_dataframe_0[1,13] <-0
        results_dataframe_0[1,14] <-0
        results_dataframe_0[1,15] <-0
        full_results_dataframe <-rbind(full_results_dataframe, results_dataframe_0) 
      }
    }
    #Using for loop to full in empty dataframe 2 (Total Hazard Qutotient ALL pollutants per country) by conducting descriptive stats and EDI calculations for Argentina and Taiwan.
    colz <- c('Argentina HQ (mean)','Taiwan HQ (mean)','Argentina HQ (95th percentile)', 'Taiwan HQ (95th percentile)')
    results_dataframe_1[1,1] <- years[h]
    results_dataframe_1[1,2] <- 'Total_HQ'
    results_dataframe_1[1,3] <- NA
    results_dataframe_1[1,4] <- signif(sum(as.numeric(full_results_dataframe[,4]), na.rm = T),3)
    results_dataframe_1[1,5] <- signif(sum(as.numeric(full_results_dataframe[,5]), na.rm = T),3)
    results_dataframe_1[1,6] <- signif(sum(as.numeric(full_results_dataframe[,6]), na.rm = T),3)
    results_dataframe_1[1,7] <- signif(sum(as.numeric(full_results_dataframe[,7]), na.rm = T),3)
    results_dataframe_1[1,8] <- signif(sum(as.numeric(full_results_dataframe[,8]), na.rm = T),3)
    results_dataframe_1[1,9] <- signif(sum(as.numeric(full_results_dataframe[,9]), na.rm = T),3)
    results_dataframe_1[1,10] <- signif(sum(as.numeric(full_results_dataframe[,10]), na.rm = T),3)
    results_dataframe_1[1,11] <-signif(sum(as.numeric(full_results_dataframe[,11]), na.rm = T),3)
    results_dataframe_1[1,12] <- signif(sum(as.numeric(full_results_dataframe[,12]), na.rm = T),3)
    results_dataframe_1[1,13] <-signif(sum(as.numeric(full_results_dataframe[,13]), na.rm = T),3)
    results_dataframe_1[1,14] <-signif(sum(as.numeric(full_results_dataframe[,14]), na.rm = T),3)
    results_dataframe_1[1,15] <-signif(sum(as.numeric(full_results_dataframe[,15]), na.rm = T),3)
    
    full_results_dataframe1 <-rbind(full_results_dataframe1, results_dataframe_1)
    subsetted_yearly_dataframe1 <- as.data.frame(full_results_dataframe %>% pivot_longer(cols = all_of(colz), names_to = "countries", values_to = "values"))
    subsetted_yearly_dataframe1.1 <- as.data.frame(full_results_dataframe1 %>% pivot_longer(cols = all_of(colz), names_to = "countries", values_to = "values"))
    #print(full_results_dataframe)
    subsetted_yearly_dataframe1[,13] <- as.numeric(subsetted_yearly_dataframe1[,13])
    
    #Target Hazard Quotient 2 function for THQ calculation 
    THQcalc2 <- function(x){
      for (i in 1:nrow(x)){
        if(is.infinite(x[i,'values'])|is.nan(x[i,'values'])|is.na(x[i,'values'])){
          x[i,'values'] <- 0
        }else{
          x[i,'values'] <- x[i,'values']
        }
      }
      return(x)
    }
    
    subsetted_yearly_dataframe2 <-THQcalc2(subsetted_yearly_dataframe1)
    subsetted_yearly_dataframe2.1 <-THQcalc2(subsetted_yearly_dataframe1.1)
    
    #Creating y axis scales for plotting
    y_axis_upper_limit <- get_y_axis_upper_limit(subsetted_yearly_dataframe)
    y_scales <- rbind(y_scales, y_axis_upper_limit)
    if(nrow(y_scales)!=0){
      y_scales_df <- y_scales
    }else{
      Year <- years[h]
      pollutant <- pollutants[i]
      y_axis_upper_limit <- 0
      y_scales2 <- data.frame(Year, y_axis_upper_limit)
      y_scales_df <- y_scales2 
    }
    df_scales <- data.frame(
      Year = c("2019", "2020", "2021"),
      ymin = c(0, 0, 0),
      ymax = c(NA),
      n = c(5, 5, 5))
    
    df_scales %<>% inner_join(y_scales_df, by= "Year") %>%
      mutate(ymax = coalesce(y_axis_upper_limit)) %>%select(Year, ymin, ymax, n)
    
    df_scales <- split(df_scales, df_scales$Year)
    scales <- lapply(df_scales, function(x) {
      scale_y_continuous(limits = c(x$ymin, x$ymax), n.breaks = x$n)
    })
    # Determine pollutant range and subset the dataset based on its presence----
    if (grepl("Metal", colnames(dataset_with_numerical_values)[16])) {
      if(years[h]=='2019'){
        subsetted_yearly_dataframe19 <-subsetted_yearly_dataframe2 %>% group_by(pollutant) %>% subset(Year == years[h])
        subsetted_yearly_dataframe19.1 <-subsetted_yearly_dataframe2.1 %>% group_by(pollutant) %>% subset(Year == years[h])
        pollutant_levels=c("Metal_F","Metal_G","Metal_B","Metal_D","Metal_A","Metal_H","Metal_C","Metal_J","Metal_I","Metal_E")
        plt1 <- ggplot(subsetted_yearly_dataframe19, aes(x=factor (pollutant, levels=pollutant_levels), y=values, fill=countries)) +
          geom_bar(stat='summary',color="black", position=position_dodge(), width = 0.90) + 
          coord_cartesian(ylim = c(0, 1.5))+
          coord_cartesian(expand = FALSE)+
          geom_hline(yintercept=1, linetype="dashed", color = "red")+
          labs(title = paste(years[h], 'Hazard Quotient(HQ)',sep =" "), y = "General Population HQ values", x = "Trace Metals", fill= ' Countries')
        list0<-append(list(plt1),list0, 0)
        name0 <- paste(years[h],"HQbarplots", sep = "")
        list0names <- append(list0names,name0)
        #for total 2019----
        colz <- c('Argentina HQ (mean)','Taiwan HQ (mean)','Argentina HQ (95th percentile)', 'Taiwan HQ (95th percentile)')
        plt2 <- ggplot(subsetted_yearly_dataframe19.1, aes(x=factor (countries, levels=colz), y=values, fill=countries)) +
          geom_bar(stat='summary',color="black", position=position_dodge(), width = 0.90) + 
          coord_cartesian(ylim = c(0, 1.5))+
          coord_cartesian(expand = FALSE)+
          geom_hline(yintercept=1, linetype="dashed", color = "red")+
          labs(title = paste(years[h], 'Total Hazard Quotient(HQ)',sep =" "), y = "General Population HQ values", x = "Trace Metals", fill= ' Countries')+ theme(axis.text.x = element_text(colour = 'black', angle = 90))
        list0<-append(list(plt2),list0, 0)
        name0 <- paste(years[h],"TotalHQ", sep = "")
        list0names <- append(list0names,name0)
      }else if (years[h]=='2020'){
        subsetted_yearly_dataframe20 <-as.data.frame(subsetted_yearly_dataframe2 %>% group_by(pollutant) %>% subset(Year == years[h]))
        subsetted_yearly_dataframe20.1 <-as.data.frame(subsetted_yearly_dataframe2.1 %>% group_by(pollutant) %>% subset(Year == years[h]))
        pollutant_levels=c("Metal_F","Metal_G","Metal_B","Metal_D","Metal_A","Metal_H","Metal_C","Metal_J","Metal_I","Metal_E")
        plt3 <- ggplot(subsetted_yearly_dataframe20, aes(x=factor(pollutant, levels=pollutant_levels), y=values, fill=countries)) +
          geom_bar(stat='summary',color="black", position=position_dodge(), width = 0.90) + 
          coord_cartesian(ylim = c(0, 1.5))+
          coord_cartesian(expand = FALSE)+
          geom_hline(yintercept=1, linetype="dashed", color = "red")+
          labs(title = paste(years[h], 'Hazard Quotient(HQ)',sep =" "), y = "General Population HQ values", x = "Trace Metals", fill= ' Countries')
        list1<-append(list(plt3),list1, 0)
        name1 <- paste(years[h],"HQbarplots", sep = "")
        list1names <- append(list1names,name1)
        #For 2020 total----
        colz <- c('Argentina HQ (mean)','Taiwan HQ (mean)','Argentina HQ (95th percentile)', 'Taiwan HQ (95th percentile)')
        plt4 <- ggplot(subsetted_yearly_dataframe20.1, aes(x=factor(countries, levels=colz), y=values, fill=countries)) +
          geom_bar(stat='summary',color="black", position=position_dodge(), width = 0.90) + 
          coord_cartesian(ylim = c(0, 1.5))+
          coord_cartesian(expand = FALSE)+
          geom_hline(yintercept=1, linetype="dashed", color = "red")+
          labs(title = paste(years[h], 'Total Hazard Quotient(HQ)',sep =" "), y = "General Population HQ values", x = "Trace Metals", fill= ' Countries')+ theme(axis.text.x = element_text(colour = 'black', angle = 90))
        list1<-append(list(plt4),list1, 0)
        name1 <- paste(years[h],"TotalHQ", sep = "")
        list1names <- append(list1names,name1)
      }else{
        subsetted_yearly_dataframe21 <-subsetted_yearly_dataframe2 %>% group_by(pollutant) %>% subset(Year == years[h])
        subsetted_yearly_dataframe21.1 <-subsetted_yearly_dataframe2.1 %>% group_by(pollutant) %>% subset(Year == years[h])
        pollutant_levels=c("Metal_F","Metal_G","Metal_B","Metal_D","Metal_A","Metal_H","Metal_C","Metal_J","Metal_I","Metal_E")
        plt5 <- ggplot(subsetted_yearly_dataframe21, aes(x= factor(pollutant, levels=pollutant_levels), y=values, fill=countries)) +
          geom_bar(stat='summary',color="black", position=position_dodge(), width = 0.90) + 
          coord_cartesian(ylim = c(0, 1.5))+
          coord_cartesian(expand = FALSE)+
          geom_hline(yintercept=1, linetype="dashed", color = "red")+
          labs(title = paste(years[h], 'Hazard Quotient(HQ)',sep =" "), y = "General Population HQ values", x = "Trace Metals", fill= ' Countries')
        list2<-append(list(plt5),list2, 0)
        name2 <- paste(years[h],"HQbarplots", sep = "")
        list2names <- append(list2names,name2)
        #For total 2021----
        colz <- c('Argentina HQ (mean)','Taiwan HQ (mean)','Argentina HQ (95th percentile)', 'Taiwan HQ (95th percentile)')
        plt6 <- ggplot(subsetted_yearly_dataframe21.1, aes(x= factor(countries, levels=colz), y=values, fill=countries)) +
          geom_bar(stat='summary',color="black", position=position_dodge(), width = 0.90) + 
          coord_cartesian(ylim = c(0, 1.5))+
          coord_cartesian(expand = FALSE)+
          geom_hline(yintercept=1, linetype="dashed", color = "red")+
          labs(title = paste(years[h], 'Total Hazard Quotient(HQ)',sep =" "), y = "General Population HQ values", x = "Trace Metals", fill= ' Countries')+ theme(axis.text.x = element_text(colour = 'black', angle = 90))
        list2<-append(list(plt6),list2, 0)
        name2 <- paste(years[h],"TotalHQ", sep = "")
        list2names <- append(list2names,name2)
      }
    }else{
      if(years[h]=='2019'){
        subsetted_yearly_dataframe19 <-subsetted_yearly_dataframe2 %>% group_by(pollutant) %>% subset(Year == years[h])
        subsetted_yearly_dataframe19.1 <-subsetted_yearly_dataframe2.1 %>% group_by(pollutant) %>% subset(Year == years[h])
        pollutant_levels <- c("Organic_A","Organic_B","Organic_C","Organic_D", "Total_HQ")
        plt1 <- ggplot(subsetted_yearly_dataframe19, aes(x=factor (pollutant, levels=pollutant_levels), y=values, fill=countries)) +
          geom_bar(stat='summary',color="black", position=position_dodge(), width = 0.90) + 
          coord_cartesian(ylim = c(0, 1.5))+
          coord_cartesian(expand = FALSE)+
          geom_hline(yintercept=1, linetype="dashed", color = "red")+
          labs(title = paste(years[h], 'Hazard Quotient(HQ)',sep =" "), y = "General Population HQ values", x = "Organic Compounds", fill= ' Countries')+ theme(axis.text.x = element_text(colour = 'black', angle = 90))
        list0<-append(list(plt1),list0, 0)
        name0 <- paste(years[h],"HQbarplots", sep = "")
        list0names <- append(list0names,name0)
        #for total 2019----
        colz <- c('Argentina HQ (mean)','Taiwan HQ (mean)','Argentina HQ (95th percentile)', 'Taiwan HQ (95th percentile)')
        plt2 <- ggplot(subsetted_yearly_dataframe19.1, aes(x=factor (countries, levels=colz), y=values, fill=countries)) +
          geom_bar(stat='summary',color="black", position=position_dodge(), width = 0.90) + 
          coord_cartesian(ylim = c(0, 1.5))+
          coord_cartesian(expand = FALSE)+
          geom_hline(yintercept=1, linetype="dashed", color = "red")+
          labs(title = paste(years[h], 'Total Hazard Quotient(HQ)',sep =" "), y = "General Population HQ values", x = "Organic Compounds", fill= ' Countries')+ theme(axis.text.x = element_text(colour = 'black', angle = 90))
        list0<-append(list(plt2),list0, 0)
        name0 <- paste(years[h],"TotalHQ", sep = "")
        list0names <- append(list0names,name0)
      }else if (years[h]=='2020'){
        subsetted_yearly_dataframe20 <-subsetted_yearly_dataframe2 %>% group_by(pollutant) %>% subset(Year == years[h])
        subsetted_yearly_dataframe20.1 <-subsetted_yearly_dataframe2.1 %>% group_by(pollutant) %>% subset(Year == years[h])
        pollutant_levels <- c("Organic_A","Organic_B","Organic_C","Organic_D", "Total_HQ")
        plt3 <- ggplot(subsetted_yearly_dataframe20, aes(x=factor(pollutant, levels=pollutant_levels), y=values, fill=countries)) +
          geom_bar(stat='summary',color="black", position=position_dodge(), width = 0.90) + 
          coord_cartesian(ylim = c(0, 1.5))+
          coord_cartesian(expand = FALSE)+
          geom_hline(yintercept=1, linetype="dashed", color = "red")+
          labs(title = paste(years[h], 'Hazard Quotient(HQ)',sep =" "), y = "General Population HQ values", x = "Organic Compounds", fill= ' Countries')+ theme(axis.text.x = element_text(colour = 'black', angle = 90))
        list1<-append(list(plt3),list1, 0)
        name1 <- paste(years[h],"HQbarplots", sep = "")
        list1names <- append(list1names,name1)
        #For 2020 total----
        colz <- c('Argentina HQ (mean)','Taiwan HQ (mean)','Argentina HQ (95th percentile)', 'Taiwan HQ (95th percentile)')
        plt4 <- ggplot(subsetted_yearly_dataframe20.1, aes(x=factor(countries, levels=colz), y=values, fill=countries)) +
          geom_bar(stat='summary',color="black", position=position_dodge(), width = 0.90) + 
          coord_cartesian(ylim = c(0, 1.5))+
          coord_cartesian(expand = FALSE)+
          geom_hline(yintercept=1, linetype="dashed", color = "red")+
          labs(title = paste(years[h], 'Total Hazard Quotient(HQ)',sep =" "), y = "General Population HQ values", x = "Organic Compounds", fill= ' Countries')+ theme(axis.text.x = element_text(colour = 'black', angle = 90))
        list1<-append(list(plt4),list1, 0)
        name1 <- paste(years[h],"TotalHQ", sep = "")
        list1names <- append(list1names,name1)
      }else{
        subsetted_yearly_dataframe21 <-subsetted_yearly_dataframe2 %>% group_by(pollutant) %>% subset(Year == years[h])
        subsetted_yearly_dataframe21.1 <-subsetted_yearly_dataframe2.1 %>% group_by(pollutant) %>% subset(Year == years[h])
        pollutant_levels <- c("Organic_A","Organic_B","Organic_C","Organic_D", "Total_HQ")
        plt5 <- ggplot(subsetted_yearly_dataframe21, aes(x= factor(pollutant, levels=pollutant_levels), y=values, fill=countries)) +
          geom_bar(stat='summary',color="black", position=position_dodge(), width = 0.90) + 
          coord_cartesian(ylim = c(0, 1.5))+
          coord_cartesian(expand = FALSE)+
          geom_hline(yintercept=1, linetype="dashed", color = "red")+
          labs(title = paste(years[h], 'Hazard Quotient(HQ)',sep =" "), y = "General Population HQ values", x = "Organic Compounds", fill= ' Countries')+ theme(axis.text.x = element_text(colour = 'black', angle = 75))
        list2<-append(list(plt5),list2, 0)
        name2 <- paste(years[h],"HQbarplots", sep = "")
        list2names <- append(list2names,name2)
        #For total 2021----
        colz <- c('Argentina HQ (mean)','Taiwan HQ (mean)','Argentina HQ (95th percentile)', 'Taiwan HQ (95th percentile)')
        plt6 <- ggplot(subsetted_yearly_dataframe21.1, aes(x= factor(countries, levels=colz), y=values, fill=countries)) +
          geom_bar(stat='summary',color="black", position=position_dodge(), width = 0.90) + 
          coord_cartesian(ylim = c(0, 1.5))+
          coord_cartesian(expand = FALSE)+
          geom_hline(yintercept=1, linetype="dashed", color = "red")+
          labs(title = paste(years[h], 'Total Hazard Quotient(HQ)',sep =" "), y = "General Population HQ values", x = "Organic Compounds", fill= ' Countries')+ theme(axis.text.x = element_text(colour = 'black', angle = 90))
        list2<-append(list(plt6),list2, 0)
        name2 <- paste(years[h],"TotalHQ", sep = "")
        list2names <- append(list2names,name2)
      }
    }
  }
  #saving dataset with results for viweing later
  list3<-append(list(full_results_dataframe),list3, 0)
  name3 <- paste("EDI_and_HQ_stats", sep = "")
  list3names <- append(list3names,name3)
  names(list0)<-list0names
  names(list1)<-list1names
  names(list2)<-list2names
  names(list3)<-list3names
  return(list (pollutant2019=list0, pollutant2020=list1, pollutant2021=list2, FullEDI_HQstats=list3 ))
}

EDI_and_HQ_stats <- EDI_and_HQ_calculation(datasets_for_trace_metals, remove.zeroes = TRUE)

save_custom_outputs <- function(output_list) {

  years <- c('2019', '2020', '2021')  # Expected years
  
  for (year in years) {
    # Try to find the sublist name that contains this year
    matching_name <- names(output_list)[grepl(year, names(output_list))]
    
    if (length(matching_name) == 0) {
      cat("⚠️ No data for year:", year, "\n")
      next
    }
    
    year_data <- output_list[[matching_name]]
    
    # Extract the two ggplot objects from the first sublist
    plot_pair <- year_data[[1]]
    if (length(year_data) != 2 || !all(sapply(year_data, inherits, "gg"))) {
      cat("⚠️ Skipping year", year, "- first sublist must contain exactly 2 ggplot objects.\n")
      next
    }
    
    # Detect pollutant type to choose output folder
    example_plot_data <- plot_pair[[1]]
    pollutant_types <- unique(example_plot_data$pollutant)
    
    if (any(grepl("Metal", pollutant_types, ignore.case = TRUE))) {
      root_folder <- "Squid_Concentration_Analysis/5-Human_Health_Risk/EDI_and_HQ_plots_and_tables/Trace_metals"
      file_name <- print(paste("EDI_and_HQ_tables_tm_",year,".xlsm", sep = ""))
      excel_file <- file.path(root_folder, year, file_name)
    } else {
      root_folder <- "Squid_Concentration_Analysis/5-Human_Health_Risk/EDI_and_HQ_plots_and_tables/Organic_compounds"
      file_name <- print(paste("EDI_and_HQ_tables_oc_",year,".xlsm", sep = ""))
      excel_file <- file.path(root_folder, year, file_name)
    }
    
    # Create year-specific folder
    output_folder <- file.path(root_folder, year)
    if (!dir.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }
    
    # Combine and save the two plots side by side
    combined_plot <- grid.arrange(grobs = year_data, ncol = 2)
    plot_filename <- paste0("EDI_and_HQ_plots_", year, ".png")
    png_path <- file.path(output_folder, plot_filename)
    
    tryCatch({
      png(png_path, width = 1800, height = 1000)
      grid.draw(combined_plot)
      dev.off()
      cat("✅ Plot saved:", png_path, "\n")
    }, error = function(e) {
      cat("❌ Error saving plot for year", year, ":", e$message, "\n")
    })
    
    # Process 4th sublist (data.frame)
    df <- as.data.frame(output_list[[4]])
    if (!is.data.frame(df)) {
      cat("❌ Year", year, "4th sublist is not a data frame. Skipping Excel export.\n")
      next
    }
    
    # Write the data frame to Excel
    if (file.exists(excel_file)) {
      wb <- loadWorkbook(excel_file)
      if ("summary" %in% tolower(names(wb))) {
        removeWorksheet(wb, names(wb)[tolower(names(wb)) == "summary"])
      }
    } else {
      wb <- createWorkbook()
    }
    
    addWorksheet(wb, "summary")
    writeData(wb, "summary", df)
    saveWorkbook(wb, file = excel_file, overwrite = TRUE)
    
    cat("✅ Excel file saved for year", year, "at", excel_file, "\n")
  }
}

 save_custom_outputs(EDI_and_HQ_stats)
 