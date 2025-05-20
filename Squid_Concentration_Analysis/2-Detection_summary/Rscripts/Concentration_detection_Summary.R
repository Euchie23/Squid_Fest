#LOADING LIBRARIES----
library(ggplot2)  # For plotting graphs
library(grid) # for creating, modifying, and arranging graphical objects ("grobs") like text, lines, rectangles, and complex layouts.
library(dplyr) # for data manipulation
library(tidyr) # For a collection of R packages used for data manipulation and visualization.
library(stringr) # For String Manipulation
library(tibble)



#Data Processor Function before doing analysis
#IT is recommended that the 'Keep_LOQ_values' argument remain as FALSE to remove all values below blank control (BB) or LOQ (BLOQ) to get a more accurate figure for the number of outliers within the dataset for the detection summary otherwise it will also classify values that are BB or BLOQ  as outliers leading to double counting.
process_dataset_for_detection_summary <- function(data, keep_LOQ_values=FALSE) {
  # Check if the dataset is Trace Metals.
  if (grepl("Metal", colnames(data)[16])) {
    
    # Processing for Trace Metals dataset
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




#HELPER FUNCTIONS FOR PROCESSING DATA

#This helper function iterates through the dataset_for_numerical_values_per_tissue by row in the concentration column. If the concentration is three times the mean of the other concentrations within that dataset then it is flagged as an outlier. It also counts the number of outliers for each tissue in each year. 
#x=dataset_for_numerical_values_per_tissue which has already been subsetted per pollutant 
#y=input_dataset_numerical_values which is the initial dataset loaded before being subsetted.
outliercheck <- function(x, y){
  if (grepl("Metal", colnames(y)[16])) {
    column_range <- 16:25
    z<-y
  }else{
    column_range <- 16:19
    z<-y
  }
  
  #preparing empty lists to store results
  list0 <- list()
  list0names <- c()
  count_outliers<- c()
  
  #For loop iterates over dataset_for_numerical_values_per_tissue to find outlier based on compliance check. It iterates over each value within that dataset and if the value is three times the mean of the other values within that dataset then it is flagged as an outlier.
  for (row in 1:nrow(x)){
    if((as.numeric(x[row,'concentration']))>(3*mean(as.numeric(x[-row,'concentration'])))){
      count_outliers <- append(count_outliers, 1)
      x[row,'outlier'] <- 'yes'
    }else{
      count_outliers <- append(count_outliers, 0)
      x[row,'outlier'] <- 'no'
    }
  }
  
  #This dataset gives the  number of outliers for each tissue per pollutant and labelled as outlier count.
  final_count_outliers <- sum(count_outliers)
  list0<-append(list(final_count_outliers),list0, 0)
  name0 <- paste("summarized_categorical_values_1", sep = "")
  list0names <- append(list0names,name0)
  
  #This dataset tells you which value within the subsetted dataset is considered an outlier based on the rules set.
  list0<-append(list(x),list0, 0)
  name0 <- paste("outlier_detection_dataset1", sep = "")
  list0names <- append(list0names,name0)
  
  
  #NEED TO RECHECK THIS BECAUSE I MIGHT NOT NEED IT
  list0<-append(list(z),list0, 0)
  name0 <- paste("partial_input_dataset", sep = "")
  list0names <- append(list0names,name0)
  
  #Returning list to be further processsed
  return(outlier_dataset_list=list0)
}


#This helper function iterates uses the data_list input and subsets both the numerical and categoral datasets per year, pollutant and tissue for detailed processing of data distribution which includes summarizing the data per year, tissue and pollutant to find out which and how many of the concentration values using the classification categories (detected, below limit of quantification (BLOQ) and below limit of detection (BLOD) and Below blank control (BB)). In addition it uses the outliercheck function to detect and summarize the number of outliers in each subsetted dataset and returns summarized dataframes showing the detected outlier values and final datasets where the outliers are removed or not based on the users preference. It also returns the summarized categorical dataset showing the number of values for each category per year, pollutant and tissue.
summarizing_and_subsetting_datasets <- function (x, y, classification_categories, status_levels, column_range){
  
  
  #LOADING INPUT DATASETS
  #Loading datasets from lists to be subsetted later for data distribution processing
  Subset_of_dataset_with_numerical_values <- x[,c(column_range,1,3,6)]
  Subset_of_dataset_with_categorical_values <- y[,c(column_range,1,3,6)]
  
  
  
  input_dataset_numerical_values<-x #saving the dataset_with_numerical_values as the input dataset for outliercheck and fiinal_dataset creation
  #print(input_dataset_numerical_values)
  
  #loading classification categories
  {if(!is.na(classification_categories[1]))category1<-(classification_categories[1])}
  {if(!is.na(classification_categories[2]))category2<-(classification_categories[2])}
  {if(!is.na(classification_categories[3]))category3<-(classification_categories[3])}
  
  
  #HOUSEKEEPING BEFORE PROCESSING
  #Getting column names from datasets
  column_names <- colnames(input_dataset_numerical_values)[column_range]
  
  
  
  #arranging tissues into a vector specifically to rank them accoriding to toxicity (from tissue with highest toxicity to lowest)
  tissues <- c('liver', 'stomach', 'muscle', 'inksac')
  
  #Saving years as levels for for loop
  years <- levels(factor(input_dataset_numerical_values[,3]))
  
  #Changing loaded datasets to long format for further processing
  long_dataset_with_categorical_values <- Subset_of_dataset_with_categorical_values %>% pivot_longer(all_of(column_names), names_to = "pollutant", values_to = "status")
  
  long_dataset_with_numerical_values_no_zero <- Subset_of_dataset_with_numerical_values %>% pivot_longer(all_of(column_names), names_to = "pollutant", values_to = "concentration")%>%subset(concentration !=0)
  
  
  #CREATING EMPTY DATAFRAMES
  #Empty dataframe to store the accumulated yearly total for the categorical values to be visualized later
  summarized_categorical_values_full <- data.frame(matrix(ncol=8, nrow = 0), check.names = FALSE)
  colnames(summarized_categorical_values_full)<-c('year', 'pollutant', 'Tissue', 'Total_N', classification_categories, 'Outliers')
  
  #Empty dataset for outlier detection using numerical dataset
  full_dataset_for_outlier_detection_using_numerical_values <- data.frame(matrix(ncol=5, nrow = 0), check.names = FALSE)
  colnames(full_dataset_for_outlier_detection_using_numerical_values)<-c('year','Tissue', 'pollutant', 'concentration', 'outlier')
  
  #Empty dataset for outlier count.
  summarized_categorical_values_1 <- data.frame(matrix(ncol=8, nrow = 0), check.names = FALSE)
  colnames(summarized_categorical_values_1)<-c('year', 'pollutant', 'Tissue', 'Total_N', classification_categories, 'Outliers')
  
  
  #Empty dataset for accumulated input dataset.
  full_input_dataset <- data.frame(matrix(ncol=ncol(input_dataset_numerical_values), nrow = 0), check.names = FALSE)
  colnames(full_input_dataset)<-colnames(input_dataset_numerical_values)
  
  
  
  #SUBSETTING INPUT DATASETS
  for (h in 1:length(years)){ 
    year <- years[h] 
    # This empty dataset was constructed to record the yearly total for the categorical values to be visualized later
    summarized_categorical_values_1 <- data.frame(matrix(ncol=8, nrow = 0), check.names = FALSE)
    colnames(summarized_categorical_values_1)<-c('year', 'pollutant', 'Tissue', 'Total_N', classification_categories, 'Outliers')
    
    #Empty dataframes for accumulated categirical data
    summarized_categorical_values_2_accumulated <- data.frame(matrix(ncol=8, nrow = 0), check.names = FALSE)
    colnames(summarized_categorical_values_2_accumulated)<-c('year', 'pollutant', 'Tissue', 'Total_N', classification_categories, 'Outliers')
    
    
    long_dataset_with_categorical_values_per_year <-as.data.frame(filter(long_dataset_with_categorical_values, Year==year))
    long_dataset_with_numerical_values_per_year<-as.data.frame(filter(long_dataset_with_numerical_values_no_zero, Year==year))
    
    # Used to subset categorical and numerical datasets per pollutant
    for (i in 1:length(column_names)) {
      pollutant_name <- column_names[i] 
      #Empty dataset for outlier detection using numerical dataset
      outlier_detection_using_numerical_values <- data.frame(matrix(ncol=5, nrow = 0), check.names = FALSE)
      colnames(outlier_detection_using_numerical_values) <-c('year','Tissue', 'pollutant', 'concentration', 'outlier')
      
      
      dataset_for_categorical_values_per_pollutant <-as.data.frame(filter(long_dataset_with_categorical_values_per_year, pollutant==pollutant_name))
      dataset_for_numerical_values_per_pollutant <-as.data.frame(filter(long_dataset_with_numerical_values_per_year, pollutant== pollutant_name))
      
      
      # Used to subset categorical and numerical datasets per tissue
      for(j in 1:length(tissues)){ 
        tissue_name <- tissues[j] 
        dataset_for_categorical_values_per_tissue<-as.data.frame(filter(dataset_for_categorical_values_per_pollutant, Tissue==tissue_name))
        dataset_for_numerical_values_per_tissue<-as.data.frame(filter(dataset_for_numerical_values_per_pollutant, Tissue==tissue_name)%>%mutate(outlier=NA))
        
        
        #summarizing dataset based on the count of each classification category with the final column as the sum of the outlier for the detected values using dataset_for_numerical_values_per_tissue   
        summarized_categorical_values_1[1,1] <- year
        summarized_categorical_values_1[1,2] <- pollutant_name
        summarized_categorical_values_1[1,3] <- tissue_name
        summarized_categorical_values_1[1,4] <- nrow(dataset_for_categorical_values_per_tissue)
        summarized_categorical_values_1[1,5] <- sum(str_count(dataset_for_categorical_values_per_tissue$status,category1))
        summarized_categorical_values_1[1,6] <- sum(str_count(dataset_for_categorical_values_per_tissue$status,fixed(category2)))
        summarized_categorical_values_1[1,7] <- sum(str_count(dataset_for_categorical_values_per_tissue$status,category3))
        # This "if function" is mainly used to accommodate datasets after being subsetted for tissue with at least one or no entries. for example; inksac in 2019.
        if(nrow(dataset_for_numerical_values_per_tissue)>1){
          summarized_categorical_values_1[1,8] <- outliercheck(x=dataset_for_numerical_values_per_tissue, y=input_dataset_numerical_values)[[1]]
          outlier_detection_dataset1 <- outliercheck(x=dataset_for_numerical_values_per_tissue, y=input_dataset_numerical_values)[[2]]
          outlier_detection_using_numerical_values <- rbind(outlier_detection_using_numerical_values, outlier_detection_dataset1)
          partial_input_dataset <- outliercheck(x=dataset_for_numerical_values_per_tissue, y=input_dataset_numerical_values)[[3]]
          full_input_dataset <- rbind(full_input_dataset, partial_input_dataset)
        
        }else{ #If the number of rows for dataset_for_numerical_values_per_tissue is less than 1 then we run below code
          summarized_categorical_values_1[1,8] <- 0 #put 0 for last column in summarized_categorical_values_1 dataset
 
          #Reconstruct the dataset_for_numerical_values_per_tissue to record the dearth in data for this tissue.
          dataset_for_numerical_values_per_tissue[ 1,'Year'] <- paste(year)
          dataset_for_numerical_values_per_tissue[ 1,'pollutant'] <- paste(pollutant_name)
          dataset_for_numerical_values_per_tissue[ 1,'Tissue'] <- paste(tissue_name)
          
          #If the dataset_for_numerical_values_per_tissue has atleast 1 row then we just keep the concentration
          if(nrow(dataset_for_numerical_values_per_tissue)==1){
            dataset_for_numerical_values_per_tissue[1,'concentration'] <- paste(dataset_for_numerical_values_per_tissue[,'concentration'])
          }else{ #If the dataset_for_numerical_values_per_tissue has no rows then we put the concentration as 0
            dataset_for_numerical_values_per_tissue[ 1,'concentration'] <- 0 
          }
          #Whether the dataset_for_numerical_values_per_tissue has 1 or no rows then we record in the last column that there are no outliers sinc there will be no other values to compare that one value of no values with.
          dataset_for_numerical_values_per_tissue[ 1,'outlier'] <- 'no'
          
          #Accumulating the dataset_for_numerical_values_per_tissue for a final dataset for all tissues
          outlier_detection_using_numerical_values <- rbind(outlier_detection_using_numerical_values, dataset_for_numerical_values_per_tissue)
          
        }
        #Accumulating the summarized_categorical_values_1 dataset for a final dataset for all tissues
        summarized_categorical_values_2_accumulated <- rbind(summarized_categorical_values_2_accumulated,summarized_categorical_values_1)
        
      } #Tissue
      #Accumulating the outlier_detection_using_numerical_values dataset for a final dataset for all tissues
      
      full_dataset_for_outlier_detection_using_numerical_values <- rbind(full_dataset_for_outlier_detection_using_numerical_values, outlier_detection_using_numerical_values)
      
      
    }# pollutants
    summarized_categorical_values_full <- rbind(summarized_categorical_values_full,summarized_categorical_values_2_accumulated)
    
    summarized_categorical_values_yearly_total <- data.frame(matrix(ncol=8, nrow = 0), check.names = FALSE)
    colnames(summarized_categorical_values_yearly_total)<-c('year', 'pollutant', 'Tissue', 'Total_N',classification_categories, 'Outliers')
    
    #summarizing datasets based on the count of each classification category using the long_dataset_with_categorical_values_per_year for the yearly categorical values 
    summarized_categorical_values_yearly_total[1,1] <- year
    summarized_categorical_values_yearly_total[1,2] <- 'Total'
    summarized_categorical_values_yearly_total[1,3] <- 'Total'
    summarized_categorical_values_yearly_total[1,4] <- nrow(long_dataset_with_categorical_values_per_year) 
    summarized_categorical_values_yearly_total[1,5] <- sum(str_count(long_dataset_with_categorical_values_per_year$status,category1))
    summarized_categorical_values_yearly_total[1,6] <- sum(str_count(long_dataset_with_categorical_values_per_year$status,fixed(category2)))
    summarized_categorical_values_yearly_total[1,7] <- sum(str_count(long_dataset_with_categorical_values_per_year$status,category3))
    summarized_categorical_values_yearly_total[1,8] <- sum(as.numeric(summarized_categorical_values_full$Outliers))
    #Accumulating the summarized_categorical_values_yearly_total for each year into one dataset.
    summarized_categorical_values_full <- rbind(summarized_categorical_values_full, summarized_categorical_values_yearly_total)
  } #years
  summarized_categorical_values_full1 <- summarized_categorical_values_full
  
  for(row in 1:nrow(summarized_categorical_values_full1)) {
    summarized_categorical_values_full1[row,'Detected'] <- summarized_categorical_values_full1[row,'Detected']-summarized_categorical_values_full1[row,'Outliers']
  }
  status_levels<- status_levels
  final_dataset_categorical_values<- summarized_categorical_values_full1 %>% pivot_longer(all_of(status_levels), names_to = "status", values_to = "values")
  
  
  
  final_dataset_for_plotting_by_tissue <- final_dataset_categorical_values %>%group_by(status, Tissue,pollutant, year) %>%mutate(Percentage = (values/Total_N)* 100) #correct (status, Tissue,pollutant, year)
  
  
  final_dataset_for_plotting_by_pollutants <- final_dataset_categorical_values %>%
    filter(!(Tissue == "inksac" & year == 2019)) %>%  # Remove rows with inksacs and 2019
    group_by(status,pollutant, year) %>%
    mutate(Percentage = (values / Total_N) * 100)
  
  final_dataset_for_plotting_by_tissue[is.na(final_dataset_for_plotting_by_tissue) | final_dataset_for_plotting_by_tissue < 0] = 0
  
  final_dataset_for_plotting_by_pollutants[is.na(final_dataset_for_plotting_by_pollutants) | final_dataset_for_plotting_by_pollutants < 0] = 0

  
  print('Below are the final results from summarizing_and_subsetting_datasets function')
  cat("\n")
  print('This dataset is called summarized_categorical_values_full')
  print(head(summarized_categorical_values_full,10))
  cat("\n")
  cat("\n")
  print('This dataset is called outlier_detection_using_numerical_value')
  print(head(outlier_detection_using_numerical_values,10))
  cat("\n")
  cat("\n")
  print('This dataset is called final_dataset_for_plotting_by_tissues')
  print(head(as.data.frame(final_dataset_for_plotting_by_tissue),10))
  cat("\n")
  cat("\n")
  print('This dataset is called final_dataset_for_plotting_by_pollutants')
  print(head(as.data.frame(final_dataset_for_plotting_by_pollutants),10))
  
  return(list (summarized_categorical_values_full=summarized_categorical_values_full, full_dataset_for_outlier_detection_using_numerical_values=full_dataset_for_outlier_detection_using_numerical_values,input_dataset_numerical_values=input_dataset_numerical_values, final_dataset_for_plotting_by_tissue=final_dataset_for_plotting_by_tissue, final_dataset_for_plotting_by_pollutants=final_dataset_for_plotting_by_pollutants))
}


#This helper function iterates through the outlier_detection_using_numerical_values dataset by row. If the outlier column says yes then it picks out the pollutant for that row to match it with the column name from the input_dataset_numerical_values. When it matches based on the same ID, Year and Tissues and if the user chooses keep the outliers in the dataset, they can do so or replace the outliers with 0 and return a new dataset.
#x=outlier_detection_using_numerical_values 
#y=input_dataset_numerical_values
final_dataset_numerical_values <- function(data_list, keep_outliers, column_range){
  
  #LOADING DATASETS FOR PROCESSING 
  outlier_detection_using_numerical_values <-data_list$full_dataset_for_outlier_detection_using_numerical_values
  input_dataset_numerical_values <-data_list$input_dataset_numerical_values
  #FOR LOOP USED TO PROCESS FINAL DATASET
  for (value in 1:nrow(outlier_detection_using_numerical_values)){
    if(outlier_detection_using_numerical_values[value,'outlier']=='yes'){
      contaminant <- unique(outlier_detection_using_numerical_values[value,'pollutant'])
      for(column_index in 1:length(column_range)){
        if(colnames(input_dataset_numerical_values)[column_index+15]!= contaminant){
          next
        }else{
          for (row in 1:nrow(input_dataset_numerical_values)){
            if(outlier_detection_using_numerical_values[value,'ID']==input_dataset_numerical_values[row,'ID'] & outlier_detection_using_numerical_values[value,'Year']==input_dataset_numerical_values[row,'Year'] & outlier_detection_using_numerical_values[value,'Tissue']==input_dataset_numerical_values[row,'Tissue']){
              if (keep_outliers == FALSE){
                input_dataset_numerical_values[row,column_index+15] <- 0
                final_dataset_for_numerical_values <- input_dataset_numerical_values
              }else{
                input_dataset_numerical_values[row,column_index+15]<-input_dataset_numerical_values[row,column_index+15]
                final_dataset_for_numerical_values <- input_dataset_numerical_values
                
              }
              
              final_dataset_for_numerical_values <- input_dataset_numerical_values
            }else{ #If the IDs, Year and Tissue are not equal to each other then it goes to the value in the input_dataset_numerical_values until the IDs, Year and Tissue match up with those from the outlier_detection_using_numerical_values
              next
            }
          }
        }
      }
    }else{ #If outlier is not equal to 'yes' then it goes to the next value 
      next
    }
    
  }
  return(final_dataset_for_numerical_values)
}


#This helper function plots the data distribution for visualization purposes.
visualization <- function(data_list1){
  
  #LOADING DATASET FOR PLOTTING
  final_dataset_for_plotting_by_tissue <- data_list1$final_dataset_for_plotting_by_tissue #Loading dataset for plotting distribution by tissues
  
  final_dataset_for_plotting_by_pollutants <- data_list1$final_dataset_for_plotting_by_pollutants #Loading dataset for plotting distribution by pollutants
  
  #Preparing empty lists to store results
  list0 <- list()
  list0names <- c()
  
  #Used to create plots
  if (grepl("Metal", final_dataset_for_plotting_by_pollutants[1,'pollutant'])) {
    status_levels<- c('BLOD','BLOQ','Outliers','Detected')
    Colors <-setNames( c('#F8766D','#00A9FF','yellow','#00BA38'),status_levels)
    pollutant_levels <- c("Metal_A","Metal_B","Metal_C","Metal_D","Metal_E","Metal_F","Metal_G","Metal_H","Metal_I","Metal_J", "Total")
    
    plot_pollutants <-ggplot(final_dataset_for_plotting_by_pollutants, aes(x = factor(pollutant, levels = pollutant_levels), y = Percentage, fill= factor(status, levels = status_levels))) + geom_bar(stat="summary", width=0.97) + scale_fill_manual(values=Colors)+
      coord_cartesian(expand = FALSE)+
      labs(title = paste('Concentration Detection Summary for Trace Metals',sep =" "),
           y = "Classification % per Pollutant", x = "Pollutants", fill= 'Detection Classifications')+
      #scale_fill_manual(values=Colors)+
      scale_y_continuous(labels = function(x) paste0(x, "%"))+
      facet_wrap(vars(year), scales ="free_x", ncol=3, drop = FALSE)+ theme(strip.text = element_text(size = 15, colour = 'black'), axis.text.x = element_text(size = 10, colour = 'black'))
    print(plot_pollutants)
    list0<-append(list(plot_pollutants),list0, 0)
    name0 <- paste("Concentration Detection Summary using pollutants", sep = "")
    list0names <- append(list0names,name0)
    
    status_levels<- c('BLOD','BLOQ','Outliers','Detected')
    Colors <-setNames( c('#F8766D','#00A9FF','yellow','#00BA38'),status_levels)
    tissue_levels <- c("liver","stomach","muscle","inksac","Total")
    plot_tissues <-ggplot(final_dataset_for_plotting_by_tissue, aes(x = factor(Tissue, levels = tissue_levels), y = Percentage, fill= factor(status, levels = status_levels))) + geom_bar(stat="summary", width=0.97) + scale_fill_manual(values=Colors)+
      coord_cartesian(expand = FALSE)+
      labs(title = paste('Concentration Detection Summary for Trace Metals',sep =" "),
           y = "Classification % per Tissue", x = "Tissues", fill= 'Detection Classifications')+
      #scale_fill_manual(values=Colors)+
      scale_y_continuous(labels = function(x) paste0(x, "%"))+
      facet_wrap(vars(year), scales ="free_x", ncol=3, drop = FALSE)+ theme(strip.text = element_text(size = 15, colour = 'black'), axis.text.x = element_text(size = 10, colour = 'black'))
    print(plot_tissues)
    list0<-append(list(plot_tissues),list0, 0)
    name0 <- paste("Concentration Detection Summary using tissues", sep = "")
    list0names <- append(list0names,name0)
  }else{
    status_levels<- c('BLOD','BLOQ','Outliers','Detected')
    Colors <-setNames( c('#F8766D','#00A9FF','yellow','#00BA38'),status_levels)
    pollutant_levels <- c("Adipic_acid","Caprolactam","Chlorpyrifos","Ibuprofen","Total")
    
    plot_pollutants <-ggplot(final_dataset_for_plotting_by_pollutants, aes(x = factor(pollutant, levels = pollutant_levels), y = Percentage, fill= factor(status, levels = status_levels))) + geom_bar(stat="summary", width=0.97) + scale_fill_manual(values=Colors)+
      coord_cartesian(expand = FALSE)+
      labs(title = paste('Concentration Detection Summary for Organic Compounds',sep =" "),
           y = "Classification % per Pollutant", x = "Pollutants", fill= 'Detection Classifications')+
      #scale_fill_manual(values=Colors)+
      scale_y_continuous(labels = function(x) paste0(x, "%"))+
      facet_wrap(vars(year), scales ="free_x", ncol=3, drop = FALSE)+ theme(strip.text = element_text(size = 15, colour = 'black'), axis.text.x = element_text(size = 10, colour = 'black'))
    print(plot_pollutants)
    list0<-append(list(plot_pollutants),list0, 0)
    name0 <- paste("Concentration Detection Summary using pollutants", sep = "")
    list0names <- append(list0names,name0)
    
    status_levels<- c('BLOD','BLOQ','Outliers','Detected')
    Colors <-setNames( c('#F8766D','#00A9FF','yellow','#00BA38'),status_levels)
    tissue_levels <- c("liver","stomach","muscle","inksac","Total")
    plot_tissues <-ggplot(final_dataset_for_plotting_by_tissue, aes(x = factor(Tissue, levels = tissue_levels), y = Percentage, fill= factor(status, levels = status_levels))) + geom_bar(stat="summary", width=0.97) + scale_fill_manual(values=Colors)+
      coord_cartesian(expand = FALSE)+
      labs(title = paste('Concentration Detection Summary for Organic Compounds',sep =" "),
           y = "Classification % per Tissue", x = "Tissues", fill= 'Detection Classifications')+
      #scale_fill_manual(values=Colors)+
      scale_y_continuous(labels = function(x) paste0(x, "%"))+
      facet_wrap(vars(year), scales ="free_x", ncol=3, drop = FALSE)+ theme(strip.text = element_text(size = 15, colour = 'black'), axis.text.x = element_text(size = 10, colour = 'black'))
    print(plot_tissues)
    list0<-append(list(plot_tissues),list0, 0)
    name0 <- paste("Concentration Detection Summary using tissues", sep = "")
    list0names <- append(list0names,name0)
  }  
  
  names(list0) <- list0names
  return(data_distribution_plots=list0)
}


#This main Function incorporates all the previous functions to help  process the data and create the tables and graphs needed to visualize and interpret the data distribution. The keep_outliers argument is for the final updated dataset. If it is 'FALSE' then it replaces the outliers with 0 in the updated final dataset. If 'TRUE' then it keeps the outliers. 
detection_summary <- function(data_list, keep_outliers){
  
  dataset_with_categorical_values <- data_list$dataset_with_categorical_values
  dataset_with_numerical_values <- data_list$dataset_with_numerical_values
  
  keep_outliers
  
  x <- data_list$dataset_with_categorical_values
  
  if (grepl("Metal", colnames(x)[16])) {
    
    column_range <- 16:25
    
    classification_categories <- c('BLOD',"BLOQ",'Detected')
    
    status_levels <- c('BLOD',"BLOQ", 'Outliers', 'Detected')
    
    data_list1 <- summarizing_and_subsetting_datasets(dataset_with_numerical_values, dataset_with_categorical_values, classification_categories, status_levels, column_range)
    
    new_dataset <- final_dataset_numerical_values(data_list1, keep_outliers, column_range)
    
    graph <- visualization (data_list1)
    
  }else{
    
    column_range <- 16:19
    
    classification_categories <- c('BLOD',"BLOQ",'Detected')
    
    status_levels <- c('BLOD',"BLOQ", 'Outliers', 'Detected')
    
    #Loading datasets from lists to be subsetted later for data distribution processing
    dataset_with_categorical_values <- data_list$dataset_with_categorical_values
    dataset_with_numerical_values <- data_list$dataset_with_numerical_values
    
    data_list1 <- summarizing_and_subsetting_datasets(x=dataset_with_numerical_values, y=dataset_with_categorical_values, classification_categories, status_levels, column_range)
    
    new_dataset <- final_dataset_numerical_values(data_list1, keep_outliers, column_range)
    
    graph <- visualization (data_list1)
    
  }
  return(list (summarized_and_subsetted_datasets=data_list1, #summarized and subsetted the datasets finding the outliers and counting the classification categories for each pollutant in each year and for each tissue.  
               updated_dataset_of_numerical_values=new_dataset, #if outliers are chosen to be removed then all the outliers in the updated dataset will be zero
               detection_summary_graph=graph))
}

detection_summaries <- detection_summary(datasets_for_organic_compounds, keep_outliers=TRUE)

#Below code saves multiple plots into individual PNG files. It loops through the list of plots and and for each plot it extracts its name, and the plot then saves it in one .png file using grid.draw()
save_graphs <- function(graph_list) {
  
  # Extract pollutant types from the first available plot
  
  plot_data <- graph_list[["detection_summary_graph"]][["Concentration Detection Summary using pollutants"]][[1]]
  
  pollutant_types <- unique(plot_data$pollutant) 
  
  
  # Define the folder where you want to save the PNG files 
  if (any(grepl("Metal", pollutant_types))) {
    output_folder <- file.path("/Users/mrnobody/Documents/GitHub/Squid_Fest/Squid_Concentration_Analysis/2-Detection_summary/Detection_summary_plots/Trace_metals")
  }else{
    output_folder <- file.path("/Users/mrnobody/Documents/GitHub/Squid_Fest/Squid_Concentration_Analysis/2-Detection_summary/Detection_summary_plots/Organic_compounds")  
  }
  
  # Create the folder if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  #Getting plot names
  plot_names <- graph_list[["detection_summary_graph"]]

  # Loop through actual ggplot objects in graph_list
  for (plot_name in names(plot_names)) {
    plot_object <- graph_list[["detection_summary_graph"]][[plot_name]]
    
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
save_graphs(detection_summaries)


library(dplyr)
library(tidyr)
library(purrr)
library(gt)
library(gridExtra)
library(tibble)

# Assume your data is in a dataframe called your_data
# Replace with your actual dataframe name
df <- detection_summary_count  

# Capitalize first letter of column names if needed
colnames(df) <- gsub("^(\\w)", "\\U\\1", colnames(df), perl = TRUE)

# Rearrange columns to have Tissue first, then others
df <- df %>%
  select(Year, Pollutant, Tissue, Total_N, BLOD, BLOQ, Detected, Outliers)

# Unique years and pollutants
years <- unique(df$Year)
pollutants <- unique(df$Pollutant)

# Helper function to create a spacer tibble with same column names and row count
make_spacer <- function(n) {
  tibble(
    Tissue = rep("", n),
    BLOD = rep("", n),
    BLOQ = rep("", n),
    Detected = rep("", n),
    Outliers = rep("", n)
  )
}

# Create list to store gt tables by year and pollutant
tables_by_year <- list()

for (yr in years) {
  # Filter year data
  df_year <- df %>% filter(Year == yr)
  
  # List to hold pollutant tables for this year
  pollutant_tables <- list()
  
  for (poll in pollutants) {
    # Filter pollutant data for the year
    df_sub <- df_year %>% filter(Pollutant == poll)
    
    # Select and reorder columns, Tissue first
    df_sub <- df_sub %>%
      select(Tissue, BLOD, BLOQ, Detected, Outliers)
    
    # Create gt table and add title above table
    tbl <- gt(df_sub) %>%
      tab_header(title = md(paste0("**", poll, "-", yr, "**")))
    
    pollutant_tables[[poll]] <- tbl
  }
  
  # Now arrange pollutant tables side-by-side with spacer columns in between
  # We convert gt tables to grobs for grid.arrange
  
  grobs <- list()
  n_rows <- max(map_int(pollutant_tables, ~nrow(as.data.frame(.x$`_data`))))
  
  # For each pollutant table, convert to grob and add spacer if not last
  for (i in seq_along(pollutant_tables)) {
    grobs[[length(grobs)+1]] <- gt::as_raw_html(pollutant_tables[[i]])
    if (i < length(pollutant_tables)) {
      # spacer as a blank plot with same height
      spacer <- grid::nullGrob()
      grobs[[length(grobs)+1]] <- spacer
    }
  }
  
  # Store grobs list by year
  tables_by_year[[as.character(yr)]] <- grobs
}

# The above part generates gt tables and tries to arrange grobs but gt tables can't be converted directly to grobs
# Instead, we can print gt tables individually in RMarkdown or Shiny and rely on layout there.

# For a base R approach, we can output tables as data.frames side-by-side with spacer columns

# Alternative base R style: create data.frames with spacer columns for each year and bind them side-by-side

library(purrr)

for (yr in years) {
  df_year <- df %>% filter(Year == yr)
  
  # Get number of rows per pollutant for this year
  rows_per_pollutant <- df_year %>% 
    group_by(Pollutant) %>% 
    summarize(n = n()) %>% 
    pull(n)
  
  max_rows <- max(rows_per_pollutant)
  
  pollutant_tables_df <- list()
  
  for (poll in pollutants) {
    df_sub <- df_year %>% filter(Pollutant == poll) %>%
      select(Tissue, BLOD, BLOQ, Detected, Outliers)
    
    # Pad with blank rows to max_rows
    if (nrow(df_sub) < max_rows) {
      n_pad <- max_rows - nrow(df_sub)
      pad <- tibble(
        Tissue = rep("", n_pad),
        BLOD = rep("", n_pad),
        BLOQ = rep("", n_pad),
        Detected = rep("", n_pad),
        Outliers = rep("", n_pad)
      )
      df_sub <- bind_rows(df_sub, pad)
    }
    
    # Add title row as a separate data.frame (single-row, spanning all columns is tricky in base R)
    title_row <- tibble(
      Tissue = paste0(poll, "-", yr),
      BLOD = rep("", 1),
      BLOQ = rep("", 1),
      Detected = rep("", 1),
      Outliers = rep("", 1)
    )
    
    # Bind title on top of the data
    table_with_title <- bind_rows(title_row, df_sub)
    
    pollutant_tables_df[[poll]] <- table_with_title
  }
  
  # Create spacer column with same number of rows including title row
  spacer <- tibble(
    Tissue = rep("", max_rows + 1),
    BLOD = rep("", max_rows + 1),
    BLOQ = rep("", max_rows + 1),
    Detected = rep("", max_rows + 1),
    Outliers = rep("", max_rows + 1)
  )
  
  # Now bind tables side-by-side with spacer columns between
  combined <- pollutant_tables_df[[1]]
  
  if (length(pollutant_tables_df) > 1) {
    for (i in 2:length(pollutant_tables_df)) {
      combined <- bind_cols(combined, spacer, pollutant_tables_df[[i]])
    }
  }
  
  # Print or write combined table for this year
  print(combined)
  
  # Optionally, save to CSV for inspection
  # write.csv(combined, paste0("Pollutant_tables_", yr, ".csv"), row.names = FALSE)
}
