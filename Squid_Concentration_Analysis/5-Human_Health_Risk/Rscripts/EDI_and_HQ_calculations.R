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

#' Helper function to identify pollutant type and prepare concentration and reference data
get_pollutant_info <- function(df) {
 
  if (grepl("Metal", colnames(df)[16])) {
    # For trace metals
    range <- colnames(df[16:25])
    number_range <- 16:25
    df[, number_range] <- lapply(df[, number_range], as.numeric)
    
    recommended_levels <- data.frame(
      pollutants = c("Metal_F", "Metal_G", "Metal_B", "Metal_D", "Metal_A", "Metal_H", "Metal_C", "Metal_J", "Metal_I", "Metal_E"),
      lower_recommended_levels = c(0.01, 2, 0.0016, 30, 100, 0.1, 0.05, 8, 0.45, 30),
      upper_recommended_levels = rep(NA, 10),
      levels = c('Grasso et al. 2021: 0.01mg/kg','FAO/WHO: <0.05-2mg/kg','EFSA: 0.0016mg/kg','ANVISA: 30mg/kg', 'FAO/WHO: 100mg/kg','Brodziak-Dopierała et al. 2023: 0.1mg/kg','FAO/WHO: <0.05-2mg/kg','FAO/WHO: <0.5-8mg/kg','Makridis and Amberger, 1996; LaCoste et al. 2001:\n <0.45-2.28mg/kg (permissible range for animal feed)','FAO/WHO: <30-100mg/kg'),
      oral_reference_dosage = c(0.005, 0.01, 0.03, 0.04, 0.7, 0.1, 0.003, 0.04, 0.00001, 0.3)
    )
  } else {
    # For organics
    range <- colnames(df[16:19])
    number_range <- 16:19
    df[, number_range] <- lapply(df[, number_range], as.numeric)
    
    recommended_levels <- data.frame(
      pollutants = c("Organic_A", "Organic_B", "Organic_C", "Organic_D"),
      lower_recommended_levels = c(470, 50, 0.01, 40),
      upper_recommended_levels = c(NA, NA, NA, NA),
      levels = c('EPA: 470mg/kg/day','EPA: 50mg/kg/day', 'FAO/WHO: 0.01mg/kg/day','The Mayo Clinic: 40mg/kg/day'),
      oral_reference_dosage = c(2, 0.5, 0.01, 1200)
    )
  }
  
  list(range = range, number_range = number_range, df = df, recommended_levels = recommended_levels)
}

#' Helper function to reshape data to long format, filter for 'muscle', join recommended dosage
reshape_and_join_data <- function(df, range, recommended_levels, remove_zeroes) {
  long_df <- df %>%
    pivot_longer(all_of(range), names_to = "pollutant", values_to = "concTai") %>%
    subset(Tissue == 'muscle') %>%
    mutate(concArg = concTai, EDIgenpopArg = NA, EDIgenpopTai = NA)
  
  if (remove_zeroes) {
    long_df <- subset(long_df, concTai != 0)
  }
  
  long_df <- left_join(long_df, recommended_levels, by = c('pollutant' = 'pollutants'))
  return(long_df)
}

#' Helper function to calculate EDI and HQ statistics (mean and 95th percentile) for one pollutant/year
calculate_THQ <- function(subset_df, pollutant, year) {
  ord <- unique(subset_df$oral_reference_dosage)
  Arg_concentration_mean <- mean(subset_df$concArg, na.rm = TRUE)
  Tai_concentration_mean <- mean(subset_df$concTai, na.rm = TRUE)
  Arg_EDI_mean <- (6.78 * Arg_concentration_mean) / 65 # 65(kg) is the average weight of the general population in Argentina and the 6.78(kg) is the average amount of seafood eaten per day by the general population
  Tai_EDI_mean <- (29.76 * Tai_concentration_mean) / 65 # 65(kg) is the average weight of the general population in Argentina and the 29.76(kg) is the average amount of seafood eaten per day by the general population
  Arg_HQ_mean <- Arg_EDI_mean / ord
  Tai_HQ_mean <- Tai_EDI_mean / ord
  Arg_concentration_95_percentile <- quantile(subset_df$concArg, 0.95, na.rm = TRUE)
  Tai_concentration_95_percentile <- quantile(subset_df$concTai, 0.95, na.rm = TRUE)
  Arg_EDI_95_percentile <- (6.78 * Arg_concentration_95_percentile) / 65
  Tai_EDI_95_percentile <- (29.76 * Tai_concentration_95_percentile) / 65
  Arg_HQ_95_percentile <- Arg_EDI_95_percentile / ord
  Tai_HQ_95_percentile <- Tai_EDI_95_percentile / ord
  
  return(data.frame(
    Year = year,
    pollutant = pollutant,
    oral_reference_dosage = ord,
    Arg_concentration_mean = Arg_concentration_mean,
    Tai_concentration_mean = Tai_concentration_mean,
    Arg_EDI_mean = Arg_EDI_mean,
    Tai_EDI_mean = Tai_EDI_mean,
    Arg_HQ_mean = Arg_HQ_mean,
    Tai_HQ_mean = Tai_HQ_mean,
    Arg_concentration_95_percentile = Arg_concentration_95_percentile,
    Tai_concentration_95_percentile = Tai_concentration_95_percentile,
    Arg_EDI_95_percentile = Arg_EDI_95_percentile,
    Tai_EDI_95_percentile = Tai_EDI_95_percentile,
    Arg_HQ_95_percentile = Arg_HQ_95_percentile,
    Tai_HQ_95_percentile = Tai_HQ_95_percentile
  ))
}

#' Helper function to loop over pollutants for a given year and compute THQ (Target hazard quotient) summaries
calculate_yearly_summary <- function(data_long, pollutants, year) {
  result_list <- list()
  for (poll in pollutants) {
    sub_df <- subset(data_long, pollutant == poll & Year == year)
    if (nrow(sub_df) > 0) {
      thq <- calculate_THQ(sub_df, poll, year)
    } else {
      # Handle missing data
      thq <- data.frame(matrix(0, nrow = 1, ncol = 15))
      colnames(thq) <- c("Year", "pollutant", "oral_reference_dosage",
                         "Arg_concentration_mean", "Tai_concentration_mean",
                         "Arg_EDI_mean", "Tai_EDI_mean",
                         "Arg_HQ_mean", "Tai_HQ_mean",
                         "Arg_concentration_95_percentile",    "Tai_concentration_95_percentile",
                        "Arg_EDI_95_percentile", "Tai_EDI_95_percentile",
                         "Arg_HQ_95_percentile", "Tai_HQ_95_percentile")
      thq$Year <- year
      thq$pollutant <- poll
    }
    result_list[[length(result_list) + 1]] <- thq
  }
  return(do.call(rbind, result_list))
}


#' Helper function to generate Hazard Quotient plots per pollutant
generate_hq_plot <- function(df, title, x_lab, pollutant_levels, fill_title) {
  ggplot(df, aes(x = factor(pollutant, levels = pollutant_levels), y = values, fill = countries)) +
    geom_bar(stat = 'summary', color = "black", position = position_dodge(), width = 0.9) +
    coord_cartesian(ylim = c(0, 1.5))+
    coord_cartesian(expand = FALSE)+
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    labs(title = title, y = "General Population HQ values", x = x_lab, fill = fill_title)
}

#' Helper function to generate Total Hazard Quotient plots per year
generate_total_hq_plot <- function(df, title, x_levels, x_lab, fill_title) {
  ggplot(df, aes(x = factor(countries, levels = x_levels), y = values, fill = countries)) +
    geom_bar(stat = 'summary', color = "black", position = position_dodge(), width = 0.9) +
    coord_cartesian(ylim = c(0, 1.5))+
    coord_cartesian(expand = FALSE)+
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    labs(title = title, y = "General Population HQ values", x = x_lab, fill = fill_title) +
    theme(axis.text.x = element_text(colour = 'black', angle = 90))
}


#' Master plot generation function to create HQ plots for individual pollutants and total pollutants for each year
create_yearly_plots <- function(year, dataset, is_metal = TRUE) {
  #recoding
  dataset1 <- dataset %>%
    mutate(countries = recode(countries,
                         "Arg_HQ_mean" = "Argentina HQ (mean)",
                          "Tai_HQ_mean" = "Taiwan HQ (mean)",
    "Arg_HQ_95_percentile" = "Argentina HQ (95th percentile)",
    "Tai_HQ_95_percentile" = "Taiwan HQ (95th percentile)"))
  
  dataset2 <- dataset %>%
    mutate(countries = recode(countries,
                         "Arg_HQ_mean" = "Argentina HQ (mean)",
                         "Tai_HQ_mean" = "Taiwan HQ (mean)",
                         "Arg_HQ_95_percentile" = "Argentina HQ (95th percentile)",
                         "Tai_HQ_95_percentile" = "Taiwan HQ (95th percentile)"))
  

  colz <- c('Argentina HQ (mean)', 'Taiwan HQ (mean)', 
            'Argentina HQ (95th percentile)', 'Taiwan HQ (95th percentile)')
  
  if (is_metal) {
    pollutant_levels <- c("Metal_A", "Metal_B", "Metal_C", "Metal_D", 
                          "Metal_E", "Metal_F", "Metal_G", "Metal_H", 
                          "Metal_I", "Metal_J")
    x_lab <- "Trace Metals"
  } else {
    pollutant_levels <- c("Organic_A", "Organic_B", "Organic_C", "Organic_D", "Total_HQ")
    x_lab <- "Organic Compounds"
  }
  
  title_base <- paste(year, "Hazard Quotient(HQ)")
  
  cat("\n---------------------------------\n")
  df_main <- dataset1 %>% group_by(pollutant) %>% filter(Year == year)
  print(head(as.data.frame(df_main)))
  
  #Dataset for yearly plots with pollutants on the x axis

  df_total <- dataset2 %>%
    filter(Year == year) %>%
    group_by(Year, countries) %>%
    summarise(values = sum(values, na.rm = TRUE), .groups = 'drop') %>%
    mutate(pollutant = "Total_HQ") %>%
    select(Year, pollutant, everything())  # Order columns if needed
  #print head of dataset
    print(head(as.data.frame(df_total)))
  
  plt1 <- generate_hq_plot(df_main, title_base, x_lab, pollutant_levels, "Countries")
  plt2 <- generate_total_hq_plot(df_total, paste(year, "Total Hazard Quotient(HQ)"), colz, x_lab, "Countries")
  
  return(list(plt1, plt2))
}


#' === MAIN FUNCTION ===
# This function calculates Estimated Daily Intake (EDI) and Hazard Quotient (HQ) values for various pollutants (either trace metals or organic compounds) found in squid tissue.The goal is to estimate potential human health risks from consuming contaminated squid by comparing observed pollutant concentrations to established safety thresholds.The function first determines whether the dataset includes trace metals or organic pollutants. Based on this, it sets the correct concentration columns and joins reference data, including oral reference doses and recommended levels and the average weight and seafood consumption of the general population of two consumers of these squid species .It then reshapes the data into a long format and filters it to focus on muscle tissue only, which is most relevant for human consumption. Optionally, it can remove zero values if needed.For each year in the dataset, and for each pollutant, the function calculates:
# - Mean and 95th percentile concentrations
# - Corresponding EDI values for two countries (Argentina and Taiwan)
# - HQ values for both countries, which are ratios of EDI to oral reference doses
#
# These metrics are returned in a combined summary table that can be used to assess how pollutant exposure changes over time and whether it exceeds safety levels.
EDI_and_HQ_calculations <- function(data_list, remove.zeroes = FALSE) {
  df <- data_list$dataset_with_numerical_values
  
  # Detect pollutant type and reference levels
  info <- get_pollutant_info(df)
  df <- info$df
  range <- info$range
  number_range <- info$number_range
  recommended_levels <- info$recommended_levels
  
  # Reshape and filter data
  long_df <- reshape_and_join_data(df, range, recommended_levels, remove.zeroes)
  
  # Compute THQ values year by year
  years <- unique(df$Year)
  pollutants <- colnames(df)[number_range]
  
  #Creating empty lists
  list0 <- list(); list1 <- list(); list2 <- list(); list3 <- list()
  
  for (year in years) {
    #to get pollution summary per year and save it as the final df after combining the rows
    all_results <- data.frame(matrix(ncol=15, nrow = 0))
    #creating list to save plots names
    list12names <- list()
    yearly_data <- calculate_yearly_summary(long_df, pollutants, year)
    all_results <-rbind(all_results, yearly_data) 
    
    #rounding all values in columns 4 to 15
    all_results[,c(4:15)] <- round(all_results[,c(4:15)],2)
    # Selecting specific columns (HQ metrics by country) to reshape into long format
   columz=c("Arg_HQ_mean","Tai_HQ_mean","Arg_HQ_95_percentile","Tai_HQ_95_percentile")
    

   # Reshaping the dataset from wide to long format for easier comparison across countries and percentiles
    long_yearly_data <- as.data.frame(yearly_data %>% pivot_longer(cols = all_of(columz), names_to = "countries", values_to = "values"))
    is_metal <- grepl("Metal", colnames(df)[16])  # Or some smarter check
    dataset <- long_yearly_data  # Dataset for yearly plots with pollutants on the x axis
   
    plots <- create_yearly_plots(year, dataset, is_metal)
    
    if (year == "2019") {
      list0 <- plots
    } else if (year == "2020") {
      list1 <- plots
    } else if (year == "2021") {
      list2 <- plots
    }
    # creating and saving names for plots within the lists
    name1 <- paste(year,"_HQbarplots", sep = "")
    name2 <- paste(year,"_TotalHQ", sep = "")
    list12names <- append(list12names,name1)
    list12names <- append(list12names,name2)
   
    
     # naming the items in the sublists that are within the last list
    if (year == "2019") {
    names(list0)<-list12names
    # subsetting the all_results datasets for 2019 data
    all_results19 <- subset(all_results, Year == '2019')
    # saving and naming the 2019 all_results datasets within the last list
    list3<-append(list(all_results19),list3, 0)
    name3 <- paste(year,"_EDI_and_HQ_stats", sep = "")
    names(list3)<-name3
    } else if (year == "2020") {
    names(list1)<-list12names
    # subsetting the all_results datasets for 2020 data
    all_results20 <- subset(all_results, Year == '2020')
    # saving and naming the 2020 all_results datasets within the last list
    list3<-append(list(all_results20),list3, 0)
    name3 <- paste(year,"_EDI_and_HQ_stats", sep = "")
    names(list3)<-name3
    } else if (year == "2021") {
    names(list2)<-list12names
    # subsetting the all_results datasets for 2021
    all_results21 <- subset(all_results21, Year == '2021')
    # saving and naming the 2021 all_results datasets within the last list
    list3<-append(list(all_results21),list3, 0)
    name3 <- paste(year,"_EDI_and_HQ_stats", sep = "")
    names(list3)<-name3
    }
  }
  #Naming the list and returning plots
  return(list(pollutant2019 = list0, pollutant2020 = list1, pollutant2021 = list2, FullEDI_HQstats = list3))
  
}

EDI_and_HQ_stats <- EDI_and_HQ_calculations(datasets_for_trace_metals, remove.zeroes = TRUE)


#Below code saves and combines multiple plots into individual PNG files. It loops through the list of plots in each year and combines the two plots into one png then saves them in their respective folders it also extracts the dataset saves it as an excel file which is converted into a table using VBA macros.
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