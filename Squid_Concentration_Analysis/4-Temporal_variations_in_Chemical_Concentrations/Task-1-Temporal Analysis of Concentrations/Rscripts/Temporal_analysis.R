#LOADING LIBRARIES----
library(DescTools) # For descriptive statistics
library(FSA)      # For Dunn's test
library(ggplot2)  # For plotting graphs
library(dplyr) # for data manipulation
library(tidyr) # For a collection of R packages used for data manipulation and visualization.
library(rcompanion) # For Compact Letter Displays
library(agricolae)  # For Tukey HSD compact letter displays


#FUNCTIONS FOR PROCESSING DATA----

#Step 1----
temporal_analysis_step1 <- function(data, remove_zeroes = FALSE) {
  # Determine the range of columns to process
  if (grepl("Fe|Ag", colnames(data)[16])) {
    pollutants <- colnames(data)[16:25]
  } else {
    pollutants <- colnames(data)[16:19]
  }
  
  # Prepare the long-format data
  long_data <- data %>%
    select(c(16:ncol(data), Tissue = 6, Year = 3, ID_num = 4)) %>%
    pivot_longer(cols = all_of(pollutants), names_to = "Pollutant", values_to = "Concentration") %>%
    mutate(
      Concentration = as.numeric(Concentration),
      Year = factor(Year),
      Tissue = factor(Tissue)
    )
  
  if (remove_zeroes == TRUE) {
    long_data <- long_data %>% filter(Concentration != 0)
  }
  
  # Initialize results containers
  shapiro_results <- data.frame()
  summary_statistics <- list()
  
  for (pollutant in pollutants) {
    pollutant_data <- long_data %>% filter(Pollutant == pollutant)
    tissues <- unique(pollutant_data$Tissue)
    years <- unique(pollutant_data$Year)
    
    for (tissue in tissues) {
      for (year in years) {
        subset_data <- pollutant_data %>%
          filter(Tissue == tissue, Year == year)
        
        
        if (nrow(subset_data) < 3 | all(subset_data$Concentration[-1] == subset_data$Concentration[1])) {
          shapiro_results <- rbind(
            shapiro_results,
            data.frame(
              Pollutant = pollutant,
              Tissue = tissue,
              Year = year,
              ShapiroPValue = "<3/No observations",
              TestResult = "<3/No observations"
            )
          )
          next
        }
        
        if (nrow(subset_data) > 3 | all(subset_data$Concentration[-1] != subset_data$Concentration[1])){
          # Perform Shapiro-Wilk test
          shapiro_test <- shapiro.test(subset_data$Concentration)
          test_result <- ifelse(shapiro_test$p.value > 0.05, "Pass", "Fail")
          # Store Shapiro-Wilk results
          shapiro_results <- rbind(
            shapiro_results,
            data.frame(
              Pollutant = pollutant,
              Tissue = tissue,
              Year = year,
              ShapiroPValue = shapiro_test$p.value,
              TestResult = test_result
            )
          )
        }
      }
    }
    
            # Define all tissues and years you want in the output
            all_tissues <- c("liver", "muscle", "inksac", "stomach")
            all_years <- c(2019, 2020, 2021)
            
            
            # Generate a complete set of combinations
            all_combinations <- expand.grid(
              Pollutant = unique(pollutant_data$Pollutant),
              Tissue = all_tissues,
              Year = all_years
            )
            
            # Summarize with explicit handling for zero observations
            summary_stats <- pollutant_data %>%
              group_by(Pollutant, Tissue, Year) %>%
              summarize(
                N = n(),
                Mean = ifelse(N > 0, mean(Concentration, na.rm = TRUE), 0),
                Median = ifelse(N > 0, median(Concentration, na.rm = TRUE), 0),
                Max = ifelse(N > 0, max(Concentration, na.rm = TRUE), 0),
                Min = ifelse(N > 0, min(Concentration, na.rm = TRUE), 0),
                SD = ifelse(N > 0, sd(Concentration, na.rm = TRUE), 0),
                SE = ifelse(N > 0, SD / sqrt(N), 0),
              .groups = "drop"
            ) 
            
            # Ensure 'Year' is the same type in both datasets
            all_combinations <- all_combinations %>%
              mutate(Year = as.character(Year))
            
            summary_stats <- summary_stats %>%
              mutate(Year = as.character(Year))
            
            # Join the summary back to the complete set of combinations
            final_stats <- all_combinations %>%
              left_join(summary_stats, by = c("Pollutant", "Tissue", "Year")) %>%
              replace_na(list(
                N = 0, Mean = 0, Median = 0, Max = 0, Min = 0, SD = 0, SE = 0
              ))
          
            #print(final_stats)
          summary_statistics[[pollutant]] <- final_stats

      }
  # Return results
  list(
    ShapiroResults = shapiro_results,
    SummaryStatistics = summary_statistics,
    LongData = long_data
  )
} #This function processes the data to give the normality test results, summary statistics for each pollutant and manipulating the dataset from wide to long structure.

temporal_analysis_step1 <-temporal_analysis_step1(processed_hm_data) #Run Function #1





#Step 2----
temporal_analysis_step2 <- function(data_list, remove_zeroes = FALSE) {
  # Unpack the data list into individual elements
  results_data <- data_list$ShapiroResults
  long_data <- data_list$LongData
  summary_statistics <- data_list$SummaryStatistics
  #print(remove_zeroes)
  
  # Initialize empty lists to store results
  test_results <- list()
  post_hoc_tests_list <- list()
  group_comparison_tests_list <- list()
  final_summary_list <- list()
  final_summary_names <- c()
  group_comparison_tests_names <- c()
  post_hoc_tests_names <- c()
  
  # Iterate over each unique pollutant in the summary statistics
  
  pollutants <- names(summary_statistics)

  for (pollutant in pollutants) {
    
    # Filter data for the current pollutant
    results_for_pollutant <- results_data %>%
      filter(Pollutant == pollutant)
    
    long_data_for_pollutant <- long_data %>%
      filter(Pollutant == pollutant)
    
    # Iterate over each tissue for the current pollutant
    for (tissue in unique(results_for_pollutant$Tissue)) {
      # Filter data for the current tissue
      data <- results_for_pollutant %>%
        filter(Tissue == tissue)

      print(data)
      
      subset_long <- long_data_for_pollutant %>%
        filter(Tissue == tissue)

      # Initialize an empty dataframe for compact letters
      compact_letters2 <- data.frame(matrix(ncol = 4, nrow = 0))
      
      # For interaction between Tissue and Year to examine how these two predictors jointly influence the response
      interactions <- interaction(data.frame(subset_long[,1], subset_long[,2]))
      
      
      # Check if Shapiro-Wilk test failed, indicating non-normality
      if (any(data$TestResult == "Fail")) {
        
        print(paste("I am in running KW.TEST for", pollutant, tissue))
        
        # Perform Kruskal-Wallis test for non-normal data
        group_comparison_tests <- kruskal.test(Concentration ~ interactions, data = subset_long)
        
      
        # Perform Dunn's test for pairwise comparisons
        post_hoc_test <- dunnTest(Concentration ~ interactions, data = subset_long, method = "bonferroni")

        # Compact letter displays to show comparisons
        compact_letter_displays <- cldList(P.adj ~ Comparison, data = post_hoc_test$res, threshold = 0.05, remove.zero = remove_zeroes)
        
       # Combine compact letter results
       compact_letters1 <-compact_letter_displays %>% separate(Group, c("Tissue", "Year"))
       
      #Checking if the letters are the same in column 3, if they all are then we replace them with a blank space and then add them to the big dataset compact_letters2
       if(all(compact_letters1[-1,3]== compact_letters1[1,3])){
         compact_letters1[,c(3:4)] <- paste(" ")
         compact_letters2 <- rbind(compact_letters2,compact_letters1)
       }else{
         compact_letters2 <- rbind(compact_letters2,compact_letters1)
       }
       
       # Since We didnt collect inksac for 2019 we mainly used this piece of code as a placeholder for for visualization in the graphs
       if(unique(subset_long$Tissue)=='inksac' & '2019' %in% subset_long[,'Year']==FALSE){
         compact_letters_for_inksac_2019 <- data.frame(matrix(ncol = 4, nrow = 1))
       compact_letters_names <-c("Tissue","Year","Letter","MonoLetter")
       colnames(compact_letters_for_inksac_2019) <- compact_letters_names
       compact_letters_for_inksac_2019[,1]<- 'inksac'
       compact_letters_for_inksac_2019[,2]<- '2019'
       compact_letters_for_inksac_2019[,c(3:4)]<- paste(" ")
       
       compact_letters2 <- rbind(compact_letters2,compact_letters_for_inksac_2019)
       }
       
  
        # Store results in the list as a data frame
        test_results[[length(test_results) + 1]] <- data.frame(
          Pollutant = pollutant,
          Tissue = tissue,
          Year= compact_letters2[,2],
          TestType = "Kruskal-Wallis",
          CompactLetters = compact_letters2[,3]
        )
       
      } else if (any(data$TestResult != "Fail") & any(data$TestResult == "Pass")){
        
        print(paste("I am in running ANOVA for", pollutant, tissue))
        
        # Perform ANOVA for normally distributed data
        group_comparison_tests <- aov(Concentration ~ interactions, data = subset_long)
        
        
        # Run post hoc Tukey HSD test
        post_hoc_test <- HSD.test(group_comparison_tests, "interactions", group = TRUE)
        #compact_letters1.1 <- post_hoc_test$groups
        
        compact_letters1 <- post_hoc_test$groups %>%
          rownames_to_column(var = "Variables") %>% # Convert row names to a column
          separate(Variables, into = c("Tissue", "Year"), sep = "\\.") # Split "Group" column into "Tissue" and "Year"
        
        
        compact_letters1.1 <- compact_letters1 %>%
          select(-Concentration) %>%                 # Remove the "Concentration" column
          mutate(Monoletter = groups, .keep = "all") %>% # Add a replicated "groups" column as "Monoletters"
          dplyr::rename(Letter = groups)                   # Rename "groups" to "Letters"
      
    
        # Assign compact letter results for Tukey test
        if(all(compact_letters1.1[-1,3]== compact_letters1.1[1,3])){
          compact_letters1.1[,c(3:4)] <- paste(" ")
          compact_letters2 <- rbind(compact_letters2,compact_letters1.1)
        }else{
          compact_letters2 <- rbind(compact_letters2,compact_letters1.1)
        }

        
        # Store results in the list as a data frame
        test_results[[length(test_results) + 1]] <- data.frame(
          Pollutant = pollutant,
          Tissue = tissue,
          Year= compact_letters2[,2],
          TestType = "ANOVA",
          CompactLetters = compact_letters2[,3]
        )
   # For those pollutants that had less than 3 or no observations and hence couldn't be used to run tests
      }else if (all(data$TestResult == "<3/No observations")){
        print(paste(pollutant, tissue, "doesn't have enough obervations to run tests"))
        compact_letters_for_few_obs <- data.frame(matrix(ncol = 4, nrow = 3))
        compact_letters_names <-c("Tissue","Year","Letter","MonoLetter")
        colnames(compact_letters_for_few_obs) <- compact_letters_names
        compact_letters_for_few_obs[,1]<- data$Tissue
        compact_letters_for_few_obs[,2]<- data$Year
        compact_letters_for_few_obs[,c(3:4)]<- paste(" ")
        
        compact_letters2 <- rbind(compact_letters2,compact_letters_for_few_obs) 
        
        
        # Store results in the list as a data frame
        test_results[[length(test_results) + 1]] <- data.frame(
          Pollutant = pollutant,
          Tissue = tissue,
          Year= compact_letters2[,2],
          TestType = "No Test",
          CompactLetters = compact_letters2[,3]
        )
        
      }
      
      #Appending group comparisons tests results and test result names to list
      group_comparison_tests_list<-append(list(group_comparison_tests),group_comparison_tests_list, 0)
      name1 <-paste(pollutant, tissue, "group_comparison_tests_list", sep = " ")
      group_comparison_tests_names <- append(group_comparison_tests_names,name1)
      
 
      #Appending post hoc tests results and test result names to list
      post_hoc_tests_list<-append(list(post_hoc_test),post_hoc_tests_list, 0)
      name2 <-paste(pollutant, tissue, "post_hoc_tests_list", sep = " ")
      post_hoc_tests_names <- append(post_hoc_tests_names,name2)
    }
  }
  
  #adding names from name list to results from results list
  names(group_comparison_tests_list)<-group_comparison_tests_names
  
  names(post_hoc_tests_list)<-post_hoc_tests_names
  
  # Convert the test results list into a data frame
  test_results_df <- bind_rows(test_results)
 
  
  
  for (pollutant in pollutants) {
    
    #filtering the test_results_dataframe for each pollutant to merge with the summary statistics
    test_results_per_pollutant <- test_results_df  %>%
      filter(Pollutant == pollutant)
    
    #plucking out the summary statistics for each pollutant to merge with the test results
    summary_statistics_per_pollutant <-summary_statistics[[pollutant]]
   
    # Merging the test results dataframe and the summary statistics
    merged_data <- merge(summary_statistics_per_pollutant, test_results_per_pollutant, by = c("Pollutant", "Tissue", "Year"))
    
    #appending merged data frames to a list and naming these dataframes and appending them to a list
    final_summary_list<-append(list(merged_data),final_summary_list, 0)
    name <-paste(pollutant,"summary_stats", sep = " ")
    final_summary_names <- append(final_summary_names,name)
  
  }
  
  #Adding names to list
  names(final_summary_list)<-final_summary_names
  
  #return(final_summary_list, group_comparison_tests_list, and post_hoc_tests_list )
  return(list (final_summary_list=final_summary_list, group_comparison_tests_list=group_comparison_tests_list, post_hoc_tests_list=post_hoc_tests_list))
}#This function processes the data to run the group comparisons test, based on the normality test results, in tandem with the corresponding post hoc tests and modification of the summary statistic by adding the compact letter displays.

temporal_analysis_step2 <- temporal_analysis_step2 (temporal_analysis_step1) #Run Function #2






#Step 3----
temporal_analysis_step3 <- function(data_list){
# Unpack the data list into individual elements

summary_statistics <- data_list$final_summary_list
  
# Remove "summmary_stats" from the names for the first name of summaruy statistics dataset in list
pollutant_name <- gsub(" summary_stats","", names(data_list$final_summary_list)[1])


# Initialize empty lists to store results

plots_list <- list()
plot_names <- c()


if(grepl("Fe|Ag", pollutant_name)) { # if Fe or Ag is detected  as the name of the frist dataset in the list then run the below code fro heavy metals.
  filename <- "HMicons//" # heavy metal file containing icons for classification
  #recommended levels for heavy metals and organic compounds in mg/kg accumulated from different datasets:
  recommended_levels <- data.frame(pollutants=c("Ag","Cd","Co","Cu","Fe","Hg","Ni","Pb","Tl","Zn"), 
                   lower_recommended_levels=c(0.01,2,0.0016,30,100,0.1,0.05,8,0.45,30), upper_recommended_levels=c(rep(NA, 10)), 
                   levels=c('Grasso et al. 2021: 0.01mg/kg','FAO/WHO: <0.05-2mg/kg','EFSA: 0.0016mg/kg','ANVISA: 30mg/kg', 'FAO/WHO: 100mg/kg','Brodziak-Dopierała et al. 2023: 0.1mg/kg','FAO/WHO: <0.05-2mg/kg','FAO/WHO: <0.5-8mg/kg','Makridis and Amberger, 1996; LaCoste et al. 2001:\n <0.45-2.28mg/kg (permissible range for animal feed)','FAO/WHO: <30-100mg/kg'),oral_reference_dosage=c(0.005, 0.01, 0.03, 0.04, 0.7, 0.1, 0.003, 0.04,0.00001, 0.3))
}else{ # if Fe or Ag is not detected  as the name of the first dataset in the list then run the below code for organic compounds.
  filename <- "OCicons//" # Organic compound file containing icons for classification
  recommended_levels <- data.frame(pollutants=c("Adipic_acid","Caprolactam","Chlorpyrifos","Ibuprofen"), lower_recommended_levels=c(470, 50,0.01,40), upper_recommended_levels=c(NA, NA,NA,NA), levels=c('EPA: 470mg/kg/day','EPA: 50mg/kg/day', 'FAO/WHO: 0.01mg/kg/day','The Mayo Clinic: 40mg/kg/day'))
}


  # Creating vector of names for for loop
  pollutants <-  names(data_list[["final_summary_list"]])
  
  # Iterate over each unique pollutant in the summary statistics
  for (pollutant in pollutants) {
  
    contaminant <- gsub(" summary_stats","",  pollutant)
    
    # plucking out the recommended_maximum_levels (rml) for each pollutant
    rml <-recommended_levels %>% subset(pollutants == contaminant)
    
    
    # Filter data for the current pollutant
    summary_statistics_per_pollutant <- as.data.frame(summary_statistics[[pollutant]])
    
    #change column name (5th column/ mean column) to the pollutant name in  the summary statistics dataset
    colnames(summary_statistics_per_pollutant)[5] <-gsub(" summary_stats","",  pollutant)
    
    #creating a vector of the years for each summary statititc dataset to be used for plotting later
    years <- levels(factor(summary_statistics_per_pollutant$Year))
    
#Dynamic Plot Annotations used for pollutant classification. This piece of R code is checking for the existence of a PNG image file in the HMicons or OCicons folder. If the file exists,it reads it and processes it as a graphical object. If it doesnt exist it sets 'icon' to FALSE indicating that no image is available. Some pollutants for example Hg may have multiple classifications like pharmaceuticals, industry etc. hence why we have mutlpile icons as indicated by "1" and "2" in the following code.
if(file.exists(paste0(filename,contaminant,".png"))==TRUE){
  icon <- file.exists(paste0(filename,contaminant,".png"))
  icons = png::readPNG(paste0(filename,contaminant,".png")) %>%
    rasterGrob(x= 0.95, y=1.03,interpolate = TRUE, height = 0.11, width = 0.06)}else{  
      icon <- FALSE
    }
#_________________________________1
if(file.exists(paste0(filename,contaminant,"1",".png"))==TRUE){
  icon1 <- file.exists(paste0(filename,contaminant,"1",".png"))
  iconz = png::readPNG(paste0(filename,contaminant,"1",".png")) %>%
    rasterGrob(x= 0.88, y=1.03,interpolate = TRUE, height = 0.11, width = 0.06)}else{
      icon1 <- FALSE
    } 
#__________________________________________2
if(file.exists(paste0(filename,contaminant,"2",".png"))==TRUE){
  icon2 <- file.exists(paste0(filename,contaminant,"2",".png"))
  iconsz = png::readPNG(paste0(filename,contaminant,"2",".png")) %>%
    rasterGrob(x= 0.80, y=1.03,interpolate = TRUE, height = 0.11, width = 0.06)}else{
      icon2 <- FALSE
    } 
    
    
    #Checks if all the compact letter displays are the same for the summary statitics for each pollutant.
if (all(summary_statistics_per_pollutant[-1, 12] == summary_statistics_per_pollutant[1, 12])==FALSE){ # If they are different then then it runs the below code for plotting which essentially adds the letter displays on the corresponding bars using this piece of code "geom_text(aes(label=CompactLetters)"

  tissues <- c('liver', 'stomach', 'muscle', 'inksac')
  Colors <-setNames( c('red', 'green','blue'),years)
  barplot <- print(ggplot(summary_statistics_per_pollutant, aes(factor(x=Tissue, levels = tissues), y=!! rlang::sym(paste0(contaminant)), fill=factor(Year, levels=c('2019','2020','2021')))) +
                  geom_bar(stat='identity', color="black", position=position_dodge()) +scale_fill_manual(values=Colors) +
                  geom_errorbar(aes(ymin=!! rlang::sym(paste0(contaminant)),ymax=!! rlang::sym(paste0(contaminant))+SE),width=.1,position=position_dodge(.8))+
                  labs(title = paste0(contaminant))+
                    #checks if the icon exists if it does it adds to the graph if not it keeps it blank.
                  {if(icon==TRUE)annotation_custom(icons)}+
                  {if(icon1==TRUE)annotation_custom(iconz)}+
                  {if(icon2==TRUE)annotation_custom(iconsz)}+
                  coord_cartesian(clip = 'off')+
                  theme(plot.title = element_text(hjust = 0.5))+
                  labs(y = "Concentration mg/kg", x = "Tissue")+
                  labs(fill = "Years") + geom_text(aes(label=N), position = position_dodge(1), size = 3,vjust=1.5, color='#993300')+
                  geom_text(aes(label=CompactLetters), position = position_dodge(1), size = 5,vjust=-0.2, hjust=-0.005, colour ="black")+ 
                    #This below piece of code compares the value in the second column of the rml object (rml[, 2]) with the maximum value in the column of summary_statistics_per_pollutant corresponding to the variable contaminant.summary_statistics_per_pollutant[, paste(contaminant)] dynamically selects the column using paste(contaminant) (which converts the contaminant variable to a string, likely matching a column name).If rml[, 2] is greater than this maximum value, the block inside the if statement is executed. Otherwise, the else block is executed. If true, it adds a text annotation to a plot using the annotate() function from ggplot2. The x-coordinate of the text, calculated as 78% of the maximum unique factor level (converted to numeric) from the Tissue column. The y-coordinate of the text, calculated as 87% of the scaled maximum value of the contaminant column in summary_statistics_per_pollutant. label: The text to be displayed, taken from the fourth column of the rml object (rml[, 4]). The overall logic for the else part indicates if rml[, 2] is greater than the maximum value in the relevant contaminant column it annotates the plot with a bold, red text label at specified coordinates. Otherwise: it adds a dashed red horizontal line at the rml[, 2] value.
                  {if((rml[,2])>max(summary_statistics_per_pollutant[,paste(contaminant)]))annotate('text', x=max(as.numeric(as.factor(unique(summary_statistics_per_pollutant$Tissue))))*.78, y=max(summary_statistics_per_pollutant[,paste(contaminant)]*1.40)*.87, label= rml[,4],fontface='bold', size=2.2, color="red") else geom_hline(yintercept=rml[,2], linetype="dashed", color = "red")})
  plots_list<-append(list(barplot),plots_list, 0)
  name3 <-paste(contaminant,"barplot", sep = "")
  plot_names <- append(plot_names,name3)
} else { #If the compact letter displays are the same then it doesn't add the letter displays and does this using the below code. This makes for a more readable graph. Therefore the only difference between the two pieces of code in the if-else function is the "geom_text(aes(label=CompactLetters)" which is not in the "else" section onf the code.  
  tissues <- c('liver', 'stomach', 'muscle', 'inksac')
  Colors <-setNames( c('red', 'green','blue'),years)
  barplot <- print(ggplot(summary_statistics_per_pollutant, aes(factor(x=Tissue, levels = tissues), y=!! rlang::sym(paste0(contaminant)), fill=factor(Year, levels=c('2019', '2020', '2021')))) +
                    geom_bar(stat='identity', color="black", position=position_dodge()) +scale_fill_manual(values=Colors) +
                    geom_errorbar(aes(ymin=!! rlang::sym(paste0(contaminant)), ymax=!! rlang::sym(paste0(contaminant))+SE), width=.1, position=position_dodge(.8))+
                    labs(title = paste0(contaminant))+
                    {if(icon==TRUE)annotation_custom(icons)}+
                    {if(icon1==TRUE)annotation_custom(iconz)}+
                    {if(icon2==TRUE)annotation_custom(iconsz)}+
                    coord_cartesian(clip = 'off')+
                    theme(plot.title = element_text(hjust = 0.5))+
                    labs(y = "Concentration mg/kg", x = "Tissue")+
                    labs(fill = "Years")+geom_text(aes(label=N), position = position_dodge(1),size = 3,vjust=2, color='#993300')+
                    {if((rml[,2])>max(summary_statistics_per_pollutant[,paste(contaminant)]))annotate('text', x=max(as.numeric(as.factor(unique(summary_statistics_per_pollutant$Tissue))))*.78, y=max(summary_statistics_per_pollutant[,paste(contaminant)]*1.40)*.87, label= rml[,4],fontface='bold', size=2.2, color="red") else geom_hline(yintercept=rml[,2], linetype="dashed", color = "red")})
  plots_list<-append(list(barplot),plots_list, 0)
  name3 <-paste(contaminant,"barplot", sep = "")
  plot_names <- append(plot_names,name3)
  
}

names(plots_list)<-plot_names

}

return(list (plots=plots_list))
}#This function processes the summary statistics data to run the plots for each pollutant then save them in a list.

temporal_analysis_step3 <- temporal_analysis_step3 (temporal_analysis_step2) #Run Function #3





# Calling plots for visualization from saved list in the temporal_analysis_step3_dataset.----
do.call("grid.arrange", c(temporal_analysis_step3$plots[c(1,2,3,4,5, 6, 7,8,9,10)], ncol=5)) #Heavy Metals

do.call("grid.arrange", c(temporal_analysis_step3$plots[c(1,2,3,4,5,7,10)], ncol=4)) #Organic Compounds

