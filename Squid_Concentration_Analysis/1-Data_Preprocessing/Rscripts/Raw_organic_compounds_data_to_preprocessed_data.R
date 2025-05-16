#LOADING LIBRARIES----
library(readxl)
library(dplyr)
library(magrittr)
library(stringr)
library(stringi)
library(gdata)



#STEP 1: SUPPLEMENTARY FUNCTIONS USED IN PROCESSING DRYWEIGHT AND CONCENTRATION DATASET----

#STRING MANINIPULATION FUNCTION #1 FOR DATA CLEANING
data_manipulation1<- function(x){
  colnames(x) = gsub("\\..", "_", colnames(x)) 
  x$Sample_Name <- gsub("_D10000", "", x$Sample_Name)
  x$Sample_Name <- gsub("*NAT*", "", x$Sample_Name)
  x$Sample_Name <- gsub("\\**", "", x$Sample_Name)
  x$Tissue <- str_extract(x$Sample_Name, "^[^_]*")#Takes only the first characters before the underscore
  x$Tissue <- gsub("_", "", x$Tissue)
  x$Tissue <- gsub("\\d+$", "", x$Tissue)
  fid <- str_extract(x[,1], "(?<=_)[^_]+[^_]") 
  #print(fid)
  x$Sample_Name <- gsub("-", "_", x$Sample_Name)
  x$Sample_Name <- paste(fid,x$Tissue, sep = "_")
  x$Sample_Name = tolower(x$Sample_Name)
  x$Sample_Name <- gsub("-", "_", x$Sample_Name)
  x$Sample_Name <- gsub(" ", "", x$Sample_Name)
  return(x)
}

#1 HELPER FUNCTIONS TO HELP MODIFY SAMPLE IDS FOR CONCENTRATION DATASET
sample_area_concentration_dataset_mod <- function(x){#padding 0 to first two digits
  if (str_detect(substring(x[1],1,2), '_') == TRUE){ 
    modified_area=print(paste('0',substring(x[1],1,)))
    modified_area<- gsub(" ", "",  modified_area)
    x[1] <- gsub(substring(x[1],1,), modified_area,  x[1])
  }
  x[1] =  gsub("-", "_",  x[1])
  return(x)
}

sample_id_concentration_dataset_mod <-function(x){ #padding 0 to second two digits
  if (str_detect(substring(x[1],4,5), '_') == TRUE){ 
    modified_id=print(paste('0',substring(x[1],4,)))
    modified_id<- gsub(" ", "",  modified_id)
    x[1] <- gsub(substring(x[1],4,), modified_id,  x[1]) 
  }
  return(x)
}

#STRING MANINIPULATION FUNCTION #2 FOR DATA CLEANING 
Data_manipulation2<- function(x){
  x$ID_num <- str_extract(x$Sample_Name, "(?<=_)..")
  x$ID_num <- gsub("-", "", x$ID_num)
  x$Area= str_extract(x$Sample_Name, "(?<=)..")
  x$dry_weight=NA
  x$conc_mg_kg=NA
  x$ID_num <- gsub("_", "", x$ID_num)
  x[,'Cal_Conc.'] <- gsub("N/A", 0,  x$Cal_Conc.)# replace N/A with 0 because no peak was detected meaning no concentration of the small molecule was detected.
  x$Cal_Conc. <- gsub("BLOQ", 9999999,  x$Cal_Conc.) #replace BLOQ with 999999
  x$Cal_Conc. <- gsub("< 0", 9999999,  x$Cal_Conc.) #replace < 0 with 999999
  x$Cal_Conc. <- gsub(" ","",  x$Cal_Conc.)
  x$Cal_Conc. = as.numeric( x$Cal_Conc.)
  x<-select(x, -contains(c('Expected_RT','Height','Retention_Time','Retention_Time_Delta_(min)','Accuracy','Ion_Ratio', 'Component_Name','Actual_Concentration','Used','X','Dilu_Factor','Back_al_Conc.')))
  return(x)
}

#2 HELPER FUNCTIONS TO HELP MODIFY SAMPLE IDS FOR DRYWEIGHT DATASET
sample_area_dry_weight_mod <- function(x){#padding 0 to first two digits
  x[1] =  gsub("-", "_",  x[1])
  #print(paste(x[1]))
  if (str_detect(substring(x[1],1,2), '_') == TRUE){
    modified_area=print(paste('0',substring(x[1],1,)))
    modified_area<- gsub(" ", "",  modified_area)
    x[1] <- gsub(substring(x[1],1,), modified_area,  x[1])
  }
  return(x)
}

sample_id_dry_weight_mod <-function(x){ #padding 0 to second two digits
  if (str_detect(substring(x[1],5), "^$") == TRUE){
    modified_id=print(paste('_0',substring(x[1],4,)))
    modified_id<- gsub(" ", "",  modified_id)
    x[1] <- gsub(substring(x[1],3,), modified_id,  x[1])
  }
  return(x)
}

#HELPER FUNCTION TO ADD DRYWEIGHT TO CONCENTRATION DATASET
#combining formatted sample names in dry weight dataset to the concentration dataset for mg/kg calculation
add_dry_weight <- function(y,x){ #x= dryweight dataset, y=mass_spec_res 
  i<-2
  x=x
  for(h in 1:nrow(y)){
    while((str_extract(y[h,1], "(?<=.....)[^_]") == colnames(x)[i]) ==FALSE) {
      if(i==ncol(x)){
        i<-2
      }
      if((str_extract(y[h,1], "(?<=.....)[^_]") == colnames(x)[i]) ==FALSE) {
        #print(paste("This is h",h, "column", i))
        i<-i+1
        #print(paste("This is h",h, "column", i))
      }
    }
    if((str_extract(y[h,1], "(?<=.....)[^_]") == colnames(x)[i]) ==TRUE) {
      for(j in 1:nrow(x)){
        if ((str_extract(y[h,1], "(?<=)[^_]+[^_]...")== x[j,1]) ==TRUE) { 
          y[,'dry_weight'][h] <- x[j,i]
          #print(paste("h", h, "matches with", "j",j,"column",i))
          if((y$Cal_Conc.[h] == "9999999")!=TRUE){
            y[,'conc_mg_kg'][h] <- round(as.numeric(y$Cal_Conc.[h])/as.numeric(y$dry_weight[h])*1000/1000,digits = 3)#calculating concentration mg/kg if the row in Cal_conc. is not equal to 9999999
          }else{
            y[,'conc_mg_kg'][h] <- paste(as.numeric(y[,'LOQ'][h]),"BLOQ") #if the row in Col.conc is equal to 999999 then paste LOQ it to BLOQ
          }
        }else{
          j<-j+1
        }
      }
      
      if (h == nrow(y)){
        break
      }
    }
  }
  return(y)
}

#HELPER FUNCTION TO MODIFY SAMPLE IDS FOR DTL AND BI DATASET 
sample_area_bi_and_dtl_dataset_mod<-function(x){ #padding 0 TO AREA COLUMN
  if (str_detect(substring(x[1],2), "^$") == TRUE){
    modified_area=print(paste('0',substring(x[1],1)))
    modified_area<- gsub(" ", "",  modified_area)
    #print(paste(modified_area))
    x[1] <- gsub(substring(x[1],1), modified_area,  x[1])
    #print(paste(x[1]))
  }
  return(x)
}

#ID NUMBER
sample_id_bi_and_dtl_dataset <-function(x){ #padding 0 to ID_NUMBER COLUMN
  if (str_detect(substring(x[2],2), "^$") == TRUE){
    modified_id=print(paste('0',substring(x[2],1)))
    modified_id <- gsub(" ", "",  modified_id)
    #print(paste(modified_id))
    x[2] <- gsub(substring(x[2],1), modified_id,  x[2])
    #print(paste(x[2]))
  }
  return(x)
}




# STEP 2: ACTIVATING MAIN FUNCTION TO BE USED IN CONCENTRATION DATASET MANIPULATION---- 
#In this process the concentrations for the squid samples are calculated  with the help of the previous functions in STEP 1 and also the corresponding dry weight, squid catch data and distance to land datasets
preprocessed_organic_compounds_concentration_dataset <- function(files, path, dry_weight, squid_info, distance_to_land, col_names){

# loading datasets  
files <- files
dtl <- distance_to_land 
bi <- squid_info
#saving path for dataset to be used later

path <- path

OCcsv <- list()
for (i in 1:length(files)) {
  OCcsv[[i]] <- read.csv(paste0(path,files[i]), header = T, sep = ",", dec = ".")
}

#adding csv files to list
ocnames <- c('Organic_A','Organic_B','Organic_C','Organic_D')

names(OCcsv) <- ocnames


if(colnames(dry_weight)[ncol(dry_weight)]!= "Ink.sac..mg."){
  colnames(dry_weight) <- c("ID","s","l","m") 
}else{
  colnames(dry_weight) <- c("ID","s","l","m","i")
}

OC <- lapply(OCcsv, data_manipulation1) #This is to clean the data by removing unwanted columns and manipulating the strings to modify the sample names
# #Check point 1 
#------------------------------------------------------------------------------------------------

#2 code to activate function for sample name modification 
OC1 <- list()
temp <- data.frame(matrix(NA, ncol=ncol(OC[[1]])))
temp1 <- data.frame(matrix(NA, ncol=ncol(OC[[1]])))
for (l in 1:length(OC)){
  temp <- apply(OC[[l]], MARGIN = 1, sample_area_concentration_dataset)
  temp1 <- apply(t(temp), MARGIN = 1, sample_id_concentration_dataset)
  OC1<-append(list(data.frame(t(temp1))),OC1, 0)
}
names(OC1)<-names(OC)

OC2<-lapply(OC1, Data_manipulation2) #This is to create new columns as well as modify the calculated concentration column

#check point 2 
#------------------------------------------------------------------------------------------

#adding sample separation data (dry weight) to concentration dataset and calculating concentration mg/kg
temp2 <- apply(dry_weight, MARGIN = 1, sample_area_dry_weight) #pad 0 to site in dry weight sample names
dry_weight1 <- apply(t(temp2), MARGIN = 1, sample_id_dry_weight) #pad 0 to ID num in dry weight sample names
dry_weight <- data.frame(t(dry_weight1))

#combining formatted sample names in dry weight dataset to the concentration dataset for mg/kg calculation
OC3 <- list() 
temp4 <- data.frame(matrix(NA, ncol=ncol(OC2[[1]])))
temp5 <- data.frame(matrix(NA, ncol=ncol(OC2[[1]])))
for (l in 1:length(OC2)){
  temp5 <- add_dry_weight(OC2[[l]], dry_weight)#add dry weight to dataframes
  OC3<-append(list(temp5),OC3, 0)
}
names(OC3)<-names(OC2)
#check point 3

#----------------------------------------------------------------------------------
#STEP 3: ADDING CONCENTRATIONS MG/KG FROM EACH DATASET IN A NEW DATASET TO RESEMBLE HEAVY METALS DATASET----
OC4 <- data.frame(matrix(NA, nrow=nrow(OC2[[1]]), ncol=length(OC2)+5))
OC4[,1]<-OC3[[1]][,1]
OC4[,2]<-OC3[[1]][,2]
OC4[,3]<-OC3[[1]][,9]
OC4[,4]<-OC3[[1]][,8]
OC4[,5]<-OC3[[1]][,7]
colnames(OC4)[1]<-colnames(OC2[[1]])[1]
colnames(OC4)[2]<-colnames(OC2[[1]])[2]
colnames(OC4)[3]<-colnames(OC2[[1]])[9]
colnames(OC4)[4]<-colnames(OC2[[1]])[8]
colnames(OC4)[5]<-colnames(OC2[[1]])[7]
colnames(OC4)[1:length(names(OC3))+5]<-names(OC3)


k <- 6
h <- 1
l <- 1
x <- OC4

print('This is OC.2')
print(x)

while (l!= length(OC3)+1){
    if (names(OC3)[[l]]== colnames(x)[k]){
        print(paste('L is ', l))
            for (i in 1:nrow(OC4)){
              print(paste('L is ', l,'in the loop'))
                  if((OC3[[1]][i,1] == x[h,1]) ==TRUE) {
                    x[h,k] <- OC3[[l]][i,'conc_mg_kg']
                    h <- h+1
                     }
              if(h == nrow(x)+1){
                h <- 1
                break
              }
              }
      l <- l+1
      
    }else{
      k <- k+1
    }
}


x$Year <- case_when(
  26 %in% x$Area ~ 2019,
  60 %in% x$Area ~ 2020,
  TRUE ~ 2021
)



x <- x %>% relocate(Year, .after = dry_weight)

#check point 4----

#formatting samples names to match concentration dataset
dtl$Area. <- as.character(dtl$Area.) 
dtl1<-data.frame(t(apply(dtl,1, sample_area_bi_and_dtl_dataset)))
dtl2<-data.frame(t(apply(dtl1,1, sample_id_bi_and_dtl_dataset)))
print(dtl1)
#ADDING distance to Argentina and Falkland Islands
x$dta_km <- NA
x$dtfl_km <- NA
h <- 1 
while(h!= nrow(x)+1){
  for (i in 1:nrow(dtl2)){
    if ((x$Area[h] == dtl2$Area.[i])==TRUE){
      x$dta_km[h] <- dtl2[i,4]
      x$dtfl_km[h] <- dtl2[i,5]
      print(paste('This is  x', h,'and dtl2', i))
      h <- h+1
      if (h == nrow(x)+1){
        break
      }
    }
  }
}


#FOR SETTING UP CATCH DATA F0R SQUIDS MAKE SURE "ID" AND "AREA' COLUMNS ARE CHARACTERS
bi$ID <- as.character(bi$ID)
bi$Area <- as.character(bi$Area)

#formatting samples names to match concentration dataset
bi1<-data.frame(t(apply(bi,1, sample_area_bi_and_dtl_dataset)))
binfo<-data.frame(t(apply(bi1,1, sample_id_bi_and_dtl_dataset)))

#ADDING CATCH DATA TO DATASET
x$Gender <- NA
x$Longitude <- NA
x$Latitude <- NA
x$Month_of_Capture <- NA
x$Mantle_length_mm <- NA
x$Wet_Weight_g <- NA
x$Maturity_level <- NA
h <- 1 
while(h!= nrow(x)+1){
  
  for (i in 1:nrow(binfo)){
    if ((x$Area[h] == binfo$Area[i] && x$ID_num[h] == binfo$ID[i])==TRUE){
      x$Longitude[h] <- binfo$Longitude[i]
      x$Latitude[h] <- binfo$Latitude[i]
      x$Month_of_Capture[h] <- binfo$Month_of_Capture[i]
      x$Mantle_length_mm[h] <- binfo$Mantle_length_mm[i]
      x$Wet_Weight_g[h] <- binfo$Wet_Weight_g[i]
      # Check if Gender column exists.. Only females were caught for 2019 which is why no gender was included for 2019,
      if ("Gender" %in% colnames(binfo)) { #0 = Females, 1 = Males
        x$Gender[h] <- binfo$Gender[i]
      } else{ # this means that if binfo doesn't have gender then it must be from 2019 catch data
        x$Gender[h] <- 0
      }
      x$Maturity_level[h] <- binfo$Maturity_level[i]
      print(paste('This is x', h,'and bi1', i))#tells you which row in the datasets match.
      h <- h+1
      if (h == nrow(x)+1){
        break
      }
    }
  }
}


#REARRANGING COLUMNS FOR FINAL DATASET
x=x[,c(1:6,13, 11:12,14:19, 7:10)]
print('This is x')
print(x)
colnames(x)[c(1,3)] <- c("ID", "DW")

   # #EXPORTING EXTRACTION RESULTS FILE INTO CSV/EXCEL
   # write.csv(x, "Results/Final_SMresults_mgkg.csv", row.names = FALSE)
   
   #APPENDING FUTURE BATCHES TO EXTRACTION RESULTS CSV
   Final_res_OC = "Datasets/Results/Final_OCresults_mgkg.csv"
   write.table(x, file = Final_res_OC, sep = ",",#change the rows index
               append = TRUE, quote = FALSE,
               col.names = col_names, row.names = FALSE)

   return(x)
   
   }
   


  
# STEP 3: LOADING DATASETS TO BE USED IN MAIN FUNCTION IN STEP 5----

#below code goes through the excel file and makes each sheet into one CSV file and saves all the CSV files in one folder with todays date and time as the name. each CSV file represents a organic compound only need to use once. UNMUTE IF YOU WANT TO START FROM THE EXCEL FILES 

#-------------------------------------------RUN FIRST AND ONLY ONCE TO CHOOSE ORGANIC COMPOUND (OC) EXCEL FILE, THEN GO BACK IN SAME FOLDER TO SEE ANOTHER FOLDER WITH CSV FILES (IGNORE WARNING MESSAGE) ALSO YOU CAN CHANGE THE NAME TO MAKE IT MORE RELATABLE IN MY EXAMPLE I RENAMED THE FILES WITH THE SAME NAME AS THE EXCEL FILES --------------------------------------------#


# require(gdata)
# excelFile <- file.choose() # Ask for Excel file
# names = sheetNames(excelFile)
# noOfSheets = length(names)
# out <- paste(dirname(excelFile), as.POSIXct(Sys.time()), sep="/") # same directory as source file + timestamp to avoid override
# dir.create(out)
# for (i in 1:noOfSheets) {
#   currentSheet <- read.xls(excelFile, sheet=i)
#   write.csv(currentSheet, file=paste(out, paste(names[i], "csv", sep="."), sep="/"), row.names = FALSE)
# }


#STEP 4: LOADING DATASETS TO BE USED IN STEP 5---- 
#taking the CSV files from the folder and making them into a list (each file/organic compounds has its own dataset) #PLEASE REMEMBER TO CHANGE DATASET TO CORRECT YEAR

files <- list.files(path = "Datasets/Organic_Compounds_Raw_Data/2021_OC/")
path <- "Datasets/Organic_Compounds_Raw_Data/2021_OC/"

dry_weight <- read.csv("Datasets/Organic_Compounds_Raw_Data/Dry_weight_data /2021dryweight_SM.csv", header= TRUE)

#distance to Argentina and Falkland Islands.distance to land (dtl)
distance_to_land <- read.csv("Datasets/Squid_Catch_Data/Distance_to_land.csv", header= TRUE)

#SQUID CATCH DATA...CHANGE BASED ON THE YEAR YOU ARE PROCESSING, Basic information = bi
squid_info <- read.csv("Datasets/Squid_Catch_Data/2021_catch_data.csv", header= TRUE)


#STEP 5: RUNNING MAIN FUNCTION WITH REQUIRED DATASETS----
organic_compound_concentration_dataset <- preprocessed_organic_compounds_concentration_dataset(files, path, dry_weight, squid_info, distance_to_land, col_names = FALSE)  
   