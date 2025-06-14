#LIBRARIES----
library(readxl)
library(dplyr)
library(janitor)
library(magrittr)
library(tidyr)
library(openxlsx)
library(stringr)

# STEP 1: LOADING DATASETS TO BE USED IN MAIN FUNCTION IN STEP 4----
trace_metal_batch<- read.csv("1-Data_Prep/Data/Metals_Raw/M_20_MLBatch.csv", header= FALSE)
dry_weight <- read.csv("1-Data_Prep/Data/Metals_Raw/Dry_wght/20drywght_M.csv", header=TRUE)# Has to be changed based on the year
squid_info <- read.csv("1-Data_Prep/Data/Squid_Catch/20_catch.csv", header= TRUE) # Has to be changed based on the year
distance_to_land <- read.csv("1-Data_Prep/Data/Squid_Catch/Dist_to_land.csv", header= TRUE) # Has to be changed based on the year


# STEP 2: STANDARD CONCENTRATION CALCULATION FUNCTION---- 
#This standard concentration dataset is used to find the LOQ, LOD, Slope and Intercept for the calibration concentrations in each batch to help classify the concentrations for the squid samples, for said batch, when processing the concentration datsaet later.
standard_concentration_calculation <- function(batch) {
#LOADING DATA----
# loading raw data..CHANGE BASED ON THE YEAR YOU ARE PROCESSING
results0<- batch
results1 <- as.data.frame(results0)
results <- results1[,-c(1:4)]

#SQUID CATCH DATA...CHANGE BASED ON THE YEAR YOU ARE PROCESSING
bi<- squid_info

# STEP 1: CALCULATING STANDARD CONCENTRATION (ppb) FOR EACH BATCH----
std_conc0 <- results[grep("CalStd", results[,1]), ]# removing all rows that have the calibration standards (top 10 rows)
std_conc0 = rbind(results[c(1:2),], std_conc0) #adding the column headings from the results dataset
std_conc0 = std_conc0[,-c(1,2)] # removing the 1st and 2nd columns
std_conc0[std_conc0=="CPS"] <- "Avg.intensity"# changing  CPS column name to average intensity.
std_conc0[std_conc0=="CPS RSD"] <- "std"# Changing CPS RSD column name to std (standard deviation)
std_conc1<-as.data.frame(t(std_conc0))# transposing the dataframe to run calculations
colnames(std_conc1) <-std_conc1[1, ]#putting first row as column names
std_conc1<-std_conc1[-1, ]#removing first row
std_conc2 <- matrix(unlist(std_conc1), ncol = 12) #unlisting dataset and converting it to a matrix
std_conc3 <-as.data.frame(matrix(as.numeric(std_conc2[,c(3:12)]), ncol = 10)) #turning values from charater back to numeric and convereting matrix into dataframe.
std_conc4 <- round(std_conc3, digits = 2) #rounding values to 2 decimal places
#names(std_conc4) <-NULL
cols<-std_conc1[,c(1:2)]#save the first two columns
std_conc6<- cbind(cols,std_conc4)# combining two datasets
cal_curv = c(0,1,2.5,5,7.5,10,20,30,40,50)# calibration curve points
cal_curvFe = c(0,2.5,7.5,10,20,30,40,50)# calibration curve points
cal_curvZn = c(0,1,2.5,7.5,10,20,30,40,50)# calibration curve points
cal_curvHg = c(0,1,2.5,5,7.5)# calbration curve point for HMs with low conc..
new_row <- data.frame(matrix(NA,ncol=ncol(std_conc6))) # making a new row and add to dataset
new_row[,2]<-"y-y0"#giving name to second column.
colnames(new_row) <- colnames(std_conc6) #adding column names to new row
#cols = as.data.frame(rbind(new_row, cols))

#including blank rows to in std_conc6 before each trace metal to calculate y-y0
std_conc7<- data.frame(matrix(ncol=ncol(std_conc6)))
colnames(std_conc7) <- colnames(std_conc6)
uu<-c(2:3)
for(cal in seq(1, nrow(std_conc6), by = 3)) {
  cale <- rbind(std_conc6[cal,],std_conc6[uu,])
  calv <- rbind(new_row,cale)
  std_conc7 = rbind(std_conc7,calv)
  uu<-uu+3
  if (cal == 28) {
    break
  }
}
#______________________________________________
colnames(std_conc7)[3:12] <- cal_curv #re-adding column names 

#calculating y-y0
for (ff in seq(2, nrow(std_conc7), by = 4)){
  for(zz in seq(3, ncol(std_conc7), by = 1)){
    std_conc7[ff,zz] <- std_conc7[ff+2,zz]-std_conc7[ff+2,3]# removing the average intensity of of the 0 caibration standard from the remaining calibration standards (y-Y0)  doing this helps us to subtract the noise from the calibration curve making it more accurate to help predict sample concetrations
}
}

#____________________________________________________
lod_loq <- data.frame(matrix(NA, nrow= nrow(std_conc7), ncol= 2))#making a dataframe fro LOD and LOQ
lm <- data.frame(matrix(NA, nrow= nrow(std_conc7), ncol= 2)) #making a dataframe to store the regression coefficients
b <- data.frame(matrix(NA, ncol=ncol(std_conc7)))
for(i in seq(2, nrow(std_conc7), by = 4)) { # for-loop over y-y0 rows
   if(str_detect(std_conc7[i+1,1], '[_F|_B|_C|_I|_J]') == TRUE){#used mainly for Metals_: F,B,C,I and J as they have low concentrations. the more points used the less accurate it is at low concentrations.
     bHg <- std_conc7[i,c(3:7)]#takes only the (0,1,2.5,5,7.5) cal curve for regression
     cHg <- lm(unlist(bHg) ~ 0 + cal_curvHg) #finding the regression coefficients, cal_curvHg = c(0,1,2.5,5,7.5)- Y intercept forced to) to avoid getting negative values
     lm[i,1] <- cHg$coefficients# saving the slope in the two column dataframe (lm)
     lm[i,2] <- std_conc7[i+2,3]# saving the intercept (avg. intensity) into (lm) 
    }else{ #finding the regression coefficients, those not Ag or Hg.
    b <- std_conc7[i,c(3:12)]  #cal_curv = c(0,1,2.5,5,7.5,10,20,30,40,50) for regression
    c  <- lm(unlist(b) ~ 0 + cal_curv)
    lm[i,1] <- c$coefficients
    lm[i,2] <- std_conc7[i+2,3]
}
}

#Calculating LOD and LOQ
for(k in seq(2, nrow(std_conc7), by = 4)) {
  lod_loq[k,1] <- round((lm[k,2]+3*std_conc7[k+3,3]*0.01*lm[k,2]),2)#LOD CALCULATION
  lod_loq[k,2] <- round((lm[k,2]+10*std_conc7[k+3,3]*0.01*lm[k,2]),2)#LOQ CALCULATION 
  #lod_loq[k,2] <- round((lod_loq[k,1]*10/3),2)
}

#Finishing up standard concentration data
std_conc<- data.frame(std_conc7=std_conc7, lm=lm, lod_loq=lod_loq) #combining the dataframes
colnames(std_conc)[1:2] <- c("Trace_metals"," ")
colnames(std_conc)[3:12] <- cal_curv
colnames(std_conc)[13:16] <- c('slope', 'intercept','LOD', 'LOQ')
std_conc[1,1] <- 'Calibration curve --->'


#checking year for dataset based on bi (squid basic information from fishing vessel/catch data) and saving it to year.
if (26 %in% bi$Area) {
  year<-2019
} else if(60 %in% bi$Area){
  year<-2020
}else{
  year<-2021
}

#WRITING STANDARD CONCENTRATION DAAZSET INTO CSV FILE
std_conc_res = paste0("1-Data_Prep/Data/clean/", year, "_Std_conc_ppb.csv")#CHECK POINT 1 (check std_conc) ----
write.table(std_conc, file = std_conc_res, sep = ",",#change the rows index
            append = TRUE, quote = FALSE,
            col.names = FALSE, row.names = FALSE)
return (list(standard_concentration=std_conc, results=results))
}
standard_concentration <- standard_concentration_calculation(trace_metal_batch) 



# STEP 3: HELPER FUNCTIONS USED IN PROCESSING VALUES IN DRYWEIGHT DATASET AND CONCENTRATION DATASET----
#FUNCTIONS TO HELP MODIFY SAMPLE AREA IN CONCENTRATION DATASET
sample_area_concentration_dataset_mod <- function(x){#padding 0 to first two digits
  if (str_detect(substring(x[1],1,2), '_') == TRUE){ 
    modified_area=print(paste('0',substring(x[1],1,)))
    modified_area<- gsub(" ", "",  modified_area)
    x[1] <- gsub(substring(x[1],1,), modified_area,  x[1])
  }
  x[1] =  gsub("-", "_",  x[1])
  return(x)
}

#HELPER FUNCTION TO HELP MODIFY SAMPLE ID IN CONCENTRATION DATASET
sample_id_concentration_dataset_mod<-function(x){ #padding 0 to second two digits
  if (str_detect(substring(x[1],4,5), '_') == TRUE){
    modified_id=print(paste('0',substring(x[1],4,)))
    modified_id<- gsub(" ", "",  modified_id)
    x[1] <- gsub(substring(x[1],4,), modified_id,  x[1])
  }
  return(x)
}

#HELPER FUNCTION TO HELP MODIFY SAMPLE ID IN DRY WEIGHT DATASET
sample_id_dry_weight_mod<-function(x){ #padding o to second two digits
  if (str_detect(substring(x[1],5), "^$") == TRUE){
    modified_id=print(paste('_0',substring(x[1],4,)))
    modified_id<- gsub(" ", "",  modified_id)
    print(paste(modified_id))
    x[1] <- gsub(substring(x[1],3,), modified_id,  x[1])
    print(paste(x[1]))
  }
  return(x)
}

# HELPER FUNCTION FOR ADDING DRY WEIGHT TO CONCENTRATION RESULTS IN SAMPLE SEPARATION DATASET (DRY WEIGHT DATASET)
add_dry_weight<- function(y,x){ #x= dryweight dataset, y=mass_spec_res dataset #WORKS
  i<-2
  x=x
  for(h in 1:nrow(y)){
    while((str_extract(y[h,1], "(?<=.....)[^_]") == colnames(x)[i]) ==FALSE) {
      if(i==ncol(x)){
        i<-2
      }
      if((str_extract(y[h,1], "(?<=.....)[^_]") == colnames(x)[i]) ==FALSE) {
        print(paste("This is h",h, "column", i))
        i<-i+1
        print(paste("This is h",h, "column", i))
      }
    }
    if((str_extract(y[h,1], "(?<=.....)[^_]") == colnames(x)[i]) ==TRUE) {
      for(j in 1:nrow(x)){
        if ((str_extract(y[h,1], "(?<=)[^_]+[^_]...")== x[j,1]) ==TRUE) { 
          y[,'DW'][h] <- x[j,i]
          print(paste("h", h, "matches with", "j",j+1,"column",i))
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

# HELPER FUNCTION FOR MODIFYING AREA FOR SQUID INFORMATION
sample_area_bi_mod<-function(x){ #padding 0 to Area column
  if (str_detect(substring(x[1],2), "^$") == TRUE){
    modified_area=print(paste('0',substring(x[1],1)))
    modified_area<- gsub(" ", "",  modified_area)
    print(paste(modified_area))
    x[1] <- gsub(substring(x[1],1), modified_area,  x[1])
  }
  return(x)
}


#HELPER FUNCTION FOR MODIFYING ID NUMBER FOR SQUID INFORMATION
sample_id_bi_mod<-function(x){ #padding 0 to ID_num column
  if (str_detect(substring(x[2],2), "^$") == TRUE){
    modified_id=print(paste('0',substring(x[2],1)))
    modified_id<- gsub(" ", "",  modified_id)
    print(paste(modified_id))
    x[2] <- gsub(substring(x[2],1), modified_id,  x[2])
    #print(paste(x[2]))
  }
  return(x)
}



# STEP 4: ACTIVATING MAIN FUNCTION TO BE USED FOR CONCENTRATION DATASET MANIPULATION---- 
#In this process the concentrations for the squid samples are calculated and classified with the help of the standard concentration from the previous functions and also the corresponding dry weight, squid catch data and distance to land datasets
preprocessed_trace_metals_concentration_dataset <- function(data_list, dry_weight, squid_info, distance_to_land, col_names) {
  
  #loading raw data for dry weight of samples... CHANGE BASED ON THE YEAR YOU ARE PROCESSING
  dry_weight <-dry_weight
  
  #SQUID CATCH DATA...CHANGE BASED ON THE YEAR YOU ARE PROCESSING
  bi<- squid_info
  
  #distance to Argentina and Falkland Islands.distance to land (dtl)
  dtl<- distance_to_land
  
  # results from previous function
  results <- data_list$results
  std_conc <- data_list$standard_concentration
  
# data cleaning and manipulation for concentration dataset
ext_res0 <- results[grep("ample", results[,1]), ]
ext_res0= ext_res0[-grep("ash", ext_res0[,3]), ]# To remove rows showing wash cycle
ext_res0 = rbind(results[c(1:2),], ext_res0)
ext_res0 = ext_res0[,c(-1,-2)]
ext_res1 <- matrix(unlist(ext_res0), ncol = ncol(ext_res0))

#column names with CPS RSD column
ext_rows_CPS<- data.frame(ext_res1[c(1:3),])
ext_rows_CPS[2,1]<-'ID'
colnames(ext_rows_CPS) <- ext_rows_CPS[2,]
ext_rows_CPS <- ext_rows_CPS[-2,]
ext_rows_CPS[,'DW']=NA
ext_rows_CPS = ext_rows_CPS[,c(1, 32, 2:31)]
colnames(ext_rows_CPS)[2] <- paste("DW")
colnames(ext_rows_CPS)[seq(5,ncol(ext_rows_CPS),3)] <- paste("(CPS_RSD)")
colnames(ext_rows_CPS)[seq(3,ncol(ext_rows_CPS),3)] <- paste("(Conc_ppb)")
colnames(ext_rows_CPS)[seq(4,ncol(ext_rows_CPS),3)] <- paste("(Avg.intensity)")


#column names with Conc_mg/kg column
ext_rows_Conc<- data.frame(ext_res1[c(1:3),])
ext_rows_Conc[2,1]<-'ID'
colnames(ext_rows_Conc) <- ext_rows_Conc[2,]
ext_rows_Conc <- ext_rows_Conc[-2,]
ext_rows_Conc[,'DW']=NA
ext_rows_Conc = ext_rows_Conc[,c(1, 32, 2:31)]
colnames(ext_rows_Conc)[seq(5,ncol(ext_rows_Conc),3)] <- paste("(Conc_mg/kg)")
colnames(ext_rows_Conc)[seq(3,ncol(ext_rows_Conc),3)] <- paste("(Conc_ppb)")
colnames(ext_rows_Conc)[seq(4,ncol(ext_rows_Conc),3)] <- paste("(Avg.intensity)")

#EXTRACTION RESULTS OF SAMPLES FOR DATA MANIPULATION
ext_res2 <- as.data.frame(tibble(matrix(as.numeric(ext_res1[c(3:nrow(ext_res1)),c(2:ncol(ext_res1))]), ncol = ncol(ext_res1)-1))) #take batch control as well in this
ext_res3 <- round(ext_res2, digits = 4)
names(ext_res3) <-NULL
ext_cols<-data.frame(ext_res1[-c(1:3),1])
ext_res4 <- data.frame(ext_res3)
ext_res4 = ext_res4[-1,]
ext_res4.1 = cbind(ext_cols, ext_res4)
colnames(ext_res4.1)[1] <- 'ID'
ext_res4.1$ID = gsub("-", "_", ext_res4.1$ID)
ext_res4.1$DW<- NA
ext_res4.1$ID = tolower(ext_res4.1$ID)

# #FUNCTIONS TO HELP MODIFY SAMPLE NAMES in concentration dataset
rw <- apply(ext_res4.1, MARGIN = 1, sample_area_concentration_dataset)
rw1<- apply(t(rw), MARGIN = 1, sample_id_concentration_dataset)
ext_res4.1.1 <- data.frame(t(rw1))



# ADDING DRY WEIGHT DATA TO CONCENTRATION DATASET
HM.1 <- dry_weight 
HM.2 <-data.frame(HM.1)
if(colnames(dry_weight)[ncol(dry_weight)]!= "Ink.sac..mg."){
  colnames(HM.2) <- c("ID","s","l","m") 
}else{
colnames(HM.2) <- c("ID","s","l","m","i")
}


HM.3<-data.frame(t(apply(HM.2,1, sample_id_dry_weight)))
HM.3$ID <- gsub("-", "_", HM.3$ID)

# #ADDING DRY WEIGHT TO EXTRACTION RESULTS DATAFRAME FROM SAMPLE SEPARATION DATASET
ext_res4.2 <- add_dry_weight(ext_res4.1.1,HM.3 )
ext_res4.2 <- ext_res4.2 %>% relocate(DW, .after = ID)
colnames(ext_res4.2) <- colnames(ext_rows_CPS)

#adding back batch-control row to dataframe
ext_res5 <- data.frame(rbind(ext_rows_CPS, ext_res4.2))
colnames(ext_res5) <- colnames(ext_rows_CPS)

print('1')
print(ext_res5)


#CONCENTRATION CALCULATIONS
#MAKING COLUMNS NUMERIC AGAIN
ext_res6.2 <- matrix(unlist(ext_res5), ncol = ncol(ext_res5))
ext_res6.3 <- data.frame(matrix(as.numeric(ext_res6.2[c(2:nrow(ext_res6.2)),c(2:ncol(ext_res6.2))]), ncol = ncol(ext_res6.2)-1))
ext_res6.4 <- cbind(ext_res5[-1,1], ext_res6.3)
colnames(ext_res6.4) <- colnames(ext_res5) 


# MAKING DATASET INTO LONG FORMAT FOR CALCULATIONS
ext_res6.5<- data.frame(matrix(nrow = 0, ncol = 4))
colnames(ext_res6.5) <- colnames(ext_res6.4)[1:4]
v <-4
for(i in seq(from=3, to=ncol(ext_res6.4), by=3)) {
  sl<-cbind(ext_res6.4[,c(1,2)], ext_res6.4[,c(i,v)])
  ext_res6.5 = rbind(ext_res6.5, sl)
  v<-v+3
  if (i == 10) {
    break
  }
}



# CALCULATING CONC_PPB,CONC_MGKG AND STATUS OF CONCENTRATION LEVELS
ext_res6.5$LOQ <- NA #including LOQ to check against average intensity
ext_res6.5$First_Status <- NA
ext_res6.5$MinusBC_ppb<-NA
ext_res6.5$Conc_mgkg <- NA
ext_res6.5$Final_Status <- NA
rownames(ext_res6.5) = rownames(nrow(ext_res5)*10)

fz <-c(1:nrow(ext_res6.4))
for (f in seq(2, nrow(std_conc), by = 4)){
  for(z in fz){
    ext_res6.5[z,'LOQ']<- std_conc[f,16]#include a column for the LOQ for each trace metal from standard concentration dataset.
    if (is.na(ext_res6.5[z,4]==TRUE)){ #if value in column 4 (average intensity) is NA then put 0 in the corresponding row in column 3 (conc ppb)
      ext_res6.5[z,3] <- 0
    }else if((ext_res6.5[z,4] >= std_conc[f,16])== TRUE){# if value in column 4 is more than/equal to value in standard concentration column 16 (LOQ) then subtract that value from standard concentration column 14 (intercept) and divide by the slope (standard concentration column 13) then result in same row of conc ppb column.
    ext_res6.5[z,3] = round(((ext_res6.5[z,4]-std_conc[f,14])/std_conc[f,13]),3)
    ext_res6.5[z,'First_Status'] = "above LOQ"
    }else if((ext_res6.5[z,4] < std_conc[f,16] & ext_res6.5[z,4] >= std_conc[f,15])== TRUE){# if value in column 4 is less than the value in standard concentration column 16 (LOQ) but more than LOD (standard concentration column 15) then subtract that value from standard concentration column 14 (intercept) and divide by the slope (standard concentration column 13) then result in same row of conc ppb column.
    ext_res6.5[z,3] = round(((ext_res6.5[z,4]-std_conc[f,14])/std_conc[f,13]),3) 
    ext_res6.5[z,'First_Status'] = "below LOQ"
    }else{(ext_res6.5[z,4] < std_conc[f,15])# if value in column 4 is less than value in standard concentration column 15 (LOD) then place 0 as the result in same row of conc ppb column.
      ext_res6.5[z,3] = 0
      ext_res6.5[z,'First_Status'] = "not detected"
    }
  } 
  #(print(c(f,z)))
  if(f == nrow(std_conc)-3){
    break
  }
   
  fz <- fz+nrow(ext_res6.4)
} 




#subtracting conc ppb from black control to find final concentration
fz <-c(1:nrow(ext_res6.4))
bz = 1 #blank control concentration
for (ff in seq(2, nrow(std_conc), by = 4)){
  for(zz in fz){  
    if (is.na(ext_res6.5[zz,3])|str_detect(ext_res6.5[zz,6], "not detected")==TRUE){#if row in column 3 or blank control is NA or it says "not detected" in column 6 then....
      ext_res6.5[zz,'MinusBC_ppb'] <- 0 #put zero in MinusBC_ppb column
      ext_res6.5[zz,'Conc_mgkg'] <- 0 #put 0 in Conc_mgkg column
      ext_res6.5[zz,'Final_Status'] = "not detected"#put "not detected in final status column.
     }else if (is.na(ext_res6.5[bz,3])==TRUE){#if blank control row is NA or if DW column then....
        ext_res6.5[bz,3] <- 0 #replace NA with 0
        ext_res6.5[zz,'Conc_mgkg'] <- 0 #put 0 in Conc-mg/kg column
      ext_res6.5[zz,'Final_Status'] = "not detected"#put "not detected in final status column.
    }else if((ext_res6.5[zz,3] < ext_res6.5[bz,3] & str_detect(ext_res6.5[zz,6], "above LOQ"))== TRUE){#if  row in column 3 is less than blank control in column 3 & it says above LOQ in column 6 then...
      ext_res6.5[zz,'MinusBC_ppb'] <- 0 # put 0 in MinusBC_ppb column
      ext_res6.5[zz,'Conc_mgkg'] <- paste(round(((ext_res6.5[zz,3]*100*5)/(ext_res6.5[zz,2]/1000)/1000),2),"BLOQ") #paste the calculated concentration mg/kg using the Concentration ppb BEFORE subtracting blank control and add BLOQ (to indicate that this concentration is Below Limit of Quantification)__
      ext_res6.5[zz,'Final_Status'] = "BLOQ" #"below blank" 
    }else if((ext_res6.5[zz,3] < ext_res6.5[bz,3] & str_detect(ext_res6.5[zz,6], "below LOQ"))== TRUE){#if  row in column 3 is less than blank control in column 3 & it says below LOQ in column 6 then...
      ext_res6.5[zz,'MinusBC_ppb'] <- 0
      ext_res6.5[zz,'Conc_mgkg'] <- paste(round(((ext_res6.5[zz,3]*100*5)/(ext_res6.5[zz,2]/1000)/1000),2),"BLOQ")#paste the calculated concentration mg/kg using the Concentration ppb BEFORE subtracting blank control + BLOQ (to indicate that this concentration is Below LOQ)
      ext_res6.5[zz,'Final_Status'] = "below LOQ"
    }else{
      ext_res6.5[zz,'MinusBC_ppb']<- ext_res6.5[zz,3] - ext_res6.5[bz,3]#else subtract the concentration ppb from blank control.
      ext_res6.5[zz,'Conc_mgkg'] <- round(((ext_res6.5[zz,7]*100*5)/(ext_res6.5[zz,2]/1000)/1000),2)#calculating conc mk/kg
      ext_res6.5[zz,'Final_Status'] = "detected"
    }
  }
    #(print(c(ff,zz,bz)))
    if(ff == nrow(std_conc)-3){
      break
    }
    bz<-bz+nrow(ext_res6.4)
    fz <- fz+nrow(ext_res6.4)
} 

#CHECK POINT 2 (check ext_res6.5) ----
#removing LOQ column
ext_res6.5 <-ext_res6.5[, -which(names(ext_res6.5) == "LOQ" )]


#MAKING DATASET BACK INTO WIDE FORMAT
ext_res7<- data.frame(matrix(nrow=nrow(ext_res6.4), ncol = 0))
u<-c(2:nrow(ext_res6.4))
k <- c(3:8)
for(i in seq(from=1, to=nrow(ext_res6.5), by=nrow(ext_res6.4))) {
  s<-rbind(ext_res6.5[i,k], ext_res6.5[u,k])
  ext_res7 = cbind(ext_res7, s)
  u<-u+nrow(ext_res6.4)
  #(print(i))
  if (i == nrow(ext_res6.5)-nrow(ext_res6.4)+1) {
    break
  }
}

#adding back sample names and dry weight columns back to concentration dataset
ext_res7 = cbind(ext_res6.4[,c(1:2)], ext_res7)
ext_res7 = ext_res7[-1,] #this is used to remove the blank control row
#_______________________________________________________


#Adding back trace metal names to column names to differentiate which columns describe which trace metals FOR the FINAL DATASET
ext_res0[1,] <- gsub("[^[:alnum:]]","", ext_res0[1,])
ext_res0[1,] <- gsub("He","", ext_res0[1,])
excolnames<-ext_res0[1,][seq(2,ncol(ext_res0),3)]
colnames(ext_res7)[seq(7,ncol(ext_res7),6)] <- paste(excolnames,"(Conc_mg/kg)", sep = "")
colnames(ext_res7)[seq(3,ncol(ext_res7),6)] <- paste(excolnames,"(Conc_ppb)", sep = "")
colnames(ext_res7)[seq(4,ncol(ext_res7),6)] <- paste(excolnames,"(Avg.intensity)", sep ="")
colnames(ext_res7)[seq(5,ncol(ext_res7),6)] <- paste(excolnames,"(First_Status)", sep ="")
colnames(ext_res7)[seq(8,ncol(ext_res7),6)] <- paste(excolnames,"(Final_Status)", sep ="")


#FINAL EXTRACTION RESULTS USED FOR STATISTICAL ANALYSIS
ext_res7.6 <- ext_res7 #saving it into a new datset


#changing capital letters in the sample names to small letters in case there are any capital letters
for (row in 2:nrow(ext_res7.6)){
  if (str_detect(ext_res7.6[row,1],"[[:upper:]]") == TRUE){
    ext_res7.6[row,1] = tolower(ext_res7.6[row,1])
  }else{
    ext_res7.6[row,1] = ext_res7.6[row,1]
  }
}

#TAKING OUT THE COLUMNS I DONT NEED
ext_res7.7 <- ext_res7.6 %>% select(-contains("ppb"))%>% select(-contains("intensity"))%>% select(-contains("Status"))

#SEPARATING ID NUMBER AND ADDING OTHER VARIABLES TO DATAFRAME
ext_res7.7$ID_num <- NA
ext_res7.7$Area <- NA
ext_res7.7$ Tissue <-NA
for (ro in 1:nrow(ext_res7.7)){ 
  ext_res7.7$Area[ro]= substring(ext_res7.7[ro,1],1,2)
  ext_res7.7$ Tissue[ro]=str_extract(ext_res7.7[ro,1], "(?<=_)[^_]+[^_]+[^_].*")
  ext_res7.7$ ID_num[ro]=str_extract(ext_res7.7[ro,1], "(?<=_)[^_]*")
}
#____________________________________________________________



#ADDING BASIC SQUID INFORMATION FROM THE FISHING VESSEL AND LAB AND APPENDING TO BATCHES

#FOR SETTING UP CATCH DATA F0R SQUIDS MAKE SURE ID IS A CHARACTER
bi$ID <- as.character(bi$ID)
bi$Area <- as.character(bi$Area)
#AREA


#formatting samples names to match concentration dataset
bi1<-data.frame(t(apply(bi,1, sample_area_bi_mod)))
binfo<-data.frame(t(apply(bi1,1, sample_id_bi_mod)))

#ADDING BASIC INFO TO DATASET
ext_res7.7$Gender <- NA
ext_res7.7$Longitude <- NA
ext_res7.7$Latitude <- NA
ext_res7.7$Month_of_Capture <- NA
ext_res7.7$Mantle_length_mm <- NA
ext_res7.7$Wet_Weight_g <- NA
ext_res7.7$Maturity_level <- NA
h <- 1 
while(h!= nrow(ext_res7.7)+1){

  for (i in 1:nrow(binfo)){
    if ((ext_res7.7$Area[h] == binfo$Area[i] && ext_res7.7$ID_num[h] == binfo$ID[i])==TRUE){
      ext_res7.7$Longitude[h] <- binfo$Longitude[i]
      ext_res7.7$Latitude[h] <- binfo$Latitude[i]
      ext_res7.7$Month_of_Capture[h] <- binfo$Month_of_Capture[i]
      ext_res7.7$Mantle_length_mm[h] <- binfo$Mantle_length_mm[i]
      ext_res7.7$Wet_Weight_g[h] <- binfo$Wet_Weight_g[i]
      # Check if Gender column exists.. Only females were caught for 2019 which is why no gender was included for 2019,
      if ("Gender" %in% colnames(binfo)) { #0 = Females, 1 = Males
        ext_res7.7$Gender[h] <- binfo$Gender[i]
      } else{ # this means that if binfo doesnt have gender then it must be from 2019 catch data
        ext_res7.7$Gender[h] <- 0
      }
      ext_res7.7$Maturity_level[h] <- binfo$Maturity_level[i]
      print(paste('This is ext_res7.7', h,'and bi1', i))#tells you which row in the datasets match.
      h <- h+1
      if (h == nrow(ext_res7.7)+1){
        break
      }
    }
  }
}

print('4')
print(ext_res7.7)
#formatting samples names to match concentration dataset
dtl$Area. <- as.character(dtl$Area.)
dtl1<-data.frame(t(apply(dtl,1, sample_area_bi_mod)))
#dtl2<-data.frame(t(apply(dtl1,1, dck)))
#ADDING distance to Argentina and Falkland Islands
ext_res7.7$dta_km <- NA
ext_res7.7$dtfl_km <- NA
h <- 1 
while(h!= nrow( ext_res7.7)+1){
  for (i in 1:nrow(dtl1)){
    if (( ext_res7.7$Area[h] == dtl1$Area.[i])==TRUE){
      ext_res7.7$dta_km[h] <- dtl1[i,4]
      ext_res7.7$dtfl_km[h] <- dtl1[i,5]
      print(paste('This is  ext_res7.7', h,'and dtl2', i))
      h <- h+1
      if (h == nrow( ext_res7.7)+1){
        break
      }
    }
  }
}

#Checking year based on ID numbers
if (26 %in% ext_res7.7$Area) {
  ext_res7.7$Year<-2019
} else if(60 %in% ext_res7.7$Area){
  ext_res7.7$Year<-2020
}else{
  ext_res7.7$Year<-2021
}

#REARRANGING COLUMNS FOR FINAL DATASET
ext_res7.7=ext_res7.7[,c(1,2, 25, 13:16, 23:24 , 17:22, 3:12)]

#This is to use only keeping trace metals names e.g "Fe" for as column names 
colnames(ext_res7.7)[16:20] <- substring(colnames(ext_res7.7[16:20]),3,4)
colnames(ext_res7.7)[21:25] <- substring(colnames(ext_res7.7[21:25]),4,5)

#CHECK POINT 3 (check ext_res7.7) 

#____________________________________________________________
#Final_HMresults_mgkg.csv"


#APPENDING FUTURE BATCHES TO MAKE ONE EXTRACTION RESULTS FILE
Final_res = "1-Data_Prep/Data/Clean/Fnl_Mres_mgkg.csv"
write.table(ext_res7.7, file = Final_res, sep = ",",
            append = TRUE, quote = FALSE, 
            col.names = col_names, row.names = FALSE) #CHANGE COLUMN NAMES TO FALSE AFTER PROCESSING FIRST BATCH

return( list(Results=ext_res7.7,squid_catch_data=binfo, distance_to_land=dtl1))
}

# STEP 5: RUNNING MAIN FUNCTION WITH REQUIRED DATASETS----
trace_metals_concentration_dataset <- preprocessed_trace_metals_concentration_dataset(standard_concentration, dry_weight, squid_info, distance_to_land, col_names = FALSE) 
