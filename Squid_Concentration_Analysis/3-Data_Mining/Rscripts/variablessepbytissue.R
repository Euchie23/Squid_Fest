#Loading libraries----
library(ARTool)
library(readr)
library(ggplot2)
library(Rmisc)
library("gridExtra") 
library(multcompView)
library(dplyr)
library(mvnormtest)
library(rcompanion)
library(pgirmess)
library(FSA)
library(car)
library(factoextra)
library(corrplot)
library(vegan)
library(DescTools)
library(corrplot)
library(dunn.test)
library(stringr)
library(stringi)
library(tidyverse)
library(purrr)
library(patchwork)
library(reshape)
library(grid)
library(ggrepel)
library(ggpubr)
library(ggtext)

#HM datasets----

# FUNCTIONS TO HELP MODIFY AREA NUMBERS FOR FURTHER ANALYSIS (HM)
dcc <- function(x){#padding 0 to first two digits
  if (str_detect(substring(x[4],1,2),"\\.") == TRUE){ 
    sk=print(paste('0',substring(x[4],1,)))
    sk<- gsub(" ", "",  sk)
    x[4] <- gsub(substring(x[4],1,), sk,  x[4])
  }
  return(x)
}
#Final resuitls for Heavy Metals
hm1<- read.csv("Chem_lab_excel_files /R_Exported_excel:CSV_files/final results HM & SM/HM_results_forTSUNGHAN.csv", header= TRUE)
hm1[,4] <- paste0(hm1[,4], ".")
hmfull <- as.data.frame(t(apply(hm1, MARGIN = 1, dcc)))
hmfull$Area <- gsub("\\.", "",hmfull$Area)
hmfull[,c(17:26)] <- lapply(hmfull[,c(17:26)], gsub, pattern = ".*BLOQ.*", replacement = 0)
hmfull[,c(17:26)] <- lapply(hmfull[,c(17:26)], gsub, pattern = ".*BB.*", replacement = 0)
hmfull[,c(17:26)] <- lapply(hmfull[,c(17:26)], gsub, pattern = "^0$", replacement = 0)
hmfull[,c(17:26)]  <- lapply(hmfull[,c(17:26)] , gsub, pattern = "N/A", replacement = 0)
hmfull[,c(7,10:12,14:26)] =as.numeric(unlist(hmfull[,c(7,10:12,14:26)]))
hmfull[,10]<- cut(hmfull[,10], breaks = 3, labels =c(2,3,4))
hmfull[,7]<- cut(hmfull[,7], breaks = 2, labels =c(0,1))
hmfull[, 12] <- as.numeric(gsub(2811.1, 281.1, hmfull[, 12]))
hmfull[,13]<- as.character(hmfull[,13])
hmfull[,3]<- as.character(hmfull[,3])
# hmfull[,15]<- cut(hmfull[,15], breaks = 4, labels =c(200, 400, 600, 800))
# hmfull[,16]<- cut(hmfull[,16], breaks = 5, labels =c(100, 130, 140, 150, 190))


#SM datasets----

# FUNCTIONS TO HELP MODIFY AREA NUMBERS FOR FURTHER ANALYSIS (SM)
dkc <- function(x){#padding 0 to first two digits
  if (str_detect(substring(x[4],1,2),"\\.") == TRUE){ 
    sk=print(paste('0',substring(x[4],1,)))
    sk<- gsub(" ", "",  sk)
    x[4] <- gsub(substring(x[4],1,), sk,  x[4])
  }
  return(x)
}

sm2<- read.csv("Chem_lab_excel_files /R_Exported_excel:CSV_files/final results HM & SM/SM_results_forTSUNGHAN.csv", header= TRUE)
sm2[,4] <- paste0(sm2[,4], ".")
colnames(sm2)[4]<- "Area"
sm1 <- as.data.frame(t(apply(sm2, MARGIN = 1, dkc)))
sm1$Area <- gsub("\\.", "",sm1$Area)
sm <- sm1 %>% relocate(Area, .after = ID_num)
sm[,c(17:30)] <- lapply(sm[,c(17:30)], gsub, pattern = ".*BLOQ.*", replacement = 0)
sm[,c(17:30)] <- lapply(sm[,c(17:30)], gsub, pattern = "N/A", replacement = 0)
sm[,c(17:30)] <- lapply(sm[,c(17:30)], gsub, pattern = "^0$", replacement = 0)
sm[,c(10:12,15:16,17:30)] =as.numeric(unlist(sm[,c(10:12,15:16,17:30)]))
sm[, 12] <- as.numeric(gsub(2811.1, 281.1, sm[, 12]))
sm[,10]<- cut(sm[,10], breaks = 3, labels =c(2,3,4))
sm[,6]<- tolower(sm[,6])
sm[,7]<- as.character(sm[,7])
sm[,3]<- as.character(sm[,3])
sm[,13]<- as.character(sm[,13])
#sm[,11]<- cut(sm[,11], breaks = 4, labels =c(150, 250, 350, 450))
#sm[,15]<- cut(sm[,15], breaks = 4, labels =c(200, 400, 600, 800))
#sm[,16]<- cut(sm[,16], breaks = 5, labels =c(100, 130, 140, 150, 190))
sm1 <- sm 

#working to automatically create plots using tidyverse, ggplot and for loop----
#generating the markdown for reading images:
#SMicons for Small Molecules
SMiconz <- data.frame(SM=c("Adipic_acid","Aminobenzoic_acid","Caprolactam","Chlorpyrifos","Diaminohexane","Estradiol","Ethylene_glycol","Ibuprofen","Metolachlor","Nortestosterone","Sulpiride","Terephthalic_acid","Toluidine","Tolycaine"), icons=c("https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=yeuBdleHsCXN&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=15168&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=108787&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=108787&format=png","https://img.icons8.com/?size=38&id=15168&format=png","https://img.icons8.com/?size=38&id=108787&format=png","https://img.icons8.com/?size=38&id=108787&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=15168&format=png","https://img.icons8.com/?size=38&id=108787&format=png"))
urls <-SMiconz$icons
names(urls) <- SMiconz$SM

#HMicons for Heavy Metals:
HMiconz <- data.frame(SM=c("Ag","Cd","Co","Cu","Fe","Hg","Ni","Pb","Tl","Zn"), icons=c("https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png"))
urlz <-HMiconz$icons
names(urlz) <- HMiconz$SM

#prepping markdown for ggplot. This is used to turn the url text into an image.
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
markdownfuncsm <- function(x){
  #step 1 saving the columns as vectors
  y <- paste0(x[1],"<img src=", "SMicons//", x[1], ".png", " width='17'/>")
  return(y)
}
#For Heavy Metals:
markdownfunchm <- function(x){
  if(file.exists(paste0("HMicons//",x[1],".png"))&file.exists(paste0("HMicons//",x[1],"1",".png"))&file.exists(paste0("HMicons//",x[1],"2",".png"))==TRUE){
    y <- paste0(x[1]," ","<img src=", "HMicons//", x[1], ".png", " width='17'/>"," ", " ","<img src=","HMicons//", x[1],"1",".png"," width='17'/>"," ", " ","<img src=","HMicons//", x[1],"2",".png"," width='17'/>")
  }else if(file.exists(paste0("HMicons//",x[1],".png"))&file.exists(paste0("HMicons//",x[1],"1",".png"))==TRUE){
    y <- paste0(x[1]," ", " ","<img src=", "HMicons//", x[1], ".png", " width='17'/>"," ", " ","<img src=", "HMicons//", x[1],"1", ".png", " width='17'/>")
  }else{
    y <- paste0(x[1]," ","<img src=", "HMicons//", x[1], ".png", " width='17'/>")
  }
  return(y)
}
forcof <- function (x, per_of_outl_for_yaxis=.95){
#x=dfnz
  M <- levels(factor(x$SM)) #remeber to change based on HM orSM
  tis <- levels(factor(x$Tissue))
  yr <- levels(factor(x$Year))
  s<-1
  ctrfull1 <- data.frame(matrix(ncol=8, nrow = 0))
  while(s != 4) {
    ctr0 <- data.frame(matrix(ncol=8, nrow = 0))
    ctr1 <- data.frame(matrix(ncol=8, nrow = 0))
    colnames(ctr1) <- c('Tissue','Year', 'SM','conc','distance','rho', 'pval' ,'yul')
    colnames(ctr0) <- c('Tissue','Year','SM', 'conc', 'distance','rho', 'pval', 'yul')
    ctrfull <- data.frame(matrix(ncol=8, nrow = 0))
    colnames(ctrfull) <- colnames(ctr1)
    colnames(ctrfull1) <- colnames(ctr1)
    year <- filter(x, Year == yr[s])
    for(h in 1:length(M)){
      metmol <- filter(as.data.frame(year), SM == M[h])
      filtbymet <- filter(as.data.frame(x), SM == M[h])
      outl  <-which(filtbymet$conc > quantile(filtbymet$conc,.99))
      outls <- filtbymet$conc[c(outl)]
      if(length(outls)!=0){
        metmol2 <- filtbymet[-c(outl),]
        maxconc <- max(metmol2$conc)
        minoutl <- min(outls)
        #print(M[h])
        yscale <-minoutl*per_of_outl_for_yaxis 
        #print(yscale)
      }else{
        yscale <-0
        maxconc <- max(as.numeric(filtbymet$conc))
      }
      for (i in 1:length(tis)){
        tissue <- filter(metmol, Tissue == tis[i])
        filtbytis <- filter(filtbymet, Tissue == tis[i])
        filtbyyear <- filter(filtbytis, Year == yr[s])
        #conc_max <- max(filtbytis$conc) #taking the maximum concentration to make into an X-coord
        if(nrow(tissue)<2|all(tissue[-1,4] == tissue[1, 4])==TRUE){
          next
        }else if(max(tissue$conc)!= 0 & yr[s]== '2019'){
          ctr <- cor.test(as.numeric(as.factor(tissue$distance)), as.numeric(tissue$conc), method="spearman", exact = FALSE)
          ctr0[1,1] <- tis[i]
          ctr0[1,2] <- yr[s]
          ctr0[1,3] <- M[h]
          ctr0[1,4] <- signif((yscale*.85), 3)# X-coord for r coefficient and p-value
          if(unique(filtbytis$place) == 'dtfl_km'){
            ctr0[1,5] <- max(filtbytis$distance)*.2
          }else if (unique(filtbytis$place) == 'dta_km'){
            ctr0[1,5] <- max(filtbytis$distance)*.5
            #ctr0[1,5] <- max(as.numeric(as.factor(filtbytis$distance)))*.2
          }else if (unique(filtbytis$place) == 'Mantle_Length_mm') {
            ctr0[1,5] <- max(filtbytis$distance)*.7
          } else{
          ctr0[1,5] <- max(as.numeric(as.factor(filtbytis$distance)))*.5
          }
          ctr0[1,6] <- ctr$estimate
          ctr0[1,7] <- ctr$p.value
          ctr0[1,8] <- yscale
          ctrfull <- rbind(ctrfull, ctr0)
        }else if(max(tissue$conc)!= 0 & yr[s]== '2020'){
          ctr <- cor.test(as.numeric(as.factor(tissue$distance)), as.numeric(tissue$conc), method="spearman", exact = FALSE)
          ctr0[1,1] <- tis[i]
          ctr0[1,2] <- yr[s]
          ctr0[1,3] <- M[h]
          ctr0[1,4] <- signif((yscale*.75), 3)
          if(unique(filtbytis$place) == 'dtfl_km'){
            ctr0[1,5] <- max(filtbytis$distance)*.2
          }else if (unique(filtbytis$place) == 'dta_km'){
            ctr0[1,5] <- max(filtbytis$distance)*.5
          }else if (unique(filtbytis$place) == 'Mantle_Length_mm') {
            ctr0[1,5] <- max(filtbytis$distance)*.7
          } else{
            ctr0[1,5] <- max(as.numeric(as.factor(filtbytis$distance)))*.5
          }
          ctr0[1,6] <- ctr$estimate
          ctr0[1,7] <- ctr$p.value
          ctr0[1,8] <- yscale
          ctrfull <- rbind(ctrfull, ctr0)
        }else if(max(tissue$conc)!= 0 & yr[s]== '2021'){
          ctr <- cor.test(as.numeric(as.factor(tissue$distance)), as.numeric(tissue$conc), method="spearman", exact = FALSE)
          ctr0[1,1] <- tis[i]
          ctr0[1,2] <- yr[s]
          ctr0[1,3] <- M[h]
          ctr0[1,4] <- signif((yscale*.65), 3)
          if(unique(filtbytis$place) == 'dtfl_km'){
            ctr0[1,5] <- max(filtbytis$distance)*.2
          }else if (unique(filtbytis$place) == 'dta_km'){
            ctr0[1,5] <- max(filtbytis$distance)*.5
          }else if (unique(filtbytis$place) == 'Mantle_Length_mm') {
            ctr0[1,5] <- max(filtbytis$distance)*.7
          } else{
            ctr0[1,5] <- max(as.numeric(as.factor(filtbytis$distance)))*.5
          }
          ctr0[1,6] <- ctr$estimate
          ctr0[1,7] <- ctr$p.value
          ctr0[1,8] <- yscale
          ctrfull <- rbind(ctrfull, ctr0)
        }
      }
    }
    s<-s+1
    ctrfull1 <- rbind(ctrfull1, ctrfull)
    ctrfull1$Tissue <- as.character(ctrfull1$Tissue)
    ctrfull1$Year <- as.character(ctrfull1$Year)
    ctrfull1$SM <- as.character(ctrfull1$SM)
  }
  return (ctrfull1)
}
finalcof <- function(x){
x$rho <- signif(x$rho, 4)
x$pval <- signif(x$pval)
x$rho<-ifelse(x$pval>0.05|x$pval==0, x$rho==NA, x$rho)
x$pval<-ifelse(x$pval>0.05|x$pval==0, x$pval==NA,x$pval)
x$rho <- paste0("r=",x$rho)
x$pval <- paste0("p-val=",x$pval)
x$pval[duplicated(x$pval)] <- NA
x$rho[duplicated(x$rho)] <- NA
x$rho <- gsub(".*NA.*", NA,x$rho)
x$pval <- gsub(".*NA.*", NA,x$pval)
x$rho[is.na(x$rho)] <- 0
x$pval[is.na(x$pval)] <- 0
x$pval[is.nan(x$pval)] <- 0
y <-as.data.frame(subset(x, pval!= 0))
return(y)
}
variablessepbytissue <- function(y, remove.zeroes=FALSE){
#y=hmfull
list0 <- list()
list1 <- list()
list2 <- list()
list3 <- list()
list0names <- c()
list1names <- c()
list2names <- c()
list3names <- c()
if(str_detect(colnames(y[17]),'Fe')==TRUE){
  range <- colnames(y[17:26])#Fe:Pb HM
  icons_hm <- apply(HMiconz, 1, markdownfunchm)
  names(icons_hm) <- names(urlz)
  icons_markdown <- icons_hm
  numrang <- 15:26
}else{
  range <- colnames(y[17:30])#Adipic_acid:Tolycaine SM
  icons_sm <- apply(SMiconz, 1, markdownfuncsm)
  names(icons_sm) <- names(urls)
  icons_markdown <- icons_sm
  numrang <- 15:30
}
for(h in 1:4){ 
  ndf <-y[,c(7,9,10:13,numrang, 6,3)]# streamlining dataframe for efficiency change back to 26 from 30 for heavy metals
  tis <- levels(factor(y[,6]))
  for(i in 1:8) {
    hm <-colnames(ndf)[i] 
     if(remove.zeroes==FALSE){
    dfnz<- ndf %>% pivot_longer(all_of(range), names_to = "SM", values_to = "conc")%>%pivot_longer (!! rlang::sym(paste0(hm)), names_to = "place", values_to = "distance")
    }else{
    dfnz<- ndf %>% pivot_longer(all_of(range), names_to = "SM", values_to = "conc")%>%pivot_longer (!! rlang::sym(paste0(hm)), names_to = "place", values_to = "distance")%>%subset(conc !=0)
    }
    nzT<-dfnz %>% group_by(SM) %>% subset(Tissue == tis[h])

    #print(nzT)
    nzP <- forcof(nzT)
    pvals <- finalcof(nzP)
    nzV <-nzP[-c(4:7)]
    nzV2 <- nzV %>% dplyr::distinct(SM, yul)
    nzYcoord <- nzT%>% left_join(nzV2, by=c('SM'='SM'))
    repfunc <-function(x){
      if(is.na(x[14])==TRUE){
        x[14]<-x[11]
      }
      return(x)
    }
    nzY1 <-data.frame(t(apply(nzYcoord, 1, repfunc)))
    colnames(nzY1) <- colnames(nzYcoord)
    nzY1$yul <- as.numeric(nzY1$yul)
    nzY1.1<-nzY1 %>% group_by(SM) %>% filter(yul==max(yul))
    nzY2<-nzY1.1 %>% dplyr::distinct(SM, yul)
    if(str_detect(colnames(y[17]),'Fe')==TRUE){
      df_scales <- data.frame(
        SM = c("Ag", "Cd", "Co","Cu", "Fe", "Hg","Ni", "Pb", "Tl", "Zn"),
        ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ymax = c(NA),
        n = c(5, 5, 5,5, 5, 5, 5, 5, 5,5))
      
      df_scales %<>% inner_join(nzY2, by= "SM") %>%
        mutate(ymax = coalesce(yul)) %>%select(SM, ymin, ymax, n)
      
      df_scales <- split(df_scales, df_scales$SM)
      scales <- lapply(df_scales, function(x) {
        scale_y_continuous(limits = c(x$ymin, x$ymax), n.breaks = x$n)
      })
    }else{
      df_scales <- data.frame(
        SM = c("Adipic_acid","Aminobenzoic_acid","Caprolactam","Chlorpyrifos","Diaminohexane","Estradiol","Ethylene_glycol","Ibuprofen","Metolachlor","Nortestosterone","Sulpiride","Terephthalic_acid","Toluidine","Tolycaine"),
        ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ymax = c(NA),
        n = c(5, 3, 4,3, 3, 3, 5, 2, 2,3, 5, 2, 2,3))
      
      df_scales %<>% inner_join(nzY2, by= "SM") %>%
        mutate(ymax = coalesce(yul)) %>%
        select(SM, ymin, ymax, n)
      
      df_scales <- split(df_scales, df_scales$SM)
      scales <- lapply(df_scales, function(x) {
        scale_y_continuous(limits = c(x$ymin, x$ymax), n.breaks = x$n)
      })
    }
    print(tis[h])
    print(hm)
    print(pvals)
    # print(nzT[,c(7:13)])
    #print(scales)
    if(tis[h]=='inksac'){
      if(hm=='Latitude'){
        plt <-nzT %>% ggplot(aes(distance, conc, colour = Year, group=Year)) +
          geom_smooth(method=lm, se=FALSE)+
          labs(title = paste(tis[h],hm,"Vs Conc mg/kg",sep =" "),
               y = "Concentration mg/kg", x = paste(hm))+
          facet_wrap(vars(SM), labeller = as_labeller(icons_markdown), scales ="free_y", ncol=2, drop = TRUE) +
          ggh4x::facetted_pos_scales(y = scales)+ 
           theme_icons()+
          geom_point(aes(shape = Year, color = Year), size = 2)+
          {if(nrow(pvals)!=0) geom_text(pvals, mapping=aes(label=paste(rho, pval, sep= ",")),hjust =-0.7, size = 3.5, fontface="italic", position = position_dodge(width = .1), check_overlap = FALSE)}
        list0<-append(list(plt),list0, 0)
        name0 <- paste(hm,"plots", tis[h], sep = "")
        list0names <- append(list0names,name0)
      }else{
      plt <-nzT %>% ggplot(aes(distance, conc, colour = Year, group=Year)) +
        geom_smooth(method=lm, se=FALSE)+
        labs(title = paste(tis[h],hm,"Vs Conc mg/kg",sep =" "),
             y = "Concentration mg/kg", x = paste(hm))+
        facet_wrap(vars(SM), labeller = as_labeller(icons_markdown), scales ="free_y", ncol=3, drop = TRUE) +
        ggh4x::facetted_pos_scales(y = scales)+ 
        theme_icons()+
        geom_point(aes(shape = Year, color = Year), size = 2)+
        {if(hm=='dta_km|dtfl_km')scale_x_continuous(limits = c(min(dfnz$distance), max(dfnz$distance)))}+
        {if(nrow(pvals)!=0) geom_text(pvals, mapping=aes(label=paste(rho, pval, sep= ",")),hjust =-0.7, size = 3.5, fontface="italic", position = position_dodge(width = .1), check_overlap = FALSE)}
      list0<-append(list(plt),list0, 0)
      name0 <- paste(hm,"plots", tis[h], sep = "")
      list0names <- append(list0names,name0)
      }
    }else if(tis[h]=='liver'){
      if(hm=='Latitude'){
        plt <-nzT %>% ggplot(aes(distance, conc, colour = Year, group=Year)) +
          geom_smooth(method=lm, se=FALSE)+
          labs(title = paste(tis[h],hm,"Vs Conc mg/kg",sep =" "),
               y = "Concentration mg/kg", x = paste(hm))+
          facet_wrap(vars(SM), labeller = as_labeller(icons_markdown), scales ="free_y", ncol=2, drop = TRUE) +
          ggh4x::facetted_pos_scales(y = scales)+ 
          theme_icons()+
          geom_point(aes(shape = Year, color = Year), size = 2)+
          {if(nrow(pvals)!=0) geom_text(pvals, mapping=aes(label=paste(rho, pval, sep= ",")),hjust =-0.7, size = 3.5, fontface="italic", position = position_dodge(width = .1), check_overlap = FALSE)}
        list1<-append(list(plt),list1, 0)
        name1 <- paste(hm,"plots", tis[h], sep = "")
        list1names <- append(list1names,name1)
      }else{
        plt <-nzT %>% ggplot(aes(distance, conc, colour = Year, group=Year)) +
          geom_smooth(method=lm, se=FALSE)+
          labs(title = paste(tis[h],hm,"Vs Conc mg/kg",sep =" "),
               y = "Concentration mg/kg", x = paste(hm))+
          facet_wrap(vars(SM), labeller = as_labeller(icons_markdown), scales ="free_y", ncol=3, drop = TRUE) +
          ggh4x::facetted_pos_scales(y = scales)+ 
          theme_icons()+
          geom_point(aes(shape = Year, color = Year), size = 2)+
          {if(nrow(pvals)!=0) geom_text(pvals, mapping=aes(label=paste(rho, pval, sep= ",")),hjust =-0.7, size = 3.5, fontface="italic", position = position_dodge(width = .1), check_overlap = FALSE)}
        list1<-append(list(plt),list1, 0)
        name1 <- paste(hm,"plots", tis[h], sep = "")
        list1names <- append(list1names,name1)
      }
    }else if(tis[h]=='muscle'){
      if(hm=='Latitude'){
        plt <-nzT %>% ggplot(aes(distance, conc, colour = Year, group=Year)) +
          geom_smooth(method=lm, se=FALSE)+
          labs(title = paste(tis[h],hm,"Vs Conc mg/kg",sep =","),
               y = "Concentration mg/kg", x = paste(hm))+
          facet_wrap(vars(SM), labeller = as_labeller(icons_markdown), scales ="free_y", ncol=2, drop = TRUE) +
          ggh4x::facetted_pos_scales(y = scales)+ 
          theme_icons()+
          geom_point(aes(shape = Year, color = Year), size = 2)+
          {if(nrow(pvals)!=0) geom_text(pvals, mapping=aes(label=paste(rho, pval, sep= ",")),hjust =-0.7, size = 3.5, fontface="italic", position = position_dodge(width = .1), check_overlap = FALSE)} 
        list2<-append(list(plt),list2, 0)
        name2 <- paste(hm,"plots", tis[h], sep = "")
        list2names <- append(list2names,name2)
      }else{
        plt <-nzT %>% ggplot(aes(distance, conc, colour = Year, group=Year)) +
          geom_smooth(method=lm, se=FALSE)+
          labs(title = paste(tis[h],hm,"Vs Conc mg/kg",sep =" "),
               y = "Concentration mg/kg", x = paste(hm))+
          facet_wrap(vars(SM), labeller = as_labeller(icons_markdown), scales ="free_y", ncol=3, drop = TRUE) +
          ggh4x::facetted_pos_scales( y = scales)+ 
          theme_icons()+
          geom_point(aes(shape = Year, color = Year), size = 2)+
          {if(nrow(pvals)!=0) geom_text(pvals, mapping=aes(label=paste(rho, pval, sep= ",")),hjust =-0.7, size = 3.5, fontface="italic", position = position_dodge(width = .1), check_overlap = FALSE)}
        list2<-append(list(plt),list2, 0)
        name2 <- paste(hm,"plots", tis[h], sep = "")
        list2names <- append(list2names,name2)
      }
    }else{
      if(hm=='Latitude'){
        plt <-nzT %>% ggplot(aes(distance, conc, colour = Year, group=Year)) +
          geom_smooth(method=lm, se=FALSE)+
          labs(title = paste(tis[h],hm,"Vs Conc mg/kg",sep =" "),
               y = "Concentration mg/kg", x = paste(hm))+
          facet_wrap(vars(SM), labeller = as_labeller(icons_markdown), scales ="free_y", ncol=2, drop = TRUE) +
          ggh4x::facetted_pos_scales(y = scales)+ 
          theme_icons()+
          geom_point(aes(shape = Year, color = Year), size = 2)+
          geom_text(pvals, mapping=aes(label=paste(rho, pval, sep = ",")),hjust =-0.7, size = 3.5, fontface="italic", position = position_dodge(width = .1), check_overlap = FALSE) 
        list3<-append(list(plt),list3, 0)
        name3 <- paste(hm,"plots", tis[h], sep = "")
        list3names <- append(list3names,name3)
      }else{
        plt <-nzT %>% ggplot(aes(distance, conc, colour = Year, group=Year)) +
          geom_smooth(method=lm, se=FALSE)+
          labs(title = paste(tis[h],hm,"Vs Conc mg/kg",sep =" "),
               y = "Concentration mg/kg", x = paste(hm))+
          facet_wrap(vars(SM), labeller = as_labeller(icons_markdown), scales ="free_y", ncol=3, drop = TRUE) +
          ggh4x::facetted_pos_scales(y = scales)+ 
          theme_icons()+
          geom_point(aes(shape = Year, color = Year), size = 2)+
          {if(nrow(pvals)!=0) geom_text(pvals, mapping=aes(label=paste(rho, pval, sep= ",")),hjust =-0.7, size = 3.5, fontface="italic", position = position_dodge(width = .1), check_overlap = FALSE)} 
          #stat_cor(method = "spearman", aes(label =paste(r.label, "pvalue",..p.., sep = "~")), position = position_dodge(width = .1), hjust=-0.7, size = 3.5)
          #{if(hm=='dta_km|dtfl_km')scale_x_continuous(limits = c(min(dfnz$distance), max(dfnz$distance)))}+
          # geom_text(pvals, mapping=aes(label=paste(rho, pval, sep = ",")),hjust =-0.7, size = 3.5, fontface="italic", position = position_dodge(width = .1), check_overlap = FALSE) 
        list3<-append(list(plt),list3, 0)
        name3 <- paste(hm,"plots", tis[h], sep = "")
        list3names <- append(list3names,name3)
      }
    }
    names(list0)<-list0names
    names(list1)<-list1names
    names(list2)<-list2names
    names(list3)<-list3names
  }
}
 return(list (inksac=list0, liver=list1, muscle=list2, stomach=list3))

}

smvstnz <- variablessepbytissue (sm1, remove.zeroes = TRUE)

for(i in 1:length(smvstnz$inksac))
{
  png(paste0(names(smvstnz$inksac)[i], ".png"), width = 1400, height = 800)
  plot(smvstnz$inksac[[i]],title = names(smvstnz$inksac)[i])
  dev.off()
}