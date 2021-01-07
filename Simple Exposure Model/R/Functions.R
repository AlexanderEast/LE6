
read.individuals <- function(filename){

file <- str_c("../Input/",filename)
i    <- read_excel(file, sheet = "Individuals")
e    <- data.frame(i[,c("Individual","n")])

# Bodyweight (For PBPK)
e$BWKG     <- i$`Bodyweight (kg)`

# Dust Inhalation Factor  
e$DUSTF    <- i$`Dust Ingestion Rate (g/day)`*i$`Dust Ingestion AF`

# Dermal Dust Absorption Factor
e$DDUSTF   <- i$`Dermal Dust Load (g/m3)`*i$`Dermal Dust Transfer Coefficient (m2/h)`*
              i$`Dermal Dust Time (hr)`*i$`Dermal Dust AF`

# Indoor Air Inhalation Factor
e$INDOORF  <- i$`Inhalation Rate (m3/day)`*i$`Fraction Time Indoors (h/day)`*
              i$`Inhalation AF`

# Outdoor Air Inhalation Factor
e$OUTDOORF <- i$`Inhalation Rate (m3/day)`*i$`Fraction Time Outdoors (h/day)`*
              i$`Inhalation AF`

# Water Ingestion Factor
e$WATERF   <- i$`Water Intake (L/day)`*i$`Liquid AF`

# Beverage Ingestion Factor
e$BEVF     <- i$`Beverage Intake (L/day)`*i$`Liquid AF`

# Food Ingestion Factor
e$FOODF    <- i$`Bodyweight (kg)`*i$`Dietary AF`

# Soil Ingestion Factor
e$SOILF    <- i$`Soil Ingestion (g/day)`*i$`Soil AF`

# PK FACTOR
e$PKF      <- i$`Vd (Volume Distribution, ml/kg bw)`*
              i$`kP (Elimination Rate, day -1)`

# Append Seed
e$SEED     <- as.numeric(read_excel(file, sheet = "Metadata")$Seed)
return(e)
}

nanounits <-function(x){
  
  numerics <- c("Min","Max","Median","Mean","SD","GM","GSD","P10","P25","P75","P90","P95","P99")
  x[,numerics] <- x[,numerics] %>% lapply(function(x) as.numeric(as.character(x)))
  
  x<- x %>%mutate(UNITFACTOR = case_when(
    (Units %in% c("ng/m³","ng/L","ng/g","µg/kg","ug/kg","pg/mL","pg/ml")) ~ 1,
    (Units %in% c("pg/m³","pg/g")) ~ 0.001,
    (Units %in% c("ng/mL","ug/l","µg/L","ug/m³","µg/m³")) ~ 1000)) %>%
    mutate_at(numerics,~.*UNITFACTOR) %>%
    mutate(Units = case_when(
      (Units %in% c("ug/m3","µg/m³","pg/m³","ng/m³")) ~ "ng/m³",
      (Units %in% c("ng/mL","ug/l","ug/L","µg/l","pg/ml","pg/mL","ng/L")) ~ "ng/L",
      (Units %in% c("pg/g","µg/kg","ug/kg","ng/g")) ~ "ng/g")) %>%
    select(-UNITFACTOR)
  
  return(x)
}

GMGSD.estimator <- function(x){
  
  # A. Estimate GM using Pleil 1. 
  x <- x %>% mutate(GM = if_else(!is.na(GM),GM , Median))
  
  # B. Estimate GM using Pleil 2.
  x <- x %>% mutate(GM = if_else(!is.na(GM),GM , Mean/(1+0.5 *(SD/Mean)^2)))
  
  # C. Estimate GSD using Pleil 1.
  x <- x %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P10/GM)/qnorm(.10)))) %>%
    mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P25/GM)/qnorm(.25)))) %>%
    mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P75/GM)/qnorm(.75)))) %>%
    mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P90/GM)/qnorm(.90)))) %>%
    mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P95/GM)/qnorm(.95)))) %>%
    mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P99/GM)/qnorm(.99))))
  
  # D. Estimate GSD using Pleil 2.
  x <- x %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(sqrt(2 * log(Mean/GM)))))
  # E. Estimate GM using Pleil 3.
  x <- x %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(Max/GM)/qnorm(1-1/`Sample Size`))))
  x <- x %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(Min/GM)/qnorm(1/`Sample Size`))))
  
  # F. Estimate Mean and SD using "Estimating.xlsx" Methods (5) and (16). Mean calculated from min, median, maximum,
  # and SD from minimum, median, maximum, and range.
  
  x <- x %>% mutate(Mean = if_else(!is.na(Mean),Mean, (Min+2*Median+Max)/4))
  x <- x %>% mutate(SD = if_else(!is.na(SD),SD, sqrt ((1/12) * ((Min-2*Median+Max)^2)/4 + (Max-Min)^2)))
  
  
  # G. Estimate SD using Ramirez & Cox Method and range rule. Applied only if sample size  > 10.
  x <- x %>% mutate(SD = if_else((!is.na(SD) & `Sample Size` > 10),SD, (Max-Min)/ (3*sqrt(log(`Sample Size`))-1.5)))
  x <- x %>% mutate(SD = if_else((!is.na(SD) & `Sample Size` > 10),SD, (Max-Min)/4))
  
  # ______________________________ Repeat A - E. ______________________________ #
  
  
  # H. Estimate GM using Pleil 1.
  x <- x %>% mutate(GM = if_else(!is.na(GM),GM , Median))
  
  # I. Estimate GM using Pleil 2.
  x <- x %>% mutate(GM = if_else(!is.na(GM),GM , Mean/(1+0.5 *(SD/Mean)^2)))
  
  # J. Estimate GSD using Pleil 1.
  x <- x %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P10/GM)/qnorm(.10)))) %>%
    mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P25/GM)/qnorm(.25)))) %>%
    mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P75/GM)/qnorm(.75)))) %>%
    mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P90/GM)/qnorm(.90)))) %>%
    mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P95/GM)/qnorm(.95)))) %>%
    mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(P99/GM)/qnorm(.99))))
  
  # K. Estimate GSD using Pleil 2.
  x <- x %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(sqrt(2 * log(Mean/GM)))))
  
  # L. Estimate GM using Pleil 3.
  x <- x %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(Max/GM)/qnorm(1-1/`Sample Size`))))
  x <- x %>% mutate(GSD = if_else(!is.na(GSD),GSD ,exp(log(Min/GM)/qnorm(1/`Sample Size`))))
  
  return(x)
}

prompt.samplesize <- function(x){
  
  maxss <- max(rbind.fill(x)$`Sample Size`)
  
  minsamples <- suppressWarnings((as.numeric(readline(prompt = "Enter the smallest sample size for concentraiton studies: "))))
  
  while(!(minsamples %in% 1:maxss)){
    msg <- str_c("     Please enter a numeric value lower than your largest sample size, ",maxss,".")
    cat(msg)
    minsamples <- suppressWarnings((as.numeric(readline(prompt = "Enter the smallest sample size for concnetration studies: "))))
  }
  return(minsamples)
}

get.GMGSD <- function(filename){
file <- str_c("../Input/",filename)
i    <- rio::import_list(file)

e    <- i[!names(i) %in% c("Cover","Metadata","Individuals")]
e    <- suppressWarnings(lapply(e, nanounits))
e    <- suppressWarnings(lapply(e, GMGSD.estimator))

return(e)
}

sorter <- function(x,minsamples){

Selection <- x$`Sample Size`> minsamples & 
     complete.cases(x$`Weight`,x$GM,x$GSD)

x<- cbind(Selection,x)
x<- x[order(x$Selection, decreasing = TRUE),]
return(x)
}

file.marker <- function(name,extension){
  
  date   <- str_replace_all(Sys.time(),"[[:punct:]]"," ")
  year   <- substr(date,1,4)
  month  <- substr(date,6,7)
  day    <- substr(date,9,10)
  hour   <- as.numeric(substr(date,12,13))
  minute <- substr(date,15,16)
  
  if (hour > 12){
    hour <- as.character(hour-12)
    minute <- str_c(minute,"PM")
  } else {
    minute <- str_c(minute,"AM")
  }
  
  filename <- str_c(name," ",hour,minute," ",month," ",day," ",year,extension)
  
  return(filename)
}
