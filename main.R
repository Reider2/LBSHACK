library(tidyverse)
# install.packages("googleCloudVisionR")
# install.packages("OpenImageR")
library(OpenImageR)
Sys.setenv("GCV_AUTH_FILE" = "Keys/LBS-Hack-1618f495a49d.json")
library(googleCloudVisionR)
library(tidyr)
library(readxl)
library("httr")
library("jsonlite")


# Functions ---------------------------------------------------------------


find_co2_consumption <- function(foods =c("Avocado","Mango","Bread"),food_databse){
  
  
  products_of_interest = food_databse %>%
    filter( Name%in%foods)
  water = sum(products_of_interest$Water)
  co2 = sum(products_of_interest$CO2)
  
  print(paste(paste("In total, you have consumed",water, sep=" "),"Liters of Water",sep=" "))
  print(paste(paste("In total, you have prduced",co2, sep=" "),"grams of C02",sep=" "))
  print("By prodcut group you have generate [Liters of Water, grams of CO2]")
  print(products_of_interest[,1:3])
  
  return(products_of_interest)
}

co2_comparision <- function(df){
  #Calculate the total usage and respective usage into easier to understand equivalents
  water = sum(df$Water)
  co2 = sum(df$CO2)
  
  df$PET = floor(df$CO2/bottle)
  df$Miles = floor(df$CO2/car)
  df$Iphone_charge = floor(df$CO2/Iphone)
  df$LED =floor((((df$CO2/kwh)*1000)/LED)/24)
  df$Kettle=floor(((df$CO2/kwh)*1000)/Kettle)
  
  
  
  print("Given your shopping list you coudl have: ")
  print(paste(paste("produced: ", as.character(floor(co2/bottle))), " PET bottles"))
  print(paste(paste("driven: ", as.character(floor(co2/car))), " miles"))
  print(paste(paste("charged your Iphone: ", as.character(floor(((co2/kwh)*1000)/Iphone)), " times")))
  print(paste(paste("switched you LED light Bulb on for ", as.character(floor((((co2/kwh)*1000)/LED)/24)), " Days")))
  print(paste(paste("Boil a kettle of water ", as.character(floor((((co2/kwh)*1000)/Kettle))), " times")))
  
  return(df)
}

OCR_recognition <- function(imagepath){
  vision_Response=gcv_get_image_annotations(
    imagePaths = image_path,
    feature = "DOCUMENT_TEXT_DETECTION"
  )
  
  vision_Response$Line_Change = F
  vision_Response = separate(data = vision_Response, col = y, into = c("y1", "y2","y3","y4"), sep = ",")
  vision_Response = vision_Response %>% 
    select(-image_path,-feature)
  
  vision_Response$y1lag = lag(vision_Response$y1)
  vision_Response$y1 = as.numeric(vision_Response$y1)
  vision_Response$y1lag = as.numeric(vision_Response$y1lag)
  vision_Response$y1Diff = vision_Response$y1-vision_Response$y1lag
  vision_Response$Line_Change=ifelse(abs(vision_Response$y1Diff)>10, T, F)
  
  # cumsum(rle(vision_Response$y1Diff))
  
  vision_Response[is.na(vision_Response)] <- F
  vision_Response$Group =0
  index = 1
  for (row in 1:nrow(vision_Response)) {
    if(vision_Response[row,]$Line_Change==T){
      index = index + 1
    }
    vision_Response[row,]$Group = index
    
  }
  
  
  cleanesed = vision_Response %>%
    group_by(Group) %>% summarise(text = paste(description, collapse=" "))
  return(cleanesed)
}

get_Food_Items <- function(food_databse, ocrscan){
  food_list = vector()
  for (food in food_databse$Name) {
    for(row in 1:nrow(ocrscan)){
      if(grepl(tolower(food), tolower(ocrscan[row,]$text), fixed = TRUE)){
        print(food)
        food_list = append(food_list,food)
      }
    }
  }
  return(food_list)
}


get_AuthToken_Cassandra <-function(ASTRA_DB_ID,ASTRA_DB_REGION,ASTRA_DB_USERNAME,ASTRA_DB_PASSWORD){
  #CURL Wrapper
  URL = paste(
    paste(paste("https://",
                ASTRA_DB_ID, sep = ""),
          ASTRA_DB_REGION, sep = "-"),
    ".apps.astra.datastax.com/api/rest/v1/auth",
    sep = ""
  )
  posting<-'{"username":"hackers","password":"LSMRLSMR"}'
  
  
  r <- POST(URL, body=posting, 
            httr::add_headers(`accept` = 'application/json'), 
            httr::content_type('application/json')) #encode="json"
  httr::add_headers()
  
  return(content(r))
}


get_Cassandra_Foot_Data <- function(URL){
  #CURL Wrapper
  r <- GET(URL,
           httr::add_headers(`X-Cassandra-Token` = ASTRA_AUTHORIZATION_TOKEN)
  )
  return(r)
}

# Global Variables --------------------------------------------------------
#Energy Per
kwh = 256
car = 411
bottle= 82.8
Iphone = 9.9
LED = 5
Kettle = 20

#Database Variables
ASTRA_DB_ID="<ENTER>"
ASTRA_DB_REGION="<ENTER>"
ASTRA_DB_USERNAME="<ENTER>"
ASTRA_DB_KEYSPACE="<ENTER>"
ASTRA_DB_PASSWORD="<database_password>"
ASTRA_AUTHORIZATION_TOKEN =NULL
ASTRA_AUTHORIZATION_TOKEN = get_AuthToken_Cassandra(ASTRA_DB_ID,ASTRA_DB_REGION,ASTRA_DB_USERNAME,ASTRA_DB_PASSWORD)[[1]]

# Data --------------------------------------------------------------------
image_path = "Data/Image/image3.jpeg"

if(is.null(ASTRA_AUTHORIZATION_TOKEN)){
  #If Connection to Database cannot be established use local Excel backup
  Food_list <- read_excel("Data/Food_list.xlsx", 
                          sheet = "Master_List")
}else{
  #Connection is Life, can access DataStax Apache Cassandra
  URL_getData = paste(paste(
    paste(
      paste(paste("https://",
                  ASTRA_DB_ID, sep = ""),
            ASTRA_DB_REGION, sep = "-"),
      ".apps.astra.datastax.com/api/rest/v1/auth/keyspaces/",
      sep = ""
    ),
    ASTRA_DB_KEYSPACE,
    sep = ""
  ),
  "/tables/food_data/rows",
  sep = "")
  
  Food_list = get_Cassandra_Foot_Data(URL_getData)
}




# Script ------------------------------------------------------------------

OCR_from_Image = OCR_recognition(image_path)
foods_in_reciept = get_Food_Items(Food_list,OCR_from_Image)
return_co2_consumption = find_co2_consumption(foods_in_reciept,Food_list)
co2_comparision(return_co2_consumption)

