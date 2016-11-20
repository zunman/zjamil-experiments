library(stringr)
separateFileName <- function(filepaths){
  filepaths <- as.data.frame(filepaths)
  temp <- as.data.frame(str_split(filepaths$filepaths, "/"))
  temp <- as.data.frame(t(temp))
  temp1 <- str_split(temp$V4, ".tweets")
  temp1 <- as.data.frame(temp1)
  temp1 <- t(temp1)
  temp1 <- as.data.frame(temp1)
  filepaths$userID <- temp1$V1
  
  filepaths$filepaths <- as.character(filepaths$filepaths)
  filepaths$userID <- as.character(filepaths$userID)
  
  return(filepaths)
}

library(rjson)
importJson <- function(json_str){
  
  isNotNull <- function(x) ! is.null(x)
  
  load("~/CLP/results/dataFrameTemplate.RData")
  df <- template
  df[1,] <- "NULL"
  json_obj <- fromJSON(json_str)
  df$truncated[1] <- json_obj$truncated
  df$text[1] <- json_obj$text
  df$id[1] <- json_obj$id
  df$favorite_count[1] <- json_obj$favorite_count
  df$retweeted[1] <- json_obj$retweeted
  #df$entities <- as.character(json_obj$entities)
  df$id_str[1] <- json_obj$id_str
  df$retweet_count[1] <- json_obj$retweet_count
  #df$screen_name_statistics <- json_obj$screen_name_statistics
  #df$user <- json_obj$user
  df$lang[1] <- json_obj$lang
  df$created_at[1] <- json_obj$created_at
  
  
  if("place" %in% names(json_obj) & isNotNull(json_obj$place)){
    df$place[1] <- json_obj$place  
    }
  
  if("in_reply_to_status_id_str" %in% names(json_obj) & isNotNull(json_obj$in_reply_to_status_id_str)){
    df$in_reply_to_status_id_str[1] <- json_obj$in_reply_to_status_id_str
  }
  
  if("in_reply_to_user_id" %in% names(json_obj) & isNotNull(json_obj$in_reply_to_user_id)){
    df$in_reply_to_user_id_str[1] <- json_obj$in_reply_to_user_id_str
  }
 
  if("geo" %in% names(json_obj) & isNotNull(json_obj$geo)){
    df$geo[1] <- json_obj$geo
  }
  
  if("in_reply_to_user_id" %in% names(json_obj) & isNotNull(json_obj$in_reply_to_user_id)){
    df$in_reply_to_user_id[1] <- json_obj$in_reply_to_user_id
  }
  
  if("in_reply_to_screen_name" %in% names(json_obj) & isNotNull(json_obj$in_reply_to_screen_name)){
    df$in_reply_to_screen_name[1] <- json_obj$in_reply_to_screen_name
  }
  
  if("in_reply_to_status_id" %in% names(json_obj) & isNotNull(json_obj$in_reply_to_status_id)){
    df$in_reply_to_status_id[1] <- json_obj$in_reply_to_status_id
  } 
  
  if("retweeted_status" %in% names(json_obj) & isNotNull(json_obj$retweeted_status)){
    df$retweeted_status[1] <- json_obj$retweeted_status
  } 
  
  if("possibly_sensitive" %in% names(json_obj) & isNotNull(json_obj$possibly_sensitive)){
    df$possibly_sensitive[1] <- json_obj$possibly_sensitive
  } 
  
  return(df)
}

####read in list of control, depressed, PTSD users####
trainingUserInfo <- read.csv("data/clp2015/anonymized_user_info_by_chunk_training.csv")

controlUsers <- as.character(trainingUserInfo[trainingUserInfo$condition == "control", 1])
depressionUsers <- as.character(trainingUserInfo[trainingUserInfo$condition == "depression",1])
ptsdUsers <- as.character(trainingUserInfo[trainingUserInfo$condition == "ptsd",1])


training.file.names <- list.files(path="data/clp2015/training_data", pattern="*.tweets", full.names=T, recursive=FALSE)
training.file.names <- separateFileName(training.file.names)
dIndex <- which(training.file.names$userID %in% depressionUsers)
cIndex <- which(training.file.names$userID %in% controlUsers)
pIndex <- which(training.file.names$userID %in% ptsdUsers)

####

#library(jsonlite)

getJSON <- function(ind){
  # data <- clp2015.ptsd[0,]
  # data[,(1:21)] <- sapply(data[,(1:21)], as.character)
  # save(data, file="results/dataFrameTemplate.RData")
  data <- do.call(rbind, lapply(readLines(training.file.names$filepaths[ind]), importJson))
  #data <- as.data.frame(data)
  data$nodeID <- training.file.names$userID[ind]
  return(data)
}



depressionData <- do.call(rbind, lapply(dIndex, getJSON)) #327 users
save(depressionData, file="results/depression.RData")
rm(depressionData)

ptsdData <- do.call(rbind, lapply(pIndex, getJSON))
save(ptsdData, file="results/ptsd.RData")
rm(ptsdData)

controlData <- do.call(rbind, lapply(cIndex, getJSON))
save(controlData, file="results/control.RData")
rm(controlData)


json_data <- getJSON(1) 

json_data_17 <- getJSON(17)

json_data_101 <- getJSON(101)

json_data_129 <- getJSON(129)

for(i in 1:length(dIndex)){
  index <- dIndex[i]
  
  temp_json_data <- getJSON(index)
  
  if("retweeted_status" %in% colnames(temp_json_data)){
    if("possibly_sensitive" %in% colnames(temp_json_data)){
      json_data <- rbind(json_data, temp_json_data)
    } else {
      json_data_129 <- rbind(json_data_129, temp_json_data)
    }
    
  } else {
    if("possibly_sensitive" %in% colnames(temp_json_data)){
      json_data_17 <- rbind(json_data_17, temp_json_data)
    } else {
      json_data_101 <- rbind(json_data_101, temp_json_data)
    }
  }
  
}

l <- c("json_data", "json_data_17", "json_data_101", "json_data_129")
save(list = l, file="results/depression.RData")




json_data <- getJSON(1) 

json_data_17 <- getJSON(17)

json_data_101 <- getJSON(101)

json_data_129 <- getJSON(129)

for(i in 1:length(pIndex)){
  index <- pIndex[i]
  
  temp_json_data <- getJSON(index)
  
  if("retweeted_status" %in% colnames(temp_json_data)){
    if("possibly_sensitive" %in% colnames(temp_json_data)){
      json_data <- rbind(json_data, temp_json_data)
    } else {
      json_data_129 <- rbind(json_data_129, temp_json_data)
    }
    
  } else {
    if("possibly_sensitive" %in% colnames(temp_json_data)){
      json_data_17 <- rbind(json_data_17, temp_json_data)
    } else {
      json_data_101 <- rbind(json_data_101, temp_json_data)
    }
  }
  
}

l <- c("json_data", "json_data_17", "json_data_101", "json_data_129")
save(list = l, file="results/ptsd.RData")

json_data <- getJSON(1) 

json_data_17 <- getJSON(17)

json_data_101 <- getJSON(101)

json_data_129 <- getJSON(129)

for(i in 1:length(cIndex)){
  index <- cIndex[i]
  
  temp_json_data <- getJSON(index)
  
  if("retweeted_status" %in% colnames(temp_json_data)){
    if("possibly_sensitive" %in% colnames(temp_json_data)){
      json_data <- rbind(json_data, temp_json_data)
    } else {
      json_data_129 <- rbind(json_data_129, temp_json_data)
    }
    
  } else {
    if("possibly_sensitive" %in% colnames(temp_json_data)){
      json_data_17 <- rbind(json_data_17, temp_json_data)
    } else {
      json_data_101 <- rbind(json_data_101, temp_json_data)
    }
  }
  
}

l <- c("json_data", "json_data_17", "json_data_101", "json_data_129")
save(list = l, file="results/control.RData")

###load data from saved objects, get unique rows (especially ones just for intializing
###data frame will be duplicated). maybe remove unwanted columns as the size is tooo large

prepareCLPdata <- function(df1, df2, df3, df4){
  addEmptyColumns <- function(df, s){
    if(!("retweeted_status" %in% colnames(df))){
      df$retweeted_status <- c("NULL")
    }
    if(!("possibly_sensitive" %in% colnames(df))){
      df$possibly_sensitive <- c("NULL")
    }
    
    ##reorder
    df <- df[s]
    
    return(df)
  }
  
  clp2015 <- df1
  clp2015 <- rbind(clp2015, addEmptyColumns(df2))
  clp2015 <- rbind(clp2015, addEmptyColumns(df3))
  clp2015 <- rbind(clp2015, addEmptyColumns(df4))
  clp2015 <- unique(clp2015)
  
  return(clp2015)
}

load("~/CLP/results/control.RData")
clp2015.control <- prepareCLPdata(json_data, json_data_17, json_data_101, json_data_129)
save(clp2015.control, "results/clp2015.control.RData")

load("~/CLP/results/depression.RData")
clp2015.depression <- prepareCLPdata(json_data, json_data_17, json_data_101, json_data_129)
save(clp2015.depression, "results/clp2015.depression.RData")

load("~/CLPs/results/ptsd.RData")
clp2015.ptsd <- prepareCLPdata(json_data, json_data_17, json_data_101, json_data_129)
save(clp2015.ptsd, "results/clp2015.ptsd.RData")

