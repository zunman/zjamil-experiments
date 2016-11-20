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
  data <- do.call(rbind, lapply(readLines(training.file.names$filepaths[ind]), importJson))
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
