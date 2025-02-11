# 結果保存用の行列作成
createEmptyMatrix = function( nrow, ncol, colnames = c() ){
  if( missing( ncol ) && length( colnames ) > 0 ){
    ncol = length( colnames )
  }
  matrix( vector(mode = "character"), nrow, ncol, dimnames = list( c(), colnames ) )
}

# クエリのevidence指定
evd_list <- list("高温_S","金属_S","動きが鈍い_S")

if(length(evd_list) == 1){
  evd <- paste("(", as.character(evd_list), " == ", 1, ")", sep = "")
}else{
  temp_begin <- paste("(", evd_list[1], " == ", 1, ") & ", sep = "")
  temp = ""
  if (length(evd_list) > 2){
    for (i in 2:(length(evd_list) - 1)){
      temp <- paste(temp, "(", evd_list[i], " == ", 1, ") & ", sep = "")
    }
  }
  temp_end <- paste("(", evd_list[length(evd_list)], " == ", 1, ")", sep = "")
  evd <- paste(temp_begin, temp, temp_end, sep = "")
}

isStateInEvd = function(state, evd_list = list()){
  for(i in 1:length(evd_list)){
    if(state == evd_list[i]){
      return(TRUE)
    }
  }
  return(FALSE)
}

for(i in 1:length(states)){
  if(isStateInEvd(states[i], evd_list = evd_list)){
    next
  }
  evd <- paste(evd, " & (", states[i], " == ", 0, ")", sep = "")
}

# クエリ実行
matrix_states <- createEmptyMatrix(length(states), colnames = c("event", "probability"))

for(i in 1:length(states)){
  p <- paste(
    "cpquery(fitted, event = ", 
    "(", states[i], " == ", 1, "), ", 
    "evidence = ", evd, ", " ,
    "n = 10^6", 
    ")",
    sep = ""
  )
  result <- eval(parse(text = p))
  prob <- round(result, digits = 3)
  result <- paste(states[i], ":", prob)
  matrix_states[i, 1] <- states[i]
  matrix_states[i, 2] <- prob
  # print(result)
}

matrix_inc <- createEmptyMatrix(length(incidents), colnames = c("event", "probability"))

for(i in 1:length(incidents)){
  p <- paste(
    "cpquery(fitted, event = ", 
    "(", incidents[i], " == ", 1, "), ", 
    "evidence = ", evd, ", " ,
    "n = 10^6", 
    ")",
    sep = ""
  )
  result <- eval(parse(text = p))
  prob <- round(result, digits = 3)
  result <- paste(incidents[i], ":", prob)
  matrix_inc[i, 1] <- incidents[i]
  matrix_inc[i, 2] <- prob
  # print(result)
}

matrix_cause <- createEmptyMatrix(length(causes), colnames = c("event", "probability"))

for(i in 1:length(causes)){
  p <- paste(
    "cpquery(fitted, event = ", 
    "(", causes[i], " == ", 1, "), ", 
    "evidence = ", evd, ", " ,
    "n = 10^6", 
    ")",
    sep = ""
  )  
  result <- eval(parse(text = p))
  prob <- round(result, digits = 3)
  result <- paste(causes[i], ":", prob)
  matrix_cause[i, 1] <- causes[i]
  matrix_cause[i, 2] <- prob
  # print(result)
}

# 結果のcsv出力
if(length(evd_list) == 1){
  filename <- as.character(evd_list)
}else{
  temp_begin <- paste(evd_list[1], "_", sep = "")
  temp = ""
  if (length(evd_list) > 2){
    for (i in 2:(length(evd_list) - 1)){
      temp <- paste(temp, evd_list[i], "_", sep = "")
    }
  }
  temp_end <- paste(evd_list[length(evd_list)], sep = "")
  filename <- paste(temp_begin, temp, temp_end, sep = "")
}

directory_states <- paste("~/rscripts/output/cpt/", filename, "_state_cpt_2.csv", sep = "")
directory_inc <- paste("~/rscripts/output/cpt/", filename, "_inc_cpt_2.csv", sep = "")
directory_cause <- paste("~/rscripts/output/cpt/", filename, "_cause_cpt_2.csv", sep = "")

write.csv(matrix_states, file = directory_states, fileEncoding = "CP932")
write.csv(matrix_inc, file = directory_inc, fileEncoding = "CP932")
write.csv(matrix_cause, file = directory_cause, fileEncoding = "CP932")