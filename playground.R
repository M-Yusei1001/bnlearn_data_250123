evd_list <- list("高温_S","機械的力_S","硬い_S")

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



print(evd)