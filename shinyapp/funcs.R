factorfunc <- function(data, genres){
  for(i in 1:length(genres))
  {
    data[[genres[i]]]<- as.factor(data[[genres[i]]])
  }
  return (data)
}

queryfunc <- function(data, genre=NULL,anfangsjahr, endjahr) {
  tmp <- data
  if(is.character(genre)){
    tmp <- subset(tmp,tmp[[genre]]==1)
  }
  if(is.double(anfangsjahr)&is.double(endjahr)){
    tmp <- subset(tmp,anfangsjahr<=tmp$year & tmp$year<=endjahr)
  }
  #  if(is.double(dauer)){
  #   tmp <- subset(tmp,tmp$length<=dauer)
  # }
  return(tmp)
}