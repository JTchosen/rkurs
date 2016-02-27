factorfunc <- function(data, genres){
  for(i in 1:length(genres))
  {
    data[[genres[i]]]<- as.factor(data[[genres[i]]])
  }
  return (data)
}