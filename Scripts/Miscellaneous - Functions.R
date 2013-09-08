HPD <- function(x, conf=0.95){
  conf <- min(conf, 1-conf)
  n <- length(x)
  nn <- round( n*conf )
  x <- sort(x)
  xx <- x[ (n-nn+1):n ] - x[1:nn]
  m <- min(xx)
  nnn <- which(xx==m)[1]
  return( c( x[ nnn ], x[ n-nn+nnn ] ) )
}

.Score_H <- function(R){
  if (R == "D"){
    1
  } else if (R =="A") {
    0
  } else if (R == "H") {
    3
  } else {
    NA
  }
}

.Score_A <- function(R){ #based off home team result!
  
  if (R == "D"){
    1
  } else if (R =="A") {
    3
  } else if (R == "H") {
    0
  } else {
    NA
  }
}