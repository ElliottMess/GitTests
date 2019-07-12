initial_commit <- "Initial Commit"
second_commit <- "This is a second commit"

del_dels <- function(x,y){
  add_adds <- paste0(x,y)
  del_dels <- gsub(x, "", add_adds)
  return(del_dels)
}

add_adds <- function(x,y){
  add_adds <- paste(x,y)
  return(add_adds)
}
