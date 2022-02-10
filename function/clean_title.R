
#x = "<title>CALL STH OFF | meaning in the Cambridge English Dictionary</title>\n"
clean_title <- function(x){
  x <- str_split(x,pattern = fixed("|"))
  x <- sapply(x,\(xi)xi[1])
  x <- str_remove(x,pattern = fixed("<title>"))
  x <- tolower(x)
  x
}