# Függvények

# 4.3. get_gender függvény


rm(get_gender)
get_gender <- function(nev) {
  
  sub_comic1 <-subset(comic_characters, comic_characters$name_pri==nev) 
  sub_comic2<-subset(comic_characters,comic_characters$name_pri!=nev) 
  sub_comic1<-as.data.frame(sub_comic1)
  df1<-as.data.frame(sub_comic1$name_pri)
  
  
  if (any(comic_characters$name_pri==nev)){
    apply(df1, 1, function(x) {ifelse(any(x == nev), ifelse(is.na(comic_characters$gsm), strsplit(as.character(comic_characters$sex), split = " ")[[1]][[1]],strsplit(as.character(comic_characters$gsm), split = " ")[[1]]), "")})
    
  }
  else {}
  
}