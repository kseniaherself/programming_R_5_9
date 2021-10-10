con  <- file("Deskrop/text_for_r.txt", open = "r") 
open(con);
results.list <- list();

#y<-readLines('text_for_r.txt') 

#-----------------задание_1---------------------------
a = 'десять букв' 
is.odd <- function(y){
  as.logical(nchar(y)%%2) 
} 
is.odd(c("odd", "true")) 


#---------------------которая убирает лишние пробелы.Т_Е_5-----------------------
spaceless <- function(string){
  library(stringr)
  gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", string, perl=TRUE)
} 
spaceless(c("мама мыла  раму."))
spaceless(c("two  spaces", "five     spaces"))
string <- "Как дела?  Мои — хорошо. "

#-----------задача_4-------------------------- 
mirror_case <- function(var_1){
  var_1 <- substring(var_1, 1:nchar(var_1), 1:nchar(var_1)) 
  upper <- toupper(var_1) 
  paste0(sapply(seq_along(var_1), function(var_2){
    ifelse(var_1[var_2] == upper[var_2], 
           tolower(upper[var_2]), upper[var_2])}), collapse = "")
}  
mirror_case("ЖиЛи БыЛи ТрИ мЕдВеДя")

#-----------------задание_2.1------------------- 
is.palindrome <- function(palindromos){
  palindromos <- gsub("\\W", "", toupper(palindromos))
  palindromos == paste0(rev(substring(palindromos, 1:nchar(palindromos), 1:nchar(palindromos))), collapse = "")
}
is.palindrome("פרשנו רעבתן שבדבש נתבער ונשרף")
is.palindrome("Замучена он, но не чумаз") 
is.palindrome("A man, a plan, a canal - Panama!")

#----------------задание_2.2---------------------
library(stringr) 
p <- readLines("https://goo.gl/YDkMwa") 
#is.palindrome(paste0(v))
p <- paste(p, collapse = " ")

nchar(p)
str_length(p)
print(p)
is.palindrome(p)

#--------задание_5-------------------------- 
deduplicate_words <- function(str_1, onlyUnique = TRUE){
  a <- if (isTRUE(onlyUnique)) unique(str_1) else str_1
  paste(a, collapse = " ")
}
deduplicate_words(c("мама мыла, мыла раму раму")) 



#------------------------------------- 
ordering_in_locales <- function(var_1, var_2){
  table(
    sapply(stringi::stri_locale_list(), function(var_3){
      paste(stringr::str_sort(c(var_1, var_2), locale = var_3), collapse = "_")}))
}

ordering_in_locales("а", "п")

#------------------exerc_6---------------
un <- function(a){
  #b = a 
  toupper(a)
  strsplit(a, " ") 
  print(a)
  b = a
  unique(a,b)
}
un("мама мыла раму и мыла")

fun_1 <- function(v){
  strsplit(v, " ")
  print(v)
  #y <- v[duplicated(v)]
  #z <- v[!v%in%y]
  #print(z)
  if element in v: 
    a <- paste(a, element)
  
}

fun_1(("hello hi my way hello"))







un <- function(a){
  a <- toupper(c(a))
  print(c(a))
  a <- strsplit(a, " ") 
  print(a)
  b = a
  #union(a,b)
  for 
}

un("мама мыла раму и мыла")



#------------------- 
telefon <- function(fons) {
  fons$phone<- gsub("[\\+\\(\\) -]",'', fons$phone)
  print(fons$phone)
}

telefones("+7456789876")


