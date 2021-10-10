letters
a <- letters
sample(a, size = 20)
sample(a, size = 200)
sample(a, size = 200, replace = TRUE)
sample(1:1000, size = 200, replace = TRUE)
sample(1:1000, size = 200, replace = FALSE)
sample(1000:57, size = 200, replace = TRUE) 

rnorm(100, mean = 100, sd = 20) 
rpois(1000, 10) 
rt(1000, 10) 
rchisq(1000, 10)
rbinom(100, 20, 0.4) 

set.seed(42)
sample(a, size = 20) 
rbinom(100, 20, 0.4) 

#functions 
------------- 
parapam <- function(qqq) {
	qqq**3 
	}
parapam(5)

funk1(3, 4) 
funk1(per = 3, vtor = 4) 
funk1(4, 3) 
funk1(p = 3, v = 4) 

apropos("mean") #берёт имена всех функций и ищет строку 

#что-то про соединение баз и дат 
--------------- 
# join-ы 
# dplyR — операции с данными (строками?) 
# tidyR 
# ggplot2 — вроде про рисование 
# gg-plot читается 
# tideverse 
# readR — удобный пакет для чтения файлов 

dice = c('1', '2', '3', '4', '5', '6')
sample(a = dice, size = 6, replace = FALSE)


