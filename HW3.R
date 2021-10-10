#----------nome_work_3---------------------- 
#----------задание_1.1---------------------- 
#--Постройте столбцы зависимости выпадения [в] от типа фразовой позиции.--- 

library(magrittr) 
library(ggplot2)
ex_1 <- read.csv("http://goo.gl/aE3yVg") 
ggplot(ex_1, aes(v.elision, fill=position)) + 
           geom_bar(position = "dodge") + 
           ggtitle("зависимость выпадения [в] от типа позиции") + 
           theme(plot.title = element_text(hjust = 0.5)) + 
           labs(x = "выпадение [в]", y = "количество случаев") 

#-----код Ир------ 
library("ggplot2")
d_f <- read.csv("http://goo.gl/aE3yVg", header = TRUE, sep=',',encoding = "UTF-8")
ggplot(d_f, aes(v.elision, fill=position)) +
  geom_bar(position = "dodge") +
  ggtitle("Зависимости выпадения [в] от типа позиции") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "выпадение [в]", y = "количество случаев") 

#----------задание_1.2---------------------- 
#--Кружочки с цифрами: зависимость выпадения [в] от типа фразовой позиции.--- 

library(magrittr) 
library(ggplot2) 
library(tidyverse)
ex_1 <- read.csv("http://goo.gl/aE3yVg") 
ex_1 %>% 
  group_by(v.elision, position) %>% 
  summarise(number = n()) %>% 
  ggplot(aes(v.elision, position, color = position, size = number, lable = number)) + 
  geom_point() + 
  geom_text(color = 'white', size = 8) + 
  ggtitle("зависимость выпадения [в] от типа позиции") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_size_continuous(range = c(14, 30)) + 
  labs(x = "выпадение [в]", y = "количество случаев") + 
  guides(size = F, color = F) 

#-----код с пары------1.2------
library("tidyverse") 
df <- read.csv("http://goo.gl/aE3yVg") 
df %>% 
  group_by (v.elision, position) %>% 
  summarise(number = n()) %>% 
  ggplot(aes(v.elision, position, color = position, size = number, label = number)) + 
  geom_point() + 
  geom_text(color = 'white', size = 8) + 
  ggtitle("зависимости выпадения [в] от фразовой позиции") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_size_continuous(range = c(14, 30)) + 
  labs(x = "выпадение [в]", y = "фразовая позиция") + 
  guides(size = F, color = F) 

library("tidyverse")
d_f <- read.csv("http://goo.gl/aE3yVg", header = TRUE, sep=',',encoding = "UTF-8")
df %>%
  group_by (v.elision, position) %>%
  summarise(num=n()) %>%
  ggplot(aes(v.elision, position, color = position, size = num, label = num)) +
  geom_point() +
  geom_text(color = 'white', size = 8) +
  ggtitle("Зависимости выпадения [в] от фразовой позиции") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_size_continuous(range = c(14, 30)) +
  labs(x = "выпадение [в]", y = "фразовая позиция") +
  guides(size = FALSE, color = FALSE) 

#----------задание_1.3-------------- 
# чувств 

#----------задание_1.4---------------------- 
#-----выпадения [в] в третьей группе слов в сильной позиции. столбцы----- 

library(magrittr) 
library(ggplot2)
ex_1 <- read.csv("http://goo.gl/aE3yVg") 
ex_1 <- subset(ex_1, position == "strong" & group == "third")
ggplot(ex_1, aes(v.elision, fill=word)) + 
  geom_bar(position=position_dodge()) + 
  ggtitle("зависимость выпадения [в] в корне 
          (в сильной фразовой позиции, третья группа слов) ") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x = "выпадение [в]", y = "количество случаев") 

#------------задание_2.1-------- 

ex_2 <- read.csv("https://goo.gl/LQAyJ9") 
ex_2 <- subset(ex_2, select = c("a.vtl", "e.vtl", "i.vtl", "o.vtl", "u.vtl")) 
names(ex_2) <- sub("....$", "", names(ex_2))
ex_2 <- gather(ex_2, key, value) 
ex_2 %>%
  ggplot(aes(key, value)) +
  geom_boxplot() + 
  geom_hline(yintercept = mean(ex_2$value), linetype ="dashed") + 
  ggtitle("зависимость длины речевого тракта от гласного") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x = "тип гласного", y = "длина речевого тракта") 
  

#--------задание_2.2-------------- 

ex_2 <- read.csv("https://goo.gl/LQAyJ9") 
ex_2 <- subset(ex_2, select = c("a.vtl", "e.vtl", "i.vtl", "o.vtl", "u.vtl")) 
names(ex_2) <- sub("....$", "", names(ex_2))
ex_2 <- gather(ex_2, key, value) 
ex_2 %>%
  ggplot(aes(key, value)) +
  geom_violin(trim = F, fill = "light blue", colour = "light blue") + 
  geom_point(shape = "\u2013") +
  ggtitle("зависимость длины речевого тракта от гласного") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x = "тип гласного", y = "длина речевого тракта") 



#--------задание_2.3-------------- 

ex_2 <- read.csv("https://goo.gl/LQAyJ9") 
ex_2 <- subset(ex_2, select = c("a.vtl", "e.vtl", "i.vtl", "o.vtl", "u.vtl")) 
names(ex_2) <- sub("....$", "", names(ex_2))
ex_2 <- gather(ex_2, key, value) 
#ex_2 %>%
ggplot(ex_2, aes(value, fill=key)) + 
  geom_histogram(data = transform(ex_2, key = NULL), bins=8, fill = "grey", show.legend=FALSE)+
  geom_histogram(bins = 8) +
  facet_wrap(~key) +
  ggtitle("гистограмма значений длины речевого акта") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x = "vtl", y = "count") 

#--------- 
geom_histogram(data = transform(ex_2, key = NULL), 
            bins=8, fill = "grey", show.legend=FALSE)+
  
  > library("ggplot2")
d_f <- read.csv("https://goo.gl/LQAyJ9", header = TRUE, sep=',',encoding = "UTF-8")
sorting <- subset (d_f, select = c("a.vtl","e.vtl","i.vtl","o.vtl","u.vtl"))
names(sorting) = sub("....$",'',names(sorting))
sorting <- gather(sorting, key, value)
ggplot(sorting, aes(value, fill = key,stat = "count")) + 
  geom_histogram(bins = 8) +
  facet_wrap(~key)+
  ggtitle("Гистограмма значений длины речевого тракта") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "vtl", y = "count")

#--------задание_2.4----- 
#фридман и страглс (по 8)

#---------заданеи_2.5----- pagina 162: 171 

library(ggplot2) 
library(magrittr) 
ex_2 <- read.csv("https://goo.gl/LQAyJ9") 
ggplot(ex_2) + 
  geom_point(shape = "a", aes(ID, a.vtl), size = 3) + 
  geom_point(shape = "e", aes(ID, e.vtl), size = 3) + 
  geom_point(shape = "i", aes(ID, i.vtl), size = 3) + 
  geom_point(shape = "o", aes(ID, o.vtl), size = 3) + 
  geom_point(shape = "u", aes(ID, u.vtl), size = 3) + 
  ggtitle("длина речевого тракта у всех носителей") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x = "носитель", y = "длина речевого тракта") 

#------------задание_2.6--------- 



library(ggplot2) 
library(magrittr) 
ex_2 <- read.csv("https://goo.gl/LQAyJ9", header = TRUE, sep=',',encoding = "UTF-8") 
ex_2 <- subset(ex_2, select = c("ID", "a.vtl", "e.vtl", "i.vtl", "o.vtl", "u.vtl")) 
ex_2$mean <- (ex_2$a.vtl + ex_2$e.vtl + ex_2$i.vtl + ex_2$o.vtl + ex_2$u.vtl)/5
ex_2$a.vtl <- reorder(ex_2$a.vtl, ex_2$mean) 
ex_2$e.vtl <- reorder(ex_2$e.vtl, ex_2$mean)  
ex_2$i.vtl <- reorder(ex_2$i.vtl, ex_2$mean) 
ex_2$o.vtl <- reorder(ex_2$o.vtl, ex_2$mean) 
ex_2$u.vtl <- reorder(ex_2$u.vtl, ex_2$mean) 
ex_2$ID <- reorder(ex_2$ID, ex_2$mean) 

ggplot(ex_2) + 
  geom_point(shape = "a", aes(ID, a.vtl), size = 4, color = "cyan2") + 
  geom_line(aes(ID, a.vtl, group = 1), color = "cyan2") + 
  geom_point(shape = "e", aes(ID, e.vtl), size = 4, color = "darkseagreen4") + 
  geom_line(aes(ID, e.vtl, group = 2), color = "darkseagreen4") + 
  geom_point(shape = "i", aes(ID, i.vtl), size = 4, color = "coral2") + 
  geom_line(aes(ID, i.vtl, group = 3), color = "coral2") + 
  geom_point(shape = "o", aes(ID, o.vtl), size = 4, color = "bisque3") +   
  geom_line(aes(ID, o.vtl, group = 4), color = "bisque3") + 
  geom_point(shape = "u", aes(ID, u.vtl), size = 4, color = "darkorange") + 
  geom_line(aes(ID, u.vtl, group = 5), color = "darkorange") + 
    
  ggtitle("длина речевого тракта у всех носителей") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x = "носитель", y = "длина речевого тракта") 

#--------танин код ------ 

library("ggplot2")
df <- read.csv("http://goo.gl/aE3yVg", header = TRUE, sep=',',encoding = "UTF-8")
p <- boxplot(data = df) +
  boxplot(mapping = aes(v.elision)) +
  ggtitle("зависимость длины речевого тракта от гласного") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "тип гласного", y = "длинна речевого такта")
