#-----3.1---- 
#--В работе предложены стандартные значения первой и второй форманты 
#для мужского голоса всех гласных международного фонетического алфавита. 
#Для [i] предложены значения 240 и 2400. В ходе эксперимента были записаны 
#по 20 носителей русского, французского и испанского. 
#При сравнении среднего значения первой форманты какого языка 
#с эталоном получается наименьшее p-value? Перечислите их все.
#http://goo.gl/TRRx9Y 

ex_3 <- read.csv("http://goo.gl/TRRx9Y", header = TRUE, sep=',',encoding = "UTF-8") 
ex_r <- ex_3$f1[ex_3$name == "russian"] 
ex_s <- ex_3$f1[ex_3$name == "spanish"] 
ex_f <- ex_3$f1[ex_3$name == "french"] 
t.test(ex_r, mu = 240) 
t.test(ex_s, mu = 240) 
t.test(ex_f, mu = 240) 

#-------3.2------- 
#--36 языках — прилагательные, в 8 — глагол, в 11 — существительное, в 6 — наречие.-- 
#--(Adj — 50%, N — 25%, V — 12.5% и Adv — 12.5%).-- 
ex_3 <- read.csv("http://sails.clld.org/parameters/NP610#4/3.95/296.94", header = TRUE, sep=',',encoding = "UTF-8") 
ex_adj <- 36 
ex_v <- 8 
ex_n <- 11 
ex_adv <- 6
t.test(ex_adj, mu = 0.5) 


#ex_3 <- read.csv("http://sails.clld.org/parameters/NP610#4/3.95/296.94", header = TRUE, sep=',',encoding = "UTF-8") 
#ex_adj <- 36 
#ex_v <- 8 
#ex_n <- 11 
#ex_adv <- 6
binom.test(36, 61, 0.5) 
binom.test(8, 61, 0.125) 
binom.test(11, 61, 0.25) 
binom.test(6, 61, 0.125) 

#-----3.3------ 
#есть послелоги (59), предлоги (5) и нет ни предлогов, ни послелогов (11). 
#Оцените насколько результат отличается от предполагаемых распределений 
#(Post — 49%, Prep — 43%, No adpositions — 3%).

binom.test(59, 75, 0.49) 
binom.test(5, 75, 0.43) 
binom.test(11, 75, 0.03) 

#-----------4.1.1------------ 
# t test 
# "http://goo.gl/p2kmC3", sep="\t"
ex_4 <- read.csv("http://goo.gl/p2kmC3", header = TRUE, sep='\t',encoding = "UTF-8") 
ex_r <- ex_4$LI[ex_4$HQ == "RH"] 
ex_l <- ex_4$LI[ex_4$HQ == "LH"] 
t.test(ex_r, ex_l) 

#------------4.2.1------------ 
#С помощью фМРТ измерялся индекс латерализации речевых функций (LI) 
#в височной и лобной доле у одних и тех же участников. 
#Сравните значения LI в лобной и в височной долях.
ex_4 <- read.csv("http://goo.gl/MNkVws", header = TRUE, sep='\t',encoding = "UTF-8") 
ex_f <- ex_4$LI.Front #[ex_4$HQ == "RH"] 
ex_t <- ex_4$LI.Temp #[ex_4$HQ == "LH"] 
t.test(ex_f, ex_t, paired = T) 

#------------4.3.1----------- 
#--предлоги "в" и "на". Распределение падежей одинаково и не зависит от предлога? 
# Или же у каждого предлога свое соотношение?
ex_r <- read.csv("http://goo.gl/KljqjU", header = TRUE, sep='\t',encoding = "UTF-8") 
ex_p <- read.csv("http://goo.gl/xNozm2", header = TRUE, sep='\t',encoding = "UTF-8") 
ex_r_c <- ex_r$case #[ex_4$HQ == "RH"] 
ex_r_pr <- ex_4$prep #[ex_4$HQ == "LH"] 
ex_p_c <- ex_p$case #[ex_4$HQ == "RH"] 
ex_p_pr <- ex_p$prep #[ex_4$HQ == "LH"] 
chisq.test

#----------------------------код_Иры---------------------------------------- 
russian <- read.csv('http://goo.gl/KljqjU')
polish <- read.csv('http://goo.gl/xNozm2')
w_acc <- length(russian$prep[russian$prep=='w'&russian$case=='acc'])
w_loc <- length(russian$prep[russian$prep=='w'&russian$case=='loc'])
na_acc <- length(russian$prep[russian$prep=='na'&russian$case=='acc'])
na_loc <- length(russian$prep[russian$prep=='na'&russian$case=='loc'])

r_table <- as.table(rbind(c(w_acc, w_loc), c(na_acc, na_loc)))
chisq.test(r_table)

polish <- read.csv('http://goo.gl/xNozm2')
w_acc <- length(polish$prep[polish$prep=='w'&polish$case=='acc'])
w_loc <- length(polish$prep[polish$prep=='w'&polish$case=='loc'])
na_acc <- length(polish$prep[polish$prep=='na'&polish$case=='acc'])
na_loc <- length(polish$prep[polish$prep=='na'&polish$case=='loc'])

p_table <- as.table(rbind(c(w_acc, w_loc), c(na_acc, na_loc)))
fisher.test(p_table) 


#---------------------------------4.4.1------------------------------------- 
ex_4 <- read.csv("http://goo.gl/nNpgrq") 
do_pst <- length(ex_4$prep[ex_4$prep=='do'&ex_4$tense=='pst'])
ku_pst <- length(ex_4$prep[ex_4$prep=='ku'&ex_4$tense=='pst'])
do_npst <- length(ex_4$prep[ex_4$prep=='do'&ex_4$tense=='npst'])
ku_npst <- length(ex_4$prep[ex_4$prep=='ku'&ex_4$tense=='npst'])

tense_t <- as.table(rbind(c(do_pst, do_npst), c(ku_pst, ku_npst)))
lsr::cramersV(tense_t) 

#------------------------------1.2----------------------------------------- 
ex_1 <- read.csv("http://goo.gl/Vlvc5M") 
mean(ex_1$number.of.borrowed.affixes, trim = 0.1)

#------------------------------1.3------------------------------------------ 


#-------------------1.4--------------- 
#евразия 34 

#-------------------1.5---------------- 
# Напишите функцию, которая принимает на вход вектор значений 
#и возвращает вектор с выбросами (т. е. значения, 
#которые отклоняются от медианы более чем на 1.5 межквартильных размаха). 
#Выдавать функция должна датафрейм, состоящий из получившегося вектора 
#и еще двух векторов: первый сообщает на сколько каждое из значений отклонилось 
#от медианы, второй сообщает ту же информацию в IQR. 
ex_1 <- read.csv("http://goo.gl/Vlvc5M") 

fun_1 <- function(x) {
  mediana <- median(x)
  iq_range <- IQR(x)
  t <- data.frame(outs = x[abs(x - mediana) > 1.5*iq_range])
  deviations <- abs(t$outs - mediana)
  iq_deviations <- deviations/iq_range
  outer <- cbind(t, deviations)
  cbind(outer , iq_deviations)
}

#----------Ira_A--------
fin_1 <- function(x) {
  mediana <- median(x) 
  iq_range <- IQR(x) 
  t <- data.frame(outs = x[abs(x - mediana) > 1.5*iq_range]) 
  deviations <- abs(t$outs - mediana) 
  iq_deviation <- deviations/iq_range 
  outer <- cbind(t, deviations) 
  cbind(outer , iq_deviation) 
}

#----------------------------------4.5---------------------------------------- 
#В пятой главе WALS приводится выборка из 567 языков, 
#в 97 из которых встретился хотя бы один увулярный звук. 
#В базе данных PHOIBLE из выборки размером 2155 языков увулярные 
#встретились в 419 языках. Если принимать распределение, 
#представленное в WALS за вероятность встретить язык с увулярным согласным, 
#какая вероятность получить результаты, представленные в PHOIBLE?
#WALS: http://wals.info/chapter/6 PHOIBLE: http://phoible.org/parameters



#----------------------------------4.6----------------------------------------- 
# Рассмотрим информацию о частотности конструкций 
#с творительным падежом типа "Он был учителем" и практически синонимичных 
#им конструкций с именительным падежом "Он был учитель". 
#В НКРЯ до 1950 года встретилось 5485 примера первой конструкции 
#и 15913 примеров второй. После 1950 года обнаружено 8318 примеров первой 
#и 8259 примеров второй. Какова величина эффекта произошедшего изменения? 
#Приведите значения соответствующего статистического теста. 

do_pst <- length(ex_4$prep[ex_4$prep=='do'&ex_4$tense=='pst'])
ku_pst <- length(ex_4$prep[ex_4$prep=='ku'&ex_4$tense=='pst'])
do_npst <- length(ex_4$prep[ex_4$prep=='do'&ex_4$tense=='npst'])
ku_npst <- length(ex_4$prep[ex_4$prep=='ku'&ex_4$tense=='npst'])

t_nkrj <- as.table(rbind(c(5485, 15913), c(8318, 8259)))

#table(stopfricg) 
#mcnemar.test(table(stopfricg)) 
mcnemar.test(t_nkrj) 
lsr::cohensD(a,b) 

#------------------код_Иры--------------------------------------------------- 
before_50 <- c(5485, 15913)
after_50 <- c(8318, 8259)
mcnemar.test(as.table (rbind(before_50, after_50)))
lsr::cohensD(before_50, after_50, method = "paired")

#-----------------так вроде не надо-------------------------------------------
instr <- c(5485, 8318)
nom <- c(15913, 8259)
mcnemar.test(as.table (rbind(instr, nom)))
lsr::cohensD(instr, nom, method = "paired")


#---------------------4.7---------------------------------------------------- 
#(you must read it) и эпистемическое (you must be kidding). 
#Приведите результаты статистического анализа распределения значений 
#и его интерпретации. Каждый глагол следует анализировать отдельно. 
#Следует написать нулевую и альтернативную гипотезы 
#и значения соответствующих статистик.
#Данные (http://goo.gl/4iEt4j) основаны на Coates, J., Leech, G. (1980) 
#The Meanings of the Modals in British and American English 
#(http://files.eric.ed.gov/fulltext/ED202217.pdf).

# 4.7 ---------------------------------------------------------------------
must_data <- read.csv('http://goo.gl/4iEt4j')
must_data <- subset(must_data, must_data$word=='must')
amer_root <- length(must_data$var[must_data$var=='American'&must_data$meaning=='root'])
amer_epis <- length(must_data$var[must_data$var=='American'&must_data$meaning=='epistemic'])
brit_root <- length(must_data$var[must_data$var=='British'&must_data$meaning=='root'])
brit_epis <- length(must_data$var[must_data$var=='British'&must_data$meaning=='epistemic'])
t<- as.table(rbind(c(amer_epis, amer_root), c(brit_epis, brit_root)))
t
chisq.test(t)

must_data <- read.csv('http://goo.gl/4iEt4j')
must_data <- subset(must_data, must_data$word=='have to')
amer_root <- length(must_data$var[must_data$var=='American'&must_data$meaning=='root'])
amer_epis <- length(must_data$var[must_data$var=='American'&must_data$meaning=='epistemic'])
brit_root <- length(must_data$var[must_data$var=='British'&must_data$meaning=='root'])
brit_epis <- length(must_data$var[must_data$var=='British'&must_data$meaning=='epistemic'])
t<- as.table(rbind(c(amer_epis, amer_root), c(brit_epis, brit_root)))
t
e <-chisq.test(t)
e$expected
chisq.test(t)



