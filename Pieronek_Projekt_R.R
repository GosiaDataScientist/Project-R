getwd()
setwd("C:/Users/moje/Desktop/Bank Marketing data") 

#wczytanie danych
DaneMarketingowe <- read.csv('Bank Marketing Data.csv', sep=";")
head(DaneMarketingowe,3) 
str(DaneMarketingowe)  #sprawdzenie struktury danych
summary(DaneMarketingowe) #statystyki i licznosci

#wystepuje duza dysproporcja w odpowiedziach yes/no
table(DaneMarketingowe$y)
barplot(table(DaneMarketingowe$y), main = "Subscribed a term deposit")

#nie ma zadnych brakow danych, które sa "nullami", ale s¹ takie gdzie jest oznaczenie "unknown"
table(is.na(DaneMarketingowe))

#usuwam obserwacje w ktorych takie wartosci sie pojawiaja
DaneMarketingowe <- DaneMarketingowe[!DaneMarketingowe$marital=="unknown",]
DaneMarketingowe <- DaneMarketingowe[!DaneMarketingowe$housing=="unknown",]
DaneMarketingowe <- DaneMarketingowe[!DaneMarketingowe$loan=="unknown",]
DaneMarketingowe <- DaneMarketingowe[!DaneMarketingowe$job=="unknown",]
DaneMarketingowe <- DaneMarketingowe[!DaneMarketingowe$education=="unknown",]
#poniewaz w przypadku danej Edukacja znajduje siê tylko jedna obserwacja "illiterate" zostanie ona usunieta
#jedna obserwacja nie wplynie na wynik modelu, a moglaby zaburzyc dalsze obliczenia
#jak na przyklad test chi-kwadrat
DaneMarketingowe <- DaneMarketingowe[!DaneMarketingowe$education=="illiterate",]

#zmieniam format danych, zeby usuniete kategorie sie nie pojawialy
DaneMarketingowe$marital <- factor(DaneMarketingowe$marital)
DaneMarketingowe$housing <- factor(DaneMarketingowe$housing)
DaneMarketingowe$loan <- factor(DaneMarketingowe$loan)
DaneMarketingowe$job <- factor(DaneMarketingowe$job)
DaneMarketingowe$education <- factor(DaneMarketingowe$education)

summary(DaneMarketingowe)
str(DaneMarketingowe)

#duration nie bedzie brane pod uwage
#default równiez nie- wystêpuje nadreprezentacja odpowiedzi „no” (3315 obserwacji) w stosunku do odpowiedzi „yes” 
table(DaneMarketingowe$default)
#pdays rowniez nie bedzie uwzglednione poniewaz 96% klientow nie bylo wczesniej kontaktowanych 
table(DaneMarketingowe$pdays)
DaneMarketingowe<-DaneMarketingowe[,-c(5,11,13)]

barplot(table(DaneMarketingowe$default), main = "Deafult")
barplot(table(DaneMarketingowe$pdays), main = "Pdays")

#dziele zbior na uczacy i testowy
#ustawiam ziarno zeby wyniki byly powtarzalne
set.seed(10)
indeksy<-sample(3810,2300)
uczacy<-DaneMarketingowe[indeksy,]
testowy<-DaneMarketingowe[-indeksy,]

#Badam zaleznosc zmiennych korzystajac z testu niezale¿nosci chi-kwadrat
#przyjmujac poziom istotnosci 0,05 nieistotne sa zmienne: loan, marital, housing, education, day_of_week

contact_y<-table(uczacy$contact,uczacy$y)
chisq.test(contact_y)   # p-value = 3.051e-10

job_y<-table(uczacy$job,uczacy$y)
chisq.test(job_y)       # p-value = 1.445e-05

month_y<-table(uczacy$month,uczacy$y)
chisq.test(month_y)     # p-value < 2.2e-16

loan_y<-table(uczacy$loan,uczacy$y)
chisq.test(loan_y)      # p-value = 0.8696

marital_y<-table(uczacy$marital,uczacy$y)
chisq.test(marital_y)   #  p-value = 0.1521

housing_y<-table(uczacy$housing,uczacy$y)
chisq.test(housing_y)   # p-value = 0.5876

education_y<-table(uczacy$education,uczacy$y)
chisq.test(education_y) # p-value = 0.05351

day_of_week_y<-table(uczacy$day_of_week,uczacy$y)
chisq.test(day_of_week_y) # p-value = 0.9002

poutcome_y<-table(uczacy$poutcome,uczacy$y)
chisq.test(poutcome_y) # p-value < 2.2e-16

#przekodowanie danych
#sposrod zmiennych ktore wyszly istotne przekodowuje job na podstawie udzialu yes/no 
table(uczacy$job)
levels(uczacy$job)<-c("grupa1_job", "grupa2_job", "grupa2_job", "grupa3_job", "grupa3_job",
                                "grupa4_job", "grupa3_job", "grupa3_job", "grupa4_job", "grupa1_job",
                                "grupa4_job")
table(uczacy$job)

#przekodowuje zmienna "month"
table(uczacy$month)
levels(uczacy$month)<-c("grupa2_month", "grupa1_month", "grupa2_month", "grupa3_month", "grupa1_month",
                                  "grupa2_month", "grupa3_month", "grupa1_month", "grupa2_month", "grupa2_month")
table(uczacy$month)

# sprawdzam istotnosc zmiennych numerycznych
#install.packages("Information")
library(Information)

#IV licze tylko dla numerycznych
DaneMarketingoweIV<-subset(uczacy, select = c(1,10,11,13,14,15,16,17,18))
#DaneMarketingoweIV$y <- factor(DaneMarketingoweIV$y)
str(DaneMarketingoweIV)
# do policzenia IV -> y zamieniam na numeryczna zmienna
DaneMarketingoweIV$y<-ifelse(DaneMarketingoweIV$y=="yes",0,1)
IV <- create_infotables(data=DaneMarketingoweIV, y="y", bins=10, parallel=TRUE)
IV_Value <- data.frame(IV$Summary)
IV_Value

#ze wzgledu na wartosc IV odrzucona zostanie zmienna "age" i "campaign" poniewaz wartoœæ IV wynosi  0.1
#wiec zmienne age,campaign nie zostana uwzglednione w modelu

#licze wspolczynnik korelacji Pearsona
korelacjaPearson<- (uczacy[,c("previous", "emp.var.rate", "cons.price.idx", "cons.conf.idx", 
                               "euribor3m", "nr.employed")])
cor(korelacjaPearson)
pairs(korelacjaPearson)

#install.packages(GGally)
library(GGally)
ggcorr(korelacjaPearson, label = TRUE)

#nr_employed bedzie usuniete bo ma wysoki VIF i wspolczynnik korelacji
korelacjaPearson<- (uczacy[,c("previous", "emp.var.rate", "cons.price.idx", "cons.conf.idx", 
                               "euribor3m")])
cor(korelacjaPearson)
pairs(korelacjaPearson)

#emp.var.rate  zostanie usuniete bo ma wysoki wspolczynnik korelacji z kazda zmienna
korelacjaPearson<- (uczacy[,c("previous", "cons.price.idx", "cons.conf.idx", 
                               "euribor3m")])
cor(korelacjaPearson)
pairs(korelacjaPearson)

#euribor3m  zostanie usuniete bo ma wysoki wspolczynnik korelacji z kazda zmienna
korelacjaPearson<- (uczacy[,c("previous", "cons.price.idx", "cons.conf.idx")])
cor(korelacjaPearson)
pairs(korelacjaPearson)

#zostaly dwie zmienne po usunieciu wspolliniowosci
korelacjaPearson<- (uczacy[,c("previous", "cons.conf.idx")])
cor(korelacjaPearson)
pairs(korelacjaPearson)

#wybieram te zmienne ktore beda w modelu
str(uczacy)
uczacy2 <- subset(uczacy, select = c("job", "contact", "month", "poutcome", "cons.conf.idx", "previous", "y"))
str(uczacy2)

##  dostawienie sztucznie zmiennych
#install.packages("dummies")
library("dummies")

uczacy3<- dummy.data.frame(uczacy2, names=c("job","contact", "month", "poutcome"), sep=".")
str(uczacy3)

#jedna kategoria dla kazdej zmiennej kategorialnej pozostanie kategoria referencyjna
uczacy4<- uczacy3[,-c(1,5,8,12)]  #nie brac kategorii referencyjnych
str(uczacy3)

#Robie OVERSAMPLING
#install.packages("ROSE")
library("ROSE")
uczacy5<-ovun.sample(y~., data=uczacy4, method= "over", N=4102)$data
table(uczacy5$y)

#krokowa metoda doboru zmiennych
full_model<-glm(y~.,data = uczacy5, family=binomial)
summary(full_model)

no_model<-glm(y~1,data = uczacy5, family=binomial)
summary(no_model)

#wybiera zmienne miedzy modelem pustym a modelem ze wszystkimi zmiennymi
logistic_steps = step(no_model,
                      scope=list(lower=formula(no_model),
                                 upper=formula(full_model)),direction="both",
                      family=binomial)

#patrze na Kryterium informacyjne Akaikego (AIC)
#wybieram model gdzie AIC przestaje drastycznie spadac, po to zeby nie brac modelu 
#ze zbyt duza liczba zmiennych

#buduje ten model ktory wyszedl najlepszy
# FINALNA POSTAC MODELU
Select_model<-glm(y ~ previous +  poutcome.failure + contact.telephone + 
                    job.grupa4_job + poutcome.nonexistent + job.grupa3_job + 
                    job.grupa2_job +  cons.conf.idx, data=uczacy5,
                  family=binomial)
summary(Select_model)

#badanie wspolliniowosci zmiennych
#Statystyki VIF bliskie 10 oznaczaj¹, ¿e dany predyktor jest silnie powi¹zany z innym 
#i nale¿y siê zastanowiæ nad jego usuniêciem lub agregacj¹ z innym predyktorem. 

#sprawdzam czy nie zachodzi wspolliniowosc miedzy zmiennymi, jesli>10 to trzeba usunac
#install.packages("car")
library("car")
vif(Select_model)

#sprawdzam prognozowane wartosci
uczacy5$predicted_val<-predict(Select_model)
edit(uczacy5)
uczacy5$prob<-exp(uczacy5$predicted_val)/(1+exp(uczacy5$predicted_val))

#zbior testowy
#w tym zbiorze uwzgledniam tylko dane ktore weszly do finalnego modelu
# trzeba zrobic wszystkie modyfikacje zmiennych jak na zbiorze uczacym
table(testowy$job)
levels(testowy$job)<-c("grupa1_job", "grupa2_job", "grupa2_job", "grupa3_job", "grupa3_job",
                      "grupa4_job", "grupa3_job", "grupa3_job", "grupa4_job", "grupa1_job",
                      "grupa4_job")
table(testowy$job)

#przekodowuje zmienna "month"
table(testowy$month)
levels(testowy$month)<-c("grupa2_month", "grupa1_month", "grupa2_month", "grupa3_month", "grupa1_month",
                        "grupa2_month", "grupa3_month", "grupa1_month", "grupa2_month", "grupa2_month")
table(testowy$month)

testowy2 <- subset(uczacy, select = c("job", "contact", "month", "poutcome", "cons.conf.idx", "previous", "y"))

testowy3<- dummy.data.frame(testowy2, names=c("job","contact", "month", "poutcome"), sep=".")
str(testowy3)

#jedna kategoria dla kazdej zmiennej kategorialnej pozostanie kategoria referencyjna
testowy4<- testowy3[,-c(1,5,8,12)]  #nie brac kategorii referencyjnych
str(testowy3)

# w modelu na danych testowych wszystkie zmienne sa istotne
testowy_model<-glm(y ~ previous +  poutcome.failure + contact.telephone + 
                    job.grupa4_job + poutcome.nonexistent + job.grupa3_job + 
                    job.grupa2_job + cons.conf.idx, data=testowy4,
                  family=binomial)
summary(testowy_model)
vif(testowy_model)

# sprawdzic czy znak przy wspolczynnikach sie nie zmienil
# jesli w testowym jakas zmienna wyjdzie nieistotna to trzeba poprawic model

#wyliczam prawdopodobienstwa
testowy4$predicted_val<-predict(testowy_model)
hist(testowy4$predicted_val)  
testowy4$prob<-exp(testowy4$predicted_val)/(1+exp(testowy4$predicted_val))
edit(testowy4)

# macierz trafnosci
fitted.probabilities <- predict(testowy_model,newdata=testowy4,type='response') #response po to zeby pokazalo oszacowania
table(testowy4$y, fitted.probabilities > 0.5)
#w wierszu to zbior danych (real class)
#w kolumnach to predykcje (model)

#ROC
#install.packages("ROCR")
library(ROCR)
p<-predict(Select_model, newdata=uczacy5, type="response")
pr<-prediction(p,testowy4$y)   
prf<-performance(pr,measure="tpr", x.measure="fpr")
plot(prf)

#AUC
auc<-performance(pr,measure="auc")
auc<-auc@y.values[[1]]
auc
