if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("corrplot")) install.packages("corrplot")
if (!require("readxl")) install.packages("readxl")
if (!require("knitr")) install.packages("knitr")
if (!require("Amelia")) install.packages("Amelia")
if (!require("caret")) install.packages("caret")
if (!require("mass"))install.packages("mass")


# Kütüphanelerin çağrılması
library(tidyverse)
library(ggplot2)
library(corrplot)
library(readxl)
library(knitr)
library(Amelia)
library(caret)
yas<-read_xlsx("C:/Users/90533/Desktop/sınav 701/internet.xlsx",sheet=1)
egitim<-read_xlsx("C:/Users/90533/Desktop/sınav 701/internet.xlsx",sheet=2)
isgucu<-read_xlsx("C:/Users/90533/Desktop/sınav 701/internet.xlsx",sheet=3)
erkek<-read_xlsx("C:/Users/90533/Desktop/sınav 701/internet.xlsx",sheet=4)
kadin<-read_xlsx("C:/Users/90533/Desktop/sınav 701/internet.xlsx",sheet=5)
tum<-read_xlsx("C:/Users/90533/Desktop/sınav 701/internet.xlsx",sheet=6)
GencYasli<-read_xlsx("C:/Users/90533/Desktop/sınav 701/internet.xlsx",sheet=7)
tuik_veri<-read_xlsx("C:/Users/90533/Desktop/sınav 701/internet.xlsx",sheet=8)


view(tuik_veri)
View(yas)
view(isgucu)



# Veri setini inceleyelim
head(tuik_veri)
head(isgucu,n=nrow(isgucu))
head(yas)
head(egitim)
head(isgucu)
head(erkek)
head(kadin)

# Eksik değer kontrolü
sum(is.na(yas))
sum(is.na(tuik_veri))

# Veri seti ,max,min,çeyrekler,medyan istatistikleri
summary(yas)

# Veri setinin boyutları, veri sınıfı,liste ,
dim(yas)
class(yas)
list(yas)
str(yas)

#sütün başlıkları
colnames(yas)

#Değişkenler arası korelasyon analizi 
missmap(yas)

corrplot(isgucu)
kor<-cor(isgucu)
corrplot(kor,method ="circle")

#1, yas histogram


Yas<-yas%>%
  pivot_longer(cols = everything(),
               names_to = "Yaslar" ,
               values_to ="yuzde")
ggplot(data = Yas,aes(x=Yaslar,y=yuzde)) + 
  geom_bar(stat = "identity", position = "dodge")+
   ylim(0,100) +
  labs(title = " Yaşlara gore Kadın_Erkek İnternet Kullanım yuzde oranları",
       x = "Yıl",
       y = "İnternet Kullanım Yuzde Oran")+
      
    theme(axis.text.x=element_text(angle = 45,vjust=1,hjust=1))




#2.egitim histogram 
  colnames(egitim)             
  Egitim<-egitim%>%
    pivot_longer(cols =- yil ,
                 names_to = "Egitim" ,
                 values_to ="yuzde")
     ggplot(data = Egitim,aes(x=Egitim,y=yuzde)) + 
    geom_bar(stat = "identity", position = "dodge")+
      labs(title = "Egitim durumlarına gore Kadın_Erkek_toplam İnternet Kullanım yuzde oranları",
         x = "Yıl",
         y = "İnternet Kullanım Yuzde Oran")+
     theme(axis.text.x=element_text(angle = 45,vjust=1,hjust=1))
  

#3.boxplot 

colnames(kadin) 
class(kadin)
Kadin<-kadin%>%
    pivot_longer(cols = -yil,
               names_to = "Kadin" ,
               values_to ="yuzde")
ggplot(data=Kadin,aes(x=Kadin,y=yuzde))+
         geom_boxplot(fill=NA,notch = FALSE)+
         geom_jitter(size=1,color="black",width = 0.2)+
  theme(axis.text.x=element_text(angle = 45,vjust=1,hjust=1))
         

colnames(isgucu) 
str(isgucu)
KadinErkek<-isgucu%>%
  pivot_longer(cols =-yil,
               names_to = "Kadin_Erkek" ,
               values_to ="yuzde")
ggplot(data=KadinErkek,aes(x=Kadin_Erkek,y=yuzde))+
  geom_boxplot(fill=NA,notch = FALSE)+
  geom_jitter(size=1,color="black",width = 0.2)+
  theme(axis.text.x=element_text(angle = 45,vjust=1,hjust=1))


#jitter noktalı gösterim

Tum<-erkek%>%
pivot_longer(cols = -yil,
             names_to = "Kategoriler" ,
             values_to ="Yuzde")
ggplot(data=Tum,aes(x=Kategoriler,y=Yuzde))+
 geom_jitter(size=1,color="black",width = 0.2)+
  theme(axis.text.x=element_text(angle = 45,vjust=1,hjust=1))

Tum<-kadin%>%
  pivot_longer(cols=-yil,
               names_to = "Kategoriler" ,
               values_to ="Yuzde")
ggplot(data=Tum,aes(x=Kategoriler,y=Yuzde))+
  geom_jitter(size=1,color="black",width = 0.2)+
  theme(axis.text.x=element_text(angle = 45,vjust=1,hjust=1))


# Kadının  ve erkek in eğitim ve isgucunun yas ile arasındaki karşılaştırması
Tum<-tum%>%
  pivot_longer(cols=c("erkekOkurYazarDegil","erkekIlkokul","erkekOrtaokul","erkekLiseyukseklisans",
                          "erkekDoktora","erkekUcretliCalisan","erkekAilesiyelCalisan","erkekIssiz",
                        "erkekOgrenci","erkekEmekli"),
               names_to = "Egitim_isgucu",
               values_to = "Yas")
head(kadin)

Tum<-kadin%>%
pivot_longer(cols = c("kadinOkurYazarDegil", "kadinIlkokul", "kadinOrtaokul", "kadinLiseyukseklisans", 
                      "kadinUcretliCalisan", "kadinAilesiyelCalisan", "kadinIssiz", "kadinOgrenci", "kadinEmekli"),
             names_to = "Egitim_isgucu",
             values_to = "Yuzde")
ggplot(data=Tum,aes(x=Egitim_isgucu,y=Yuzde))+
  geom_jitter(size=1,color="black",width = 0.2)+
  theme(axis.text.x=element_text(angle = 45,vjust=1,hjust=1))

Tum<-isgucu%>%
  pivot_longer(cols = -yil,
               names_to = "Egitim_isgucu",
               values_to = "Yuzde")
ggplot(data=Tum,aes(x=Egitim_isgucu,y=Yuzde))+
  geom_jitter(size=1,color="black",width = 0.2)+
  theme(axis.text.x=element_text(angle = 45,vjust=1,hjust=1))


#nokta ve line grafik 

ggplot(data = yas) + ylim(0,100)+
  geom_point(mapping = aes(x =yil, y =kadin25_34),shape = 7, size = 4, color ="black")

ggplot(data = yas) +  ylim(0,100)+
  geom_line(mapping = aes(y = kadin25_34, x =yil), size=1,color ="black")

ggplot(data = egitim) +  ylim(0,100)+
  geom_line(mapping = aes(y =kadinOkurYazarDegil, x =yil), size=1,color ="black")

#2 li veri tek grafik

ggplot(yas, aes(x = yil)) +
  geom_line(aes(y = erkek65_74, color = "erkek65_74")) +
  geom_line(aes(y =kadin65_74, color = "kadin65_74")) +
  labs(title = "65_74 yaslar arası kadın ve erkeklerin internet kullanım yuzde oranları",
       x = "Yıl",
       y = "internet kullanım yuzde oranları") +
  ylim(0,100)+
  scale_color_manual(values = c("erkek65_74"="blue","kadin65_74" = "red")) +
  theme(axis.text.x=element_text(angle = 45,vjust=1,hjust=1))


ggplot(yas, aes(x = yil)) +
  geom_line(aes(y = kadin16_24, color = "kadin16_24")) +
  geom_line(aes(y =kadin65_74, color = "kadin65_74")) +
  labs(title = "16_24 ve 65_74 yaslar arası kadın internet kullanım yuzde oranları",
       x = "Yıl",
       y = "internet kullanım yuzde oranları") +
  scale_color_manual(values = c("kadin16_24"="blue","kadin65_74" = "red")) +
  theme_minimal()



#4 lü veri tek grafik

ggplot(GencYasli, aes(x = yil)) +
  geom_line(aes(y = toplam16_24, color = "toplam16_24")) +
  geom_line(aes(y = toplamLise, color = "toplamLise")) +
  geom_line(aes(y = toplamIssiz, color = " toplamIssiz")) +
  geom_line(aes(y = toplam65_74, color = " toplam65_74")) +
  
    labs(title = "Liseli,issiz,16-24 yaşlarındaki gencler ve 65-74 yaslarındaki yaslılar internet kullanımı",
       x = "Yıl",
       y = "internet kullanım yuzde oranları") +
  scale_color_manual(values = c("toplam16_24"="black","toplamLise" = "blue", " toplam65_74"="gray",
                                " toplamIssiz" = "red")) +
  theme(axis.text.x=element_text(angle = 45,vjust=1,hjust=1))


#kDeğişkenler arası korelasyon analizi gençlerin yaşlılar üzrindeki etkileri:Gencyasli
missmap(GencYasli) 
corrplot(GencYasli)
kor<-cor(GencYasli)
corrplot(kor,method ="circle")


#egitim ve test analiz

head(GencYasli)
dim(GencYasli)
str(GencYasli)

set.seed(1)
egitimindis <- createDataPartition(GencYasli$toplam65_74,p=0.7,list = FALSE)

egitim_veri <- GencYasli[egitimindis, ]
test_veri<-GencYasli[-egitimindis, ]
egitim_veri$YasIsgucuEgitim<-(GencYasli$toplam16_24[egitimindis] + #gençlerin yaşı+işgücü+eğitmlerinin ortalamsı:Yaisgucuegitim
                                GencYasli$toplamLise[egitimindis] + #
                                GencYasli$toplamIssiz[egitimindis]) / 3
egitim_veri$Yasli<-egitim_veri$toplam65_74#topla 65-74 grubundaki yaşlılar :egitim-veri$yasli
str(egitim_veri)
sum(is.na(egitim_veri$Yasli))
sum(is.na(egitim_veri$YasIsgucuEgitim))
#model oluşturma
model<-lm(Yasli~YasIsgucuEgitim,data=egitim_veri)
summary(model)
summary(model)$coef
#egitin_verileri
head(egitim_veri,n=nrow(egitim_veri))
nrow(egitim_veri)
dim(egitim_veri)
summary(egitim_veri)
#eğitim grafik

ggplot(egitim_veri, aes(x = yil)) +
  geom_line(aes(y =YasIsgucuEgitim , color = "Gencleri Yas_Isgucu_Egitim")) +
  geom_line(aes(y =Yasli, color = "Yasli")) +
  labs(title = "Genclerin yaslılar uzerindeki etkilerinin internet kullanım yuzde oranları",
       x = "Yıl",
       y = "Yas_Isgucu_Egitim") +
  scale_color_manual(values = c("Gencleri Yas_Isgucu_Egitim"="blue","Yasli" = "red")) +
  theme(axis.text.x=element_text(angle = 45,vjust=1,hjust=1))

grafik_egitim_veri= ggplot(egitim_veri,aes(yil,YasIsgucuEgitim ))
grafik_egitim_veri+geom_point()+geom_smooth()

#nokta egitim grafik
ggplot(egitim_veri, aes(x = yil)) +
  geom_point(aes(y =YasIsgucuEgitim , color = "Gencleri Yas_Isgucu_Egitim")) +
  geom_point(aes(y =Yasli, color = "Yasli")) +
  labs(title = "Genclerin yaslılar uzerindeki etkilerinin internet kullanım yuzde oranları",
       x = "Yıl",
       y = "Yas_Isgucu_Egitim") +
  scale_color_manual(values = c("Gencleri Yas_Isgucu_Egitim"="blue","Yasli" = "red")) +
  theme(axis.text.x=element_text(angle = 45,vjust=1,hjust=1))


#test_verileri

test_veri$YasIsgucuEgitim <- (test_veri$toplam16_24 + 
                                test_veri$toplamLise + 
                                test_veri$toplamIssiz) / 3
test_veri$Yasli<-test_veri$toplam65_74
head(test_veri,n=nrow(test_veri))
print(test_veri)
nrow(test_veri)
dim(test_veri)
summary(test_veri)
#test grafik

ggplot(test_veri, aes(x = yil)) +
  geom_line(aes(y =YasIsgucuEgitim , color = "Gencleri Yas_Isgucu_Egitim")) +
  geom_line(aes(y =Yasli, color = "Yasli")) +
  labs(title = "Genclerin yaslılar uzerindeki etkilerinin internet kullanım yuzde oranları",
       x = "Yıl",
       y = "Yas_Isgucu_Egitim") +
  scale_color_manual(values = c("Gencleri Yas_Isgucu_Egitim"="blue","Yasli" = "red")) +
  theme(axis.text.x=element_text(angle = 45,vjust=1,hjust=1))



set.seed(1)
tahmin<-predict(model,newdata=test_veri)
tahmin_veri <- data.frame(yil=test_veri$yil,Tahmin = tahmin)
gercek <- data.frame(yil=test_veri$yil,gercek = test_veri$toplam65_74)
sonuc<-data.frame(gercek,tahmin)
head(tahmin_veri)
print(tahmin)
dim(tahmin_veri)
nrow(tahmin_veri)

head(tahmin_veri)
print(gercek)
dim(gercek)
summary(gercek)

print(sonuc)
dim(sonuc)
summary(sonuc)

#gercek ve tahmin grafiği
ggplot(sonuc, aes(x = yil)) +
  geom_line(aes(y =tahmin , color = "Tahmin")) +
  geom_line(aes(y =gercek, color = "Gercek")) +
  labs(title = "Genclerin yaslılar uzerindeki etkilerinin internet kullanım yuzde oranları",
       x = "Yıl",
       y = "Yas_Isgucu_Egitim yuzdesi") +
  scale_color_manual(values = c("Gercek"="blue","Tahmin" = "red")) +
  theme(axis.text.x=element_text(angle = 45,vjust=1,hjust=1))



ggplot(sonuc, aes(x = yil)) +
  geom_point(aes(y =tahmin , color = "Tahmin")) +
  geom_point(aes(y =gercek, color = "Gercek")) +
  labs(title = "Genclerin yaslılar uzerindeki etkilerinin internet kullanım yuzde oranları",
       x = "Yıl",
       y = "Yas_Isgucu_Egitim yuzdesi") +
  scale_color_manual(values = c("Gercek"="blue","Tahmin" = "red")) +
  theme(axis.text.x=element_text(angle = 45,vjust=1,hjust=1))


#regresyon doğrusal             

ggplot(sonuc, aes(x = tahmin, y = gercek))+
  geom_point() +  
  geom_smooth(method = ("ln"), se = FALSE, color = "blue") +  
  labs(title = "Gercek ve Tahmin Degerleri Arasındaki İlişki",
       x = "Tahmin ",
       y = "Gercek ") +  
  theme(axis.text.x=element_text(angle = 45,vjust=1,hjust=1))
#hata değerleri 

MAE<-mean(abs(sonuc$gercek-sonuc$tahmin))
RMSE<-sqrt(mean (abs(sonuc$gercek-sonuc$tahmin)^2))
cat("Mean Absoute Error: ",MAE,"\n")
cat("Root Mean Squart Error: ",RMSE,"\n")


