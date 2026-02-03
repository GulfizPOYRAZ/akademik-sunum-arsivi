
if (!require("mass"))install.packages("scales")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("corrplot")) install.packages("corrplot")
if (!require("readxl")) install.packages("readxl")
if (!require("knitr")) install.packages("learnr")
if (!require("Amelia")) install.packages("knitr")
if (!require("caret")) install.packages("caret")
if (!require("mass"))install.packages("mass")
if (!require("mass"))install.packages("PerformanceAnalytics")


# Kütüphanelerin çağrılması
library(tidyverse)
library(ggplot2)
library(corrplot)
library(readxl)
library(learnr)
library(knitr)
library(caret)
library(PerformanceAnalytics)
library(scales)

kesim<-read_xlsx("C:/Users/90533/Desktop/veri_hazırlama/hayvan/hayvancılık.xlsx",sheet = 1)
kesim_tuik_kucuk_buyuk_bas<-read_xlsx("C:/Users/90533/Desktop/veri_hazırlama/hayvan/hayvancılık.xlsx",sheet = 2)
kesim_tuik_kumes<-read_xlsx("C:/Users/90533/Desktop/veri_hazırlama/hayvan/hayvancılık.xlsx",sheet = 3)
#https://data.tuik.gov.tr/Bulten/Index?p=Hayvansal-Uretim-Istatistikleri-2024-53935 tüik veri linki

#veri setinin yeni bir sekmede  tablo olarak gösterimi
View(kesim)
# Veri setini başlıkları
head(kesim)
# Veri setinin boyutları
dim(kesim)
# veri sınıfları
class(kesim)
#satır,sütun ,başlıları ve veri sınıflarının açıklaması
str(kesim)


# Eksik değer kontrolü 
sum(is.na(kesim))




#sütün başlıkları listesi
colnames(kesim)

#veri seti içinden yeni veri seti oluşturalım

buyuk_bas_kesim<-cor(kesim[ ,c(1 ,2:5)], method="pearson")
View(buyuk_bas_kesim)

#kucuk_bas_kesim<-cor(kesim[ ,c(1,6:9)], method="pearson")
#View(kucuk_bas_kesim)

kumes_kesim<-cor(kesim[ ,c(1,10:13)], method="pearson")
View(kumes_kesim)

#ton_kesim<-cor(kesim[, c(1, 3, 5, 7, 9, 11, 13)], method = "pearson")
#View(ton_kesim)

adet_kesim<-cor(kesim[ ,c(1,2,4,6,8,10,12)], method="pearson")
View(adet_kesim)

#Değişkenler arası eksik veri /doluluk durumu
missmap(kesim)


#korelasyon analizi

kor<-cor(kesim)
#corrplot(kor,method ="circle")
#corrplot(kor,method ="pie")
#corrplot(kor,type="upper" )
#corrplot(kor,type="lower" )
corrplot(kor,type="upper" ,order="hclust",col=c("blue","red"),bg="black")
      
#corrplot(kor,type="upper" ,order="hclust",col=c("blue","red"),bg="black",
        # tl.srt=45)

#histogram dağılımların gösterilmesi
#chart.Correlation(kesim[ ,c(1, 2:6)],histogram=TRUE, pc=19)
#chart.Correlation(kesim[ ,c(1, 10:13)],histogram=TRUE, pc=19)
chart.Correlation(kesim[ ,c(1,3,5,7,9,11,13)],histogram=TRUE, pc=19)
#chart.Correlation(kesim[ ,c(1,2,4,6,8)],histogram=TRUE, pc=19)


# Veri seti ,max,min,çeyrekler,medyan değerlerinin gösterimi
summary(kesim$`sigirEtMiktari(ton)`)
#summary(kesim$`sigirEtMiktari(ton)`)
# veri setinin % 0 -%25-%50-%75-%100 değerlerinin gösterimi
quantile(kesim$`sigirEtMiktari(ton)`)
#quantile(kesim$`sigirEtMiktari(ton)`)
# ver setindeki en küçük ve en büyük değerlerinin gösterimi
range(kesim$`sigirEtMiktari(ton)`)
#range(kesim$`sigirEtMiktari(ton)`)

#standart sapma değerlerinin gösterimi
sd(kesim$`sigirEtMiktari(ton)`)
sd(kesim$`sigirEtMiktari(ton)`)

#missing değerleri yani veri seti içerisinideki eksik/kaçan veri varsa TRUE (NA) yazar,eksik veri yoksa FALSE(veri var) döner.  
View(is.na(kesim))#NA değerlerini gösterir
View(kesim_tuik_kumes[!complete.cases(kesim_tuik_kumes ),])
kesim_tuik<-kesim_tuik_kumes[is.na( kesim_tuik_kumes$...5), ]
View(kesim_tuik)


#veri seti içerisinde değişkenleri kullan demek.(burada "kesim$" yazmaya gerek kalmıyor.)
with(kesim,cor(`tavukKesimMiktari(ton)`, `hindiKesimMiktari(ton)`))


#kategorik değerlere karşılk geleb sayısal değerlerin gösterimi

table(kesim$`tavukKesimAdeti(bas)`)
#table(kesim$`sigirEtMiktari(ton)`)

# n tane değer aralığı yaratmak 
#range(kesim$`mandaKesimAdeti(bas)` )
#cut(kesim$`mandaKesimAdeti(bas)`,breaks=10)#değerler büyük olduğu için e ile gösteriyor.
cut(kesim$`mandaKesimAdeti(bas)`,breaks=7210)

#belirlenen aralıklarda kaçar tane değer aldığı değerlerin gösterimi

table(cut(kesim$`mandaKesimAdeti(bas)`,seq(19000,70000,5000)))
range(kesim$`mandaKesimAdeti(bas)` )
range(kesim$`mandaEtMiktari(ton)` )
#histogram gösterimi
ggplot(data = kesim, aes(x = `mandaEtMiktari(ton)`)) +
  geom_histogram(binwidth = 450,fill = "blue", color = "black") +
  labs(
    title = "Mandaların Adet ile Et Miktarı  Arasındaki Dağılımı",
    x = "Et Miktarı (ton)",
    y = "Adet Miktarı(Bas)"#Frekans olarak değer geliyor.
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(hjust = 1)
  ) +
  ylim(0,5)


#Küçük be Büyük baş hayvanların baş adetlerinin boxplot ile gösterimi
colnames(kesim)
buyuk_kucuk_bas <- kesim %>%
  pivot_longer(
    cols = c("sigirKesimAdeti(bas)",  "koyunKesimAdeti(bas)",  "keciKesimAdeti(bas)" ),
    names_to = "buyuk_kucuk_bas",
    values_to = "Ton"
  )
ggplot(data = buyuk_kucuk_bas, aes(x = as.factor(buyuk_kucuk_bas), y = Ton)) +
  geom_boxplot(notch = FALSE, fill = "blue") +
  #geom_jitter(size = 1, color = "black", width = 0.2) + #noktalı göstermek istersen (#) kaldır.
  scale_y_continuous(labels = label_number(scale = 1/10000, suffix = "K")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Buyuk ve Kucukbas Hayvan Kesim Adetleri",
    x = "Hayvan Turu",
    y = "Adet (Bin Bas)"
  )

# aykırı değerleri aldık (out olunca aykırı değerleri veir)
kesim.cleaned<-na.omit(kesim)
boxplot.stats(kesim.cleaned$`koyunKesimAdeti(bas)`)$out

#aykırı değerlerin yeini göstermek 
out<-boxplot.stats(kesim.cleaned$`koyunKesimAdeti(bas)`)$out
out_row<-which(kesim.cleaned$`koyunKesimAdeti(bas)`%in%c(out))
View(kesim.cleaned[out_row,])

#çan eğrisine göre iki uçtaki kısımlar 0,025ile 0,975 değerlerini alır.bunların dışında kalan.
lower_bound<-quantile(kesim.cleaned$`koyunKesimAdeti(bas)`,0,025)
upper_bound<-quantile(kesim$`koyunKesimAdeti(bas)`,0,975)

#tekrar eden verileri gösterir.benim veri setimde kesim içinde tekrar edenveri yok.
duplicated(kesim)

#z score standizasyon yapma

View(kesim)
housing.z <- scale(kesim)
View(housing.z)

#GRAFİKLER#

# hindi kesim adet miktarının yıllara göre nokta grafiği

Hindi_ton_miktari<-kesim$`hindiKesimMiktari(ton)`
ggplot(data = kesim) + ylim(15000, 70000)+
  geom_point(mapping = aes(x =yil, y=Hindi_ton_miktari),shape =19, size = 4, color ="black")+
  labs(
    title = " Yıllar gore Hindi Eti (ton) Miktarı",
    x = "Yıllar",
    y = "Et (ton)Miktarı"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

range(kesim$`hindiKesimMiktari(ton)`)

#manda nın kesilen et miktarının adet miktarı ile arasındaki ilişkiyi veren grafik

Manda_Et_Miktari<-kesim$`mandaEtMiktari(ton)`
Manda_Kesim_Adet_Miktari<-kesim$`mandaKesimAdeti(bas)`
ggplot() +
    geom_point(mapping = aes(x = Manda_Et_Miktari, y = Manda_Kesim_Adet_Miktari), shape = 19, size = 4, color = "blue") +
    xlim(3500, 16000) +
    ylim(19000, 70000) +
    labs(
      title = "Mandaların Et Miktarı ve Kesim Adedi Dağılımı",
      x = "Et Miktarı (ton)",
      y = "Kesim Adeti (baş)"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      plot.title = element_text(hjust = 0.5)
    )
range(kesim$`mandaKesimAdeti(bas)`)
range(kesim$`mandaEtMiktari(ton)`)


#manda nın bas adeyi ve ton miktarlarının gösterimi

manda <- kesim %>%
  pivot_longer(
    cols = c( "mandaKesimAdeti(bas)",  "hindiKesimMiktari(ton)" ),
    names_to = "Adet (Bin_Bas)",
    values_to = ""
  )
ggplot(data = manda, aes(x = as.factor(`Adet (Bin_Bas)`), y = Ton)) +
  geom_boxplot(notch = FALSE, fill = "blue") +
  #geom_jitter(size = 1, color = "black", width = 0.2) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Manda Kesim(bas) Adeti ve Hindi Kesim(ton)Miktarı Yayılım grafiği",
    x = " ",
    y = "Manda(bas)/Hindi(ton) miktarı "
    )

range(kesim$`mandaKesimAdeti(bas)`)
range(kesim$`hindiKesimMiktari(ton)`)

#yatay bar grafik

kesim$Tavuk_Hindi_Toplam <- kesim$`tavukKesimMiktari(ton)` + kesim$`hindiKesimMiktari(ton)`
kesim$sigir_koyun_keci_toplam<-kesim$`sigirEtMiktari(ton)`+kesim$`koyunEtMiktari(ton)`+kesim$`keciEtMiktari(ton)`
kesim_3 <- kesim[, c( "Tavuk_Hindi_Toplam","sigir_koyun_keci_toplam","sigirEtMiktari(ton)")]
kesim_3 <- pivot_longer(kesim_3,
                          cols = everything(),
                          names_to = "Tur",
                          values_to = "Miktar")
barchart(
  Tur ~ Miktar,
  groups = Tur,
  data = kesim_3,
  stack = FALSE,
  auto.key = list(space = "right", title = "Tur"),
  par.settings = list(superpose.polygon = list(col = c("blue", "red","black"))),
  scales = list(x = list(limits = c(25000, NA))),
  xlab = "Miktar (ton)",
  main = "Sığır ve Tavuk+Hindi Toplamı"
)
#pie kullanamdım.verilerim büyük geldi.zaman çok alıyor.blg.açamadı.
#veri setim int olduğu için dummy kullanamadım.




