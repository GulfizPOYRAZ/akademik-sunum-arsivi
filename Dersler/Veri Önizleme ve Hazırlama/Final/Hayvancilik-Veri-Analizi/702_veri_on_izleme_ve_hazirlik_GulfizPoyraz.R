
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
install.packages("arules")

 
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
library(arules)
library(dplyr)
library(lattice)
#************************* Veri Setim  **********************

kesim<-read_xlsx("C:/Users/90533/Desktop/veri_hazırlama/hayvan/hayvancılık.xlsx",sheet = 1)
kesim_tuik_kucuk_buyuk_bas<-read_xlsx("C:/Users/90533/Desktop/veri_hazırlama/hayvan/hayvancılık.xlsx",sheet = 2)
kesim_tuik_kumes<-read_xlsx("C:/Users/90533/Desktop/veri_hazırlama/hayvan/hayvancılık.xlsx",sheet = 3)
#https://data.tuik.gov.tr/Bulten/Index?p=Hayvansal-Uretim-Istatistikleri-2024-53935 tüik veri linki

#************************* Data Frame **********************

#veri setinin yeni bir sekmede  tablo olarak gösterimi
View(kesim)

# Veri setini başlıkları
head(kesim)

# Eksik değer kontrolü 
sum(is.na(kesim))

# veri sınıfları
class(kesim)

# veri setinin listelenmesi
list(kesim)

# Veri setinin boyutları
dim(kesim)

#satır,sütun ,başlıları ve veri sınıflarının açıklaması
str(kesim)

#sütün başlıkları listesi
colnames(kesim)

#veri seti içinden yeni veri seti oluşturalım
buyuk_bas_kesim<-cor(kesim[ ,c(1 ,2:5)], method="pearson")
View(buyuk_bas_kesim)


chart.Correlation(kesim[ ,c(1,3,5,7,9,11,13)],histogram=TRUE, pc=19)

#*****************  verilerin istatistiksel yayılımları/dağılımı ****************

# Veri seti ,max,min,çeyrekler,medyan değerlerinin gösterimi
summary(kesim$`sigirEtMiktari(ton)`)


# veri setinin % 0 -%25-%50-%75-%100 değerlerinin gösterimi
quantile(kesim$`sigirEtMiktari(ton)`)


# manda et miktarının 7000tondan fazla olanların gösterimi
manda_yuksek <- kesim$`mandaEtMiktari(ton)` > 7000
manda_yuksek70 <- kesim[manda_yuksek, ]
View(manda_yuksek70)


# ver setindeki en küçük ve en büyük değerlerinin gösterimi
range(kesim$`sigirEtMiktari(ton)`)


# veri içindeki değerlerin tek olanlarını alır.
unique(kesim$`sigirKesimAdeti(bas)`)


# veri içinden seçilen sütuna göre diğer sütunları küçükten büyüğe doğru sıralar.
OrdPc <- order(kesim$`mandaKesimAdeti(bas)`)
View(kesim[OrdPc, ])

#standart sapma değerlerinin gösterimi
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


#belirlenen aralıklarda kaçar tane değer aldığı değerlerin gösterimi
table(cut(kesim$`mandaKesimAdeti(bas)`,seq(19000,70000,5000)))

# n tane değer aralığı yaratmak.cut(kesim$`mandaKesimAdeti(bas)`,breaks=10)#değerler büyük olduğu için e ile gösteriyor.
cut(kesim$`mandaKesimAdeti(bas)`,breaks=7210)

#*******************  Correlation analizi   *****************

#Değişkenler arası eksik veri /doluluk durumu
missmap(kesim)

#korelasyon analizi
kor<-cor(kesim)
corrplot(kor,type="upper" ,order="hclust",col=c("blue","red"),bg="black")


#**********************  Grafik gösterimi ****************

#********************    Histogram *****
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

range(kesim$`mandaKesimAdeti(bas)` )
range(kesim$`mandaEtMiktari(ton)` )

#***************** boxplot ile gösterim **

buyuk_kucuk_bas <- kesim %>%
  pivot_longer(
    cols = c("sigirKesimAdeti(bas)", "koyunKesimAdeti(bas)", "keciKesimAdeti(bas)"),
    names_to = "buyuk_kucuk_bas",
    values_to = "Ton"
  )
ggplot(data = buyuk_kucuk_bas, aes(x = as.factor(buyuk_kucuk_bas), y = Ton)) +
  geom_boxplot(notch = FALSE, fill = "blue") +
  geom_jitter(size = 1, color = "black", width = 0.2) + #noktalı göstermek istersen (#) kaldır.
  scale_y_continuous(labels = label_number(scale = 1/10000, suffix = "K")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Buyuk ve Kucukbas Hayvan Kesim Adetleri",
    x = "Hayvan Turu",
    y = "Adet (Bin Bas)"
  )
summary(kesim$`keciKesimAdeti(bas)`)
summary(kesim$`koyunKesimAdeti(bas)`)
summary(kesim$`sigirKesimAdeti(bas)`)

#*****************  Aykırı değerleri temizledik *
#(out olunca aykırı değerleri verir)
kesim.cleaned<-na.omit(kesim)
boxplot.stats(kesim.cleaned$`koyunKesimAdeti(bas)`)$out

#***************** Aykırı değerlerin yerini göstermek *

out<-boxplot.stats(kesim.cleaned$`koyunKesimAdeti(bas)`)$out
out_row<-which(kesim.cleaned$`koyunKesimAdeti(bas)`%in%c(out))
View(kesim.cleaned[out_row,])

#************ 1.çeyrek ile 3.çeyrek arasında kalan değerler *
#Çan eğrisine göre iki uçtaki kısımlar 0,025ile 0,975 değerlerini alır
lower_bound<-quantile(kesim.cleaned$`koyunKesimAdeti(bas)`,0,025)
upper_bound<-quantile(kesim$`koyunKesimAdeti(bas)`,0,975)

#******* Tekrar eden verileri gösterir*
#benim veri setimde kesim içinde tekrar edenveri yok.
duplicated(kesim)

#************ z score standizasyon yapma *

View(kesim)
housing.z <- scale(kesim)
View(housing.z)

#**************  Eşit aralıklara bölerek kategorize edildi.***
kesim_catEW <- cut(kesim$`sigirKesimAdeti(bas)`,
                   breaks = 5,
                   labels = c("cok dusuk","dusuk","orta","yuksek"," cok yuksek"),
                   include.lowest = TRUE
)
table(kesim_catEW)
kesim$`sigirEtMiktari(ton)`<-kesim_catEW
View(kesim)

#******************  GRAFİKLER  ***

# hindi kesim adet miktarının yıllara göre nokta grafiği

Hindi_ton_miktari<-kesim$`hindiKesimMiktari(ton)`
ggplot(data = kesim) + ylim(15000, 70000)+
  geom_point(mapping = aes(x =yil, y=Hindi_ton_miktari),shape =19, size = 4, color ="black")
range(kesim$`hindiKesimMiktari(ton)`)

#manda nın kesilen et miktarının adet miktarı ile arasındaki ilişkiyi veren grafik

Manda_Et_Miktari<-kesim$`mandaEtMiktari(ton)`
Manda_Kesim_Adet_Miktari<-kesim$`mandaKesimAdeti(bas)`
ggplot() +
  geom_point(mapping = aes(x = Manda_Et_Miktari, y = Manda_Kesim_Adet_Miktari), shape = 19, size = 4, color = "blue") +
  xlim(3500, 16000) +
  ylim(19000, 70000) +
  labs(
    title = "Mandaların Et Miktarı ve Kesim(bas) Adedi Dağılımı",
    x = "Et Miktarı (ton)",
    y = "Kesim Adeti (baş)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
range(kesim$`mandaKesimAdeti(bas)`)
range(kesim$`mandaEtMiktari(ton)`)


#yatay bar grafik
kesim<-read_xlsx("C:/Users/90533/Desktop/veri_hazırlama/hayvan/hayvancılık.xlsx",sheet = 1)
kesim$sigir_koyun_keci_toplam<-kesim$`sigirEtMiktari(ton)`+kesim$`koyunEtMiktari(ton)`+kesim$`keciEtMiktari(ton)`
kesim_3 <- kesim %>%
  select(
    `hindiKesimMiktari(ton)`,
    `sigirEtMiktari(ton)`,
    `keciEtMiktari(ton)`,
    sigir_koyun_keci_toplam,
    `tavukKesimMiktari(ton)`,
    `koyunEtMiktari(ton)`
  ) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "Tur",
    values_to = "Miktar"
  )
barchart(
  Tur ~ Miktar,
  groups = Tur,
  data = kesim_3,
  stack = FALSE,
  box.width = 1,
  auto.key = list(space = "right", title = "Tur"),
  scales = list(
    x = list(
      at     = pretty(c(13000, max(kesim_3$Miktar))),
      labels = pretty(c(13000, max(kesim_3$Miktar)))
    )
  ),
  xlab = "Miktar (ton)",
  main = "Sığır,Keci,Koyun,Hindi,Tavuk Et kesim(ton) Miktarları")


#pie kullanamdım.verilerim büyük geldi.zaman çok alıyor.blg.açamadı.
#veri setim int olduğu için dummy kullanamadım.


