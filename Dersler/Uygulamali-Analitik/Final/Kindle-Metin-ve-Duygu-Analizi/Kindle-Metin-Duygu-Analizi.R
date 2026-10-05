

#---------------------GEREKLI PAKETLERIN YuKLENMESI ------------------------------------------
install.packages("tm")           # Metin madenciligi
install.packages("SnowballC")    # Kelime kokleri
install.packages("wordcloud")    # Kelime bulutu
install.packages("RColorBrewer") # Renk paletleri
install.packages("syuzhet")      # Duygu analizi
install.packages("ggplot2")      # Grafik cizimi
install.packages(c("caret","e1071","glmnet","pROC"))
install.packages("caret")
install.packages("klaR")
library(tm)           
library(wordcloud)    
library(RColorBrewer) 
library(syuzhet)      
library(ggplot2)      
---------------------------------------------------------------------------------------------
 
  
  
   # VERIYI ICE AKTARMA
  
dosya_yolu <- file.choose()

kindle <- read.csv(dosya_yolu,
                   
                   stringsAsFactors = FALSE)

#Veri yapısına hızlı bakıS
View(kindle)
head(kindle)         # Veri Seti Kontrolu
str(kindle)          # Veri Istatistigi
colnames(kindle)     # DegiSken Adları


# Gereksiz ID sutununu (X) kaldır

if ("X" %in% names(kindle)) {
    kindle$X <- NULL
  }





------------------------------------------------------------------------------------------------
  
  ## METIN VEKToRu (SENTIMENT IcIN HAM METIN)
  
  
  # Metin vektorunu al, Metnin oldugu sutun adı sende: reviewText
text_col <- "reviewText"
text_vec <- kindle[[text_col]]

names(kindle)

# NA / boSları temizle
text_vec <- text_vec[!is.na(text_vec)]
text_vec <- trimws(text_vec)
text_vec <- text_vec[nchar(text_vec) > 0]

cat("Kullanılacak yorum sayısı:", length(text_vec), "\n")





------------------------------------------------------------------------------------------
  
  ### CORPUS OLUSTURMA (FREKANS/DTM/WORDCLOUD IcIN) -
  
  
  library(tm)

corpus <- VCorpus(VectorSource(text_vec))

# Kontrol: ilk 2 dokuman

inspect(corpus[1:2]) #sutunlar icerik uzunlugunu gostermektedir.

--------------------------------------------------------------------------------------------
  
  #### METIN oN ISLEME (TM ILE TEMIZLEME)
  
  

library(SnowballC)
corpus <- tm_map(corpus, stemDocument)

# Kontrol: temizlenmiS ilk 2 dokuman
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

corpus <- tm_map(corpus, toSpace, "/")
corpus <- tm_map(corpus, toSpace, "@")
corpus <- tm_map(corpus, toSpace, "\\|")

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus[1:2])

----------------------------------------------------------------------------------------------------
  
  ##### DTM (VACTORIZATION ) + KELIME FREKANSLARI
  
  
dtm <- DocumentTermMatrix(corpus) #metinleri sayısal bir tabloya donuSturulur
View(dtm)
cat("DTM boyutu (dokuman x kelime):", dim(dtm)[1], "x",dim(dtm)[2], "\n")

freq <- colSums(as.matrix(dtm))
freq <- sort(freq, decreasing = TRUE)

word_freq <- data.frame(
    word = names(freq),
    freq = as.numeric(freq),
    row.names = NULL
  )

#Tüm Kelime Frekansları
head(word_freq, 10)


# Top 5 En Sık Gecen Kelime BARPLOT Grafigi

top5 <- head(word_freq,5)

# Sag tarafta yazılar icin boSluk ac (sag marjı buyut)
par(mar = c(5, 6, 4, 8))

bp <- barplot(
    top5$freq,
    names.arg = top5$word,
    horiz = TRUE,
    las = 1,
    col = "#2C7FB8",
    main = "En Sık Gecen 5 Kelime",
    xlab = "Frekans",
    xlim = c(0,20000  )  # yazılar taSmasın diye alan geniSlet
  )

text(
    x = top5$freq,
    y = bp,                 # <- barplot'un gercek y konumları
    labels = top5$freq,
    pos = 4,
    cex = 0.8,
    xpd = NA                # <- panel dıSına taSsa bile goster
  )


## En Sık Gorulen 5 Kelime (Pasta Grafik) Sayısal
# Top 5 kelime
#top5 <- head(word_freq, 5) gereksiz

# Etiketleri sayısal frekansla hazırla
labels_count <- paste0(top5$word, " (", top5$freq, ")")

# YumuSak & akademik renk paleti
soft_colors <- c("#4E79A7", "#59A14F", "#F28E2B", "#B07AA1", "#76B7B2")

# Sayısal degerli pasta grafik
pie(
   top5$freq,
    labels = labels_count,
    col = soft_colors,
    main = "En Sık Gorulen 5 Kelime (Frekans Dagılımı)"
  )


--------------------------------------------------------------------------------------------
  
  
    ###### WORDCLOUD Word Cloud (Kelime Bulutu)
  
  
library(wordcloud)
library(RColorBrewer)

set.seed(123)
wordcloud(
    words = word_freq$word,
    freq  = word_freq$freq,
    min.freq = 20,
    max.words = 100, #(en sık gecen kelime sınırı)
    random.order = FALSE,
    rot.per = 0.35,
    colors = brewer.pal(8, "Dark2")
  )

head(word_freq, 10)

---------------------------------------------------------------------------------------------------------
    ######## NRC (National Research Council) duygu sozlugu 
  # anger = "Ofke"  ,
  # anticipation = "Beklenti"   ,
  # disgust = "Tiksinti"  , 
  # fear = "Korku"  ,
  # joy = "Mutluluk"   ,
  # sadness = "Uzuntu"  ,
  # surprise = "Saskinlik",   
  # trust = "Guven") 
  
# NRC duygu skorları
  install.packages("syuzhet")
library(syuzhet)

nrc_emotions <- get_nrc_sentiment(text_vec)

# 8 duygu toplamı (1:8) -> yuzdeye cevir
emotion_totals <- colSums(nrc_emotions[, 1:8])
emotion_totals <- sort(emotion_totals, decreasing = FALSE)

nrc_pct <- round(100 * emotion_totals / sum(emotion_totals), 2)

# Kenar boSlukları (solda duygu isimleri icin geniS)
par(mar = c(5, 8, 4, 6))

# X eksenini geniSlet (etiketler dıSarı taSmasın)
x_max <- max(nrc_pct) + 4

# Barplot ciz
bar_pos <- barplot(
    nrc_pct,
    horiz = TRUE,
    las = 1,
    main = "NRC Metindeki Duygular (Yuzde)",
    xlab = "Yuzde (%)",
    cex.names = 0.95,
    col = "#D9D9D9",
    border = "gray40",
    xlim = c(0, x_max)
  )

# Bar sonlarına yuzde etiketini yazdır
text(
    x = nrc_pct + 0.4,
    y = bar_pos,
    labels = paste0(nrc_pct, "%"),
    pos = 4,
    cex = 0.95,
    col = "black"
  )



---------------------------------------------------------------------------------  
  
  ####### SENTIMENT (SYUZHET / BING / AFINN)
  
  
  library(syuzhet)

sent_syuzhet <- get_sentiment(text_vec, method = "syuzhet")
sent_bing    <- get_sentiment(text_vec, method = "bing")
sent_afinn   <- get_sentiment(text_vec, method = "afinn")

summary(sent_syuzhet)
summary(sent_bing)
summary(sent_afinn)


-------------------------------------------
  # Ilk 500 icin guvenli uzunluk
  n <- min(
    500,
    length(sent_syuzhet),
    length(sent_bing),
    length(sent_afinn)
  )

cmp_df <- data.frame(
  index   = 1:n,
  syuzhet = sent_syuzhet[1:n],
  bing    = sent_bing[1:n],
  afinn   = sent_afinn[1:n]
)

matplot(
  cmp_df$index,
  as.matrix(cmp_df[, c("syuzhet", "bing", "afinn")]),
  type = "l",
  lty = 1,
  col = c("#4C72B0", "#55A868", "#DD8452"),
  lwd = 1.6,
  xlab = "Yorum Index (Ilk 500)",
  ylab = "Duygu Skoru",
  main = "Syuzhet, Bing ve AFINN Yontemlerinin Karsilastirilmasi",
  cex.lab = 1.2,
  cex.main = 1.2
)

legend(
  "topright",
  legend = c("Syuzhet", "Bing", "AFINN"),
  col = c("#4C72B0", "#55A868", "#DD8452"),
  lty = 1,
  lwd = 2,
  bty = "n"
)

  
------------------------------------------------------------------------------------------------
  
 
   
  #########  RATING ILE Syuzhet,Bing ve Afinn DUYGU ILISKISI
  
  
  df_sent <- data.frame(
      rating  = kindle$rating,
        syuzhet = sent_syuzhet,
        bing    = sent_bing,
        afinn   = sent_afinn
      )

# NA değerleri temizle
df_sent <- df_sent[complete.cases(df_sent), ]

# Syuzhet  

boxplot(
    syuzhet ~ rating,
    data = df_sent,
    main = "Rating'e Gore Syuzhet Duygu Skoru",
    xlab = "Rating",
    ylab = "Syuzhet Skoru",
    col = "#D9E6F2",
    border = "#4E79A7",
    outline = FALSE
  )

# Ortalama Syuzhet skorları
means_syuzhet <- tapply(df_sent$syuzhet, df_sent$rating, mean, na.rm = TRUE)

points(
    1:5,
    means_syuzhet,
    col = "#4E79A7",
    pch = 19,
    cex = 1.3
  )


##Bing

boxplot(
    bing ~ rating,
    data = df_sent,
    main = "Rating'e Gore Bing Duygu Skoru",
    xlab = "Rating",
    ylab = "Bing Skoru",
    col = "#E8F3E8",
    border = "#59A14F",
    outline = FALSE
  )

means_bing <- tapply(df_sent$bing, df_sent$rating, mean, na.rm = TRUE)
points(1:5, means_bing, col = "#59A14F", pch = 19, cex = 1.3)


###Afinn

boxplot(
    afinn ~ rating,
    data = df_sent,
    main = "Rating'e Gore Afinn Duygu Skoru",
    xlab = "Rating",
    ylab = "Afinn Skoru",
    col = "#FBE6D5",
    border = "#DD8452",
    outline = FALSE
  )

means_afinn <- tapply(df_sent$afinn, df_sent$rating, mean, na.rm = TRUE)
points(1:5, means_afinn, col = "#DD8452", pch = 19, cex = 1.3)




############################################################
# BASİT ML: Duygu skorları ile Rating tahmini (Binary)
# Model: Logistic Regression
# 1-2 = Negatif (0), 4-5 = Pozitif (1), 3 = çıkar
############################################################

# 0) Kontrol
need_cols <- c("rating","syuzhet","bing","afinn")
if(!exists("df_sent")) stop("df_sent yok. Önce df_sent oluşturmalısın.")
if(!all(need_cols %in% names(df_sent))){
    stop(paste("df_sent şu sütunları içermeli:", paste(need_cols, collapse=", ")))
 }

# 1) Temiz veri
df_ml <- df_sent[, need_cols]
df_ml <- df_ml[complete.cases(df_ml), ]

# 2) Rating'i 2 sınıfa indir
df_ml$y <- ifelse(df_ml$rating <= 2, 0,
        ifelse(df_ml$rating >= 4, 1, NA))
df_ml <- df_ml[!is.na(df_ml$y), ]
df_ml$y <- factor(df_ml$y, levels=c(0,1))

cat("Sınıf dağılımı (0=neg, 1=pos):\n")
print(table(df_ml$y))


# 3) Train/Test (70/30)
set.seed(123)
idx <- sample(seq_len(nrow(df_ml)), size = floor(0.7*nrow(df_ml)))
train <- df_ml[idx, ]
test  <- df_ml[-idx, ]
# 4) Modeli kur
logit_model <- glm(y ~ syuzhet + bing + afinn, data = train, family = binomial())

# 5) Tahmin
prob <- predict(logit_model, newdata = test, type = "response")
pred <- factor(ifelse(prob >= 0.5, 1, 0), levels=c(0,1))

# 6) Sonuç (Confusion Matrix + Accuracy)
cm <- table(Predicted = pred, Actual = test$y)
acc <- sum(diag(cm)) / sum(cm)

cat("\n--- Confusion Matrix ---\n")
print(cm)
cat("\nAccuracy:", round(acc, 4), "\n")

# 7) (İsteğe bağlı) Model katsayıları (yorumlamak için)
cat("\n--- Model Özeti (katsayılar) ---\n")
print(summary(logit_model))



#grafik olarak görsel sunum

#Değişken önemleri
# Paket
library(randomForest)

# 1) Random Forest modeli
rf_model <- randomForest(
  y ~ syuzhet + bing + afinn,
  data = train,
  importance = TRUE
)

# 2) Değişken önemini al
imp <- randomForest::importance(rf_model)

# 3) Data frame'e çevir
imp_df <- data.frame(
  feature = rownames(imp),
  MDA = imp[, "MeanDecreaseAccuracy"]
)

# 4) Önem değerine göre sırala
imp_df <- imp_df[order(imp_df$MDA, decreasing = TRUE), ]

# 5) Grafik ayarları (sağ tarafa yazı sığması için)
par(mar = c(5, 8, 4, 6))

# X eksenini genişlet
x_max <- max(imp_df$MDA) + 10

# 6) Barplot
bp <- barplot(
  imp_df$MDA,
  names.arg = imp_df$feature,
  horiz = TRUE,
  las = 1,
  col = "#F4A261",
  border = "#E76F51",
  main = "Random Forest – Değişken Önemi (Mean Decrease Accuracy)",
  xlab = "Mean Decrease Accuracy",
  xlim = c(0, x_max)
)

# 7) Sayısal değerleri ekle
text(
  x = imp_df$MDA + 1,
  y = bp,
  labels = round(imp_df$MDA, 2),
  pos = 4,
  cex = 1,
  col = "black",
  xpd = NA
)































########kendime çalışma
# 8 duygu toplamı (1:8) -> yuzdeye cevir
emotion_totals <- colSums(nrc_emotions[, 1:8])
emotion_totals <- sort(emotion_totals, decreasing = FALSE)

nrc_pct <- round(100 * emotion_totals / sum(emotion_totals), 2)

# EN -> TR duygu sozlugu
emotion_tr <- c(
  anger = "Ofke",
  anticipation = "Beklenti",
  disgust = "Tiksinti",
  fear = "Korku",
  joy = "Mutluluk",
  sadness = "Uzuntu",
  surprise = "Saskinlik",
  trust = "Guven"
)

# EN + TR isimleri AYNI YERE yaz
names(nrc_pct) <- paste0(
  names(nrc_pct), " (", emotion_tr[names(nrc_pct)], ")"
)

# Kenar bosluklari
par(mar = c(5, 14, 4, 6))

# X ekseni payi
x_max <- max(nrc_pct) + 4

# Barplot
bar_pos <- barplot(
  nrc_pct,
  horiz = TRUE,
  las = 1,
  main = "NRC Metindeki Duygular (Yuzde)",
  xlab = "Yuzde (%)",
  col = "#D9D9D9",
  border = "gray40",
  xlim = c(0, x_max)
)

# Yuzde etiketleri
text(
  x = nrc_pct + 0.4,
  y = bar_pos,
  labels = paste0(nrc_pct, "%"),
  pos = 4,
  cex = 0.9
)
