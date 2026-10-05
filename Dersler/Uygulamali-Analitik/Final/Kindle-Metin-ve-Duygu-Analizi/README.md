# Kindle Metin ve Duygu Analizi

Bu çalışma, Uygulamalı Analitik dersi kapsamında final projesi olarak hazırlanmıştır.

## Amaç

Amazon Kindle kullanıcı yorumlarının R programlama dili kullanılarak metin madenciliği ve duygu analizi yöntemleriyle incelenmesi amaçlanmıştır.

## Veri Seti

Çalışmada Amazon Kindle kullanıcı yorumlarını içeren `preprocessed_kindle_review.csv` veri seti kullanılmıştır. Veri setinde yıldız puanı (`rating`) ve kullanıcı yorumları (`reviewText`) başta olmak üzere ürün ve yorum bilgileri yer almaktadır.

## Uygulanan İşlemler

- Metin verisinin içe aktarılması ve kontrolü
- Metin temizleme ve ön işleme
- Corpus oluşturma
- Stopword temizliği
- Stemming
- Document-Term Matrix oluşturma
- Kelime frekansı analizi
- En sık geçen kelimelerin görselleştirilmesi
- Kelime bulutu oluşturma
- NRC duygu sözlüğü ile duygu dağılımı analizi
- Syuzhet, Bing ve AFINN duygu skorlarının hesaplanması
- Rating değerleri ile duygu skorlarının karşılaştırılması
- Logistic Regression ile pozitif/negatif sınıflandırma
- Random Forest ile değişken önemi analizi

## Kullanılan Teknolojiler

- R
- tm
- SnowballC
- wordcloud
- syuzhet
- ggplot2
- caret
- randomForest

## Dosyalar

- `Kindle-Metin-Duygu-Analizi.R`
- `Kindle-Metin-Duygu-Analizi.pptx`
- `Kindle-Metin-Duygu-Analizi.pdf`
- `preprocessed_kindle_review.csv`
