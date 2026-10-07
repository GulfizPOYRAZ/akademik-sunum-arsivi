# Twitter ABD Havayolu Duygu Analizi

Bu çalışma, Web ve Metin Analitiği dersi kapsamında dönem finali olarak hazırlanmıştır.

## Proje Konusu

Twitter üzerinden havayolu firmalarına yönelik paylaşılan kullanıcı yorumları kullanılarak duygu analizi ve metin sınıflandırma çalışması gerçekleştirilmiştir.

Veri setindeki tweetler pozitif, negatif ve nötr duygu sınıfları açısından incelenmiş; metin ön işleme, keşifsel analiz, özellik çıkarımı ve makine öğrenmesi modelleri uygulanmıştır.

## Uygulanan Analizler

- Veri setinin yapısının ve eksik değerlerin incelenmesi
- Metin temizleme ve ön işleme
- Kelime frekansı analizi
- Kelime bulutu
- Duygu sınıfı dağılımının incelenmesi
- Bigram ve trigram analizi
- TF-IDF özellik çıkarımı
- Logistic Regression
- Multinomial Naive Bayes
- Support Vector Machine (SVM)
- Confusion Matrix ve model performanslarının karşılaştırılması
- Orijinal ve dengelenmiş veri setlerinin karşılaştırılması
- Sınıf dengesizliğinin model sonuçları üzerindeki etkisinin değerlendirilmesi

## Genel Bulgular

Analiz sonucunda kullanıcı yorumlarının özellikle uçuş iptali, gecikme ve müşteri hizmetleri gibi konular etrafında yoğunlaştığı görülmüştür.

Orijinal veri setinde negatif duygu sınıfının baskın olduğu belirlenmiştir. Sınıf dengeleme işlemi sonrasında doğruluk değerlerinde düşüş görülmesine rağmen, modellerin pozitif, negatif ve nötr sınıfları daha dengeli biçimde ayırt edebildiği gözlemlenmiştir.

Çalışma, duygu analizi ve metin sınıflandırma problemlerinde yalnızca doğruluk değerine değil, sınıf dağılımı ve farklı performans ölçütlerine birlikte bakılması gerektiğini göstermektedir.

## Dosyalar

- `Twitter-ABD-Havayolu-Duygu-Analizi-Raporu.docx`
- `Twitter-ABD-Havayolu-Duygulari.csv`

> Analizde kullanılan kod dosyası daha sonra bulunması durumunda bu klasöre eklenecektir.
