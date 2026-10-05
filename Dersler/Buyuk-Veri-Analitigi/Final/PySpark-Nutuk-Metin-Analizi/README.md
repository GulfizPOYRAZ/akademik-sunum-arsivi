# PySpark ile Nutuk Metin Analizi

Bu çalışma, Büyük Veri Analitiği dersi kapsamında final projesi olarak hazırlanmıştır.

## Amaç

Nutuk metninin PySpark kullanılarak büyük veri ve metin madenciliği yöntemleriyle analiz edilmesi amaçlanmıştır. Metin; yapısal özellikleri, kelime frekansları, TF-IDF sonuçları ve tematik kümeleri açısından incelenmiştir.

## Çalışma Kapsamı

- Nutuk metninin Apache Spark ortamına aktarılması
- Ham veri yapısının ve satır uzunluklarının incelenmesi
- Türkçe karakter ve veri kalitesi kontrolleri
- Metin temizleme ve ön işleme
- Tokenizasyon
- Stopword temizliği
- Kelime frekanslarının hesaplanması
- CountVectorizer ile sayısal temsil
- TF-IDF analizi
- N-gram analizi
- Kelime bulutu ve grafiklerle görselleştirme
- K-Means ile tematik kümeleme

## Kullanılan Teknolojiler

- Python
- PySpark
- Apache Spark
- Spark SQL
- MLlib
- CountVectorizer
- TF-IDF
- K-Means
- Matplotlib
- WordCloud

## Veri

Çalışmada Nutuk metni büyük hacimli metinsel veri olarak ele alınmıştır. Analiz öncesinde yayın bilgileri, başlıklar ve analizle doğrudan ilişkili olmayan yapısal içerikler temizlenmiş; ham ve temizlenmiş metin dosyaları ayrı olarak saklanmıştır.

## Dosyalar

- `Nutuk-Analizi.ipynb`
- `Nutuk-Sunum.pptx`
- `Nutuk-Orijinal-Metin.txt`
- `Nutuk-Temizlenmis-Metin.txt`
