# Apple Power BI Fiyat Analizi

Bu çalışma, Veri Görselleştirme ve Raporlama dersi kapsamında final sınavı için hazırlanmıştır.

## Sınav Görevi

Final sınavında, Apple hisse senedi verileri kullanılarak Power BI üzerinde uçtan uca bir veri hazırlama, modelleme ve görselleştirme çalışması yapılması istenmiştir.

### 1. Query Editor

- `Apple-Part1` ve `Apple-Part2` veri dosyalarının yüklenmesi
- İlk satırların sütun adı olarak düzenlenmesi
- Boş, N/A, eksik ve hatalı kayıtların kontrol edilmesi
- İki veri dosyasının `Apple-Combined` adlı yeni bir sorguda birleştirilmesi
- Yalnızca tarih, açılış ve kapanış fiyatı sütunlarının bırakılması
- Gerekli filtreleme ve sütun adlandırma işlemlerinin yapılması
- Veri tiplerinin tanımlanması
- `Weekdays` adlı yardımcı tablonun oluşturulması

### 2. Data Modelling

- 04 Ocak 2010 – 11 Mayıs 2017 dönemini kapsayan `Calendar` tablosunun oluşturulması
- `Apple-Combined`, `Calendar` ve `Weekdays` tabloları arasında ilişkilerin kurulması
- Açılış ve kapanış fiyatı arasındaki yüzde değişimi gösteren `End-vs-Start` hesaplanmış sütununun oluşturulması
- `AveragePrice-End`, `MinimumPrice-End` ve `MaximumPrice-End` ölçülerinin hazırlanması

### 3. Data Visualization

- Yıllara ve aylara göre ortalama kapanış fiyatını gösteren çizgi grafiği
- 2010 Q1 – 2017 Q2 dönemini kapsayan birleşik sütun ve çizgi grafiği
- Hafta içi günlerini filtrelemek için slicer
- Hafta içi günlerine göre ortalama kapanış fiyatını gösteren bar chart
- Minimum, maksimum ve ortalama kapanış fiyatlarını gösteren gauge chart
## Çıktılar

Power BI raporunda 2010–2017 dönemi için:

- Yıllara göre ortalama kapanış fiyatları
- Çeyreklere göre fiyat değişimleri
- Günlere göre ortalama kapanış fiyatları
- Ortalama fiyat göstergeleri
- Piyasa durumu göstergesi

görselleştirilmiştir.

## Dosyalar

- `Apple-Power-BI-Analizi.pbix`
- `Apple-Power-BI-Raporu.pdf`
- `apple-part1.csv`
- `apple-part2.csv`
