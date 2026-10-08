# PDF'den Excel'e Veri Çıkarma ve Temizleme

Bu proje, yarı yapılandırılmış PDF raporlarından veri çıkarılması, temizlenmesi, doğrulanması ve Excel formatında düzenlenmesi sürecini göstermektedir.

## Projenin Amacı

Çalışmanın amacı, çok sayfalı ve tablo yapısı tam düzenli olmayan PDF raporlarındaki verileri Python kullanarak yapılandırılmış bir veri setine dönüştürmektir.

Proje kapsamında:

- PDF yapısının incelenmesi
- Kayıtların ayrıştırılması
- Dağınık alanların temizlenmesi
- Benzersiz kayıtların eşleştirilmesi
- Veri doğrulama işlemleri
- Excel çıktısının hazırlanması
- Portföy için gizlilik açısından güvenli sentetik örnek oluşturulması

işlemleri gerçekleştirilmiştir.

## Kullanılan Teknolojiler

- Python
- Pandas
- Microsoft Excel

## Veri İşleme Süreci

Projede 10 sayfalık yarı yapılandırılmış bir acil çağrı raporu analiz edilmiştir.

Toplam:

- 69 kayıt yapılandırılmıştır
- 19 veri alanı standardize edilmiştir
- Kayıtlar benzersiz Log ID değerleri üzerinden eşleştirilmiştir
- Eksik ve tutarsız alanlar kontrol edilmiştir
- Yinelenen kayıtlar doğrulanmıştır

## Portföy ve Gizlilik

Gerçek kaynak dosyada kişisel bilgiler bulunduğu için GitHub üzerinde gerçek veri paylaşılmamıştır.

Public portföy sürümünde sentetik veriler kullanılmış ve kişisel olarak tanımlanabilir bilgiler çıkarılmıştır.

## Proje Dosyaları

- `PDF_to_Excel_Descriptive_Analysis.png`
- `PDF_to_Excel_Portfolio_Sample_Safe.xlsx`
- `Portfolio_Sample_Emergency_Call_Report.png`
- `Portfolio_Sample_Emergency_Call_Report_Synthetic.pdf`

## Sonuç

Bu proje, karmaşık PDF raporlarının analiz edilerek temiz ve kullanılabilir Excel veri setlerine dönüştürülmesi konusunda örnek bir veri işleme ve kalite kontrol çalışmasıdır.

Çalışma aynı zamanda gerçek verilerle çalışırken gizlilik ve veri güvenliği ilkelerinin korunmasına yönelik bir portföy örneği sunmaktadır.
