# Personel Kayıt ve Otomatik Kart Sistemi

Bu çalışma, Yapay Zeka ve Teknolojileri dersi kapsamında final projesi olarak hazırlanmıştır.

## Projenin Amacı

Projenin amacı, personel bilgilerinin dijital ortamda kaydedilmesi ve kayıtlı personellere doğum günü ile özel günlerde otomatik olarak kişiselleştirilmiş kutlama kartları gönderilmesini sağlayan bir otomasyon sistemi geliştirmektir.

## Sistem Yapısı

Proje iki temel bileşenden oluşmaktadır:

### Personel Kayıt Formu

HTML, CSS ve JavaScript kullanılarak personel kayıt arayüzü geliştirilmiştir.

Form üzerinden:

- Ad soyad
- Telefon
- Doğum tarihi
- Kayıt tarihi
- E-posta
- Cinsiyet
- Departman
- Görev
- Personel fotoğrafı

bilgileri alınmaktadır.

Personel fotoğrafları Cloudinary üzerinde saklanmakta, personel kayıtları ise Supabase veritabanına aktarılmaktadır.

### n8n Otomasyon Sistemi

n8n workflow yapısı kullanılarak Supabase üzerinde kayıtlı personel bilgileri otomatik olarak kontrol edilmektedir.

Sistem kapsamında:

- Günlük personel kayıtlarının kontrol edilmesi
- Doğum günü olan personelin belirlenmesi
- Yaklaşan doğum günlarının tespit edilmesi
- Kişiye özel kutlama kartlarının oluşturulması
- Cloudinary üzerinden dinamik görsel üretimi
- Gmail üzerinden otomatik e-posta gönderimi
- Dünya Kadınlar Günü gibi özel günlerin kontrol edilmesi
- Yeni yıl kutlama kartlarının otomatik gönderilmesi

işlemleri gerçekleştirilmektedir.

## Kullanılan Teknolojiler

- n8n Workflow Automation
- HTML
- CSS
- JavaScript
- Supabase
- Cloudinary
- Gmail
- JSON

## Kişiselleştirilmiş Kart Sistemi

Personel fotoğrafı, adı ve ilgili tarih bilgileri kullanılarak Cloudinary üzerinde dinamik kart görselleri oluşturulmaktadır.

Bu kartlar n8n workflow üzerinden ilgili personelin e-posta adresine otomatik olarak gönderilmektedir.

## Gizlilik

GitHub üzerinde paylaşılan proje sürümünde kişisel personel fotoğrafları ve kişisel veriler paylaşılmamıştır.

Workflow dosyası, sistem mimarisini ve otomasyon mantığını gösterecek şekilde kişisel verilerden arındırılmıştır.

## Proje Dosyaları

- `DG-Personel-Kayit-Formu.html`
- `Final-Kart-Gonderme-n8n-Workflow.json`
- `gorsel.png`

## Sonuç

Bu proje ile personel kayıt süreci, doğum günü takibi ve özel gün iletişimlerinin tek bir otomasyon yapısı altında birleştirilmesi amaçlanmıştır.

Supabase, Cloudinary, Gmail ve n8n entegrasyonları sayesinde personel verilerinin yönetimi ve kişiselleştirilmiş kutlama süreçleri otomatik hale getirilmiştir.
