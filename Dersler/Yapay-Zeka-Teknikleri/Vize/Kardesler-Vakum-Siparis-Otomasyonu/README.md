# Kardeşler Vakum – n8n Tabanlı Yapay Zeka Destekli Sipariş Otomasyon Sistemi

Bu çalışma, Yapay Zeka ve Teknolojileri dersi kapsamında vize projesi olarak hazırlanmıştır.

## Projenin Amacı

Kardeşler Vakum için müşteri siparişlerini otomatik olarak toplamak, doğrulamak, kaydetmek ve ilgili kişilere bildirim göndermek amacıyla n8n tabanlı, yapay zeka destekli bir sipariş otomasyon sistemi geliştirilmiştir.

Sistem; HTML tabanlı sipariş formu, n8n workflow yapısı, webhook bağlantıları, JavaScript işlemleri, Google Sheets, Gmail ve Google Gemini tabanlı AI Agent bileşenlerini tek bir süreçte bir araya getirmektedir.

## Kullanılan Teknolojiler

- n8n Workflow Automation
- Webhook
- JavaScript
- HTML
- JSON
- Google Sheets
- Gmail
- Google Gemini
- AI Agent

## Sistem Yapısı

Sistem, HTML tabanlı sipariş formundan gelen müşteri verilerini webhook aracılığıyla n8n workflow'una aktarmaktadır.

Sipariş verileri işlenerek:

- Müşteri bilgileri alınır
- Ürün ve miktar bilgileri kontrol edilir
- Sipariş numarası oluşturulur
- Tarih ve saat bilgileri eklenir
- Sipariş doğrulaması yapılır
- Doğru ve hatalı siparişler ayrıştırılır
- Veriler Google Sheets'e kaydedilir
- Müşteri ve firma için otomatik e-posta bildirimleri oluşturulur

## Yapay Zeka Kullanımı

Projede Google Gemini tabanlı AI Agent kullanılarak sipariş bilgilerinin kontrol edilmesi ve eksik veya hatalı alanların belirlenmesi amaçlanmıştır.

AI Agent çıktıları yapılandırılmış JSON formatında işlenerek n8n otomasyon akışı içerisinde kullanılmaktadır.

## Web Arayüzü

Kullanıcıların sipariş oluşturabilmesi için HTML tabanlı bir ürün ve sipariş formu hazırlanmıştır.

Arayüzde:

- Ürün kartları
- Ürün kodları
- Telefon numarası format kontrolü
- Ürün kodu doğrulaması
- Sipariş miktarı
- Müşteri mesajı
- Başarılı ve hatalı işlem bildirimleri

yer almaktadır.

## Proje Dosyaları

- `Kardesler-Vakum-Siparis-Otomasyon-Raporu.docx`
- `Yapay-Zeka-Teknikleri-Vize-Sinav-Dokumani.pdf`
- `Kardesler-Vakum-n8n-Workflow.json`
- `Kardesler-Vakum-Siparis-Formu.html`
- `Kardesler-Vakum-Webhook-Test.js`
- `1.png`
- `2.png`
- `3.png`
- `4.png`
- `5.png`
- `6.png`
- `7.png`
- `8.png`
- `logo.png`

## Sonuç

Proje ile sipariş sürecinin manuel işlemlerden çıkarılarak otomatik, izlenebilir ve daha düzenli bir yapıya dönüştürülmesi amaçlanmıştır.

Geliştirilen sistem; n8n tabanlı workflow yapısı üzerinde sipariş doğrulama, veri kaydı, yapay zeka destekli kontrol ve otomatik e-posta bildirimlerini tek bir otomasyon sürecinde birleştirmektedir.

## Sistem Yapısı

Sistem, HTML tabanlı sipariş formundan gelen müşteri verilerini webhook aracılığıyla n8n workflow'una aktarmaktadır.

Sipariş verileri işlenerek:

- Müşteri bilgileri alınır
- Ürün ve miktar bilgileri kontrol edilir
- Sipariş numarası oluşturulur
- Tarih ve saat bilgileri eklenir
- Sipariş doğrulaması yapılır
- Doğru ve hatalı siparişler ayrıştırılır
- Veriler Google Sheets'e kaydedilir
- Müşteri ve firma için otomatik e-posta bildirimleri oluşturulur

## Yapay Zeka Kullanımı

Projede Google Gemini tabanlı AI Agent kullanılarak sipariş bilgilerinin kontrol edilmesi ve eksik veya hatalı alanların belirlenmesi amaçlanmıştır.

AI Agent çıktıları yapılandırılmış JSON formatında işlenerek otomasyon akışında kullanılmaktadır.

## Web Arayüzü

Kullanıcıların sipariş oluşturabilmesi için HTML tabanlı bir ürün ve sipariş formu hazırlanmıştır.

Arayüzde:

- Ürün kartları
- Ürün kodları
- Telefon numarası format kontrolü
- Ürün kodu doğrulaması
- Sipariş miktarı
- Müşteri mesajı
- Başarılı ve hatalı işlem bildirimleri

yer almaktadır.

## Proje Dosyaları

- `Kardesler-Vakum-Siparis-Otomasyon-Raporu.docx`
- `Yapay-Zeka-Teknikleri-Vize-Sinav-Dokumani.pdf`
- `Kardesler-Vakum-n8n-Workflow.json`
- `Kardesler-Vakum-Siparis-Formu.html`
- `Kardesler-Vakum-Webhook-Test.js`
- `1.png`
- `2.png`
- `3.png`
- `4.png`
- `5.png`
- `6.png`
- `7.png`
- `8.png`
- `logo.png`

## Sonuç

Proje ile sipariş sürecinin manuel işlemlerden çıkarılarak otomatik, izlenebilir ve daha düzenli bir yapıya dönüştürülmesi amaçlanmıştır.

Geliştirilen sistem; sipariş doğrulama, veri kaydı, yapay zeka destekli kontrol ve otomatik e-posta bildirimlerini tek bir workflow üzerinde bir araya getirmektedir.
