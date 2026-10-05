# Meme Kanseri Tespiti

Bu çalışma, Sağlık Analitiği dersi kapsamında final projesi olarak hazırlanmıştır.

## Amaç

Wisconsin Meme Kanseri veri seti kullanılarak benign ve malign vakaların makine öğrenmesi yöntemleriyle sınıflandırılması ve farklı modellerin performanslarının karşılaştırılması amaçlanmıştır.

## Veri Ön İşleme

- Eksik veri kontrolü
- Aykırı değerlerin IQR yöntemiyle incelenmesi
- Kategorik değişkenlerin sayısal forma dönüştürülmesi
- StandardScaler ile ölçeklendirme
- Korelasyon matrisi ve ısı haritası analizi

## Kullanılan Yöntemler

- Logistic Regression
- K-Nearest Neighbors
- Decision Tree
- Random Forest
- Gaussian Naive Bayes
- Support Vector Machine

## Model Değerlendirme

Modeller; Accuracy, Precision, Recall, F1 Score, Confusion Matrix ve ROC-AUC değerleri kullanılarak karşılaştırılmıştır.

## Sonuç

Çalışmada Random Forest, Logistic Regression ve Decision Tree modelleri güçlü performans göstermiştir. Özellikle Random Forest modeli yüksek Recall ve AUC değerleriyle öne çıkmıştır.

## Dosyalar

- `MEME KANSERİ TESPİTİ.pdf`
- `meme_kanseri.xlsx`

