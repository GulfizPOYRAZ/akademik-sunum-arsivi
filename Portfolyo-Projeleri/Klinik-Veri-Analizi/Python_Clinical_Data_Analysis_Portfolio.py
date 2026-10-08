# -*- coding: utf-8 -*-

# %% HÜCRE 01 — Gerekli Paketleri Hazırla

import importlib
import subprocess
import sys


def paket_hazirla(modul_adi, paket_adi=None):
    """Eksik paketi yalnızca gerektiğinde kurar."""
    paket_adi = paket_adi or modul_adi
    try:
        importlib.import_module(modul_adi)
    except ImportError:
        subprocess.check_call(
            [sys.executable, "-m", "pip", "install", paket_adi, "-q"]
        )


PAKETLER = {
    "pandas": "pandas",
    "numpy": "numpy",
    "openpyxl": "openpyxl",
    "scipy": "scipy",
    "docx": "python-docx",
}

for modul, paket in PAKETLER.items():
    paket_hazirla(modul, paket)

print("Gerekli paketler hazır.")

# %% HÜCRE 02 — Kütüphaneleri Çağır

import os
import re
import unicodedata
import warnings
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.stats import fisher_exact, mannwhitneyu, spearmanr

try:
    from IPython.display import display
except ImportError:
    def display(nesne):
        print(nesne)

pd.set_option("display.max_columns", None)
pd.set_option("display.max_rows", 100)
pd.set_option("display.width", 180)

warnings.filterwarnings("ignore", category=RuntimeWarning)

print("Kütüphaneler hazır.")

# %% HÜCRE 03 — Dosya ve Analiz Ayarları

KLASOR_YOLU = Path(".")

EXCEL_DOSYA_ADI = "anonymized_clinical_data.xlsx"
EXCEL_YOLU = KLASOR_YOLU / EXCEL_DOSYA_ADI

CIKTI_EXCEL_ADI = "clinical_analysis_outputs.xlsx"
CIKTI_WORD_ADI = "clinical_analysis_report.docx"

CIKTI_EXCEL_YOLU = KLASOR_YOLU / CIKTI_EXCEL_ADI
CIKTI_WORD_YOLU = KLASOR_YOLU / CIKTI_WORD_ADI

MIN_HASTA_NO = 1
MAX_HASTA_NO = 20
HASTA_GRUBU_SON_NO = 10

VHIT_DUSUK_GAIN_ESIGI = 0.80
OPK_DUSUK_ESIGI = 56
SAKKAD_UZUN_LATANS_ESIGI = 250
ANLAMLILIK_DUZEYI = 0.05

# Excel'deki değerler esas alınır. Doğrulanmış bir manuel düzeltme gerekiyorsa
# yalnızca burada açıkça tanımlanmalıdır. Varsayılan olarak düzeltme yoktur.
MANUEL_DUZELTMELER = {
    # Örnek:
    # "H10": {
    #     "SP_Sag_Goz_Kazanc_%": 74.4,
    #     "SP_Sol_Goz_Kazanc_%": 82.2,
    # }
}

KLASOR_YOLU.mkdir(parents=True, exist_ok=True)

print("Çalışma klasörü:", KLASOR_YOLU)
print("Excel dosyası:", EXCEL_YOLU)
print("Excel dosyası var mı?:", EXCEL_YOLU.exists())

if not EXCEL_YOLU.exists():
    raise FileNotFoundError(
        f"Excel dosyası bulunamadı: {EXCEL_YOLU}\n"
        "Dosya adını veya KLASOR_YOLU ayarını kontrol edin."
    )

# %% HÜCRE 04 — Yardımcı Fonksiyonlar


def metin_norm(deger):
    """Türkçe karakterleri ve gereksiz işaretleri arama için standartlaştırır."""
    if pd.isna(deger):
        return ""

    metin = str(deger).strip().lower()
    ceviri = str.maketrans({
        "ç": "c", "ğ": "g", "ı": "i", "İ": "i",
        "ö": "o", "ş": "s", "ü": "u",
    })
    metin = metin.translate(ceviri)
    metin = unicodedata.normalize("NFKD", metin)
    metin = "".join(ch for ch in metin if not unicodedata.combining(ch))
    metin = re.sub(r"[^a-z0-9%]+", " ", metin)
    return re.sub(r"\s+", " ", metin).strip()


def benzersiz_sutun_adlari(sutunlar):
    """Boş veya tekrar eden Excel başlıklarını güvenli adlara dönüştürür."""
    sonuc = []
    sayac = {}

    for i, sutun in enumerate(sutunlar):
        if pd.isna(sutun) or str(sutun).strip() == "":
            temel = f"Bos_Sutun_{i + 1}"
        else:
            temel = str(sutun).strip()

        adet = sayac.get(temel, 0)
        sayac[temel] = adet + 1
        sonuc.append(temel if adet == 0 else f"{temel}_{adet + 1}")

    return sonuc


def sayisal_deger(deger):
    """Virgüllü, birimli veya sınırlı metinle yazılmış sayıları float yapar."""
    if pd.isna(deger):
        return np.nan

    if isinstance(deger, (int, float, np.integer, np.floating)):
        return float(deger)

    metin = str(deger).strip().lower()
    if metin in {"", "nan", "none", "-", "—"}:
        return np.nan

    duzeltmeler = {
        "bir nokta 04": "1.04",
        "bir nokta04": "1.04",
        "bir nokta 01": "1.01",
        "bir nokta01": "1.01",
        "on alti": "16",
        "on altı": "16",
        "on yedi": "17",
        "on sekiz": "18",
        "on uc": "13",
        "on üç": "13",
        "otuz": "30",
        "elli": "50",
    }

    if metin in duzeltmeler:
        metin = duzeltmeler[metin]

    metin = metin.replace("%", "").replace(",", ".")
    eslesme = re.search(r"[-+]?\d+(?:\.\d+)?", metin)
    return float(eslesme.group()) if eslesme else np.nan


def hasta_no_sayisal(deger):
    sayi = sayisal_deger(deger)
    if pd.isna(sayi):
        return np.nan
    return int(sayi)


def kisi_id_uret(hasta_no):
    if pd.isna(hasta_no):
        return np.nan
    hasta_no = int(hasta_no)
    if hasta_no <= HASTA_GRUBU_SON_NO:
        return f"H{hasta_no:02d}"
    return f"K{hasta_no - HASTA_GRUBU_SON_NO:02d}"


def grup_uret(hasta_no):
    if pd.isna(hasta_no):
        return np.nan
    return "Hasta" if int(hasta_no) <= HASTA_GRUBU_SON_NO else "Kontrol"


def ilk_dolu_deger(seri):
    for deger in seri:
        if pd.notna(deger) and str(deger).strip().lower() not in {"", "nan", "none"}:
            return deger
    return np.nan


def sutunlari_bul(df, tumu=(), herhangi=(), haric=()):
    """Normalize edilmiş sütun adlarında güvenli anahtar kelime araması yapar."""
    tumu = [metin_norm(x) for x in tumu]
    herhangi = [metin_norm(x) for x in herhangi]
    haric = [metin_norm(x) for x in haric]

    bulunan = []
    for sutun in df.columns:
        ad = metin_norm(sutun)

        if tumu and not all(kelime in ad for kelime in tumu):
            continue
        if herhangi and not any(kelime in ad for kelime in herhangi):
            continue
        if haric and any(kelime in ad for kelime in haric):
            continue

        bulunan.append(sutun)

    return bulunan


def en_uygun_sutun(df, aday_gruplari):
    """Verilen koşul gruplarından ilk eşleşen sütunu döndürür."""
    for kosullar in aday_gruplari:
        bulunan = sutunlari_bul(df, **kosullar)
        if bulunan:
            return bulunan[0]
    return None


def gercek_veri_var_mi(satir, bilgi_sutunlari):
    for deger in satir[bilgi_sutunlari]:
        if pd.notna(deger) and str(deger).strip().lower() not in {"", "nan", "none", "-"}:
            return True
    return False


def kaynak_on_ekle(df, on_ek):
    """Birleştirmede aynı adlı sütunların birbirini ezmesini engeller."""
    yeni_adlar = {
        sutun: f"{on_ek}__{sutun}"
        for sutun in df.columns
        if sutun != "Hasta No"
    }
    return df.rename(columns=yeni_adlar)


def uc_durumlu_sinifla(seri, kosul):
    """Eksik değeri eksik bırakır; yalnızca mevcut veriyi Var/Yok yapar."""
    sonuc = pd.Series(np.nan, index=seri.index, dtype=object)
    mevcut = seri.notna()
    sonuc.loc[mevcut & kosul] = "Var"
    sonuc.loc[mevcut & ~kosul] = "Yok"
    return sonuc


def p_yaz(p):
    if pd.isna(p):
        return "p değeri hesaplanamadı"
    if p < 0.001:
        return "p<0.001"
    return f"p={p:.4f}"


def deger_yaz(deger, basamak=2):
    if pd.isna(deger):
        return "hesaplanamadı"
    return f"{float(deger):.{basamak}f}"


UYARILAR = []


def uyari_ekle(mesaj):
    UYARILAR.append(mesaj)
    print("UYARI:", mesaj)

# %% HÜCRE 05 — Excel Sayfalarını Adlarıyla Bul

excel = pd.ExcelFile(EXCEL_YOLU)

print("Excel içindeki sayfalar:")
for i, sayfa in enumerate(excel.sheet_names):
    print(i, "=>", repr(sayfa))


def sayfa_bul(arananlar):
    normalize_harita = {metin_norm(ad): ad for ad in excel.sheet_names}

    for aranan in arananlar:
        aranan_n = metin_norm(aranan)
        if aranan_n in normalize_harita:
            return normalize_harita[aranan_n]

    for aranan in arananlar:
        aranan_n = metin_norm(aranan)
        for temiz_ad, gercek_ad in normalize_harita.items():
            if aranan_n in temiz_ad or temiz_ad in aranan_n:
                return gercek_ad

    raise KeyError(
        f"Sayfa bulunamadı. Aranan adlar: {arananlar}. "
        f"Mevcut sayfalar: {excel.sheet_names}"
    )


SAYFA_ADLARI = {
    "odyometri": sayfa_bul(["odyometri", "audiometry"]),
    "timpanometri": sayfa_bul(["timpanometri", "tympanometry"]),
    "dhi": sayfa_bul(["dhi"]),
    "vhit": sayfa_bul(["vhit", "v hit"]),
    "vng": sayfa_bul(["vng"]),
}

print("Bulunan sayfa eşleşmeleri:")
for anahtar, sayfa in SAYFA_ADLARI.items():
    print(f"{anahtar} => {repr(sayfa)}")

# %% HÜCRE 06 — Sayfaları Güvenli Biçimde Oku ve Temizle


def tabloyu_guvenli_oku(excel_yolu, sayfa_adi):
    """
    Sayfayı header=None ile okur; ilk 20 satır içinde Hasta No başlığını bulur.
    Hasta Adı sütununu ancak başlık düzeltildikten sonra siler.
    """
    ham = pd.read_excel(excel_yolu, sheet_name=sayfa_adi, header=None, dtype=object)
    ham = ham.dropna(axis=0, how="all").dropna(axis=1, how="all").reset_index(drop=True)

    if ham.empty:
        raise ValueError(f"{sayfa_adi} sayfası boş.")

    baslik_satiri = None
    for i in range(min(20, len(ham))):
        satir = [metin_norm(x) for x in ham.iloc[i].tolist()]
        if any(("hasta" in hucre and "no" in hucre) for hucre in satir):
            baslik_satiri = i
            break

    if baslik_satiri is None:
        raise ValueError(
            f"{sayfa_adi} sayfasında ilk 20 satır içinde 'Hasta No' başlığı bulunamadı."
        )

    basliklar = benzersiz_sutun_adlari(ham.iloc[baslik_satiri].tolist())
    df = ham.iloc[baslik_satiri + 1:].copy()
    df.columns = basliklar
    df = df.dropna(axis=0, how="all").dropna(axis=1, how="all")
    df.reset_index(drop=True, inplace=True)

    hasta_no_adaylari = [
        sutun for sutun in df.columns
        if "hasta" in metin_norm(sutun) and "no" in metin_norm(sutun)
    ]

    if not hasta_no_adaylari:
        raise ValueError(
            f"{sayfa_adi} sayfasında temizleme sonrası Hasta No sütunu bulunamadı. "
            f"Sütunlar: {df.columns.tolist()}"
        )

    df.rename(columns={hasta_no_adaylari[0]: "Hasta No"}, inplace=True)
    df["Hasta No"] = df["Hasta No"].apply(hasta_no_sayisal)

    # Tekrarlanan başlıklar ve tablo dışı satırlar çıkarılır.
    df = df[df["Hasta No"].notna()].copy()

    aralik_disi = ~df["Hasta No"].between(MIN_HASTA_NO, MAX_HASTA_NO)
    if aralik_disi.any():
        uyari_ekle(
            f"{sayfa_adi}: {int(aralik_disi.sum())} satır, Hasta No "
            f"{MIN_HASTA_NO}-{MAX_HASTA_NO} aralığında olmadığı için çıkarıldı."
        )
        df = df[~aralik_disi].copy()

    # Kişisel ad/soyad alanları başlık düzeltildikten sonra silinir.
    kimlik_sutunlari = []
    for sutun in df.columns:
        ad = metin_norm(sutun)
        if sutun == "Hasta No":
            continue
        if (
            ("hasta" in ad and ("adi" in ad or "ad soyad" in ad))
            or ad in {"isim", "isim soyisim", "ad", "ad soyad", "hasta id"}
        ):
            kimlik_sutunlari.append(sutun)

    if kimlik_sutunlari:
        df.drop(columns=kimlik_sutunlari, inplace=True, errors="ignore")

    # Aynı Hasta No birden fazla satırdaysa satır çoğalmasını önlemek için
    # her sütundaki ilk dolu değer alınır.
    if df["Hasta No"].duplicated().any():
        tekrar_sayisi = int(df["Hasta No"].duplicated(keep=False).sum())
        uyari_ekle(
            f"{sayfa_adi}: {tekrar_sayisi} yinelenen Hasta No satırı birleştirildi."
        )
        df = (
            df.groupby("Hasta No", as_index=False, sort=True)
            .agg(ilk_dolu_deger)
        )

    df["Hasta No"] = df["Hasta No"].astype(int)
    df["Kisi_ID"] = df["Hasta No"].apply(kisi_id_uret)
    df["Grup"] = df["Hasta No"].apply(grup_uret)

    ilk_sutunlar = ["Kisi_ID", "Grup", "Hasta No"]
    diger_sutunlar = [c for c in df.columns if c not in ilk_sutunlar]
    return df[ilk_sutunlar + diger_sutunlar].sort_values("Hasta No").reset_index(drop=True)


odyometri_t = tabloyu_guvenli_oku(EXCEL_YOLU, SAYFA_ADLARI["odyometri"])
timpanometri_t = tabloyu_guvenli_oku(EXCEL_YOLU, SAYFA_ADLARI["timpanometri"])
dhi_t = tabloyu_guvenli_oku(EXCEL_YOLU, SAYFA_ADLARI["dhi"])
vhit_t = tabloyu_guvenli_oku(EXCEL_YOLU, SAYFA_ADLARI["vhit"])
vng_t = tabloyu_guvenli_oku(EXCEL_YOLU, SAYFA_ADLARI["vng"])

sayfalar = {
    "Odyometri": odyometri_t,
    "Timpanometri": timpanometri_t,
    "DHI": dhi_t,
    "vHIT": vhit_t,
    "VNG": vng_t,
}

for ad, df in sayfalar.items():
    print(f"\n--- {ad.upper()} ---")
    print("Boyut:", df.shape)
    print("Sütunlar:", df.columns.tolist())
    display(df.head(5))

# %% HÜCRE 07 — Doğrulanmış Manuel Düzeltmeleri Uygula


def manuel_duzeltmeleri_uygula(df, duzeltmeler, tablo_adi):
    df = df.copy()

    for kisi_id, alanlar in duzeltmeler.items():
        mask = df["Kisi_ID"] == kisi_id
        if not mask.any():
            uyari_ekle(f"{tablo_adi}: manuel düzeltme için {kisi_id} bulunamadı.")
            continue

        for sutun, yeni_deger in alanlar.items():
            if sutun not in df.columns:
                uyari_ekle(
                    f"{tablo_adi}: manuel düzeltme sütunu bulunamadı: {sutun}"
                )
                continue
            df.loc[mask, sutun] = yeni_deger

    return df


# Bu veri setindeki manuel düzeltmeler VNG parametreleriyle ilgiliyse burada uygulanır.
vng_t = manuel_duzeltmeleri_uygula(vng_t, MANUEL_DUZELTMELER, "VNG")
sayfalar["VNG"] = vng_t

# %% HÜCRE 08 — Ana Kişi Listesini Bütün Sayfalardan Oluştur

tum_hasta_nolari = pd.concat(
    [df["Hasta No"] for df in sayfalar.values()],
    ignore_index=True,
)

ana_kisi = pd.DataFrame({"Hasta No": tum_hasta_nolari})
ana_kisi = (
    ana_kisi.dropna()
    .drop_duplicates()
    .sort_values("Hasta No")
    .reset_index(drop=True)
)
ana_kisi["Hasta No"] = ana_kisi["Hasta No"].astype(int)
ana_kisi["Grup"] = ana_kisi["Hasta No"].apply(grup_uret)
ana_kisi["Kisi_ID"] = ana_kisi["Hasta No"].apply(kisi_id_uret)
ana_kisi = ana_kisi[["Kisi_ID", "Grup", "Hasta No"]]

print("Ana kişi listesi oluşturuldu.")
print("Toplam kişi sayısı:", len(ana_kisi))
display(ana_kisi)

if len(ana_kisi) != MAX_HASTA_NO:
    uyari_ekle(
        f"Ana kişi listesinde {len(ana_kisi)} kişi bulundu; beklenen sayı {MAX_HASTA_NO}."
    )

# %% HÜCRE 09 — Gerçek Veri Varlığı ve Eksik Veri Kontrolü


def veri_durumu_haritasi(df):
    kimlik = {"Kisi_ID", "Grup", "Hasta No"}
    bilgi_sutunlari = [c for c in df.columns if c not in kimlik]

    if not bilgi_sutunlari:
        return {hasta_no: "Yok" for hasta_no in df["Hasta No"]}

    durum = df.apply(
        lambda satir: "Var" if gercek_veri_var_mi(satir, bilgi_sutunlari) else "Yok",
        axis=1,
    )
    return dict(zip(df["Hasta No"], durum))


eksik_veri_kontrol = ana_kisi.copy()

for ad, df in sayfalar.items():
    durum_haritasi = veri_durumu_haritasi(df)
    eksik_veri_kontrol[ad] = eksik_veri_kontrol["Hasta No"].map(durum_haritasi).fillna("Yok")

print("Kişi bazında gerçek veri varlığı:")
display(eksik_veri_kontrol)

veri_sutunlari = list(sayfalar.keys())
eksik_ozet = (
    eksik_veri_kontrol.groupby("Grup")[veri_sutunlari]
    .agg(lambda seri: int((seri == "Var").sum()))
)

print("Gruba göre gerçek verisi bulunan kişi sayısı:")
display(eksik_ozet)

# %% HÜCRE 10 — Sayısal Analiz Sütunlarını Güvenli Biçimde Bul

# DHI toplam sütunu
DHI_TOPLAM_SUTUNU = en_uygun_sutun(
    dhi_t,
    [
        {"tumu": ("dhi", "toplam")},
        {"tumu": ("toplam",), "haric": ("hasta",)},
    ],
)

if DHI_TOPLAM_SUTUNU is None:
    uyari_ekle("DHI sayfasında toplam skor sütunu bulunamadı.")
else:
    print("DHI toplam sütunu:", DHI_TOPLAM_SUTUNU)

# vHIT gain sütunları
vhit_gain_sutunlari = sutunlari_bul(
    vhit_t,
    herhangi=("gain", "kazanc"),
    haric=("hasta",),
)

vhit_horizontal_sutunlari = [
    c for c in vhit_gain_sutunlari
    if any(
        anahtar in metin_norm(c).split()
        or anahtar in metin_norm(c)
        for anahtar in ("horizontal", "hor", "lateral")
    )
]

vhit_vertikal_sutunlari = [
    c for c in vhit_gain_sutunlari
    if any(
        anahtar in metin_norm(c).split()
        or anahtar in metin_norm(c)
        for anahtar in ("anterior", "posterior", "vertikal", "vertical")
    )
]

if not vhit_gain_sutunlari:
    uyari_ekle("vHIT sayfasında gain/kazanç sütunu bulunamadı.")
if not vhit_horizontal_sutunlari:
    uyari_ekle(
        "vHIT horizontal gain sütunları bulunamadı. Sütun adlarında "
        "horizontal, hor veya lateral ifadesi aranmıştır."
    )
if not vhit_vertikal_sutunlari:
    uyari_ekle(
        "vHIT vertikal gain sütunları bulunamadı. Sütun adlarında "
        "anterior, posterior veya vertikal ifadesi aranmıştır."
    )

print("vHIT bütün gain sütunları:", vhit_gain_sutunlari)
print("vHIT horizontal gain sütunları:", vhit_horizontal_sutunlari)
print("vHIT vertikal gain sütunları:", vhit_vertikal_sutunlari)

# VNG sütunları
sp_sutunlari = sutunlari_bul(
    vng_t,
    tumu=("kazanc",),
    herhangi=("sp", "smooth pursuit"),
    haric=("opk", "optokinetik", "asimetri", "zayiflik"),
)

opk_sutunlari = sutunlari_bul(
    vng_t,
    tumu=("kazanc",),
    herhangi=("opk", "optokinetik"),
    haric=("asimetri",),
)

sakkad_latans_sutunlari = sutunlari_bul(
    vng_t,
    tumu=("sakkad",),
    herhangi=("latans", "latency"),
)

sakkad_velocity_sutunlari = sutunlari_bul(
    vng_t,
    tumu=("sakkad",),
    herhangi=("velocity", "hiz"),
)

for ad, sutunlar in {
    "Smooth pursuit kazanç": sp_sutunlari,
    "OPK kazanç": opk_sutunlari,
    "Sakkad latans": sakkad_latans_sutunlari,
    "Sakkad velocity": sakkad_velocity_sutunlari,
}.items():
    print(f"{ad} sütunları:", sutunlar)
    if not sutunlar:
        uyari_ekle(f"VNG sayfasında {ad} sütunu bulunamadı.")

# Odyometri ve timpanometri kategorik sütunları
ODYOMETRI_BULGU_SUTUNU = en_uygun_sutun(
    odyometri_t,
    [
        {"herhangi": ("not", "bulgu", "yorum"), "haric": ("hasta",)},
    ],
)

TIMPANOMETRI_SONUC_SUTUNU = en_uygun_sutun(
    timpanometri_t,
    [
        {"tumu": ("timpanometri",), "haric": ("hasta",)},
        {"herhangi": ("tip", "sonuc", "bulgu"), "haric": ("hasta",)},
    ],
)

if ODYOMETRI_BULGU_SUTUNU is None:
    uyari_ekle("Odyometri sayfasında Not/Bulgu/Yorum sütunu bulunamadı.")
else:
    print("Odyometri bulgu sütunu:", ODYOMETRI_BULGU_SUTUNU)

if TIMPANOMETRI_SONUC_SUTUNU is None:
    uyari_ekle("Timpanometri sonuç/tip sütunu bulunamadı.")
else:
    print("Timpanometri sonuç sütunu:", TIMPANOMETRI_SONUC_SUTUNU)

# %% HÜCRE 11 — Kaynak Tablolarda Sayısallaştırma ve Özet Değerleri Hesapla


def sayisal_ortalama_tablosu(df, sutunlar, yeni_sutun):
    sonuc = df[["Hasta No"]].copy()

    if not sutunlar:
        sonuc[yeni_sutun] = np.nan
        return sonuc

    sayisal = df[sutunlar].apply(lambda seri: seri.map(sayisal_deger))
    sonuc[yeni_sutun] = sayisal.mean(axis=1, skipna=True)
    sonuc.loc[sayisal.notna().sum(axis=1) == 0, yeni_sutun] = np.nan
    return sonuc


# DHI
dhi_ozet = dhi_t[["Hasta No"]].copy()
if DHI_TOPLAM_SUTUNU:
    dhi_ozet["DHI_Toplam"] = dhi_t[DHI_TOPLAM_SUTUNU].map(sayisal_deger)
else:
    dhi_ozet["DHI_Toplam"] = np.nan

# vHIT
vhit_horizontal_ozet = sayisal_ortalama_tablosu(
    vhit_t,
    vhit_horizontal_sutunlari,
    "vHIT_Horizontal_Ortalama",
)
vhit_vertikal_ozet = sayisal_ortalama_tablosu(
    vhit_t,
    vhit_vertikal_sutunlari,
    "vHIT_Vertikal_Ortalama",
)
vhit_genel_ozet = sayisal_ortalama_tablosu(
    vhit_t,
    list(dict.fromkeys(vhit_horizontal_sutunlari + vhit_vertikal_sutunlari)),
    "vHIT_Genel_Ortalama",
)

# VNG
sp_ozet = sayisal_ortalama_tablosu(vng_t, sp_sutunlari, "SP_Ortalama")
opk_ozet = sayisal_ortalama_tablosu(vng_t, opk_sutunlari, "OPK_Ortalama")
sakkad_latans_ozet = sayisal_ortalama_tablosu(
    vng_t,
    sakkad_latans_sutunlari,
    "Sakkad_Latans_Ortalama",
)
sakkad_velocity_ozet = sayisal_ortalama_tablosu(
    vng_t,
    sakkad_velocity_sutunlari,
    "Sakkad_Velocity_Ortalama",
)

# Odyometri kategorik bulgu
odyometri_ozet = odyometri_t[["Hasta No"]].copy()
if ODYOMETRI_BULGU_SUTUNU:
    odyo_ham = odyometri_t[ODYOMETRI_BULGU_SUTUNU]
    odyo_sonuc = pd.Series(np.nan, index=odyometri_t.index, dtype=object)
    odyo_mevcut = odyo_ham.notna()
    odyo_normal = odyo_ham.astype(str).str.strip().str.lower().isin(
        ["", "-", "yok", "normal"]
    )
    odyo_sonuc.loc[odyo_mevcut & odyo_normal] = "Yok"
    odyo_sonuc.loc[odyo_mevcut & ~odyo_normal] = "Var"
    odyometri_ozet["Odyometri_Bulgusu"] = odyo_sonuc
else:
    odyometri_ozet["Odyometri_Bulgusu"] = np.nan

# Timpanometri kategorik bulgu
timpanometri_ozet = timpanometri_t[["Hasta No"]].copy()
if TIMPANOMETRI_SONUC_SUTUNU:
    timp_ham = timpanometri_t[TIMPANOMETRI_SONUC_SUTUNU]

    def timpanometri_sinifla(deger):
        if pd.isna(deger) or str(deger).strip() == "":
            return np.nan
        temiz = metin_norm(deger).replace(" ", "")
        return "Var" if temiz in {"ad", "as", "tipad", "tipas"} else "Yok"

    timpanometri_ozet["Timpanometri_Normal_Degil"] = timp_ham.map(timpanometri_sinifla)
else:
    timpanometri_ozet["Timpanometri_Normal_Degil"] = np.nan

# %% HÜCRE 12 — Ana Analiz ve Hipotez Tablolarını Hasta No ile Birleştir

# Ham/temiz kaynakların tamamını içeren geniş ana tablo
ana_analiz = ana_kisi.copy()
for on_ek, df in {
    "ODYO": odyometri_t,
    "TIMP": timpanometri_t,
    "DHI": dhi_t,
    "VHIT": vhit_t,
    "VNG": vng_t,
}.items():
    birlesecek = df.drop(columns=["Kisi_ID", "Grup"], errors="ignore")
    birlesecek = kaynak_on_ekle(birlesecek, on_ek)
    ana_analiz = ana_analiz.merge(
        birlesecek,
        on="Hasta No",
        how="left",
        validate="one_to_one",
    )

# İstatistikte kullanılacak sade tablo
hipotez_tablo = ana_kisi.copy()

for ozet in [
    dhi_ozet,
    vhit_horizontal_ozet,
    vhit_vertikal_ozet,
    vhit_genel_ozet,
    sp_ozet,
    opk_ozet,
    sakkad_latans_ozet,
    sakkad_velocity_ozet,
    odyometri_ozet,
    timpanometri_ozet,
]:
    hipotez_tablo = hipotez_tablo.merge(
        ozet,
        on="Hasta No",
        how="left",
        validate="one_to_one",
    )

# Eksik değerler Var/Yok olarak zorlanmaz.
hipotez_tablo["vHIT_Dusuk_Gain"] = uc_durumlu_sinifla(
    hipotez_tablo["vHIT_Horizontal_Ortalama"],
    hipotez_tablo["vHIT_Horizontal_Ortalama"] < VHIT_DUSUK_GAIN_ESIGI,
)

hipotez_tablo["OPK_Dusuk"] = uc_durumlu_sinifla(
    hipotez_tablo["OPK_Ortalama"],
    hipotez_tablo["OPK_Ortalama"] < OPK_DUSUK_ESIGI,
)

hipotez_tablo["Sakkad_Latans_Uzun"] = uc_durumlu_sinifla(
    hipotez_tablo["Sakkad_Latans_Ortalama"],
    hipotez_tablo["Sakkad_Latans_Ortalama"] > SAKKAD_UZUN_LATANS_ESIGI,
)

print("Ana analiz tablosu oluşturuldu:", ana_analiz.shape)
print("Hipotez tablosu oluşturuldu:", hipotez_tablo.shape)
display(hipotez_tablo)

# %% HÜCRE 13 — Eksik ve Kullanılabilir Analiz Verisi Özeti

analiz_degiskenleri = [
    "DHI_Toplam",
    "vHIT_Horizontal_Ortalama",
    "vHIT_Vertikal_Ortalama",
    "vHIT_Genel_Ortalama",
    "SP_Ortalama",
    "OPK_Ortalama",
    "Sakkad_Latans_Ortalama",
    "Sakkad_Velocity_Ortalama",
]

kullanilabilir_sonuclar = []
for degisken in analiz_degiskenleri:
    for grup in ["Hasta", "Kontrol"]:
        alt = hipotez_tablo[hipotez_tablo["Grup"] == grup][degisken]
        kullanilabilir_sonuclar.append({
            "Degisken": degisken,
            "Grup": grup,
            "Kullanilabilir_Kisi": int(alt.notna().sum()),
            "Eksik_Kisi": int(alt.isna().sum()),
        })

kullanilabilir_ozet = pd.DataFrame(kullanilabilir_sonuclar)

print("Her değişken için kullanılabilir veri sayısı:")
display(kullanilabilir_ozet)

eksik_satirlar = hipotez_tablo[
    hipotez_tablo[analiz_degiskenleri].isna().any(axis=1)
][["Kisi_ID", "Grup", "Hasta No"] + analiz_degiskenleri]

print("Eksik analiz değeri olan kişiler:")
display(eksik_satirlar)

# %% HÜCRE 14 — Tanımlayıcı İstatistikler

tanimsal_sonuclar = []

for degisken in analiz_degiskenleri:
    for grup in ["Hasta", "Kontrol"]:
        alt = hipotez_tablo.loc[
            hipotez_tablo["Grup"] == grup,
            degisken,
        ].dropna()

        tanimsal_sonuclar.append({
            "Degisken": degisken,
            "Grup": grup,
            "n": len(alt),
            "Ortalama": alt.mean() if len(alt) else np.nan,
            "Standart_Sapma": alt.std() if len(alt) > 1 else np.nan,
            "Medyan": alt.median() if len(alt) else np.nan,
            "Minimum": alt.min() if len(alt) else np.nan,
            "Maximum": alt.max() if len(alt) else np.nan,
        })


tanimsal_tablo = pd.DataFrame(tanimsal_sonuclar)
for sutun in ["Ortalama", "Standart_Sapma", "Medyan", "Minimum", "Maximum"]:
    tanimsal_tablo[sutun] = tanimsal_tablo[sutun].round(4)

print("Gruplara göre tanımlayıcı istatistikler:")
display(tanimsal_tablo)

# %% HÜCRE 15 — Mann–Whitney U Testleri

istatistik_sonuclar = []

for degisken in analiz_degiskenleri:
    hasta = hipotez_tablo.loc[
        hipotez_tablo["Grup"] == "Hasta",
        degisken,
    ].dropna()

    kontrol = hipotez_tablo.loc[
        hipotez_tablo["Grup"] == "Kontrol",
        degisken,
    ].dropna()

    if len(hasta) > 0 and len(kontrol) > 0:
        test = mannwhitneyu(hasta, kontrol, alternative="two-sided")
        u_degeri = float(test.statistic)
        p_degeri = float(test.pvalue)
        yorum = (
            "Anlamlı fark var"
            if p_degeri < ANLAMLILIK_DUZEYI
            else "Anlamlı fark yok"
        )
    else:
        u_degeri = np.nan
        p_degeri = np.nan
        yorum = "Test hesaplanamadı"

    istatistik_sonuclar.append({
        "Degisken": degisken,
        "Hasta_n": len(hasta),
        "Kontrol_n": len(kontrol),
        "Hasta_Medyan": hasta.median() if len(hasta) else np.nan,
        "Kontrol_Medyan": kontrol.median() if len(kontrol) else np.nan,
        "U_Degeri": u_degeri,
        "p_Degeri": p_degeri,
        "Yorum": yorum,
    })

istatistik_tablo = pd.DataFrame(istatistik_sonuclar)
for sutun in ["Hasta_Medyan", "Kontrol_Medyan", "U_Degeri", "p_Degeri"]:
    istatistik_tablo[sutun] = istatistik_tablo[sutun].round(4)

print("Hasta ve kontrol grupları için Mann–Whitney U testi:")
display(istatistik_tablo)

# %% HÜCRE 16 — Dinamik Hipotez Özeti


def mw_bilgi(degisken):
    bulunan = istatistik_tablo[istatistik_tablo["Degisken"] == degisken]
    return bulunan.iloc[0] if len(bulunan) else None


def hipotez_degerlendir(degisken, beklenen_yon=None):
    satir = mw_bilgi(degisken)
    if satir is None or pd.isna(satir["p_Degeri"]):
        return "Hesaplanamadı", "Yeterli veya uygun veri bulunamadı."

    anlamli = satir["p_Degeri"] < ANLAMLILIK_DUZEYI
    hasta_medyan = satir["Hasta_Medyan"]
    kontrol_medyan = satir["Kontrol_Medyan"]

    if not anlamli:
        return "Desteklenmedi", "Gruplar arasında istatistiksel olarak anlamlı fark saptanmadı."

    if beklenen_yon == "yuksek" and hasta_medyan > kontrol_medyan:
        return "Desteklendi", "Anlamlı fark beklenen yüksek değer yönündedir."
    if beklenen_yon == "dusuk" and hasta_medyan < kontrol_medyan:
        return "Desteklendi", "Anlamlı fark beklenen düşük değer yönündedir."
    if beklenen_yon is None:
        return "Desteklendi", "Gruplar arasında istatistiksel olarak anlamlı fark saptandı."

    return "Desteklenmedi", "Anlamlı fark bulundu; ancak beklenen yönün tersindedir."


hipotez_tanimlari = [
    ("H1g - DHI toplam skoru hasta grubunda daha yüksektir", "DHI_Toplam", "yuksek"),
    ("H1a - vHIT horizontal VOR gain hasta grubunda daha düşüktür", "vHIT_Horizontal_Ortalama", "dusuk"),
    ("H1b - OPK kazançları hasta grubunda daha düşüktür", "OPK_Ortalama", "dusuk"),
    ("H1c - Sakkad latans süreleri hasta grubunda daha uzundur", "Sakkad_Latans_Ortalama", "yuksek"),
    ("Sakkad velocity hasta ve kontrol grubu arasında farklıdır", "Sakkad_Velocity_Ortalama", None),
    ("Smooth pursuit kazançları hasta ve kontrol grubu arasında farklıdır", "SP_Ortalama", None),
]

hipotez_sonuclari = []
for hipotez, degisken, beklenen_yon in hipotez_tanimlari:
    sonuc, yorum = hipotez_degerlendir(degisken, beklenen_yon)
    satir = mw_bilgi(degisken)

    hipotez_sonuclari.append({
        "Hipotez": hipotez,
        "Degisken": degisken,
        "Sonuc": sonuc,
        "Yorum": yorum,
        "Hasta_n": satir["Hasta_n"] if satir is not None else np.nan,
        "Kontrol_n": satir["Kontrol_n"] if satir is not None else np.nan,
        "Hasta_Medyan": satir["Hasta_Medyan"] if satir is not None else np.nan,
        "Kontrol_Medyan": satir["Kontrol_Medyan"] if satir is not None else np.nan,
        "p_Degeri": satir["p_Degeri"] if satir is not None else np.nan,
    })

hipotez_ozet_detayli = pd.DataFrame(hipotez_sonuclari)
print("Hipotez bazlı sonuç özeti:")
display(hipotez_ozet_detayli)

# %% HÜCRE 17 — Kategorik Bulgular İçin Fisher Exact Testi

kategorik_degiskenler = [
    "vHIT_Dusuk_Gain",
    "OPK_Dusuk",
    "Sakkad_Latans_Uzun",
    "Odyometri_Bulgusu",
    "Timpanometri_Normal_Degil",
]

kategorik_sonuclar = []

for degisken in kategorik_degiskenler:
    veri = hipotez_tablo[["Grup", degisken]].dropna().copy()
    veri = veri[veri[degisken].isin(["Var", "Yok"])]

    capraz = pd.crosstab(veri["Grup"], veri[degisken]).reindex(
        index=["Hasta", "Kontrol"],
        columns=["Var", "Yok"],
        fill_value=0,
    )

    if int(capraz.values.sum()) == 0:
        oran = np.nan
        p_degeri = np.nan
        yorum = "Test hesaplanamadı"
    else:
        try:
            oran, p_degeri = fisher_exact(capraz.values, alternative="two-sided")
            yorum = (
                "Anlamlı ilişki var"
                if p_degeri < ANLAMLILIK_DUZEYI
                else "Anlamlı ilişki yok"
            )
        except ValueError:
            oran = np.nan
            p_degeri = np.nan
            yorum = "Test hesaplanamadı"

    kategorik_sonuclar.append({
        "Degisken": degisken,
        "Hasta_Var": int(capraz.loc["Hasta", "Var"]),
        "Hasta_Yok": int(capraz.loc["Hasta", "Yok"]),
        "Kontrol_Var": int(capraz.loc["Kontrol", "Var"]),
        "Kontrol_Yok": int(capraz.loc["Kontrol", "Yok"]),
        "Odds_Ratio": oran,
        "p_Degeri": p_degeri,
        "Yorum": yorum,
    })

kategorik_tablo = pd.DataFrame(kategorik_sonuclar)
for sutun in ["Odds_Ratio", "p_Degeri"]:
    kategorik_tablo[sutun] = kategorik_tablo[sutun].round(4)

print("Kategorik bulgular için Fisher Exact testi:")
display(kategorik_tablo)

# %% HÜCRE 18 — VM Grubunda DHI ile Spearman Korelasyonu

vm_veri = hipotez_tablo[hipotez_tablo["Grup"] == "Hasta"].copy()

korelasyon_degiskenleri = [
    "vHIT_Horizontal_Ortalama",
    "vHIT_Vertikal_Ortalama",
    "vHIT_Genel_Ortalama",
    "SP_Ortalama",
    "OPK_Ortalama",
    "Sakkad_Latans_Ortalama",
    "Sakkad_Velocity_Ortalama",
]

korelasyon_sonuclari = []

for degisken in korelasyon_degiskenleri:
    alt = vm_veri[["DHI_Toplam", degisken]].dropna()

    if (
        len(alt) >= 3
        and alt["DHI_Toplam"].nunique() > 1
        and alt[degisken].nunique() > 1
    ):
        rho, p_degeri = spearmanr(alt["DHI_Toplam"], alt[degisken])
        yorum = (
            "Anlamlı korelasyon var"
            if p_degeri < ANLAMLILIK_DUZEYI
            else "Anlamlı korelasyon yok"
        )
        yon = "Negatif" if rho < 0 else "Pozitif" if rho > 0 else "Yok"
    else:
        rho = np.nan
        p_degeri = np.nan
        yorum = "Yetersiz veya sabit veri"
        yon = "Hesaplanamadı"

    korelasyon_sonuclari.append({
        "Degisken": degisken,
        "n": len(alt),
        "Spearman_rho": rho,
        "p_Degeri": p_degeri,
        "Yon": yon,
        "Yorum": yorum,
    })

korelasyon_tablo = pd.DataFrame(korelasyon_sonuclari)
for sutun in ["Spearman_rho", "p_Degeri"]:
    korelasyon_tablo[sutun] = korelasyon_tablo[sutun].round(4)

print("VM grubunda DHI ile test parametreleri arasındaki Spearman korelasyonu:")
display(korelasyon_tablo)

# %% HÜCRE 19 — Dinamik Bulgular Metni


def kat_bilgi(degisken):
    bulunan = kategorik_tablo[kategorik_tablo["Degisken"] == degisken]
    return bulunan.iloc[0] if len(bulunan) else None


def kor_bilgi(degisken):
    bulunan = korelasyon_tablo[korelasyon_tablo["Degisken"] == degisken]
    return bulunan.iloc[0] if len(bulunan) else None


def mw_cumlesi(etiket, degisken, basamak=2):
    satir = mw_bilgi(degisken)
    if satir is None or pd.isna(satir["p_Degeri"]):
        return f"{etiket} için karşılaştırma, yeterli veri bulunamadığı için hesaplanamamıştır."

    anlam = (
        "istatistiksel olarak anlamlı fark saptanmıştır"
        if satir["p_Degeri"] < ANLAMLILIK_DUZEYI
        else "istatistiksel olarak anlamlı fark saptanmamıştır"
    )

    return (
        f"{etiket} açısından hasta ve kontrol grupları arasında {anlam}. "
        f"Hasta grubu medyanı {deger_yaz(satir['Hasta_Medyan'], basamak)}, "
        f"kontrol grubu medyanı {deger_yaz(satir['Kontrol_Medyan'], basamak)} "
        f"olarak hesaplanmıştır ({p_yaz(satir['p_Degeri'])})."
    )


hasta_sayisi = int((ana_kisi["Grup"] == "Hasta").sum())
kontrol_sayisi = int((ana_kisi["Grup"] == "Kontrol").sum())

bulgu_paragraflari = [
    (
        f"Çalışmaya {hasta_sayisi} vestibüler migren tanılı hasta ve "
        f"{kontrol_sayisi} sağlıklı kontrol olmak üzere toplam "
        f"{len(ana_kisi)} birey dahil edilmiştir."
    ),
    mw_cumlesi("DHI toplam skoru", "DHI_Toplam", 2),
    mw_cumlesi("Horizontal vHIT ortalaması", "vHIT_Horizontal_Ortalama", 4),
    mw_cumlesi("Vertikal vHIT ortalaması", "vHIT_Vertikal_Ortalama", 4),
    mw_cumlesi("Genel vHIT ortalaması", "vHIT_Genel_Ortalama", 4),
    mw_cumlesi("Smooth pursuit ortalaması", "SP_Ortalama", 2),
    mw_cumlesi("OPK ortalaması", "OPK_Ortalama", 2),
    mw_cumlesi("Sakkad latans ortalaması", "Sakkad_Latans_Ortalama", 2),
    mw_cumlesi("Sakkad velocity ortalaması", "Sakkad_Velocity_Ortalama", 2),
]

for etiket, degisken in [
    ("Düşük vHIT gain", "vHIT_Dusuk_Gain"),
    ("OPK düşüklüğü", "OPK_Dusuk"),
    ("Uzun sakkad latansı", "Sakkad_Latans_Uzun"),
    ("Odyometrik bulgu", "Odyometri_Bulgusu"),
    ("Normal dışı timpanometri", "Timpanometri_Normal_Degil"),
]:
    satir = kat_bilgi(degisken)
    if satir is None or pd.isna(satir["p_Degeri"]):
        bulgu_paragraflari.append(
            f"{etiket} için kategorik karşılaştırma hesaplanamamıştır."
        )
    else:
        bulgu_paragraflari.append(
            f"{etiket}; hasta grubunda {int(satir['Hasta_Var'])}/"
            f"{int(satir['Hasta_Var'] + satir['Hasta_Yok'])}, kontrol grubunda "
            f"{int(satir['Kontrol_Var'])}/"
            f"{int(satir['Kontrol_Var'] + satir['Kontrol_Yok'])} kişide görülmüştür "
            f"({p_yaz(satir['p_Degeri'])})."
        )

anlamli_korelasyonlar = korelasyon_tablo[
    korelasyon_tablo["p_Degeri"].notna()
    & (korelasyon_tablo["p_Degeri"] < ANLAMLILIK_DUZEYI)
]

if anlamli_korelasyonlar.empty:
    bulgu_paragraflari.append(
        "Vestibüler migren grubunda DHI toplam skoru ile incelenen objektif "
        "test parametreleri arasında istatistiksel olarak anlamlı korelasyon saptanmamıştır."
    )
else:
    adlar = ", ".join(anlamli_korelasyonlar["Degisken"].tolist())
    bulgu_paragraflari.append(
        f"DHI toplam skoru ile anlamlı korelasyon gösteren değişkenler: {adlar}."
    )

bulgular_metni = "\n\n".join(bulgu_paragraflari)
print("Makale bulgular metni:")
print(bulgular_metni)

makale_bulgular_tablo = pd.DataFrame([
    {"Bolum": "Makale Bulgular Metni", "Metin": bulgular_metni}
])

# %% HÜCRE 20 — Tartışma ve Sonuç Taslağı

anlamli_sayisal = istatistik_tablo[
    istatistik_tablo["p_Degeri"].notna()
    & (istatistik_tablo["p_Degeri"] < ANLAMLILIK_DUZEYI)
]["Degisken"].tolist()

anlamli_kategorik = kategorik_tablo[
    kategorik_tablo["p_Degeri"].notna()
    & (kategorik_tablo["p_Degeri"] < ANLAMLILIK_DUZEYI)
]["Degisken"].tolist()

sayisal_ifade = ", ".join(anlamli_sayisal) if anlamli_sayisal else "hiçbir sayısal parametre"
kategorik_ifade = ", ".join(anlamli_kategorik) if anlamli_kategorik else "hiçbir kategorik bulgu"

tartisma_metni = f"""
Bu pilot çalışmada vestibüler migren tanılı bireyler ile sağlıklı kontrol grubu odyolojik, vestibüler, okülomotor ve öznel baş dönmesi etkilenimi açısından karşılaştırılmıştır. Güncel veri üzerinden yapılan analizlerde istatistiksel olarak anlamlı farklılık gösteren sayısal değişkenler {sayisal_ifade} olarak belirlenmiştir. Kategorik analizlerde anlamlı ilişki gösteren değişkenler ise {kategorik_ifade} olarak bulunmuştur.

Bulgular yorumlanırken örneklem büyüklüğünün sınırlı olduğu dikkate alınmalıdır. Çalışma {hasta_sayisi} hasta ve {kontrol_sayisi} kontrol bireyiyle yürütülen pilot nitelikte bir analizdir. Bu nedenle p değerlerinin yanında grup medyanları, kullanılabilir kişi sayıları, veri dağılımları ve klinik yön de birlikte değerlendirilmelidir.

DHI gibi öznel değerlendirme araçları ile vHIT ve VNG gibi objektif testler farklı klinik boyutları ölçmektedir. Bu nedenle öznel etkilenim ile objektif vestibüler veya okülomotor parametrelerin her zaman paralel sonuç vermemesi mümkündür. Korelasyon sonuçları bu çerçevede ve test anındaki klinik durum göz önünde bulundurularak yorumlanmalıdır.

Analiz kodunda tüm tablolar Hasta No üzerinden eşleştirilmiş, eksik değerler normal sonuç olarak sınıflandırılmamış ve Excel'de bulunmayan veriler için yapay değer kullanılmamıştır. Böylece rapor yalnızca kaynak dosyada bulunan ve sayısala dönüştürülebilen verilere dayanmaktadır.
""".strip()

sonuc_metni = f"""
Bu pilot analizde hasta ve kontrol grupları arasında anlamlı farklılık gösteren sayısal değişkenler {sayisal_ifade}; anlamlı kategorik değişkenler ise {kategorik_ifade} olarak belirlenmiştir. Bulguların klinik anlamı, küçük örneklem büyüklüğü ve eksik veri durumu dikkate alınarak yorumlanmalıdır. Sonuçların daha geniş örneklemli ve prospektif çalışmalarla doğrulanması önerilmektedir.
""".strip()

makale_tartisma_tablo = pd.DataFrame([
    {"Bolum": "Tartışma", "Metin": tartisma_metni},
    {"Bolum": "Sonuç", "Metin": sonuc_metni},
])

print("Tartışma metni:")
print(tartisma_metni)
print("\nSonuç metni:")
print(sonuc_metni)

# %% HÜCRE 21 — Uyarı ve Sütun Eşleştirme Raporu

sutun_esleme_tablo = pd.DataFrame([
    {"Analiz": "DHI toplam", "Bulunan_Sutunlar": str([DHI_TOPLAM_SUTUNU] if DHI_TOPLAM_SUTUNU else [])},
    {"Analiz": "vHIT horizontal", "Bulunan_Sutunlar": str(vhit_horizontal_sutunlari)},
    {"Analiz": "vHIT vertikal", "Bulunan_Sutunlar": str(vhit_vertikal_sutunlari)},
    {"Analiz": "Smooth pursuit", "Bulunan_Sutunlar": str(sp_sutunlari)},
    {"Analiz": "OPK", "Bulunan_Sutunlar": str(opk_sutunlari)},
    {"Analiz": "Sakkad latans", "Bulunan_Sutunlar": str(sakkad_latans_sutunlari)},
    {"Analiz": "Sakkad velocity", "Bulunan_Sutunlar": str(sakkad_velocity_sutunlari)},
    {"Analiz": "Odyometri bulgusu", "Bulunan_Sutunlar": str([ODYOMETRI_BULGU_SUTUNU] if ODYOMETRI_BULGU_SUTUNU else [])},
    {"Analiz": "Timpanometri sonucu", "Bulunan_Sutunlar": str([TIMPANOMETRI_SONUC_SUTUNU] if TIMPANOMETRI_SONUC_SUTUNU else [])},
])

uyarilar_tablo = pd.DataFrame({"Uyari": UYARILAR if UYARILAR else ["Uyarı bulunmuyor."]})

print("Sütun eşleştirme raporu:")
display(sutun_esleme_tablo)
print("Uyarılar:")
display(uyarilar_tablo)

# %% HÜCRE 22 — Bütün Analiz Sonuçlarını Excel'e Kaydet

with pd.ExcelWriter(CIKTI_EXCEL_YOLU, engine="openpyxl") as writer:
    ana_kisi.to_excel(writer, sheet_name="Ana_Kisi", index=False)
    eksik_veri_kontrol.to_excel(writer, sheet_name="Veri_Varligi", index=False)
    kullanilabilir_ozet.to_excel(writer, sheet_name="Kullanilabilir_Veri", index=False)
    hipotez_tablo.to_excel(writer, sheet_name="Hipotez_Tablosu", index=False)
    tanimsal_tablo.to_excel(writer, sheet_name="Tanimsal", index=False)
    istatistik_tablo.to_excel(writer, sheet_name="Mann_Whitney", index=False)
    hipotez_ozet_detayli.to_excel(writer, sheet_name="Hipotez_Ozeti", index=False)
    kategorik_tablo.to_excel(writer, sheet_name="Fisher", index=False)
    korelasyon_tablo.to_excel(writer, sheet_name="Spearman", index=False)
    sutun_esleme_tablo.to_excel(writer, sheet_name="Sutun_Esleme", index=False)
    uyarilar_tablo.to_excel(writer, sheet_name="Uyarilar", index=False)
    odyometri_t.to_excel(writer, sheet_name="Temiz_Odyometri", index=False)
    timpanometri_t.to_excel(writer, sheet_name="Temiz_Timpanometri", index=False)
    dhi_t.to_excel(writer, sheet_name="Temiz_DHI", index=False)
    vhit_t.to_excel(writer, sheet_name="Temiz_vHIT", index=False)
    vng_t.to_excel(writer, sheet_name="Temiz_VNG", index=False)

print("Excel analiz çıktısı kaydedildi:", CIKTI_EXCEL_YOLU)

# %% HÜCRE 23 — Word Raporunu Oluştur

from docx import Document
from docx.enum.table import WD_TABLE_ALIGNMENT
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Inches, Pt, RGBColor


def set_cell_background(cell, fill_hex):
    tc_pr = cell._element.get_or_add_tcPr()
    shd = OxmlElement("w:shd")
    shd.set(qn("w:val"), "clear")
    shd.set(qn("w:color"), "auto")
    shd.set(qn("w:fill"), fill_hex)
    tc_pr.append(shd)


def ekle_baslik(doc, metin, seviye=1):
    baslik = doc.add_heading(metin, level=seviye)
    baslik.alignment = WD_ALIGN_PARAGRAPH.LEFT
    for run in baslik.runs:
        run.font.name = "Arial"
        run.font.bold = True
        run.font.color.rgb = RGBColor(0, 0, 0)
        run.font.size = Pt(16 if seviye == 0 else 13 if seviye == 1 else 11)


def ekle_paragraf(doc, metin, italik=False):
    for parca in str(metin).split("\n\n"):
        parca = parca.strip()
        if not parca:
            continue
        paragraf = doc.add_paragraph()
        run = paragraf.add_run(parca)
        run.font.name = "Arial"
        run.font.size = Pt(10)
        run.font.italic = italik
        paragraf.paragraph_format.space_after = Pt(6)


def ekle_sik_tablo(doc, df, baslik_metni):
    ekle_baslik(doc, baslik_metni, 2)
    tablo = doc.add_table(rows=1, cols=len(df.columns))
    tablo.alignment = WD_TABLE_ALIGNMENT.CENTER
    tablo.style = "Table Grid"

    baslik_hucreleri = tablo.rows[0].cells
    for i, sutun in enumerate(df.columns):
        baslik_hucreleri[i].text = str(sutun)
        set_cell_background(baslik_hucreleri[i], "262626")
        for paragraf in baslik_hucreleri[i].paragraphs:
            paragraf.alignment = WD_ALIGN_PARAGRAPH.CENTER
            for run in paragraf.runs:
                run.font.name = "Arial"
                run.font.size = Pt(8)
                run.font.bold = True
                run.font.color.rgb = RGBColor(255, 255, 255)

    for sira, (_, satir) in enumerate(df.iterrows()):
        hucreler = tablo.add_row().cells
        arka_plan = "F7F7F7" if sira % 2 else "FFFFFF"

        for i, deger in enumerate(satir):
            hucreler[i].text = "" if pd.isna(deger) else str(deger)
            set_cell_background(hucreler[i], arka_plan)
            for paragraf in hucreler[i].paragraphs:
                for run in paragraf.runs:
                    run.font.name = "Arial"
                    run.font.size = Pt(8)
                    run.font.color.rgb = RGBColor(0, 0, 0)
                    if str(deger) in {"Anlamlı fark var", "Anlamlı ilişki var", "Desteklendi"}:
                        run.font.bold = True

    doc.add_paragraph("")


def onemli_bulgu_satiri(degisken, etiket):
    satir = mw_bilgi(degisken)
    if satir is None or pd.isna(satir["p_Degeri"]):
        return f"• {etiket}: hesaplanamadı."

    durum = "anlamlı" if satir["p_Degeri"] < ANLAMLILIK_DUZEYI else "anlamlı değil"
    return (
        f"• {etiket}: hasta medyanı {deger_yaz(satir['Hasta_Medyan'])}, "
        f"kontrol medyanı {deger_yaz(satir['Kontrol_Medyan'])}; "
        f"fark {durum} ({p_yaz(satir['p_Degeri'])})."
    )


doc = Document()
for bolum in doc.sections:
    bolum.top_margin = Inches(0.8)
    bolum.bottom_margin = Inches(0.8)
    bolum.left_margin = Inches(0.8)
    bolum.right_margin = Inches(0.8)

# Ana başlık
ekle_baslik(doc, "İSTATİSTİKSEL ANALİZ VE AKADEMİK RAPOR", 0)
ekle_paragraf(
    doc,
    "Vestibüler Migren ve Sağlıklı Kontrol Grubu Karşılaştırması",
    italik=True,
)
doc.add_paragraph("─" * 60)

# Yönetici özeti
ekle_baslik(doc, "YÖNETİCİ ÖZETİ VE TEMEL BULGULAR", 1)
ekle_paragraf(
    doc,
    f"• Toplam örneklem: {len(ana_kisi)} kişi "
    f"({hasta_sayisi} vestibüler migren hastası, {kontrol_sayisi} sağlıklı kontrol).",
)
for degisken, etiket in [
    ("DHI_Toplam", "DHI toplam skoru"),
    ("OPK_Ortalama", "OPK ortalaması"),
    ("Sakkad_Velocity_Ortalama", "Sakkad velocity ortalaması"),
]:
    ekle_paragraf(doc, onemli_bulgu_satiri(degisken, etiket))

if UYARILAR:
    ekle_baslik(doc, "VERİ VE SÜTUN UYARILARI", 2)
    for mesaj in UYARILAR:
        ekle_paragraf(doc, f"• {mesaj}")

# İstatistik tabloları
doc.add_page_break()
ekle_baslik(doc, "BÖLÜM 1 — İSTATİSTİKSEL ANALİZ", 1)
ekle_sik_tablo(doc, tanimsal_tablo, "1. Tanımlayıcı İstatistikler")
ekle_sik_tablo(doc, istatistik_tablo, "2. Mann–Whitney U Testi Sonuçları")
ekle_sik_tablo(doc, kategorik_tablo, "3. Fisher Exact Testi Sonuçları")
ekle_sik_tablo(doc, korelasyon_tablo, "4. Spearman Korelasyon Analizi")
ekle_sik_tablo(doc, hipotez_ozet_detayli, "5. Hipotez Özeti")

# Akademik metin
doc.add_page_break()
ekle_baslik(doc, "BÖLÜM 2 — AKADEMİK METİN TASLAĞI", 1)
ekle_baslik(doc, "Bulgular", 2)
ekle_paragraf(doc, bulgular_metni)
ekle_baslik(doc, "Tartışma", 2)
ekle_paragraf(doc, tartisma_metni)
ekle_baslik(doc, "Sonuç", 2)
ekle_paragraf(doc, sonuc_metni)

# Teknik kontrol
doc.add_page_break()
ekle_baslik(doc, "BÖLÜM 3 — TEKNİK VERİ KONTROLÜ", 1)
ekle_sik_tablo(doc, kullanilabilir_ozet, "1. Kullanılabilir Veri Sayıları")
ekle_sik_tablo(doc, sutun_esleme_tablo, "2. Kullanılan Kaynak Sütunlar")
ekle_sik_tablo(doc, uyarilar_tablo, "3. Uyarılar")

doc.save(CIKTI_WORD_YOLU)
print("Word raporu kaydedildi:", CIKTI_WORD_YOLU)

# %% HÜCRE 24 — Son Kontrol

print("\n" + "=" * 70)
print("ANALİZ TAMAMLANDI")
print("=" * 70)
print("Temiz Excel çıktısı:", CIKTI_EXCEL_YOLU)
print("Word raporu:", CIKTI_WORD_YOLU)
print("Toplam uyarı sayısı:", len(UYARILAR))

if UYARILAR:
    print("\nKontrol edilmesi gereken noktalar:")
    for sira, mesaj in enumerate(UYARILAR, start=1):
        print(f"{sira}. {mesaj}")
else:
    print("Sütun ve veri eşleştirmelerinde uyarı bulunmadı.")