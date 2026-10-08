# -*- coding: utf-8 -*-
"""
Klinik Tez Biyostatistik Analizi
================================

GitHub / portföy için anonimleştirilmiş örnek analiz akışı.

Bu dosya gerçek katılımcı adları, kişisel bilgiler, özel Google Drive yolları,
Colab bağlantıları veya kurumsal kimlik bilgileri içermez.

Beklenen çalışma kitabı:
    clinical_data.xlsx

Beklenen sayfalar:
    01_DHI_Anket
    02_Timpanogram
    03_Odyogram
    04_VHIT
    05_VNG

Temel anonim kimlik alanı:
    kayit_id

Not:
Bu sürüm, gerçek klinik veri setini paylaşmadan veri hazırlama ve
biyostatistiksel analiz yaklaşımını göstermek amacıyla hazırlanmıştır.
"""

from pathlib import Path
import warnings

import numpy as np
import pandas as pd
from scipy import stats

warnings.filterwarnings("ignore", category=RuntimeWarning)

# ---------------------------------------------------------------------
# 01 — AYARLAR
# ---------------------------------------------------------------------

DATA_PATH = Path("Klinik_Tez_Sentetik_Veriler.xlsx")
OUTPUT_DIR = Path("outputs")
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

GROUP_COL = "grup"
ID_COL = "kayit_id"

EXPECTED_GROUPS = ["Hasta", "Kontrol"]


# ---------------------------------------------------------------------
# 02 — YARDIMCI FONKSİYONLAR
# ---------------------------------------------------------------------

def sayisala_cevir(seri: pd.Series) -> pd.Series:
    """Virgül/nokta farklarını temizleyerek seriyi sayısal formata çevirir."""
    return (
        seri.astype(str)
        .str.replace(",", ".", regex=False)
        .str.strip()
        .replace(["", "nan", "NaN", "None"], np.nan)
        .pipe(pd.to_numeric, errors="coerce")
    )


def p_degeri_formatla(p: float) -> str:
    """p değerini raporlamaya uygun biçime getirir."""
    if pd.isna(p):
        return ""
    if p < 0.001:
        return "<0,001"
    return f"{p:.4f}".replace(".", ",")


def require_columns(df: pd.DataFrame, columns: list[str], table_name: str) -> None:
    """Zorunlu sütunların veri setinde bulunup bulunmadığını kontrol eder."""
    missing = [col for col in columns if col not in df.columns]
    if missing:
        raise ValueError(
            f"{table_name} tablosunda eksik sütun(lar): {', '.join(missing)}"
        )


def clean_id(series: pd.Series) -> pd.Series:
    """Anonim kayıt kodlarını standartlaştırır."""
    return series.astype(str).str.strip().str.upper()


def group_descriptives(
    df: pd.DataFrame,
    variables: list[str],
    group_col: str = GROUP_COL,
) -> pd.DataFrame:
    """Sayısal değişkenler için grup bazında tanımlayıcı istatistik üretir."""
    rows = []

    for variable in variables:
        if variable not in df.columns:
            continue

        for group_name, group_df in df.groupby(group_col):
            values = group_df[variable].dropna()

            rows.append(
                {
                    "Değişken": variable,
                    "Grup": group_name,
                    "n": len(values),
                    "Ortalama": values.mean(),
                    "SS": values.std(),
                    "Medyan": values.median(),
                    "Minimum": values.min(),
                    "Maksimum": values.max(),
                }
            )

    result = pd.DataFrame(rows)

    for col in ["Ortalama", "SS", "Medyan", "Minimum", "Maksimum"]:
        if col in result.columns:
            result[col] = result[col].round(3)

    return result


def compare_two_groups(
    df: pd.DataFrame,
    variables: list[str],
    group_col: str = GROUP_COL,
    group_a: str = "Hasta",
    group_b: str = "Kontrol",
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """
    İki bağımsız grubu karşılaştırır.

    - Her iki grup normal dağılıyorsa:
        Bağımsız örneklem t testi / Welch t testi
    - Aksi durumda:
        Mann–Whitney U testi
    """
    normality_rows = []
    comparison_rows = []

    for variable in variables:
        if variable not in df.columns:
            continue

        a = df.loc[df[group_col] == group_a, variable].dropna()
        b = df.loc[df[group_col] == group_b, variable].dropna()

        if len(a) < 3 or len(b) < 3:
            continue

        a_stat, a_p = stats.shapiro(a)
        b_stat, b_p = stats.shapiro(b)

        normality_rows.extend(
            [
                {
                    "Değişken": variable,
                    "Grup": group_a,
                    "n": len(a),
                    "Shapiro-Wilk W": round(a_stat, 3),
                    "p": a_p,
                    "Dağılım": "Normal" if a_p >= 0.05 else "Normal değil",
                },
                {
                    "Değişken": variable,
                    "Grup": group_b,
                    "n": len(b),
                    "Shapiro-Wilk W": round(b_stat, 3),
                    "p": b_p,
                    "Dağılım": "Normal" if b_p >= 0.05 else "Normal değil",
                },
            ]
        )

        if a_p >= 0.05 and b_p >= 0.05:
            _, levene_p = stats.levene(a, b)

            if levene_p >= 0.05:
                test_name = "Bağımsız örneklem t testi"
                stat, p = stats.ttest_ind(a, b, equal_var=True)
            else:
                test_name = "Welch t testi"
                stat, p = stats.ttest_ind(a, b, equal_var=False)
        else:
            levene_p = np.nan
            test_name = "Mann-Whitney U testi"
            stat, p = stats.mannwhitneyu(a, b, alternative="two-sided")

        comparison_rows.append(
            {
                "Değişken": variable,
                f"{group_a} n": len(a),
                f"{group_a} ort±SS": f"{a.mean():.2f}±{a.std():.2f}",
                f"{group_a} medyan": round(a.median(), 2),
                f"{group_b} n": len(b),
                f"{group_b} ort±SS": f"{b.mean():.2f}±{b.std():.2f}",
                f"{group_b} medyan": round(b.median(), 2),
                "Test": test_name,
                "Test istatistiği": round(stat, 3),
                "Levene p": levene_p,
                "p": p,
                "p değeri": p_degeri_formatla(p),
                "Anlamlılık": "Anlamlı" if p < 0.05 else "Anlamlı değil",
            }
        )

    return pd.DataFrame(normality_rows), pd.DataFrame(comparison_rows)


def categorical_test(
    df: pd.DataFrame,
    variable: str,
    group_col: str = GROUP_COL,
) -> tuple[pd.DataFrame, dict]:
    """
    Kategorik değişkeni gruplar arasında karşılaştırır.

    2x2 tabloda beklenen değer düşükse Fisher kesin testi,
    diğer durumlarda Ki-kare testi kullanılır.
    """
    table = pd.crosstab(df[group_col], df[variable])

    if table.shape[0] < 2 or table.shape[1] < 2:
        return table, {
            "Değişken": variable,
            "Test": "Uygulanamadı",
            "Test istatistiği": np.nan,
            "sd": np.nan,
            "p": np.nan,
            "p değeri": "",
            "Anlamlılık": "Değerlendirilemedi",
        }

    chi2, p_chi, dof, expected = stats.chi2_contingency(table)
    min_expected = expected.min()

    if table.shape == (2, 2) and min_expected < 5:
        stat, p = stats.fisher_exact(table)
        test_name = "Fisher kesin testi"
        sd = np.nan
    else:
        stat, p = chi2, p_chi
        test_name = "Ki-kare testi"
        sd = dof

    result = {
        "Değişken": variable,
        "Test": test_name,
        "Test istatistiği": round(stat, 3),
        "sd": sd,
        "Beklenen en küçük değer": round(min_expected, 3),
        "p": p,
        "p değeri": p_degeri_formatla(p),
        "Anlamlılık": "Anlamlı" if p < 0.05 else "Anlamlı değil",
    }

    return table, result


def spearman_pairs(
    df: pd.DataFrame,
    pairs: list[tuple[str, str]],
    analysis_group: str,
) -> pd.DataFrame:
    """Belirlenen değişken çiftleri için Spearman korelasyonu hesaplar."""
    rows = []

    for x, y in pairs:
        if x not in df.columns or y not in df.columns:
            continue

        subset = df[[x, y]].dropna()

        if len(subset) < 3:
            continue

        rho, p = stats.spearmanr(subset[x], subset[y])

        rows.append(
            {
                "Analiz grubu": analysis_group,
                "Değişken 1": x,
                "Değişken 2": y,
                "n": len(subset),
                "Spearman rho": round(rho, 3),
                "p": p,
                "p değeri": p_degeri_formatla(p),
                "Anlamlılık": "Anlamlı" if p < 0.05 else "Anlamlı değil",
            }
        )

    return pd.DataFrame(rows)


# ---------------------------------------------------------------------
# 03 — VERİYİ YÜKLEME
# ---------------------------------------------------------------------

if not DATA_PATH.exists():
    raise FileNotFoundError(
        f"{DATA_PATH} bulunamadı. "
        "Anonimleştirilmiş çalışma kitabını proje klasörüne ekleyin."
    )

dhi = pd.read_excel(DATA_PATH, sheet_name="01_DHI_Anket")
timp = pd.read_excel(DATA_PATH, sheet_name="02_Timpanogram")
odyo = pd.read_excel(DATA_PATH, sheet_name="03_Odyogram")
vhit = pd.read_excel(DATA_PATH, sheet_name="04_VHIT")
vng = pd.read_excel(DATA_PATH, sheet_name="05_VNG")

tables = {
    "DHI": dhi,
    "Timpanogram": timp,
    "Odyogram": odyo,
    "vHIT": vhit,
    "VNG": vng,
}

for name, df in tables.items():
    require_columns(df, [ID_COL], name)
    df[ID_COL] = clean_id(df[ID_COL])

    if GROUP_COL in df.columns:
        df[GROUP_COL] = df[GROUP_COL].astype(str).str.strip()


# ---------------------------------------------------------------------
# 04 — VERİ KALİTE KONTROLLERİ
# ---------------------------------------------------------------------

quality_rows = []

for name, df in tables.items():
    quality_rows.append(
        {
            "Tablo": name,
            "Kayıt sayısı": len(df),
            "Tekrarlı kayıt ID": int(df[ID_COL].duplicated().sum()),
            "Eksik kayıt ID": int(df[ID_COL].isna().sum()),
        }
    )

quality_control = pd.DataFrame(quality_rows)

# Ana kayıt listesi odyogram tablosu üzerinden tanımlanır.
master_ids = set(odyo[ID_COL].dropna())

id_alignment_rows = []

for name, df in tables.items():
    current_ids = set(df[ID_COL].dropna())

    id_alignment_rows.append(
        {
            "Tablo": name,
            "Ana listede olup tabloda olmayan": len(master_ids - current_ids),
            "Tabloda olup ana listede olmayan": len(current_ids - master_ids),
        }
    )

id_alignment = pd.DataFrame(id_alignment_rows)


# ---------------------------------------------------------------------
# 05 — ANA ANALİZ VERİ SETİNİ OLUŞTURMA
# ---------------------------------------------------------------------

require_columns(dhi, [ID_COL, GROUP_COL, "dhi_toplam"], "DHI")
require_columns(
    odyo,
    [
        ID_COL,
        "dogum_yili",
        "cinsiyet",
        "pta_hava_sag",
        "pta_hava_sol",
        "pta_kemik_sag",
        "pta_kemik_sol",
        "srt_sag_db",
        "srt_sol_db",
        "sds_sag_yuzde",
        "sds_sol_yuzde",
    ],
    "Odyogram",
)

dhi_columns = [ID_COL, GROUP_COL, "dhi_toplam"] + [
    f"dhi{i:02d}" for i in range(1, 26) if f"dhi{i:02d}" in dhi.columns
]

timp_columns = [
    ID_COL,
    "sag_ecv_ml",
    "sag_peak_ml",
    "sag_peak_dapa",
    "sol_ecv_ml",
    "sol_peak_ml",
    "sol_peak_dapa",
]
timp_columns = [col for col in timp_columns if col in timp.columns]

odyo_columns = [
    ID_COL,
    "dogum_yili",
    "cinsiyet",
    "pta_hava_sag",
    "pta_hava_sol",
    "pta_kemik_sag",
    "pta_kemik_sol",
    "srt_sag_db",
    "srt_sol_db",
    "sds_sag_yuzde",
    "sds_sol_yuzde",
]

dhi_analysis = dhi[dhi_columns].copy()
timp_analysis = timp[timp_columns].copy()
odyo_analysis = odyo[odyo_columns].copy()

vhit_analysis = vhit.drop(
    columns=[GROUP_COL, "ad_soyad", "isim", "name"],
    errors="ignore",
).copy()

vng_analysis = vng.drop(
    columns=[GROUP_COL, "ad_soyad", "isim", "name"],
    errors="ignore",
).copy()

analysis_df = dhi_analysis.merge(timp_analysis, on=ID_COL, how="left")
analysis_df = analysis_df.merge(odyo_analysis, on=ID_COL, how="left")
analysis_df = analysis_df.merge(vhit_analysis, on=ID_COL, how="left")
analysis_df = analysis_df.merge(vng_analysis, on=ID_COL, how="left")


# ---------------------------------------------------------------------
# 06 — SAYISAL DÖNÜŞÜM VE TÜRETİLMİŞ DEĞİŞKENLER
# ---------------------------------------------------------------------

numeric_columns = [
    "dhi_toplam",
    "sag_ecv_ml",
    "sag_peak_ml",
    "sag_peak_dapa",
    "sol_ecv_ml",
    "sol_peak_ml",
    "sol_peak_dapa",
    "dogum_yili",
    "pta_hava_sag",
    "pta_hava_sol",
    "pta_kemik_sag",
    "pta_kemik_sol",
    "srt_sag_db",
    "srt_sol_db",
    "sds_sag_yuzde",
    "sds_sol_yuzde",
    "vHIT_HOR_Sag_Gain",
    "vHIT_HOR_Sol_Gain",
    "vHIT_sağ_posteRior_gain",
    "vHIT_sol_posterior_gain",
    "vHIT_sol_anterior_gain",
    "vHIT_sağ_anterior_gain",
    "SP_Sag_Goz_Kazanc_%",
    "SP_Sol_Goz_Kazanc_%",
    "SP_UNİLATERAL_ZAYIFLIK",
    "OPK_Sol45_Kazanc_%",
    "OPK_Sag45_Kazanc_%",
    "OPT_YÖN_ASİMETRİ",
    "Sakkad_Sol_Latans_ms_",
    "Sakkad_Sag_Latans_ms_",
    "Sakkad_Sol_Velocity",
    "Sakkad_Sag_Velocity",
]

numeric_columns += [f"dhi{i:02d}" for i in range(1, 26)]

for col in numeric_columns:
    if col in analysis_df.columns:
        analysis_df[col] = sayisala_cevir(analysis_df[col])

# Çalışma yılı örneği. Gerekirse veri toplama yılına göre değiştirilebilir.
REFERENCE_YEAR = 2026

if "dogum_yili" in analysis_df.columns:
    analysis_df["yas_hesaplanan"] = REFERENCE_YEAR - analysis_df["dogum_yili"]

analysis_df["pta_hava_ortalama"] = analysis_df[
    ["pta_hava_sag", "pta_hava_sol"]
].mean(axis=1)

analysis_df["pta_kemik_ortalama"] = analysis_df[
    ["pta_kemik_sag", "pta_kemik_sol"]
].mean(axis=1)

analysis_df["sds_ortalama"] = analysis_df[
    ["sds_sag_yuzde", "sds_sol_yuzde"]
].mean(axis=1)

if {
    "vHIT_HOR_Sag_Gain",
    "vHIT_HOR_Sol_Gain",
}.issubset(analysis_df.columns):
    analysis_df["vhit_horizontal_gain_ortalama"] = analysis_df[
        ["vHIT_HOR_Sag_Gain", "vHIT_HOR_Sol_Gain"]
    ].mean(axis=1)

if {
    "vHIT_sol_anterior_gain",
    "vHIT_sağ_posteRior_gain",
}.issubset(analysis_df.columns):
    analysis_df["vhit_larp_gain_ortalama"] = analysis_df[
        ["vHIT_sol_anterior_gain", "vHIT_sağ_posteRior_gain"]
    ].mean(axis=1)

if {
    "vHIT_sağ_anterior_gain",
    "vHIT_sol_posterior_gain",
}.issubset(analysis_df.columns):
    analysis_df["vhit_ralp_gain_ortalama"] = analysis_df[
        ["vHIT_sağ_anterior_gain", "vHIT_sol_posterior_gain"]
    ].mean(axis=1)


# ---------------------------------------------------------------------
# 07 — MANTIK KONTROLLERİ
# ---------------------------------------------------------------------

logic_rows = []

if "dhi_toplam" in analysis_df.columns:
    dhi_items = [
        f"dhi{i:02d}" for i in range(1, 26)
        if f"dhi{i:02d}" in analysis_df.columns
    ]

    if len(dhi_items) == 25:
        valid_dhi = analysis_df[dhi_items].isin({0, 2, 4}).all(axis=1)
        calculated_total = analysis_df[dhi_items].sum(axis=1)

        logic_rows.append(
            {
                "Kontrol": "DHI madde değerleri 0/2/4",
                "Sorunlu kayıt": int((~valid_dhi).sum()),
            }
        )
        logic_rows.append(
            {
                "Kontrol": "DHI toplam puan tutarlılığı",
                "Sorunlu kayıt": int(
                    (analysis_df["dhi_toplam"] != calculated_total).sum()
                ),
            }
        )

if {"sds_sag_yuzde", "sds_sol_yuzde"}.issubset(analysis_df.columns):
    sds_invalid = (
        (analysis_df["sds_sag_yuzde"] < 0)
        | (analysis_df["sds_sag_yuzde"] > 100)
        | (analysis_df["sds_sol_yuzde"] < 0)
        | (analysis_df["sds_sol_yuzde"] > 100)
    )
    logic_rows.append(
        {
            "Kontrol": "SDS 0–100 aralığı",
            "Sorunlu kayıt": int(sds_invalid.sum()),
        }
    )

logic_control = pd.DataFrame(logic_rows)


# ---------------------------------------------------------------------
# 08 — KLİNİK SINIFLANDIRMALAR
# ---------------------------------------------------------------------

def isitme_kaybi_sinifi(pta):
    if pd.isna(pta):
        return np.nan
    if pta <= 25:
        return "Normal"
    if pta <= 40:
        return "Hafif"
    if pta <= 55:
        return "Orta"
    if pta <= 70:
        return "Orta ileri"
    if pta <= 90:
        return "İleri"
    return "Çok ileri"


def dhi_sinifi(score):
    if pd.isna(score):
        return np.nan
    if score <= 14:
        return "Yok / çok hafif"
    if score <= 34:
        return "Hafif"
    if score <= 52:
        return "Orta"
    return "Şiddetli"


analysis_df["isitme_kaybi_derecesi"] = analysis_df[
    "pta_hava_ortalama"
].apply(isitme_kaybi_sinifi)

analysis_df["dhi_etkilenim_sinifi"] = analysis_df[
    "dhi_toplam"
].apply(dhi_sinifi)


# ---------------------------------------------------------------------
# 09 — TANIMLAYICI İSTATİSTİKLER
# ---------------------------------------------------------------------

main_numeric_variables = [
    "yas_hesaplanan",
    "pta_hava_ortalama",
    "sds_ortalama",
    "dhi_toplam",
    "vhit_horizontal_gain_ortalama",
    "vhit_larp_gain_ortalama",
    "vhit_ralp_gain_ortalama",
]

descriptives = group_descriptives(
    analysis_df,
    main_numeric_variables,
)

gender_table = (
    pd.crosstab(analysis_df[GROUP_COL], analysis_df["cinsiyet"], margins=True)
    if "cinsiyet" in analysis_df.columns
    else pd.DataFrame()
)

hearing_loss_table = pd.crosstab(
    analysis_df[GROUP_COL],
    analysis_df["isitme_kaybi_derecesi"],
    margins=True,
)

dhi_class_table = pd.crosstab(
    analysis_df[GROUP_COL],
    analysis_df["dhi_etkilenim_sinifi"],
    margins=True,
)


# ---------------------------------------------------------------------
# 10 — HASTA / KONTROL SAYISAL KARŞILAŞTIRMALARI
# ---------------------------------------------------------------------

normality_results, numeric_comparisons = compare_two_groups(
    analysis_df,
    main_numeric_variables,
)


# ---------------------------------------------------------------------
# 11 — KATEGORİK KARŞILAŞTIRMALAR
# ---------------------------------------------------------------------

categorical_results = []
categorical_tables = {}

categorical_variables = [
    "cinsiyet",
    "isitme_kaybi_derecesi",
    "dhi_etkilenim_sinifi",
]

for variable in categorical_variables:
    if variable not in analysis_df.columns:
        continue

    table, result = categorical_test(analysis_df, variable)
    categorical_tables[variable] = table
    categorical_results.append(result)

categorical_comparisons = pd.DataFrame(categorical_results)


# ---------------------------------------------------------------------
# 12 — SPEARMAN KORELASYONLARI
# ---------------------------------------------------------------------

correlation_pairs = [
    ("pta_hava_ortalama", "vhit_horizontal_gain_ortalama"),
    ("pta_hava_ortalama", "vhit_larp_gain_ortalama"),
    ("pta_hava_ortalama", "vhit_ralp_gain_ortalama"),
    ("sds_ortalama", "vhit_horizontal_gain_ortalama"),
    ("sds_ortalama", "vhit_larp_gain_ortalama"),
    ("sds_ortalama", "vhit_ralp_gain_ortalama"),
    ("dhi_toplam", "pta_hava_ortalama"),
    ("dhi_toplam", "sds_ortalama"),
    ("dhi_toplam", "vhit_horizontal_gain_ortalama"),
    ("dhi_toplam", "vhit_larp_gain_ortalama"),
    ("dhi_toplam", "vhit_ralp_gain_ortalama"),
]

correlation_all = spearman_pairs(
    analysis_df,
    correlation_pairs,
    "Tüm katılımcılar",
)

patient_df = analysis_df[
    analysis_df[GROUP_COL] == "Hasta"
].copy()

correlation_patient = spearman_pairs(
    patient_df,
    correlation_pairs,
    "Hasta grubu",
)

correlation_results = pd.concat(
    [correlation_all, correlation_patient],
    ignore_index=True,
)


# ---------------------------------------------------------------------
# 13 — vHIT KANAL DÜZLEMLERİ
# ---------------------------------------------------------------------

vhit_planes = [
    "vhit_horizontal_gain_ortalama",
    "vhit_larp_gain_ortalama",
    "vhit_ralp_gain_ortalama",
]

vhit_friedman_rows = []

if all(col in analysis_df.columns for col in vhit_planes):
    for group_name in EXPECTED_GROUPS:
        group_df = analysis_df.loc[
            analysis_df[GROUP_COL] == group_name,
            vhit_planes,
        ].dropna()

        if len(group_df) >= 3:
            stat, p = stats.friedmanchisquare(
                group_df[vhit_planes[0]],
                group_df[vhit_planes[1]],
                group_df[vhit_planes[2]],
            )

            vhit_friedman_rows.append(
                {
                    "Grup": group_name,
                    "n": len(group_df),
                    "Test": "Friedman testi",
                    "Test istatistiği": round(stat, 3),
                    "p": p,
                    "p değeri": p_degeri_formatla(p),
                    "Anlamlılık": "Anlamlı" if p < 0.05 else "Anlamlı değil",
                }
            )

vhit_friedman_results = pd.DataFrame(vhit_friedman_rows)


# ---------------------------------------------------------------------
# 14 — VNG NİSTAGMUS BULGULARI
# ---------------------------------------------------------------------

vng_nystagmus_variables = {
    "Spontan_Nistagmus": "Spontan nistagmus",
    "DH_Sag_nist": "Dix-Hallpike sağ nistagmus",
    "DH_Sol_nist": "Dix-Hallpike sol nistagmus",
    "Head_Right_NİST": "Head right nistagmus",
    "Head_LEFT_NİST": "Head left nistagmus",
    "Gaze_Merkez_NİST_": "Gaze merkez nistagmus",
    "Gaze_Sag_NİST_": "Gaze sağ nistagmus",
    "Gaze_Sol_NİST_": "Gaze sol nistagmus",
    "Gaze_Yukari_NİST_": "Gaze yukarı nistagmus",
    "Gaze_Asagi_NİST_": "Gaze aşağı nistagmus",
}

vng_binary_results = []

for variable, label in vng_nystagmus_variables.items():
    if variable not in analysis_df.columns:
        continue

    temp = analysis_df[[GROUP_COL, variable]].copy()
    temp[variable] = pd.to_numeric(temp[variable], errors="coerce")
    binary_col = f"{variable}_var_yok"
    temp[binary_col] = np.where(temp[variable] > 0, "Var", "Yok")

    _, result = categorical_test(
        temp,
        binary_col,
        group_col=GROUP_COL,
    )
    result["Değişken"] = label
    vng_binary_results.append(result)

vng_binary_comparisons = pd.DataFrame(vng_binary_results)


# ---------------------------------------------------------------------
# 15 — VNG SAYISAL OKÜLOMOTOR DEĞİŞKENLER
# ---------------------------------------------------------------------

vng_numeric_variables = [
    "SP_UNİLATERAL_ZAYIFLIK",
    "OPK_Sol45_Kazanc_%",
    "OPK_Sag45_Kazanc_%",
    "OPT_YÖN_ASİMETRİ",
    "Sakkad_Sol_Latans_ms_",
    "Sakkad_Sag_Latans_ms_",
    "Sakkad_Sol_Velocity",
    "Sakkad_Sag_Velocity",
]

vng_normality, vng_numeric_comparisons = compare_two_groups(
    analysis_df,
    vng_numeric_variables,
)


# ---------------------------------------------------------------------
# 16 — ÇIKTILARI EXCEL DOSYASINA YAZMA
# ---------------------------------------------------------------------

output_excel = OUTPUT_DIR / "Klinik_Biyostatistik_Analiz_Sonuclari.xlsx"

with pd.ExcelWriter(output_excel, engine="openpyxl") as writer:
    quality_control.to_excel(
        writer,
        sheet_name="Kalite_Kontrol",
        index=False,
    )

    id_alignment.to_excel(
        writer,
        sheet_name="ID_Uyumu",
        index=False,
    )

    logic_control.to_excel(
        writer,
        sheet_name="Mantik_Kontrol",
        index=False,
    )

    descriptives.to_excel(
        writer,
        sheet_name="Tanimlayici",
        index=False,
    )

    normality_results.to_excel(
        writer,
        sheet_name="Normalite",
        index=False,
    )

    numeric_comparisons.to_excel(
        writer,
        sheet_name="Sayisal_Karsilastirma",
        index=False,
    )

    categorical_comparisons.to_excel(
        writer,
        sheet_name="Kategorik_Karsilastirma",
        index=False,
    )

    correlation_results.to_excel(
        writer,
        sheet_name="Korelasyon",
        index=False,
    )

    vhit_friedman_results.to_excel(
        writer,
        sheet_name="vHIT_Friedman",
        index=False,
    )

    vng_binary_comparisons.to_excel(
        writer,
        sheet_name="VNG_VarYok",
        index=False,
    )

    vng_normality.to_excel(
        writer,
        sheet_name="VNG_Normalite",
        index=False,
    )

    vng_numeric_comparisons.to_excel(
        writer,
        sheet_name="VNG_Sayisal",
        index=False,
    )


# ---------------------------------------------------------------------
# 17 — ÖZET
# ---------------------------------------------------------------------

print("Analiz tamamlandı.")
print(f"Ana veri seti boyutu: {analysis_df.shape}")
print(f"Sonuç dosyası: {output_excel.resolve()}")
print(
    "Bu GitHub sürümü kişisel isim, özel Drive yolu ve gerçek katılımcı "
    "eşleştirmeleri içermez."
)
