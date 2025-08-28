import pandas as pd
import numpy as np
from datetime import datetime

# ===== 設定 =====
FILE_PATH = "matsudo.xlsx"            # または '/mnt/data/test.xlsx'
SHEET_NAME = "Sheet1"
START_DATE = pd.to_datetime("2021-02-01")   # 観察開始日（必要に応じて変更）
MAX_DOSE_STATE = 8                         # 0〜8回まで集計

# ===== ヘルパー =====
def parse_date_series(s):
    """様々な形式の列を安全に日付に変換する（YYYYMMDDの数値や日付型などに対応）"""
    ser = pd.to_datetime(s, errors='coerce', format="%Y%m%d")
    # うまく変換できなかったセルに対して一般的なパースを試す
    mask = ser.isna()
    if mask.any():
        ser2 = pd.to_datetime(s, errors='coerce', infer_datetime_format=True)
        ser[mask] = ser2[mask]
    return ser

def earliest_notna(ts_list, default=None):
    """pd.Timestamp のリストから非NAの最小値を返す。なければ default."""
    valid = [t for t in ts_list if pd.notna(t)]
    if not valid:
        return default
    return min(valid)

def latest_notna(ts_list, default=None):
    valid = [t for t in ts_list if pd.notna(t)]
    if not valid:
        return default
    return max(valid)

# ===== データ読み込み =====
df = pd.read_excel(FILE_PATH, sheet_name=SHEET_NAME, dtype=object)

# 接種日列を自動抽出（列名に "接種日" を含むもの全て）
dose_cols = [c for c in df.columns if "接種日" in str(c)]
# それに加えて転出日・死亡日の列名（存在しなければ無視）
transfer_col = next((c for c in df.columns if "転出日" == str(c)), None)
death_col = next((c for c in df.columns if "死亡または死亡届出日" == str(c)), None)

# ===== 日付列をパースして _p 列に格納 =====
parsed_cols = []
for c in dose_cols:
    parsed_name = f"{c}__p"
    df[parsed_name] = parse_date_series(df[c])
    parsed_cols.append(parsed_name)

if transfer_col is not None:
    df["転出日__p"] = parse_date_series(df[transfer_col])
    parsed_cols.append("転出日__p")
else:
    df["転出日__p"] = pd.NaT

if death_col is not None:
    df["死亡日__p"] = parse_date_series(df[death_col])
    parsed_cols.append("死亡日__p")
else:
    df["死亡日__p"] = pd.NaT

# データ中で利用可能な最大観察日（データ中の最大日または今日）
#all_max = None
#if parsed_cols:
    # 各列の最大値を取り、その中の最大（グローバル最終日）を取る
#    col_maxes = [df[c].max() for c in parsed_cols]
#    all_max = latest_notna(col_maxes, default=None)

#if all_max is None or pd.isna(all_max):
#    all_max = pd.to_datetime(datetime.today())  # デフォルトは「今日」
all_max = pd.to_datetime("2025-03-31")

# ===== 各人について 0..8 の状態ごとの日数を計算 =====
person_days = np.zeros(MAX_DOSE_STATE + 1, dtype=float)  # 0..8 の日数合計（人日）

for idx, row in df.iterrows():

#    if row[0] not in ("1956～1960", "1951～1955", "1946～1950", "1941～1945", "1936～1940"): #65～89歳
#    if row[0] not in ("1971～1975", "1966～1970", "1961～1965"): #50～64歳
    if row[0] not in ("2001～2005", "1996～2000", "1991～1995", "1986～1990",  "1981～1985", "1976～1980"): #20～49歳

        continue
    
    # 各人の打てる接種日（非NA）を収集・昇順ソート
    doses = []
    for c in dose_cols:
        parsed = row.get(f"{c}__p")
        if pd.notna(parsed):
            doses.append(pd.to_datetime(parsed))
    doses = sorted(doses)

    # 観察終了日は (転出日 または 死亡日 の早い方) またはデータ最終日
    person_transfer = row.get("転出日__p", pd.NaT)
    person_death = row.get("死亡日__p", pd.NaT)
    # 候補の最小（存在するものの中で）
    censor = earliest_notna([person_transfer, person_death], default=all_max)
    # 最終的には研究の最終日も候補（censor が NaT の場合に備える）
    if pd.isna(censor):
        censor = all_max
    # ただし、censor が all_max より大きい（データ中の未来日）でもOK（データに従う）

    # --- 状態 0: START_DATE から最初の接種日または censor まで ---
    if doses:
        first_dose = doses[0]
        end0 = min(first_dose, censor)
    else:
        end0 = censor
    # 0状態の開始は START_DATE
    s0 = START_DATE
    e0 = end0
    days0 = (e0 - s0).days if (pd.notna(e0) and pd.notna(s0)) else 0
    if days0 < 0:
        days0 = 0
    person_days[0] += days0

    # --- 状態 1..MAX_DOSE_STATE（N回）: N回目の接種日から (N+1)回目またはcensorまで ---
    for N in range(1, MAX_DOSE_STATE + 1):
        # N回目が存在するか
        if len(doses) >= N:
            state_start = doses[N - 1]
        else:
            state_start = None

        # 次の接種（N+1回目）が存在するか
        if len(doses) >= (N + 1):
            next_dose = doses[N]
        else:
            next_dose = None

        if state_start is None:
            # この人は N回目に到達していない → 日数0
            continue

        s = max(state_start, START_DATE)  # 状態開始は N回目接種日またはSTART_DATEのいずれか後
        if next_dose is not None:
            e = min(next_dose, censor)
        else:
            e = censor

        if pd.isna(s) or pd.isna(e):
            d = 0
        else:
            d = (e - s).days
            if d < 0:
                d = 0
        person_days[N] += d

# ===== 日数 -> 人年 に換算して結果表示 =====
person_years = person_days / 365.0

result = pd.DataFrame({
    "接種回数": list(range(0, MAX_DOSE_STATE + 1)),
    "人日合計": person_days,
    "人年合計": person_years
})
print(result.to_string(index=False))