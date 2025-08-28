import csv
from datetime import datetime
import re

jinnen = [0] * 8

# 対象年齢帯（必要に応じ変更）
#age_targets = ["65～69歳", "70～74歳", "75～79歳", "80～84歳", "85～89歳"]
age_targets = ["50～54歳", "55～59歳", "60～64歳"]
#age_targets = ["20～24歳", "25～29歳", "30～34歳", "35～39歳", "40～44歳", "45～49歳"]

start_date = datetime.strptime("2021/2/1", "%Y/%m/%d")
reference_date = datetime.strptime("2024/6/30", "%Y/%m/%d")
zen_to_han = str.maketrans("０１２３４５６７８９", "0123456789")

def safe_get(row, idx):
    return row[idx].strip() if idx < len(row) else ""

with open("input.csv", encoding="utf-8-sig") as csvfile:
    reader = list(csv.reader(csvfile))
    for i in range(1, len(reader)):
        row = reader[i]

        if safe_get(row, 2) not in age_targets:
            continue

        fifth_col = safe_get(row, 4)
        eighth_col = safe_get(row, 7)
        death_col = safe_get(row, 11)

        # 5列目が空なら「未接種（終観察まで）」扱い
        if fifth_col == "":
            try:
                if death_col and death_col != "NULL":
                    death_date = datetime.strptime(death_col, "%Y-%m-%d")
                    delta = (death_date - start_date).days
                else:
                    delta = (reference_date - start_date).days
                jinnen[0] += delta
            except ValueError:
                continue
            continue

        # "X回目" を抽出（全角・複数桁対応）
        match = re.match(r"([0-9０-９]+)回目", fifth_col)
        if not match:
            continue

        x = int(match.group(1).translate(zen_to_han))

        # 現在接種日のパース
        try:
            current_date = datetime.strptime(eighth_col, "%Y-%m-%d")
        except ValueError:
            continue

        # --- 修正点: 1回目を見つけたら start_date -> current_date を 0回（未接種）に加算 ---
        if x == 1:
            # current_date が study start より後なら差分を加える（負にならないようガード）
            if current_date > start_date:
                jinnen[0] += (current_date - start_date).days
        # ------------------------------------------------------------------------------

        # 次接種日が同一被験者の次行にあれば end_date にする（全角/半角対応）
        expected_next = f"{x+1}回目"
        expected_next_fw = expected_next.translate(str.maketrans("0123456789", "０１２３４５６７８９"))
        end_date = None
        if (i + 1) < len(reader):
            next_row = reader[i + 1]
            next_fifth = safe_get(next_row, 4)
            next_eighth = safe_get(next_row, 7)
            if next_fifth in (expected_next, expected_next_fw):
                try:
                    end_date = datetime.strptime(next_eighth, "%Y-%m-%d")
                except ValueError:
                    end_date = None

        # 次接種が無ければ死亡日／観察終了日を end_date とする
        if end_date is None:
            if death_col and death_col != "NULL":
                try:
                    end_date = datetime.strptime(death_col, "%Y-%m-%d")
                except ValueError:
                    end_date = reference_date
            else:
                end_date = reference_date

        delta = (end_date - current_date).days

        if x >= len(jinnen):
            jinnen.extend([0] * (x - len(jinnen) + 1))
        jinnen[x] += delta

# 結果出力（日数と人年）
print("=== jinnen (days) ===")
for idx, value in enumerate(jinnen):
    print(f"jinnen[{idx}] = {value}")

print("\n=== jinnen (person-years) ===")
for idx, value in enumerate(jinnen):
    py = value / 365
    print(f"jinnen[{idx}] = {py:.6f}")