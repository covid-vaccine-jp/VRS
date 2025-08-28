# 浜松市の全死因死者数を2021年下半期～2024年上半期までワクチン接種回数別に出力するプログラム
import csv
from datetime import datetime
import re
import numpy as np

#OUT = "death_hm65-89.csv" #65～89歳
#OUT = "death_hm50-64.csv" #50～64歳
OUT = "death_hm20-49.csv" #20～49歳

date = [0] * 7
death_count = np.zeros((8, 6), dtype=int)
#age_targets = ["65～69歳", "70～74歳", "75～79歳", "80～84歳", "85～89歳"]
#age_targets = ["50～54歳", "55～59歳", "60～64歳"]
age_targets = ["20～24歳", "25～29歳", "30～34歳", "35～39歳", "40～44歳", "45～49歳"]

date[0] = datetime.strptime("2021/7/1", "%Y/%m/%d")
date[1] = datetime.strptime("2022/1/1", "%Y/%m/%d")
date[2] = datetime.strptime("2022/7/1", "%Y/%m/%d")
date[3] = datetime.strptime("2023/1/1", "%Y/%m/%d")
date[4] = datetime.strptime("2023/7/1", "%Y/%m/%d")
date[5] = datetime.strptime("2024/1/1", "%Y/%m/%d")
date[6] = datetime.strptime("2024/7/1", "%Y/%m/%d")
zen_to_han = str.maketrans("０１２３４５６７８９", "0123456789")

with open("input.csv", encoding="utf-8-sig") as csvfile: # 浜松のデータ("input.csv")を読み込む
    reader = list(csv.reader(csvfile))
    length = len(reader)
    for i in range(1, len(reader)):
        row = reader[i]

        if len(row) < 8:
            continue  # 不完全な行をスキップ

        if row[2].strip() not in age_targets:
            continue

        second_col = row[1].strip()
        fifth_col = row[4].strip()
        eighth_col = row[7].strip()

        # 2列目が "該当" のとき
        if second_col == "該当":
            if fifth_col == "":
                if not row[8] == "NULL":
                    death_date = datetime.strptime(row[8], "%Y-%m-%d")
                    for j in range(6):
                        if ((death_date - date[j]).days >= 0) and ((date[j+1] - death_date).days >= 1):
                            death_count[0][j] += 1

                continue  # 日付処理は不要、次の行へ

            # 5列目が "X回目" のとき
            match = re.match(r"([0-9０-９])回目", fifth_col)
            if match:
                try:
                    current_date = datetime.strptime(eighth_col, "%Y-%m-%d")
                except ValueError:
                    continue

                x = int(match.group(1).translate(zen_to_han))
                if not row[8] == "NULL":
                    death_date = datetime.strptime(row[8], "%Y-%m-%d")
                    for j in range(6):
                        if ((death_date - date[j]).days >= 0) and ((date[j+1] - death_date).days > 0):
                            death_count[x][j] += 1

# 結果出力
with open(OUT, "w") as o:
    for i in range(8):
        for j in range(6):
            print(death_count[i][j], "," , end="", file=o)
        print("", file=o)

        