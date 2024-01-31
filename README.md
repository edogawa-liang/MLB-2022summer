# 2021 年美國職棒打者表現之評估
- 完成日期: 2022/07
- Side Project

## 簡介
此作品挑選了 46 個打擊相關的變數，希望以全面而非單一的角度評估打者表現，並找出選手各自的優勢。由於得分為影響比賽勝負的關鍵，我針對打者「每打席的平均得分」建立迴歸模型，將影響得分的重要變數作為評估打者表現的指標。這裡採取向後選取法搭配主成分分析，以及 Sparse Group Lasso 兩種方法做變數選擇與維度縮減，再分別以階層式分群法，評估每一群打者的能力與狀況。

## 檔案說明
- `作品_2021年美國職棒打者表現之評估.pdf`: 完整報告書
- `1to3_cleaning.R`: 資料前處理
- `4_dm_reduction.R`: 變數選擇 (Sparse Group Lasso)
- `5_clustering.R`: 分群 (Hierachical clustering)
- `5_球員分群.xlsx`:球員的分群結果
- `6_dm_pca.R`: PCA 維度縮減


 ## Note
- 若有任何問題或改進的建議，歡迎通過以下的電子郵件與我聯繫：[tiffany217.h@gmail.com](mailto:tiffany217.h@gmail.com)。

