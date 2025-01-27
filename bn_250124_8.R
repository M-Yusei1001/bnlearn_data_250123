#data_250123は類似表現の統一をしたデータを用いたBN
# ライブラリ読み込み
library(bnlearn)

#データ読み込み,処理
product <- read.csv("~/rscripts/data/250124_8/data_products_20250124.csv")
product <- as.data.frame(lapply(product, factor))

states <- read.csv("~/rscripts/data/250124_8/data_states_20250124.csv")
states <- states$states

incidents <- read.csv("~/rscripts/data/250124_8/data_incidents_20250124.csv")
incidents <- incidents$incidents

causes <- read.csv("~/rscripts/data/250124_8/data_causes_20250124.csv")
causes <- causes$causes

print("Data load: done")


# ブラックリスト作成
createEmptyMatrix = function( nrow, ncol, colnames = c() ){
  if( missing( ncol ) && length( colnames ) > 0 ){
    ncol = length( colnames )
  }
  matrix( vector(mode = "character"), nrow, ncol, dimnames = list( c(), colnames ) )
}

N = (length(states) * length(states)) + (length(incidents) * length(incidents)) + (length(causes) * length(causes)) + (length(incidents) * length(causes)) + (length(causes) * length(states)) + (length(incidents) * length(states) * 2)

bl <- createEmptyMatrix(N, colnames = c("from", "to"))

# 層内のブラックリスト作成
row_num = 1

for (i in 1:length(states)){
  for (j in 1:length(states)){
    if (states[i] != states[j]){
      if (is.na(states[i]) | is.na(states[j])){
        next
      }
      bl[row_num, 1] <- states[i]
      bl[row_num, 2] <- states[j]
      row_num <- row_num + 1
    }
  }
}
print(row_num/N*100)
print("Done: states and states")

for (i in 1:length(incidents)){
  for (j in 1:length(incidents)){
    if (incidents[i] != incidents[j]){
      if (is.na(incidents[i]) | is.na(incidents[j])){
        next
      }
      bl[row_num, 1] <- incidents[i]
      bl[row_num, 2] <- incidents[j]
      row_num <- row_num + 1
    }
  }
}
print(row_num/N*100)
print("Done: incidents and incidents")

for (i in 1:length(causes)){
  for (j in 1:length(causes)){
    if (causes[i] != causes[j]){
      if (is.na(causes[i]) | is.na(causes[j])){
        next
      }
      bl[row_num, 1] <- causes[i]
      bl[row_num, 2] <- causes[j]
      row_num <- row_num + 1
    }
  }
}

print(row_num/N*100)
print("Done: causes and causes")
print("blacklist in each layers: done")

#層間のブラックリスト指定
for (i in 1:length(incidents)){
  for (j in 1:length(causes)){
    if (incidents[i] != causes[j]){
      if (is.na(incidents[i]) | is.na(causes[j])){
        next
      }
      bl[row_num, 1] <- incidents[i]
      bl[row_num, 2] <- causes[j]
      row_num <- row_num + 1
    }
  }
}
print(row_num/N*100)
print("Done: incidents to causes")

for (i in 1:length(causes)){
  for (j in 1:length(states)){
    if (causes[i] != states[j]){
      if (is.na(causes[i]) | is.na(states[j])){
        next
      }
      bl[row_num, 1] <- causes[i]
      bl[row_num, 2] <- states[j]
      row_num <- row_num + 1
    }
  }
}
print(row_num/N*100)
print("Done: causes to states")

for (i in 1:length(states)){
  for (j in 1:length(incidents)){
    if (states[i] != incidents[j]){
      if (is.na(states[i]) | is.na(incidents[j])){
        next
      }
      bl[row_num, 1] <- states[i]
      bl[row_num, 2] <- incidents[j]
      row_num <- row_num + 1
    }
  }
}
print(row_num/N*100)
print("Done: states to incidents")

for (i in 1:length(incidents)){
  for (j in 1:length(states)){
    if (incidents[i] != states[j]){
      if (is.na(incidents[i]) | is.na(states[j])){
        next
      }
      bl[row_num, 1] <- incidents[i]
      bl[row_num, 2] <- states[j]
      row_num <- row_num + 1
    }
  }
}
print(row_num/N*100)
print("Done: incidents to states")

bl <- bl[complete.cases(bl), ]

# 構造学習
dag <- hc(product, blacklist = bl)

# パラメータ学習
fitted = bn.fit(dag, product, method = "bayes")

# グラフのプロット
graphviz.plot(fitted)

# AICとBICのスコア表示
score(dag, product, type = "aic")
score(dag, product, type = "bic")

# DOT言語のファイルを出力
write.dot("~/rscripts/output/data_250124_8.dot", fitted)

