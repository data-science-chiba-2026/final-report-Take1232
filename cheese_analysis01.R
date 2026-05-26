# ==============================

# 「何由来のチーズが多いのか」を可視化

# ==============================

# ------------------------------

# 1. パッケージを読み込む

# ------------------------------

library(tidyverse)

# ------------------------------

# 2. データを読み込む

# ------------------------------

cheeses <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-06-04/cheeses.csv"
)

# ------------------------------

# 3. データ確認

# ------------------------------

# 最初の6行を表示

head(cheeses)

# 列名を表示

colnames(cheeses)

# milk列の種類を確認

unique(cheeses$milk)

# ------------------------------

# 4. 欠損値を削除

# ------------------------------

milk_data <- cheeses %>%
  drop_na(milk)

# ------------------------------

# 5. milk列を分割

# ------------------------------

# 例:

# "cow, goat"

# ↓

# cow

# goat

milk_tidy <- milk_data %>%
  
  separate_rows(milk, sep = ",") %>%
  
  mutate(
    milk = str_trim(milk)
  )

# ------------------------------

# 6. 件数を集計

# ------------------------------

milk_count <- milk_tidy %>%
  
  count(milk) %>%
  
  arrange(desc(n))

# 集計結果を表示

print(milk_count)

# ------------------------------

# 7. 横棒グラフを作成

# ------------------------------

ggplot(
  milk_count,
  aes(
    x = reorder(milk, n),
    y = n
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    title = "Number of Cheeses by Milk Type",
    x = "Milk Type",
    y = "Count"
  ) +
  
  theme_minimal(base_size = 14)

# ------------------------------

# 8. 割合を計算

# ------------------------------

milk_ratio <- milk_count %>%
  
  mutate(
    percent = n / sum(n) * 100
  )

# 割合を表示

print(milk_ratio)

# ------------------------------

# 9. 円グラフを作成

# ------------------------------

ggplot(
  milk_ratio,
  aes(
    x = "",
    y = percent,
    fill = milk
  )
) +
  
  geom_col(width = 1) +
  
  coord_polar(theta = "y") +
  
  labs(
    title = "Percentage of Cheese by Milk Type"
  ) +
  
  theme_void()

# ==============================

# 国別 × milk由来 分析

# ==============================

# --------------------------------

# country列を分割

# --------------------------------

country_milk_data <- cheeses %>%
  
  drop_na(country, milk) %>%
  
  separate_rows(country, sep = ",") %>%
  
  mutate(
    country = str_trim(country)
  )

# --------------------------------

# milk列を分割

# --------------------------------

country_milk_data <- country_milk_data %>%
  
  separate_rows(milk, sep = ",") %>%
  
  mutate(
    milk = str_trim(milk)
  )

# --------------------------------

# 国 × milk の件数集計

# --------------------------------

country_milk_count <- country_milk_data %>%
  
  count(country, milk) %>%
  
  arrange(desc(n))

print(country_milk_count)

# --------------------------------

# チーズ数が多い国 TOP20

# --------------------------------

top_countries <- country_milk_data %>%
  
  count(country, sort = TRUE) %>%
  
  slice_head(n = 20)

top_country_names <- top_countries$country

# --------------------------------

# TOP20だけ抽出

# --------------------------------

plot_data <- country_milk_count %>%
  
  filter(country %in% top_country_names)

# --------------------------------

# 積み上げ棒グラフ

# --------------------------------

ggplot(
  plot_data,
  aes(
    x = reorder(country, n),
    y = n,
    fill = milk
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    title = "Milk Types by Country",
    x = "Country",
    y = "Number of Cheeses",
    fill = "Milk Type"
  ) +
  
  theme_minimal(base_size = 14)

# --------------------------------

# 国ごとの割合を計算

# --------------------------------

country_ratio <- country_milk_count %>%
  
  group_by(country) %>%
  
  mutate(
    percent = n / sum(n) * 100
  ) %>%
  
  ungroup()

# --------------------------------

# 割合ベースのグラフ

# --------------------------------

plot_ratio <- country_ratio %>%
  
  filter(country %in% top_country_names)

ggplot(
  plot_ratio,
  aes(
    x = reorder(country, percent),
    y = percent,
    fill = milk
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    title = "Percentage of Milk Types by Country",
    x = "Country",
    y = "Percentage",
    fill = "Milk Type"
  ) +
  
  theme_minimal(base_size = 14)

# ==============================

# milk由来ごとに国を分析

# 例:

# cow cheese はどの国が多いか

# ==============================

# --------------------------------

# milk列とcountry列を整理

# --------------------------------

milk_country_data <- cheeses %>%
  
  drop_na(milk, country) %>%
  
  separate_rows(milk, sep = ",") %>%
  
  separate_rows(country, sep = ",") %>%
  
  mutate(
    milk = str_trim(milk),
    country = str_trim(country)
  )

# --------------------------------

# milk × country の件数集計

# --------------------------------

milk_country_count <- milk_country_data %>%
  
  count(milk, country) %>%
  
  arrange(milk, desc(n))

print(milk_country_count)

# --------------------------------

# cow milk のみ抽出

# --------------------------------

cow_data <- milk_country_count %>%
  
  filter(milk == "cow") %>%
  
  slice_max(n, n = 15)

# --------------------------------

# cow milk の国別グラフ

# --------------------------------

ggplot(
  cow_data,
  aes(
    x = reorder(country, n),
    y = n
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    title = "Top Countries for Cow Milk Cheese",
    x = "Country",
    y = "Count"
  ) +
  
  theme_minimal(base_size = 14)

# --------------------------------

# goat milk のみ抽出

# --------------------------------

goat_data <- milk_country_count %>%
  
  filter(milk == "goat") %>%
  
  slice_max(n, n = 15)

# --------------------------------

# goat milk の国別グラフ

# --------------------------------

ggplot(
  goat_data,
  aes(
    x = reorder(country, n),
    y = n
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    title = "Top Countries for Goat Milk Cheese",
    x = "Country",
    y = "Count"
  ) +
  
  theme_minimal(base_size = 14)

# --------------------------------

# sheep milk のみ抽出

# --------------------------------

sheep_data <- milk_country_count %>%
  
  filter(milk == "sheep") %>%
  
  slice_max(n, n = 15)

# --------------------------------

# sheep milk の国別グラフ

# --------------------------------

ggplot(
  sheep_data,
  aes(
    x = reorder(country, n),
    y = n
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    title = "Top Countries for Sheep Milk Cheese",
    x = "Country",
    y = "Count"
  ) +
  
  theme_minimal(base_size = 14)

# --------------------------------

# milkごとにfacet表示

# --------------------------------

top_milk_country <- milk_country_count %>%
  
  group_by(milk) %>%
  
  slice_max(n, n = 10) %>%
  
  ungroup()

ggplot(
  top_milk_country,
  aes(
    x = reorder(country, n),
    y = n,
    fill = milk
  )
) +
  
  geom_col(show.legend = FALSE) +
  
  coord_flip() +
  
  facet_wrap(~ milk, scales = "free") +
  
  labs(
    title = "Top Countries by Milk Type",
    x = "Country",
    y = "Count"
  ) +
  
  theme_minimal(base_size = 14)

# ==============================

# ミルク由来と脂肪分の関係

# ==============================

# --------------------------------

# 必要な列を抽出

# --------------------------------

milk_fat_data <- cheeses %>%
  
  select(cheese, milk, fat_content) %>%
  
  drop_na(milk, fat_content)

# --------------------------------

# fat_content を数値化

# --------------------------------

milk_fat_data <- milk_fat_data %>%
  
  mutate(
    
    fat_content = str_remove_all(fat_content, "%"),
    
    fat_value = case_when(
      
      # "30-40" → 平均35
      str_detect(fat_content, "-") ~
        (
          as.numeric(str_extract(fat_content, "^\\d+")) +
            as.numeric(str_extract(fat_content, "\\d+$"))
        ) / 2,
      
      # "45"
      str_detect(fat_content, "\\d+") ~
        as.numeric(str_extract(fat_content, "\\d+")),
      
      TRUE ~ NA_real_
    )
    
  )

# --------------------------------

# 数値化失敗を除外

# --------------------------------

milk_fat_data <- milk_fat_data %>%
  
  drop_na(fat_value)

# --------------------------------

# milk列を分割

# --------------------------------

milk_fat_data <- milk_fat_data %>%
  
  separate_rows(milk, sep = ",") %>%
  
  mutate(
    milk = str_trim(milk)
  )

# --------------------------------

# 件数確認

# --------------------------------

milk_count <- milk_fat_data %>%
  
  count(milk, sort = TRUE)

print(milk_count)

# --------------------------------

# 件数5以上のみ使用

# --------------------------------

major_milk <- milk_count %>%
  
  filter(n >= 5) %>%
  
  pull(milk)

plot_data <- milk_fat_data %>%
  
  filter(milk %in% major_milk)

# --------------------------------

# 箱ひげ図

# --------------------------------

ggplot(
  plot_data,
  aes(
    x = reorder(milk, fat_value, median),
    y = fat_value
  )
) +
  
  geom_boxplot() +
  
  coord_flip() +
  
  labs(
    title = "Fat Content Distribution by Milk Type",
    x = "Milk Type",
    y = "Fat Content (%)"
  ) +
  
  theme_minimal(base_size = 14)

# --------------------------------

# 平均・中央値確認

# --------------------------------

milk_summary <- plot_data %>%
  
  group_by(milk) %>%
  
  summarise(
    mean_fat = mean(fat_value),
    median_fat = median(fat_value),
    count = n()
  ) %>%
  
  arrange(desc(mean_fat))

print(milk_summary)

# --------------------------------

# 平均脂肪分グラフ

# --------------------------------

ggplot(
  milk_summary,
  aes(
    x = reorder(milk, mean_fat),
    y = mean_fat
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    title = "Average Fat Content by Milk Type",
    x = "Milk Type",
    y = "Average Fat Content (%)"
  ) +
  
  theme_minimal(base_size = 14)


# ==============================

# 色味(color)と脂肪分(fat_content)の関係分析

# ==============================

library(tidyverse)

# ------------------------------

# 必要な列だけ取得

# ------------------------------

color_fat_data <- cheeses %>%
  
  select(cheese, color, fat_content) %>%
  
  drop_na(color, fat_content)

# ------------------------------

# fat_content を数値化

# ------------------------------

color_fat_data <- color_fat_data %>%
  
  mutate(
    
    fat_content = str_remove_all(fat_content, "%"),
    
    fat_value = case_when(
      
      str_detect(fat_content, "-") ~
        (
          as.numeric(str_extract(fat_content, "^\\d+")) +
            as.numeric(str_extract(fat_content, "\\d+$"))
        ) / 2,
      
      str_detect(fat_content, "\\d+") ~
        as.numeric(str_extract(fat_content, "\\d+")),
      
      TRUE ~ NA_real_
    )
  
    
  )

# ------------------------------

# NA削除

# ------------------------------

color_fat_data <- color_fat_data %>%
  
  drop_na(fat_value)

# ------------------------------

# color を分割

# ------------------------------

color_fat_data <- color_fat_data %>%
  
  separate_rows(color, sep = ",") %>%
  
  mutate(
    color = str_trim(color)
  )

# ------------------------------

# 色ごとの件数

# ------------------------------

color_count <- color_fat_data %>%
  
  count(color, sort = TRUE)

print(color_count)

# ------------------------------

# 件数5以上のみ

# ------------------------------

major_colors <- color_count %>%
  
  filter(n >= 5) %>%
  
  pull(color)

plot_data <- color_fat_data %>%
  
  filter(color %in% major_colors)

# ------------------------------

# 箱ひげ図

# ------------------------------

ggplot(
  plot_data,
  aes(
    x = reorder(color, fat_value, median),
    y = fat_value
  )
) +
  
  geom_boxplot() +
  
  coord_flip() +
  
  labs(
    title = "Fat Content Distribution by Cheese Color",
    x = "Color",
    y = "Fat Content (%)"
  ) +
  
  theme_minimal(base_size = 14)

# ------------------------------

# 平均脂肪分

# ------------------------------

color_summary <- plot_data %>%
  
  group_by(color) %>%
  
  summarise(
    mean_fat = mean(fat_value),
    median_fat = median(fat_value),
    count = n()
  ) %>%
  
  arrange(desc(mean_fat))

print(color_summary)

# ==============================

# チーズの硬さ(type)と脂肪分の関係

# ==============================

# --------------------------------

# 必要な列を抽出

# --------------------------------

texture_fat_data <- cheeses %>%
  
  select(cheese, type, texture, fat_content) %>%
  
  drop_na(fat_content)

# --------------------------------

# fat_content を数値化

# --------------------------------

texture_fat_data <- texture_fat_data %>%
  
  mutate(
    
    fat_content = str_remove_all(fat_content, "%"),
    
    fat_value = case_when(
      
      # "30-40" → 平均35
      str_detect(fat_content, "-") ~
        (
          as.numeric(str_extract(fat_content, "^\\d+")) +
            as.numeric(str_extract(fat_content, "\\d+$"))
        ) / 2,
      
      # "45"
      str_detect(fat_content, "\\d+") ~
        as.numeric(str_extract(fat_content, "\\d+")),
      
      TRUE ~ NA_real_
    )
    
  )

# --------------------------------

# 数値化失敗を除外

# --------------------------------

texture_fat_data <- texture_fat_data %>%
  
  drop_na(fat_value)

# --------------------------------

# type列から硬さ情報を抽出

# --------------------------------

# type列には：

# "semi-hard"

# "hard"

# "soft, artisan"

# などが入っている

texture_fat_data <- texture_fat_data %>%
  
  mutate(
    
    hardness = case_when(
      
      str_detect(type, regex("soft", ignore_case = TRUE)) ~ "soft",
      
      str_detect(type, regex("semi-hard", ignore_case = TRUE)) ~ "semi-hard",
      
      str_detect(type, regex("hard", ignore_case = TRUE)) ~ "hard",
      
      str_detect(type, regex("firm", ignore_case = TRUE)) ~ "firm",
      
      TRUE ~ NA_character_
    )
    
  )

# --------------------------------

# NA除去

# --------------------------------

texture_fat_data <- texture_fat_data %>%
  
  drop_na(hardness)

# --------------------------------

# 件数確認

# --------------------------------

hardness_count <- texture_fat_data %>%
  
  count(hardness, sort = TRUE)

print(hardness_count)

# --------------------------------

# 箱ひげ図

# --------------------------------

ggplot(
  texture_fat_data,
  aes(
    x = reorder(hardness, fat_value, median),
    y = fat_value
  )
) +
  
  geom_boxplot() +
  
  coord_flip() +
  
  labs(
    title = "Fat Content Distribution by Cheese Hardness",
    x = "Hardness",
    y = "Fat Content (%)"
  ) +
  
  theme_minimal(base_size = 14)

# --------------------------------

# 平均脂肪分を確認

# --------------------------------

hardness_summary <- texture_fat_data %>%
  
  group_by(hardness) %>%
  
  summarise(
    mean_fat = mean(fat_value),
    median_fat = median(fat_value),
    count = n()
  ) %>%
  
  arrange(desc(mean_fat))

print(hardness_summary)

# --------------------------------

# 平均脂肪分グラフ

# --------------------------------

ggplot(
  hardness_summary,
  aes(
    x = reorder(hardness, mean_fat),
    y = mean_fat
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    title = "Average Fat Content by Cheese Hardness",
    x = "Hardness",
    y = "Average Fat Content (%)"
  ) +
  
  theme_minimal(base_size = 14)

