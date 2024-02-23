if(!require("pacman")){
  install.packages("pacman")
}


pacman::p_load(tidyverse,
               estatapi,
               lubridate,
               openxlsx,
               ggthemes
               )

appId = "26e8de92a6dfdc808578d9cc40bccc61a989e8ab"

getdata <- function() {
  DATA <- list()
  
  ### CPI ###
  meta <- estat_getMetaInfo(appId = appId,
                            statsDataId = "0003427113")
  
  DATA[["CPI"]] <- estat_getStatsData(
    appId = appId,
    statsDataId = "0003427113",
    cdTimeFrom = as.character(from - 1),
    cdTimeTo = as.character(to),
    cdArea = "00000",
    cdTab = "1",
    cdCat01  = meta$cat01 %>% filter(`@level` == "1") %>% pull(`@code`)
  )
  
  # ウェイトの取得
  DATA[["CPI_weight_zai_service"]] <- read.csv("https://www.e-stat.go.jp/stat-search/file-download?statInfId=000032103845&fileKind=1",
                                    fileEncoding = "shift-jis",
                                    header = FALSE) %>%
    .[c(1,5), -1] %>%
    t() %>%
    as_tibble()
  
  DATA[["CPI_weight"]] <- read.csv("https://www.e-stat.go.jp/stat-search/file-download?statInfId=000032103842&fileKind=1",
                                   fileEncoding = "shift-jis",
                                   header = FALSE) %>%
    .[c(1,5), -1] %>%
    t() %>%
    as_tibble()
  
  ### GDP gap ###
  
  DATA[["gap"]] <- read.xlsx("https://www.boj.or.jp/research/research_data/gap/gap.xlsx",
                              startRow=5,
                              cols = 1:4)
  return(DATA)
}

CPI <- function(from = 2001,
                to = 2024,
                sogo = "総合") {
  
  data0 <- DATA[["CPI"]]
  
  zai_service <- FALSE
  
  if(sogo == "総合") {
    judai <- c("食料", "住居", "光熱・水道", "家具・家事用品", "被服及び履物", "保健医療", "交通・通信", "教育", "教養娯楽", "諸雑費")
  } else if (sogo == "生鮮食品を除く総合") {
    judai <- c("生鮮食品を除く食料", "住居", "光熱・水道", "家具・家事用品", "被服及び履物", "保健医療", "交通・通信", "教育", "教養娯楽", "諸雑費")
  } else if (sogo == "生鮮食品を除く総合(財・サービス)") {
    judai <- c("生鮮食品を除く財", "サービス")
    sogo <- c("生鮮食品を除く総合")
    zai_service <- TRUE
  }
  
  all <- c(sogo, judai)

  weight <- if (zai_service) {
    DATA[["CPI_weight_zai_service"]]
  } else {
    DATA[["CPI_weight"]]
  }
  colnames(weight) <- c("category", "weight")
  weight$weight <- as.numeric(weight$weight)
  
  if(!("生鮮食品を除く総合" %in% weight$category)) {
    A <- weight %>% filter(category == "生鮮食品を除く財") %>% pull(weight)
    B <- weight %>% filter(category == "サービス") %>% pull(weight)
    
    weight <- bind_rows(weight,
                        tibble(category = "生鮮食品を除く総合",
                               weight = A + B))
  }
  
  
  # データの整形
  data <- data0 %>%
    filter(cat01_code != "0201") %>% #総合が２回出てくるため
    filter(str_sub(time_code, -2, -1) != "00") %>%
    mutate(year = str_sub(time_code, 1, 4),
           month = str_sub(time_code, -2, -1),
           time = ymd(paste0(year, "-", month, "-01")),
           category = str_split(`2020年基準品目`, pattern = " ") %>% map_chr(2)) %>%
    filter(category %in% all) %>%
    select(time, category, value) %>%
    group_by(category) %>%
    mutate(lag_value = dplyr::lead(value, 12)) %>%
    ungroup() %>%
    filter(between(time, as.Date(paste0(from, "-01-01")), as.Date(paste0(to, "-01-01")))) %>%
    left_join(weight, by = "category")
    
  data_sogo <- data %>%
    filter(category == sogo) 
  
  data2 <- data %>%
    left_join(data_sogo, by = "time", suffix = c("", "_sogo")) %>%
    mutate(kiyodo = (value - lag_value)/lag_value_sogo * weight/weight_sogo * 100,
           category = factor(category, levels = all))
  
  ggplot(mapping=aes()) + 
    geom_bar(data=subset(data2, category != sogo), mapping=aes(x = time, y = kiyodo, fill=category), stat="identity") + 
    geom_line(data=subset(data2, category == sogo), mappin = aes(x = time, y = kiyodo, color = category)) + 
    scale_color_manual(values = c("black")) +
    labs(title = "消費者物価指数（全国）",
         x = "（年/月）",
         y = "前年同期比(%)",
         color = "",
         fill = "",
         caption = "出典：総務省") + 
    guides(color = guide_legend(order = 1), fill = guide_legend(order = 2)) -> p

  return(p)
}

gap <- function(from = 2001,
                to =2024) {
  
  data0 <- DATA[["gap"]]
  
  colnames(data0) <- c("time0","需給ギャップ", "資本投入ギャップ", "労働投入ギャップ")
  
  data <- data0 %>%
    mutate(year = str_sub(time0, 1, 4),
           month = case_when(str_ends(time0, "1Q") ~ "-01-01",
                             str_ends(time0, "2Q") ~ "-04-01",
                             str_ends(time0, "3Q") ~ "-07-01",
                             str_ends(time0, "4Q") ~ "-10-01"),
           time = paste0(year, month) %>% ymd()) %>%
    select(-time0, -year, -month) %>%
    drop_na() %>%
    pivot_longer(-time) %>%
    filter(time >= as.Date(paste0(from, "-01-01")) & time <= as.Date(paste0(to, "-01-01")))
  
  ggplot() + 
    geom_bar(data=subset(data, name != "需給ギャップ"), mapping=aes(x = time, y = value, fill=name), stat="identity") + 
    geom_line(data=subset(data, name == "需給ギャップ"), mapping=aes(x = time, y = value, color = name)) + 
    scale_color_manual(values = c("black")) +
    labs(title = "需給ギャップ",
         x = "（年/四半期）",
         y = "(%)",
         color = "",
         fill = "",
         caption = "出典：日本銀行") + 
    guides(color = guide_legend(order = 1), fill = guide_legend(order = 2)) -> p
  
  return(p)
    
    
}
