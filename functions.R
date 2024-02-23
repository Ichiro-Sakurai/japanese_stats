if(!require("pacman")){
  install.packages("pacman")
}


pacman::p_load(tidyverse,
               estatapi,
               lubridate,
               openxlsx
               )

appId = "26e8de92a6dfdc808578d9cc40bccc61a989e8ab"

CPI <- function(from = 2001,
                to = 2024,
                sogo = "総合") {
  
  
  meta <- estat_getMetaInfo(appId = appId,
                            statsDataId = "0003427113")
  
  if(sogo == "総合") {
    judai <- c("食料", "住居", "光熱・水道", "家具・家事用品", "被服及び履物", "保健医療", "交通・通信", "教育", "教養娯楽", "諸雑費")
  } else if (sogo == "生鮮食品を除く総合") {
    judai <- c("生鮮食品を除く食料", "住居", "光熱・水道", "家具・家事用品", "被服及び履物", "保健医療", "交通・通信", "教育", "教養娯楽", "諸雑費")
  } 
  
  all <- c(sogo, judai)

  data0 <- estat_getStatsData(
    appId = appId,
    statsDataId = "0003427113",
    cdTimeFrom = as.character(from - 1),
    cdTimeTo = as.character(to),
    cdArea = "00000",
    cdTab = "1",
    cdCat01  = meta$cat01 %>% filter(`@level` == "1") %>% pull(`@code`)
    )
  
  weight <- read.xlsx("https://www.stat.go.jp/data/cpi/2020/zuhyou/rensa-wt_2020.xlsx") %>%
    filter(`類・品目` %in% all) %>%
    select(`類・品目`, `ウエイト`) %>%
    rename(category = `類・品目`, weight = `ウエイト`) %>%
    mutate(weight = as.numeric(weight))
  
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
         x = "",
         y = "前年同期比(%)",
         color = "",
         fill = "",
         caption = "出典：総務省") + 
    guides(color = guide_legend(order = 1), fill = guide_legend(order = 2)) -> p
  
  return(p)
}

