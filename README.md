# japanese_stats

## Use app in shinyapps.io

~~Please visit https://s9x60f-ichiro-sakurai.shinyapps.io/japanese_stats/~~

-> It does not work, because the application load the all data before the begining of the session to avoid load data for every time, and it seems to be hard for the server.



## Use app in your local R environment

Please run the following code in your local R environment.

以下のコードをRstudio等で実行してください。

```
library(shiny)
runGitHub(repo = "japanese_stats", username = "Ichiro-Sakurai")
```

## 注意

このサービスは、政府統計総合窓口(e-Stat)のAPI機能を使用していますが、サービスの内容は国によって保証されたものではありません。

