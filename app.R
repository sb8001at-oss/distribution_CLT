source("global.R")

ui <- page_sidebar(
  title = "確率密度と中央極限定理",
  theme = bs_theme(bootswatch = "united"),
  sidebar = sidebar(
    selectInput("dist_selected", label="分布", choices = distributions, selected = "正規分布"),
    card(
      card_header("パラメータの入力"),
      navset_hidden(
        id = "switcher",
        nav_panel(
          title = "正規分布", 
          sliderInput("norm_param1", "平均値", min = -10, max = 10, value = 0, step = 0.1),
          sliderInput("norm_param2", "標準偏差", min = 0.1, max = 10, value = 1, step = 0.1)
        ),
        nav_panel(
          title = "一様分布", 
          sliderInput("unif_param1", "最小値", min = -10, max = 0, value = -5, step = 0.1),
          sliderInput("unif_param2", "最大値", min = 0, max = 10, value = 5, step = 0.1)
        ),
        nav_panel(
          title = "二項分布", 
          sliderInput("binom_param1", "試行数", min = 1, max = 50, value = 20, step = 1),
          sliderInput("binom_param2", "成功確率", min = 0, max = 1, value = 0.5, step = 0.01)
        ),
        nav_panel(
          title = "ポアソン分布", 
          sliderInput("pois_param1", "平均値", min = 0.1, max = 20, value = 5, step = 0.1)
        ),
        nav_panel(
          title = "幾何分布", 
          sliderInput("geom_param1", "成功確率", min = 0, max = 1, value = 0.5, step = 0.01)
        ),
        nav_panel(
          title = "超幾何分布", 
          sliderInput("hyper_param1", "白玉の数", min = 1, max = 20, value = 5, step = 1),
          sliderInput("hyper_param2", "黒玉の数", min = 1, max = 20, value = 5, step = 1),
          sliderInput("hyper_param3", "取り出す玉の数", min = 0, max = 10, value = 5, step = 1)
        ),
        nav_panel(
          title = "負の二項分布", 
          sliderInput("nbinom_param1", "試行数", min = 1, max = 20, value = 5, step = 1),
          sliderInput("nbinom_param2", "成功確率", min = 0.1, max = 1, value = 0.5, step = 0.01)
        ),
        nav_panel(
          title = "t分布", 
          sliderInput("t_param1", "自由度", min = 1, max = 20, value = 5, step = 1),
          sliderInput("t_param2", "非心率", min = -5, max = 5, value = 0, step = 0.1)
        ),
        nav_panel(
          title = "コーシー分布", 
          sliderInput("cauchy_param1", "中央値", min = -50, max = 50, value = 0, step = 1),
          sliderInput("cauchy_param2", "スケール", min = 0.1, max = 50, value = 1, step = 0.1)
        ),
        nav_panel(
          title = "ロジスティック分布", 
          sliderInput("logi_param1", "中央値", min = -50, max = 50, value = 0, step = 1),
          sliderInput("logi_param2", "スケール", min = -5, max = 5, value = 1, step = 0.1)
        ),
        nav_panel(
          title = "対数正規分布", 
          sliderInput("lnorm_param1", "平均値", min = -10, max = 10, value = 0, step = 0.1),
          sliderInput("lnorm_param2", "標準偏差", min = 0.01, max = 10, value = 1, step = 0.01)
        ),
        nav_panel(
          title = "ベータ分布", 
          sliderInput("beta_param1", "Shape1", min = 0.1, max = 10, value = 1, step = 0.1),
          sliderInput("beta_param2", "Shape2", min = 0.1, max = 10, value = 1, step = 0.1),
          sliderInput("beta_param3", "非心度", min = 0, max = 5, value = 0, step = 0.1)
        ),
        nav_panel(
          title = "カイ二乗分布", 
          sliderInput("chisq_param1", "自由度", min = 1, max = 50, value = 5, step = 0.1),
          sliderInput("chisq_param2", "非心度", min = -5, max = 5, value = 0, step = 0.1)
        ),
        nav_panel(
          title = "F分布", 
          sliderInput("f_param1", "自由度1", min = 1, max = 50, value = 5, step = 1),
          sliderInput("f_param2", "自由度2", min = 1, max = 50, value = 5, step = 1),
          sliderInput("f_param3", "非心度", min = 0, max = 5, value = 0, step = 0.1)
        ),
        nav_panel(
          title = "指数分布", 
          sliderInput("exp_param1", "rate", min = 0.001, max = 4, value = 1, step = 0.001)
        ),
        nav_panel(
          title = "ワイブル分布", 
          sliderInput("weibull_param1", "shape", min = 0.01, max = 10, value = 1, step = 0.01),
          sliderInput("weibull_param2", "scale", min = 0.1, max = 10, value = 1, step = 0.1)
        ),
        nav_panel(
          title = "ガンマ分布", 
          sliderInput("gamma_param1", "shape", min = 0.01, max = 10, value = 1, step = 0.01),
          sliderInput("gamma_param2", "rate", min = 0, max = 10, value = 1, step = 0.1)
        )
      )
    ),
    card(
      card_header("生成する乱数の個数"),
      sliderInput("rand_number", "個数", min = 2, max = 10, value = 3, step = 1)
    ),
    card(
      card_header("乱数の個数の平方根"), 
      textOutput("sq_rand_number")
    ),
    card(
      card_header("乱数の標準偏差/乱数の平均値の標準偏差"), 
      textOutput("ratio_sd"),
      div(p("＊この値は乱数の生成回数が増えると上の乱数の個数の平方根に近づきます。標準誤差に関係のある値です。"), style = "font-size: 0.5rem;")
    )
  ),
  navset_tab(
    nav_panel(
      title = "確率密度",
      layout_columns(
        card(
          card_header("確率密度"),
          plotOutput("plotdensity")
        ),
        card(
          card_header("累積確率密度"),
          plotOutput("plot_cum_dist")
        )
      ),
      card(
        card_header("確率分布の説明"),
        div(textOutput("name_dist"), style = "font-size: 2rem; font-weight: bold;"),
        textOutput("info_dist"),
        uiOutput("dist_fun")
      )
    ),
    nav_panel(
      title = "乱数と平均値の分布",
      layout_columns(
        card(
          card_header("乱数の分布"),
          plotOutput("plot_rand_trial"),
          div(p("＊色は乱数のセットの回ごとに設定。"), style = "font-size: 0.5rem;")
        ),
        card(
          card_header("乱数の平均値の分布"),
          plotOutput("plot_mean_trial"),
          div(p("＊乱数の生成回数が増えると、どのような分布であっても中央極限定理のため正規分布に近づく。"), style = "font-size: 0.5rem;")
        )
      ),
      card(
        card_header("乱数を追加する"),
        layout_columns(
          actionButton("add_1_trial", "1回追加"),
          actionButton("add_10_trial", "10回追加"),
          actionButton("add_100_trial", "100回追加")
        )
      ),
      layout_columns(
        value_box("乱数のセットの数", textOutput("rand_nrow"), theme = value_box_theme("primary")),
        value_box("乱数の平均値・標準偏差", div(textOutput("rand_ms"), style = "font-size: 1.5rem;"), theme = value_box_theme("teal")),
        value_box("乱数の平均値の平均・標準偏差", div(textOutput("rand_mean_ms"), style = "font-size: 1.5rem;"), theme = value_box_theme("pink"))
      )
    )
  )
)


  
  
server <- function(input, output) {
  
  param_l <- reactiveVal(list(0, 1))
  
  observe(
    param_l({
      if(input$dist_selected == "正規分布"){
        list(input$norm_param1, input$norm_param2)
      } else if(input$dist_selected == "一様分布"){
        list(input$unif_param1, input$unif_param2)
      } else if(input$dist_selected == "二項分布"){
        list(input$binom_param1, input$binom_param2)
      } else if(input$dist_selected == "ポアソン分布"){
        list(input$pois_param1)
      } else if(input$dist_selected == "幾何分布"){
        list(input$geom_param1)
      } else if(input$dist_selected == "超幾何分布"){
        list(input$hyper_param1, input$hyper_param2, input$hyper_param3)
      } else if(input$dist_selected == "負の二項分布"){
        list(input$nbinom_param1, input$nbinom_param2)
      } else if(input$dist_selected == "t分布"){
        list(input$t_param1, input$t_param2)
      } else if(input$dist_selected == "コーシー分布"){
        list(input$cauchy_param1, input$cauchy_param2)
      } else if(input$dist_selected == "ロジスティック分布"){
        list(input$logi_param1, input$logi_param2)
      } else if(input$dist_selected == "対数正規分布"){
        list(input$lnorm_param1, input$lnorm_param2)
      } else if(input$dist_selected == "ベータ分布"){
        list(input$beta_param1, input$beta_param2, input$beta_param3)
      } else if(input$dist_selected == "カイ二乗分布"){
        list(input$chisq_param1, input$chisq_param2)
      } else if(input$dist_selected == "F分布"){
        list(input$f_param1, input$f_param2, input$f_param3)
      } else if(input$dist_selected == "指数分布"){
        list(input$exp_param1)
      } else if(input$dist_selected == "ワイブル分布"){
        list(input$weibull_param1, input$weibull_param2)
      } else if(input$dist_selected == "ガンマ分布"){
        list(input$gamma_param1, input$gamma_param2)
      }
    })
  )
  
  observeEvent(input$dist_selected, {
    nav_select(id = "switcher", selected = input$dist_selected)
    if(input$dist_selected == "超幾何分布"){
      updateSliderInput(inputId = "hyper_param3", min = 0, max = input$hyper_param1 + input$hyper_param2, value = round(input$hyper_param1 + input$hyper_param2)/2, step = 1)
    }
  })
  
  observeEvent(input$hyper_param1, {
    updateSliderInput(inputId = "hyper_param3", min = 0, max = input$hyper_param1 + input$hyper_param2, value = round(input$hyper_param1 + input$hyper_param2)/2, step = 1)
  })

  observeEvent(input$hyper_param2, {
    updateSliderInput(inputId = "hyper_param3", min = 0, max = input$hyper_param1 + input$hyper_param2, value = round(input$hyper_param1 + input$hyper_param2)/2, step = 1)
  })

  output$plotdensity <- renderPlot({
    temp_density <- dist_d_f(input$dist_selected, seq_val, param_l())
    if(input$dist_selected == "F分布"){
      plot_density(seq_val[seq_val > 0], temp_density)
    } else {
      plot_density(seq_val, temp_density)
    }
  })
  
  output$plot_cum_dist <- renderPlot({
    temp_cumdist <- dist_p_f(input$dist_selected, seq_val, param_l())
    if(input$dist_selected == "F分布"){
      plot_cumdist(seq_val[seq_val > 0], temp_cumdist)
    } else {
      plot_cumdist(seq_val, temp_cumdist)
    }
  })
  
  rand_mat <- reactiveVal(NULL)
  
  observeEvent(input$dist_selected, {
    rand_mat(NULL)
  })
  
  observeEvent(input$rand_number, {
    rand_mat(NULL)
  })
  
  observeEvent(input$add_1_trial, {
    rand_mat(
      add_mat_rand(rand_mat(), 1, input$dist_selected, input$rand_number, param_l())
    )
  })
  
  output$plot_rand_trial <- renderPlot({
    if(is.null(rand_mat())){return(NULL)}
    data.frame(exp = factor(1:nrow(rand_mat())), value = rand_mat() |> as.vector()) |> 
      ggplot(aes(x=value, color = exp, fill = exp)) +
      geom_histogram(bins = 35)+
      theme(legend.position = "none")+
      labs(x = NULL, y = "頻度") +
      theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 20))
  })
  
  output$plot_mean_trial <- renderPlot({
    if(is.null(rand_mat())){return(NULL)}
    data.frame(value = apply(rand_mat(), 1, mean)) |> 
      ggplot(aes(x = value)) +
      geom_histogram(fill = "#E95420", color = "#E95420", bins = 35)+
      theme(legend.position = "none")+
      labs(x = NULL, y = "頻度") +
      theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 20))
  })
  
  
  observeEvent(input$add_10_trial, {
    rand_mat(
      add_mat_rand(rand_mat(), 10, input$dist_selected, input$rand_number, param_l())
    )
  })
  
  observeEvent(input$add_100_trial, {
    rand_mat(
      add_mat_rand(rand_mat(), 100, input$dist_selected, input$rand_number, param_l())
    )
  })
  
  output$rand_nrow <- renderText(paste0(input$rand_number, "個×", nrow(rand_mat()), "回"))
  
  output$rand_ms <- renderText({
    if(is.null(rand_mat())){return(NULL)}
    paste0(
      "平均：", 
      rand_mat() |> as.vector() |> mean() |> round(2), 
      ", 標準偏差：",
      rand_mat() |> as.vector() |> sd() |> round(2)
    )
  })
  
  output$rand_mean_ms <- renderText({
    if(is.null(rand_mat())){return(NULL)}
    paste0(
      "平均：", 
      rand_mat() |> apply(1, mean) |> mean() |> round(2), 
      ", 標準偏差：",
      rand_mat() |> apply(1, mean) |> sd() |> round(2)
    )
  })
  
  output$sq_rand_number <- renderText(input$rand_number ^ 0.5 |> round(2))
  
  output$ratio_sd <- renderText({
    if(is.null(rand_mat())){return(NULL)}
    (rand_mat() |> as.vector() |> sd() / rand_mat() |> apply(1, mean) |> sd()) |> round(2)
    })
  
  output$name_dist <- renderText(input$dist_selected)
  output$info_dist <- renderText(input$dist_selected |> dist_explanation())
  output$dist_fun <- renderUI(input$dist_selected |> dist_function() |> HTML() |> withMathJax())
}

shinyApp(ui = ui, server = server)
