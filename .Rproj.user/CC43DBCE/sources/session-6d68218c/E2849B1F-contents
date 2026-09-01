library(shiny)
library(bslib)
library(ggplot2)
library(showtext)

font_paths("./")
font_add("noto", "NotoSansJP-VariableFont_wght.ttf")
showtext_auto()

distributions = c(
  "正規分布",
  "一様分布",
  "二項分布",
  "ポアソン分布",
  "幾何分布",
  "超幾何分布",
  "負の二項分布",
  "t分布",
  "コーシー分布",
  "ロジスティック分布",
  "対数正規分布",
  "ベータ分布",
  "カイ二乗分布",
  "F分布",
  "指数分布",
  "ワイブル分布",
  "ガンマ分布"
)

distributions_named = c(
  normal = "正規分布",
  uniform = "一様分布",
  binom = "二項分布",
  poisson = "ポアソン分布",
  goem = "幾何分布",
  hyper = "超幾何分布",
  nbniom = "負の二項分布",
  t = "t分布",
  cauchy = "コーシー分布",
  logistic = "ロジスティック分布",
  lnorm = "対数正規分布",
  beta = "ベータ分布",
  chisq = "カイ二乗分布",
  f = "F分布",
  exp = "指数分布",
  weibull = "ワイブル分布",
  gamma = "ガンマ分布"
)

seq_val <- seq(-100, 100, by = 0.01)

dist_rand_f <- function(dist, n, params){
  if(dist == "正規分布"){
    rnorm(n, mean = params[[1]], sd = params[[2]])
  } else if(dist == "一様分布"){
    runif(n, min = params[[1]], max = params[[2]])
  } else if(dist == "二項分布"){
    rbinom(n, size = as.integer(params[[1]]), prob = params[[2]])
  } else if(dist == "ポアソン分布"){
    rpois(n, lambda = params[[1]])
  } else if(dist == "幾何分布"){
    rgeom(n, prob = params[[1]])
  } else if(dist == "超幾何分布"){
    rhyper(n, m = params[[1]], n = params[[2]], k = params[[3]])
  } else if(dist == "負の二項分布"){
    rnbinom(n, size = params[[1]], prob = params[[2]])
  } else if(dist == "t分布"){
    rt(n, df = params[1], ncp = params[[2]])
  } else if(dist == "コーシー分布"){
    rcauchy(n, location = params[[1]], scale = params[[2]])
  } else if(dist == "ロジスティック分布"){
    rlogis(n, location = params[[1]], scale = params[[2]])
  } else if(dist == "対数正規分布"){
    rlnorm(n, meanlog = params[[1]], sdlog = params[[2]])
  } else if(dist == "ベータ分布"){
    rbeta(n, shape1 = params[[1]], shape2 = params[[2]], ncp = params[[3]])
  } else if(dist == "カイ二乗分布"){
    rchisq(n, df = params[[1]], ncp = params[[2]])
  } else if(dist == "F分布"){
    rf(n, df1 = params[[1]], df2 = params[[2]], ncp = params[[3]])
  } else if(dist == "指数分布"){
    rexp(n, rate = params[[1]])
  } else if(dist == "ワイブル分布"){
    rweibull(n, shape = params[[1]], scale = params[[2]])
  } else if(dist == "ガンマ分布"){
    rgamma(n, shape = params[[1]], rate = params[[2]])
  }
}

rep_matrix_rand <- function(rep_n, dist, n, param){
  if(n == 1){
    replicate(rep_n, matrix(dist_rand_f(dist, n, param), nrow = 1))
  } else {
    replicate(rep_n, dist_rand_f(dist, n, param)) |> t()
  }
}

add_mat_rand <- function(mat, rep_n, dist, n, param){
  rbind(mat, rep_matrix_rand(rep_n, dist, n, param))
}


dist_d_f <- function(dist, seq_v, params){
  if(dist == "正規分布"){
    dnorm(seq_v, mean = params[[1]], sd = params[[2]])
  } else if(dist == "一様分布"){
    dunif(seq_v, min = params[[1]], max = params[[2]])
  } else if(dist == "二項分布"){
    dbinom(seq_v, size = as.integer(params[[1]]), prob = params[[2]]) |> suppressWarnings()
  } else if(dist == "ポアソン分布"){
    dpois(seq_v, lambda = params[[1]]) |> suppressWarnings()
  } else if(dist == "幾何分布"){
    dgeom(seq_v, prob = params[[1]]) |> suppressWarnings()
  } else if(dist == "超幾何分布"){
    dhyper(seq_v, m = params[[1]], n = params[[2]], k = params[[3]]) |> suppressWarnings()
  } else if(dist == "負の二項分布"){
    dnbinom(seq_v, size = params[[1]], prob = params[[2]]) |> suppressWarnings()
  } else if(dist == "t分布"){
    dt(seq_v, df = params[[1]], ncp = params[[2]])
  } else if(dist == "コーシー分布"){
    dcauchy(seq_v, location = params[[1]], scale = params[[2]])
  } else if(dist == "ロジスティック分布"){
    dlogis(seq_v, location = params[[1]], scale = params[[2]])
  } else if(dist == "対数正規分布"){
    dlnorm(seq_v, meanlog = params[[1]], sdlog = params[[2]])
  } else if(dist == "ベータ分布"){
    dbeta(seq_v, shape1 = params[[1]], shape2 = params[[2]], ncp = params[[3]])
  } else if(dist == "カイ二乗分布"){
    dchisq(seq_v, df = params[[1]], ncp = params[[2]])
  } else if(dist == "F分布"){
    df(seq_v[seq_v > 0] , df1 = params[[1]], df2 = params[[2]], ncp = params[[3]])
  } else if(dist == "指数分布"){
    dexp(seq_v, rate = params[[1]])
  } else if(dist == "ワイブル分布"){
    dweibull(seq_v, shape = params[[1]], scale = params[[2]])
  } else if(dist == "ガンマ分布"){
    dgamma(seq_v, shape = params[[1]], rate = params[[2]])
  }
}

dist_p_f <- function(dist, seq_v, params){
  if(dist == "正規分布"){
    pnorm(seq_v, mean = params[[1]], sd = params[[2]])
  } else if(dist == "一様分布"){
    punif(seq_v, min = params[[1]], max = params[[2]])
  } else if(dist == "二項分布"){
    pbinom(seq_v, size = as.integer(params[[1]]), prob = params[[2]]) |> suppressWarnings()
  } else if(dist == "ポアソン分布"){
    ppois(seq_v, lambda = params[[1]]) |> suppressWarnings()
  } else if(dist == "幾何分布"){
    pgeom(seq_v, prob = params[[1]]) |> suppressWarnings()
  } else if(dist == "超幾何分布"){
    phyper(seq_v, m = params[[1]], n = params[[2]], k = params[[3]]) |> suppressWarnings()
  } else if(dist == "負の二項分布"){
    pnbinom(seq_v, size = params[[1]], prob = params[[2]]) |> suppressWarnings()
  } else if(dist == "t分布"){
    pt(seq_v, df = params[[1]], ncp = params[[2]])
  } else if(dist == "コーシー分布"){
    pcauchy(seq_v, location = params[[1]], scale = params[[2]])
  } else if(dist == "ロジスティック分布"){
    plogis(seq_v, location = params[[1]], scale = params[[2]])
  } else if(dist == "対数正規分布"){
    plnorm(seq_v, meanlog = params[[1]], sdlog = params[[2]])
  } else if(dist == "ベータ分布"){
    pbeta(seq_v, shape1 = params[[1]], shape2 = params[[2]], ncp = params[[3]])
  } else if(dist == "カイ二乗分布"){
    pchisq(seq_v, df = params[[1]], ncp = params[[2]])
  } else if(dist == "F分布"){
    pf(seq_v[seq_v > 0] , df1 = params[[1]], df2 = params[[2]], ncp = params[[3]])
  } else if(dist == "指数分布"){
    pexp(seq_v, rate = params[[1]])
  } else if(dist == "ワイブル分布"){
    pweibull(seq_v, shape = params[[1]], scale = params[[2]])
  } else if(dist == "ガンマ分布"){
    pgamma(seq_v, shape = params[[1]], rate = params[[2]])
  }
}

plot_density <- function(seq_val, density_val){
  temp <- cbind(seq_val, density_val) |> data.frame()
  subset(temp, density_val > 10^(-7)) |> 
    ggplot(aes(x = seq_val, ymax = density_val, ymin = 0)) + 
    geom_ribbon(fill = "#E95420", color = "#E95420") +
    labs(x = NULL, y = "確率密度") +
    theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 20))
}

plot_cumdist <- function(seq_val, density_val){
  temp <- cbind(seq_val, density_val) |> data.frame()
  subset(temp, density_val > 10^(-5) & density_val < 1 - 10^(-5)) |> 
    ggplot(aes(x = seq_val, ymax = density_val, ymin = 0)) + 
    geom_ribbon(fill = "#E95420", color = "#E95420") +
    labs(x = NULL, y = "累積確率密度") +
    theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 20))
}

dist_explanation <- function(dist){
  if(dist == "正規分布"){
    "正規分布は、様々な分布での値の平均値の分布を反映したもので（中心極限定理）、統計では最も一般的に用いられているものです。正規分布のパラメータは平均値（μ）と標準偏差（σ）です。正規分布は-無限大～無限大の範囲を持つ連続値を取ります。"
  } else if(dist == "一様分布"){
    "一様分布は、最小値（a）から最大値（b）まで、一定の確率で現れる現象を示す分布です。サイコロの出目が一様分布の典型的な例です。一様分布のパラメータは最小値aと最大値bの2つです。"
  } else if(dist == "二項分布"){
    "二項分布はベルヌーイ試行（コイントスのように、1と0だけが確率的に結果として得られる試行）の試行回数（size）と成功確率（prob、コイントスであれば表を成功として、表が出る確率）から、成功回数xが得られる確率を示したものです。コイントスですので、整数でない成功回数（4.21回成功など）が起こることはありませんし、マイナスの試行数も取りません。ですので、二項分布は成功回数xが正の整数のみの離散的な確率分布になります。分布の範囲は0～試行回数です。"
  } else if(dist == "ポアソン分布"){
    "ポアソン分布は、起こる確率が非常に低い現象が、ある一定の時間に何回起こるかを確率として表した確率分布です。ポアソン分布の確率質量関数は二項分布の確率を0に、試行回数をに近づけた極限として計算されます。ポアソン分布のパラメータはλのみで、ポアソン分布は平均値も標準偏差もλとなる特徴があります。分布が取りうる範囲は0～無限大となります。"
  } else if(dist == "幾何分布"){
    "幾何分布は、ベルヌーイ試行を始めて成功させたときの試行数がxとなる確率を指します。二項分布との違いは、成功回数が必ず1回であるところになります。x以外には、成功確率（prob）のみをパラメータとして設定します。分布の範囲は0～無限大です。試行数xは必ず整数となるため、幾何分布も二項分布と同じく、離散的な分布で、xは正の整数を取ります。"
  } else if(dist == "超幾何分布"){
    "超幾何分布は、ツボの中に白と黒のボールが入っていて、このツボからボールを取り出す試行（非復元抽出）を表現した確率分布です。Rのhyper関数では、白ボールm個、黒ボールn個が入ったツボからk個ボールを取ったとき、x個の白ボールが取れる確率を計算します。xは整数のみを取る離散的な分布です。分布の範囲は0～kとなります。"
  } else if(dist == "負の二項分布"){
    "負の二項分布は、ベルヌーイ試行を行ったとき、n回成功する間にx回失敗する確率を反映した確率分布です。負の二項分布は正の整数のみを取る離散的な分布で、その範囲は0～無限大となります。"
  } else if(dist == "t分布"){
    "t分布は標本サイズが小さいときの母平均と標本平均の差の分布を反映した確率分布です。t分布はt統計量と呼ばれるものの分布を示します。t分布は-無限大～無限大の範囲を持つ連続値を取り、平均値は0です。t分布は正規分布とよく似た釣鐘型の分布ですが、正規分布と比較して裾が重い、0から遠い値の確率が大きくなる性質を持ちます。自由度dfが無限大に近づくと、t分布の形は正規分布と一致します。"
  } else if(dist == "コーシー分布"){
    "コーシー分布はt分布で自由度が1のときの分布です。コーシー分布はt分布よりもさらに裾が重く、平均値も標準偏差も定義できない分布です。コーシー分布のパラメータは中央値であるtとスケールパラメータであるsの2つです。コーシー分布も正規分布やt分布と同じく、-無限大～無限大の範囲を持つ連続値を取ります。"
  } else if(dist == "ロジスティック分布"){
    "ロジスティック分布はロジスティック関数を微分したものを分布としたもの、つまり、ロジスティック分布の積分である累積分布関数がロジスティック関数を取る分布のことを指します。ロジスティック分布は-無限大～無限大の範囲を持つ連続値を取ります。"
  } else if(dist == "対数正規分布"){
    "対数正規分布は正規分布を指数変換した分布です。年収がこの分布に従うことで有名です。パラメータは正規分布と同じくμとσですが、μとσは分布の平均値・標準偏差ではありません。対数正規分布は左に歪み、右側の裾が長い分布を示し、0～無限大の範囲を持つ連続値を取ります。"
  } else if(dist == "ベータ分布"){
    "ベータ分布は、ベルヌーイ試行などの成功確率をモデル化する際に用いられる分布です。ベータ分布の確率密度関数には2つのパラメータがあり、それぞれα、βと呼ばれます。どちらも形状パラメータを意味しています。ベータ分布は0～1までの連続値を取ります。"
  } else if(dist == "カイ二乗分布"){
    "カイ二乗分布は正規分布に従う確率変数の二乗和の確率分布です。カイ二乗分布は度数の検定に用いられる分布で、そのパラメータは自由度νのみです。カイ二乗分布は右側の裾が長い分布を示し、0～無限大の範囲を持つ連続値を取ります。"
  } else if(dist == "F分布"){
    "F分布はカイ二乗に従う2つの独立な変数の比の分布です。F分布は分散分析で用いられる分布です。F分布のパラメータは独立の2変数それぞれの自由度（ν1とν2）です。F分布は右側の裾が長い分布を示し、0～無限大の範囲を持つ連続値を取ります。"
  } else if(dist == "指数分布"){
    "指数分布はポアソン分布する事象（まれに起こる事象）が起こる時間間隔の分布を示す確率分布です。指数分布は生存時間解析などの、イベントの発生数と時間の関係を解析するのに用いられています。指数分布のパラメータはλのみで、λは正の値を取ります。指数分布は単調減少で、0～無限大の範囲を持つ連続値を取ります。"
  } else if(dist == "ワイブル分布"){
    "ワイブル分布は指数分布を拡張したような確率分布です。指数分布では、ポアソン分布する事象（イベント）が起きる確率は常に一定であることを仮定していますが、ワイブル分布を用いると単調増加・単調減少でイベントが起きる確率が変化する場合を表現することができます。ワイブル分布のパラメータは形状パラメータのλとスケールパラメータのkの2つです。ワイブル分布はガンマ分布と同じく、右側の裾が長い分布で、最頻値が0以上となる形状を示し、0～無限大の範囲を持つ連続値を取ります。"
  } else if(dist == "ガンマ分布"){
    "ガンマ分布は機械の信頼性や降雨量などの説明に用いられる分布です。ガンマ分布のパラメータは形状パラメータγとスケールパラメータβの2つです。ガンマ分布は右側の裾が長い分布で、最頻値が0以上となる形状を示し、0～無限大の範囲を持つ連続値を取ります。"
  }
}

dist_function <- function(dist){
  if(dist == "正規分布"){
    "$$ Norm(x, \\mu, \\sigma)=\\frac{1}{\\sqrt{2\\pi \\sigma^2}} \\cdot \\exp \\left( \\frac{(x-\\mu)^2}{2\\sigma^2} \\right) $$"
  } else if(dist == "一様分布"){
    "$$Uniform(x, a, b)=\\frac{1}{b-a}$$"
  } else if(dist == "二項分布"){
    "$$Binom(x,n,p)=_{n}C_{x} \\cdot p^{x} \\cdot (1-p)^{n-x}$$"
  } else if(dist == "ポアソン分布"){
    "$$Poisson(x, \\lambda)=\\frac{\\lambda^x \\cdot \\exp(-\\lambda)}{x!}$$"
  } else if(dist == "幾何分布"){
    "$$Geom(x,p)=p  \\cdot (1-p)^{x-1}$$"
  } else if(dist == "超幾何分布"){
    "$$ Hyper(x,m,n,k)= \\frac{_{m}C_{k} \\cdot _{n}C_{x-k}}{_{m+n}C_{x}}$$"
  } else if(dist == "負の二項分布"){
    "$$Nbinom(x,n,p)=_{x+n+1}C_{x} \\cdot p^x(1-p)^n$$"
  } else if(dist == "t分布"){
    "$$t=\\frac{x-\\mu}{s/ \\sqrt n}$$"
  } else if(dist == "コーシー分布"){
    "$$Cauchy(x, t, s)=\\frac{1}{s \\cdot \\pi (1+((x-t)/s)^2)} $$"
  } else if(dist == "ロジスティック分布"){
    "$$logis(x)=\\frac{1}{(1+\\exp(-(ax+b))}$$"
  } else if(dist == "対数正規分布"){
    "$$Lognorm(x, \\mu, \\sigma)=\\frac{1}{x\\sigma\\sqrt{2\\pi}} \\cdot \\exp(-\\frac{(\\ln x-\\mu)^2}{2\\sigma^2})$$"
  } else if(dist == "ベータ分布"){
    "$$Beta(x, \\alpha, \\beta)=\\frac{\\Gamma(\\alpha + \\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)} \\cdot x^{\\alpha-1} \\cdot (1-x)^{\\beta-1}$$"
  } else if(dist == "カイ二乗分布"){
    "$$Chisq(x, \\nu)=\\frac{\\exp(-x/2) \\cdot \\exp(\\nu/2-1)}{2^{\\nu/2} \\cdot \\Gamma(\\nu/2)}$$"
  } else if(dist == "F分布"){
    "$$F(x, \\nu_{1}, \\nu_{2})=
      \\frac{\\Gamma(\\frac{\\nu_{1}+\\nu_{2}}{2}) \\cdot (\\frac{\\nu_{1}}{\\nu_{2}})^{\\nu_{1}/2 \\cdot x^{\\nu_{1}/2-1}}}
      {\\Gamma(\\frac{\\nu_{1}}{2}) \\cdot \\Gamma(\\frac{\\nu_{2}}{2}) \\cdot (1+\\frac{\\nu_{1}x}{\\nu_{2}})^{\\frac{\\nu_{1}+\\nu_{2}}{2}}}$$"
  } else if(dist == "指数分布"){
    "$$Exp(x, \\lambda)=\\lambda \\cdot \\exp(-\\lambda x)$$"
  } else if(dist == "ワイブル分布"){
    "$$Weibull(x, \\lambda, k)=\\frac{k}{\\lambda}(\\frac{x}{\\lambda})^{k-1} \\cdot \\exp(-\\frac{x}{\\lambda})^k$$"
  } else if(dist == "ガンマ分布"){
    "$$Gamma(x, \\gamma, \\beta)=\\frac{(\\frac{x}{\\beta})^{\\gamma-1} \\cdot \\exp(-\\frac{x}{\\beta})}{\\beta \\cdot \\Gamma(\\gamma)}$$"
  }
}
