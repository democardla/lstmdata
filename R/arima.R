# usethis::use_package(package = "patchwork", type = "Depends")
# usethis::use_package(package = "patchwork", type = "Imports")


#' 获得模型预测序列
#'
#' @param check T: data.frame[required.col.names <-  c(date: Date)] 传入的验证数据集
#' @param fit T: Arima 传入的模型
#' @param h T: num 向后预测的个数
#' @param forecast T: charactor 指定要被预测的列名
#' @param level T: num 置信区间\%
#'
#' @returns T: data.frame[col.names <- c("date", "actual", "forecast", "lo", "hi")]
#' @export
arima_forecast_with_ci <- function(check, fit, forecast, level = 95) {
  # 提取 date 和 incidence.rate
  temp <- subset.data.frame(check, select = c("date", forecast))

  # 生成预测结果并转换为数据框
  forecast_result <- forecast(fit, h = nrow(check), level = c(level)) %>% as.data.frame()

  # 添加日期信息
  forecast_result$date <- temp$date

  # 调整行名
  rownames(forecast_result) <- c(1:nrow(temp))

  # 进行合并
  temp.1 <- left_join(temp, forecast_result, by = "date")

  # 重新命名列
  colnames(temp.1) <- c("date", "actual", "forecast", "lo", "hi")

  return(temp.1)
}


#' 获得拟合序列
#'
#' @param fit T: Arima 传入的模型
#' @param train train T: data.frame[required.col.names <-  c(date: Date)] 传入的训练数据集
#' @param forecast T: charactor 指定要被预测的列名
#'
#' @returns T: data.frame[col.names <- c("date", "actual", "forecast", "lo", "hi")]
#' @export
arima_fitted_with_actual <- function(fit, train, forecast) {
  # 获取拟合值并转换为数据框
  temp <- fitted(fit) %>% as.data.frame()
  colnames(temp) <- c("forecast")

  # 添加日期列
  temp$date <- train$date

  # 选择实际数据的子集
  temp.1 <- subset.data.frame(train, select = c("date", forecast))
  colnames(temp.1) <- c("date", "actual")
  # 合并数据
  temp <- left_join(temp.1, temp, by = "date")

  # 添加置信区间列，默认填充 NA
  temp$lo <- NA
  temp$hi <- NA

  return(temp)
}


#' 获得拟合与预测序列
#'
#' @param train T: data.frame[required.col.names <-  c(date: Date)] 传入的训练数据集
#' @param check T: data.frame[required.col.names <-  c(date: Date)] 传入的验证数据集
#' @param fit T: Arima 传入的模型
#' @param h T: num 向后预测的个数
#' @param level T: num 置信区间\%
#' @param forecast T: charactor 指定要被预测的列名
#'
#' @return T: data.frame[col.names <- c("date", "actual", "forecast", "lo", "hi")]
#' @export
arima.forecast.data.frame <- function(train, check, fit, forecast, h = 12, level = 95) {
  temp <- arima_fitted_with_actual(fit, train, forecast)
  temp.1 <- arima_forecast_with_ci(check, fit, forecast)
  return(bind_rows(temp, temp.1))
}


#' 获得备选模型的aic值
#'
#' @param model.list T: list<Arima> 备选模型列表
#' @param seasonal T: logical 是否提取季节性（P, D, Q）
#'
#' @returns T: data.frame[col.names <- c("model", "aic", "bic", "p", "d", "q")]
#' @export
#'
arima.aic.bic <- function(model.list, seasonal = TRUE) {
  ret <- data.frame() # 初始化空数据框

  for (name in names(model.list)) { # 遍历列表的名称
    model <- model.list[[name]] # 获取模型对象

    # 提取模型名称和AIC值
    temp <- data.frame(
      model = name, # 使用列表的名称
      aic = AIC(model), # 使用 AIC() 函数获取AIC值
      bic = BIC(model),
      p = model$call$order[[2]],
      d = model$call$order[[3]],
      q = model$call$order[[4]]
    )

    if (seasonal) {
      temp$P <- model$call$seasonal[[2]]
      temp$D <- model$call$seasonal[[3]]
      temp$Q <- model$call$seasonal[[4]]
    }

    # 按行合并数据框
    ret <- bind_rows(ret, temp)
  }

  return(ret)
}

#' 绘制实际数据与拟合&预测数据的对比折线图
#'
#' @param fitted.data.frame T: data.frame[required.col.names <-  c("date", "incidence.rate", "lo", "hi", )]
#' @param title T: charactor 图像的标题
#'
#' @returns T: list<Any> 实际数据与拟合&预测数据的对比折线图元数据列表
#' @export
arima.plot.forecast.comparison <- function(fitted.data.frame, title) {
  ggplot(data = fitted.data.frame, aes(x = date)) +
    # 添加置信区间
    geom_ribbon(aes(ymin = lo, ymax = hi, fill = "Confidence Interval"), alpha = 0.5) +

    # 绘制实际折线
    geom_line(aes(y = actual, color = "original"), size = 0.5) +
    geom_point(aes(y = actual, color = "original"), shape = 1, size = 3) +

    # 绘制预测线
    geom_line(aes(y = forecast, color = "forecast"), size = 0.5, linetype = 5) +
    geom_point(aes(y = forecast, color = "forecast"), shape = 2, size = 3) +

    # 设置颜色并确保图例正常显示
    scale_color_manual(values = c("original" = "black", "forecast" = "red")) +
    scale_fill_manual(values = c("Confidence Interval" = "blue")) + # 设置置信区间填充颜色

    # 图表美化
    labs(
      title = title,
      x = "Date",
      y = "Rate",
      color = "Value",
      fill = "CI"
    ) + # 确保置信区间出现在图例中
    theme_classic() +
    theme(
      legend.title = element_text(face = "bold"),
      legend.position = "right"
    ) # 调整图例位置
}

#' TODO:检查残差
#'
#' @param model.list T: list<Arima> 备选模型列表
#'
#' @returns T: list<Any> 有关于图像原数据的列表
#' @export
my.checkresiduals <- function(model.list) {
  for (model in model.list) {
    checkresiduals(model)
  }
}

#' 拆分样本
#'
#' @param data T: data.frame[required.col.names <- c(date: Date)] 被拆分的数据集，date列必须是Date类型
#' @param cut.point T: Date char 分割点，必须是Date格式的字符串
#' @param percentage T: vector<num, num> 分割比例，代表训练集和验证集的百分点
arima.split.sample <- function(data, cut.point = NULL, percentage = c(80, 20)) {
  n <- nrow(data) # 数据总行数

  if (!is.null(cut.point)) {
    # 通过 cut.point 分割
    train_data <- subset(data, date < cut.point)
    check_data <- subset(data, date >= cut.point)
  } else {
    # 按顺序比例分割
    train_size <- round(n * percentage[1] / sum(percentage)) # 计算训练集大小
    train_data <- data[1:train_size, ] # 取前 train_size 行作为训练集
    check_data <- data[(train_size + 1):n, ] # 其余作为验证集
  }

  return(list(train = train_data, check = check_data))
}

#' 绘制时间序列趋势图（包含均值 ± 2 标准差的置信区间）
#'
#' @description
#' 本函数用于绘制时间序列数据的趋势图，包含：
#' - 原始数据的折线图
#' - 均值 ± 2 标准差的灰色置信区间
#' - 关键时间刻度（默认按年份）
#'
#' @param data T: data.frame[required.col.names <- c(date: Date, observe: num)]
#' 时间序列数据集，`date` 列必须是 `Date` 类型，`observe` 列必须是数值类型
#' @param observe T: char 指定需要观察的列名（必须是 `data` 中的数值列）
#' @param title F: char （默认 `""`）图表标题
#' @param breaks_by F: char （默认 `"year"`）x 轴刻度的间隔，可以是 `"month"`, `"quarter"`, `"year"` 等
#'
#' @returns ggplot 对象，绘制时间序列趋势图
#' @export
#'
#' @examples
#' # 示例数据
#' file.time <- data.frame(
#'   date = seq(as.Date("2020-01-01"), by = "month", length.out = 36),
#'   incidence.rate = rnorm(36, mean = 50, sd = 10)
#' )
#'
#' # 绘制趋势图
#' arima.tendency.chart(file.time, "incidence.rate", title = "Incidence Rate Trend")
arima.tendency.chart <- function(data, observe, title = "", breaks_by = "year") {
  # 计算均值和标准差
  mean_value <- mean(data[[observe]], na.rm = TRUE)
  sd_value <- sd(data[[observe]], na.rm = TRUE)

  # 计算上下限（均值 ± 2 * 标准差）
  upper_bound <- mean_value + 2 * sd_value
  lower_bound <- mean_value - 2 * sd_value

  # 绘制带有置信区间的时间序列图
  p <- ggplot(data, aes(x = date, y = .data[[observe]])) +
    geom_line(color = "black", linewidth = 0.5) +
    geom_point(shape = 1, size = 3, color = "black") + # 添加空心点
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound),
      fill = "grey70", alpha = 0.3
    ) + # 添加置信区间
    scale_x_date(
      breaks = seq(min(data$date), max(data$date), by = breaks_by),
      labels = date_format("%Y") # 只显示年份
    ) +
    labs(title = title, x = "Date", y = observe) +
    theme_classic()

  return(p)
}


#' 绘制 ACF 和 PACF 图
#'
#' @description
#' 本函数用于绘制指定数据列的自相关函数（ACF）和偏自相关函数（PACF）图，
#' 并使用 `patchwork` 进行排版。
#'
#' @param data T: data.frame 包含时间序列数据的数据框
#' @param observe T: char 需要绘制 ACF 和 PACF 的列名（必须是数值型列）
#'
#' @returns ggplot 对象，包含 ACF 和 PACF 图
#' @export
#'
#' @examples
#' # 示例数据
#' file.time <- data.frame(
#'   date = seq(as.Date("2020-01-01"), by = "month", length.out = 36),
#'   incidence.rate = rnorm(36, mean = 50, sd = 10)
#' )
#'
#' # 绘制 ACF 和 PACF 图
#' p <- arima.plot.acf.pacf(file.time, "incidence.rate")
arima.plot.acf.pacf <- function(data, observe) {
  # ACF 图
  acf_plot <- autoplot(Acf(data[[observe]])) +
    ggtitle(paste(char.tools.uppercased(observe), "ACF Plot")) +
    theme_classic()

  # PACF 图
  pacf_plot <- autoplot(Pacf(data[[observe]])) +
    ggtitle(paste(char.tools.uppercased(observe), "PACF Plot")) +
    theme_classic()

  # 使用 patchwork 拼接 ACF 和 PACF 图
  combined_plot <- (acf_plot | pacf_plot) + plot_layout(nrow = 2)

  return(combined_plot)
}

#' 获得ARIMA模型预测评价指标
#' @description
#' 包括 ME, RMSE, MAE, MPE, MAPE, MASE, ACF1 等误差度量用于评价ARIMA模型的性能。
#'
#' @param fit T: arima 模型对象
#' @param check T: data.frame，验证集数据，包含实际数据和预测数据
#' @param train T: data.frame，训练集数据，包含实际数据和预测数据
#' @param forecast T: data.frame，包含拟合的预测结果
#'
#' @returns T: list<matrix> 函数将返回包括 ME, RMSE, MAE, MPE, MAPE, MASE, ACF1 等误差度量的矩阵列表
#' @export
#'
#' @examples
#' error_measures <- arima.prediction.performance(model.list$fit.2, check_data, train_data, forecast_data)
arima.prediction.performance <- function(fit, check, train, forecast) {
  # 获取训练集的拟合值
  train <- arima_fitted_with_actual(fit, train, forecast)

  # 获取验证集的预测结果和置信区间
  check <- arima_forecast_with_ci(check, fit, forecast)

  # 计算训练集的误差度量
  train_error <- accuracy(train$actual, train$forecast)

  # 计算验证集的误差度量
  check_error <- accuracy(check$actual, check$forecast)

  return(list(
    train = train_error, # 返回训练集的误差度量
    check = check_error # 返回验证集的误差度量
  ))
}


#' 提取ARIMA模型系数和标准误
#' @details
#' **报错**
#'
#' `Warning message: In sqrt(diag(x$var.coef)) : NaNs produced` -> 您选择的p,d,q,P,D,Q不合理
#'
#' @param model T: arima 模型对象
#'
#' @returns T: data.frame 返回包含系数及其标准误的表格
#' @export
#'
#' @examples
#' tidy_model <- arima.coef.se(model.list$fit.2)
arima.coef.se <- function(model) {
  # 使用 tidy() 提取 ARIMA 模型的结果
  tidy_model <- tidy(model) %>% stat.confidence.interval()
  return(tidy_model)
}

#' 执行 ARIMA 模型的ADF检验
#' @description
#' 对给定数据列进行 Augmented Dickey-Fuller（ADF）检验，检查数据是否平稳。
#'
#' @param observe T: numeric 向量，需要进行平稳性检验的时间序列数据列
#' @param d T: integer 差分阶数，指定进行几阶差分
#'
#' @returns T: list 返回ADF检验的结果，包括统计量和p值
#' @export
#'
#' @examples
#' adf_results <- arima.stationary.test(file.time.train$diff.incidence.rate, d = 1)
arima.stationary.test <- function(observe, d = 0) {
  diff_data <- observe
  # 对观察值进行差分
  if (d != 0) {
    diff_data <- diff(observe, differences = d)
  }

  # 执行ADF检验
  adf_result <- adf.test(diff_data, k = 1) # 使用k=1选择滞后阶数

  return(adf_result)
}

#' 转换数据框中的非日期列为时间序列类型
#'
#' @description
#' 该函数将数据框中除了 `date` 列之外的所有列转换为时间序列类型 (ts)，并根据给定的频率参数设置时间序列的频率。
#' 结果返回的数据框中，`date` 列保持不变，其他列将被转换为时间序列对象。
#'
#' @param data T: data.frame (S3: data.frame) 包含 `date` 列的数据框。除了 `date` 列外的其他列将被转换为时间序列类型。
#' @param frequency T: numeric (S3: numeric) 时间序列的频率，默认为 12（即每年 12 个观测值）。
#'
#' @returns T: data.frame (S3: data.frame) 返回一个转换后的数据框，其中非 `date` 列被转换为时间序列（ts）类型。
#'
#' @examples
#' # 假设 data 是一个包含 `date` 和其他数值列的数据框
#' result <- arima.ts.data.frame(data, frequency = 12)
#'
arima.ts.data.frame <- function(data, frequency = 12) {
  # 找到数据框中非 `date` 列的索引
  indice <- which(names(data) != "date")

  # 遍历数据框的每一列，将非 `date` 列转换为时间序列
  for (index in 1:length(data)) {
    if (index %in% indice) {
      data[[index]] <- ts(data[[index]], frequency = frequency)
    } else {
      data[[index]] <- as.Date(data[[index]])
    }
  }

  return(data)
}
