% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arima.R
\name{arima.ts.data.frame}
\alias{arima.ts.data.frame}
\title{转换数据框中的非日期列为时间序列类型}
\usage{
arima.ts.data.frame(data, frequency = 12)
}
\arguments{
\item{data}{T: data.frame (S3: data.frame) 包含 `date` 列的数据框。除了 `date` 列外的其他列将被转换为时间序列类型。}

\item{frequency}{T: numeric (S3: numeric) 时间序列的频率，默认为 12（即每年 12 个观测值）。}
}
\value{
T: data.frame (S3: data.frame) 返回一个转换后的数据框，其中非 `date` 列被转换为时间序列（ts）类型。
}
\description{
该函数将数据框中除了 `date` 列之外的所有列转换为时间序列类型 (ts)，并根据给定的频率参数设置时间序列的频率。
结果返回的数据框中，`date` 列保持不变，其他列将被转换为时间序列对象。
}
\examples{
# 假设 data 是一个包含 `date` 和其他数值列的数据框
result <- arima.ts.data.frame(data, frequency = 12)

}
