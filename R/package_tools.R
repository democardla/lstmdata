#' 自动安装并加载依赖包
#'
#' @description
#' 该函数会检查所需的 R 包是否已安装，未安装的会自动下载并安装，然后加载所有包。
#'
#' @export
#' @examples
#' lstmdata.init()
lstmdata.init <- function() {
  # 依赖包列表
  required_packages <- c(
    "broom", "dplyr", "forecast", "foreign", "ggplot2",
    "patchwork", "purrr", "readxl", "scales", "tidyr",
    "tools", "tseries", "utils"
  )

  # 检查未安装的包
  missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

  # 安装缺失的包
  if (length(missing_packages) > 0) {
    message("正在安装缺失的 R 包: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages, dependencies = TRUE)
  }

  # 加载所有包
  invisible(lapply(required_packages, library, character.only = TRUE))

  message("所有 R 包均已加载完成 ✅")
}
