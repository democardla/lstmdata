#' 点数据地图可视化
#'
#' @param shp 底图数据（sf对象）
#' @param sample 点数据（包含longitude和latitude列的数据框）
#' @param point_color 点颜色（默认"red"）
#' @param point_size 点大小（默认1）
#' @param point_alpha 点透明度（默认0.8）
#' @param scale_position 比例尺位置（默认"bl" - 左下角）
#' @param arrow_position 指北针位置（默认"tr" - 右上角）
#' @param title 地图标题（可选）
#' @param subtitle 地图副标题（可选）
#' @param caption 地图说明（可选）
#'
#' @return ggplot对象
#'
#' @examples
#' # 基本使用
#' map_point(shp = shanxi_shp, sample = points_df)
#'
#' # 自定义样式
#' map_point(
#'   shp = shanxi_shp,
#'   sample = points_df,
#'   point_color = "blue",
#'   point_size = 2,
#'   title = "山西省采样点分布",
#'   subtitle = "2023年调查数据"
#' )
map_point <- function(shp, sample,
                      point_color = "red",
                      point_size = 1,
                      point_alpha = 0.8,
                      scale_position = "bl",
                      arrow_position = "tr",
                      title = NULL,
                      subtitle = NULL,
                      caption = NULL) {

  # 验证输入数据
  if (!inherits(shp, "sf")) {
    stop("shp参数必须是sf对象")
  }

  if (!all(c("longitude", "latitude") %in% colnames(sample))) {
    stop("sample数据框必须包含longitude和latitude列")
  }

  # 创建基础地图
  p <- ggplot() +
    geom_sf(data = shp, fill = "lightgray", color = "gray50", alpha = 0.5) +
    geom_point(
      data = sample,
      aes(x = longitude, y = latitude),
      color = point_color,
      size = point_size,
      alpha = point_alpha
    ) +
    annotation_scale(
      location = scale_position,
      bar_cols = c("black", "white"),
      text_cex = 0.8,
      pad_x = unit(0.5, "cm"),
      pad_y = unit(0.5, "cm")
    ) +
    annotation_north_arrow(
      location = arrow_position,
      which_north = "true",
      style = north_arrow_nautical(),
      height = unit(1.5, "cm"),
      width = unit(1.5, "cm")
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      plot.caption = element_text(hjust = 1, size = 9),
      panel.grid.major = element_line(color = "gray90", size = 0.2),
      panel.background = element_rect(fill = "white")
    )

  return(p)
}

# 扩展功能：添加点标签
map_point_label <- function(..., label_var = NULL, label_size = 3) {
  p <- map_point(...)

  if (!is.null(label_var)) {
    # 获取原始参数
    args <- list(...)
    sample <- args$sample

    if (!label_var %in% colnames(sample)) {
      stop(paste("标签变量", label_var, "不存在于sample数据框中"))
    }

    p <- p +
      ggrepel::geom_label_repel(
        data = sample,
        aes(x = longitude, y = latitude, label = !!sym(label_var)),
        size = label_size,
        box.padding = 0.35,
        point.padding = 0.5,
        segment.color = "gray50",
        max.overlaps = Inf
      )
  }

  return(p)
}
