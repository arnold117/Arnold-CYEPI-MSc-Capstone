library(ggplot2)

# 数据准备
theta <- seq(0, 2 * pi, length.out = 100)
data <- data.frame(
  x = cos(theta),
  y = sin(theta),
  theta = theta
)

# 角度 theta
theta <- pi / 4
cos_theta <- cos(theta)
sin_theta <- sin(theta)

# 2d 欧拉公式图
ggplot(data, aes(x = x, y = y)) +
  geom_path(color = "blue", size = 1) +  # 圆的轨迹
  geom_segment(aes(x = 0, y = 0, xend = cos_theta, yend = sin_theta), 
               arrow = arrow(length = unit(0.2, "cm")), color = "red", inherit.aes = FALSE) +  # 从原点到点的箭头
  geom_segment(aes(x = cos_theta, y = 0, xend = cos_theta, yend = sin_theta), 
               linetype = "dashed", color = "gray", inherit.aes = FALSE) +  # 垂线到横轴
#   geom_segment(aes(x = 0, y = sin_theta, xend = cos_theta, yend = sin_theta), 
#                linetype = "dashed", color = "gray", inherit.aes = FALSE) +  # 垂线到纵轴
  geom_hline(yintercept = 0, color = "black", size = 0.5) +  # 手动添加横轴
  geom_vline(xintercept = 0, color = "black", size = 0.5) +  # 手动添加纵轴
  geom_path(data = data.frame(
    x = 0.2 * cos(seq(0, theta, length.out = 100)),
    y = 0.2 * sin(seq(0, theta, length.out = 100))
  ), aes(x = x, y = y), color = "blue", size = 0.8) +  # 添加圆弧表示角度 theta
  annotate("text", x = 0.25 * cos(theta / 2), y = 0.25 * sin(theta / 2), 
           label = "θ", color = "blue", size = 5) +  # 在圆弧上标注 theta
  annotate("text", x = 0.3, y = -0.1, label = "cos(theta)", parse = TRUE, hjust = 0, color = "darkgreen") +  # 标注 cos(theta)
  annotate("text", x = 0.75, y = 0.3, label = "i * sin(theta)", parse = TRUE, vjust = 0, color = "darkgreen") +  # 修改 sin 标注为 i * sin(theta)
  annotate("text", x = 0.3, y = 0.8, 
           label = "e^{i * theta} == cos(theta) + i * sin(theta)", 
           parse = TRUE, hjust = 0, size=5.5) +  # 在点旁边标注欧拉公式
  labs(
    x = "Re (Real Axis)",  # 横轴标签
    y = "Im (Imaginary Axis)"  # 纵轴标签
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # 设置背景为白色
    plot.background = element_rect(fill = "white", color = NA),  # 设置绘图区域背景为白色
    axis.text = element_text(color = "black"),                   # 设置轴标签颜色
    axis.title = element_text(color = "black"),                  # 设置轴标题颜色
    panel.grid = element_blank(),                                # 移除网格线
    axis.ticks = element_line(color = "black")                   # 添加刻度线
  ) +
  coord_fixed(ratio = 1, xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))  # 固定比例，设置范围

# 保存图像
ggsave("euler_diagram_2d_optimized.png", width = 6, height = 6, bg = "white")

