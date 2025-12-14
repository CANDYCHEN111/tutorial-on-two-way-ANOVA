library(bruceR)
library(tidyr) # 用于数据转换
library(dplyr) # 用于数据处理

# 1. 还原你图片中的数据 (宽格式)
df_wide <- data.frame(
  ID = 1:8,
  A = c(1, 1, 1, 1, 2, 2, 2, 2),        # 被试间变量 (2水平)
  B1 = c(3, 6, 4, 3, 4, 5, 3, 3),       # 被试内水平1
  B2 = c(4, 6, 4, 2, 8, 9, 8, 7),       # 被试内水平2
  B3 = c(5, 7, 5, 2, 12, 13, 12, 11)    # 被试内水平3
)

# 【关键步骤】转换为长格式
# 也就是把 B1, B2, B3 合并成一列，不仅要有分数，还要有一个标签告诉R这是哪个B
df_long <- df_wide %>%
  pivot_longer(
    cols = c("B1", "B2", "B3"), # 需要合并的列
    names_to = "B",   # 新变量名：用来存 "B1","B2" 这些标签
    values_to = "Score"         # 新变量名：用来存具体的分数
  ) %>%
  mutate(
    A = factor(A),             # 重要：必须把分组变量转化为因子(Factor)类型
    B = factor(B),
    ID = factor(ID)
  )

# 查看转换后的数据样子 (这是 bruceR 需要的格式)
print(head(df_long))


# 2. 运行方差分析
library(bruceR)

model <- MANOVA(
  data = df_long,
  subID = "ID",            # 被试编号
  dv = "Score",            # 因变量 (分数)
  between = "A",           # 被试间因子 
  within = "B",            # 被试内因子
  sph.correction = "GG" # 球形度校正 (Greenhouse-Geisser) 
)

cat("\n\n=== 3. 主效应多重比较  ===\n")

# 检验变量 B 的主效应 (B1 vs B2 vs B3)
EMMEANS(model, effect = "B", p.adjust = "bonferroni")


cat("\n\n=== 4.1 简单效应：固定 A，看 B 的差异 (最常用) ===\n")
# 含义：在 A1 水平下，B1/B2/B3 有差异吗？在 A2 水平下呢？
sim_eff_B_by_A <- EMMEANS(model, effect = "B", by = "A", p.adjust = "bonferroni")
print(sim_eff_B_by_A)

cat("\n\n=== 4.2 简单效应：固定 B，看 A 的差异 ===\n")
# 含义：在 B1 阶段，A1 和 A2 有差异吗？在 B3 阶段呢？
sim_eff_A_by_B <- EMMEANS(model, effect = "A", by = "B", p.adjust = "bonferroni")
print(sim_eff_A_by_B)

# =======================================================
# 第 5 步：结果可视化 (交互作用图)
# =======================================================
library(emmeans)
library(ggplot2)

# 1. 使用 emmeans::emmeans 原生函数提取数据
# 注意：语法是 ~ 变量B | 变量A
emm_data <- emmeans(model, ~ B | A) %>% 
  as.data.frame()

# 查看提取出的数据，确保没有报错
print(head(emm_data))

# 2. 使用 ggplot2 绘图
ggplot(emm_data, aes(x = A, y = emmean, group = B, color = B)) +
  geom_point(position = position_dodge(0.2), size = 3) +
  geom_line(position = position_dodge(0.2), linewidth = 1) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                width = 0.2, position = position_dodge(0.2)) +
  labs(x = "A", y = "Estimated Marginal Means", color = "B") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  
  # === 添加这一行 ===
  scale_color_brewer(palette = "Set2")