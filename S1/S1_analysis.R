# ============================================================
# Study 1a 复现
# 论文: Steinmetz et al. (2017)
#   "Mental Simulation of Visceral States Affects Preferences
#    and Behavior"
# ------------------------------------------------------------
# 实验设计:
#   参与者被随机分配到两组:
#     条件1 (hot)  - 想象自己在炎热环境中
#     条件2 (cold) - 想象自己在寒冷环境中
#   然后测量:
#     - 操纵检查: Q42.0 (当前感受到多冷/热)
#     - 因变量: warm_pref (取暖活动偏好, 由5个题项合成)
#       题项: A3, A4(反向), A6(反向), A8, A10(反向)
#       评分范围 1-9, 分数越高 = 越偏好取暖活动
# ============================================================

# ---- 1. 加载包 ----
library(haven)    # 读取 SPSS .sav 文件
library(psych)    # 信度分析 (Cronbach's alpha)
library(tidyverse) # 数据处理和可视化

# ---- 2. 读取数据 ----
raw <- read_sav(
  "osfstorage-archive/Study1/S1.sav"
)

# 查看变量名
names(raw)

# 查看实验条件分布
table(raw$condition)  # 1=hot, 2=cold

# ---- 3. 数据预处理 ----

# 3a. 反向计分
# 注意: SPSS语法中有一处笔误 "COMPUTE A10_rev=10 - 10"
#       实际应为 10 - A10, 这里按正确写法
raw <- raw %>%
  mutate(
    A4_rev  = 10 - as.numeric(A4),
    A6_rev  = 10 - as.numeric(A6),
    A10_rev = 10 - as.numeric(A10)
  )

# 3b. 信度分析 (Cronbach's alpha)
#     检查这5个题项是否适合合成一个总分
alpha_result <- psych::alpha(
  raw %>% select(A4_rev, A6_rev, A10_rev, A3, A8) %>%
    mutate(across(everything(), as.numeric))
)
alpha_result$total$raw_alpha  # 原始alpha系数
alpha_result$alpha.drop       # 删除各题项后的alpha

# 3c. 合成因变量: 取暖活动偏好 (取均值)
raw <- raw %>%
  mutate(
    warm_pref = rowMeans(
      across(c(A4_rev, A6_rev, A10_rev, A3, A8), as.numeric),
      na.rm = TRUE
    )
  )

# ---- 4. 描述统计 ----
raw %>%
  group_by(condition) %>%
  summarise(
    n         = n(),
    warm_mean = mean(as.numeric(warm_pref), na.rm = TRUE),
    warm_sd   = sd(as.numeric(warm_pref), na.rm = TRUE),
    feel_mean = mean(as.numeric(Q42.0), na.rm = TRUE),
    feel_sd   = sd(as.numeric(Q42.0), na.rm = TRUE)
  )

# ---- 5. 独立样本 t 检验 ----
# 检验假设: 想象寒冷的人比想象炎热的人更偏好取暖活动

# 5a. 对 warm_pref 做 t 检验
t_warm <- t.test(
  as.numeric(warm_pref) ~ condition,
  data = raw,
  var.equal = TRUE   # 等同SPSS默认设置
)
t_warm

# 5b. 对操纵检查变量 Q42.0 做 t 检验
#     验证: 想象寒冷的人确实报告感觉更冷
t_feel <- t.test(
  as.numeric(Q42.0) ~ condition,
  data = raw,
  var.equal = TRUE
)
t_feel

# ---- 6. 回归分析 ----
# 6a. 简单回归: warm_pref ~ 感受(Q42.0)
#     检验感受是否显著预测偏好
lm1 <- lm(as.numeric(warm_pref) ~ as.numeric(Q42.0), data = raw)
summary(lm1)
confint(lm1)

# 6b. 多元回归: warm_pref ~ condition + Q42.0
#     加入条件后, 感受的效应是否仍然显著?
lm2 <- lm(
  as.numeric(warm_pref) ~ factor(condition) + as.numeric(Q42.0),
  data = raw
)
summary(lm2)
confint(lm2)

# ---- 7. (可选) 可视化 ----
# 箱线图: 两组取暖偏好对比
p <- ggplot(raw, aes(x = factor(condition, labels = c("热", "冷")),
                y = as.numeric(warm_pref))) +
  geom_boxplot(fill = c("#FFB5C2", "#A8D8EA")) +  # 马卡龙粉, 马卡龙蓝
  labs(
    title = "想象热 vs 冷 对取暖偏好的影响",
    x = "实验条件",
    y = "取暖活动偏好 (1-9)"
  ) +
  theme_minimal()

print(p)

# 保存图片到 S1 文件夹
ggsave("S1/Fig_condition_vs_warmpref.png", plot = p, width = 6, height = 4, dpi = 300)

# ---- Session Info ----
sessionInfo()
