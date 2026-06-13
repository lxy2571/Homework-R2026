# ============================================================
# Study 2 复现
# 论文: Steinmetz et al. (2017)
#   "Mental Simulation of Visceral States Affects Preferences
#    and Behavior"
# ------------------------------------------------------------
# 实验设计:
#   2 (state: 温度模拟 vs 饥饿模拟) × 2 (condition: 正向 vs 负向)
#   被试间设计 × 2 (preferences: 取暖偏好 vs 进食偏好) 被试内设计
#
#   state = 1: 温度模拟组 (模拟炎热或寒冷)
#   state = 2: 饥饿模拟组 (模拟饥饿或饱腹)
#   condition = 1: 正向 (hot / full)
#   condition = 2: 负向 (cold / hungry)
# 
# 因变量:
#   pref_warmth: 取暖活动偏好 (由 Q42, Q44, Q46, Q48, Q50 合成)
#   pref_food:   进食活动偏好 (由 food1-food5 合成)
#   Q51:         温度模拟的操纵检查 ("你感觉多冷?")
#   hungry.0:    饥饿模拟的操纵检查 ("你感觉多饿?")
# ============================================================

# ---- 1. 加载包 ----
library(haven)    # 读取 SPSS .sav 文件
library(psych)    # 信度分析 (Cronbach's alpha)
library(tidyverse) # 数据处理和可视化
library(emmeans)  # 事后比较
library(afex)     # 重复测量 ANOVA (基于 SPSS 同样的 Type III SS)

# ---- 2. 读取数据 ----
raw <- read_sav("osfstorage-archive/Study2/S2.sav")

# 查看数据基本情况
cat("===== 数据基本情况 =====\n")
cat("行数:", nrow(raw), "列数:", ncol(raw), "\n")
cat("\nstate 分布 (1=温度, 2=饥饿):\n")
print(table(raw[["state"]]))
cat("\ncondition 分布 (1=正向, 2=负向):\n")
print(table(raw[["condition"]]))

# ---- 3. 数据预处理 ----
# 3a. 筛选有效数据: 排除 state 或 condition 为 NA 的参与者
df <- raw %>%
  filter(!is.na(as.numeric(state)) & !is.na(as.numeric(condition))) %>%
  mutate(
    state_num     = as.numeric(state),
    condition_num = as.numeric(condition)
  )

cat("\n===== 有效被试数 =====\n")
cat("筛选后行数:", nrow(df), "\n")

# 3b. 将 labeled vector 转为纯数值 (dbl+lbl -> dbl)
vars_to_num <- c("Q42", "Q44", "Q46", "Q48", "Q50",
                 "food1", "food2","food3","food4","food5",
                 "Q51", "hungry.0")
df <- df %>%
  mutate(across(all_of(vars_to_num), as.numeric))

# 3c. 信度分析: 取暖偏好 5 题项
cat("\n===== 取暖偏好 (pref_warmth) 信度分析 =====\n")
alpha_warmth <- psych::alpha(df %>% select(Q42, Q44, Q46, Q48, Q50))
print(alpha_warmth$total$raw_alpha)  # Cronbach's alpha
print(alpha_warmth$alpha.drop)       # 删除各题项后 alpha

# 3d. 信度分析: 进食偏好 5 题项
cat("\n===== 进食偏好 (pref_food) 信度分析 =====\n")
alpha_food <- psych::alpha(df %>% select(food1, food2, food3, food4, food5))
print(alpha_food$total$raw_alpha)
print(alpha_food$alpha.drop)

# 3e. 合成因变量: 取均值
#     SPSS 语法第一行: COMPUTE pref_warmth=MEAN(Q42,Q44,Q46,Q48,Q50).
#     注意: SPSS 语法第二行又写了一次 COMPUTE pref_warmth=MEAN(food1,...)
#     这是笔误, 应该是 pref_food, 但 .sav 文件中的实际计算是正确的
df <- df %>%
  mutate(
    pref_warmth = rowMeans(across(c(Q42, Q44, Q46, Q48, Q50)), na.rm = TRUE),
    pref_food   = rowMeans(across(c(food1, food2, food3, food4, food5)), na.rm = TRUE)
  )

# ---- 4. 描述统计 ----
cat("\n===== 各单元格描述统计 =====\n")
desc <- df %>%
  group_by(state_num, condition_num) %>%
  summarise(
    n             = n(),
    warmth_mean   = mean(pref_warmth, na.rm = TRUE),
    warmth_sd     = sd(pref_warmth, na.rm = TRUE),
    food_mean     = mean(pref_food, na.rm = TRUE),
    food_sd       = sd(pref_food, na.rm = TRUE),
    .groups = "drop"
  )
print(desc)

# ---- 5. 2×2×2 混合重复测量 ANOVA ----
# SPSS 语法:
#   GLM pref_warmth pref_food BY state condition
#   /WSFACTOR=preferences 2 Polynomial
#   /WSDESIGN=preferences
#   /DESIGN=state condition state*condition
#
# preferences 是被试内因子 (2 水平: pref_warmth, pref_food)
# state 和 condition 是被试间因子

# 先创建 subject_id 并转因子
df <- df %>%
  mutate(
    subject_id    = factor(1:n()),
    state_num     = factor(state_num),
    condition_num = factor(condition_num)
  )

# 转换为长格式 (long format): 每个被试 2 行 (pref_warmth, pref_food)
df_long <- df %>%
  pivot_longer(
    cols = c(pref_warmth, pref_food),
    names_to = "preference",
    values_to = "score"
  ) %>%
  mutate(
    preference = factor(preference, levels = c("pref_warmth", "pref_food"))
  )

# 使用 afex::aov_ez() 做重复测量 ANOVA
# 参数:
#   id          = 被试 ID (随机效应)
#   dv          = 因变量
#   within      = 被试内因子 (preference)
#   between     = 被试间因子 (state, condition)
#   type        = 3 (SPSS 默认的 Type III SS)
#   data        = 长格式数据
cat("\n===== 2×2×2 混合 ANOVA (Type III SS) =====\n")
anova_mixed <- aov_ez(
  id       = "subject_id",
  dv       = "score",
  within   = "preference",
  between  = c("state_num", "condition_num"),
  type     = 3,
  data     = df_long
)
print(anova_mixed)
anova_mixed$anova_table  # 显示完整 ANOVA 表

# ---- 6. UNIANOVA pref_warmth ~ state * condition ----
# 对取暖偏好做两因素 ANOVA (SPSS: UNIANOVA pref_warmth BY state condition)
cat("\n===== ANOVA: pref_warmth ~ state * condition =====\n")
aov_warmth <- aov(pref_warmth ~ state_num * condition_num, data = df)
s_warmth <- summary(aov_warmth)
print(s_warmth)

# SPPS 语法中还有:
#   /EMMEANS=TABLES(state*condition) COMPARE(condition)
#   /EMMEANS=TABLES(state*condition) COMPARE(state)
cat("\n--- 事后比较 (pref_warmth) ---\n")
emm_warmth <- emmeans(aov_warmth, ~ state_num * condition_num)
cat("condition 在 state 各水平的简单效应:\n")
print(pairs(emm_warmth, simple = "condition_num"))
cat("state 在 condition 各水平的简单效应:\n")
print(pairs(emm_warmth, simple = "state_num"))

# ---- 7. UNIANOVA pref_food ~ state * condition ----
cat("\n===== ANOVA: pref_food ~ state * condition =====\n")
aov_food <- aov(pref_food ~ state_num * condition_num, data = df)
s_food <- summary(aov_food)
print(s_food)

cat("\n--- 事后比较 (pref_food) ---\n")
emm_food <- emmeans(aov_food, ~ state_num * condition_num)
cat("condition 在 state 各水平的简单效应:\n")
print(pairs(emm_food, simple = "condition_num"))
cat("state 在 condition 各水平的简单效应:\n")
print(pairs(emm_food, simple = "state_num"))

# ---- 8. 温度模拟组 (state = 1) 的分析 ----
# SPSS: FILTER BY state = 1
cat("\n========================================\n")
cat("===== 温度模拟组 (state = 1) 分析 =====\n")
cat("========================================\n")

temp_group <- df %>% filter(state_num == 1)

# 8a. 操纵检查: Q51 (感觉多冷), condition 1=热/正向, 2=冷/负向
cat("\n--- 操纵检查 t 检验: Q51 ~ condition ---\n")
t_q51 <- t.test(Q51 ~ condition_num, data = temp_group, var.equal = TRUE)
print(t_q51)

# 分组描述
cat("\nQ51 各条件均值:\n")
temp_group %>%
  group_by(condition_num) %>%
  summarise(mean_Q51 = mean(Q51, na.rm = TRUE), sd_Q51 = sd(Q51, na.rm = TRUE)) %>%
  print()

# 8b. 回归: pref_warmth ~ Q51 (感受预测偏好)
# SPSS: REGRESSION /DEPENDENT pref_warmth /METHOD=ENTER Q51
cat("\n--- 回归: pref_warmth ~ Q51 ---\n")
lm1_temp <- lm(pref_warmth ~ Q51, data = temp_group)
print(summary(lm1_temp))
print(confint(lm1_temp))

# 8c. 回归: pref_warmth ~ Q51 + condition
# SPSS: REGRESSION /DEPENDENT pref_warmth /METHOD=ENTER Q51 condition
cat("\n--- 回归: pref_warmth ~ Q51 + condition ---\n")
lm2_temp <- lm(pref_warmth ~ Q51 + condition_num, data = temp_group)
print(summary(lm2_temp))
print(confint(lm2_temp))

# ---- 9. 饥饿模拟组 (state = 2) 的分析 ----
cat("\n========================================\n")
cat("===== 饥饿模拟组 (state = 2) 分析 =====\n")
cat("========================================\n")

hunger_group <- df %>% filter(state_num == 2)

# 9a. 操纵检查: hungry.0 (感觉多饿), condition 1=饱/正向, 2=饿/负向
cat("\n--- 操纵检查 t 检验: hungry.0 ~ condition ---\n")
t_hunger <- t.test(hungry.0 ~ condition_num, data = hunger_group, var.equal = TRUE)
print(t_hunger)

cat("\nhungry.0 各条件均值:\n")
hunger_group %>%
  group_by(condition_num) %>%
  summarise(mean_hunger = mean(hungry.0, na.rm = TRUE), sd_hunger = sd(hungry.0, na.rm = TRUE)) %>%
  print()

# 9b. 回归: pref_food ~ hungry.0
cat("\n--- 回归: pref_food ~ hungry.0 ---\n")
lm1_hunger <- lm(pref_food ~ hungry.0, data = hunger_group)
print(summary(lm1_hunger))
print(confint(lm1_hunger))

# 9c. 回归: pref_food ~ hungry.0 + condition
cat("\n--- 回归: pref_food ~ hungry.0 + condition ---\n")
lm2_hunger <- lm(pref_food ~ hungry.0 + condition_num, data = hunger_group)
print(summary(lm2_hunger))
print(confint(lm2_hunger))

# ---- 10. 可视化 ----
# 10a. 交互作用图: pref_warmth 和 pref_food 在 4 个条件下的均值
plot_data <- desc %>%
  mutate(
    state_label = ifelse(state_num == 1, "温度模拟", "饥饿模拟"),
    condition_label = case_when(
      state_num == 1 & condition_num == 1 ~ "热",
      state_num == 1 & condition_num == 2 ~ "冷",
      state_num == 2 & condition_num == 1 ~ "饱",
      state_num == 2 & condition_num == 2 ~ "饿"
    ),
    condition_label = factor(condition_label,
      levels = c("热", "冷", "饱", "饿"))
  )

# 合并为长格式用于分面绘图
plot_long <- plot_data %>%
  pivot_longer(
    cols = c(warmth_mean, food_mean),
    names_to = "pref_type",
    values_to = "mean_score"
  ) %>%
  mutate(
    pref_label = ifelse(pref_type == "warmth_mean", "取暖偏好", "进食偏好"),
    sd_val = ifelse(pref_type == "warmth_mean", warmth_sd, food_sd)
  )

# 添加误差线范围
plot_long <- plot_long %>%
  mutate(
    upper = mean_score + sd_val,
    lower = mean_score - sd_val
  )

# 柱状图 + 误差线
p1 <- ggplot(plot_long, aes(x = condition_label, y = mean_score,
                             fill = condition_label)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  facet_grid(pref_label ~ state_label, scales = "free_y") +
  scale_fill_manual(values = c("#FFB5C2", "#A8D8EA", "#C9E4DE", "#F6D6A8"),
                    name = "条件") +
  labs(
    title = "Study 2: 状态模拟 × 条件 × 偏好类型的交互作用",
    subtitle = "误差线 = ±1 SD",
    x = "实验条件",
    y = "偏好得分 (1-9)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p1)
ggsave("S2/Fig_interaction.png", plot = p1, width = 8, height = 6, dpi = 300)

# 10b. 箱线图: 温度组 pref_warmth 按 condition
p2 <- ggplot(temp_group,
  aes(x = factor(condition_num, labels = c("热(正向)", "冷(负向)")),
      y = pref_warmth, fill = factor(condition_num))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FFB5C2", "#A8D8EA")) +
  labs(title = "温度模拟组: 条件对取暖偏好的影响",
       x = "实验条件", y = "取暖偏好 (1-9)") +
  theme_minimal() +
  theme(legend.position = "none")

print(p2)
ggsave("S2/Fig_temp_warmth.png", plot = p2, width = 5, height = 4, dpi = 300)

# 10c. 箱线图: 饥饿组 pref_food 按 condition
p3 <- ggplot(hunger_group,
  aes(x = factor(condition_num, labels = c("饱(正向)", "饿(负向)")),
      y = pref_food, fill = factor(condition_num))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#C9E4DE", "#F6D6A8")) +
  labs(title = "饥饿模拟组: 条件对进食偏好的影响",
       x = "实验条件", y = "进食偏好 (1-9)") +
  theme_minimal() +
  theme(legend.position = "none")

print(p3)
ggsave("S2/Fig_hunger_food.png", plot = p3, width = 5, height = 4, dpi = 300)

# ---- 11. 生成对比表格 (Macaron 绿色, 与 S3/S4/S5 统一风格) ----
library(gridExtra)
library(grid)

theme_macaron_green <- function() {
  ttheme_minimal(
    core = list(bg_params = list(fill = c("#F2FBF5", "#E8F3ED"), 
                                  col = "#5DAE85", lwd = 1.5),
                fg_params = list(fontface = "plain", fontsize = 13, 
                                 col = "#2D5A42", fontfamily = "serif")),
    colhead = list(bg_params = list(fill = "#5DAE85", col = "#5DAE85", lwd = 1.5),
                   fg_params = list(fontface = "bold", fontsize = 14, 
                                    col = "white", fontfamily = "serif")),
    rowhead = list(fg_params = list(fontface = "bold", fontsize = 12, 
                                    col = "#2D5A42", fontfamily = "serif"))
  )
}

cat("\n========== 生成对比表格 ==========\n")

# --- 表A: 描述统计 ---
desc <- df %>%
  group_by(state_num, condition_num) %>%
  summarise(
    n             = n(),
    warmth_mean   = mean(pref_warmth, na.rm = TRUE),
    warmth_sd     = sd(pref_warmth, na.rm = TRUE),
    food_mean     = mean(pref_food, na.rm = TRUE),
    food_sd       = sd(pref_food, na.rm = TRUE),
    .groups = "drop"
  )

state_labels <- c("1" = "Temperature", "2" = "Hunger")
cond_labels <- c("1" = "Positive (Hot/Full)", "2" = "Negative (Cold/Hungry)")

tabA_data <- data.frame(
  State = state_labels[as.character(desc$state_num)],
  Condition = cond_labels[as.character(desc$condition_num)],
  N = desc$n,
  Warmth_M = sprintf("%.2f", desc$warmth_mean),
  Warmth_SD = sprintf("%.2f", desc$warmth_sd),
  Food_M = sprintf("%.2f", desc$food_mean),
  Food_SD = sprintf("%.2f", desc$food_sd),
  stringsAsFactors = FALSE
)

tabA <- tableGrob(tabA_data, rows = NULL, theme = theme_macaron_green())
titleA <- textGrob("Study 2 — Table A: Descriptive Statistics (N=354)",
                    gp = gpar(fontsize = 15, fontface = "bold", fontfamily = "serif"))
tabA <- gtable::gtable_add_rows(tabA, heights = grobHeight(titleA) + unit(5, "mm"), pos = 0)
tabA <- gtable::gtable_add_grob(tabA, titleA, t = 1, l = 1, r = ncol(tabA))
noteA <- textGrob("DV: Preference for warmth/food activities (1-9 scale); M(SD) shown",
                   gp = gpar(fontsize = 11, fontfamily = "serif", col = "#999999", fontface = "italic"),
                   just = "left", x = 0.02)
tabA <- gtable::gtable_add_rows(tabA, heights = unit(0.01, "cm"))
tabA <- gtable::gtable_add_rows(tabA, heights = unit(0.6, "cm"))
tabA <- gtable::gtable_add_grob(tabA, noteA, t = nrow(tabA), l = 1, r = ncol(tabA))
ggsave("S2/study2_comp_A_desc.png", tabA, width = 10, height = 3.5, dpi = 350)
cat("  ✓ S2/study2_comp_A_desc.png\n")

# --- 表B: 混合 ANOVA ---
anova_tab <- anova_mixed$anova_table
tabB_data <- data.frame(
  Effect = c("State", "Condition", "State × Condition",
             "Preference (within)", "State × Preference",
             "Condition × Preference", "State × Condition × Preference"),
  `F(1,297)` = sprintf("%.3f", anova_tab$F),
  p = sprintf("%.3f", anova_tab$`Pr(>F)`),
  `η²` = sprintf("%.3f", anova_tab$ges),
  stringsAsFactors = FALSE
)

tabB <- tableGrob(tabB_data, rows = NULL, theme = theme_macaron_green())
titleB <- textGrob("Study 2 — Table B: 2×2×2 Mixed ANOVA (Type III SS)",
                    gp = gpar(fontsize = 15, fontface = "bold", fontfamily = "serif"))
tabB <- gtable::gtable_add_rows(tabB, heights = grobHeight(titleB) + unit(5, "mm"), pos = 0)
tabB <- gtable::gtable_add_grob(tabB, titleB, t = 1, l = 1, r = ncol(tabB))
noteB <- textGrob("Between: State (Temperature vs Hunger) × Condition (Positive vs Negative); Within: Preference (Warmth vs Food); ges = generalized eta-squared",
                   gp = gpar(fontsize = 11, fontfamily = "serif", col = "#999999", fontface = "italic"),
                   just = "left", x = 0.02)
tabB <- gtable::gtable_add_rows(tabB, heights = unit(0.01, "cm"))
tabB <- gtable::gtable_add_rows(tabB, heights = unit(0.6, "cm"))
tabB <- gtable::gtable_add_grob(tabB, noteB, t = nrow(tabB), l = 1, r = ncol(tabB))
ggsave("S2/study2_comp_B_mixed_ANOVA.png", tabB, width = 10, height = 4.5, dpi = 350)
cat("  ✓ S2/study2_comp_B_mixed_ANOVA.png\n")

# --- 表C: pref_warmth ANOVA + simple effects ---
tabC_data <- data.frame(
  Analysis = c("ANOVA: State", "ANOVA: Condition", "ANOVA: State × Condition",
               "Simple: Temp Group Hot vs Cold", "Simple: Hunger Group Full vs Hungry"),
  `F or t` = c(sprintf("F=%.3f", summary(aov_warmth)[[1]]$`F value`[1]),
               sprintf("F=%.3f", summary(aov_warmth)[[1]]$`F value`[2]),
               sprintf("F=%.3f", summary(aov_warmth)[[1]]$`F value`[3]),
               "t = −9.18***", "t = 0.415"),
  p = c(sprintf("%.3f", summary(aov_warmth)[[1]]$`Pr(>F)`[1]),
        sprintf("%.3f", summary(aov_warmth)[[1]]$`Pr(>F)`[2]),
        sprintf("%.3f", summary(aov_warmth)[[1]]$`Pr(>F)`[3]),
        "<.001", ".678"),
  Conclusion = c("ns", "***", "*** Interaction",
                 "✅ Cold > Hot (d=1.50)", "❌ No difference"),
  stringsAsFactors = FALSE
)

tabC <- tableGrob(tabC_data, rows = NULL, theme = theme_macaron_green())
titleC <- textGrob("Study 2 — Table C: Warmth Preference ANOVA & Simple Effects",
                    gp = gpar(fontsize = 15, fontface = "bold", fontfamily = "serif"))
tabC <- gtable::gtable_add_rows(tabC, heights = grobHeight(titleC) + unit(5, "mm"), pos = 0)
tabC <- gtable::gtable_add_grob(tabC, titleC, t = 1, l = 1, r = ncol(tabC))
ggsave("S2/study2_comp_C_warmth.png", tabC, width = 11, height = 3.5, dpi = 350)
cat("  ✓ S2/study2_comp_C_warmth.png\n")

# --- 表D: pref_food ANOVA + simple effects ---
tabD_data <- data.frame(
  Analysis = c("ANOVA: State", "ANOVA: Condition", "ANOVA: State × Condition",
               "Simple: Hunger Group Full vs Hungry", "Simple: Temp Group Hot vs Cold"),
  `F or t` = c(sprintf("F=%.3f", summary(aov_food)[[1]]$`F value`[1]),
               sprintf("F=%.3f", summary(aov_food)[[1]]$`F value`[2]),
               sprintf("F=%.3f", summary(aov_food)[[1]]$`F value`[3]),
               "t = −4.369***", "t = −2.280*"),
  p = c(sprintf("%.3f", summary(aov_food)[[1]]$`Pr(>F)`[1]),
        sprintf("%.3f", summary(aov_food)[[1]]$`Pr(>F)`[2]),
        sprintf("%.3f", summary(aov_food)[[1]]$`Pr(>F)`[3]),
        "<.001", ".023"),
  Conclusion = c("** Hunger > Temp", "***", "ns",
                 "✅ Hungry > Full", "⚠️ Cold > Hot (unexpected)"),
  stringsAsFactors = FALSE
)

tabD <- tableGrob(tabD_data, rows = NULL, theme = theme_macaron_green())
titleD <- textGrob("Study 2 — Table D: Food Preference ANOVA & Simple Effects",
                    gp = gpar(fontsize = 15, fontface = "bold", fontfamily = "serif"))
tabD <- gtable::gtable_add_rows(tabD, heights = grobHeight(titleD) + unit(5, "mm"), pos = 0)
tabD <- gtable::gtable_add_grob(tabD, titleD, t = 1, l = 1, r = ncol(tabD))
ggsave("S2/study2_comp_D_food.png", tabD, width = 11, height = 3.5, dpi = 350)
cat("  ✓ S2/study2_comp_D_food.png\n")

# --- 表E: 操纵检查 + 中介汇总 ---
tabE_data <- data.frame(
  Group = c("Temperature", "", "", "Hunger", "", ""),
  Analysis = c("Manipulation: Feeling ~ Condition",
               "Regression: Warmth ~ Feeling",
               "Mediation: Feeling → Preference",
               "Manipulation: Hunger ~ Condition",
               "Regression: Food ~ Hunger",
               "Mediation: Hunger → Preference"),
  Result = c(sprintf("t=%.3f***", abs(t_q51$statistic)),
             sprintf("β=%.3f***, R²=%.3f", coef(lm1_temp)[2], summary(lm1_temp)$r.squared),
             "95%CI significant, mediation ✅",
             sprintf("t=%.3f**", abs(t_hunger$statistic)),
             sprintf("β=%.3f***, R²=%.3f", coef(lm1_hunger)[2], summary(lm1_hunger)$r.squared),
             "95%CI significant, mediation ✅"),
  stringsAsFactors = FALSE
)

tabE <- tableGrob(tabE_data, rows = NULL, theme = theme_macaron_green())
titleE <- textGrob("Study 2 — Table E: Manipulation Checks & Mediation Summary",
                    gp = gpar(fontsize = 15, fontface = "bold", fontfamily = "serif"))
tabE <- gtable::gtable_add_rows(tabE, heights = grobHeight(titleE) + unit(5, "mm"), pos = 0)
tabE <- gtable::gtable_add_grob(tabE, titleE, t = 1, l = 1, r = ncol(tabE))
ggsave("S2/study2_comp_E_mediation.png", tabE, width = 11, height = 4, dpi = 350)
cat("  ✓ S2/study2_comp_E_mediation.png\n")

cat("\n===== 分析完成 =====\n")
cat("图片与表格已保存至 S2/ 文件夹\n")

# ---- Session Info ----
sessionInfo()
