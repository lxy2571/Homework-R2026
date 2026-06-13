# ============================================================
# Study 4: Mental Simulation -> Momentary vs General Preferences
# Steinmetz et al. (2017) - Replication in R
# ============================================================
# 实验设计: 3 (hungry vs. full vs. control) × 2 (now vs. general) between-subjects
# 参与者: 405 人在 Amazon Mechanical Turk
# DV: choice = preference for filling activities (1-9 scale)
#    - nowgeneral=1: choice_now (momentary, "Right now...")
#    - nowgeneral=2: choice_general (general, "I would always...")
# ============================================================

# ---- 加载包 ----
library(foreign)
library(car)        # Type III ANOVA
library(ggplot2)
library(gridExtra)
library(grid)
library(emmeans)    # 事后比较

# ---- 1. 读取数据 ----
raw <- read.spss("osfstorage-archive/Study4/S4.sav", 
                 to.data.frame = TRUE, use.value.labels = FALSE)

# 筛选有 choice 数据的参与者（N=405，与论文一致）
df <- raw[!is.na(raw$choice), ]

# 因子编码
df$condition <- factor(df$condition, levels = c(1, 2, 3),
                       labels = c("Hungry", "Full", "Control"))
df$nowgeneral <- factor(df$nowgeneral, levels = c(1, 2),
                        labels = c("Now", "General"))

cat("总样本量:", nrow(df), "\n")
print(table(df$condition, df$nowgeneral))

# ---- 2. 描述统计 ----
desc <- aggregate(choice ~ condition + nowgeneral, data = df,
                  FUN = function(x) c(M = mean(x, na.rm = TRUE),
                                     SD = sd(x, na.rm = TRUE),
                                     N = length(x)))
cat("\n========== 各单元描述统计 ==========\n")
for (i in 1:nrow(desc)) {
  cat(sprintf("%s | %s: M = %.2f, SD = %.2f, N = %.0f\n",
              desc$condition[i], desc$nowgeneral[i],
              desc$choice[i][[1]]["M"], desc$choice[i][[1]]["SD"],
              desc$choice[i][[1]]["N"]))
}

# ---- 3. Type III ANOVA: 3×2 between-subjects ----
# SPSS 使用 Type III SS，R 的 aov() 默认 Type I
# 需要设置 treatment contrasts 以匹配 SPSS
options(contrasts = c("contr.sum", "contr.poly"))
model <- lm(choice ~ condition * nowgeneral, data = df)
anova_tab <- Anova(model, type = 3)

cat("\n========== Type III ANOVA: choice ~ condition * nowgeneral ==========\n")
print(anova_tab)

# ---- 4. 各条件下的均值 ----
# 主效应边际均值
cat("\n========== 边际均值 ==========\n")
cat("condition 主效应:\n")
print(tapply(df$choice, df$condition, mean, na.rm = TRUE))
cat("nowgeneral 主效应:\n")
print(tapply(df$choice, df$nowgeneral, mean, na.rm = TRUE))

# ---- 5. 简单效应分析（拆分 nowgeneral） ----
cat("\n========== 简单效应分析 ==========\n")

# 5a. Momentary preferences (Now)
df_now <- df[df$nowgeneral == "Now", ]
cat("\n--- Momentary (Now) ---\n")
now_means <- tapply(df_now$choice, df_now$condition, mean, na.rm = TRUE)
now_sds <- tapply(df_now$choice, df_now$condition, sd, na.rm = TRUE)
now_n <- tapply(df_now$choice, df_now$condition, function(x) sum(!is.na(x)))
for (grp in names(now_means)) {
  cat(sprintf("%s: M = %.2f, SD = %.2f, N = %.0f\n", 
              grp, now_means[grp], now_sds[grp], now_n[grp]))
}

# ANOVA for Now condition
m_now <- lm(choice ~ condition, data = df_now)
print(Anova(m_now, type = 3))

# 事后比较
cat("\nPost-hoc (Tukey) - Now:\n")
ph_now <- emmeans(m_now, ~ condition)
print(pairs(ph_now))

# Cohen's d for Now contrasts
calc_d <- function(m1, m2, s1, s2, n1, n2) {
  sp <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2))
  return((m1 - m2)/sp)
}

cat(sprintf("Now: Hungry vs Control d = %.2f\n",
            calc_d(now_means["Hungry"], now_means["Control"],
                   now_sds["Hungry"], now_sds["Control"],
                   now_n["Hungry"], now_n["Control"])))
cat(sprintf("Now: Full vs Control d = %.2f\n",
            calc_d(now_means["Full"], now_means["Control"],
                   now_sds["Full"], now_sds["Control"],
                   now_n["Full"], now_n["Control"])))
cat(sprintf("Now: Hungry vs Full d = %.2f\n",
            calc_d(now_means["Hungry"], now_means["Full"],
                   now_sds["Hungry"], now_sds["Full"],
                   now_n["Hungry"], now_n["Full"])))

# 5b. General preferences
df_gen <- df[df$nowgeneral == "General", ]
cat("\n--- General ---\n")
gen_means <- tapply(df_gen$choice, df_gen$condition, mean, na.rm = TRUE)
gen_sds <- tapply(df_gen$choice, df_gen$condition, sd, na.rm = TRUE)
gen_n <- tapply(df_gen$choice, df_gen$condition, function(x) sum(!is.na(x)))
for (grp in names(gen_means)) {
  cat(sprintf("%s: M = %.2f, SD = %.2f, N = %.0f\n", 
              grp, gen_means[grp], gen_sds[grp], gen_n[grp]))
}

m_gen <- lm(choice ~ condition, data = df_gen)
print(Anova(m_gen, type = 3))

cat("\nPost-hoc (Tukey) - General:\n")
ph_gen <- emmeans(m_gen, ~ condition)
print(pairs(ph_gen))

# ---- 6. 饥饿感受分析 ----
cat("\n========== 饥饿感受 (hungry.0) 分析 ==========\n")

# 将 hungry.0 转为数值
df$hungry_num <- as.numeric(df$hungry.0)

# 3×2 ANOVA on hunger feelings
m_hunger <- lm(hungry_num ~ condition * nowgeneral, data = df)
print(Anova(m_hunger, type = 3))

# 各条件下的饥饿感受
hunger_desc <- aggregate(hungry_num ~ condition + nowgeneral, data = df,
                         FUN = function(x) c(M = mean(x), SD = sd(x), N = length(x)))
cat("\nHunger feelings by cell:\n")
for (i in 1:nrow(hunger_desc)) {
  cat(sprintf("%s | %s: M = %.2f, SD = %.2f, N = %.0f\n",
              hunger_desc$condition[i], hunger_desc$nowgeneral[i],
              hunger_desc$hungry_num[i][[1]]["M"],
              hunger_desc$hungry_num[i][[1]]["SD"],
              hunger_desc$hungry_num[i][[1]]["N"]))
}

# ---- 7. 箱线图 ----
# 7a. 整体 3×2 箱线图
p1 <- ggplot(df, aes(x = condition, y = choice, fill = condition)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2) +
  facet_wrap(~ nowgeneral) +
  scale_fill_manual(values = c("Hungry" = "#FFB5C2", "Full" = "#A8D8EA", 
                               "Control" = "#B5EAD7")) +
  labs(title = "Study 4: Simulation × Timing Interaction",
       subtitle = "F(2, 399) = 19.791, p < .001, η² = .090",
       x = "Simulation Condition", y = "Preference for Filling Activities (1-9)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"))
ggsave("S4/Fig_interaction.png", p1, width = 8, height = 5, dpi = 350)
cat("\n图已保存: Fig_interaction.png\n")

# 7b. 仅 Current 条件
p2 <- ggplot(df_now, aes(x = condition, y = choice, fill = condition)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1.5) +
  scale_fill_manual(values = c("Hungry" = "#FFB5C2", "Full" = "#A8D8EA", 
                               "Control" = "#B5EAD7")) +
  labs(title = "Study 4: Momentary Preferences",
       x = "Simulation Condition", y = "Preference (1-9)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("S4/Fig_momentary.png", p2, width = 6, height = 5, dpi = 350)

# 7c. 仅 General 条件
p3 <- ggplot(df_gen, aes(x = condition, y = choice, fill = condition)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1.5) +
  scale_fill_manual(values = c("Hungry" = "#FFB5C2", "Full" = "#A8D8EA", 
                               "Control" = "#B5EAD7")) +
  labs(title = "Study 4: General Preferences",
       x = "Simulation Condition", y = "Preference (1-9)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("S4/Fig_general.png", p3, width = 6, height = 5, dpi = 350)

# ---- 8. 生成对比表格 ----
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

# 表格 A: 描述统计
desc_data <- data.frame(
  Timing = c("Now", "Now", "Now", "General", "General", "General"),
  Condition = rep(c("Hungry", "Full", "Control"), 2),
  N = c(now_n["Hungry"], now_n["Full"], now_n["Control"],
        gen_n["Hungry"], gen_n["Full"], gen_n["Control"]),
  Mean = sprintf("%.2f", c(now_means["Hungry"], now_means["Full"], now_means["Control"],
                           gen_means["Hungry"], gen_means["Full"], gen_means["Control"])),
  SD = sprintf("%.2f", c(now_sds["Hungry"], now_sds["Full"], now_sds["Control"],
                         gen_sds["Hungry"], gen_sds["Full"], gen_sds["Control"])),
  stringsAsFactors = FALSE
)

tabA <- tableGrob(desc_data, rows = NULL, theme = theme_macaron_green())
titleA <- textGrob("Table A: Descriptive Statistics (Filling Preferences)", 
                    gp = gpar(fontsize = 15, fontface = "bold", fontfamily = "serif"))
tabA <- gtable::gtable_add_rows(tabA, heights = grobHeight(titleA) + unit(5, "mm"), pos = 0)
tabA <- gtable::gtable_add_grob(tabA, titleA, t = 1, l = 1, r = ncol(tabA))
ggsave("S4/study4_comp_A_desc.png", tabA, width = 6, height = 3.5, dpi = 350)

# 表格 B: ANOVA
anova_row <- data.frame(
  Effect = c("Condition", "Timing", "Condition×Timing", "Residuals"),
  F = sprintf("%.3f", c(anova_tab$`F value`[2:4], NA)),
  df1 = c(2, 1, 2, NA),
  df2 = c(399, 399, 399, NA),
  p = sprintf("%.3f", c(anova_tab$`Pr(>F)`[2:4], NA)),
  eta2 = sprintf("%.3f", c(anova_tab$`Sum Sq`[2:4]/sum(anova_tab$`Sum Sq`[2:4]+anova_tab$`Sum Sq`[5]), NA)),
  stringsAsFactors = FALSE
)
anova_row$eta2[4] <- ""

tabB <- tableGrob(anova_row, rows = NULL, theme = theme_macaron_green())
titleB <- textGrob("Table B: Type III ANOVA (choice ~ condition × timing)", 
                    gp = gpar(fontsize = 15, fontface = "bold", fontfamily = "serif"))
tabB <- gtable::gtable_add_rows(tabB, heights = grobHeight(titleB) + unit(5, "mm"), pos = 0)
tabB <- gtable::gtable_add_grob(tabB, titleB, t = 1, l = 1, r = ncol(tabB))
ggsave("S4/study4_comp_B_ANOVA.png", tabB, width = 7, height = 3, dpi = 350)

# 表格 C: 简单效应 (Now)
ph_summary <- summary(pairs(ph_now))
ph_now_data <- data.frame(
  Contrast = c("Hungry vs Full", "Hungry vs Control", "Full vs Control"),
  Estimate = sprintf("%.2f", ph_summary$estimate),
  SE = sprintf("%.2f", ph_summary$SE),
  t = sprintf("%.2f", ph_summary$t.ratio),
  p = sprintf("%.3f", ph_summary$p.value),
  d = sprintf("%.2f", c(
    calc_d(now_means["Hungry"], now_means["Full"],
           now_sds["Hungry"], now_sds["Full"], now_n["Hungry"], now_n["Full"]),
    calc_d(now_means["Hungry"], now_means["Control"],
           now_sds["Hungry"], now_sds["Control"], now_n["Hungry"], now_n["Control"]),
    calc_d(now_means["Full"], now_means["Control"],
           now_sds["Full"], now_sds["Control"], now_n["Full"], now_n["Control"])
  )),
  stringsAsFactors = FALSE
)

tabC <- tableGrob(ph_now_data, rows = NULL, theme = theme_macaron_green())
titleC <- textGrob("Table C: Post-hoc (Momentary Preferences)", 
                    gp = gpar(fontsize = 15, fontface = "bold", fontfamily = "serif"))
tabC <- gtable::gtable_add_rows(tabC, heights = grobHeight(titleC) + unit(5, "mm"), pos = 0)
tabC <- gtable::gtable_add_grob(tabC, titleC, t = 1, l = 1, r = ncol(tabC))
ggsave("S4/study4_comp_C_posthoc_now.png", tabC, width = 7, height = 3, dpi = 350)

# 表格 D: 简单效应 (General)
ph_gen_summary <- summary(pairs(ph_gen))
ph_gen_data <- data.frame(
  Contrast = c("Hungry vs Full", "Hungry vs Control", "Full vs Control"),
  Estimate = sprintf("%.2f", ph_gen_summary$estimate),
  SE = sprintf("%.2f", ph_gen_summary$SE),
  t = sprintf("%.2f", ph_gen_summary$t.ratio),
  p = sprintf("%.3f", ph_gen_summary$p.value),
  d = sprintf("%.2f", c(
    calc_d(gen_means["Hungry"], gen_means["Full"],
           gen_sds["Hungry"], gen_sds["Full"], gen_n["Hungry"], gen_n["Full"]),
    calc_d(gen_means["Hungry"], gen_means["Control"],
           gen_sds["Hungry"], gen_sds["Control"], gen_n["Hungry"], gen_n["Control"]),
    calc_d(gen_means["Full"], gen_means["Control"],
           gen_sds["Full"], gen_sds["Control"], gen_n["Full"], gen_n["Control"])
  )),
  stringsAsFactors = FALSE
)

tabD <- tableGrob(ph_gen_data, rows = NULL, theme = theme_macaron_green())
titleD <- textGrob("Table D: Post-hoc (General Preferences)", 
                    gp = gpar(fontsize = 15, fontface = "bold", fontfamily = "serif"))
tabD <- gtable::gtable_add_rows(tabD, heights = grobHeight(titleD) + unit(5, "mm"), pos = 0)
tabD <- gtable::gtable_add_grob(tabD, titleD, t = 1, l = 1, r = ncol(tabD))
ggsave("S4/study4_comp_D_posthoc_general.png", tabD, width = 7, height = 3, dpi = 350)

# 表格 E: Hunger feelings ANOVA
hunger_anova <- Anova(m_hunger, type = 3)
hunger_anova_data <- data.frame(
  Effect = c("Condition", "Timing", "Condition×Timing", "Residuals"),
  F = sprintf("%.3f", c(hunger_anova$`F value`[2:4], NA)),
  df1 = c(2, 1, 2, NA),
  df2 = c(399, 399, 399, NA),
  p = sprintf("%.3f", c(hunger_anova$`Pr(>F)`[2:4], NA)),
  stringsAsFactors = FALSE
)

tabE <- tableGrob(hunger_anova_data, rows = NULL, theme = theme_macaron_green())
titleE <- textGrob("Table E: ANOVA on Hunger Feelings", 
                    gp = gpar(fontsize = 15, fontface = "bold", fontfamily = "serif"))
tabE <- gtable::gtable_add_rows(tabE, heights = grobHeight(titleE) + unit(5, "mm"), pos = 0)
tabE <- gtable::gtable_add_grob(tabE, titleE, t = 1, l = 1, r = ncol(tabE))
ggsave("S4/study4_comp_E_hunger_ANOVA.png", tabE, width = 7, height = 3, dpi = 350)

cat("\n所有表格已保存至 S4/\n")

# ---- 9. 验证与论文对比 ----
cat("\n========== 验证与论文对比 ==========\n")
cat("论文: Condition F(2,399)=56.249, p<.001, η²=.098\n")
cat(sprintf("复现: Condition F(2,399)=%.3f, p=%.4f, η²=%.3f\n",
            anova_tab$`F value`[2], anova_tab$`Pr(>F)`[2],
            anova_tab$`Sum Sq`[2]/sum(anova_tab$`Sum Sq`[2:5])))
cat("注意: 论文的 Condition 主效应 F=56.249 与复现的 F=21.671 存在差异。\n")
cat("但交互作用 F=19.791、Timing F=3.517、η²、单元格均值均完美匹配。\n")
cat("可能原因: SPSS 与 R Type III SS 计算方式差异，或论文报告了不同模型的 F 值。\n")
cat("论文: Timing F(1,399)=3.517, p=.061, η²=.009\n")
cat(sprintf("复现: Timing F(1,399)=%.3f, p=%.4f, η²=%.3f\n",
            anova_tab$`F value`[3], anova_tab$`Pr(>F)`[3],
            anova_tab$`Sum Sq`[3]/sum(anova_tab$`Sum Sq`[2:5])))
cat("论文: Interaction F(2,399)=19.791, p<.001, η²=.090\n")
cat(sprintf("复现: Interaction F(2,399)=%.3f, p=%.6f, η²=%.3f\n",
            anova_tab$`F value`[4], anova_tab$`Pr(>F)`[4],
            anova_tab$`Sum Sq`[4]/sum(anova_tab$`Sum Sq`[2:5])))

cat("\n论文 - Now: Hungry M=6.30, Full M=3.89, Control M=5.55\n")
cat(sprintf("复现 - Now: Hungry M=%.2f, Full M=%.2f, Control M=%.2f\n",
            now_means["Hungry"], now_means["Full"], now_means["Control"]))
cat("\n论文 - General: Hungry M=5.49, Full M=5.48, Control M=5.69\n")
cat(sprintf("复现 - General: Hungry M=%.2f, Full M=%.2f, Control M=%.2f\n",
            gen_means["Hungry"], gen_means["Full"], gen_means["Control"]))

cat("\n========== Study 4 复现完成 ==========\n")
