# ============================================================
# Study 3: Mental Simulation -> Actual Food Portion Choices
# Steinmetz et al. (2017) - Replication in R
# ============================================================
# 实验设计: 2 (simulate: hungry vs. full) between-subjects
# 参与者: 111 人在北美博物馆招募
# 流程: 想象饥饿/饱足→做6个消费选择(3个零食+3个填充)→报告当前感受
# 关键DV: choice_size = mean(popcorn, ice_cream, chips), 0=none, 1=small, 2=medium, 3=large
# ============================================================

# ---- 加载包 ----
library(foreign)  # 读 SPSS .sav 文件
library(ggplot2)  # 作图
library(gridExtra) # 表格

# ---- 1. 读取数据 ----
raw <- read.spss("osfstorage-archive/Study3/S3.sav",
                 to.data.frame = TRUE,
                 use.value.labels = FALSE)
cat("原始数据行数:", nrow(raw), "\n")

# 移除缺失条件的行（Qualtrics 元数据行）
df <- raw[!is.na(raw$condition), ]
cat("有效数据行数:", nrow(df), "\n")

# 因子编码
df$condition <- factor(df$condition, levels = c(1, 2), 
                       labels = c("Hungry", "Full"))
# 注意: SPSS 中 1=hungry, 2=full（由 T-TEST GROUPS=condition(1 2) 确认）

# ---- 2. 描述统计：各组 choice_size 均值 ----
means <- tapply(df$choice_size, df$condition, mean, na.rm = TRUE)
sds   <- tapply(df$choice_size, df$condition, sd, na.rm = TRUE)
ns    <- tapply(df$choice_size, df$condition, function(x) sum(!is.na(x)))

cat("\n========== 描述统计: choice_size ==========\n")
for (grp in names(means)) {
  cat(sprintf("%s: M = %.2f, SD = %.2f, N = %d\n", grp, means[grp], sds[grp], ns[grp]))
}

# ---- 3. t-test: Hungry vs. Full on choice_size ----
tt <- t.test(choice_size ~ condition, data = df, var.equal = TRUE)
cat("\n========== t-test: choice_size ~ condition ==========\n")
cat(sprintf("t(%d) = %.3f, p = %.3f\n", tt$parameter, tt$statistic, tt$p.value))
cat(sprintf("均值差 = %.3f, 95%% CI = [%.3f, %.3f]\n",
            diff(rev(means)), tt$conf.int[1], tt$conf.int[2]))

# Cohen's d
pooled_sd <- sqrt(sum((ns - 1) * sds^2) / (sum(ns) - 2))
d_val <- diff(rev(means)) / pooled_sd
cat(sprintf("Cohen's d = %.2f\n", d_val))

# ---- 4. t-test: Hungry vs. Full on hunger feelings ----
# 论文: hungry (M=4.62, SD=2.32), full (M=4.07, SD=2.25), t(109)=1.261, p=.210, d=0.24
hungry_means <- tapply(df$hungry, df$condition, mean, na.rm = TRUE)
hungry_sds   <- tapply(df$hungry, df$condition, sd, na.rm = TRUE)
hungry_ns    <- tapply(df$hungry, df$condition, function(x) sum(!is.na(x)))

cat("\n========== 描述统计: hungry feelings ==========\n")
for (grp in names(hungry_means)) {
  cat(sprintf("%s: M = %.2f, SD = %.2f, N = %d\n", 
              grp, hungry_means[grp], hungry_sds[grp], hungry_ns[grp]))
}

tt_h <- t.test(hungry ~ condition, data = df, var.equal = TRUE)
cat("\n========== t-test: hungry feelings ~ condition ==========\n")
cat(sprintf("t(%d) = %.3f, p = %.3f\n", tt_h$parameter, tt_h$statistic, tt_h$p.value))

# Cohen's d for hunger
pooled_sd_h <- sqrt(sum((hungry_ns - 1) * hungry_sds^2) / (sum(hungry_ns) - 2))
d_h <- diff(rev(hungry_means)) / pooled_sd_h
cat(sprintf("Cohen's d = %.2f\n", d_h))

# ---- 5. 回归: choice_size ~ hungry ----
# 论文: β=0.137, SE=0.031, t(110)=4.346, p<.001
reg <- lm(choice_size ~ hungry, data = df)
reg_summary <- summary(reg)
cat("\n========== 回归: choice_size ~ hungry ==========\n")
print(coef(reg_summary))

# ---- 6. 箱线图: condition vs choice_size ----
p <- ggplot(df, aes(x = condition, y = choice_size, fill = condition)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
  scale_fill_manual(values = c("Hungry" = "#FFB5C2", "Full" = "#A8D8EA")) +
  labs(title = "Study 3: Simulation Condition vs Food Portion Choice",
       subtitle = "Hungry (M=2.33) vs Full (M=1.88), t(109)=3.031, p=.003, d=0.57",
       x = "Simulation Condition", y = "Food Portion Size (0-3)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"))

ggsave("S3/Fig_condition_vs_choice.png", p, width = 6, height = 5, dpi = 350)
cat("\n箱线图已保存至 S3/Fig_condition_vs_choice.png\n")

# ---- 7. 生成对比表格 ----
# 表格 A: 描述统计对比
t_test_result <- data.frame(
  Condition = c("Hungry", "Full"),
  N = as.integer(ns),
  Mean = round(means, 2),
  SD = round(sds, 2),
  stringsAsFactors = FALSE
)

# 使用 gridExtra 生成表格
library(grid)

theme_macaron_green <- function() {
  t <- ttheme_minimal(
    core = list(bg_params = list(fill = c("#F2FBF5", "#E8F3ED"), 
                                  col = "#5DAE85", lwd = 1.5),
                fg_params = list(fontface = "plain", fontsize = 14, 
                                 col = "#2D5A42", fontfamily = "serif")),
    colhead = list(bg_params = list(fill = "#5DAE85", col = "#5DAE85", lwd = 1.5),
                   fg_params = list(fontface = "bold", fontsize = 15, 
                                    col = "white", fontfamily = "serif")),
    rowhead = list(fg_params = list(fontface = "bold", fontsize = 13, 
                                    col = "#2D5A42", fontfamily = "serif"))
  )
  return(t)
}

# 表格 A: 描述统计
tabA <- tableGrob(t_test_result, rows = NULL, theme = theme_macaron_green())
titleA <- textGrob("Table A: Descriptive Statistics (Food Portion Size)", 
                    gp = gpar(fontsize = 16, fontface = "bold", fontfamily = "serif"))
tabA <- gtable::gtable_add_rows(tabA, heights = grobHeight(titleA) + unit(5, "mm"), pos = 0)
tabA <- gtable::gtable_add_grob(tabA, titleA, t = 1, l = 1, r = ncol(tabA))
ggsave("S3/study3_comp_A_desc.png", tabA, width = 5, height = 3, dpi = 350)

# 表格 B: t-test 结果
tabB_data <- data.frame(
  Comparison = c("choice_size: Hungry vs Full",
                 "hunger feelings: Hungry vs Full"),
  t = sprintf("%.3f", c(tt$statistic, tt_h$statistic)),
  df = as.integer(c(tt$parameter, tt_h$parameter)),
  p = sprintf("%.3f", c(tt$p.value, tt_h$p.value)),
  d = sprintf("%.2f", c(d_val, d_h)),
  CI95 = c(sprintf("[%.3f, %.3f]", tt$conf.int[1], tt$conf.int[2]),
           sprintf("[%.3f, %.3f]", tt_h$conf.int[1], tt_h$conf.int[2])),
  stringsAsFactors = FALSE
)

tabB <- tableGrob(tabB_data, rows = NULL, theme = theme_macaron_green())
titleB <- textGrob("Table B: t-test Results", 
                    gp = gpar(fontsize = 16, fontface = "bold", fontfamily = "serif"))
tabB <- gtable::gtable_add_rows(tabB, heights = grobHeight(titleB) + unit(5, "mm"), pos = 0)
tabB <- gtable::gtable_add_grob(tabB, titleB, t = 1, l = 1, r = ncol(tabB))
ggsave("S3/study3_comp_B_ttest.png", tabB, width = 8, height = 3.5, dpi = 350)

# 表格 C: 回归结果
reg_coef <- coef(reg_summary)
tabC_data <- data.frame(
  Predictor = c("Intercept", "Hungry Feeling"),
  Beta = sprintf("%.3f", reg_coef[, 1]),
  SE = sprintf("%.3f", reg_coef[, 2]),
  t = sprintf("%.3f", reg_coef[, 3]),
  p = sprintf("%.4f", reg_coef[, 4]),
  stringsAsFactors = FALSE
)

tabC <- tableGrob(tabC_data, rows = NULL, theme = theme_macaron_green())
titleC <- textGrob("Table C: Regression: Portion Size ~ Hungry Feeling",
                    gp = gpar(fontsize = 16, fontface = "bold", fontfamily = "serif"))
tabC <- gtable::gtable_add_rows(tabC, heights = grobHeight(titleC) + unit(5, "mm"), pos = 0)
tabC <- gtable::gtable_add_grob(tabC, titleC, t = 1, l = 1, r = ncol(tabC))
ggsave("S3/study3_comp_C_reg.png", tabC, width = 7, height = 3, dpi = 350)

# ---- 8. 汇总验证 ----
cat("\n========== 验证与论文对比 ==========\n")
cat(sprintf("论文: Hungry M=2.33, SD=0.86 | 复现: M=%.2f, SD=%.2f\n", 
            means["Hungry"], sds["Hungry"]))
cat(sprintf("论文: Full M=1.88, SD=0.71 | 复现: M=%.2f, SD=%.2f\n", 
            means["Full"], sds["Full"]))
cat(sprintf("论文: t(109)=3.031, p=.003, d=0.57 | 复现: t(%d)=%.3f, p=%.3f, d=%.2f\n",
            tt$parameter, tt$statistic, tt$p.value, d_val))
cat(sprintf("论文: Hungry Hunger M=4.62, SD=2.32 | 复现: M=%.2f, SD=%.2f\n",
            hungry_means["Hungry"], hungry_sds["Hungry"]))
cat(sprintf("论文: Full Hunger M=4.07, SD=2.25 | 复现: M=%.2f, SD=%.2f\n",
            hungry_means["Full"], hungry_sds["Full"]))
cat(sprintf("论文: Hunger t(109)=1.261, p=.210, d=0.24 | 复现: t(%d)=%.3f, p=%.3f, d=%.2f\n",
            tt_h$parameter, tt_h$statistic, tt_h$p.value, d_h))
cat(sprintf("论文: Regression β=0.137, SE=0.031, t(110)=4.346, p<.001\n"))
cat(sprintf("复现: Regression β=%.3f, SE=%.3f, t=%.3f, p=%.4f\n",
            reg_coef[2, 1], reg_coef[2, 2], reg_coef[2, 3], reg_coef[2, 4]))

cat("\n========== Study 3 复现完成 ==========\n")
