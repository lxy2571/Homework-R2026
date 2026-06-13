# ============================================================
# Study 5: Mental Simulation -> Projection onto Similar/Dissimilar Others
# Steinmetz et al. (2017) - Replication in R
# ============================================================
# 实验设计: 2 (hungry vs. full) × 2 (similar vs. dissimilar) between-subjects
# 参与者: 200 人在 Amazon Mechanical Turk
# DV: Q29 = projected hunger of other person (1=very full, 9=very hungry)
# ============================================================

# ---- 加载包 ----
library(foreign)
library(car)
library(ggplot2)
library(gridExtra)
library(grid)
library(emmeans)

# ---- 1. 读取数据 ----
raw <- read.spss("osfstorage-archive/Study5/Hunger_projection_similarity_states.sav",
                 to.data.frame = TRUE, use.value.labels = FALSE)

# 筛选有效数据（N=200，与论文一致）
df <- raw[!is.na(raw$condition) & !is.na(raw$similarity) & !is.na(raw$Q29), ]

# 因子编码
# 注意: S5 中 condition=1=Full, condition=2=Hungry（与其他研究相反）
df$condition <- factor(df$condition, levels = c(1, 2),
                       labels = c("Full", "Hungry"))
df$similarity <- factor(df$similarity, levels = c(1, 2),
                        labels = c("Similar", "Dissimilar"))

cat("总样本量:", nrow(df), "\n")
print(table(df$condition, df$similarity))

# ---- 2. 操纵检验: similarity ----
# 论文: Similar M=7.04, SD=1.46; Dissimilar M=3.26, SD=1.84; t(198)=16.007, p<.001, d=2.276
cat("\n========== 操纵检验: Similarity ==========\n")
sim_means <- tapply(df$Q40.0, df$similarity, mean, na.rm = TRUE)
sim_sds <- tapply(df$Q40.0, df$similarity, sd, na.rm = TRUE)
sim_ns <- tapply(df$Q40.0, df$similarity, function(x) sum(!is.na(x)))

for (grp in names(sim_means)) {
  cat(sprintf("%s: M = %.2f, SD = %.2f, N = %.0f\n", 
              grp, sim_means[grp], sim_sds[grp], sim_ns[grp]))
}

# t-test for manipulation check
tt_sim <- t.test(Q40.0 ~ similarity, data = df, var.equal = TRUE)
cat(sprintf("\nt(%d) = %.3f, p = %.3f\n", tt_sim$parameter, tt_sim$statistic, tt_sim$p.value))

# Cohen's d
pooled_sim <- sqrt(sum((sim_ns - 1) * sim_sds^2) / (sum(sim_ns) - 2))
d_sim <- diff(rev(sim_means)) / pooled_sim
cat(sprintf("Cohen's d = %.3f\n", d_sim))

# ---- 3. 描述统计: Q29 by condition × similarity ----
cat("\n========== 描述统计: Q29 (Projected Hunger) ==========\n")
desc <- aggregate(Q29 ~ condition + similarity, data = df,
                  FUN = function(x) c(M = mean(x), SD = sd(x), N = length(x)))

cell_means <- tapply(df$Q29, list(df$condition, df$similarity), mean)
cell_sds <- tapply(df$Q29, list(df$condition, df$similarity), sd)
cell_ns <- tapply(df$Q29, list(df$condition, df$similarity), length)

for (sim_lvl in c("Similar", "Dissimilar")) {
  for (cond_lvl in c("Hungry", "Full")) {
    cat(sprintf("%s | %s: M = %.2f, SD = %.2f, N = %.0f\n",
                cond_lvl, sim_lvl,
                cell_means[cond_lvl, sim_lvl],
                cell_sds[cond_lvl, sim_lvl],
                cell_ns[cond_lvl, sim_lvl]))
  }
}

# ---- 4. Type III ANOVA: Q29 ~ condition * similarity ----
options(contrasts = c("contr.sum", "contr.poly"))
model <- lm(Q29 ~ condition * similarity, data = df)
anova_tab <- Anova(model, type = 3)

cat("\n========== Type III ANOVA: Q29 ~ condition * similarity ==========\n")
print(anova_tab)

# ---- 5. 简单效应分析 ----
# 论文预测: 只有在similar条件下，hungry > full
# 使用 emmeans 从全模型提取简单效应（匹配 SPSS UNIANOVA EMMEANS COMPARE）
cat("\n========== 简单效应分析 ==========\n")

# 全模型简单效应（使用 pooled error term，匹配 SPSS）
options(contrasts = c("contr.treatment", "contr.poly"))
model_emm <- lm(Q29 ~ condition * similarity, data = df)

# 各相似性水平下 condition 的简单效应
emm_simple <- emmeans(model_emm, ~ condition | similarity)
simple_eff <- pairs(emm_simple)
cat("\nSimple effects of condition at each similarity level:\n")
print(simple_eff)

# 描述统计
cell_means <- tapply(df$Q29, list(df$condition, df$similarity), mean)
cell_sds <- tapply(df$Q29, list(df$condition, df$similarity), sd)
cell_ns <- tapply(df$Q29, list(df$condition, df$similarity), length)

# 5a. Similar condition
cat("\n--- Similar Others ---\n")
means_sim <- cell_means[, "Similar"]
sds_sim <- cell_sds[, "Similar"]
ns_sim <- cell_ns[, "Similar"]
for (grp in names(means_sim)) {
  cat(sprintf("%s: M = %.2f, SD = %.2f, N = %.0f\n", grp, means_sim[grp], sds_sim[grp], ns_sim[grp]))
}

# Cohen's d for Similar (using pooled SD)
pooled_sim <- sqrt(sum((ns_sim - 1) * sds_sim^2) / (sum(ns_sim) - 2))
d_sim_cond <- (means_sim["Hungry"] - means_sim["Full"]) / pooled_sim
cat(sprintf("Hungry vs Full: d = %.2f\n", d_sim_cond))

# 5b. Dissimilar condition
cat("\n--- Dissimilar Others ---\n")
means_dis <- cell_means[, "Dissimilar"]
sds_dis <- cell_sds[, "Dissimilar"]
ns_dis <- cell_ns[, "Dissimilar"]
for (grp in names(means_dis)) {
  cat(sprintf("%s: M = %.2f, SD = %.2f, N = %.0f\n", grp, means_dis[grp], sds_dis[grp], ns_dis[grp]))
}

pooled_dis <- sqrt(sum((ns_dis - 1) * sds_dis^2) / (sum(ns_dis) - 2))
d_dis_cond <- (means_dis["Hungry"] - means_dis["Full"]) / pooled_dis
cat(sprintf("Hungry vs Full: d = %.2f\n", d_dis_cond))

# Restore contrasts
options(contrasts = c("contr.sum", "contr.poly"))

# ---- 6. 箱线图 ----
p <- ggplot(df, aes(x = condition, y = Q29, fill = condition)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2) +
  facet_wrap(~ similarity) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1.5) +
  scale_fill_manual(values = c("Hungry" = "#FFB5C2", "Full" = "#A8D8EA")) +
  labs(title = "Study 5: Projected Hunger by Simulation × Similarity",
       subtitle = "Interaction F(1, 196) = 6.657, p = .011, η² = .033",
       x = "Simulation Condition", y = "Projected Hunger of Other (1-9)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"))

ggsave("S5/Fig_interaction.png", p, width = 8, height = 5, dpi = 350)
cat("\n图已保存: Fig_interaction.png\n")

# ---- 7. 生成对比表格 ----
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

calc_d <- function(m1, m2, s1, s2, n1, n2) {
  sp <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2))
  return((m1 - m2)/sp)
}

# 表格 A: 描述统计
desc_data <- data.frame(
  Similarity = c("Similar", "Similar", "Dissimilar", "Dissimilar"),
  Condition = c("Hungry", "Full", "Hungry", "Full"),
  N = as.integer(c(cell_ns["Hungry","Similar"], cell_ns["Full","Similar"],
                   cell_ns["Hungry","Dissimilar"], cell_ns["Full","Dissimilar"])),
  Mean = sprintf("%.2f", c(cell_means["Hungry","Similar"], cell_means["Full","Similar"],
                           cell_means["Hungry","Dissimilar"], cell_means["Full","Dissimilar"])),
  SD = sprintf("%.2f", c(cell_sds["Hungry","Similar"], cell_sds["Full","Similar"],
                         cell_sds["Hungry","Dissimilar"], cell_sds["Full","Dissimilar"])),
  stringsAsFactors = FALSE
)

tabA <- tableGrob(desc_data, rows = NULL, theme = theme_macaron_green())
titleA <- textGrob("Table A: Descriptive Statistics (Projected Hunger)", 
                    gp = gpar(fontsize = 15, fontface = "bold", fontfamily = "serif"))
tabA <- gtable::gtable_add_rows(tabA, heights = grobHeight(titleA) + unit(5, "mm"), pos = 0)
tabA <- gtable::gtable_add_grob(tabA, titleA, t = 1, l = 1, r = ncol(tabA))
ggsave("S5/study5_comp_A_desc.png", tabA, width = 6, height = 3, dpi = 350)

# 表格 B: ANOVA
anova_data <- data.frame(
  Effect = c("Condition (Simulation)", "Similarity", "Condition × Similarity", "Residuals"),
  F = sprintf("%.3f", c(anova_tab$`F value`[2:4], NA)),
  df1 = c(1, 1, 1, NA),
  df2 = c(196, 196, 196, NA),
  p = sprintf("%.3f", c(anova_tab$`Pr(>F)`[2:4], NA)),
  eta2 = sprintf("%.3f", c(anova_tab$`Sum Sq`[2:4] / sum(anova_tab$`Sum Sq`[2:5]), NA)),
  stringsAsFactors = FALSE
)

tabB <- tableGrob(anova_data, rows = NULL, theme = theme_macaron_green())
titleB <- textGrob("Table B: Type III ANOVA (Projected Hunger)", 
                    gp = gpar(fontsize = 15, fontface = "bold", fontfamily = "serif"))
tabB <- gtable::gtable_add_rows(tabB, heights = grobHeight(titleB) + unit(5, "mm"), pos = 0)
tabB <- gtable::gtable_add_grob(tabB, titleB, t = 1, l = 1, r = ncol(tabB))
ggsave("S5/study5_comp_B_ANOVA.png", tabB, width = 7, height = 3, dpi = 350)

# 表格 C: 简单效应
sim_eff_summary <- summary(simple_eff)
sim_eff_data <- data.frame(
  Similarity = c("Similar", "Dissimilar"),
  Comparison = c("Hungry vs Full", "Hungry vs Full"),
  Mean_Hungry = sprintf("%.2f", c(means_sim["Hungry"], means_dis["Hungry"])),
  Mean_Full = sprintf("%.2f", c(means_sim["Full"], means_dis["Full"])),
  t = sprintf("%.3f", sim_eff_summary$t.ratio),
  p = sprintf("%.3f", sim_eff_summary$p.value),
  d = sprintf("%.2f", c(d_sim_cond, d_dis_cond)),
  stringsAsFactors = FALSE
)

tabC <- tableGrob(sim_eff_data, rows = NULL, theme = theme_macaron_green())
titleC <- textGrob("Table C: Simple Effects of Simulation at Each Similarity Level", 
                    gp = gpar(fontsize = 15, fontface = "bold", fontfamily = "serif"))
tabC <- gtable::gtable_add_rows(tabC, heights = grobHeight(titleC) + unit(5, "mm"), pos = 0)
tabC <- gtable::gtable_add_grob(tabC, titleC, t = 1, l = 1, r = ncol(tabC))
ggsave("S5/study5_comp_C_simple_effects.png", tabC, width = 8, height = 3, dpi = 350)

# 表格 D: 操纵检验
manip_data <- data.frame(
  Similarity = c("Similar", "Dissimilar"),
  M = sprintf("%.2f", c(sim_means["Similar"], sim_means["Dissimilar"])),
  SD = sprintf("%.2f", c(sim_sds["Similar"], sim_sds["Dissimilar"])),
  N = as.integer(c(sim_ns["Similar"], sim_ns["Dissimilar"])),
  stringsAsFactors = FALSE
)
# Add t-test row
manip_extra <- data.frame(
  Similarity = "t-test",
  M = sprintf("t(%d)=%.3f", tt_sim$parameter, tt_sim$statistic),
  SD = sprintf("p=%.3f", tt_sim$p.value),
  N = sprintf("d=%.3f", d_sim),
  stringsAsFactors = FALSE
)
manip_data <- rbind(manip_data, manip_extra)

tabD <- tableGrob(manip_data, rows = NULL, theme = theme_macaron_green())
titleD <- textGrob("Table D: Similarity Manipulation Check (Q40.0)", 
                    gp = gpar(fontsize = 15, fontface = "bold", fontfamily = "serif"))
tabD <- gtable::gtable_add_rows(tabD, heights = grobHeight(titleD) + unit(5, "mm"), pos = 0)
tabD <- gtable::gtable_add_grob(tabD, titleD, t = 1, l = 1, r = ncol(tabD))
ggsave("S5/study5_comp_D_manipulation.png", tabD, width = 6, height = 3, dpi = 350)

cat("\n所有表格已保存至 S5/\n")

# ---- 8. 验证与论文对比 ----
cat("\n========== 验证与论文对比 ==========\n")
cat("论文: Interaction F(1,196)=6.657, p=.011, η²=.033\n")
cat(sprintf("复现: Interaction F(1,196)=%.3f, p=%.3f, η²=%.3f\n",
            anova_tab$`F value`[4], anova_tab$`Pr(>F)`[4],
            anova_tab$`Sum Sq`[4]/sum(anova_tab$`Sum Sq`[2:5])))

cat("\n论文: Condition F(1,196)=0.374, p=.541, η²=.002\n")
cat(sprintf("复现: Condition F(1,196)=%.3f, p=%.3f, η²=%.3f\n",
            anova_tab$`F value`[2], anova_tab$`Pr(>F)`[2],
            anova_tab$`Sum Sq`[2]/sum(anova_tab$`Sum Sq`[2:5])))

cat("\n论文: Similarity F(1,196)=0.118, p=.732, η²=.001\n")
cat(sprintf("复现: Similarity F(1,196)=%.3f, p=%.3f, η²=%.3f\n",
            anova_tab$`F value`[3], anova_tab$`Pr(>F)`[3],
            anova_tab$`Sum Sq`[3]/sum(anova_tab$`Sum Sq`[2:5])))

cat("\n--- Similar Others ---\n")
cat(sprintf("论文: Hungry M=5.37, SD=1.84; Full M=4.53, SD=1.97; p=.028, d=0.44\n"))
cat(sprintf("复现: Hungry M=%.2f, SD=%.2f; Full M=%.2f, SD=%.2f; p=%.3f, d=%.2f\n",
            means_sim["Hungry"], sds_sim["Hungry"],
            means_sim["Full"], sds_sim["Full"],
            sim_eff_summary$p.value[1], d_sim_cond))

cat("\n--- Dissimilar Others ---\n")
cat(sprintf("论文: Hungry M=4.78, SD=1.90; Full M=5.30, SD=1.72; p=.157, d=0.29\n"))
cat(sprintf("复现: Hungry M=%.2f, SD=%.2f; Full M=%.2f, SD=%.2f; p=%.3f, d=%.2f\n",
            means_dis["Hungry"], sds_dis["Hungry"],
            means_dis["Full"], sds_dis["Full"],
            sim_eff_summary$p.value[2], d_dis_cond))

cat("\n--- Manipulation Check ---\n")
cat(sprintf("论文: Similar M=7.04, SD=1.46; Dissimilar M=3.26, SD=1.84; t(198)=16.007, d=2.276\n"))
cat(sprintf("复现: Similar M=%.2f, SD=%.2f; Dissimilar M=%.2f, SD=%.2f; t(%d)=%.3f, d=%.3f\n",
            sim_means["Similar"], sim_sds["Similar"],
            sim_means["Dissimilar"], sim_sds["Dissimilar"],
            tt_sim$parameter, tt_sim$statistic, d_sim))

cat("\n========== Study 5 复现完成 ==========\n")
