# ============================================================
# 生成论文全部结果表格图片
# Steinmetz et al. (2017) — 所有数据从原文提取
# ============================================================

library(gridExtra)
library(grid)
library(ggplot2)

# 辅助函数: 创建表格并保存为 PNG
save_table <- function(df, filename, title = NULL, fontsize = 3.2, 
                       col_width = NULL, row_height = 0.035) {
  
  # 创建主题
  theme_tt <- ttheme_minimal(
    core = list(
      fg_params = list(fontsize = fontsize, fontfamily = "sans"),
      bg_params = list(fill = c("white", "#F5F5F5"), alpha = 1)
    ),
    colhead = list(
      fg_params = list(fontsize = fontsize + 0.5, fontfamily = "sans", fontface = "bold"),
      bg_params = list(fill = "#2C3E50", alpha = 1)
    ),
    rowhead = list(
      fg_params = list(fontsize = fontsize, fontfamily = "sans", fontface = "bold"),
      bg_params = list(fill = "#ECF0F1", alpha = 1)
    ),
    padding = unit(c(4, 6), "mm")
  )
  
  # 移除行名
  tbl <- tableGrob(df, rows = NULL, theme = theme_tt)
  
  # 添加标题
  if (!is.null(title)) {
    title_grob <- textGrob(title, gp = gpar(fontsize = fontsize + 3, fontface = "bold", 
                                            fontfamily = "sans"),
                           just = "left", x = 0.02)
    tbl <- gtable::gtable_add_rows(tbl, heights = unit(0.8, "cm"), pos = 0)
    tbl <- gtable::gtable_add_grob(tbl, title_grob, t = 1, l = 1, r = ncol(df))
  }
  
  # 添加脚注
  # 计算表格尺寸
  tbl_widths <- sum(tbl$widths)
  tbl_heights <- sum(tbl$heights)
  
  ggsave(filename, plot = tbl, width = min(14, 6 + ncol(df) * 1.8), 
         height = min(20, 1.5 + nrow(df) * 0.45), dpi = 200, limitsize = FALSE)
  cat("  Saved:", filename, "\n")
}

# ============================================================
# Study 1a 表格
# ============================================================

cat("=== Study 1a ===\n")

# 表 1a-A: 描述统计与 t 检验
t1a_A <- data.frame(
  因变量 = c("取暖偏好 (1-9)", "", "感受温度 (1-9)", ""),
  条件 = c("Cold (n=60)", "Warm (n=59)", "Cold (n=60)", "Warm (n=59)"),
  M = c("6.23", "5.19", "5.11", "5.79"),
  SD = c("1.52", "1.73", "1.72", "1.36"),
  t = c("3.501", "", "2.375", ""),
  df = c("117", "", "117", ""),
  p = c(".001", "", ".019", ""),
  `Cohen's d` = c("0.64", "", "0.44", "")
)
save_table(t1a_A, "tables/study1a_A_desc.png", 
           "Study 1a — 表A: 描述统计与独立样本 t 检验")

# 表 1a-B: 回归分析
t1a_B <- data.frame(
  模型 = c("简单回归", "", "多元回归", "", ""),
  预测变量 = c("(截距)", "感受温度", "(截距)", "Condition (1=热,2=冷)", "感受温度"),
  β = c("—", "-0.256", "—", "0.906", "-0.203"),
  SE = c("—", "0.096", "—", "0.301", "0.095"),
  t = c("—", "2.755", "—", "3.013", "2.136"),
  p = c("—", ".007", "—", ".003", ".035"),
  `95% CI` = c("—", "[0.074, 0.455]", "—", "[0.311, 1.502]", "[0.015, 0.391]")
)
save_table(t1a_B, "tables/study1a_B_reg.png", 
           "Study 1a — 表B: 回归分析 (DV = 取暖偏好)")

# 表 1a-C: 中介
t1a_C <- data.frame(
  路径 = c("Condition → 感受温度 → 取暖偏好"),
  `间接效应` = c("Bootstrap 95% CI = [-0.057, 0.199]"),
  `Sobel's z` = c("1.80"),
  p = c(".072"),
  结论 = c("部分中介 (边缘显著)")
)
save_table(t1a_C, "tables/study1a_C_med.png", 
           "Study 1a — 表C: 中介分析")

# ============================================================
# Study 1b 表格
# ============================================================

cat("=== Study 1b ===\n")

# 表 1b-A: 操纵检查
t1b_A <- data.frame(
  条件 = c("模拟-Cold", "模拟-Warm", "启动-Cold", "启动-Warm"),
  M = c("8.28", "8.24", "5.48", "4.75"),
  SD = c("1.25", "1.43", "2.89", "2.56"),
  效应 = c("模拟 vs 启动 (主效应)", "Cold vs Warm (主效应)", "交互作用", ""),
  `F(1, 238)` = c("124.700", "1.914", "1.544", ""),
  p = c("< .001", ".168", ".215", ""),
  `η²` = c(".344", ".008", ".006", "")
)
save_table(t1b_A, "tables/study1b_A_manip.png", 
           "Study 1b — 表A: 操纵检查 (想象程度 1-9)")

# 表 1b-B: 偏好描述
t1b_B <- data.frame(
  条件 = c("模拟-Cold", "模拟-Warm", "启动-Cold", "启动-Warm"),
  M = c("6.09", "4.66", "5.70", "5.16"),
  SD = c("1.72", "1.94", "1.65", "1.96"),
  `α (信度)` = c(".64 (模拟组)", "", ".54 (启动组)", "")
)
save_table(t1b_B, "tables/study1b_B_desc.png", 
           "Study 1b — 表B: 取暖偏好描述统计")

# 表 1b-C: ANOVA + 简单效应
t1b_C <- data.frame(
  分析 = c("ANOVA: Cold vs Warm", "ANOVA: 模拟 vs 启动", "ANOVA: 交互作用", 
           "简单效应: 模拟组 Cold vs Warm", "简单效应: 启动组 Cold vs Warm"),
  `统计量` = c("F(1,238)=17.639", "F(1,238)=0.045", "F(1,238)=3.549",
             "p < .001", "p = .097"),
  p = c("< .001", ".832", ".061", "", ""),
  `η² / d` = c(".069", "< .001", ".015", "d = 0.78", "d = 0.30")
)
save_table(t1b_C, "tables/study1b_C_anova.png", 
           "Study 1b — 表C: ANOVA 与简单效应分析")

# ============================================================
# Study 2 表格
# ============================================================

cat("=== Study 2 ===\n")

# 表 2-A: 各单元格统计
t2_A <- data.frame(
  State = c("温度模拟", "温度模拟", "饥饿模拟", "饥饿模拟"),
  Intensity = c("热 (正向)", "冷 (负向)", "饱 (正向)", "饿 (负向)"),
  n = c("88", "89", "89", "88"),
  `取暖偏好 M(SD)` = c("4.93 (1.78)", "7.33 (1.44)", "6.24 (1.46)", "6.13 (1.69)"),
  `进食偏好 M(SD)` = c("5.18 (1.57)", "5.83 (1.64)", "4.32 (1.97)", "5.57 (1.82)")
)
save_table(t2_A, "tables/study2_A_desc.png", 
           "Study 2 — 表A: 各单元格描述统计 (N=354)")

# 表 2-B: 三路混合 ANOVA
t2_B <- data.frame(
  效应 = c("State", "Intensity", "State × Intensity", 
           "Preference (被试内)", "State × Preference", 
           "Intensity × Preference", "State × Intensity × Preference"),
  `F(1, 297)` = c("3.005", "52.590", "10.889",
                  "52.284", "5.532", "0.549", "36.112"),
  p = c(".084", "< .001", ".001",
        "< .001", ".019", ".460", "< .001"),
  `η²` = c(".010", ".150", ".035", ".150", ".018", ".002", ".108"),
  显著 = c("ns", "***", "**", "***", "*", "ns", "***")
)
save_table(t2_B, "tables/study2_B_mixed_anova.png", 
           "Study 2 — 表B: 2×2×2 混合重复测量 ANOVA")

# 表 2-C: pref_warmth ANOVA
t2_C <- data.frame(
  效应 = c("State", "Intensity", "State × Intensity",
           "简单效应: 温度组 Hot vs Cold", "简单效应: 饥饿组 Full vs Hungry"),
  `F / t` = c("F(1,297)=0.011", "F(1,297)=38.262", "F(1,297)=46.182",
              "t = -9.18", "t = 0.415"),
  p = c(".915", "< .001", "< .001", "< .001", ".678"),
  结论 = c("ns", "***", "***", "Cold > Hot ✅", "无差异 ❌")
)
save_table(t2_C, "tables/study2_C_warmth.png", 
           "Study 2 — 表C: 取暖偏好 ANOVA + 简单效应")

# 表 2-D: pref_food ANOVA
t2_D <- data.frame(
  效应 = c("State", "Intensity", "State × Intensity",
           "简单效应: 饥饿组 Full vs Hungry", "简单效应: 温度组 Hot vs Cold"),
  `F / t` = c("F(1,297)=7.366", "F(1,297)=22.132", "F(1,297)=2.157",
              "t = -4.369", "t = -2.280"),
  p = c(".007", "< .001", ".143", "< .001", ".023"),
  结论 = c("**", "***", "ns", "Hungry > Full ✅", "Cold > Hot ⚠意外")
)
save_table(t2_D, "tables/study2_D_food.png", 
           "Study 2 — 表D: 进食偏好 ANOVA + 简单效应")

# 表 2-E: 中介汇总
t2_E <- data.frame(
  组别 = c("温度组 (State=1)", "", "", "饥饿组 (State=2)", "", ""),
  分析 = c("操纵检查: Q51 ~ Condition", 
          "回归: pref_warmth ~ Q51",
          "中介: 感受温度 → 偏好",
          "操纵检查: hungry.0 ~ Condition",
          "回归: pref_food ~ hungry.0",
          "中介: 感受饥饿 → 偏好"),
  结果 = c("t(147)=3.633, p<.001 ✅",
          "β=-0.578, t=-5.491, p<.001, R²=.170",
          "95%CI=[-0.507,-0.107], Sobel z=3.033, p=.002 ✅",
          "t(149)=-3.224, p=.002 ✅",
          "β=0.463, t=7.721, p<.001, R²=.286",
          "95%CI=[0.178,0.847], Sobel z=2.979, p=.003 ✅")
)
save_table(t2_E, "tables/study2_E_mediation.png", 
           "Study 2 — 表E: 操纵检查与中介分析汇总")

# ============================================================
# Study 3 表格
# ============================================================

cat("=== Study 3 ===\n")

t3_A <- data.frame(
  因变量 = c("食物份量 (0-3)", "", "感受饥饿 (1-9)", ""),
  条件 = c("模拟饥饿", "模拟饱腹", "模拟饥饿", "模拟饱腹"),
  M = c("2.33", "1.88", "4.62", "4.07"),
  SD = c("0.86", "0.71", "2.32", "2.25"),
  t = c("3.031", "", "1.261", ""),
  df = c("109", "", "109", ""),
  p = c(".003", "", ".210", ""),
  `Cohen's d` = c("0.57", "", "0.24", ""),
  结论 = c("Hungry > Full ✅", "", "ns (power不足)", "")
)
save_table(t3_A, "tables/study3_A_desc.png", 
           "Study 3 — 表A: 描述统计与 t 检验")

t3_B <- data.frame(
  模型 = c("回归: 食物份量 ~ 感受饥饿", 
           "协变量: 距上次零食时间",
           "协变量: 距上次正餐时间",
           "含协变量后 simulation 效应"),
  `β / F` = c("β=0.137", "F(1,103)=4.673", "F(1,103)=0.120", "F(1,103)=7.405"),
  p = c("< .001", ".033", ".729", ".008"),
  结论 = c("感受饥饿→份量选择 ✅", "显著协变量", "不显著", "模拟效应仍显著 ✅")
)
save_table(t3_B, "tables/study3_B_reg.png", 
           "Study 3 — 表B: 回归与协变量分析")

# ============================================================
# Study 4 表格
# ============================================================

cat("=== Study 4 ===\n")

t4_A <- data.frame(
  条件 = c("模拟饥饿", "模拟饱腹", "控制组"),
  `即时偏好 M(SD)` = c("6.30 (1.63)", "3.89 (1.73)", "5.55 (1.64)"),
  `一般偏好 M(SD)` = c("5.49 (1.55)", "5.48 (1.54)", "5.69 (1.56)"),
  `感受-即时 M(SD)` = c("5.38 (2.16)", "3.74 (1.88)", "4.82 (2.06)"),
  `感受-一般 M(SD)` = c("5.00 (1.90)", "4.13 (1.99)", "4.21 (2.09)")
)
save_table(t4_A, "tables/study4_A_desc.png", 
           "Study 4 — 表A: 各条件描述统计 (N=405)")

t4_B <- data.frame(
  效应 = c("Simulation", "Timing (现在 vs 一般)", "Simulation × Timing"),
  `F` = c("56.249", "3.517", "19.791"),
  df = c("2, 399", "1, 399", "2, 399"),
  p = c("< .001", ".061", "< .001"),
  `η²` = c(".098", ".009", ".090"),
  结论 = c("***", "边缘显著", "*** ⭐关键发现")
)
save_table(t4_B, "tables/study4_B_anova.png", 
           "Study 4 — 表B: 两因素 ANOVA (进食偏好)")

t4_C <- data.frame(
  对比 = c("即时: 饥饿 vs 饱腹", "即时: 饥饿 vs 控制", "即时: 控制 vs 饱腹",
           "一般: 饥饿 vs 饱腹", "一般: 饥饿 vs 控制", "一般: 控制 vs 饱腹"),
  `均值差` = c("2.41", "0.75", "1.66",
               "0.01", "-0.20", "-0.21"),
  p = c("< .001", ".009", "< .001",
        ".971", ".474", ".450"),
  d = c("1.43", "0.46", "0.98",
        "0.01", "0.13", "0.14"),
  结论 = c("饥饿>饱腹 ✅", "饥饿>控制 ✅", "控制>饱腹 ✅",
          "无差异 ✅", "无差异 ✅", "无差异 ✅")
)
save_table(t4_C, "tables/study4_C_contrasts.png", 
           "Study 4 — 表C: 条件对比（即时 vs 一般偏好）")

t4_D <- data.frame(
  组别 = c("即时偏好组", "", "", "一般偏好组", "", ""),
  分析 = c("Simulation → 感受饥饿 (主效应)",
          "感受饥饿 → 偏好",
          "中介 (Bootstrap)",
          "Simulation → 感受饥饿 (主效应)",
          "感受饥饿 → 偏好",
          "中介 (Bootstrap)"),
  结果 = c("F(2,399)=54.799, p<.001",
          "β=0.488, t=8.963, p<.001",
          "95%CI=[-0.361, 0.005], Sobel z=-1.729, p=.084",
          "F(2,399)=54.799, p<.001",
          "β=0.067, t=1.241, p=.216",
          "不适用 (直接效应不存在)")
)
save_table(t4_D, "tables/study4_D_mediation.png", 
           "Study 4 — 表D: 中介分析 (即时 vs 一般)")

# ============================================================
# Study 5 表格
# ============================================================

cat("=== Study 5 ===\n")

t5_A <- data.frame(
  条件 = c("模拟饥饿 → 相似他人", "模拟饱腹 → 相似他人",
           "模拟饥饿 → 不相似他人", "模拟饱腹 → 不相似他人"),
  `投射饥饿感 M(SD)` = c("5.37 (1.84)", "4.53 (1.97)",
                       "4.78 (1.90)", "5.30 (1.72)"),
  n = c("50", "50", "50", "50")
)
save_table(t5_A, "tables/study5_A_desc.png", 
           "Study 5 — 表A: 各条件描述统计 (N=200)")

t5_B <- data.frame(
  效应 = c("Simulation (主效应)", "Similarity (主效应)", 
           "Simulation × Similarity",
           "简单效应: 相似他人 Hungry vs Full",
           "简单效应: 不相似他人 Hungry vs Full"),
  `F / t` = c("F(1,196)=0.374", "F(1,196)=0.118",
              "F(1,196)=6.657", "t = — (p = .028)", "t = — (p = .157)"),
  p = c(".541", ".732", ".011", ".028", ".157"),
  `η² / d` = c(".002", ".001", ".033", "d = 0.44", "d = 0.29"),
  结论 = c("ns", "ns", "* ⭐交互显著", "投射 ✅", "不投射 ✅")
)
save_table(t5_B, "tables/study5_B_anova.png", 
           "Study 5 — 表B: ANOVA 与简单效应")

# ============================================================
# 元分析汇总
# ============================================================

cat("=== Meta-analysis ===\n")

t_meta <- data.frame(
  分析 = c("模拟 → 当前感受 (Studies 1a-4)",
           "模拟 → 偏好/行为 (Studies 1a-4)"),
  `Effect Size` = c("0.87 (SE=0.13)", "1.50 (SE=0.33)"),
  z = c("6.48", "4.53"),
  p = c("< .001", "< .001"),
  结论 = c("模拟显著影响当前感受 ✅", "模拟显著影响偏好/行为 ✅")
)
save_table(t_meta, "tables/meta_analysis.png", 
           "跨研究元分析 (单论文元分析)")

cat("\n===== 全部表格生成完毕 =====")
cat("\n共生成 20 个表格图片，保存在 tables/ 文件夹\n")
