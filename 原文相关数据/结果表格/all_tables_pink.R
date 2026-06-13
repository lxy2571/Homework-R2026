# ============================================================
# 生成论文全部结果表格图片 (Macaron 粉嫩版)
# Steinmetz et al. (2017)
# ============================================================

library(gridExtra)
library(grid)
library(ggplot2)
library(RColorBrewer)

# === Macaron 配色方案 ===
macaron_pink <- "#FFB5C2"      # 主粉色
macaron_light <- "#FFE4E9"     # 浅粉背景
macaron_dark <- "#E8839A"      # 深粉表头
macaron_white <- "#FFF9FA"     # 极浅粉白
macaron_gray <- "#F7F0F2"      # 浅灰粉间隔
macaron_text <- "#5C3D46"      # 深褐粉文字
macaron_border <- "#F5CBD5"    # 边框粉
macaron_accent <- "#FFD4E0"    # 强调粉色

# 辅助函数: 创建 Macaron 风格表格并保存为 PNG
save_table <- function(df, filename, title = NULL, fontsize = 4.5,
                       title_size = 6.5, note = NULL) {
  
  # 清理列名中的点
  colnames(df) <- gsub("\\.", " ", colnames(df))
  
  # 构建核心主题
  theme_tt <- ttheme_minimal(
    core = list(
      fg_params = list(fontsize = fontsize, fontfamily = "sans", 
                       col = macaron_text, hjust = 0.5, x = 0.5),
      bg_params = list(fill = rep(c(macaron_white, macaron_gray), 
                                  length.out = nrow(df)),
                       alpha = 1, 
                       col = macaron_border, 
                       lwd = 0.8)
    ),
    colhead = list(
      fg_params = list(fontsize = fontsize + 1.5, fontfamily = "sans", 
                       fontface = "bold", col = "white", hjust = 0.5, x = 0.5),
      bg_params = list(fill = macaron_dark, alpha = 1, 
                       col = macaron_dark, lwd = 1.2)
    ),
    padding = unit(c(6, 8), "mm")
  )

  # 构建 grob
  tbl <- tableGrob(df, rows = NULL, theme = theme_tt)
  
  # 添加标题
  if (!is.null(title)) {
    title_grob <- textGrob(
      title, 
      gp = gpar(fontsize = title_size, fontface = "bold", 
                fontfamily = "sans", col = macaron_dark),
      just = "left", x = 0.02
    )
    tbl <- gtable::gtable_add_rows(tbl, heights = unit(1.0, "cm"), pos = 0)
    tbl <- gtable::gtable_add_grob(tbl, title_grob, 
                                   t = 1, l = 1, r = ncol(df))
  }
  
  # 添加脚注
  if (!is.null(note)) {
    # 先加一行空行
    tbl <- gtable::gtable_add_rows(tbl, heights = unit(0.01, "cm"))
    note_grob <- textGrob(
      note,
      gp = gpar(fontsize = fontsize - 1, fontfamily = "sans", 
                col = "#999999", fontface = "italic"),
      just = "left", x = 0.02
    )
    tbl <- gtable::gtable_add_rows(tbl, heights = unit(0.6, "cm"))
    tbl <- gtable::gtable_add_grob(tbl, note_grob,
                                   t = nrow(tbl), l = 1, r = ncol(df))
  }
  
  # 表格宽度自适应
  n_cols <- ncol(df)
  base_w <- 3.5
  per_col <- 2.0
  max_w <- 18
  
  tbl_w <- min(max_w, base_w + n_cols * per_col)
  
  # 高度自适应
  n_rows <- nrow(df) + 1  # +1 for header
  if (!is.null(title)) n_rows <- n_rows + 1
  if (!is.null(note)) n_rows <- n_rows + 1
  
  base_h <- 1.2
  per_row <- 0.55
  tbl_h <- base_h + n_rows * per_row
  
  ggsave(filename, plot = tbl, 
         width = tbl_w, height = tbl_h, 
         dpi = 350, limitsize = FALSE, bg = "white")
  cat("  ✓", filename, "\n")
}


# ============================================================
# Study 1a 表格
# ============================================================

cat("=== Study 1a ===\n")

t1a_A <- data.frame(
  因变量 = c("取暖偏好(1-9)", "", "感受温度(1-9)", ""),
  条件 = c("Cold (n=60)", "Warm (n=59)", "Cold (n=60)", "Warm (n=59)"),
  M = c("6.23", "5.19", "5.11", "5.79"),
  SD = c("1.52", "1.73", "1.72", "1.36"),
  t值 = c("3.501", "—", "2.375", "—"),
  自由度 = c("117", "—", "117", "—"),
  p值 = c(".001", "—", ".019", "—"),
  `Cohen's d` = c("0.64", "—", "0.44", "—"),
  结论 = c("Cold > Warm ***", "", "Cold < Warm *", "")
)
save_table(t1a_A, "tables/study1a_A_desc.png", 
           "🌸 Study 1a — 表A: 描述统计与独立样本 t 检验",
           note = "N=119 学生被试, 单因素被试间设计 (Cold vs Warm)")

t1a_B <- data.frame(
  模型 = c("① 简单回归", "", "② 多元回归", "", ""),
  预测变量 = c("截距", "感受温度→偏好", "截距", "Condition (热1冷2)", "感受温度→偏好"),
  β = c("—", "−0.256**", "—", "0.906**", "−0.203*"),
  SE = c("—", "0.096", "—", "0.301", "0.095"),
  t值 = c("—", "2.755", "—", "3.013", "2.136"),
  p值 = c("—", ".007", "—", ".003", ".035"),
  `R²` = c(".066", "", ".220", "", "")
)
save_table(t1a_B, "tables/study1a_B_reg.png", 
           "🌸 Study 1a — 表B: 回归分析 (DV=取暖偏好)",
           note = "β为未标准化系数; *p<.05, **p<.01, ***p<.001")

t1a_C <- data.frame(
  路径 = c("Condition → 感受温度 → 取暖偏好"),
  间接效应 = c("Bootstrap 95% CI = [−0.057, 0.199]"),
  `Sobel's z` = c("1.80"),
  p值 = c(".072"),
  结论 = c("部分中介 (边缘显著, p=.072)")
)
save_table(t1a_C, "tables/study1a_C_med.png", 
           "🌸 Study 1a — 表C: 中介分析",
           note = "Bootstrapping 1000 次; CI 包含 0 表示中介不显著")

# ============================================================
# Study 1b 表格
# ============================================================

cat("=== Study 1b ===\n")

t1b_A <- data.frame(
  条件 = c("模拟 − Cold", "模拟 − Warm", "启动 − Cold", "启动 − Warm"),
  想象程度_M = c("8.28", "8.24", "5.48", "4.75"),
  想象程度_SD = c("1.25", "1.43", "2.89", "2.56"),
  效应 = c("模拟 vs 启动 (主效应)", "Cold vs Warm (主效应)", "交互作用", ""),
  F值 = c("124.700***", "1.914", "1.544", ""),
  p值 = c("<.001", ".168", ".215", ""),
  `η²` = c(".344", ".008", ".006", "")
)
save_table(t1b_A, "tables/study1b_A_manip.png", 
           "🌸 Study 1b — 表A: 操纵检查 (想象程度 1-9, N=242)",
           note = "***p<.001; 模拟组想象程度显著高于启动组")

t1b_B <- data.frame(
  条件 = c("模拟 − Cold", "模拟 − Warm", "启动 − Cold", "启动 − Warm"),
  M = c("6.09", "4.66", "5.70", "5.16"),
  SD = c("1.72", "1.94", "1.65", "1.96"),
  信度_α = c(".64 (模拟组)", "", ".54 (启动组)", ""),
  n = c("60", "61", "60", "61")
)
save_table(t1b_B, "tables/study1b_B_desc.png", 
           "🌸 Study 1b — 表B: 取暖偏好描述统计",
           note = "2×2 被试间设计 (模拟vs启动 × ColdvsWarm)")

t1b_C <- data.frame(
  分析 = c("冷vs暖 (主效应)", "模拟vs启动 (主效应)", "交互作用",
           "简单效应: 模拟组 Cold vs Warm", "简单效应: 启动组 Cold vs Warm"),
  F或t = c("F=17.639***", "F=0.045", "F=3.549†",
           "p < .001", "p = .097"),
  `η²或d` = c(".069", "<.001", ".015",
             "d = 0.78", "d = 0.30"),
  结论 = c("Cold>Warm ***", "ns", "边缘显著 †",
           "✅ 模拟效应显著", "❌ 启动效应不显著")
)
save_table(t1b_C, "tables/study1b_C_anova.png", 
           "🌸 Study 1b — 表C: ANOVA 与简单效应分析",
           note = "†p<.10, ***p<.001; 模拟效应(d=0.78)大于启动效应(d=0.30)")

# ============================================================
# Study 2 表格
# ============================================================

cat("=== Study 2 ===\n")

t2_A <- data.frame(
  模拟状态 = c("🌡️ 温度", "🌡️ 温度", "🍽️ 饥饿", "🍽️ 饥饿"),
  强度方向 = c("热 (正向)", "冷 (负向)", "饱 (正向)", "饿 (负向)"),
  n = c("88", "89", "89", "88"),
  取暖偏好 = c("4.93 (1.78)", "7.33 (1.44)", "6.24 (1.46)", "6.13 (1.69)"),
  进食偏好 = c("5.18 (1.57)", "5.83 (1.64)", "4.32 (1.97)", "5.57 (1.82)")
)
save_table(t2_A, "tables/study2_A_desc.png", 
           "🌸 Study 2 — 表A: 各单元格描述统计 (N=354)",
           note = "均值 (标准差); 评分范围 1-9, 越高越偏好")

t2_B <- data.frame(
  效应 = c("State (温度vs饥饿)", "Intensity (正向vs负向)", "State × Intensity",
           "Preference 被试内", "State × Preference",
           "Intensity × Preference", "★ State × Intensity × Preference"),
  `F(1, 297)` = c("3.005", "52.590", "10.889",
                  "52.284", "5.532", "0.549", "36.112"),
  p = c(".084", "<.001", ".001",
        "<.001", ".019", ".460", "<.001"),
  `η²` = c(".010", ".150", ".035",
           ".150", ".018", ".002", ".108"),
  显著 = c("ns", "***", "**", "***", "*", "ns", "*** ⭐")
)
save_table(t2_B, "tables/study2_B_mixed_anova.png", 
           "🌸 Study 2 — 表B: 2×2×2 混合重复测量 ANOVA",
           note = "⭐ 三路交互显著: 模拟效应取决于状态和偏好类型的匹配")

t2_C <- data.frame(
  效应 = c("State", "Intensity", "State × Intensity ★",
           "简单效应: 温度组 Hot vs Cold",
           "简单效应: 饥饿组 Full vs Hungry"),
  F或t = c("F=0.011", "F=38.262***", "F=46.182***",
           "t = −9.18***", "t = 0.415"),
  p = c(".915", "<.001", "<.001", "<.001", ".678"),
  结论 = c("ns", "Intensity→偏好", "交互显著 ⭐",
           "✅ Cold > Hot (d=1.50)", "❌ 无差异")
)
save_table(t2_C, "tables/study2_C_warmth.png", 
           "🌸 Study 2 — 表C: 取暖偏好 ANOVA + 简单效应",
           note = "DV = pref_warmth; 只有温度组内 Cold vs Hot 有显著差异")

t2_D <- data.frame(
  效应 = c("State ★", "Intensity", "State × Intensity",
           "简单效应: 饥饿组 Full vs Hungry",
           "简单效应: 温度组 Hot vs Cold ⚠"),
  F或t = c("F=7.366**", "F=22.132***", "F=2.157",
           "t = −4.369***", "t = −2.280*"),
  p = c(".007", "<.001", ".143", "<.001", ".023"),
  结论 = c("饥饿组>温度组", "负向>正向", "ns",
           "✅ Hungry > Full", "⚠️ Cold > Hot (意外)")
)
save_table(t2_D, "tables/study2_D_food.png", 
           "🌸 Study 2 — 表D: 进食偏好 ANOVA + 简单效应",
           note = "⚠️ 意外发现: 模拟冷也增加了进食偏好 (冷→饿→吃)")

t2_E <- data.frame(
  组别 = c("🌡️ 温度组", "", "", "🍽️ 饥饿组", "", ""),
  分析 = c("操纵检查: 感受温度~Condition",
           "回归: pref_warmth~感受温度",
           "中介: 感受温度→偏好",
           "操纵检查: 感受饥饿~Condition",
           "回归: pref_food~感受饥饿",
           "中介: 感受饥饿→偏好"),
  统计结果 = c("t=3.633***",
            "β=−0.578***, R²=.170",
            "95%CI[−0.507,−0.107], z=3.033**",
            "t=−3.224**",
            "β=0.463***, R²=.286",
            "95%CI[0.178,0.847], z=2.979**"),
  结论 = c("✅", "✅", "✅ 中介成立",
           "✅", "✅", "✅ 中介成立")
)
save_table(t2_E, "tables/study2_E_mediation.png", 
           "🌸 Study 2 — 表E: 操纵检查与中介分析汇总",
           note = "*p<.05, **p<.01, ***p<.001; 两组的模拟效应均通过当前感受中介")

# ============================================================
# Study 3 表格
# ============================================================

cat("=== Study 3 ===\n")

t3_A <- data.frame(
  因变量 = c("食物份量(0-3)", "", "感受饥饿(1-9)", ""),
  条件 = c("模拟饥饿", "模拟饱腹", "模拟饥饿", "模拟饱腹"),
  M = c("2.33", "1.88", "4.62", "4.07"),
  SD = c("0.86", "0.71", "2.32", "2.25"),
  t值 = c("3.031**", "—", "1.261", "—"),
  df = c("109", "—", "109", "—"),
  `Cohen's d` = c("0.57", "—", "0.24", "—"),
  结论 = c("✅ Hungry>Full", "", "ns (power不足)", "")
)
save_table(t3_A, "tables/study3_A_desc.png", 
           "🌸 Study 3 — 表A: 描述统计与 t 检验 (N=111)",
           note = "食物份量: 0=不要, 1=小, 2=中, 3=大; 3项食品平均(α=.66)")

t3_B <- data.frame(
  分析 = c("回归: 食物份量~感受饥饿",
           "协变量: 距上次零食时间",
           "协变量: 距上次正餐时间",
           "含协变量后 Simulation 效应"),
  β或F = c("β=0.137***", "F(1,103)=4.673*", "F(1,103)=0.120", "F(1,103)=7.405**"),
  p值 = c("<.001", ".033", ".729", ".008"),
  结论 = c("感受饥饿→大份量", "显著协变量", "不显著", "模拟效应仍显著✅")
)
save_table(t3_B, "tables/study3_B_reg.png", 
           "🌸 Study 3 — 表B: 回归与协变量分析",
           note = "*p<.05, **p<.01, ***p<.001")

# ============================================================
# Study 4 表格
# ============================================================

cat("=== Study 4 ===\n")

t4_A <- data.frame(
  条件 = c("🍽️ 模拟饥饿", "🍽️ 模拟饱腹", "⚪ 控制组"),
  `即时偏好 M(SD)` = c("6.30 (1.63)", "3.89 (1.73)", "5.55 (1.64)"),
  `一般偏好 M(SD)` = c("5.49 (1.55)", "5.48 (1.54)", "5.69 (1.56)"),
  `即时组−感受饥饿` = c("5.38 (2.16)", "3.74 (1.88)", "4.82 (2.06)"),
  `一般组−感受饥饿` = c("5.00 (1.90)", "4.13 (1.99)", "4.21 (2.09)")
)
save_table(t4_A, "tables/study4_A_desc.png", 
           "🌸 Study 4 — 表A: 各条件描述统计 (N=405)",
           note = "3×2 被试间设计 (饥饿/饱腹/控制 × 即时/一般)")

t4_B <- data.frame(
  效应 = c("Simulation (饥饿/饱腹/控制)", "Timing (即时 vs 一般)",
           "★ Simulation × Timing"),
  F值 = c("56.249***", "3.517†", "19.791***"),
  df = c("2, 399", "1, 399", "2, 399"),
  `η²` = c(".098", ".009", ".090"),
  关键发现 = c("模拟影响偏好", "边缘显著", "⭐交互显著!")
)
save_table(t4_B, "tables/study4_B_anova.png", 
           "🌸 Study 4 — 表B: 两因素 ANOVA (进食偏好)",
           note = "†p<.10, ***p<.001; ⭐ 交互是核心发现: 模拟只影响即时偏好")

t4_C <- data.frame(
  对比 = c("即时: 饥饿 vs 饱腹", "即时: 饥饿 vs 控制", "即时: 控制 vs 饱腹",
           "一般: 饥饿 vs 饱腹", "一般: 饥饿 vs 控制", "一般: 控制 vs 饱腹"),
  均值差 = c("2.41", "0.75", "1.66",
             "0.01", "−0.20", "−0.21"),
  p = c("<.001", ".009", "<.001",
        ".971", ".474", ".450"),
  d = c("1.43", "0.46", "0.98",
        "0.01", "0.13", "0.14"),
  结果 = c("✅ ***", "✅ **", "✅ ***",
           "❌ ns", "❌ ns", "❌ ns")
)
save_table(t4_C, "tables/study4_C_contrasts.png", 
           "🌸 Study 4 — 表C: 条件对比 (即时 vs 一般偏好)",
           note = "即时偏好: 饥饿>控制>饱腹; 一般偏好: 三组无差异 ⭐")

t4_D <- data.frame(
  条件 = c("⏱ 即时偏好组", "", "⏱ 即时偏好组",
           "📅 一般偏好组", "", "📅 一般偏好组"),
  路径 = c("Simulation→感受饥饿",
           "感受饥饿→偏好",
           "中介 (Bootstrap)",
           "Simulation→感受饥饿",
           "感受饥饿→偏好",
           "中介"),
  结果 = c("F=54.799***",
           "β=0.488***",
           "95%CI[−0.361, 0.005]",
           "F=54.799***",
           "β=0.067 (ns)",
           "不适用(直接效应不存在)"),
  结论 = c("感受受影响", "感受预测偏好", "部分中介(p=.084)",
           "感受受影响", "❌ 感受不预测一般偏好", "✅ 与预测一致")
)
save_table(t4_D, "tables/study4_D_mediation.png", 
           "🌸 Study 4 — 表D: 中介分析 (即时 vs 一般偏好)",
           note = "模拟影响感受(两组皆然), 但只有即时偏好组用感受指导偏好 ⭐")

# ============================================================
# Study 5 表格
# ============================================================

cat("=== Study 5 ===\n")

t5_A <- data.frame(
  模拟条件 = c("模拟饥饿", "模拟饱腹", "模拟饥饿", "模拟饱腹"),
  他人类型 = c("👤 相似他人", "👤 相似他人", "👥 不相似他人", "👥 不相似他人"),
  n = c("50", "50", "50", "50"),
  投射饥饿感 = c("5.37 (1.84)", "4.53 (1.97)", "4.78 (1.90)", "5.30 (1.72)")
)
save_table(t5_A, "tables/study5_A_desc.png", 
           "🌸 Study 5 — 表A: 各条件描述统计 (N=200)",
           note = "DV: 认为他人多饿 (1=很饱, 9=很饿)")

t5_B <- data.frame(
  效应 = c("Simulation (饿vs饱)", "Similarity (相似vs不相似)",
           "★ Simulation × Similarity",
           "简单效应: 相似他人 Hungry vs Full",
           "简单效应: 不相似他人 Hungry vs Full"),
  F或t = c("F=0.374", "F=0.118",
           "F=6.657*", "p = .028*", "p = .157"),
  p = c(".541", ".732", ".011", ".028", ".157"),
  d = c("—", "—", "—", "0.44", "0.29"),
  结论 = c("ns", "ns", "⭐ 交互显著",
           "✅ 向相似他人投射", "❌ 不向不相似他人投射")
)
save_table(t5_B, "tables/study5_B_anova.png", 
           "🌸 Study 5 — 表B: ANOVA 与简单效应",
           note = "*p<.05; 与真实内脏体验模式一致: 只投射给相似他人 ⭐")

# ============================================================
# 元分析汇总
# ============================================================

cat("=== Meta-analysis ===\n")

t_meta <- data.frame(
  分析内容 = c("模拟 → 当前感受 (Studies 1a~4)",
             "模拟 → 偏好/行为 (Studies 1a~4)"),
  `Effect Size(SE)` = c("0.87 (0.13)", "1.50 (0.33)"),
  z值 = c("6.48", "4.53"),
  p值 = c("< .001", "< .001"),
  结论 = c("✅ 模拟显著影响当前感受", "✅ 模拟显著影响偏好/行为")
)
save_table(t_meta, "tables/meta_analysis.png", 
           "🌸 跨研究单论文元分析 (Single-Paper Meta-Analysis)",
           note = "McShane & Bockenholt (2017) 方法; 两项元分析均显著 ⭐")

# ============================================================
cat("\n========================================\n")
cat("🌸 全部 20 张 Macaron 粉嫩表格已生成!\n")
cat("   保存在 tables/ 文件夹 (DPI=350)\n")
cat("========================================\n")
