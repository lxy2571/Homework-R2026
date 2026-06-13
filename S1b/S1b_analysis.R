# ============================================================
# Study 1b 复现
# 论文: Steinmetz et al. (2017)
# "Mental Simulation of Visceral States Affects Preferences
#  and Behavior"
# ------------------------------------------------------------
# 实验设计:
#   2 (simulation vs priming) × 2 (cold vs warm) 被试间设计
#   N = 242 (MTurk)
# 
# 变量:
#   condition: 1=simulation, 2=priming
#   hot_cold:  1=warm, 2=cold
#   choices:   取暖偏好 (A4_rev, A6_rev, A10_rev, A3, A8 均值)
#   Q41.0:     操纵检查 (想象自己在画面中的程度 1-9)
#   Q46.0:     生动性评分 (1-9)
# ============================================================

library(haven)
library(psych)

# ---- 1. 读取数据 ----
raw <- read_sav("osfstorage-archive/Study1/Study1b/S1b_1.sav")

# 转为纯数值并筛选完整数据
raw$condition <- as.numeric(raw[["condition"]])
raw$hot_cold  <- as.numeric(raw[["hot_cold"]])
raw$choices   <- as.numeric(raw[["choices"]])
raw$Q41.0     <- as.numeric(raw[["Q41.0"]])
raw$Q46.0     <- as.numeric(raw[["Q46.0"]])

# 筛选完整记录
idx <- complete.cases(raw[, c("condition", "hot_cold", "choices")])
df <- raw[idx, ]
cat("有效被试:", nrow(df), "\n")

# 分组合名
df$group <- paste0(
  ifelse(df$condition == 1, "模拟", "启动"),
  "-",
  ifelse(df$hot_cold == 1, "暖", "冷")
)

# ---- 2. 描述统计 ----
cat("\n===== 各单元描述统计 (choices) =====\n")
desc <- aggregate(choices ~ condition + hot_cold, data = df,
                  FUN = function(x) c(n = length(x), M = mean(x), SD = sd(x)))
print(desc)

# 同 paper 格式：
for (i in 1:nrow(desc)) {
  grp <- ifelse(desc$condition[i]==1, "模拟", "启动")
  tmp <- ifelse(desc$hot_cold[i]==1, "暖", "冷")
  cat(sprintf("  %s-%s: n=%d, M=%.2f, SD=%.2f\n",
              grp, tmp,
              desc$choices[i][[1]]["n"],
              desc$choices[i][[1]]["M"],
              desc$choices[i][[1]]["SD"]))
}

# ---- 3. 信度分析 (split by condition) ----
cat("\n===== 信度分析 =====\n")
for (cond_val in 1:2) {
  grp <- ifelse(cond_val == 1, "模拟组", "启动组")
  cat("\n---", grp, "---\n")
  sub <- df[df$condition == cond_val, ]
  items <- sub[, c("A4rev", "A6rev", "A10rev", "A3", "A8")]
  items[] <- lapply(items, as.numeric)
  alpha <- psych::alpha(items)
  cat(sprintf("  Cronbach's α = %.2f\n", alpha$total$raw_alpha))
}

# ---- 4. 操纵检查 ANOVA: Q41.0 ~ condition * hot_cold ----
cat("\n===== 操纵检查 ANOVA (Q41.0) =====\n")
df$c_fac <- factor(df$condition, labels = c("模拟", "启动"))
df$h_fac <- factor(df$hot_cold, labels = c("暖", "冷"))

aov_manip <- aov(Q41.0 ~ c_fac * h_fac, data = df)
s <- summary(aov_manip)
print(s)

# 各单元均值
cat("\nQ41.0 各单元均值:\n")
print(tapply(df$Q41.0, list(df$c_fac, df$h_fac), mean, na.rm = TRUE))
print(tapply(df$Q41.0, list(df$c_fac, df$h_fac), sd, na.rm = TRUE))

# ---- 5. 两因素 ANOVA: choices ~ condition * hot_cold ----
cat("\n===== 2x2 ANOVA (choices) =====\n")
aov_choice <- aov(choices ~ c_fac * h_fac, data = df)
print(summary(aov_choice))

# ---- 6. 简单效应分析 ----
cat("\n===== 简单效应分析 =====\n")

# 模拟组内: warm vs cold
sim <- df[df$condition == 1, ]
t_sim <- t.test(choices ~ hot_cold, data = sim, var.equal = TRUE)
cat("\n模拟组: Cold vs Warm\n")
cat(sprintf("  t(%d) = %.3f, p = %.3f\n", t_sim$parameter, abs(t_sim$statistic), t_sim$p.value))
cat(sprintf("  Cold M=%.2f(SD=%.2f), Warm M=%.2f(SD=%.2f)\n",
            mean(sim$choices[sim$hot_cold==2]), sd(sim$choices[sim$hot_cold==2]),
            mean(sim$choices[sim$hot_cold==1]), sd(sim$choices[sim$hot_cold==1])))
# Cohen's d
pooled_sd <- sqrt(((sum(sim$hot_cold==2)-1)*var(sim$choices[sim$hot_cold==2]) + 
                    (sum(sim$hot_cold==1)-1)*var(sim$choices[sim$hot_cold==1])) / 
                   (sum(sim$hot_cold==2) + sum(sim$hot_cold==1) - 2))
d_sim <- (mean(sim$choices[sim$hot_cold==2]) - mean(sim$choices[sim$hot_cold==1])) / pooled_sd
cat(sprintf("  Cohen's d = %.2f\n", d_sim))

# 启动组内: warm vs cold
prim <- df[df$condition == 2, ]
t_prim <- t.test(choices ~ hot_cold, data = prim, var.equal = TRUE)
cat("\n启动组: Cold vs Warm\n")
cat(sprintf("  t(%d) = %.3f, p = %.3f\n", t_prim$parameter, abs(t_prim$statistic), t_prim$p.value))
cat(sprintf("  Cold M=%.2f(SD=%.2f), Warm M=%.2f(SD=%.2f)\n",
            mean(prim$choices[prim$hot_cold==2]), sd(prim$choices[prim$hot_cold==2]),
            mean(prim$choices[prim$hot_cold==1]), sd(prim$choices[prim$hot_cold==1])))
pooled_sd2 <- sqrt(((sum(prim$hot_cold==2)-1)*var(prim$choices[prim$hot_cold==2]) + 
                     (sum(prim$hot_cold==1)-1)*var(prim$choices[prim$hot_cold==1])) / 
                    (sum(prim$hot_cold==2) + sum(prim$hot_cold==1) - 2))
d_prim <- (mean(prim$choices[prim$hot_cold==2]) - mean(prim$choices[prim$hot_cold==1])) / pooled_sd2
cat(sprintf("  Cohen's d = %.2f\n", d_prim))

# ---- 7. 回归分析: choices ~ Q46.0 (生动性) ----
cat("\n===== 回归: choices ~ 生动性(Q46.0) =====\n")
lm_vivid <- lm(choices ~ Q46.0, data = df)
print(summary(lm_vivid))

# ---- 8. 与原文结果对比验证 ----
cat("\n===== 与原文对比 =====\n")
cat("原文模拟组: Cold M=6.09(SD=1.72), Warm M=4.66(SD=1.94), d=0.78\n")
cat(sprintf("复现模拟组: Cold M=%.2f(SD=%.2f), Warm M=%.2f(SD=%.2f), d=%.2f\n\n",
            mean(sim$choices[sim$hot_cold==2]), sd(sim$choices[sim$hot_cold==2]),
            mean(sim$choices[sim$hot_cold==1]), sd(sim$choices[sim$hot_cold==1]),
            d_sim))
cat("原文启动组: Cold M=5.70(SD=1.65), Warm M=5.16(SD=1.96), d=0.30\n")
cat(sprintf("复现启动组: Cold M=%.2f(SD=%.2f), Warm M=%.2f(SD=%.2f), d=%.2f\n\n",
            mean(prim$choices[prim$hot_cold==2]), sd(prim$choices[prim$hot_cold==2]),
            mean(prim$choices[prim$hot_cold==1]), sd(prim$choices[prim$hot_cold==1]),
            d_prim))

cat("原文主效应 Cold>Warm: F(1,238)=17.639, p<.001, η²=.069\n")
cat(sprintf("复现: F(1,238)=%.3f\n", summary(aov_choice)[[1]]$`F value`[1]))

cat("原文交互: F(1,238)=3.549, p=.061, η²=.015\n")
cat(sprintf("复现: F(1,238)=%.3f, p=%.3f\n", 
            summary(aov_choice)[[1]]$`F value`[3],
            summary(aov_choice)[[1]]$`Pr(>F)`[3]))

cat("\n===== 分析完成 =====\n")
