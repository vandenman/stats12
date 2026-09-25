# Generate tables/t-table.csv: two-tailed critical values of the t
# distribution, rounded to three decimals.
#
# Run with:  Rscript --vanilla make-t-table.R   (from inside the practice folder)

setwd("/home/don/github/education/stats12/2627/misc/practice")

df <- c(1:30, 40, 50, 60, 80, 100, 120)
alphas <- c(0.20, 0.10, 0.05, 0.02, 0.01)

out <- data.frame(df = df, check.names = FALSE)
for (alpha in alphas) {
  column <- sprintf("%.3f", qt(1 - alpha / 2, df))
  out[[sprintf("%.2f", alpha)]] <- column
}

write.csv(out, "tables/t-table.csv", row.names = FALSE, quote = FALSE)
cat("wrote tables/t-table.csv with", nrow(out), "rows\n")
