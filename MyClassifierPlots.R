fontgrey_str <- "#444444"
green_str <- "#78b45a"
classifier_theme <- theme()

calibration_plot1 <-function (test.y, pred.prob) 
{
  green_str <- "#78b45a"
  nbuckets = 10
  bucket_array <- seq(0, 1, by = 0.1)
  positive_in_band <- function(bucket) {
    in_bucket_indicator <- pred.prob >= bucket_array[bucket] & 
      pred.prob < bucket_array[bucket + 1]
    bucket_size <- sum(in_bucket_indicator)
    positive <- sum(test.y[in_bucket_indicator] == 1)
    return(qbeta(c(llb = 0.025, lb = 0.25, y = 0.5, ub = 0.75, 
                   uub = 0.965), 0.5 + positive, 0.5 + bucket_size - 
                   positive))
  }
  tbl <- data.table(bucket = 1:nbuckets, percentage = 5 + bucket_array[1:nbuckets] * 
                      100, blb = bucket_array[1:nbuckets], bub = bucket_array[(1:nbuckets) + 
                                                                                1])
  tbl <- cbind(tbl, 100 * t(sapply(tbl$bucket, positive_in_band)))
  ggplot(tbl, aes(x = percentage, y = y)) + geom_ribbon(aes(ymin = llb, 
                                                            ymax = uub), fill = green_str, alpha = 0.2) + 
    geom_ribbon(aes(ymin = lb, ymax = ub), fill = green_str, 
                alpha = 0.4) + geom_abline(slope = 1, intercept = 0, 
                                           linetype = "dotted") + scale_x_continuous(name = "Predicted probability (%)", 
                                                                                     limits = c(0, 100), breaks = seq(5, 95, 10)) + scale_y_continuous(name = "Smoothed true probability (%)", 
                                                                                                                                                       limits = c(0, 100)) + ggtitle("Calibration")
}

calculate_auc <- function(test.y, pred.prob) {
  n <- length(test.y)
  
  print("(AUC) Sorting data ...")
  test.y.bin <- test.y == 1
  roc_tbl <- data.table(y=test.y.bin, preds=pred.prob)
  roc_tbl <- roc_tbl[order(preds)]
  
  npositives <- as.double(sum(test.y.bin))
  nnegatives <- as.double(n - npositives)
  
  print("(AUC) Calculating ranks ...")
  # Main AUC calcuation. We use the MW-U stat equivalence,
  # since it's a little faster to calculate.
  # Note that tied predictions are given a rank equal to the mean of the tied set.
  roc_tbl[, rank := mean(.I), by=preds]
  
  r1 <- roc_tbl[y == T, sum(rank)]
  u1 <- r1 - (npositives*(npositives+1))/2.0
  auc <- 100*u1/(npositives*nnegatives)
  return(auc)
}

roc_plot1 <- function (test.y, pred.prob, resamps = 2000, force_bootstrap = NULL) 
{
  n <- length(test.y)
  test.y.bin <- test.y == 1
  nbins <- 50
  npositives <- sum(test.y.bin)
  nnegatives <- n - npositives
  negative_steps <- floor(nnegatives/nbins)
  print("Calculating AUC ...")
  auc <- calculate_auc(test.y, pred.prob)
  print(paste("AUC:", auc))
  big_data_cutoff <- 50000
  if (!is.null(force_bootstrap)) {
    bootstrap <- force_bootstrap
  }
  else {
    bootstrap <- n <= big_data_cutoff
  }
  pos_pred_probs <- -pred.prob[test.y.bin]
  neg_pred_probs <- -pred.prob[!test.y.bin]
  if (bootstrap) {
    print("Bootstrapping ROC curves")
    pos_pred_boots <- pos_pred_probs[c(caret::createResample(pos_pred_probs, 
                                                             times = resamps, list = F))]
    neg_pred_boots <- neg_pred_probs[c(caret::createResample(neg_pred_probs, 
                                                             times = resamps, list = F))]
    roc_tbl <- data.table(preds = c(pos_pred_boots, neg_pred_boots), 
                          y = c(rep(T, length(pos_pred_boots)), rep(F, length(neg_pred_boots))), 
                          resample = c(rep(1:resamps, each = length(pos_pred_probs)), 
                                       rep(1:resamps, each = length(neg_pred_probs))))
    setkey(roc_tbl, "resample", "preds")
    roc_tbl[, `:=`(tp, cumsum(y)), by = resample]
    roc_tbl[, `:=`(fp, cumsum(!y)), by = resample]
    roc_tbl[, `:=`(fpr_step, ((fp%%negative_steps) == 
                                0)), by = resample]
    substeps_tbl <- roc_tbl[fpr_step == T, ]
    subind <- substeps_tbl[, .I[.N], by = c("resample", 
                                            "fp")]
    roc_tbl_sub <- substeps_tbl[subind$V1]
    roc_tbl_sub_stats <- roc_tbl_sub[, as.list(quantile(tp, 
                                                        c(0.025, 0.5, 0.975))), keyby = fp]
    print("Eval AUC")
    roc_tbl[, `:=`(rank, mean(.I)), by = c("resample", 
                                           "preds")]
    r1 <- roc_tbl[y == T, sum(rank) - .N * n * (resample - 
                                                  1), keyby = "resample"]$V1
    u1 <- r1 - (npositives * (npositives + 1))/2
    aucs <- 1 - u1/(npositives * nnegatives)
    auc_bounds <- 100 * quantile(aucs, c(0.025, 0.5, 0.975))
    digits_use <- 3
    if (format(auc_bounds[1], digits = digits_use) == format(auc_bounds[3], 
                                                             digits = digits_use)) {
      digits_use <- 5
    }
  }
  else {
    roc_tbl <- data.table(preds = c(pos_pred_probs, neg_pred_probs), 
                          y = c(rep(T, length(pos_pred_probs)), rep(F, length(neg_pred_probs))))
    setkey(roc_tbl, "preds")
    roc_tbl[, `:=`(tp, cumsum(y))]
    roc_tbl[, `:=`(fp, cumsum(!y))]
    roc_tbl[, `:=`(fpr_step, ((fp%%negative_steps) == 
                                0))]
    substeps_tbl <- roc_tbl[fpr_step == T, ]
    subind <- substeps_tbl[, .I[.N], by = c("fp")]
    roc_tbl_sub_stats <- substeps_tbl[subind$V1]
    roc_tbl_sub_stats[, `:=`(`50%`, tp)]
  }
  print("Producing ROC plot")
  plt <- ggplot(roc_tbl_sub_stats, aes(x = 100 * fp/nnegatives, 
                                       y = 100 * `50%`/npositives)) + geom_line(color = green_str, 
                                                                                size = 1.5) + geom_abline(slope = 1, intercept = 0, linetype = "dotted") + 
    annotate("text", x = 70, y = 30, label = paste0("AUC ", 
                                                        format(auc, digits = 3), "%"), parse = F, size = 7, 
             colour = fontgrey_str) + scale_x_continuous(name = "1-Specificity (%))", 
                                                         limits = c(0, 100), expand = c(0, 0.3)) + scale_y_continuous(name = "Sensitivity (%)", 
                                                                                                                      limits = c(0, 100), expand = c(0, 0.3)) + classifier_theme
  if (bootstrap) {
    plt <- plt + geom_ribbon(aes(ymin = 100 * `2.5%`/npositives, 
                                 ymax = 100 * `97.5%`/npositives), fill = green_str, 
                             alpha = 0.2) + annotate("text", x = 70, 
                                                     y = 23, label = paste0("95% CI: ", format(auc_bounds[1], 
                                                                                               digits = digits_use), "% - ", format(auc_bounds[3], 
                                                                                                                                    digits = digits_use), "%"), parse = F, 
                                                     size = 4.5, colour = fontgrey_str)
  }
  return(plt + ggtitle("ROC"))
}
