#' Log Response Ratio and Effect Size Calculation
#'
#' Computes the log response ratio (LRR) between treatment and control values,
#' along with confidence intervals, Cohen's d, and Hedges' g. The function
#' supports grouped analyses and automatically removes rows containing missing
#' or non-finite treatment or control values prior to calculation.
#'
#' @param data A data frame containing treatment, control, and optional grouping variables.
#' @param trt Unquoted column name in \code{data} representing the treatment values.
#' @param ctrl Unquoted column name in \code{data} representing the control values.
#' @param group Optional character vector of column names used for grouping
#'   (e.g., \code{c("Trial", "Block")}). If \code{NULL}, all observations are
#'   analyzed together.
#' @param conf.level Confidence level for the LRR confidence interval. Defaults
#'   to \code{0.95}.
#'
#' @returns
#' A data frame containing, for each group:
#' \itemize{
#'   \item \code{n_T}: Number of non-missing treatment observations.
#'   \item \code{n_C}: Number of non-missing control observations.
#'   \item \code{n_LRR}: Number of paired observations used to compute LRR.
#'   \item \code{LRR}: Mean log response ratio.
#'   \item \code{LRR_L}: Lower confidence limit of LRR.
#'   \item \code{LRR_U}: Upper confidence limit of LRR.
#'   \item \code{Cohen_d}: Cohen's d standardized mean difference.
#'   \item \code{Hedges_g}: Hedges' g bias-corrected effect size.
#' }
#'
#' @export
#'
#' @examples
#' # install the package
#' if(!require(remotes)) install.packages("remotes")
#' if (!requireNamespace("lrr", quietly= TRUE)) {
#'  remotes::install_github("agronomy4future/lrr", force= TRUE)
#'}
#' library(remotes)
#' library(lrr)
#'
#' # data example
#' df= data.frame(Cultivar= rep(c("CV1", "CV2", "CV3", "CV4", "CV5",
#' "CV6", "CV7", "CV8"), each = 4L), Block= rep(c("I", "II", "III", "IV"), 8),
#' N0= c(608.9, 464.7, 199.3, 930, 623.7, 43.1, 200.3, 207.7, 1243.7, 776.7,
#' 1424.5, 1556.4, 352.2, 1316.7, 377.7, 980.8, 660.9, 1535.5, 1009.2, 1490.8,
#' 692.6, 1273.9, 1010.7, 1105.8, 695, 869, 396, 1147, 883.9, 786.6, 416.2, 1512.6),
#' N1= c(697.6, 601.9, 1063.4, 440.6, 313, 109.9, 119.9, 281.3, 3463.1, 1364.8, 1991.9,
#' 1264, 1837.9, 1382.5, 1160.5, 248.9, 1679.7, 1704.4, 1631.7, 1065.9, 2466.4, 1324.5,
#' 3790.1, NA, 1735, 872.3, 2314, NA, 2001.4, 1700.4, 2521.4, NA))
#'
#' # run code
#' output= lrr(data= df, trt= N0, ctrl= N1, group = c("Cultivar"))
#'
#' print (output)
#'   Cultivar n_T n_C n_LRR          LRR     LRR_L     LRR_U    Cohen_d   Hedges_g
#'1       CV1   4   4     4 -0.330514832 -1.924442 1.2634125 -0.5269730 -0.4582374
#'2       CV2   4   4     4 -0.009187594 -1.209322 1.1909472  0.3279905  0.2852091
#'3       CV3   4   4     4 -0.428739063 -1.243705 0.3862267 -1.0188530 -0.8859592
#'4       CV4   4   4     4 -0.363033297 -2.487070 1.7610031 -0.6920945 -0.6018213
#'5       CV5   4   4     4 -0.295524032 -1.154888 0.5638399 -0.9486484 -0.8249117
#'6       CV6   3   3     3 -0.876921152 -2.680809 0.9269668 -1.7118456 -1.3694765
#'7       CV7   3   3     3 -0.894653273 -3.083024 1.2937177 -1.8273893 -1.4619115
#'8       CV8   3   3     3 -1.129853628 -2.575724 0.3160167 -4.0361639 -3.2289311
#'
#' ## Note: If the Block is included, the CI is not calculated because there are no replicates.
#'
#' □ Website: https://agronomy4future.com/archives/25018
#' □ Github: https://github.com/agronomy4future/lrr
#'
#' - All Rights Reserved © J.K Kim (kimjk@agronomy4future.com)
#'
lrr = function(data, trt, ctrl, group = NULL, conf.level = 0.95) {

  trt = deparse(substitute(trt))
  ctrl = deparse(substitute(ctrl))

  # validate confidence level
  if (!is.numeric(conf.level) || conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be a number between 0 and 1")
  }

  alpha = 1 - conf.level

  # --- remove rows with NA / NaN / Inf in trt or ctrl ---
  data = data[is.finite(data[[trt]]) & is.finite(data[[ctrl]]), ]

  # --- grouping ---
  if (is.null(group)) {
    grp = factor("All")
    group_vars = NULL
  } else {
    group_vars = group

    if (!all(group_vars %in% names(data))) {
      stop("group variables not found in data")
    }

    grp = interaction(data[, group_vars], drop = TRUE, sep = " | ")
  }

  results = lapply(split(data, grp), function(df) {

    # sample sizes
    nT = sum(!is.na(df[[trt]]))
    nC = sum(!is.na(df[[ctrl]]))

    # ---------- LRR ----------
    LRR = log(df[[trt]] / df[[ctrl]])
    nLRR = length(LRR)

    if (nLRR == 0) {
      mean_LRR = NA
      LRR_L = NA
      LRR_U = NA
    } else {
      mean_LRR = mean(LRR)

      if (nLRR < 2) {
        LRR_L = NA
        LRR_U = NA
      } else {
        se_LRR = sd(LRR) / sqrt(nLRR)
        tcrit = qt(1 - alpha / 2, df = nLRR - 1)

        LRR_L = mean_LRR - tcrit * se_LRR
        LRR_U = mean_LRR + tcrit * se_LRR
      }
    }

    # ---------- Cohen's d ----------
    mT = mean(df[[trt]], na.rm = TRUE)
    mC = mean(df[[ctrl]], na.rm = TRUE)
    sdT = sd(df[[trt]], na.rm = TRUE)
    sdC = sd(df[[ctrl]], na.rm = TRUE)

    if (nT + nC < 3) {
      d = NA
      g = NA
    } else {
      sp = sqrt(((nT - 1) * sdT^2 + (nC - 1) * sdC^2) / (nT + nC - 2))
      d = (mT - mC) / sp

      # ---------- Hedges' g ----------
      J = 1 - 3 / (4 * (nT + nC) - 9)
      g = J * d
    }

    data.frame(
      n_T = nT,
      n_C = nC,
      n_LRR = nLRR,
      LRR = mean_LRR,
      LRR_L = LRR_L,
      LRR_U = LRR_U,
      Cohen_d = d,
      Hedges_g = g
    )
  })

  out = do.call(rbind, results)

  # --- recover grouping variables ---
  if (!is.null(group_vars)) {
    group_df = do.call(rbind, strsplit(rownames(out), " \\| "))
    colnames(group_df) = group_vars
    out = cbind(as.data.frame(group_df), out)
  } else {
    out = cbind(Group = "All", out)
  }

  rownames(out) = NULL
  out
}

# All Rights Reserved © J.K Kim (kimjk@agronomy4future.com). Last updated on 12/15/2025
