<!-- README.md is generated from README.Rmd. Please edit that file -->
# datacume
<!-- badges: start -->
<!-- badges: end -->

The goal of the lrr package is to compute the log response ratio (LRR) between treatment and control values, along with confidence intervals. 

□ Code explained: https://agronomy4future.com/archives/25018

## Installation

You can install lrr() like so:

Before installing, please download Rtools (https://cran.r-project.org/bin/windows/Rtools)

``` r
if(!require(remotes)) install.packages("remotes")
if (!requireNamespace("lrr", quietly= TRUE)) {
 remotes::install_github("agronomy4future/lrr", force= TRUE)
}
library(remotes)
library(lrr)
```

## Example

This is a basic code for lrr()

``` r
lrr(data, trt, ctrl, group = NULL, conf.level = 0.95)
```

## Let’s practice with actual dataset

``` r
# data example 
df= data.frame(Cultivar= rep(c("CV1", "CV2", "CV3", "CV4", "CV5",
"CV6", "CV7", "CV8"), each = 4L), Block= rep(c("I", "II", "III", "IV"), 8),
N0= c(608.9, 464.7, 199.3, 930, 623.7, 43.1, 200.3, 207.7, 1243.7, 776.7,
1424.5, 1556.4, 352.2, 1316.7, 377.7, 980.8, 660.9, 1535.5, 1009.2, 1490.8,
692.6, 1273.9, 1010.7, 1105.8, 695, 869, 396, 1147, 883.9, 786.6, 416.2, 1512.6),
N1= c(697.6, 601.9, 1063.4, 440.6, 313, 109.9, 119.9, 281.3, 3463.1, 1364.8, 1991.9,
1264, 1837.9, 1382.5, 1160.5, 248.9, 1679.7, 1704.4, 1631.7, 1065.9, 2466.4, 1324.5,
3790.1, NA, 1735, 872.3, 2314, NA, 2001.4, 1700.4, 2521.4, NA)))

# lrr() 
output= lrr(data= df, trt= N0, ctrl= N1, group = c("Cultivar"))

print (output)
  Cultivar n_T n_C n_LRR          LRR     LRR_L     LRR_U    Cohen_d   Hedges_g
1       CV1   4   4     4 -0.330514832 -1.924442 1.2634125 -0.5269730 -0.4582374
2       CV2   4   4     4 -0.009187594 -1.209322 1.1909472  0.3279905  0.2852091
3       CV3   4   4     4 -0.428739063 -1.243705 0.3862267 -1.0188530 -0.8859592
4       CV4   4   4     4 -0.363033297 -2.487070 1.7610031 -0.6920945 -0.6018213
5       CV5   4   4     4 -0.295524032 -1.154888 0.5638399 -0.9486484 -0.8249117
6       CV6   3   3     3 -0.876921152 -2.680809 0.9269668 -1.7118456 -1.3694765
7       CV7   3   3     3 -0.894653273 -3.083024 1.2937177 -1.8273893 -1.4619115
8       CV8   3   3     3 -1.129853628 -2.575724 0.3160167 -4.0361639 -3.2289311

## Note: If the Block is included, the CI is not calculated because there are no replicates.

- All Rights Reserved © J.K Kim (kimjk@agronomy4future.com)
