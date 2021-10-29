#########################################################################
#                      PRODUTIVIDADE ESTADUAL
#########################################################################

# here I will calculate descriptive statistics on the sanitation
# situation in the municipalities of São Paulo


setwd("D:/Git projects/CEPER/ceper_san3")


path_tab = "D:/Git projects/CEPER/ceper_san3/rel_tex/tables/"
path_fig = "D:/Git projects/CEPER/ceper_san3/rel_tex/figures/"



library(readxl)
library(tidyverse)
library(dplyr)
library(fGarch)
library(magrittr)
library(haven)
library(xlsx)
library(data.table)
library(ggpubr)
library(ggthemes)

options(scipen=999) # suppress scientific notation



myfunc <- function(v1) {
  deparse(substitute(v1))
}



# negate in ----

`%notin%` <- Negate(`%in%`)



# region function ----

region = function(uf){
  pos = match(uf, pib$cod)
  pib$regiao[pos]
}



# stats-----


stats = function (x, ci = 0.95)
{
  y = as.matrix(x)
  if (is.null(colnames(y))) {
    Dim = dim(y)[2]
    if (Dim == 1) {
      colnames(y) = paste(substitute(x), collapse = ".")
    }
    else if (Dim > 1) {
      colnames(y) = paste(paste(substitute(x), collapse = ""),
                          1:Dim, sep = "")
    }
  }
  cl.vals = function(x, ci) {
    x = x[!is.na(x)]
    n = length(x)
    if (n <= 1)
      return(c(NA, NA))
    se.mean = sqrt(var(x)/n)
    t.val = qt((1 - ci)/2, n - 1)
    mn = mean(x)
    lcl = mn + se.mean * t.val
    ucl = mn - se.mean * t.val
    c(lcl, ucl)
  }
  nColumns = dim(y)[2]
  ans = NULL
  for (i in 1:nColumns) {
    X = y[, i]
    X.length = length(X)
    X = X[!is.na(X)]
    X.na = X.length - length(X)
    z = c(X.length, sum(is.na(x)),  min(X), max(X), diff(range(X)),
          as.numeric(quantile(X, prob = 0.25, na.rm = TRUE)),
          as.numeric(quantile(X, prob = 0.75, na.rm = TRUE)), mean(X),
          median(X), var(X), sqrt(var(X)),(sqrt(var(X))/mean(X)) , skewness(X),
          kurtosis(X))
    znames = c("Observations", "NAs", "Minimum","Maximum", "Amplitude",  "1 Quartile",
               "3 Quartile", "Mean", "Median", "Variance",
               "Standard Deviation", 'Coefficient of Variation', "Skewness", "Kurtosis")
    result = matrix(z, nrow  = length(znames))
    rownames(result) = znames
    ans = rbind(ans, result)
  }
  colnames(result) = colnames(y)
  as_tibble(round(result, digits = 2))

}








