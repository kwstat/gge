---
title: "SVP Coordinate calculations"
author: "Kevin Wright"
date: "November 20, 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
B <- data.frame(E1= c(50, 55, 65, 50, 60, 65, 75.),
E2= c(67, 71, 76, 80, 82, 89, 95),
E3= c(90, 93, 95, 102, 97, 106, 117),
E4= c(98, 102, 105, 130, 135, 137, 133),
E5= c(120, 129, 134, 138, 151, 153, 155))
rownames(B) <- c("G1","G2","G3","G4","G5","G6","G7")
B=as.matrix(B)

library(nipals)
    # g.scal  <- sqrt(nrow(x)-1) * u
    # hl.scal  <- 1/sqrt(nrow(x)-1) * v %*% diag(d)
    genCoord <- U * sqrt(n.gen-1)
    blockCoord <- t(x.g + x.gb) %*% U / sqrt(n.gen - 1)
    resCoord <- t(x.r) %*% U * (1/sqrt(n.gen - 1))
    
  } else if (focus=="gen") { # jk
    # g.scal  <- u %*% diag(d)
    # hl.scal <- v
    genCoord <- U %*% diag(D)
    blockCoord <- t(x.g + x.gb) %*% U %*% diag(1/D)
    resCoord <- t(x.r) %*% U %*% diag(1/D)
    
  } else if (focus=="dual") { # hj
    # g.scal  <- u %*% diag(d)
    # hl.scal  <- v %*% diag(d)
    genCoord <- U %*% diag(D)
    blockCoord <- t(x.g + x.gb) %*% U
    resCoord <- t(x.r) %*% U
    
  } else if (focus=="symm") { # sqrt
    # g.scal  <- u %*% sqrt(diag(d))
    # hl.scal  <- v %*% sqrt(diag(d))
    genCoord <- U %*% sqrt(diag(D))
    blockCoord <- t(x.g + x.gb) %*% U %*% diag(1/sqrt(D))
    resCoord <- t(x.r) %*% U %*% diag(1/sqrt(D))
  }
```

