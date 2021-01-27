#SETUP

library(dplyr)
library(msni19)
library(rlang)
library(plotly)
library(ggradar)
library(UpSetR)
library(eulerr)

df <-
  read.csv("output/summary_sorted/summary_sorted_msni_all_all_all.csv")

##########################################################################
#Venn Diagram
data <- df[5, ]
data <- data[, c("lsg_but_no_cg", "cg_no_lsg", "cg_and_one_lsg")]
fit <-
  euler(c(
    A = as.numeric(data$lsg_but_no_cg),
    B = as.numeric(data$cg_no_lsg),
    `A&B` = as.numeric(data$cg_and_one_lsg)))
pdf("output/graphs/venn.pdf")
p <- plot(fit, fills = list(fill = c("#E89A94", "#D1D3D4",
                                     "#EE5859")), labels = F)
dev.off()
print(p)
