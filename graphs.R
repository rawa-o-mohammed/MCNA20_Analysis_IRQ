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
data <- df[5,]
data <- data[, c("lsg_but_no_cg", "cg_no_lsg", "cg_and_one_lsg")]
fit <-
  euler(c(
    A = as.numeric(data$lsg_but_no_cg),
    B = as.numeric(data$cg_no_lsg),
    `A&B` = as.numeric(data$cg_and_one_lsg)
  ))
pdf("output/graphs/venn.pdf")
p <- plot(fit, fills = list(fill = c("#E89A94", "#D1D3D4",
                                     "#EE5859")), labels = F)
dev.off()
print(p)




lsg = c(
  "education_score",
  "livelihoods_score",
  "food_security_score",
  "protection_score",
  "health_score",
  "snfi_score",
  "wash_score"
)
capacity_gaps = "coping_mechanism2"
weighting_function = weight_fun



df <- response_with_composites

data <- df %>% transmute(
  lsg_over_3 = pmin(1, rowSums(select(.,
                                      lsg) >= 3, na.rm = T)),
  cg_over_3 = replace_na(as.numeric(!!sym(capacity_gaps) >=
                                      3), 0),
  lsg_cg_over_3 = as.numeric(lsg_over_3 + cg_over_3 >
                               1),
  lsg_over_3 = lsg_over_3 - lsg_cg_over_3,
  cg_over_3 = cg_over_3 -
    lsg_cg_over_3,
  no_lsg_no_cg = ifelse(lsg_over_3 == 0 & lsg_cg_over_3 == 0 & cg_over_3 == 0, 1, 0)
) %>% select(lsg_over_3, cg_over_3, lsg_cg_over_3, no_lsg_no_cg)






if (is.null(weighting_function)) {
  data$weights <- rep(1, nrow(df))
} else {
  data$weights <- weighting_function(df)
}

data <- data %>% mutate_at(c("lsg_over_3", "cg_over_3",
                             "lsg_cg_over_3",
                             "no_lsg_no_cg"), ~ .x * weights) %>%
  summarize_all(sum)
data <- as.numeric(unlist(data))
cat(
  paste0(
    "LSG over 3 is ",
    round(100 * data[1] / 9634),
    "%\n",
    "CG over 3 is ",
    round(100 * data[2] / 9634),
    "%\n",
    "Overlap is ",
    round(100 * data[3] / 9634),
    "%\n",
    "No LSG and no CG is ",
    round(100 * data[4] / 9634),
    "%\n"
  )
)
fit <- euler(c(A = data[1], B = data[2], `A&B` = data[3]))
if (print_plot) {
  if (!is.null(path)) {
    plot_name <- paste(path, plot_name, sep = "/")
  }
  pdf(paste0(plot_name, ".pdf"))
  p <- plot(fit, fills = list(fill = c("#E89A94", "#D1D3D4",
                                       "#EE5859")), labels = F)
  print(p)
  dev.off()
  p
}else {
  plot(fit, fills = list(fill = c("#E89A94", "#D1D3D4",
                                  "#EE5859")), labels = F)
}
