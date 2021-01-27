#SETUP
rm(list = ls(all = T))
library(xlsx)
library(plyr) # rbind.fill
library(dplyr)
library(expss)
library(reshape)
library(data.table)
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
data <- data[, c("cg_lsg_4", "cg_lsg_1", "cg_lsg_3")]
fit <-
  euler(c(
    A = as.numeric(data$cg_lsg_4),
    B = as.numeric(data$cg_lsg_1),
    `A&B` = as.numeric(data$cg_lsg_3)
  ))
pdf("output/graphs/venn.pdf")
p <- plot(fit, fills = list(fill = c("#E89A94", "#D1D3D4",
                                     "#EE5859")), labels = F)
dev.off()
print(p)

##########################################################################
#Prison Graph
#graph outputs
p <- msni19::index_intersections(
  response_with_composites,
  lsg =  c(
    "education_score",
    "livelihoods_score",
    "food_security_score",
    "protection_score",
    "health_score",
    "snfi_score",
    "wash_score"
  ),
  lsg_labels = c(
    "Education",
    "Livelihoods",
    "Food",
    "Protection",
    "Health",
    "Shelter",
    "WASH"
  ),
  weighting_function = weight_fun,
  y_label = "",
  exclude_unique = F,
  mutually_exclusive_sets = T,
  round_to_1_percent = T,
  print_plot = T,
  path = "output/graphs",
  index_filter = c(3, 4)
)
dev.off()
print(p)

#My Method
df <-
  df %>% select(
    lsg_education:lsg_wash,
    lsg_education_livelihoods:lsg_education_livelihoods_food_protection_health_snfi_wash
  )
df <- df[5,]
cnames <- names(df)
cnames <- gsub("lsg_", "", cnames)
cnames <- gsub("education", "Education", cnames)
cnames <- gsub("livelihoods", "Livelihoods", cnames)
cnames <- gsub("food", "Food Security", cnames)
cnames <- gsub("protection", "Protection", cnames)
cnames <- gsub("health", "Health", cnames)
cnames <- gsub("snfi", "SNFI", cnames)
cnames <- gsub("wash", "WASH", cnames)
cnames <- gsub("_", "&", cnames)
names(df) <- cnames
df <-
  apply(df, 2,          
        function(x)
          as.numeric(as.character(x)))
df <- df * 100

df[df < 1 & df !=
                  0] <- 1

df <- df %>% round

pdf("output/graphs/prison_graph.pdf")
prison <- upset(fromExpression(df),
      nintersects = 15,
      nsets = 7,
      order.by = "freq")
dev.off()
print(prison)
#########################################################################
#Radar Graph
df_in_camp <-
  read.csv(
    "output/summary_sorted/summary_sorted_msni_all_population_group_idp_in_camp.csv",
    stringsAsFactors = F
  )
df_in_camp$population_group <- "In-camp IDPs"
df_in_camp <-
  df_in_camp[5, c(
    "population_group",
    "lsg_education",
    "lsg_livelihoods",
    "lsg_food",
    "lsg_protection",
    "lsg_health",
    "lsg_snfi",
    "lsg_wash"
  )]
# df_in_camp$lsg_wash <- 0

df_out_camp <-
  read.csv(
    "output/summary_sorted/summary_sorted_msni_all_population_group_idp_out_camp.csv",
    stringsAsFactors = F
  )
df_out_camp$population_group <- "Out of camp IDPs"
df_out_camp <-
  df_out_camp[5, c(
    "population_group",
    "lsg_education",
    "lsg_livelihoods",
    "lsg_food",
    "lsg_protection",
    "lsg_health",
    "lsg_snfi",
    "lsg_wash"
  )]
df_returnee <-
  read.csv(
    "output/summary_sorted/summary_sorted_msni_all_population_group_returnee.csv",
    stringsAsFactors = F
  )
df_returnee$population_group <- "Returnees"
df_returnee <-
  df_returnee[5, c(
    "population_group",
    "lsg_education",
    "lsg_livelihoods",
    "lsg_food",
    "lsg_protection",
    "lsg_health",
    "lsg_snfi",
    "lsg_wash"
  )]

data <- rbind(df_in_camp, df_out_camp, df_returnee)
data$lsg_education <- as.numeric(data$lsg_education)
data$lsg_livelihoods <- as.numeric(data$lsg_livelihoods)
data$lsg_food <- as.numeric(data$lsg_food)
data$lsg_protection <- as.numeric(data$lsg_protection)
data$lsg_health <- as.numeric(data$lsg_health)
data$lsg_snfi <- as.numeric(data$lsg_snfi)
data$lsg_wash <- as.numeric(data$lsg_wash)

pdf("output/graphs/radar.pdf")
p <-
  ggradar(
    data,
    axis.labels = c(
      "Education",
      "Livelihoods",
      "Food \nSecurity",
      "Protection",
      "Health",
      "Shelter",
      "WASH"
    ),
    axis.label.size = 4,
    axis.label.offset = 1.2,
    background.circle.transparency = 0.1,
    group.point.size = 3,
    legend.position = "right",
    legend.text.size = 10,
    group.colours = c(
      "#EE5859",
      "#58585A",
      "#D1D3D4",
      "#D2CBB8",
      "#A9C5A1",
      "#FFF67A",
      "#F69E61",
      "#95A0A9",
      "#56B3CD"
    ),
    font.radar = "Helvetica-Narrow"
  )
dev.off()
print(p)
