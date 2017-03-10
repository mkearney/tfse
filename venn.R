

## draw plot (create tikz file to be included)
tikzDevice::tikz("../tex/figures/venn.tex",
                 standAlone = FALSE,
                 width = 6,
                 height = 5)
draw.triple.venn(
  area1 = 10, area2 = 10, area3 = 10,
  n12 = 1, n23 = 1, n13 = 1, n123 = 1,
  category = c("@maddow\n@salon\n@HuffPostPol\n@paulkrugman",
    "@seanhannity\n@SarahPalinUSA\n@DRUDGE\\_REPORT\n@FoxNews",
    "@AMC\\_TV\n@SInow\n@survivorcbs\n"),
  lty = 1,
  label.col = rep("transparent", 7),
  cat.dist = c(-.05, -.05, -.03),
  cat.cex = rep(1.3, 3),
  cat.fontface = rep(1, 3),
  col = c("#002244", "#330000", "#330033"),
  lwd = rep(1.5, 3),
  fill = c("#3366ffcc", "#dd3333cc", "#dd33dd88"))
dev.off()


usr <- rtweet::lookup_users(
  c("maddow", "salon", "huffpostpol", "paulkrugman",
    "seanhannity", "sarahpalinUSA", "DRUDGE_REPORT", "foxnewspolitics",
    "AMC_TV", "AmericanIdol", "SInow", "survivorcbs")
)
usr$created_at <- as.Date(usr$created_at)

sns <- usr[, c(3)]
sns[5] <- paste(sns[5], "\n", " another", collapse = "")
stargazer::stargazer(data.frame(Group = c(rep("Dem", 4), rep("GOP", 4), rep("Ent", 4)),
  Screen_Name = sns,
  N = rep(250, 12)), rownames = FALSE, summary = FALSE)

stargazer::stargazer(usr[, c(3, 8, 9, 11, 17)], summary = FALSE)
