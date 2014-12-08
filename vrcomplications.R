### The VRsurgery.txt file is made by copy-paste of the following table
### http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3650265/table/tbl3/

library(dplyr)
library(ggplot2)

df <- read.table("VRsurgery.txt", sep = "\t", fill = T, stringsAsFactors =F)
df[,1] <- gsub(substr(df[9,1],27,27), " - ", df[,1])

co <- df[6:30, 1:2]
colnames(co) <- c("Complication", "N")
index <- sapply(co[,2], function(x) regexpr("\\(", x)[1], USE.NAMES = F)
co[,2] <- substr(co[,2], 1, index-2)
co[,2] <- as.numeric(co[,2])
row.names(co) <- NULL
remB <- function(x) {
        if (substr(x, nchar(x), nchar(x)) == "b")
                substr(x, 1, nchar(x) - 1) else x
}
co[,1] <- sapply(co[,1], remB, USE.NAMES = F)
co[,1] <- as.factor(co[,1])

test <- lapply(co$N, function(x) prop.test(x, 8257)) ### 8257 are total operations with a PPV
pct <- numeric()
lower <- numeric()
upper <- numeric()
for (i in 1:nrow(co)) {
        pct <- c(pct,round(test[[i]]$estimate * 100, 3))
        lower <- c(lower, round(test[[i]]$conf.int[[1]] * 100, 3))
        upper <- c(upper, round(test[[i]]$conf.int[[2]] * 100, 3))
}
co$pct <- pct
co$lower <- lower
co$upper <- upper
co <- arrange(co, desc(pct))

ggplot(co, aes(x = pct, y = reorder(Complication, pct))) +
        geom_point(size = 3, color = "blue") +
        annotate("segment", x = lower,  xend = upper, y = c(25:1),
                 yend = c(25:1), color = "blue", size = 1) +
        xlab("Percentage") + ylab("") + 
        scale_x_continuous(limits = c(-0.2,3.8),
                           breaks = c(seq(0, 1.2, 0.2), seq(1.2, 3.6, 0.4))) +
        theme_bw() +
        theme(axis.text.x = element_text(color = "black"),
              axis.text.y = element_text(color = "black", size = rel(1.2)),
              panel.background = element_rect(fill = "snow")) +
        annotate("text", x = pct, y = 25:1 + 0.4, label = round(co[,3], 2),
                 cex = 4) +
        annotate("text", x = lower - 0.15, y = 25:1, label = round(co[,4], 2),
                 cex = 4, color = "darkgreen") +
        annotate("text", x = upper + 0.15, y = 25:1, label = round(co[,5], 2),
                 cex = 4, color = "darkred")
