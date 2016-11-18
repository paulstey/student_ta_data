library(ggplot2)

setwd("~/projects_code/nlrb_brown_ta_data/")
merged <- read.csv("merged.csv", stringsAsFactors = FALSE)

table(merged$Degree)





collapse <- function(dat) {
    ids <- unique(dat$ID)
    
    res <- data.frame(id = ids)
    n <- length(ids)
    res[, "Degree"] <- rep(NA, n)
    res[, "Standing"] <- rep(NA, n)
    res[, "Dept"] <- rep(NA, n)
    res[, "Appointment"] <- rep(NA, n)
    
    for (i in 1:n) {
        dat_tmp <- dat[which(dat[, "ID"] == ids[i]), ]
        res[i, "Degree"] <- max(dat_tmp[, "Degree"])
        res[i, "Standing"] <- max(dat_tmp[, "Standing"])
        res[i, "Appointment"] <- paste(unique(dat_tmp[, "Appointment"]), collapse = ",")
        res[i, "Dept"] <- paste(unique(dat_tmp[, "Dept"]), collapse = ",")
    }
    return(res)
}


dset <- collapse(merged)
dset[, "Standing2"] <- ifelse(dset[, "Degree"] != "B", dset[, "Standing"]*2, dset[, "Standing"])
#dset[, "Standing"] <- ceiling(dset[, "Standing"])

#dset <- dset[which(dset["Standing2"] > 0), ]
# plotting student TA- or RA-ing by department
p6 <- ggplot(dset, aes(factor(Standing2))) + 
    geom_bar(colour = "lightblue", fill = "skyblue") +
    theme(text = element_text(size = 12), 
          axis.text.x = element_text(hjust = 1),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    xlab("Standing") +
    ylab("Students") +
    ggtitle("TA's and RA's Standing by Degree") +
    facet_wrap(~Degree, ncol = 1)

p6
table(dset[, "Degree"], dset[, "Standing"])




top_depts <- names(sort(table(dset[, "Dept"]), decreasing = TRUE)[1:15])
dset$Dept2 <- ifelse(dset$Dept %in% top_depts, dset[, "Dept"], "Other")

# plotting student TA- or RA-ing by department
p7 <- ggplot(dset, aes(Dept2)) + 
    geom_bar(colour = "lightblue", fill = "skyblue") +
    theme(text = element_text(size = 12), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    xlab(NULL) +
    ylab("Students") +
    ggtitle("TA and RA Degree by Dept") +
    facet_wrap(~Degree, ncol = 1)

p7
table(dset[, "Degree"], dset[, "Standing"])



# degree by standing
# degree by dept