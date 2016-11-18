
library(ggplot2)
library(hash)


setwd("~/projects_code/nlrb_brown_ta_data/")

# read in data
gra <- read.csv("gra.csv", stringsAsFactors = FALSE)
gta <- read.csv("gta.csv", stringsAsFactors = FALSE)
uta_wd <- read.csv("uta-department.csv", stringsAsFactors = FALSE)
uta_stdemp <- read.csv("uta-studentemployment.csv", stringsAsFactors = FALSE)
utra <- read.csv("utra.csv", stringsAsFactors = FALSE)


summary(gra)
summary(gta)
summary(uta_wd)
summary(uta_stdemp)
summary(utra)

col_names <- c("id", "name", "program", "degree", "primary_appointment", "admit_term", "subject", "num_times_ta")


# This is a quick function that returns a hash 
# table with dept code and the dept name. Note that 
# dat is a 2-col matrix where col 1 is abreviation of 
# department and col 2 is the full-name 

get_department_hash <- function(dat) {
    dept_hash <- hash()
    n <- nrow(dat)
    
    for (i in 1:n) {
        if (dat[i, 1] != "") {
            dept_hash[dat[i, 1]] <- as.character(dat[i, 2])    
        }
    } 
    return(dept_hash)
}

dept_hash <- get_department_hash(gta[, c("SUBJECT", "SUBJECT_DESC")])



# Working with undergraduate TA data

employing_dept <- function(id, dat) {
    match_idx <- which(dat[, "ID"] == id)
    
    if (length(match_idx) == 1) {
        res <- dat[match_idx, "Department"]
    } else if (length(match_idx) > 1) {
        depts <- dat[match_idx, "Department"]
        
        if (length(unique(depts)) == 1) {
            res <- depts[1]
        } else if (length(unique(depts)) > 1) {
            res <- "Multiple Depts."
        }
    }
    return(res)
}

# testing the above function
employing_dept("B01090789", uta_stdemp) == "Applied Math"
employing_dept("B00644792", uta_stdemp) == "Engineering"


home_dept <- function(id, dat, dept_hash) {
    match_idx <- which(dat[, "ID"] == id)
    
    if (length(match_idx) == 1) {
        dept_code <- dat[match_idx, "Dept"]
        res <- dept_hash[[dept_code]]
       
        if (is.null(res)) {
            res <- dept_code                # use dept code if fullname not in hash
        }
    } else if (length(match_idx) > 1) {
        depts <- dat[match_idx, "Dept"]
        
        if (length(unique(depts)) == 1) {
            res <- dept_hash[[depts[1]]]
            if (is.null(res)) {
                res <- depts[1]             # dept code if fullname not in hash
            }
        } else if (length(unique(depts)) > 1) {
            res <- "Multiple Depts."        # probably very rare (sanity check this)
        }
    }
    return(res)
}

# testing
home_dept("B01090789", uta_stdemp, dept_hash) == "Engineering"
home_dept("B00644792", uta_stdemp, dept_hash) == "Engineering"
home_dept("B00965158", uta_stdemp, dept_hash) == "Biology"
home_dept("B00898274", uta_stdemp, dept_hash) == "COMP"

count_times_ta <- function(id, dat) {
    matches <- which(dat[, "ID"] == id)
    cnt <- length(matches)
    return(cnt)
}


is_grader <- function(title) {
    pos <- grep("grader", tolower(title))
    if (length(pos) > 0) {
        res <- TRUE 
    } else if (length(pos) == 0) {
        res <- FALSE 
    }
    return(res)
}

# testing
count_times_ta("B00644792", uta_stdemp) == 2
count_times_ta("B00370646", uta_stdemp) == 4
count_times_ta("B00633190", uta_stdemp) == 6



position_type <- function(id, dat) {
    row_indcs <- which(dat[, "ID"] == id) 
    titles_ugly <- unique(dat[row_indcs, "Title"])
    titles <- titles_ugly[titles_ugly != ""]
    n <- length(titles)
    was_grader <- sapply(titles, is_grader)
    
    if (length(was_grader) == 0) {
        return(NA)
    }
    
    if (sum(was_grader) == 0) {
        res <- "TA"
    } else if (sum(was_grader) == n) {
        res <- "Grader"
    } else if (sum(was_grader) > 0) {
        res <- "Both TA and Grader"
    }
    return(res) 
}

# testing
position_type("B00644792", uta_stdemp) == "Grader"
position_type("B00900429", uta_wd) == "TA"
is.na(position_type("B00713260", uta_wd))



clean_undergrads_data <- function(dat, dept_hash) {
    res <- data.frame(id = unique(dat[, "ID"]), stringsAsFactors = FALSE)
    n <- nrow(res)
    
    # initialize empty columns in result dataframe
    res[, "name"] <- rep(NA, n)
    res[, "employing_dept"] <- rep(NA, n)             # could be multiple 
    res[, "home_dept"] <- rep(NA, n)
    res[, "ta_n_times"] <- rep(NA, n)
    res[, "position_type"] <- rep(NA, n)
    
    for (i in 1:n) {
        idx <- which(dat[, "ID"] == res[i, "id"])[1]  # assume ID-name matches perfect
        
        res[i, "name"] <- dat[idx, "Name"] 
        res[i, "employing_dept"] <- employing_dept(res[i, "id"], dat)
        res[i, "home_dept"] <- home_dept(res[i, "id"], dat, dept_hash)
        res[i, "ta_n_times"] <- count_times_ta(res[i, "id"], dat)
        res[i, "position_type"] <- position_type(res[i, "id"], dat)
    }
    return(res)
}


uta_clean <- clean_undergrads_data(uta_stdemp, dept_hash)

uta_clean2 <- clean_undergrads_data(uta_wd, dept_hash)


# combine undergrad TA data
names(uta_stdemp)[14] <- "Term"
common_cols <- c("ID", "Department", "Dept", "Term", "Title")

uta <- as.data.frame(rbind(uta_wd[, common_cols], 
                           uta_stdemp[, common_cols]), stringsAsFactors = FALSE)


uta_all_clean <- clean_undergrads_data(uta)


# plotting

p1 <- ggplot(uta_clean, aes(employing_dept)) + 
    geom_bar(colour = "lightblue", fill = "skyblue") +
    theme(text = element_text(size = 12), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    xlab(NULL) +
    ylab("Students") +
    ggtitle("Undergraduate Students TA-ing")

p1


p2 <- ggplot(uta_clean, aes(factor(ta_n_times))) + 
    geom_bar(colour = "lightblue", fill = "skyblue", width = 0.95) +
    theme(text = element_text(size = 12), 
          plot.title = element_text(size = 20, hjust = 0.5)) +
    xlab("Courses TA'd") +
    ylab("Students") +
    ggtitle("Undergraduate TAs")

p2



# counts
length(unique(c(uta_wd$ID, uta_stdemp$ID)))   # 999 total undergrad TAs and RAs 
length(unique(c(gta$ID, gra$ID)))             # 916 total grad TAs and RAs 

