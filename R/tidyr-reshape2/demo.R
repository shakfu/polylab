# note: sourcing MUST BE absolute paths
suppressMessages(library('dplyr'))
suppressMessages(library('reshape2'))
suppressMessages(library('tidyr'))

# -------------------------------------------------------------
# GLOBAL OPTIONS
# -------------------------------------------------------------
#options(digits=2)

data <- read.csv("data.csv", header=FALSE)
names(data) <- c(paste0("day_", 1:40))
patientID <- c(1:60)
data <- cbind(patientID, data)
drugs <- rep(c("A", "B", "C"), each = 20)
data <- as.data.frame(append(data, list(drug = drugs), after = 1))

reshaped_data <- melt(data, id.vars = c("patientID", "drug"),
                            variable.name = "day",
                            value.name = "inflammation")

tidied_data <- gather(data, day, inflammation, day_1:day_40)

identical(reshaped_data, tidied_data)
