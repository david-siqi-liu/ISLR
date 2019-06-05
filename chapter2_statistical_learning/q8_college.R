# (a)
college = read.csv("data/College.csv") # Read in College.csv
View(college)

# (b)
rownames(college) = college[,1] # Gives each row a name corresponding to the 1st column (i.e. university)
fix(college)
college = college[,-1] # Removes 1st column (duplicate university)
fix(college)

# (c)
summary(college)
pairs(college[,1:4])
names(college)
attach(college)
plot(Outstate ~ Private, data = college) # Side-by-side box-plots

Elite = rep("No", nrow(college)) # Initialize new column - Elite
Elite[Top10perc > 50] = "Yes" # If more than 50% of students come from top 10% of their high school
Elite = as.factor(Elite) # Encode
college = data.frame(college, Elite) # Add Elite to college
summary(Elite)
boxplot(Outstate ~ Elite, data = college, xlab = "Elite", ylab = "Outstate")

pdf("q8_figure.pdf")
par(mfrow=c(2,2)) # Divides the print window into four regions
par(mar=c(1,1,1,1)) # Fix margin
hist(Apps, breaks = 150, xlim = c(0, 5000), main = "# of Received Apps")
hist(Outstate, breaks = 25, main = "Out-of-State Tuition")
hist(Grad.Rate, breaks = 25, main = "Graduation Rate")
dev.off()
