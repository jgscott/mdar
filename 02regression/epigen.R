# epigen = read.table("epigen.txt", sep="\t", header=TRUE)

# How well do the repeated measurements correspond?
plot(methyl2 ~ methyl1, data=epigen)

# Could define the average measurement, accounting for NA's
epigen$methylavg = apply( cbind(epigen$methyl1, epigen$methyl2), 1, mean, na.rm=TRUE)

boxplot(methylavg ~ smoke + sex, data=epigen)
