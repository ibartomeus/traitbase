#add real data

#Oliveira et al. Dutch bees----
d <- read.table("real_data/Dutch_bees/dat.txt", h = TRUE)
head(d)
re_d <- melt(data = d, measure.vars = "Size_ITD", id.vars = c("Sample", "Genus", "species", "Sex"))
head(re_d)
colnames(re_d) <- c("link_id","genus","species","sex","trait","value")
re_d$category <- "morphological"
re_d$reference <- "doi:10.1371/journal.pone.0148983" #DOI?
re_d$credit <- "Oliveira MO, Freitas BM, Scheper J, Kleijn D"

temp <- fill_columns(re_d)
head(temp)

#ojo check trait is called as schema!