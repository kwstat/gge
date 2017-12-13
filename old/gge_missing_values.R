# gge_missing_values.R

# A short script to see what is the impact of deleting many locations
# for a single entry.

lib(agridat)
# load gge code in RStudio with C-S L
# lib(gge)

# complete data
data(crossa.wheat)
dat <- crossa.wheat
m1 <- gge(yield ~ gen*loc, dat)
plot(m1)
biplot(m1)

# get locs names in a certain order for sequential drops
locs <- c("CA", "SC", "SR", "AK", "SE", "PI", "EB", "KN", "SJ", "SG", 
"ES", "MM", "EG", "TC", "SS", "PA", "NB", "MG", "JM", "TB", "MS", 
"AS", "BJ", "IL", "ID")

dat <- crossa.wheat
dat$eg <- ifelse(is.element(
  dat$loc,
  c("KN","NB","PA","BJ","IL","TC",
    "JM","PI","AS","ID","SC","SS",
    "SJ","MS","MG","MM")), "Grp1", "Grp2")

# delete genotype 9 in an additional loc every time through the loop
for(ll in locs[c(1:18,23:25)]){
  cat("loc ", ll, " \n")
  dat[dat$gen=="G09" & dat$loc==ll,'yield'] <- NA
  # Specify env.group as column in data frame
  m2 <- gge(yield~gen*loc, dat, env.group=eg)
  #plot(m2)
  biplot(m2, main=paste("crossa.wheat dropped", ll))
}
