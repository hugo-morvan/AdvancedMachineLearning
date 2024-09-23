library(bnlearn)

data("asia")

head(asia)

dag <- hc(asia)

plot(dag)

# Specify some initial structure
forced_arcs <- data.frame(from=c("A", "S"),
                          to=c("B", "B"))

dag_forced <- hc(asia, whitelist = forced_arcs)

plot(dag_forced)