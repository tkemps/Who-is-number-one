library(data.table)
library(ggplot2)

verein <- data.table(verein=c("FC Bayern Muenchen","Borussia Dortmund","Eintracht Frankfurt","Bayer Leverkusen","RB Leipzig","FC Schalke 04","Hannover 96","FC Augsburg","1899 Hoffenheim","Borussia Moenchengladbach","Hertha BSC","SC Freiburg","VfB Stuttgart","VfL Wolfsburg","SV Werder Bremen","1. FSV Mainz 05","Hamburger SV","1. FC Koeln"),
	vereinId=1:18)

# files from http://dbup2date.uni-bayreuth.de/downloads/bundesliga/2018-03-09_bundesliga_CSV.zip
# inp <- paste(readLines("stdin", n=-1), collapse="\n")
spiel <- fread("https://raw.githubusercontent.com/tkemps/Who-is-number-one/master/spiel.csv")
spiel$V9 <- NULL
spiel <- spiel[Heim<=18 & Gast<=18 & Datum < '2018-03-09']
str(spiel)

n <- max(pmax.int(spiel$Heim), pmax.int(spiel$Gast))
cat("n =",n,"\n")

emat <- function(n,m,i,j,x=1) {
	m <- matrix(0, nrow=m, ncol=n)
	m[i,j] <- x
	m
}

h <- spiel[, .N, by = Heim]
g <- spiel[, .N, by = Gast]

H <- Reduce('+', mapply(function(i,m) emat(n,n,i,i)*m, h$Heim, h$N, SIMPLIFY=F))
G <- Reduce('+', mapply(function(i,m) emat(n,n,i,i)*m, g$Gast, g$N, SIMPLIFY=F))
T <- H+G
P <- Reduce('+', mapply(function(i,j) emat(n,n,i,j), spiel$Gast, spiel$Heim, SIMPLIFY=F))
M <- T-P
M1 <- M
M1[n,] <- 1

pH <- spiel[, .(D=sum(Tore_Heim-Tore_Gast)), by=Heim][order(Heim)][,D]
pG <- spiel[, .(D=-sum(Tore_Heim-Tore_Gast)), by=Gast][order(Gast)][,D]
p <- pH+pG
p1 <- p
p1[n] <- 0

r <- solve(M1,p1)

f <- spiel[, .(f=sum(Tore_Heim)), by=Heim][order(Heim), .(f)] + spiel[, .(f=sum(Tore_Gast)), by=Gast][order(Gast), .(f)]
a <- spiel[, .(a=sum(Tore_Gast)), by=Heim][order(Heim), .(a)] + spiel[, .(a=sum(Tore_Heim)), by=Gast][order(Gast), .(a)]

d <- solve(T+P, as.vector(T %*% r - f$f))
o <- r-d

massey <- data.table(Verein=verein$verein, Ranking=r, Off=o, Def=d, F=f$f, A=a$a, Heim=pH, Gast=pG, `Heim+Gast`=pH+pG)[order(Ranking, decreasing=TRUE)]
massey

ggplot(massey, aes(Off, Def)) +
	geom_point() +
    geom_text(aes(Off, Def+0.015, label=Verein))
