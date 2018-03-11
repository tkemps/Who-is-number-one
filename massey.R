library(data.table)
library(ggplot2)


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


verein <- data.table(verein=c("FC Bayern Muenchen","Borussia Dortmund","Eintracht Frankfurt","Bayer Leverkusen","RB Leipzig","FC Schalke 04","Hannover 96","FC Augsburg","1899 Hoffenheim","Borussia Moenchengladbach","Hertha BSC","SC Freiburg","VfB Stuttgart","VfL Wolfsburg","SV Werder Bremen","1. FSV Mainz 05","Hamburger SV","1. FC Koeln"),
	vereinId=1:18)

# files from http://dbup2date.uni-bayreuth.de/downloads/bundesliga/2018-03-09_bundesliga_CSV.zip
# inp <- paste(readLines("stdin", n=-1), collapse="\n")
spiel <- fread("https://raw.githubusercontent.com/tkemps/Who-is-number-one/master/spiel.csv")
spiel$V9 <- NULL
spiel <- spiel[Heim<=18 & Gast<=18 & Datum < '2018-03-09']
spiel[, Punkte_Heim := ifelse(Tore_Heim>Tore_Gast,3,ifelse(Tore_Heim==Tore_Gast,1,0))]
spiel[, Punkte_Gast := ifelse(Tore_Heim>Tore_Gast,0,ifelse(Tore_Heim==Tore_Gast,1,3))]
str(spiel)

n <- max(pmax.int(spiel$Heim), pmax.int(spiel$Gast))
cat("n =",n,"\n")

emat <- function(n,m,i,j,x=1) {
	m <- matrix(0, nrow=m, ncol=n)
	m[i,j] <- x
	m
}
spiel[, .(points=sum(Punkte_Heim)), by=Heim][order(Heim)]$points
massey <- function(spiel, measure=c("g","p")) {
	if (measure=="g") {
		spiel[, `:=`(mH = Tore_Heim, mG = Tore_Gast)]
	} else {
		spiel[, `:=`(mH = Punkte_Heim, mG = Punkte_Gast)]	}

	h <- spiel[, .N, by = Heim]
	g <- spiel[, .N, by = Gast]

	H <- Reduce('+', mapply(function(i,m) emat(n,n,i,i)*m, h$Heim, h$N, SIMPLIFY=F))
	G <- Reduce('+', mapply(function(i,m) emat(n,n,i,i)*m, g$Gast, g$N, SIMPLIFY=F))
	T <- H+G
	P <- Reduce('+', mapply(function(i,j) emat(n,n,i,j), spiel$Gast, spiel$Heim, SIMPLIFY=F))
	M <- T-P
	M1 <- M
	M1[n,] <- 1

	pH <- spiel[, .(D=sum(mH-mG)), by=Heim][order(Heim)][,D]
	pG <- spiel[, .(D=-sum(mH-mG)), by=Gast][order(Gast)][,D]
	p <- pH+pG
	p1 <- p
	p1[n] <- 0

	r <- solve(M1,p1)

	f <- spiel[, .(f=sum(mH)), by=Heim][order(Heim), .(f)] + spiel[, .(f=sum(mG)), by=Gast][order(Gast), .(f)]
	a <- spiel[, .(a=sum(mG)), by=Heim][order(Heim), .(a)] + spiel[, .(a=sum(mH)), by=Gast][order(Gast), .(a)]

	d <- solve(T+P, as.vector(T %*% r - f$f))
	o <- r-d

	w <- spiel[Punkte_Heim==3, .N, by=Heim][order(Heim)]$N + spiel[Punkte_Gast==3, .N, by=Gast][order(Gast)]$N
	l <- spiel[, .(N = sum(Punkte_Heim==0)), by=Heim][order(Heim)]$N + spiel[Punkte_Gast==0, .N, by=Gast][order(Gast)]$N
	b <- (w-l)*0.5+1
	C <- 2*diag(n)+M
	colleyRating <- solve(C,b)

	points <- spiel[, .(points=sum(Punkte_Heim)), by=Heim][order(Heim)]$points +spiel[, .(points=sum(Punkte_Gast)), by=Gast][order(Gast)]$points

	massey <- data.table(Verein=verein$verein, masseyRating=r, colleyRating=colleyRating, Off=o, Def=d, 
						  F=f$f, A=a$a, Heim=pH, Gast=pG, 
						  `Heim+Gast`=pH+pG, wins=w, losses=l, points=points)[order(masseyRating, decreasing=TRUE)]
	massey
}


print(mr <- massey(spiel, measure="g"))
summary(lm_mr <- lm(masseyRating~points, data=mr))
summary(lm_cr <- lm(colleyRating~points, data=mr))

g1 <- ggplot(mr, aes(Off, Def)) +
	geom_point() +
    geom_text(aes(Off, Def+0.015, label=Verein), size=2.5)

g2 <- ggplot(mr, aes(y=masseyRating, x=points)) +
	geom_abline(aes(slope=lm_mr$coefficients[2], intercept= lm_mr$coefficients[1]), color="magenta") +
	geom_point(color="blue") +
	geom_text(aes(y=masseyRating+0.03, x=points, label=Verein), color="darkblue", size=2.5)

g3 <- ggplot(mr, aes(y=colleyRating, x=points)) +
	geom_abline(aes(slope=lm_cr$coefficients[2], intercept= lm_cr$coefficients[1]), color="magenta") +
	geom_point(color="blue") +
	geom_text(aes(y=colleyRating+0.0075, x=points, label=Verein), color="darkblue", size=2.5)

g4 <- ggplot(mr, aes(y=colleyRating, x=masseyRating)) +
	geom_smooth() +
	geom_point(color="blue") +
	geom_text(aes(y=colleyRating+0.0075, x=masseyRating, label=Verein), color="darkblue", size=2.5)

multiplot(g1,g2,g3,g4, cols=2)

#warnings()

