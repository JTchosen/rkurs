# Einlesen der Daten
m <- read.table("movies.tab", sep="\t", header=TRUE, quote="", comment="")
# Struktur der Daten ansehen
str(m)
genres <- c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance", "Short")

for(i in length(genres))
{
  m[[genres[i]]]<- as.factor(m[[genres[i]]])
}

str(m)

summary(m)

#Parameter für Query

queryfunc <- function(data, genre=NULL,anfangsjahr=1800, endjahr=2100) {
  tmp <- data
  if(is.character(genre)){
    tmp <- subset(tmp,tmp[[genre]]==1)
  }
  if(is.double(anfangsjahr)&is.double(endjahr)){
    tmp <- subset(tmp,anfangsjahr<=tmp$year & tmp$year<=endjahr)
  }
#  if(is.double(dauer)){
#   tmp <- subset(tmp,tmp$length<=dauer)
# }
  return(tmp)
}
(output <- queryfunc(m, NULL, 1980, 2000))
str(output)
# erstelle nun von Hand neue Teilmengen aus m nach Genre sortiert

millenium <- queryfunc(m,NULL, 2000, 2010)
nineties <- queryfunc(m,NULL, 1990, 2000)
eighties <- queryfunc(m,NULL, 1980, 1990)
seventies <- queryfunc(m,NULL, 1970, 1980) 
sixties <- queryfunc(m,NULL, 1960, 1970)
fifties <- queryfunc(m,NULL, 1950, 1960)

nurAction <- queryfunc(m,"Action", NULL, NULL)           # nur Actionfilme
nurAnimation <- queryfunc(m,"Animation", NULL, NULL)     # nur Animationsfilme
nurComedy <- queryfunc(m,"Comedy", NULL, NULL)           # nur Kom?dien
nurDrama <- queryfunc(m,"Drama", NULL, NULL)             # nur Dramen
nurDocumentary <- queryfunc(m,"Documentary", NULL, NULL) # nur Dokumentationen
nurRomance <- queryfunc(m,"Romance", NULL, NULL)         # nur Romanzen
nurShort <- queryfunc(m,"Short", NULL, NULL)             # nur Kurzfilme

action2k <- queryfunc(m,"Action", 2000, 2010)   # nur Actionfilme aus 2000 oder j?nger
action90 <- queryfunc(m,"Action", 1990, 2000)     # nur Actionfilme aus den 90ern
action80 <- queryfunc(m,"Action", 1980, 1990)     # nur Actionfilme aus den 80ern

animation2k <- queryfunc(m,"Animation", 2000, 2010) # nur Animationsfilme aus 2000 oder j?nger
animation90 <- queryfunc(m,"Animation", 1990, 2000)   # nur Animationsfilme aus den 90ern
animation80 <- queryfunc(m,"Animation", 1980, 1990)      # nur Animationsfilme aus den 80ern

kurz2k <- queryfunc(m,"Short", 2000, 2010)  # nur Kurzfilme aus 2000 oder j?nger
kurz90 <- queryfunc(m,"Short", 1990, 2000)    # nur Kurzfilme aus den 90ern
kurz80 <- queryfunc(m,"Short", 1980, 1990)    # nur Kurzfilme aus den 80ern

plot(m$year,m$length)
#Achsenskalierung anpassen

plot(m$year,m$length, las=1, xlab="Jahre", ylab="Filml?nge", main="Jahre gegen Filml?nge", 
     ylim=c(0,400), pch=16, cex=0.5, col="2", cex.main=2, cex.lab=1.5)

par(mfrow=c(3,3))
par(mfrow=c(1,1))

plot(nurAction$year, nurAction$length,           main="Länge der Actionfilme",     
     cex.main=2, xlab="Jahre", ylab="L?nge", cex.lab=1.5, las=1, pch=16, cex=0.4, col=4)
plot(nurAnimation$year, nurAnimation$length,     main="Länge der Animationsfilme", 
     cex.main=2, xlab="Jahre", ylab="L?nge", cex.lab=1.5, las=1, pch=16, cex=0.4, col=4)
plot(nurComedy$year, nurComedy$length,           main="Länge der Kom?dien",        
     cex.main=2, xlab="Jahre", ylab="L?nge", cex.lab=1.5, las=1, pch=16, cex=0.4, col=4)
plot(nurDocumentary$year, nurDocumentary$length, main="Länge der Dokumentationen", 
     cex.main=2, xlab="Jahre", ylab="L?nge", cex.lab=1.5, las=1, pch=16, cex=0.4, col=4)
plot(nurDrama$year, nurDrama$length,             main="Länge der Dramen",          
     cex.main=2, xlab="Jahre", ylab="L?nge", cex.lab=1.5, las=1, pch=16, cex=0.4, col=4)
plot(nurRomance$year, nurRomance$length,         main="Länge der Romanzen",        
     cex.main=2, xlab="Jahre", ylab="L?nge", cex.lab=1.5, las=1, pch=16, cex=0.4, col=4)
plot(nurShort$year, nurShort$length,             main="Länge der Kurzfilme",       
     cex.main=2, xlab="Jahre", ylab="L?nge", cex.lab=1.5, las=1, pch=16, cex=0.4, col=4)

par(mfrow=c(1,1))

table(m$year)

barplot(table(m$year), main="H?ufigkeit aller Filme", xlab="Jahre", ylab="H?ufigkeit", las=1, cex.main=2, cex.lab=1.5)

par(mfrow=c(3,3))

barplot(table(nurAction$year),       main="Action",      xlab="Jahre", ylab="H?ufigkeit der Filme", las=1, cex.main=2, cex.lab=1.5)
barplot(table(nurAnimation$year),    main="Animation",   xlab="Jahre", ylab="H?ufigkeit der Filme", las=1, cex.main=2, cex.lab=1.5)
barplot(table(nurComedy$year),       main="Comedy",      xlab="Jahre", ylab="H?ufigkeit der Filme", las=1, cex.main=2, cex.lab=1.5)
barplot(table(nurDocumentary$year),  main="Documentary", xlab="Jahre", ylab="H?ufigkeit der Filme", las=1, cex.main=2, cex.lab=1.5)
barplot(table(nurDrama$year),        main="Drama",       xlab="Jahre", ylab="H?ufigkeit der Filme", las=1, cex.main=2, cex.lab=1.5)
barplot(table(nurRomance$year),      main="Romance",     xlab="Jahre", ylab="H?ufigkeit der Filme", las=1, cex.main=2, cex.lab=1.5)
barplot(table(nurShort$year),        main="Short",       xlab="Jahre", ylab="H?ufigkeit der Filme", las=1, cex.main=2, cex.lab=1.5)

par(mfrow=c(1,1))

countByRatings <-function(m) {
  retVal <-c(1:9)
  for(rating in 0:9) {
    lower = rating
    upper = rating + 1
    movies <- subset(m, lower <= m$rating & m$rating < upper)
    count = nrow(movies)
    if (0 < count) {
      retVal[rating] <-count;
    }
  }
  return(retVal)
}

(countByRatings(action2k))
(countByRatings(action90))
(countByRatings(action80))


(countByRatings(millenium))
(countByRatings(nineties))
(countByRatings(eighties))

(countByRatings(m))

# man sieht, dass ein 6er Rating am h?ufigsten vergeben wurde (15981 mal)


##########################################
##########################################   Erster Teil fortgesetzt
##########################################

# kommen wir nun zu einer kleinen statistischen Analyse
# anhand eines multiplen Regressionsmodells soll die Wirkung der Filml?nge und den Votes auf das Rating
# untersucht werden
# zus?tzlich wird noch ein Interaktionsterm der L?nge und der Votes eingef?gt, um evtl. Multikollinearitaet
# ausgleichen zu k?nnen
# am Beispiel des gesamten Datensatzes soll das Vorgehen demonstriert werden

# zuerst l?sst man sich die Korrelation zwischen L?nge und Votes ausgeben
cor(m$length, m$votes)

# als n?chstes wird das Modell implementiert
model1 <- lm(m$rating ~ m$length + m$votes + m$length*m$votes)

# danach wird die summary des Modells erstellt
summary(model1)

# zuletzt werden vier Plots ausgegeben mit denen man die Annahmen der linearen Regression pr?fen kann
plot(model1)

# Nach dem gleichen Muster wird nun die Wirkung der Filml?nge und der Votes auf das Rating f?r verschiedene
# Teilmengen untersucht
# Daf?r soll eine Funktion verwendet werden

LinMod <- function(y,x1,x2){
  print(cor(x1,x2))
  z <- lm(y ~ x1 + x2 + x1*x2)
  print(summary(z))
  plot(z)
}

# Hier betrachtet man nur Actionfilme in den Jahren ab 2000

par(mfrow=c(2,2))

LinMod(action2k$rating, action2k$length, action2k$votes)

#cor(action2k$length, action2k$votes)
#model2 <- lm(action2k$rating ~ action2k$length + action2k$votes + action2k$length*action2k$votes)
#summary(model2)
#plot(model2)

# Hier betrachtet man nur Actionfilme in den Neunzigern

LinMod(action90$rating, action90$length, action90$votes)

#cor(action90$length, action90$votes)
#model3 <- lm(action90$rating ~ action90$length + action90$votes + action90$length*action90$votes)
#summary(model3)
#plot(model3)

# Hier betrachtet man nur Actionfilme in den Achtzigern

LinMod(action80$rating, action80$length, action80$votes)

#cor(action80$length, action80$votes)
#model4 <- lm(action80$rating ~ action80$length + action80$votes + action80$length*action80$votes)
#summary(model4)
#plot(model4)

# Hier bezieht man sich nur auf die Jahrzehnte

LinMod(millenium$rating, millenium$length, millenium$votes)
LinMod(nineties$rating, nineties$length, nineties$votes)
LinMod(eighties$rating, eighties$length, eighties$votes)





