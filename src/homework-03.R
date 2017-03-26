########################
# Házi feladat 3       #
# Programozás I.       #
# 2016/17. II. félév   #
# Kónya Viktória       #
# 2017-03-22           #
########################



# 2. Feladat -------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 2.1. Készíts egy listát, ami 5 vektort tartalmaz. Mindegyik vektor hossza legyen egy véletlenszerű 10 és 20 közötti szám. A vektorok (1,2) intervallumon egyenletes eloszlásból származzanak.
hossz <- vector()
vec <- c(1:5)

for (j in 1:length(vec)) {
  hossz[j] <- sample(10:20,1)
}

a<- runif(hossz[1] ,1.0 , 2.0)
b<- runif(hossz[2] ,1.0 , 2.0)
c<- runif(hossz[3] ,1.0 , 2.0)
d<- runif(hossz[4] ,1.0 , 2.0)
e<- runif(hossz[5] ,1.0 , 2.0)

l<-list(a,b,c,d,e)
l

# Kevésbé favágó módon
list.of.samples<-list()
for (i in 1:length(vec)) {
  list.of.samples[i] = lapply(1:length(vec), function(x) runif(hossz[i] ,1.0 , 2.0))
  
}
list.of.samples
class(list.of.samples)

# ------------------------------------------------------------------------------
# 2.2. Nézd meg for ciklussal, hogy az előbb létrehozott listának milyen hosszúak az elemei! A végeredmény legyen egy vektor.
list_hossz_1<-vector()

for (i in 1:length(vec)){
  list_hossz_1[i]<-length(list.of.samples[[i]])
}
list_hossz_1
class(list_hossz_1)

# ------------------------------------------------------------------------------
# 2.3. Nézd meg az ```apply``` függvénycsalád egy tagjával, hogy a listának milyen hosszúak az elemei! A végeredmény legyen egy lista.
list_hossz<- lapply(list.of.samples, length)
list_hossz
class(list_hossz)

# ------------------------------------------------------------------------------
# 2.4. Nézd meg az ```apply``` függvénycsalád egy tagjával, hogy a listának milyen hosszúak az elemei! A végeredmény legyen egy vektor.

list_hossz_2 <-sapply(list.of.samples, length, simplify=T)
list_hossz_2
class(list_hossz_2)



# 3. Feladat -------------------------------------------------------------------

# ------------------------------------------------------------------------------

# 3.1. Hívd be a ```chickwts``` datasetet, amit az R alapból tartalmaz.
data(chickwts)
View(chickwts)
chickwts <- as.data.frame(chickwts)

# ------------------------------------------------------------------------------
# 3.2. Nézd meg, mennyi az átlagsúlya a különbözőképp táplált csirkéknek! Használd az ```aggregate``` függvényt!
aggregate(chickwts$weight, by = list("Taplalasi tipus" = chickwts$feed), mean)

# ------------------------------------------------------------------------------
# 3.3. Az előbb kapott aggregált data frame-et rendezd az átlagsúly szerint csökkenő sorrendbe!
sulyatlag <- aggregate(chickwts$weight, by = list("Taplalasi tipus" = chickwts$feed), mean)
class(sulyatlag)
sulyatlag

sulyatlag_ord=sulyatlag[order(sulyatlag[,2]),]
sulyatlag_ord


# 4. Feladat -------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 4.1. Hozz létre egy 50 soros, 10 oszlopos mátrixot, aminek az értékei normális eloszlásból származnak. Minden sor szórása legyen egyenlő a sor számával.
mat_4 <- matrix(nrow=50, ncol=10)

szoras <- vector()
szoras <- (1:nrow(mat_4))
szoras

# ha a sorral megegyező szórású eloszlásból származik
X <- matrix(rnorm(500, sd=szoras), nrow = 50, ncol = 10)
X
sd(X[1,])


X2<-t(mapply(rnorm,10,sd=szoras))
sd(X2[1,])

# ------------------------------------------------------------------------------
# 4.2. Számold ki for ciklussal minden sor szórását! A végeredmény legyen egy vektor.
szoras_sor <-vector()
for (i in 1: nrow(X)){
  szoras_sor[i]<-sd(X[i,])
  
}
szoras_sor
is.vector(szoras_sor)

# ------------------------------------------------------------------------------
# 4.3. Számold ki az ```apply``` függvénycsalád egy tagjával minden sor szórását! A végeredmény legyen egy vektor.
szoras_sor2 <- apply(X, 1, sd)
is.vector(szoras_sor2)

# ------------------------------------------------------------------------------

# 4.4. Normalizáld a mátrix értékeit -1-től 1-ig tartó intervallumra és nézd meg a sorok átlagát!
normalize <- function(x) { return (-1+2*(x - min(x)) / (max(x) - min(x))) }
X3 <- normalize(X)
X3

norm_atl <- apply(X3, 1, mean)
norm_atl

# 5. Feladat -------------------------------------------------------------------
# ------------------------------------------------------------------------------
# 5.1 1. Hívd be a előző házikban is használt ```fivethirtyeight``` package-et és olvasd be a ```comic_characters``` datasetet!
require(fivethirtyeight)
data("comic_characters")
View(comic_characters)

# ------------------------------------------------------------------------------
# 5.2. Alakítsd át a ```name``` nevű oszlopot úgy, hogy minden karakternek csak az elsődleges neve maradjon ott mindenféle zárójeles rész nélkül. Például "Spider-Man (Peter Parker)" helyett "Spider-Man", "Benjamin Grimm (Earth-616)" helyett "Benjamin Grimm" maradjon. Használhatod a feladathoz pl. az ```strsplit``` függvényt és az ```apply``` függvénycsalád egy tagját, de használj nyugodtan mást is, csak arra figyelj, hogy a megoldásodban ne legyen for ciklus!
comic_characters$name_pri <- sapply(strsplit(as.character(comic_characters$name), split = " \\("), "[[", 1)
comic_characters$name_pri

# Ellenorzes
comic_characters$name_pri[1]
comic_characters$name_pri[2]



# 5.3. Írj egy függvény ```get_gender``` néven, aminek az a célja, hogy egy karakter nevének megadásakor visszaadja a karakter genderét. A karakter genderét úgy add vissza, hogy ha a ```gsm``` oszlopban ```NA``` érték van, akkor a ```sex``` oszlop tartalmát írja ki a függvény, viszont ha a ```gsm``` oszlop értéke nem ```NA```, akkor a ```gsm``` oszlop tartalmát írja ki. A gender kiírásakor vágd le a " Characters" részt, tehát pl. "Male Characters" helyett "Male"-t, "Female Characters" helyett "Female"-t írjon ki a függvény. A függvényben ne legyen for ciklus! Figyelj arra is, hogy a 2. feladatban végrehajtott átalakítás miatt egy név többször is szerepelhet a ```name``` oszlopban. Ilyen esetekben minden egyező név esetén nézze meg a gendert és vektorként térjen vissza az értékeikkel.
# 5.4. Nézd meg az előzőleg írt ```get_gender``` függvénnyel, hogy milyen genderű Thor, Katherine Pryde és Loki Laufeyson! Thornál és Lokinál egy vektort kell kapnod.

get_gender("Thor")
get_gender("Loki Laufeyson")
get_gender("Katherine Pryde")

# Ellenőrzés
table(comic_characters$name_pri)["Thor"]
table(comic_characters$name_pri)["Loki Laufeyson"]
table(comic_characters$name_pri)["Katherine Pryde"]

