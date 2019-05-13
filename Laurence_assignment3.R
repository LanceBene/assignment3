## Die Library dplyr wird aufgerufen, da die "recode" Funktion sehr komfortabel ist
# Zur Dokumentation der einzelnen Befehle in der R Dokumentation siehe ?Befehl
library(dplyr)

# PatternToNumber wird als Funktion mit der Variable Pattern definiert
PatternToNumber <- function(Pattern) {
  
# Pattern wird in einzelne Elemente aufgeteilt, da strsplit eine Liste erzeugt, wird dies durch unlist aufgehoben
SplitPattern <- unlist(strsplit(Pattern, split = ""))

# Mit Recode (aus dem Paket dplyr) werden den einzelnen Basen Zahlenwerte zugewiesen
BaseToNumber <- recode(SplitPattern, "A"=0, "C"=1, "G"=2, "T"=3)

# Die leere Liste Prefix wird erstellt
Prefix <- {}

# In einem For-Loop wird die so erstellte Nummer in einen Zahlenwert zur Basis 4 umgerechnet
for (i in 0:length(BaseToNumber)) {
  Prefix[i] <- (BaseToNumber[i])*(4^(length(BaseToNumber)-i))
  BaseAsNumber <- sum(Prefix)
}
# Die Basenkombination im Vierersystem wird mit maximal 22 Stellen ausgegeben
print(BaseAsNumber, digits = 22)
}

PatternToNumber("AGT")
PatternToNumber("CTTCTCACGTACAACAAAATC")

## Die Funktion NumberToPattern mit den Variablen Number und k wird definiert
NumberToPattern <- function(Number, k){

# Quotient und Rest werden als leere Liste definiert
Quotient <- {}
Remainder <- {}

# Der erste Quotient und Rest werden durch teilen der Ausgangsnummer durch 4 ermittelt
Quotient[1] <- Number%/%4
Remainder[1] <- Number%%4

# Weitere Werte werden durch den For-Loop errechnet und in Reihenfolge gespeichert
for (i in 1:(k-1)) {
  Quotient[i+1] <- Quotient[i]%/%4
  Remainder[i+1] <- Quotient[i]%%4
}

# Die Reihenfolge der Reste wird umgekehrt
Reversed <- rev(Remainder)

# Die Buchstaben für die einzelnen Basen werden den Zahlenwerten der Reste zugewiesen
NumberAsPattern <- recode(Reversed, "0" = "A", "1" = "C", "2" = "G", "3" = "T")

# Mit paste werden die Werte zu einem einzigen kombiniert
NumberAsPattern <- paste(NumberAsPattern, collapse="")

# Die Basenkombination wird ausgegeben
print(NumberAsPattern)
}

NumberToPattern(45, 4)
NumberToPattern(5353, 7)

FrequencyArray <- {}
kmer <- {}
Index <- {}
Frequency <- {}

## Diese Funktion schneidet leider immer das erste kmer (AA) ab, alle anderen Werte sind korrekt. 
# Ausserdem gibt sie deutlich mehr aus als den gewünschten Dataframe, was vor allem bei der nächsten Funktion nervt.
# ComputingFrequencies mit den Variablen text und k wird als Funktion definiert
ComputingFrequencies <- function(text, k) {
  
# kmer, Index und Frequency werden als leere Listen definiert
kmer <- {}
Index <- {}
Frequency <- {}

# Mittels eines For-Loops werden die Listen befüllt:
# kmer werden mit der zuvor definierten NumberToPattern alle Basenpaare der Länge k zugewiesen
# Index wird von 0 bis 4^k durchnummeriert
# Frequency erhält an allen Stellen den Wert 0
for(i in 0:(4^k)) {

  kmer[i] <- NumberToPattern(i, k)

  Index[i] <- i

  Frequency[i] <- 0
}

# In diesem For-Loop wird nun der Text für jedes kmer durchgegangen und Frequency um 1 erhöht
for (i in 0:(nchar(text)-k)) {
# Pattern 2 wird als leere Liste definiert und dann mit dem jeweiligen Textausschnitt der Länge k befüllt  
  Pattern2 <- {}
  Pattern2 <- substr(text, i+1, i + k)
# j wird als leere Liste definiert und dann mit der Patternnummer im Vierersystem nach der zuvor erstellten Funktion befüllt
  j <- {}
  j <- PatternToNumber(Pattern2)
# Frequency wird an der jeweiligen Stelle der Patternnummer um 1 erhöht
  Frequency[j] <- (Frequency[j] +1)
}

# Die drei Vektoren werden in einem Dataframe zusammengefasst, im Worksprace gespeichert und ausgegeben
FrequencyArray <<- tibble(kmer, Index, Frequency)
print(FrequencyArray)
}
ComputingFrequencies("ACGCGGCTCTGAAA", 2)
ComputingFrequencies("ACTTCGCCTAAGTCATTTATCCCGTGGTACGACGCTCCCTTACAGTCTTATATCCCGGTATATACGCAGAAATGCCTACGTCCCCTCGTCCCACACACCAGGGAAGCTGAAATCGCTCATCTACTATGCGTGTACTTCCGGACGAAATCGTCGTCGGCTTCTGTCTGGCGCTGGAGATCCGGGCTTCTTGAGGGACACACCCATTATGACCGTTACAGGACTTACAACTACTCTGAGCAATGATGGTGCTCTGTAACGAACAAACGCACTCACCTCTGTTTCCTGTATGACATCCTCAAATGGATCGACCGTGATGTACTGAGCGAATAAGTGCGGATTACATTTATAGTCAGCTACATTTATTCGCCGCTCGGAGCAGAGTATAATGAATTTATACCACTTGTTAGACTCCTTCTCGCATTTAGCCCCTACCGCAAGTCGGAGCGTTGGGGTGCAATAGAGTTTTCAGTATCTACGTACCGTTAAGTCTCTCGCGTTCTTTCAGCAGGCATCAATATGTTGCTTGCTGTGGGGTCGGGTGGGGCGGAGAGCCAATAAAGTGCATCGGAATTGGCTGCCCTCCTACGAATCCGCAAGATGCGGTGATGCTACGTGATTATGACTACTAGCTTAGTCCC
", 6)


## Diese Funktion gibt immer den ganzen Output von ComputingFrequencies mit an. 
# Ausserdem wird das Ergebnis doppelt ausgegeben.
# Fasterfrequentwords wird als Funtion mit den Variablen text und k definiert
Fasterfrequentwords <- function(text, k){
# FrequentPatterns wird als leere Liste erstellt  
FrequentPatterns <- {}  
# Ein Frequency Array wird mit der zuvor erstellten Funktion ComputingFrequencies erstellt
FrequencyArray2 <- ComputingFrequencies(text, k)
# maxCount wird als höchster Wert der Frequency des Frequency Arrays definiert
maxCount <- max(FrequencyArray2$Frequency)

# In einem For-Loop werden die Pattern mit den höchsten Werten für Frequency als Patternnummer ausgegeben...
for(i in 1:((4^k)-1)) {
  if (FrequencyArray2$Frequency[i]==maxCount){
  Pattern<-{}
  Pattern <- NumberToPattern(i, k)
#...und in FrequentPatterns abgelegt  
  FrequentPatterns <- append(FrequentPatterns, Pattern)
  }}
# Duplikate werden aussortiert
  UniqueFrequentPatterns <- unique(FrequentPatterns)
# Und die häufigsten Werte ausgegeben. Diese Funktion ist deutlich schneller als die Frequentwords Funktion vom Übungszettel 2  
  print(UniqueFrequentPatterns)
}

Fasterfrequentwords("AAGCAAAGGTGGG", 2)


