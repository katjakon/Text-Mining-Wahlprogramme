# Text-Mining-Wahlprogramme
Ein Projekt für das Seminar _Textanalyse in den Politikwissenschaften_

# Idee und Fragestellung

In Wahlprogrammen stellen Parteien ihre Meinung und Ausrichtung dar und positionieren sich klar zu Themen, die ihnen wichtig erscheinen.<br>
Das wirft unter anderem folgende Fragen auf:

+ Gibt es Begriffe, die __typisch für bestimmte Parteien/Ausrichtungen__ sind?
+ Wie __präsent__ sind Themen wie Klimawandel in Wahlprogrammen?
+ Stellen Parteien, die zum Zeitpunkt der Wahl an der Regierung beteiligt waren, die Lage in Deutschland positiver dar?

# Benötigte R-Bibliotheken
+ `quanteda`, siehe [hier](https://quanteda.io/) für Informationen zur Installation.
+ `readtext`, siehe [hier](https://readtext.quanteda.io/index.html) für Informationen zur Installation.
+ `udpipe`, siehe [hier](https://cran.r-project.org/web/packages/udpipe/index.html) für Informationen zur Installation.
+ `tidyverse`, siehe [hier](https://www.tidyverse.org/packages/) für Informationen zur Installation.

# Erklärungen zum Code
Für jeden Abschnitt im Bericht können auf folgender Seite der Code, mit dem die Untersuchungen durchgeführt wurden, und dazugehörige Erklärungen abgerufen werden:<br>
https://katjakon.github.io/Text-Mining-Wahlprogramme/


# Korpus
Das Korpus enthält Wahlprogramme zur Bundestagswahl von 2002 bis 2017. Die Originale sind hier abrufbar:
+ [CDU](https://www.kas.de/de/web/geschichte-der-cdu/wahlprogramme-und-slogans)
+ [SPD](https://www.fes.de/bibliothek/grundsatz-regierungs-und-wahlprogramme-der-spd-1949-heute)
+ [FDP](https://www.freiheit.org/de/wahlprogramme-der-fdp-zu-den-bundestagswahlen)
+ [AfD 2013](https://www.abgeordnetenwatch.de/bundestag/wahl-2013/wahlprogramme)
+ [AfD 2017](https://www.afd.de/wahlprogramm/)
+ [PDS, später DIE LINKE](https://www.rosalux.de/stiftung/historisches-zentrum/archiv/download)
+ [B90/die Grüne 2002-2009](https://www.boell.de/de/navigation/archiv-4289.html)
+ [B90/die Grüne 2013](https://www.bundestagswahl-bw.de/wahlprogramm-die-gruenen)
+ [B90/die Grüne 2017](https://www.gruene.de/artikel/gruenes-wahlprogramm-zur-bundestagswahl-2017-zukunft-wird-aus-mut-gemacht)

Die Dateien wurden mithilfe von `pdftotext` und optical character recognition in Text-Dateien umgewandelt. Die manuelle Nachbearbeitung beinhaltete folgende Punkte:

+ Entfernung von Seitenzahlen
+ Entfernung von Impressen
+ Entfernung von Seitenumbrüchen 
+ Entfernung von Worttrennungen
+ Korrektur von falsch erkannten Zeichen
+ Korrektur von vertauschten Absätzen durch zweispaltiges Layout

# Autoren
+ Anina Klaus (aklaus@uni-potsdam.de)
+ Katja Konermann (katja.konermann@uni-potsdam.de)
+ Niklas Stepczynski (stepczynski@uni-potsdam.de)
