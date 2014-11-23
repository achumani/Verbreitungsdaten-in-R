Verbreitungsdaten-in-R
======================

Das Ziel ist es, Verbreitungsgebiete von beliebigen Tierarten mit den Klima- oder Morphologiedaten in ihren Verbreitungsgebieten vergleichen zu können.

Tierarten werden als shapefiles von iucnredlist.org runtergeladen (anhand ihrer Tier-ID).
Klima oder sonstwasdaten liegen meistens als Rasterdaten vor.
Die Verbreitungsdaten müssen somit ebenfalls rasterized werden.
Alle Rasters müssen im selben Maßstab/Zellauflösung vorliegen, damit sie (wie im Modul Umweltstatisitk) miteinander verglichen werden können.

Erst werden die ganzen Schritte manuell durchgefhrt.
Wenn sie klappen, dann wird der gesamte Vorgang automatisiert.
Schließlich werden verschiedenen Tierarten und Rasterdatenquellen verwendet, um die Funktionabilität zu testen.

Wichtige Punkte: Die Projektionen der einzelnen Daten automatisch erkennen und anpassen. Nicht nur dass sie gleich sind, sondern dass sie im Verbreitungsgebiet eine flächentreue Projektion ergeben (winkeltreue ist zu vernachlässigen)
                 Die Untersuchungsgebiete könnten per Hand mit getPoly(?) ausgewählt werden.
                  
http://www.r-bloggers.com/session-1-gridding-data-for-multi-scale-macroecological-analyses/