# Tower Defense Game entwickelt in elm
Tower Defense Games sind alt und langweilig? Mitnichten! Dieses Tower Defense Spiel verleiht dem Genre einen neuen spannenden Aspekt durch das flexible Platzieren der Türme. Klassicherweise versuchen üble Schergen das Schlachtfeld zu überqueren, um deine Basis zu attackieren. Auf dem Weg dorthin kannst Du es ihnen aber schwer machen und zahlreiche Türme in den Weg stellen. Wo Du die Türme platzierst, ist dabei völlig Dir selbst überlassen. Die Angreifer können Deine Türme nicht durchqueren und müssen so Umwege laufen. Nutze also die Türme als Barrikaden um den Angriff zu verlangsamen und die Angerifer genau dorthin zu leiten, wo Du sie haben möchtest.

## Spielanleitung
Die Angreifer erscheinen mittig auf der linken Seite des Schlachtfeldes und werden sich zielstrebig zum rechten Schlachtfeldrand bewegen. So hälst Du sie auf:
1. Klicke auf eine beliebige Position auf dem Schlachtfeld um einen Bauplatz auszuwählen.
2. Klicke rechts oben auf die Schaltfläche zum Erstellen eines Turmes oder drücke **Q** auf deiner Tastatur.
3. Möchtest Du den Turm reparieren, wähle diesen aus und klicke auf *Repair* oder drücke **W**.
4. Um den Turm abzureißen, wähle ihn aus und klicke auf *Destroy* oder drücke **E**.

Ein Turm verursacht nach dem Bau Schaden an Angreifern in der Nähe. Mit steigender Nutzungsdauer nutzt der Turm sich jedoch ab und verliert an Schaden. Du erkennst die Stärke der Abnutzung an dem farbigen Indikator auf dem Turm. Ist ein Turm vollständig abgenutzt, verursacht also gar keinen Schaden mehr, zerstört dieser sich selbst. Verhindere dies indem Du den Turm rechtzeitig reparierst. Das Bauen und Reparieren von Türmen kostet Dich Geld. Du erhälst Geld für jeden getöten Angreifer und beim Abriss von Türmen. Wenn Du einen Turm abreißt erhälst Du jedoch nur die Hälfte des Baupreises zurück, also überlege es Dir vorher gut!

Sollte es Dir einmal zu schnell gehen, kannst du das Spiel jederzeit pausieren und weiterhin Deine Verteidigung optimieren. Deine Türme greifen beim pausierten Spiel natürlich nicht mehr an, aber auch die Angreifer legen eine Pause ein.

## Struktur und Nutzung der Quelldateien
Im Ordner *src* befinden sich die elm-Dateien bestehend aus den Main-Module *TowerDefense.elm* und den weiteren Modulen *Points.elm*, *Bots.elm*, *Towers.elm* und *Lists.elm*. Außerdem liegt hier eine bereits erstellte HTML-Datei zum Ausführen des Spiels. Im Ordner *src/Graphics* finden sich alle im Spiel verwendeten Grafiken.

Das Spiel kann aus den Quelldateien neu erstellt werden, indem im Root-Verzeichnis der Befehl `elm make src/TowerDefense.elm` ausgeführt wird. Um das mit die Befehl erstellte Dokument *index.html* korrekt darzustellen, muss sich der Ordner mit den Grafiken im selben Verzeichnis befinden. *index.html* muss vor dem Ausführen also ggf. verschoben werden.

## Quellen
Die im Spiel verwendeten Grafiken wurden nicht vom Entwickler erstellt, sondern stammen aus folgenden Quellen:
- [Sprites für Angreifer, Buttons und Geld; Farbschema](https://opengameart.org/content/tower-defense-300-tilessprites)
- [Symbol für Lebenspunkte](https://www.pngfuel.com/free-png/crffx)