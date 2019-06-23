shinyStats:
#ACHTUNG:
	wenn man app das 1. mal startet gibt es oft einen fehler weil data frame nicht richtig gelesen wird.
	bis wir es richten kann man daten auch manuell in rstudio einlesen und erst nacher app starten 

A) Projekt Aufbau: Für jede Aufgabe eigener Folder 
	UPDATE: hab jz 1 + 2 in der selben app gemacht
B) Angabe / ToDos / Fragen :  

	1. explorativer Datenanalyse für den Datensatz "swiss"
		- Frage: sollen wir alle Variablen von swiss verwenden oder so wie in der Hü angabe? 
	2. lineare Regression zur Erklärung der Variable "education" durch ein multiples lineares Regressionsmodell 
		mit Möglichkeiten zur interaktiven Auswahl von Variablen, 
		Modellselektion und Transformation von Variablen und Überprüfung der Qualität und Voraussetzungen des 
		linearen Regressionsmodells

	3. verallgemeinerte nichtlineare Regression: 
		logistisches Regressionsmodell mit Möglichkeiten zur interaktiven Auswahl von Variablen, 
		Modellselektion und Transformation von Variablen
		Datensatz: pima (indien) ~ andere sinnvole variablen vorhersagen 

C) Allgemeine Orga: 
	- sollen wir und 'clean code' sachen überlegen?
	- Aufgaben verteilen und deadlines? 
	- Tutorials: 
		- Verständniss / Überblick: https://www.youtube.com/watch?v=G1-HDDDK87M
		- Code + verwurstbare Bsp. : http://rstudio.github.io/shiny/tutorial/
	- fällt euch nochwas ein ? 

# TODO: 
- Thea: Data View: select all button 
- Thea: Ordnen der Provinzen nach Himmelsrichtungen

- Explore Variable:
	- Thea: default Boxplot zeigen zu No setzen
	- Thea: 'Summary erzeugen'
	- ?: git man möglickeit zur log transformation + möglichkeit anzugeben ob man es für model verwenden kann / es NV ist 
 
- View Scatterplot: 
	- Richard: Auswahl der Variablen 
	- Richard: Negative Korrelation anzeigen 
	- ?: Slider um Korrelationsgrenze zu setzten
	-  
- Linear Model: 
	- Transformationen: log, ?
	- Eva: Hebelpunkte raus nehmen 
	- Eva: automatische modelselection
