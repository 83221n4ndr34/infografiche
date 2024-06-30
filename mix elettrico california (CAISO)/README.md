# Analisi della Produzione di Elettricità in California

## Dati

Dati importati grazie alle [API di GridStatus](https://www.gridstatus.io/api).

## Grafico

Il grafico rappresenta la **produzione di elettricità in California** ed il suo **prezzo offerto in borsa elettrica** per il mese di **Maggio 2024** (il prezzo riportato è il [prezzo locale marginale in tempo reale ad intervalli di 15m]([url](https://docs.gridstatus.io/en/stable/lmp.html))).
In particolare, i valori per il mix elettrico ad **intervalli di 5m** sono calcolati facendo la media delle 5 settimane nel periodo 29/04/2024 - 02/05/2024 in modo da estrarre una “**settimana tipo**”.
Il mix elettrico è disaggregato in **4 componenti**: produzione **solare**, accumulo a **batterie**, **interscambio** (differenza import - export) e la **produzione rimanente** (quindi altre rinnovabili, nucleare e tutte le fonti fossili).
È poi aggiunta una linea nera che rappresenta la **domanda di elettricità** dai consumatori (che quindi non comprende né l’export, né il caricamento delle batterie, né l’autoconsumo).

![Produzione di elettricità in California disaggregata per fonte. Elaborazione personale su dati GridStatus, 2024](mix%20elettrico%20california%20(CAISO)/Canva%20-%20California,%20Maggio%202024%20Produzione%20elettrica%20disaggregata%20per%20solare,%20batterie,%20import%20-%20export,%20produzione%20rimanente.jpg)


## Interpretazione

Questo grafico è usato per discutere la differenza fra costo e valore dell'elettricità.
Il grafico è fondamentale per illustrare le due componenti principali di questa differenza: da un lato l’andamento della domanda di elettricità e dall'altro il profilo di produzione specifico di un determinato mix energetico.
Infatti come si può vedere dal grafico:
1. La **domanda di elettricità** (linea nera) non è costante, ma segue un andamento giornaliero e settimanale. In questo specifico caso, è inoltre evidente che la domanda risenta significativamente dell'**autoconsumo** dei pannelli fotovoltaici residenziali, come dimostrato dal notevole calo della domanda nelle ore centrali del giorno.
2. Durante le ore di picco solare, la **produzione fotovoltaica** supera ampiamente la domanda, consentendo il caricamento delle batterie e l'export netto. Tuttavia, a causa della limitata capacità di accumulo e delle restrizioni nelle interconnessioni, parte della produzione viene scartata. Questo **eccesso di offerta** porta a prezzi molto bassi o addirittura negativi durante le ore con alta insolazione, ma i prezzi aumentano rapidamente quando la produzione solare diminuisce e la domanda serale cresce.

Questo fenomeno, noto come "**curva dell'anatra**" ("[duck curve]([url](https://en.wikipedia.org/wiki/Duck_curve))"), è emblematico per comprendere il concetto di “valore dell’elettricità”:
l’elettricità immessa in rete nel picco di produzione solare non ha lo stesso valore di quella immessa nei momenti serali al picco della domanda.
