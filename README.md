# ADHD-TEST
# (Dette er ikke en ADHD-test)


[![build-and-deploy](https://github.com/rolfmblindgren/personlighetstest/actions/workflows/build-and-deploy.yml/badge.svg)](https://github.com/rolfmblindgren/personlighetstest/actions/workflows/build-and-deploy.yml/badge.svg)


Dette er en liten Shiny-applikasjon som viser hvordan fravær av oppmerksomhets- og reguleringsvansker kan se ut. Testen er ikke diagnostisk, men gir et bilde av hvilke hverdagslige ferdigheter som vanligvis fungerer greit hos personer uten ADHD-relaterte vansker.

## Funksjon

Applikasjonen består av ti utsagn. Brukeren vurderer hvor godt hvert utsagn beskriver dem over tid. Det beregnes deretter en gjennomsnittsskår samt en kort tolkning.

Data lagres i en SQLite-database for enkel logging av besvarelser.

## Filstruktur

```
adhd-test/
├── app.R                 # Shiny-appen
├── custom.css            # Tilpasninger i layout
└── data/                 # Skrivebart område
    ├── adhd_test.sqlite  # Database (opprettes automatisk)
    └── app_cache         # Shiny cache
```

## Krav
	•	R ≥ 4.2
	•	shiny
	•	tibble
	•	DBI
	•	RSQLite

På Ubuntu via shiny-server må brukeren som kjører appen ha skrivetilgang til data/-mappen.

## Miljøvariabler

Applikasjonen bruker:

```
SHINY_DATA_DIR=/srv/shiny-server/data
```

Dette brukes til å finne både SQLite-databasen og Shiny-cache-mappen.

## Database

Tabellen genereres automatisk:

```
CREATE TABLE responses (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TEXT NOT NULL,
    item_id TEXT NOT NULL,
    score INTEGER NOT NULL
);
```

For å inspisere data:

```
sqlite3 adhd_test.sqlite
sqlite> SELECT * FROM responses LIMIT 20;
```

## Distribusjon på shiny-server

    1.    Plasser appen i f.eks.

```
/srv/shiny-server/adhd-test/
```

    2.    Lag en egen data-mappe:

```
sudo mkdir -p /srv/shiny-server/data
sudo chown -R shiny:shiny /srv/shiny-server/data
```

    3.    Start shiny-server på nytt:

```
sudo systemctl restart shiny-server
```

## GitHub Actions (valgfritt)

Dersom appen deployes automatisk via deployshiny, må nøklene ligge i
repo-secrets, og brukeren må ha passordløs tilgang til:

```
sudo systemctl restart shiny-server
sudo chown -R deployshiny:deployshiny <mappe>
```

eller alternativt settes opp med felles gruppe.

## Forbehold

Testen sier ingenting om diagnose. En klinisk vurdering krever utviklingshistorie, funksjon, observasjon og faglig skjønn. Skåren må tolkes som et grovt mønster, ikke som en medisinsk konklusjon.

