# ADHD-TEST
# (Dette er ikke en ADHD-test)


[![deploy](https://github.com/rolfmblindgren/adhd-test/actions/workflows/deploy.yml/badge.svg)](https://github.com/rolfmblindgren/adhd-test/actions/workflows/deploy.yml)


Dette er en liten Shiny-applikasjon som viser hvordan fravær av oppmerksomhets- og reguleringsvansker kan se ut. Testen er ikke diagnostisk, men gir et bilde av hvilke hverdagslige ferdigheter som vanligvis fungerer greit hos personer uten ADHD-relaterte vansker.

## Funksjon

Applikasjonen består av ti utsagn. Brukeren vurderer hvor godt hvert utsagn beskriver dem over tid. Det beregnes deretter en gjennomsnittsskår samt en kort tolkning.

Data lagres i en SQLite-database for enkel logging av besvarelser.

## Filstruktur

```
adhd-test/
├── app.R                     # Shiny-applikasjon
├── README.md                 # Kort dokumentasjon
├── meta.yaml                 # Metadata (tittel, beskrivelse, URL, OG-bilde osv.)
├── mod1_grendel_1f_graded.rds# Modell / skåringsgrunnlag (RDS)
├── *.tskår.md                # Skåringstekster per språk (nb, nn, en, da, de, fr, se, smh, fkv, sv …)
├── translation*.csv          # Oversettelser (CSV per språk + master)
├── translation.numbers       # Redigeringskilde (Numbers)
├── og.png                    # Open Graph-bilde
├── og.graffle                # Kilde for OG-grafikk (OmniGraffle)
├── app_cache/                # Cache generert ved kjøring (gitignored i praksis)
│   └── sass/                 # Sass/bs-theme cache fra Shiny
└── www/                      # Statiske filer
    ├── custom.css            # CSS-overstyringer
    ├── custom.js             # JS (små UI-triks / tracking e.l.)
    ├── og.png                # (ev. kopi for serving)
    └── *.svg                 # Flagg/ikoner per språk
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

