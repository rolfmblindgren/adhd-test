# ADHD-TEST
# (Dette er ikke en ADHD-test)


[![deploy](https://github.com/rolfmblindgren/adhd-test/actions/workflows/deploy.yml/badge.svg)](https://github.com/rolfmblindgren/adhd-test/actions/workflows/deploy.yml)


Dette er en liten Shiny-applikasjon som viser hvordan fravær av oppmerksomhets- og reguleringsvansker kan se ut. Testen er ikke diagnostisk, men gir et bilde av hvilke hverdagslige ferdigheter som vanligvis fungerer greit hos personer uten ADHD-relaterte vansker.

## Funksjon

Applikasjonen består av ti utsagn. Brukeren vurderer hvor godt hvert utsagn beskriver dem over tid. Det beregnes deretter en T-skår samt en kort tolkning.

Data lagres i en SQLite-database for enkel logging av besvarelser.

## Psykometri

Intern konsistens var høy (ωₜ = .92). Generalfaktoren forklarte hoveddelen av variansen (ωₕ = .76, ECV = .69), noe som støtter bruk av en samlet totalskår.

## Filstruktur

```
adhd-test/
├── app.R                      # Shiny-applikasjon
├── README.md                  # Kort dokumentasjon
├── meta.yml                   # Metadata (tittel, beskrivelse, URL, OG-bilde osv.)
├── adhd.sqlite                # Lokal SQLite (dev/enkeltoppsett)
├── content/                   # Tekstlig innhold
│   ├── *.tskår.md              # Skåringstekster per språk (nb, nn, en, da, de, fr, se, smh, fkv, sv …)
│   └── translations/          # Oversettelser + konfig (CSV-ene er kilden)
│       ├── config.yml         # shiny.i18n-konfig
│       ├── translation.csv    # Masteroversikt (alle språk, semikolon)
│       └── translation_*.csv  # Per språk — disse leses av appen
├── docs/                      # Psykometrisk note per språk (no er original)
│   └── <språk>/adhd_psychometric_note.{tex,bib,pdf}
├── models/                    # Modeller / skåringsgrunnlag
│   └── mod1_grendel_1f_graded.rds
├── scripts/                   # Analyse-skript + artefakter
│   ├── adhd-stats.R
│   ├── fa*_loadings.csv       # Faktorladninger
│   └── scree_plot.png
└── www/                       # Statiske filer (må ligge her for Shiny)
    ├── custom.css             # CSS-overstyringer
    ├── custom.js              # JS (UI-triks)
    ├── og.png                 # Open Graph-bilde
    └── *.svg                  # Flagg/ikoner per språk
```

## Krav
	•	R ≥ 4.2
	•	shiny
	•	shinyjs
	•	shiny.i18n
	•	bslib
	•	tibble
	•	DBI
	•	RSQLite
	•	mirt
	•	grendelshiny
	•	shinyseo
	•	grendelStripe

På Ubuntu via shiny-server må brukeren som kjører appen ha skrivetilgang til data/-mappen.

## Miljøvariabler

Applikasjonen bruker:

```
ADHD_DB_PATH=/srv/shiny-server/data
ADHD_DB_NAME=adhd.sqlite
```

Disse settes sammen til stien til SQLite-databasen. Uten dem brukes
`./adhd.sqlite` i appmappen (greit for lokal utvikling).

## Database

Tabellene genereres automatisk av `dbWriteTable()` ved første besvarelse.
Kjernespørsmålene (item1–item10) lagres i `responses`, de eksperimentelle
spørsmålene (item11–item13) i `experimental_responses`. Begge har samme
kolonner:

```
timestamp   TEXT     -- felles for hele besvarelsen
item_id     TEXT
score       REAL
language    TEXT     -- aktivt språk ved innsending
```

For å inspisere data:

```
sqlite3 adhd.sqlite
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

Merk: `.gitignore` utelater hele `scripts/`-mappen for nye filer, slik at
dataeksporter og mellomfiler ikke havner i git ved et uhell. Filene som
allerede er sjekket inn der, spores som vanlig.
