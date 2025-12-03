library(shiny)
library(shinyjs)
library(tibble)
library(DBI)
library(RSQLite)
library(shiny.i18n)

options(bslib.cache = FALSE)

i18n <- Translator$new(translation_csvs_path =
                         Sys.getenv("ADHD_DB_PATH"),
                       translation_csv_config=
                         paste0(Sys.getenv("ADHD_DB_PATH"),"config.yml"))

i18n$set_translation_language("nb")
print (i18n$get_languages())

ui <- fluidPage(
  
  usei18n(i18n),
  useShinyjs(),
  tags$head(
         tags$script(src = "custom.js"),
         tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
       ),
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel(i18n$t("Dette er ikke en ADHD-test")),
  sidebarLayout(
    sidebarPanel(



  selectizeInput(
  inputId = "selected_language",
  label = i18n$t("Skift språk"),
  choices = c("nb", "nn", "se", "fkv", "fr", "de", "en"),
  selected = "nb",   # <- viktig
  options = list(
    render = I("
      {
        option: function(item, escape) {
          return Shiny.renderFlagOption(item);
        },
        item: function(item, escape) {
          return Shiny.renderFlagOption(item);
        }
      }
    ")
  )
)


 ,



  h4(i18n$t("Kort om testen")),
  p(i18n$t("Denne testen viser hvordan fravær av oppmerksomhets- og reguleringsvansker kan se ut.")),
  p(i18n$t("Den er ikke diagnostisk. Den kan verken bekrefte eller avkrefte ADHD.")),
  br(),
  actionButton("beregn", i18n$t("Beregn resultat"))
  ),

  mainPanel(
    tabsetPanel(id="tabs",
                tabPanel(
                  value = "spm",
                  i18n$t("Spørsmål"),
                  br(),
                  p(i18n$t("Dra i hver slider for å velge hvor godt utsagnet har stemt for deg over tid.")),
                  hr(),
                  uiOutput("sporsmals_ui")

                ),
                tabPanel(
                  value = "res",
                  i18n$t("Resultat"),
                  br(),
                  h3(i18n$t("Tolkning")),
                  textOutput("resultat_tekst"),
                  br(),
                  h4(i18n$t("Gjennomsnittsskår")),
                  textOutput("score_tekst"),
                  br(),
                  h4(i18n$t("Forbehold")),
                  p(i18n$t("En klinisk vurdering innebærer utviklingshistorie, funksjon og faglig skjønn.")),
                  p(i18n$t("Mennesker kan ha lav struktur eller høy fart uten at det handler om ADHD."))
                )
                )
  )
  )
)

server <- function(input, output, session) {

  items_r <- reactive({

    lang <- input$selected_language   # <- gjør reactive() avhengig av språket
    i18n$set_translation_language(lang)

    tibble(
      id = paste0("item", 1:10),
      tekst = c(
        i18n$t("Jeg klarer vanligvis å starte på kjedelige oppgaver uten særlig friksjon"),
        i18n$t("Jeg mister sjelden viktige ting, og finner dem raskt hvis det skjer"),
        i18n$t("Jeg blir stort sett oppfattet som pålitelig når det gjelder avtaler og frister"),
        i18n$t("Jeg klarer som regel å holde fokus på én oppgave til den er fullført"),
        i18n$t("Jeg er ikke avhengig av kaos eller siste liten for å komme i gang"),
        i18n$t("Jeg blir ikke så rastløs at jeg må bevege meg eller avbryte andre"),
        i18n$t("Jeg glemmer ikke avtaler eller beskjeder i en grad som skaper problemer"),
        i18n$t("Når noe er viktig, klarer jeg å organisere tiden så det får plass"),
        i18n$t("Tankene løper ikke så ofte løpsk at det hindrer meg i hverdagen"),
        i18n$t("Jeg trenger ikke uvanlig mye ytre struktur for å fungere greit")
      )
    )
  })


  output$sporsmals_ui <- renderUI({
    items <- items_r()   # <- reaktiv tibble
    lapply(seq_len(nrow(items)), function(i) {
      item <- items[i, ]

      div(style = "width: 100%; max-width: 600px;",
          strong(item$tekst),
          div(class = "scale-labels",
              HTML(sprintf(
                "<span>%s</span>
     <span>%s</span>
     <span>%s</span>
     <span>%s</span>
     <span>%s</span>",
     i18n$t("Stemmer ikke"),
     i18n$t("Litt"),
     i18n$t("Delvis"),
     i18n$t("Ganske"),
     i18n$t("Stemmer helt")
     ))
     ),
     sliderInput(
       inputId = item$id,
       label = NULL,
       min = 1,
       max = 5,
       value = 3,
       step = 1,
       ticks = FALSE,
       width = "100%"
     ),
     br()
     )
    })
  })


  db_path <- paste0(Sys.getenv("ADHD_DB_PATH"),Sys.getenv("ADHD_DB_NAME"))

  observeEvent(input$selected_language, {
    i18n$set_translation_language(input$selected_language)
    update_lang(language=input$selected_language,session=session)
    shinyjs::html("beregn", i18n$t("Beregn resultat"))

  })

  observeEvent(input$beregn, {

    shinyjs::disable("beregn")
    shinyjs::html("beregn", paste0(i18n$t("Sendt"), "✔"))

    ## (valgfritt) re-enable etter 5 sekunder
    shinyjs::delay(5000, shinyjs::enable("beregn"))
    con <- dbConnect(SQLite(), db_path)

    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

    item_ids <- items_r()$id   # ta ID-er fra tibblen, ikke fra input

    scores <- vapply(item_ids, function(id) input[[id]], numeric(1))

    df <- data.frame(
      timestamp = rep(timestamp, length(item_ids)),
      item_id = item_ids,
      score = scores,
      language = rep(input$selected_language, length(item_ids))

    )

    dbWriteTable(con, "responses", df, append = TRUE)

    dbDisconnect(con)

    updateTabsetPanel(session, "tabs", selected = "res")

  })


  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  score_reaktiv <- eventReactive(input$beregn, {


    ## hent alle svar som liste
    svar <- lapply(items_r()$id, function(id) input[[id]])

    ## sjekk ubesvarte
    mangler <- vapply(svar, function(x) is.null(x) || is.na(x), logical(1))

    if (any(mangler)) {
      return(list(
        gyldig = FALSE,
        beskjed = i18n$t("Du må svare på alle utsagn før resultatet kan beregnes."),
        score = NA
      ))
    }

    ## flate ut og beregne
    svar_num <- as.numeric(unlist(svar))
    mean_score <- mean(svar_num)

    list(
      gyldig = TRUE,
      beskjed = NULL,
      score = mean_score
    )
  })

  output$score_tekst <- renderText({
    res <- score_reaktiv()
    if (!res$gyldig) return(res$beskjed)
    paste0(round(res$score, 2), i18n$t(" av 5"))
  })


  output$resultat_tekst <- renderText({
    res <- score_reaktiv()
    if (!res$gyldig) return("")

    s <- res$score

    if (s >= 4.0) {
      i18n$t("Svarene dine viser få trekk som ligner de reguleringsvanskene man ser ved ADHD. Hverdagen virker stabil og forutsigbar.")
    } else if (s >= 3.2) {
      i18n$t("Mønsteret ditt ligger godt innenfor normal variasjon: noen styrker, litt friksjon, men ingenting som peker klart i én retning.")
    } else if (s >= 2.5) {
      i18n$t("Du rapporterer en del trekk som kan minne om ADHD, men dette kan like gjerne handle om personlighet, vaner eller livssituasjon.")
    } else {
      i18n$t("Du beskriver flere områder som ofte skaper vansker i ADHD. Dette er fortsatt ikke diagnostikk, men det kan være verdt en mer formell vurdering dersom dette skaper problemer i hverdagen.")
    }
  })
}

shinyApp(ui, server)
