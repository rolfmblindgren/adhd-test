library(shiny)
library(shinyjs)
library(tibble)
library(DBI)
library(RSQLite)
library(shiny.i18n)
library(bslib)
library(mirt)
library(grendelMeta)
library(grendelStripe)
options(bslib.cache = FALSE)

addResourcePath("docs", normalizePath("./docs"))

md_files <- "./content/"

custom_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  bg = "#FFFFFF",
  fg = "#000000",
  primary = "#0199F8",
  secondary = "#FF374B",
  base_font = "Verdana",
  heading_font = "Verdana"
)

mod1 <- readRDS("./models/mod1_grendel_1f_graded.rds")

scaled_score <- function(items, mod = mod1) {
  stopifnot(length(items) == 10)

  newdata <- as.data.frame(as.list(as.integer(items)))
  names(newdata) <- paste0("i", 1:10)

  theta <- fscores(
    mod,
    method = "EAP",
    response.pattern = newdata
  )[, 1]

  T_score <- 50 + 10 * theta

  c(
    theta = as.numeric(theta),
    T = as.numeric(T_score)
  )
}


translations_csvs_path <- "./content/translations/"

i18n <- Translator$new(translation_csvs_path = translations_csvs_path,
                       translation_csv_config=paste0(translations_csvs_path,"config.yml"))

i18n$set_translation_language("nb")
## print(i18n$get_languages())

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

map_browser_language <- function(browser_lang) {
  browser_lang <- tolower(browser_lang %||% "")

  if (startsWith(browser_lang, "nn")) return("nn")
  if (startsWith(browser_lang, "nb") || startsWith(browser_lang, "no")) return("nb")
  if (startsWith(browser_lang, "sv")) return("sv")
  if (startsWith(browser_lang, "da")) return("da")
  if (startsWith(browser_lang, "se")) return("se")
  if (startsWith(browser_lang, "fkv")) return("fkv")
  if (startsWith(browser_lang, "fr")) return("fr")
  if (startsWith(browser_lang, "es")) return("es")
  if (startsWith(browser_lang, "de")) return("de")
  if (startsWith(browser_lang, "en")) return("en")

  "nb"
}

default_app_url <- "https://shiny.grendel.no/adhd-test/"

score_thresholds <- list(
  stable = 45,
  typical = 40,
  friction = 35
)

score_colors <- list(
  low = "#d9534f",
  mid = "#f0ad4e",
  high = "#5cb85c"
)

ui <- fluidPage(
  social_meta("meta.yaml"),
  theme = custom_theme,
  usei18n(i18n),
  useShinyjs(),
  tags$head(
         tags$meta(
           name = "robots",
           content = "index,follow,max-image-preview:large,max-snippet:-1,max-video-preview:-1"
         ),
         tags$link(rel = "canonical", href = "https://shiny.grendel.no/adhd-test/"),
         tags$meta(name = "twitter:card", content = "summary_large_image"),
         tags$script(
           type = "application/ld+json",
           HTML(jsonlite::toJSON(
             list(
               "@context" = "https://schema.org",
               "@type" = "WebApplication",
               name = "Dette er ikke en ADHD-test",
               applicationCategory = "MedicalWebApplication",
               operatingSystem = "Any",
               url = "https://shiny.grendel.no/adhd-test/",
               inLanguage = c("nb", "nn", "sv", "da", "se", "fkv", "fr", "es", "de", "en"),
               author = list(
                 "@type" = "Person",
                 name = "Rolf Marvin Bøe Lindgren"
               ),
               publisher = list(
                 "@type" = "Organization",
                 name = "Grendel evidensbasert psykologi AS"
               ),
               description = paste(
                 "En kort selvrapportert app om fravær av oppmerksomhets- og",
                 "reguleringsvansker. Ikke diagnostisk, men laget som et",
                 "verktøy for refleksjon om egen fungering."
               ),
               educationalUse = "Selvrefleksjon",
               isAccessibleForFree = TRUE,
               disclaimer = "Ikke for diagnostisk bruk."
             ),
             auto_unbox = TRUE
           ))
         ),
         tags$script(src = "custom.js"),
         tags$script(HTML("
  Shiny.addCustomMessageHandler('update-title', function(msg) {
    document.title = msg;
  });
")),
tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
),
titlePanel(title=i18n$t("Dette er ikke en ADHD-test"),
           windowTitle="ADHD"),
sidebarLayout(
  sidebarPanel(



    selectizeInput(
      inputId = "selected_language",
      label = i18n$t("Skift språk"),
      choices = c("nb", "nn", "sv", "da", "se", "fkv", "fr", "es", "de", "en"),
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
                  h4(i18n$t("T-skår")),
                  uiOutput("t_meter"),
                  br(),
                  h4(i18n$t("Hva betyr T-skår?")),
                  uiOutput("tsk_md"),
                  h4(i18n$t("Forbehold")),
                  p(i18n$t("En klinisk vurdering innebærer utviklingshistorie, funksjon og faglig skjønn.")),
                  p(i18n$t("Mennesker kan ha lav struktur eller høy fart uten at det handler om ADHD."))
                ),
                tabPanel(
                  value = "om",
                  i18n$t("Om testen"),
                  br(),
                  htmlOutput("om_testen")
                ),
                tabPanel(
                  value = "doner",
                  i18n$t("Støtt arbeidet"),
                  br(),
                  h3(i18n$t("Støtt arbeidet")),
                  p(i18n$t("Du kan støtte videre utvikling av ADHD-appen med en frivillig donasjon via Stripe.")),
                  numericInput(
                    "donation_amount",
                    i18n$t("Beløp i NOK"),
                    value = 75,
                    min = 20,
                    step = 10
                  ),
                  actionButton("donate", i18n$t("Doner")),
                  p(
                    style = "margin-top: 10px; color: #666;",
                    i18n$t("Du sendes til en sikker Stripe-side for betaling.")
                  )
                )
                )
  )
)
)

server <- function(input, output, session) {
  detected_lang <- reactiveVal("nb")
  user_selected_lang <- reactiveVal(NULL)

  observeEvent(input$browser_lang, {
    initial_lang <- map_browser_language(input$browser_lang)
    detected_lang(initial_lang)

    if (is.null(user_selected_lang())) {
      updateSelectizeInput(session, "selected_language", selected = initial_lang)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$selected_language, {
    if (!is.null(input$selected_language) && nzchar(input$selected_language)) {
      user_selected_lang(input$selected_language)
    }
  }, ignoreInit = TRUE)

  t_score <- reactive({
    req(input$beregn)   # evt. hva som helst som trigger beregning

    items <- c(
      input$i1, input$i2, input$i3, input$i4, input$i5,
      input$i6, input$i7, input$i8, input$i9, input$i10
    )

    res <- scaled_score(items)   # funksjonen du allerede har
    as.numeric(res["T"])
  })

  output$tsk_md <- renderUI({
    current_lang <- lang()
    ## av din versjon
    file <- sprintf("%s%s.tskår.md", md_files, current_lang)

    if (!file.exists(file)) file <- sprintf("%snb.tskår.md", md_files)  # fallback

    includeMarkdown(file)
  })

  items_r <- reactive({

    current_lang <- lang()
    i18n$set_translation_language(current_lang)

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


  db_path <- paste0(Sys.getenv("ADHD_DB_PATH"),"/",Sys.getenv("ADHD_DB_NAME"))

  lang <- reactive({
    user_selected_lang() %||% detected_lang()
  })

  observeEvent(lang(), {
    i18n$set_translation_language(lang())
    update_lang(language = lang(), session = session)
    shinyjs::html("beregn", i18n$t("Beregn resultat"))
    shinyjs::html("donate", i18n$t("Doner"))
    session$sendCustomMessage("update-title", i18n$t("Dette er ikke en ADHD-test"))
  }, ignoreInit = TRUE)

  observe({
    query <- session$clientData$url_search %||% ""

    if (grepl("donation=success", query, fixed = TRUE)) {
      showNotification(i18n$t("Takk for støtten!"), type = "message", duration = 8)
    }

    if (grepl("donation=cancel", query, fixed = TRUE)) {
      showNotification(i18n$t("Donasjonen ble avbrutt."), type = "warning", duration = 6)
    }
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

  observeEvent(input$donate, {
    amount_nok <- input$donation_amount %||% 0

    if (!is.numeric(amount_nok) || is.na(amount_nok) || amount_nok < 20) {
      showNotification(i18n$t("Velg minst 20 kroner."), type = "error")
      return()
    }

    base_url <- grendelStripe::build_app_url(session, fallback = default_app_url)
    success_url <- paste0(base_url, "?donation=success")
    cancel_url <- paste0(base_url, "?donation=cancel")

    checkout_url <- tryCatch(
      grendelStripe::create_checkout_session(
        amount_nok = amount_nok,
        success_url = success_url,
        cancel_url = cancel_url,
        product_name = i18n$t("Støtt ADHD-appen"),
        product_description = i18n$t("Frivillig støtte til videre arbeid med ADHD-appen"),
        metadata = list(
          app = "ADHD",
          donation_amount_nok = format(amount_nok, trim = TRUE, scientific = FALSE),
          purpose = "donation"
        )
      ),
      error = function(e) {
        showNotification(
          paste(i18n$t("Noe gikk galt ved opprettelse av betalingen:"), conditionMessage(e)),
          type = "error",
          duration = 8
        )
        NULL
      }
    )

    if (!is.null(checkout_url)) {
      session$sendCustomMessage("redirect-to-url", list(url = checkout_url))
    }
  }, ignoreInit = TRUE)



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
    res <- round(scaled_score(svar_num),0)

    list(
      gyldig = TRUE,
      beskjed = NULL,
      score = res["T"]
    )
  })


  output$score_tekst <- renderText({
    res <- score_reaktiv()
    if (!res$gyldig) return(res$beskjed)
    paste0(round(res$score, 2))
  })


  observeEvent(input$selected_language, {

    output$om_testen <- renderText({

      lg <- i18n$get_translation_language()
      res <- switch (lg,
                     nb = "Testen er prøvd ut på over tre hundre brukere, og viser gode psykometriske egenskaper. Det betyr ikke at den kan fortelle deg at du helt sikkert ikke har ADHD, men den kan fortelle deg at det er gode grunner til at det er overveiende sannsynlig at eventuelle problemer du har, skyldes noe annet.</p><p>Funnene tyder på at testen først og fremst fanger ett hovedmønster i hvordan folk regulerer oppmerksomhet og hverdag. Samtidig ser dette mønsteret ut til å ha to sider: en mer indre side knyttet til å komme i gang, holde fokus og styre tankene, og en mer ytre side knyttet til organisering, avtaler og praktisk pålitelighet. Testen kan derfor forstås som et mål på hvor mye friksjon en person opplever i ulike deler av livet. Slike vansker kan merkes på jobb eller skole, hjemme, i relasjoner, i tidsbruk og i praktiske gjøremål. Lavere skårer tyder på større likhet med et ADHD-relevant mønster av reguleringsvansker, mens høyere skårer tyder på mindre likhet. For en mer utførlig gjennomgang, se <a href='/docs/no/adhd_psychometric_note.pdf' target='_blank' rel='noopener'>artikkelen</a>.</p><p>Oversettelsene er gjort maskinmessig og kontrollert der det er mulig. Finner du feil, ikke nøl med å si fra.",
                     nn = "Testen er prøvd ut på over tre hundre brukarar og viser gode psykometriske eigenskapar. Det betyr ikkje at han kan fortelje deg at du heilt sikkert ikkje har ADHD, men han kan vise at det finst gode grunnar til at det er overvegande sannsynleg at eventuelle vanskar du har, kjem av noko anna.</p><p>Funna tyder på at testen først og fremst fangar eitt hovudmønster i korleis folk regulerer merksemd og kvardag. Samstundes ser dette mønsteret ut til å ha to sider: ei meir indre side knytt til å kome i gang, halde fokus og styre tankane, og ei meir ytre side knytt til organisering, avtalar og praktisk pålitelegheit. Testen kan derfor forståast som eit mål på kor mykje friksjon ein person opplever i ulike delar av livet. Slike vanskar kan merkast på jobb eller skule, heime, i relasjonar, i tidsbruk og i praktiske gjeremål. Lågare skårar tyder på større likskap med eit ADHD-relevant mønster av reguleringsvanskar, medan høgare skårar tyder på mindre likskap. For ei meir utførleg gjennomgåing, sjå <a href='/docs/nn/adhd_psychometric_note.pdf' target='_blank' rel='noopener'>artikkelen</a>.</p><p>Omsetjingane er gjorde maskinelt og kontrollerte der det har vore mogleg. Finn du feil, må du gjerne seie frå.",
                     da = "Testen er afprøvet på mere end tre hundrede brugere og viser gode psykometriske egenskaber. Det betyder ikke, at den med sikkerhed kan fortælle dig, at du ikke har ADHD, men den kan pege på, at der er gode grunde til, at det overvejende sandsynligt er, at eventuelle problemer skyldes noget andet.</p><p>Resultaterne tyder på, at testen først og fremmest fanger ét hovedmønster i, hvordan folk regulerer opmærksomhed og hverdag. Samtidig ser dette mønster ud til at have to sider: en mere indre side knyttet til at komme i gang, holde fokus og styre tankerne, og en mere ydre side knyttet til organisering, aftaler og praktisk pålidelighed. Testen kan derfor forstås som et mål for, hvor meget friktion en person oplever i forskellige dele af livet. Sådanne vanskeligheder kan mærkes på arbejde eller uddannelse, derhjemme, i relationer, i tidsstyring og i praktiske opgaver. Lavere scorer tyder på større lighed med et ADHD-relevant mønster af reguleringsvanskeligheder, mens højere scorer tyder på mindre lighed. For en mere udførlig gennemgang, se <a href='/docs/da/adhd_psychometric_note.pdf' target='_blank' rel='noopener'>artiklen</a>.</p><p>Oversættelserne er lavet maskinelt og kontrolleret, hvor det har været muligt. Finder du fejl, er du meget velkommen til at sige til.",
                     sv = "Testen har prövats på över trehundra användare och uppvisar goda psykometriska egenskaper. Det innebär inte att den med säkerhet kan säga att du inte har ADHD, men den kan visa att det finns goda skäl att anta att eventuella svårigheter sannolikt beror på något annat.</p><p>Fynden tyder på att testet framför allt fångar ett huvudmönster i hur människor reglerar uppmärksamhet och vardag. Samtidigt verkar detta mönster ha två sidor: en mer inre sida knuten till att komma igång, hålla fokus och styra tankarna, och en mer yttre sida knuten till organisering, överenskommelser och praktisk pålitlighet. Testet kan därför förstås som ett mått på hur mycket friktion en person upplever i olika delar av livet. Sådana svårigheter kan märkas i arbete eller studier, hemma, i relationer, i tidsanvändning och i praktiska uppgifter. Lägre poäng tyder på större likhet med ett ADHD-relevant mönster av regleringssvårigheter, medan högre poäng tyder på mindre likhet. För en mer utförlig genomgång, se <a href='/docs/sv/adhd_psychometric_note.pdf' target='_blank' rel='noopener'>artikeln</a>.</p><p>Översättningarna är gjorda maskinellt och har kontrollerats där det varit möjligt. Om du hittar fel, tveka inte att säga till.",
                     en = "The test has been tried out on more than three hundred users and shows good psychometric properties. This does not mean it can tell you with certainty that you do not have ADHD, but it <em>can</em> indicate that there are good reasons to believe that any difficulties you experience are likely due to something else.</p><p>The findings suggest that the test mainly captures one main pattern in how people regulate attention and everyday life. At the same time, this pattern seems to have two sides: a more internal side tied to getting started, staying focused, and keeping thoughts on track, and a more outward side tied to organization, appointments, and practical reliability. The test can therefore be understood as a measure of how much friction a person experiences across different parts of life. Difficulties like these can show up at work or in school, at home, in relationships, in time management, and in practical day-to-day tasks. Lower scores suggest greater similarity to an ADHD-relevant pattern of regulation difficulties, while higher scores suggest less similarity. For a fuller discussion, see <a href='/docs/en/adhd_psychometric_note.pdf' target='_blank' rel='noopener'>the article</a>.</p><p>Translations are machine-generated and checked when possible. If you find errors, please let me know.",
                     es = "La prueba se ha probado con más de trescientas personas usuarias y muestra buenas propiedades psicométricas. Eso no significa que pueda decirte con certeza que no tienes TDAH, pero sí puede indicar que hay buenas razones para pensar que las dificultades que experimentas probablemente se deban a otra cosa.</p><p>Los hallazgos sugieren que la prueba capta ante todo un patrón principal en cómo las personas regulan la atención y la vida cotidiana. Al mismo tiempo, ese patrón parece tener dos lados: uno más interno, relacionado con ponerse en marcha, mantener el foco y encauzar los pensamientos, y otro más externo, relacionado con la organización, las citas y la fiabilidad práctica. Por eso, la prueba puede entenderse como una medida de cuánta fricción experimenta una persona en distintas partes de la vida. Dificultades como estas pueden notarse en el trabajo o en los estudios, en casa, en las relaciones, en la gestión del tiempo y en las tareas prácticas de cada día. Las puntuaciones más bajas sugieren una mayor semejanza con un patrón de dificultades de regulación relevante para el TDAH, mientras que las más altas sugieren una menor semejanza. Para una explicación más detallada, consulta <a href='/docs/es/adhd_psychometric_note.pdf' target='_blank' rel='noopener'>el artículo</a>.</p><p>Las traducciones se han generado automáticamente y se han revisado cuando ha sido posible. Si encuentras errores, dímelo.",
                     se = "Táhppa lea geavahuvvon máŋga olbmo mielde ja čájeha buori psykomehtera iešvuođaid. Dat ii mearkkaša ahte sáhttá čilgejuhttit du addiktii ahte dus ii leat ADHD, muhto sáhttá addit buori vuođđosaččaid árvvoštallamat ahte juoga eará sáhttá leat váikkuhan du birgejumi.</p><p>Bohtosat čájehit ahte geahččaleapmi vuosttažettiin gávdná ovtta váldominstera das mo olbmot regulerejit fuomášumi ja árgga. Seammás orru ahte dás leat guokte beali: siskkáldas bealli mii laktása álggahahttimii, fokusii ja jurdagiid stivremii, ja olgguldas bealli mii laktása organiseremii, soahpamušaide ja praktihkalaš luohttevašvuhtii. Dán geahččaleami sáhttá danin ipmirdit mihttun das man ollu frikšuvdna olmmoš vásiha iešguđet bealain eallimis. Dát sáhttet oidnot barggus dahje skuvllas, ruovttus, oktavuođain, áigegeavaheamis ja praktihkalaš bargguin. Vuolit skorat čujuhit stuorát sullasašvuođa ADHD-relevánta regulerenváttisvuođaid minstarii, go bajit skorat čujuhit unnit sullasašvuođa. Dárkilut ovdanbuktima várás geahča <a href='/docs/se/adhd_psychometric_note.pdf' target='_blank' rel='noopener'>artihkkala</a>.</p><p>Jorgalusat leat mearriduvvon mearrihkka ja ovdáneaddji geavaheami bokte. Jus gávnnat meattáhusaid, de leat buorre jus dieđát.",
                     fkv = "Testi on kokkeiltu yli kolmensadalta käyttäjältä ja se näyttää hyvät psykometriset ominaisuudet. Se ei tarkoita ette testi vois varmisttaa siele ette siele ei ole ADHD:ta, mutta se voi antaat hyvät syyt ussoa ette jos sulla oon ongelmia, niissä oon todenmukhaisemmin jotaki muuta taustala.</p><p>Fyndan näyttävät ette testi nappaa ennen kaikkea yhden päämallin siinä, kuinka ihmiset säätelevät huomhiota ja arkea. Samala tässä mallissa näyttää olevan kaksi puolta: sisempi puoli liittyy alottamisseen, fokukseen ja ajatusten hallinthaan, ja ulompi puoli liittyy organisointiin, sovittuihin aikoihin ja käytännölliseen luotettavuutheeen. Testiä voi siksi ymmärtäät mittana siitä, kuinka paljon kitkaa ihminen kokee eri osissa elämää. Tämmöiset vaikeuvet voijaan näkyä työssä eli koulussa, kotona, suhteissa, ajankäytössä ja käytännöllisissä tehtävissä. Matalaammat skoorit viittaavat suuremphaan samankaltaisuutheen ADHD:heen liittyvän säätelyvaikeuksien mallin kans, kun taas korkeammmat skoorit viittaavat pienemphään samankaltaisuutheen. Jos haluat tarkemman läpikäynnin, katso <a href='/docs/fkv/adhd_psychometric_note.pdf' target='_blank' rel='noopener'>artikeli</a>.</p><p>Käännökset oon tehty koneellisesti ja tarkistettu missä mahdollista. Jos löyät virheitä, ota ihmeessä yhteyttä.",

                     de = "Der Test wurde mit mehr als dreihundert Nutzerinnen und Nutzern erprobt und zeigt gute psychometrische Eigenschaften. Das bedeutet nicht, dass er Ihnen mit Sicherheit sagen kann, dass Sie kein ADHS haben, aber er kann gute Gründe dafür liefern, dass eventuelle Schwierigkeiten, die Sie erleben, wahrscheinlich auf etwas anderes zurückzuführen sind.</p><p>Die Befunde sprechen dafür, dass der Test vor allem ein Hauptmuster darin erfasst, wie Menschen Aufmerksamkeit und Alltag regulieren. Zugleich scheint dieses Muster zwei Seiten zu haben: eine eher innere Seite, die mit dem Beginnen, dem Aufmerksamkeitsfokus und der Steuerung der Gedanken zusammenhängt, und eine eher äußere Seite, die mit Organisation, Terminen und praktischer Zuverlässigkeit zusammenhängt. Der Test kann daher als Maß dafür verstanden werden, wie viel Reibung eine Person in verschiedenen Bereichen des Lebens erlebt. Solche Schwierigkeiten können bei der Arbeit oder in der Schule, zu Hause, in Beziehungen, im Umgang mit Zeit und bei praktischen Aufgaben spürbar werden. Niedrigere Werte deuten auf eine größere Ähnlichkeit mit einem ADHS-relevanten Muster von Regulationsschwierigkeiten hin, höhere Werte auf eine geringere. Für eine ausführlichere Darstellung siehe <a href='/docs/de/adhd_psychometric_note.pdf' target='_blank' rel='noopener'>den Artikel</a>.</p><p>Die Übersetzungen wurden maschinell erstellt und dort überprüft, wo es möglich war. Wenn Sie Fehler finden, zögern Sie bitte nicht, mich darauf hinzuweisen.",

                     fr = "Le test a été essayé auprès de plus de trois cents utilisateurs et présente de bonnes propriétés psychométriques. Cela ne signifie pas qu’il puisse affirmer avec certitude que vous n’avez pas de TDAH, mais il peut indiquer qu’il existe de bonnes raisons de penser que les difficultés que vous rencontrez sont probablement dues à autre chose.</p><p>Les résultats suggèrent que le test repère avant tout un grand schéma commun dans la manière dont les personnes régulent leur attention et leur vie quotidienne. En même temps, ce schéma semble avoir deux versants : un versant plus interne, lié au démarrage, au maintien de l’attention et à la maîtrise des pensées, et un versant plus externe, lié à l’organisation, aux rendez-vous et à la fiabilité pratique. Le test peut donc être compris comme une mesure du degré de friction qu’une personne éprouve dans différentes parties de la vie. Ces difficultés peuvent se faire sentir au travail ou à l’école, à la maison, dans les relations, dans la gestion du temps et dans les tâches pratiques du quotidien. Des scores plus faibles indiquent une plus grande ressemblance avec un profil de difficultés de régulation pertinent pour le TDAH, tandis que des scores plus élevés indiquent une moindre ressemblance. Pour une présentation plus détaillée, voir <a href='/docs/fr/adhd_psychometric_note.pdf' target='_blank' rel='noopener'>l’article</a>.</p><p>Les traductions ont été générées automatiquement et vérifiées lorsque cela était possible. Si vous repérez des erreurs, n’hésitez pas à me le signaler.",

                                        # fallback hvis språk mangler
                     "Mensura fallax est, sed mensurare oportet."
                     )

      app_base <- sub("/?$", "/", grendelStripe::build_app_url(session, fallback = default_app_url))
      res <- gsub("href='/docs/", paste0("href='", app_base, "docs/"), res, fixed = TRUE)

      paste0("<p>", res, "</p><p><a href='mailto:rolf@grendel.no?subject=ADHD-testen'>© 2025 Grendel AS</a></p>")
    })
  })

  output$resultat_tekst <- renderText({

    res <- score_reaktiv()
    if (!res$gyldig) return("")

    s <- res$score

    if (s >= score_thresholds$stable) {
      paste0(i18n$t("Svarene dine viser få trekk som ligner de reguleringsvanskene man ser ved ADHD."),
             " ",
             i18n$t("Hverdagen virker stabil og forutsigbar."))
    } else if (s >= score_thresholds$typical) {
      i18n$t("Mønsteret ditt ligger godt innenfor normal variasjon: noen styrker, litt friksjon, men ingenting som peker klart i én retning.")
    } else if (s >= score_thresholds$friction) {
      i18n$t("Du rapporterer en del trekk som kan minne om ADHD, men dette kan like gjerne handle om personlighet, vaner eller livssituasjon.")
    } else {
      i18n$t("Du beskriver flere områder som ofte skaper vansker i ADHD. Dette er fortsatt ikke diagnostikk, men det kan være verdt en mer formell vurdering dersom dette skaper problemer i hverdagen.")
    }
  })

  output$t_meter <- renderUI({
    sc <- score_reaktiv()
    T  <- as.numeric(sc[["score"]])

    lo <- 20; hi <- 80
    pct <- max(0, min(100, (T - lo) / (hi - lo) * 100))

    ## velg farge basert på T
    bar_col <- if (T < score_thresholds$friction) {
                 score_colors$low
               } else if (T < score_thresholds$stable) {
                 score_colors$mid
               } else {
                 score_colors$high
               }

    tagList(
      div(style="font-size: 32px; font-weight: 700; margin: 6px 0;",
          paste0("T = ", round(T))),

      div(style="height:14px; background:#eee; border-radius:999px; overflow:hidden;",
          div(style=paste0(
                "height:100%; width:", pct, "%; background:", bar_col, ";"
              ))
          ),

      div(style="position:relative; height:16px; margin-top:6px; font-size:12px; color:#444;",
          span("20", style="position:absolute; left:0%; transform:translateX(-50%);"),
          span("30", style="position:absolute; left:16.7%; transform:translateX(-50%);"),
          span("40", style="position:absolute; left:33.3%; transform:translateX(-50%);"),
          span("50", style="position:absolute; left:50%; transform:translateX(-50%);"),
          span("60", style="position:absolute; left:66.7%; transform:translateX(-50%);"),
          span("70", style="position:absolute; left:83.3%; transform:translateX(-50%);"),
          span("80", style="position:absolute; left:100%; transform:translateX(-50%);")
          )
    )
  })

}

shinyApp(ui=ui, server=server)
