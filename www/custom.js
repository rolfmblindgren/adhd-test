window.Shiny = window.Shiny || {};

window.Shiny.renderFlagOption = function(item) {
  const lang = item.value;

  const flags = {
    nb:  "norway.svg",
    nn:  "norway.svg",
    sv: "sv.svg",
    da: "da.svg",
    se:  "sami.svg",
    // smh: "smh.svg",
    fkv: "kven.svg",
    fr:  "france.svg",
    de:  "germany.svg",
    en:  "uk.svg"
  };

  const names = {
    nb:  "Bokmål",
    nn:  "Nynorsk",
    sv: "Svenska",
    da: "Dansk",
    se:  "Davvisámegiella",
//    smh: "Julevsámegiella",
    fkv: "Kainuun kieli",
    fr:  "Français",
    de:  "Deutsch",
    en:  "English"
  };

  const flag  = flags[lang] || "";
  const label = names[lang] || lang;

  return `
    <div style="display:flex; align-items:center; gap:6px;">
      ${flag ? `<img src="${flag}" height="15" style="border:1px solid #ccc;">` : ""}
      <span>${label}</span>
    </div>
  `;
};

function sendBrowserLanguageToShiny() {
  if (!window.Shiny || typeof window.Shiny.setInputValue !== "function") {
    return false;
  }

  const languages = navigator.languages || [navigator.language || ""];
  const browserLang = (languages[0] || "").toLowerCase();

  window.Shiny.setInputValue("browser_lang", browserLang, { priority: "event" });
  window.Shiny.setInputValue("browser_langs", languages.join(", "), { priority: "event" });
  return true;
}

function scheduleBrowserLanguageRetry(maxAttempts = 20, delayMs = 250) {
  let attempts = 0;

  function trySend() {
    attempts += 1;

    if (sendBrowserLanguageToShiny() || attempts >= maxAttempts) {
      return;
    }

    window.setTimeout(trySend, delayMs);
  }

  trySend();
}

document.addEventListener("DOMContentLoaded", function() {
  scheduleBrowserLanguageRetry();
});

document.addEventListener("shiny:connected", function() {
  scheduleBrowserLanguageRetry();
});

if (window.Shiny && typeof window.Shiny.addCustomMessageHandler === "function") {
  window.Shiny.addCustomMessageHandler("redirect-to-url", function(message) {
    if (message && message.url) {
      window.location.href = message.url;
    }
  });
}
