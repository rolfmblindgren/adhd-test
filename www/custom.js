Shiny.renderFlagOption = function(item) {
  const lang = item.value;

  const flags = {
    nb:  "norway.svg",
    nn:  "norway.svg",
    se:  "sami.svg",
    fkv: "kven.svg",
    fr:  "france.svg",
    de:  "germany.svg",
    en:  "uk.svg"
  };

  const names = {
    nb:  "Bokmål",
    nn:  "Nynorsk",
    se:  "Davvisámegiella",
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
