(ns culture.facts
  "Country-level regional-culture catalog for Angola (AGO) -- national
  dishes, protected products, beverages, crafts, festivals and heritage
  sites, per ADR-2607171400 addendum 2 (cloud-itonami-municipality-
  culture-catalog Wave 1, in com-junkawasaki/root). Sibling namespace to
  `marketentry.facts` / `statute.facts` (ADR-2607141700); city-level
  counterparts live in the cloud-itonami-municipality-* repos.

  Catalog is keyed by UPPERCASE ISO3 (mirrors `statute.facts`); entries
  carry no :culture/municipality (that attribute is city-level only).

  Every entry cites a source URL that was actually fetched and read on
  :culture/retrieved-at -- never fabricated. Summaries state only what the
  cited source confirms. An item not in this table has NO spec-basis, full
  stop; extend `catalog`, do not invent an id/url.")

(def catalog
  "iso3 -> vector of culture entries."
  {"AGO"
   [{:culture/id "ago.dish.moamba-de-galinha"
     :culture/name "Moambe chicken"
     :culture/name-local "Moamba de galinha"
     :culture/country "AGO"
     :culture/kind :dish
     :culture/summary "Savory chicken dish with palm butter, regarded as a national dish of Angola (as moamba de galinha) and also of the Democratic Republic of the Congo and the Republic of the Congo."
     :culture/url "https://en.wikipedia.org/wiki/Moambe_chicken"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "ago.dish.funge"
     :culture/name "Funge"
     :culture/country "AGO"
     :culture/kind :dish
     :culture/summary "Swallow made of cassava flour whisked into boiling water, a traditional staple in Angolan cuisine."
     :culture/url "https://en.wikipedia.org/wiki/Funge"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "ago.dish.calulu"
     :culture/name "Calulu"
     :culture/country "AGO"
     :culture/kind :dish
     :culture/summary "Stew of fish or meat with vegetables such as okra and tomatoes cooked in palm oil, typical of Angola and also significant in São Tomé and Príncipe."
     :culture/url "https://en.wikipedia.org/wiki/Calulu"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "ago.dish.cocada-amarela"
     :culture/name "Cocada amarela"
     :culture/country "AGO"
     :culture/kind :dish
     :culture/summary "Traditional Angolan dessert made from eggs and coconut, with a yellow hue from its high egg content."
     :culture/url "https://en.wikipedia.org/wiki/Cocada_amarela"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "ago.beverage.cuca"
     :culture/name "Cuca"
     :culture/country "AGO"
     :culture/kind :beverage
     :culture/summary "Angolan pale lager brand established in 1947, manufactured in Luanda and Benguela by Companhia União de Cervejas de Angola."
     :culture/url "https://en.wikipedia.org/wiki/Cuca_(beer)"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "ago.heritage.mbanza-kongo"
     :culture/name "M'banza-Kongo"
     :culture/country "AGO"
     :culture/kind :heritage
     :culture/summary "Capital of Zaire Province in northwestern Angola, inscribed as a UNESCO World Heritage Site in 2017 for the vestiges of the capital of the former Kingdom of Kongo."
     :culture/url "https://en.wikipedia.org/wiki/M%27banza-Kongo"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}]})

(defn spec-basis [iso3] (get catalog iso3))

(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s)
         missing (remove catalog iso3s)]
     {:requested (count iso3s)
      :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note (str "cloud-itonami-iso3166-ago culture catalog "
                 "(ADR-2607171400 addendum 2, Wave 1): " (count (get catalog "AGO"))
                 " AGO entries, each with a fetched-and-read citation. "
                 "Extend `culture.facts/catalog`, never fabricate an id/url.")})))

(defn by-kind [iso3 kind]
  (filterv #(= (:culture/kind %) kind) (spec-basis iso3)))
