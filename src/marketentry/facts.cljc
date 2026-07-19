(ns marketentry.facts "Angola market-entry catalog.")
(def catalog
  {"AGO" {:name "Angola"
          :owner-authority "SNCP / e-procurement"
          :legal-basis "Lei dos Contratos Públicos (Lei n.º 41/20, de 23 de dezembro)"
          :national-spec "e-procurement + NIF/Registo Comercial"
          :provenance "https://www.minfin.gov.ao/"
          :required-evidence ["NIF/Registo Comercial record" "e-procurement registration record" "Commercial registry extract" "Authorized-representative record"]
          :rep-owner-authority "contracting authorities / SNCP"
          :rep-legal-basis "Angolan legal entity registration typically required for public awards"
          :rep-provenance "https://www.minfin.gov.ao/"
          :corporate-number-owner-authority "MINFIN / Guiché Único"
          :corporate-number-legal-basis "NIF / commercial registration"
          :corporate-number-provenance "https://www.minfin.gov.ao/"}
   "USA" {:name "United States" :owner-authority "GSA/SAM.gov" :legal-basis "FAR" :national-spec "SAM.gov" :provenance "https://sam.gov/"
          :required-evidence ["EIN record" "SAM.gov registration record" "State business registration record" "SAM UEI verification record"]}
   "ZAF" {:name "South Africa" :owner-authority "CSD/eTender" :legal-basis "PFMA" :national-spec "CSD" :provenance "https://www.etenders.gov.za/"
          :required-evidence ["CSD registration" "CIPC record" "SARS tax clearance" "Authorized-representative record"]}
   "BRA" {:name "Brazil" :owner-authority "Compras.gov.br" :legal-basis "Lei 14.133/2021" :national-spec "Compras.gov.br" :provenance "https://www.gov.br/compras/"
          :required-evidence ["CNPJ record" "Compras.gov.br registration" "SICAF record" "Authorized-representative record"]}})

(defn spec-basis [iso3] (get catalog iso3))
(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s) missing (remove catalog iso3s)]
     {:requested (count iso3s) :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note "R0 catalog seed"})))
(defn required-evidence-satisfied? [iso3 submitted]
  (when-let [{:keys [required-evidence]} (spec-basis iso3)]
    (= (count required-evidence) (count (filter (set submitted) required-evidence)))))
(defn evidence-checklist [iso3] (:required-evidence (spec-basis iso3) []))
(defn rep-spec-basis [iso3]
  (when-let [sb (spec-basis iso3)]
    (when (:rep-owner-authority sb)
      (select-keys sb [:rep-owner-authority :rep-legal-basis :rep-provenance]))))
(defn corporate-number-spec-basis [iso3]
  (when-let [sb (spec-basis iso3)]
    (when (:corporate-number-owner-authority sb)
      (select-keys sb [:corporate-number-owner-authority :corporate-number-legal-basis :corporate-number-provenance]))))
