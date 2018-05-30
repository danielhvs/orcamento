(ns orcamento.core
  (:require 
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.pprint :refer :all]
))

(defn dec-prazo [item]
  (assoc item :prazo (dec (:prazo item))))

(defn proximos [lista]
  (filter #(> (:prazo %) 0) (map dec-prazo lista)))

(defn valor-mensal [lista]
  (reduce + (map :valor lista)))


(defn salva [arquivo conteudo]
  (-> (str "resources/" arquivo)
      (spit conteudo)))

(defn carrega [arquivo]
  (with-open [r (io/reader (str "resources/" arquivo))]
    (doall (line-seq r))))

(defn filtra-gastos [arquivo]
  (let [gastos (carrega arquivo)]
    (filter #(re-matches #"\d\d\.\d\d\.\d\d\d\d.*" %) gastos)))

(defn formata-dinheiro [texto]
  (-> texto
      (str/replace "." "" )
      (str/replace "," "." )))

(defn parse-gasto [texto]
  {:data (subs texto 0 10)
   :descricao (str/trim (subs texto 10 49))
   :valor (formata-dinheiro (str/trim (subs texto 49 69)))
   :valor-dolar (formata-dinheiro (str/trim (subs texto 69 81)))
   }
)

(defn -main []
  (let [lista [{:prazo 1 :valor 32} {:prazo 7 :valor 8}]]
    (do
      (pprint (carrega "OUROCARD_PLATINUM_ESTILO_VISA-Dez_17.txt"))
      (pprint (filtra-gastos "OUROCARD_PLATINUM_ESTILO_VISA-Dez_17.txt"))
      (pprint (map parse-gasto (filtra-gastos "OUROCARD_PLATINUM_ESTILO_VISA-Dez_17.txt")))
      (pprint (proximos lista)))))
