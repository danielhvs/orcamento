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
  (read-string
   (-> texto
       (str/replace "." "" )
       (str/replace "," "." ))))

(defn parse-prazo [gasto]
  (let [string-prazo (re-find #"\d\d\/\d\d" (:descricao gasto))]
    (if string-prazo
      (inc
       (- (read-string (subs string-prazo 3 5)) 
          (read-string (subs string-prazo 0 2))))
      1)))

(defn parse-gasto [texto]
  (let [gasto
        {:data (subs texto 0 10)
         :descricao (str/trim (subs texto 10 50))
         :valor (formata-dinheiro (str/trim (subs texto 50 69)))
         :valor-dolar (formata-dinheiro (str/trim (subs texto 69 81)))
         }]
    (assoc gasto :prazo (parse-prazo gasto))))

(defn parse-gastos [arquivo]
  (let [gastos (map parse-gasto (filtra-gastos arquivo))]
     gastos))

(defn soma [gastos]
  (+ (- (:valor
         (first (filter #(re-matches #"PGTO DEBITO CONTA 1453.*" (:descricao %)) gastos))))
   (reduce + (map :valor gastos))))

(defn projeta-proximos [gastos]
  (proximos gastos))

(defn -main []
  (let [lista [{:prazo 1 :valor 32} {:prazo 7 :valor 8}]
        gastos (parse-gastos "OUROCARD_PLATINUM_ESTILO_VISA-Dez_17.txt")]

    (do
      (pprint (soma gastos)))))
