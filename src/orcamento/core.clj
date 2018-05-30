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

;; FIXME: regex correta, remover . e considerar o negativo
(defn texto->gastos [arquivo]
  (let [texto (filtra-gastos arquivo)]
    (map #(re-find #"\d\d,\d\d" %) texto)))

(defn formata-em-real [texto]
  (if (not (nil? texto))
    (-> texto
        (str/replace "." "" )
        (str/replace "," "." ))))

(defn -main []
  (let [lista [{:prazo 1 :valor 32} {:prazo 7 :valor 8}]]
    (do
      (pprint (filtra-gastos "OUROCARD_PLATINUM_ESTILO_VISA-Dez_17.txt"))
      (pprint (texto->gastos "OUROCARD_PLATINUM_ESTILO_VISA-Dez_17.txt"))
      (pprint (map formata-em-real (texto->gastos "OUROCARD_PLATINUM_ESTILO_VISA-Dez_17.txt")))
      (pprint (proximos lista)))))
