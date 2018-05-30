(ns orcamento.core
  (:require [clojure.edn :as edn])
)

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
  (-> (str "resources/" arquivo)
      (slurp)
      (edn/read-string)))

(defn -main []
  (let [lista [{:prazo 1 :valor 32} {:prazo 7 :valor 8}]]
    (do
      (println (valor-mensal lista))
      (println (valor-mensal (proximos lista)))
      (println lista)
      (println (proximos lista)))))
