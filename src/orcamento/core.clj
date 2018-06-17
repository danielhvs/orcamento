(ns orcamento.core
  (:require 
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.pprint :refer :all]
   [incanter.core :refer :all]
   [incanter.charts :refer :all]
   [incanter.stats :refer :all]
))

(defn dec-prazo [item]
  (assoc item :prazo (dec (:prazo item))))

(defn proximos [gastos]
  (filter #(> (:prazo %) 0) (map dec-prazo gastos)))

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

(defn remove-zero-esquerda-string [numero-string]
  (if (= \0 (first numero-string))
    (subs numero-string 1) 
    numero-string))

(defn parse-prazo [gasto]
  (let [string-prazo (re-find #"\d\d\/\d\d" (:descricao gasto))]
    (if string-prazo
      (inc
       (- (read-string (remove-zero-esquerda-string (subs string-prazo 3 5))) 
          (read-string (remove-zero-esquerda-string (subs string-prazo 0 2)))))
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

(defn remove-debito-em-conta [gastos total]
  (let [debito-em-conta (filter #(re-matches #"PGTO DEBITO CONTA 1453.*" (:descricao %)) gastos)]
    (if (empty? debito-em-conta)
      total
      (+ (- (:valor
             (first (filter #(re-matches #"PGTO DEBITO CONTA 1453.*" (:descricao %)) gastos))))
         total))))

(defn soma [gastos]
  (remove-debito-em-conta gastos
   (valor-mensal gastos)))

(defn max-prazo [gastos]
  (apply max (map :prazo gastos)))

(defn todas-faturas [gastos]
  (take (max-prazo gastos) (iterate proximos gastos)))

(defn nomes-dos-arquivos [diretorio]
  (let [files (file-seq (clojure.java.io/file diretorio))]
    (->> files (filter #(.isFile %)) (map #(.getName %)) )))

(defn calcula-gastos [gastos]
  {:projecao (map soma (todas-faturas gastos))
   :total (reduce + (map soma (todas-faturas gastos)))})

(defn pad [n coll val]
  (take n (concat coll (repeat val))))

(defn pad-com-zeros [mapas]
  (let [maior-tamanho (apply max (map #(count (:projecao %)) mapas))]
    (map #(assoc % :projecao (pad maior-tamanho (:projecao %) 0)) 
         mapas)))

(defn junta [m1 m2] 
  {:projecao (map #(+ %1 %2) (:projecao m1) (:projecao m2)) 
   :total (+ (:total m1) (:total m2))})

(defn junta-todos [mapa-todo mes-ano]
  (let [mapas-a-juntar
        (filter #(clojure.string/includes? (:nome %) mes-ano) mapa-todo)]
    (reduce junta (pad-com-zeros mapas-a-juntar))))
; fazer pad aqui

(defn -main []
  (let [nomes-arquivos (nomes-dos-arquivos "resources") 
        todos-gastos (map parse-gastos nomes-arquivos) 
        resultado (map #(assoc (calcula-gastos %1) :nome %2) todos-gastos nomes-arquivos)]
    (junta-todos resultado "Jun_18")))

;; para testes
(def nomes-arquivos (nomes-dos-arquivos "resources"))
(def todos-gastos (map parse-gastos nomes-arquivos))
(def resultado (map #(assoc (calcula-gastos %1) :nome %2) todos-gastos nomes-arquivos))
(view (histogram (sample-normal 1000)))
