(ns vending-machine.core
  (:gen-class))

(use 'clojure.java.io)

(defn get-lines [fname]
  (with-open [r (reader fname)]
    (doall (line-seq r))))
;Lista de productos
(def productos
 '((agua (15) (1) (10)) (refresco (15) (2) (10)) (jugo (10) (3) (10))) )
;Lista de ventas
(def ventas
 '(((10 10 10) (3)) ((2 2 1 10) (2))) ) 
;Lista de monedas
(def monedas 
  '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2) (5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5) (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)))
;Lista de ganancias
(def ganancias
  '((1)(2)(5)(10)(20)(50)))

;Funcion auxiliar para actualizar una lista segun su posición
(defn list-update-in-recur [l i x]
  (loop [new-data [] old-list l]
    (if (seq old-list)
      (if (= (count new-data) i)
        (recur (conj new-data x) (rest old-list))
        (recur (conj new-data (first old-list)) (rest old-list)))
      (apply list new-data))))

;Funcion auxiliar para hallar toodos los divisores de un numero
(defn divisors
  [n]
  (filter (comp zero? (partial rem n)) (range 1 n)))
;Funciion auxiliar para hallar el codigo de una venta
(defn get-codigo [ventas]
  (ffirst (map second ventas)))

;;Funcion que da el cambio de los productos en monedas 1, 2, 5 o 10 pesos
(defn get-cambio [dinero]
  ;Asigna el cociente de la división a cantidad y la moneda 
  (let [cantidad (quot dinero (apply max (divisors dinero))) moneda (apply max (divisors dinero))]
    (cond
      ;Si la moneda es una de las que se tienen en la lista, se da una lista 
      (or (= moneda 1) (= moneda 2) (= moneda 5) (= moneda 10)) (repeat cantidad moneda)
      ;Si es una de las monedas de la lista, se usa otro divisor.
      :else (repeat (quot dinero (first (rest (divisors 18)))) (first (rest (divisors dinero)))))))

(defn update-inventario [lista _productos]
  (cond
    ;Si el vendido producto es igual al producto del inventario, se actualiza la lista de inventario restando el producto vendido
    (= (ffirst lista) (ffirst _productos)) (list-update-in-recur productos (- (first (second (rest (first _productos)))) 1) (list-update-in-recur (first productos) 3 (list (- (first (second (rest (rest (first productos))))) 1))))
    :else (update-inventario lista (rest _productos))
    )
  ) 

;Funcion para actualizar las monedas segun la denominacion
(defn update-monedas [lista]
  (cond
    (= (first(second (first lista))) 1) (list-update-in-recur monedas 0 (repeat (- (count (first monedas)) (count lista)) 1))
    (= (first (second (first lista))) 2) (list-update-in-recur monedas 1 (repeat (- (count (second monedas)) (count lista)) 2))
    (= (first (second (first lista))) 5) (list-update-in-recur monedas 2 (repeat (- (count (second (rest monedas))) (count lista)) 5))
    (= (first (second (first lista))) 10) (list-update-in-recur monedas 3 (repeat (- (count (second (rest (rest monedas)))) (count lista)) 10))
    :else "Maquina sin cambio")
  )

;; (defn update-ganacias [lista ganancias]
  
  
;;     (if (= (first lista) (ffirst ganancias)) ( )
;;      else-expr)
    
  
;; )

  (defn recursive-a [lst]
    (if (seq lst)
      (if (= (first lst) "a")
        (+ 1 (recursive-a (pop lst))) (+ 0 (recursive-a (pop lst))))
      0))

(defn register-sell [productos ventas]
  (cond
    ;Si la cantidad de productos es cero, la maquina regresa sin productos
    (= 0 (ffirst (rest (rest (rest (first productos)))))) "Sin productos"
    ;Si no se tienen suficientes monedas en la sub lista, regresa fondos insuficientes. 
    (< (- (apply + (ffirst ventas)) (ffirst (rest (first productos)))) 0) "Fondos insuficientes"
    ;Si la operacion es exitosa, se regresa una lista con el producto y el cambio.
    :else (concat (cons (list (ffirst productos) (get-cambio(- (apply + (ffirst ventas)) (first (fnext (first productos)))))) '() )));(update-inventario productos (rest ventas)))
  )

(defn vending-machine [productos ventas]
           (cond
             ;Si cualquiera de las dos listas esta vacia, regresa una lista un nulo
           (and (empty? ventas) (empty? productos)) nil
             ;Realiza el registro de la venta si el codigo de la venta y de los productos son iguales
           (= (get-codigo ventas) (ffirst (rest (rest (first productos))))) (register-sell productos ventas) ;(update-inventario (ffirst productos) productos))
             ;Si no se encuentra el producto en la lista, se realiza la llamada recursiva. 
           :else (vending-machine (rest productos) ventas)))


(defn main [& args]
  
  )
;Maquina 
