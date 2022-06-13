(ns vending-machine.core
  (:gen-class))


;Lista de productos
(def productos1
'((agua (15) (1) (10)) (agua1lt (20) (2) (10)) (refresco (15) (3) (10)) (refrescoCola (18) (4) (10)) (jugoManzana (8) (5) (10)))) 

(def productos2
  '((doritos (15) (1) (10)) (sabritas (20) (2) (10)) (churrumais (15) (3) (10)) (sabritones (18) (4) (10)) (cheetos (8) (5) (10))))

(def productos3
  '((galletasoreo (12) (1) (10)) (arandanos (10) (2) (10)) (gomitas (12) (3) (10)) (cachuates (18) (4) (10)) (donas (8) (5) (10))))

;Lista de ventas
(def ventas
 '(
   ((10 10) (1))
   ((2 2 1 10 10) (3)) 
   ((10 10) (4)) 
   ((5 5 5 5 10) (5))  
   ) )


;Lista de monedas
(def monedas 
  '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2) (5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5) (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)))
;Lista de ganancias
(def ganancias
  '())

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

(defn update-inventario [lista _productos productos] 
  (cond
    ;Si el vendido producto es igual al producto del inventario, se actualiza la lista de inventario restando el producto vendido
    (= (ffirst lista) (ffirst _productos)) (list-update-in-recur productos (- (first (second (rest (first _productos)))) 1) (list-update-in-recur (first productos) 3  (list (- (first (second (rest (rest (first _productos))))) 1))))
    :else (update-inventario lista (rest _productos) productos)
    )
  ) 

;Funcion para actualizar las monedas segun la denominacion
(defn update-monedas [lista]
  (cond
    (= (first lista) 1) (list-update-in-recur monedas 0 (repeat (- (count (first monedas)) (count lista)) 1))
    (= (first lista) 2) (list-update-in-recur monedas 1 (repeat (- (count (second monedas)) (count lista)) 2))
    (= (first lista) 5) (list-update-in-recur monedas 2 (repeat (- (count (second (rest monedas))) (count lista)) 5))
    (= (first lista) 10) (list-update-in-recur monedas 3 (repeat (- (count (second (rest (rest monedas)))) (count lista)) 10))
    :else "Maquina sin cambio"))

(defn vending-machine [productos ventas]
           (cond
             ;Si cualquiera de las dos listas esta vacia, regresa una lista un nulo
           (and (empty? ventas) (empty? productos)) nil
             ;Realiza el registro de la venta si el codigo de la venta y de los productos son iguales
           (= (get-codigo ventas) (ffirst (rest (rest (first productos)))))
            (cond
              ;Si la cantidad de productos es cero, la maquina regresa sin productos
              (= 0 (ffirst (rest (rest (rest (first productos)))))) "Sin productos"
              ;Si no se tienen suficientes monedas en la sub lista, regresa fondos insuficientes. 
              (< (- (apply + (ffirst ventas)) (ffirst (rest (first productos)))) 0) "Fondos insuficientes"
              ;Si la operacion es exitosa, se regresa una lista con el producto y el cambio.
              :else  (let [venta (list (ffirst productos) (get-cambio (- (apply + (ffirst ventas)) (first (fnext (first productos))))))
                           cambio (get-cambio (- (apply + (ffirst ventas)) (first (fnext (first productos)))))
                           ganancia (ffirst ventas)]
                       (pvalues 
                        (newline)(cons venta (vending-machine productos (rest ventas))) 
                        (newline)(update-monedas cambio)   
                        ;(newline) (update-inventario venta productos _productos)
                        (newline)(apply + (concat ganancia ganancias))
                        )
                       )) 
             ;Si no se encuentra el producto en la lista, se realiza la llamada recursiva. 
           :else (vending-machine (rest productos) ventas)))

(defn main [&args]
  (pvalues (vending-machine productos1 ventas) (vending-machine productos2 ventas) (vending-machine productos3 ventas))
  )