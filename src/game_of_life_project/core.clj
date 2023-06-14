(ns game-of-life-project.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

; 2 Rules
; - a dead cell with exactly three living neighbors is reborn in the next generation
; - only a living cell with two or three living neighbors remains alive in the next generation

;  constants
(def world-width 100)
(def world-height 100)
(def cell-size 6)
(def speed 20)

(def neighbor-offsets [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]])

; start generation
(def start-generation #{[1 1] [1 5] [1 6] [2 6] [11 5] [11 6] [11 7]
                        [12 4] [12 8] [13 3] [13 9] [14 3] [14 9]
                        [15 6] [16 4] [16 8] [17 5] [17 6] [17 7]
                        [18 6] [21 3] [21 4] [21 5] [22 3] [22 4]
                        [22 5] [23 2] [23 6] [25 1] [25 2] [25 6]
                        [25 7] [35 3] [35 4] [36 3] [36 4]})

(defn get-coord
  "Returns the x or y value from a cell"
  [selector cell]
  (selector cell))

(def get-x (partial get-coord first))
(def get-y (partial get-coord last))

; examples
(get-x [3 5])
(get-y [3 5])


(defn valid-coord?
  "Checks if a coordinate is in world limits"
  [f size cell]
  (let [value (f cell)]
    (and (<= 0 value) (< value size))))

(def valid-x-coord? (partial valid-coord? get-x world-width))
(def valid-y-coord? (partial valid-coord? get-y world-height))

(defn valid-coords? [cell]
  (and (valid-x-coord? cell) (valid-y-coord? cell)))

; examples
(valid-coords? [1 2])
(valid-coords? [150 10])


(defn get-neighbors
  "Determines all neighbours of a given coordinate"
  [cell]
  (loop [idx 0 neighbors []]
    (if (< idx 8)
      (let [neighbor (map + cell (nth neighbor-offsets idx))]
        (if (valid-coords? neighbor)
          (recur (inc idx) (conj neighbors neighbor))
          (recur (inc idx) neighbors)))
      neighbors)))

; examples
(get-neighbors [3 3])
(get-neighbors [0 0])


(defn count-active-neighbors 
  "Counts how many of the neighbors are active"
  [generation neighbors]
  (loop [idx 0 active-neighbors-counter 0]
    (if (< idx (count neighbors))
      (if (contains? generation (nth neighbors idx))
        (recur (inc idx) (inc active-neighbors-counter))
        (recur (inc idx) active-neighbors-counter))
      active-neighbors-counter)))

; examples
(get-neighbors [1 1])
(count-active-neighbors #{[1 2] [4 3]} (get-neighbors [1 1]))


; hasmap: {:[cord-x]-[cord-y] [appearances]}
; = {:1-1 3 :2-1 1 ...} => counts appearances of a cell
(defn add-neighbors-to-hashmap
  "Returns a new Hasmap with the new neighbors added"
  [old-hashmap neighbors]
  (loop [idx 0 new-hashmap old-hashmap]
    (if (< idx (count neighbors))
      (let [cell-keyword (keyword
                          (str (get-x (nth neighbors idx)) "-"
                               (get-y (nth neighbors idx))))]

      ; Checks if there is already a key to the cell
        (if (contains? new-hashmap cell-keyword)

        ; yes => increment appearances +1
          (recur (inc idx)
                 (update new-hashmap cell-keyword inc))

        ; no => create entry 
          (recur (inc idx)
                 (assoc new-hashmap cell-keyword 1))))
    new-hashmap)))

; examples
(add-neighbors-to-hashmap {} [[1 1] [2 2] [2 2]])
(add-neighbors-to-hashmap {:1-1 1 :2-2 2} [[2 2] [3 3]])


(defn keyword-to-cell
  "Converts a keyword to a cell"
  [keyword]
  (into []
        (map #(Integer/parseInt %)
             (.split (name keyword) "-"))))

; exmaples
(keyword-to-cell :1-1)

(defn get-new-active-cells
  "Returns every cell with exactly 3 active neighbor cells"
  [pointers]
  (let [new-active-cells-keywords
        (filter (comp #{3} pointers) (keys pointers))]
    (map keyword-to-cell new-active-cells-keywords)))

; examples
(get-new-active-cells {:1-1 3 :2-2 2 :3-3 3})


(defn get-next-generation 
  "Returns the next generation, given the previous generation"
  [generation]
  (loop [new-generation #{}
         all-neighbors-hashmap {}
         gen-stack generation]
    
    ; Whether for all active cells of the previous generation
    ; all the active neighbors were calculated
    (if (not= (count gen-stack) 0) ; notEmpty
      
      ; get all neighbors from one cell
      (let [neighbors (get-neighbors (first gen-stack))
            
            ; get only the active neighbors
            active-neighbors (count-active-neighbors generation neighbors)]
        
        ; checks if the current cell can remain alive
        (if (contains? #{2 3} active-neighbors)
          
          ; current cell can remain alive
          (recur (conj new-generation (first gen-stack))
                 (add-neighbors-to-hashmap all-neighbors-hashmap neighbors)
                 (rest gen-stack))
          
          ; current cell dies
          (recur new-generation
                 (add-neighbors-to-hashmap all-neighbors-hashmap neighbors)
                 (rest gen-stack))))
      
      ; calculates all cells that will be born
      ; and returns all new active cells
      (into new-generation (get-new-active-cells all-neighbors-hashmap)))))


; UI
(defn setup
  []
  (q/frame-rate speed)
  (q/color-mode :hsb)
  (q/no-stroke)
  start-generation)

(defn update-state
  [state]
  (get-next-generation state))

(defn draw-state
  [state]
  (q/background 255)
  (doseq [cell state]
    (q/fill 0)
    (q/rect (* cell-size (get-x cell))
            (* cell-size (get-y cell))
            cell-size
            cell-size)))

(defn -main,
  "Start Conway's Game of Life"
  [& args]
  (q/sketch
    :host "host"
    :title "Conway's Game of Life"
    :size [(* world-width cell-size) (* world-height cell-size)]
    :setup setup
    :update update-state
    :draw draw-state
    :middleware [m/fun-mode]))