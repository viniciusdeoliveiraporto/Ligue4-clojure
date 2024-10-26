(defn changePlayer [player] (if (= player 1) 2 1))

(defn findLine
      ([col grade] (findLine 5 col grade))
      ([i col grade]
       (loop [i i]
        (cond
              (= i (- 1)) -1
              (= (get (get grade i) col) 0) i
              :else (recur (dec i))))))

(defn newGrade [n lin col grade]
               (assoc-in grade [lin col] n))

(defn checkUpright [play row column grade]
      (loop [sequen 0 l row]
        (cond
             (= l 6) sequen
             (not= (get (get grade l) column) play) sequen
             :else (recur (inc sequen) (inc l)))))

(defn checkHorizontal [play row column grade]
      (loop [c 0 sequen 0 columnPlay false]
           (cond
                (= c 7) -1
                (and (= (+ sequen 1) 4) (or (true? columnPlay) (= c column))) 4
                (and (= (get (get grade row) c) play) (= c columnPlay)) (recur (inc c) (inc sequen) true)
                (= (get (get grade row) c) play) (recur (inc c) (inc sequen) columnPlay)
                :else (recur (inc c) 0 false))))

(defn findBaseDiagonal [row column operation isBaseRow queue]
      (loop [i row j column]
            (cond
                 (and (isBaseRow i) (> j 0))
                      (recur (operation i) (dec j))
                 (= queue "line") i
                 (= queue "column") j
      )))

(defn checkDiagonal [play linePlay columnPlay grade slotRow
                    slotColumn isBaseRow operation]
      (loop [sequen 0 playHouse false slotRow slotRow slotColumn slotColumn]
            (cond
                  (or (not (isBaseRow slotRow)) (> slotColumn 6))
                      (if (and (= sequen 4) (true? playHouse)) sequen -1)
                  (and (and (= (get (get grade slotRow) slotColumn) play)
                       (= slotRow linePlay)) (= slotColumn columnPlay))
                       (recur (inc sequen) true (operation slotRow) (inc slotColumn))
                  (= (get (get grade slotRow) slotColumn) play)
                       (recur (inc sequen) playHouse (operation slotRow) (inc slotColumn))
                  (and (= sequen 4) (true? playHouse)) sequen
                  :else (recur 0 false (operation slotRow) (inc slotColumn)))))

(defn checkMainDiagonal [play line column grade]
      (checkDiagonal play line column grade
      (findBaseDiagonal line column dec (fn [y] (> y 0)) "line")
      (findBaseDiagonal line column dec (fn [y] (> y 0)) "column")
      (fn [y] (<= y 5)) inc))

(defn checkSecondaryDiagonal [play line column grade]
      (checkDiagonal play line column grade
      (findBaseDiagonal line column inc (fn [y] (< y 5)) "line")
      (findBaseDiagonal line column inc (fn [y] (< y 5)) "column")
      (fn [y] (>= y 0)) dec))

(defn checkWin [play line column grade]
      (cond
           (= (checkUpright play line column grade) 4) play
           (= (checkHorizontal play line column grade) 4) play
           (= (checkMainDiagonal play line column grade) 4) play
           (= (checkSecondaryDiagonal play line column grade) 4) play
           :else 0))


(defn main [] (loop [totalWins {:1 0 :2 0} setUpGame {:turn "s"}]
                      (cond
                            (= (get setUpGame :turn) "n") (println "Fim de jogo")
                      :else (do
                      (loop [continueGame "s" parcialWins {:1 0 :2 0} setUpTurn {:playerOfTurn 1 :playsDone 0 :grade (vec (repeat 6 (vec (repeat 7 0))))}]
                      (cond
                           (and (< (get setUpTurn :playsDone) 42) (not= continueGame "d"))
                      (do (println "Jogada? (1-7 / d): ")
                      (def play (read-line))
                      (if (= play "d") (recur play parcialWins setUpTurn)
                      (do (let [p (Integer/parseInt play)] (when (and (re-find #"\d+" play) (and (pos? p) (< p 8)))
                      (let [column (dec (Integer/parseInt play)) line (findLine column (get setUpTurn :grade))]
                      (if (not= line -1)
                      (do (def newPlaysDone (inc (get setUpTurn :playsDone))) (def grade (newGrade (get setUpTurn :playerOfTurn) line column (get setUpTurn :grade)))
                      (doseq [row grade] (println row))
                      (def score (checkWin (get setUpTurn :playerOfTurn) line column grade))
                      (if (pos? score)
                      (if (= score 1) (def newParcialWins (assoc parcialWins :1 (inc (get parcialWins :1))))
                      (if (= score 2) (def newParcialWins (assoc parcialWins :2 (inc (get parcialWins :2)))))) (def newParcialWins parcialWins))
                      ) (do (def newParcialWins parcialWins) (def newPlaysDone (get setUpTurn :playsDone))))
                      )))
                      (println (str "\nParcial da rodada: \nVitórias jogador 1: " (get newParcialWins :1)
                                    "\nVitórias jogador 2: " (get newParcialWins :2)))
                      (let [playerOfTurn (changePlayer (get setUpTurn :playerOfTurn))
                            setUpTurn (assoc setUpTurn :playerOfTurn playerOfTurn :playsDone newPlaysDone :grade grade)]
                      (recur "s" newParcialWins setUpTurn)))
                      ))
                      :else (do (println "\nFim da rodada.")
                                (if (> (get parcialWins :1) (get parcialWins :2))
                                (def newTotalWins (assoc totalWins :1 (inc (get totalWins :1))))
                                (if (> (get parcialWins :2) (get parcialWins :1))
                                (def newTotalWins (assoc totalWins :2 (inc (get totalWins :2))))
                                (do (def newTotalWins totalWins) (println "\nRodada terminou em empate")))))))
                      (println (str "--------------------\n" "Vitórias totais jogador 1: " (get newTotalWins :1)
                      "\nVitórias totais jogador 2: " (get newTotalWins :2)))
                      (println "\nNovo turno? (s / n) ") (let [newTurn (clojure.string/lower-case (read-line))
                      setUpGame (assoc setUpGame :turn newTurn)] (recur newTotalWins setUpGame))))))
(main)
