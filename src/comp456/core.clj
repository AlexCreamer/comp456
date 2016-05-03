(ns comp456.core)  

(defn nodeValidation 
  ""
  [inputVector previousPath evaluatingNode endNode]
  (if (not(contains? (:paths (nth previousPath 0)) evaluatingNode))  
    (cond
      (= endNode evaluatingNode)
        2 
      :else
        1
    )
    0
   )
 )

(defn returnResults
  ""
  [currentNode tempLength tempPath previousPath]

  (vector (assoc
    (assoc
      (nth previousPath 0)
        :pathLength
        tempLength)
      :paths 
       (conj tempPath currentNode)))
  ) 
      
 

(defn acyclic
  ""
  [inputVector currentNode endNode previousPath]
   
  (loop [i 0 tempLength Double/POSITIVE_INFINITY tempPath []]

     (if (not (< i (count (:paths (nth inputVector currentNode))))) 
           (returnResults currentNode tempLength tempPath previousPath)
   
       (let [evaluatingConnection (nth (:paths (nth inputVector currentNode))i)] 
        (let [nextNodeValidation (nodeValidation inputVector previousPath (:node evaluatingConnection) endNode)] 

          (cond
           (= nextNodeValidation 2)  
             (if (< (:cost evaluatingConnection) tempLength)
                (recur (+ i 1) (:cost evaluatingConnection) [(:node evaluatingConnection)]) 
                (recur (+ i 1) tempLength tempPath)
              )
              
           (= nextNodeValidation 1)
            (let [returnVector (acyclic inputVector (:node evaluatingConnection) endNode [(assoc (nth previousPath 0) :paths (conj (:paths (nth previousPath 0)) currentNode))])] 

              (if (< (+(:pathLength (nth returnVector 0)) (:cost evaluatingConnection)) tempLength)           
                (recur (+ i 1) (+ (:pathLength (nth returnVector 0))(:cost evaluatingConnection)) (:paths(nth returnVector 0))) 
                (recur (+ i 1) tempLength tempPath)
               )
             ) 

            :else 
             (recur (+ i 1) tempLength tempPath)
           )

          ) 
         )
        )
      )   
   ) 
     

(defn -main
  "ye" 
  []
  (let [inputVector [{:paths [{:node 1, :cost 3}]},
                   {:paths [{:node 2, :cost 1}, {:node 3, :cost 5}]},
                   {:paths [{:node 3, :cost 2}]},
                   {:paths [{:node 1, :cost 2}]}]]
                  
    (let [previousPath [{:pathLength Double/POSITIVE_INFINITY :paths []}]]
       (let [results (nth (acyclic inputVector 0 3 previousPath) 0)]
        (println (assoc results :paths (reverse (:paths results))))
        )
      )
   )
 )
 
