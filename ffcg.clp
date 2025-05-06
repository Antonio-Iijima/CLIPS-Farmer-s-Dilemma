(deffacts data
   (initial-fact))


; SETUP


(deftemplate state
   (slot valid
      (default TRUE))
   (slot farmer 
      (type SYMBOL))
   (slot fox 
      (type SYMBOL))
   (slot chicken
      (type SYMBOL))
   (slot grain
      (type SYMBOL))
   (slot step
      (type INTEGER)
      (default 1))
   (slot previous
      (default FALSE)))


(deffunction get-input (?prompt)
   (printout t ?prompt crlf)
   (read))


(defrule setup
   (initial-fact)
   =>
   (bind ?farmer (get-input "Farmer starting position: "))
   (bind ?fox (get-input "Fox starting position: "))
   (bind ?chicken (get-input "Chicken starting position: "))
   (bind ?grain (get-input "Grain starting position: "))
   (bind ?combinations (get-input "Number of invalid combinations: "))
   (assert (invalid-combinations ?combinations))
   (assert 
      (state 
         (farmer ?farmer) 
         (fox ?fox) 
         (chicken ?chicken)
         (grain ?grain))))


(defrule build-invalid-states
   ?current <- (invalid-combinations ?n)
   =>
   (if 
      (> ?n 0)
   then 
      (printout t "Specify invalid state (" ?n " remaining):" crlf)
      (bind ?rule (readline))
      (build
         (str-cat
            "(defrule invalid-state-" ?n
               " ?state <- " ?rule
               " => (duplicate ?state (valid FALSE)) (retract ?state))"))
         (assert (invalid-combinations (- ?n 1)))
   else 
      (assert (search)))
   (retract ?current))


(defrule move-farmer
   (search)
   ?current <- (state 
                  (valid TRUE)
                  (farmer ?p)
                  (fox ?f)
                  (chicken ?c)
                  (grain ?g)
                  (step ?step)
                  (previous ?prev))
   (not 
      (state
         (valid FALSE)
         (farmer ~?p)
         (fox ?f)
         (chicken ?c)
         (grain ?g)))
   =>
   (duplicate ?current
      (farmer (not ?p))
      (step (+ ?step 1))
      (previous ?current)))


(defrule move-fox
   (search)
   ?current <- (state 
                  (valid TRUE)
                  (farmer ?p)
                  (fox ?p)
                  (chicken ?c)
                  (grain ?g)
                  (step ?step)
                  (previous ?prev))
   (not 
      (state
         (valid FALSE)
         (farmer ?q&~?p)
         (fox ?q&~?p)
         (chicken ?c)
         (grain ?g)))
   =>
   (duplicate ?current
      (farmer (not ?p))
      (fox (not ?p))
      (step (+ ?step 1))
      (previous ?current)))


(defrule move-chicken
   (search)
   ?current <- (state 
                  (valid TRUE)
                  (farmer ?p)
                  (fox ?f)
                  (chicken ?p)
                  (grain ?g)
                  (step ?step)
                  (previous ?prev))
   (not 
      (state
         (valid FALSE)
         (farmer ?q&~?p)
         (fox ?f)
         (chicken ?q&~?p)
         (grain ?g)))
   =>
   (duplicate ?current
      (farmer (not ?p))
      (chicken (not ?p))
      (step (+ ?step 1))
      (previous ?current)))
      

(defrule move-grain
   (search)
   ?current <- (state 
                  (valid TRUE)
                  (farmer ?p)
                  (fox ?f)
                  (chicken ?c)
                  (grain ?p)
                  (step ?step)
                  (previous ?prev))
   (not 
      (state
         (valid FALSE)
         (farmer ?q&~?p)
         (fox ?f)
         (chicken ?c)
         (grain ?q&~?p)))
   =>
   (duplicate ?current
      (farmer (not ?p))
      (grain (not ?p))
      (step (+ ?step 1))
      (previous ?current)))


(defrule break-loop
   (auto-focus TRUE)
   (state
      (valid TRUE)
      (farmer ?p)
      (fox ?f)
      (chicken ?c)
      (grain ?g)
      (step ?n))
   ?repeat <- (state
                  (valid TRUE)
                  (farmer ?p)
                  (fox ?f)
                  (chicken ?c)
                  (grain ?g)
                  (step ?m&~?n))
   (test (< ?n ?m))
   =>
   (duplicate ?repeat (valid FALSE)))


(defrule success
   ?success <- (state 
                  (farmer TRUE) 
                  (fox TRUE) 
                  (chicken TRUE) 
                  (grain TRUE))
   ?s <- (search)
   =>
   (retract ?s)
   (assert (success ?success)))





(reset)
(watch facts)
;(watch activations)
(run)
FALSE
FALSE 
FALSE 
FALSE
2
(state (farmer ?p1) (fox ?p2&~?p1) (chicken ?p2))
(state (farmer ?p1) (chicken ?p2&~?p1) (grain ?p2))
(facts)
