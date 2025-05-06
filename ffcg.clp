; NOTES
; The following program has been modified, remodified, and rewritten
; numerous times in an effort to stop infinite progression.
; In this latest version I resorted to the brute-force "check every single
; time whether an invalid version of this solution exists," which still fails.
; I am doubtless missing some obvious mistake somewhere, but until
; I find it, this is the current program. The logic is sound, I think,
; but the implementation probably is not.


(deffacts data
   (initial-fact))


; SETUP


; Snapshot of the Dilemma at a given moment
; Side of the river indicated by TRUE | FALSE
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


; Function use simply to avoid repetition
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


; Add as many constraints as desired
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


; MOVEMENT
; Moves each individually, as I could not find a syntax to
; abstract the label of a slot as a variable.
; Further copious use of valid state checking, 
; which does not appear to work as intended.


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



; Invalid states are handled by the invalid-state builder, so
; all that's left is breaking any cycles (notably TRUE-FALSE-TRUE)
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


; Stop searching if the final state has been reached
(defrule success
   ?success <- (state 
                  (farmer TRUE) 
                  (fox TRUE) 
                  (chicken TRUE) 
                  (grain TRUE))
   ?s <- (search)
   =>
   (retract ?s)
   (assert show-answer ?success))


; Retrace the successful path (never actually got to run this, as the
; program still continues to oscillate in an infinite cycle)
(defrule answer
   ?solution <- (show-answer ?success))
   =>
   (printout t "Move " (fact-slot-value ?success farmer))
   (retract ?solution)
   (assert (show-answer (fact-slot-value ?success previous)))





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
