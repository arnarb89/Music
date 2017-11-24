(setf *random-state* (make-random-state t))

(defvar mypath "/Users/arnar/Desktop/midi")



(defvar fjoldi 100)

(defvar skali ‘(0 1 2 3 4 5 6 7 8 9 10 11 12))
(defvar lowestNote 40)
(defvar highestNote 82)

(defvar lowestVelocity 12)
(defvar attackScale '(48 96 24 12))

(defvar velocityVariation 16)

(defvar minDurationMultiplier 0.25)
(defvar durationintervals 0.25)

(defvar midiValue 5)

(defvar myFlokkur 3)

(defvar myTema (list 60 64 67 60))
(defvar fjoldiTesta 3)

(defvar myFlokkurA '(0 1 2 3 4 5 6 7 8 9 10 11 12))
(defvar myFlokkurB '(0 1 2 10 11))
(defvar myFlokkurC '(0 3 4 8 9))
(defvar myFlokkurD '(0 5 6 7))

(defvar markovChain '((0.4 0.2 0.3 0.1)(0.5 0.4 0.0 0.1)(0.2 0.1 0.7 0.0)(0.2 0.1 0.3 0.4)))
(defvar myname "stemningC.mid")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Stemning C

;;fegurð
;;frjálst val 

(setq skali ‘(0 3 5 7 10 12)) ;;done
(setq lowestNote 45) ;;done
(setq highestNote 80) ;;done
(setq myFlokkur 3) ;;done
        ;(setq A '(0 1 2 3 4 5 6 7 8 9 10 11 12))
        ;(setq B '(0 1 2 10 11))
        ;(setq C '(0 3 4 8 9))
        ;(setq D '(0 5 6 7))
        (setq myFlokkurD (list 0 2 3 7))
(setq myTema (list 60 63 65 67)) ;;done


(setq attackScale '(48 96 24 192)) ;;done
(setq markovChain '((0.25 0.25 0.25 0.25)(0.45 0.1 0.45 0)(0.25 0.25 0.25 0.25)(0.3 0.4 0.3 0))) ;;done


(setq velocityVariation 12) ;;done


(setq minDurationMultiplier 0.5) ;;done
(setq durationintervals 0.5) ;;done

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defvar *time-sign* nil)
(defvar *numb* 4)
(defvar *mesure* 4)

(defvar amplitude-list nil)
(defvar pitch-envelope-list nil)
(defvar look-midi nil)

(defun time-list2 (lst)
    (reverse (maplist #'(lambda  (el) 
        (apply #'+  el))
            (reverse lst))))

(Defun set-time-sign (num mes)
    (setf *numb* num)
    (setf *mesure* mes))

(defun time-list (lst)
    (cond((< 0 (length lst))
        (let (( ll (if (= (car lst) 0) lst(cons 0 lst))))   
            (reverse (maplist #'(lambda  (el) 
                (apply #'+  el))
                    (reverse ll)))))))

(defmacro while (test &body body)
    (let ((testlab (gensym))
        (toplab (gensym)))
        `(tagbody
        (go ,testlab)
        ,toplab
        (progn ,@body)
        ,testlab
        (when ,test (go ,toplab)))))

(defun No-parenth (lst)
    (let ((result1))
        (while lst
            (cond (( numberp (car lst)) (push  (pop lst)result1))
            ((listp (car lst))  (mapcar #'(lambda (ll)
            (push   ll result1)) (pop lst)))))
        (reverse result1)))

(defun round2 (x)
    (if (numberp x)
        (if (minusp x)
            (* (truncate (+  (abs x) 0.5)) -1)
            (truncate (+ x 0.5))) x))          

(defun variable-length-midi-time (ticks)
    (let ((high-byte1 0)(high-byte2 0)(high-byte3 0)
        (mod-ticks (mod ticks #x80)))
        (cond 
            ((> ticks #x1FFFFF)
                (setq high-byte3 (+ #x80 (truncate (/ ticks #x200000))))
                (setq high-byte2 (+ #x80 mod-ticks))
                (setq high-byte1 (+ #x80 mod-ticks))
                (setq ticks mod-ticks)
                (list high-byte3 high-byte2 high-byte1 ticks))  
            ((> ticks #x3FFF)
                (setq high-byte2 (+ #x80 (truncate (/ ticks #x4000))))
                (setq high-byte1 (+ #x80 mod-ticks))
                (setq ticks mod-ticks)
                (list high-byte2 high-byte1 ticks))  
            ((> ticks #x7F)
                (setq high-byte1 (+ #x80 (truncate (/ ticks #x80))))
                (setq ticks mod-ticks)
                (list high-byte1 ticks))  
            (t (list ticks)))))

(defun covert-length-to-4-byte-list (len)
    (let ((byte1 0)(byte2 0)(byte3 0)(byte4 0))
        (cond 
            ((> len #xFFFFFF)
                (setq byte4 (truncate (/ len #x1000000)))
                (setq byte3 (mod len #x1000000))
                (setq byte3 (mod len #x10000))
                (setq byte2 (mod len #x10000))
                (setq byte1 (mod len #x100)))
            ((> len #xFFFF)
                (setq byte3 (truncate (/ len #x10000)))
                (setq byte2 (mod len #x10000))
                (setq byte1 (mod len #x100)))
            ((> len #xFF)
                (setq byte2 (truncate (/ len #x100)))
                (setq byte1 (mod len #x100)))
            (t  (setq byte1 len)))
        (list byte4 byte3 byte2 byte1)))

(defun  make-variable-length-midi-delta-times  (midi-list)
    (let ((res)(time-now 0))
        (WHILE midi-list
            (push (append (variable-length-midi-time (- (caar midi-list) time-now))
                    (cdar midi-list)) res)
            (setq time-now (caar midi-list))
            (pop midi-list))
            (no-parenth (nreverse res) )))

(defun two-incr-lists (notes  amp pitch)
    (let ((out-list)
        (new-amp   (mapcar #'(lambda (l1)
                           (list (nth 0 l1)(+ 176 (nth 1 l1))
                                 (nth 2 l1)(nth 3 l1)))(copy-list amp)))
        (new-pitch   (mapcar #'(lambda (l1)
                           (list (nth 0 l1)(+ 224 (nth 1 l1))
                                 (nth 2 l1)(nth 3 l1)))(copy-list pitch)))) 
        (while (or  new-amp notes   new-pitch)
        (cond ((and (neq nil notes)
                (or(eq  nil new-amp) 
                   (<= (car (car notes))(car(car new-amp))))
                (or(eq  nil new-pitch) 
                   (<= (car (car notes))(car(car new-pitch)))))
           (push (pop notes)out-list))
          ((and (neq nil new-amp)
                (or(eq  nil notes)
                   (> (car (car notes))(car(car new-amp))))
                (or(eq  nil new-pitch) 
                   (> (car(car new-pitch))(car(car new-amp)))))
           (push (pop new-amp) out-list))
          (t   (if (neq nil new-pitch)(push (pop new-pitch)out-list))) 
        ))
        (reverse out-list)))

(defun  make-midi-file-list  (attacks pitches durs vels chans)
    (setq chans (mapcar #'(lambda (l1)(+ 1 l1))   (copy-list chans)))
    (let ((t-time)(note)(midi-list))
        (while attacks
        (setq t-time (pop attacks))
        (setq note (pop pitches))
        (push (list 
                t-time
                (+ #x8f (car chans))
                 note
                (pop vels))
              midi-list)
        (push (list 
                (+ t-time (car durs))
                (+ #x8f (car chans))
                note
                 0)
            midi-list)
        (pop durs)
        (pop chans))
        (if(or (neq nil amplitude-list)(neq nil pitch-envelope-list))
            (setq  midi-list (two-incr-lists(sort 
               (nreverse midi-list) #'< :key #'(lambda (a) (car a)))
          amplitude-list pitch-envelope-list))
           (setq  midi-list (sort  (nreverse midi-list) #'< :key #'(lambda (a) (car a)))))
        (setq look-midi   midi-list)
              (make-variable-length-midi-delta-times     midi-list  )))

(defun make-midi-file-0 (attacks pitches durs vels chans )
    (let ((data (make-midi-file-list attacks pitches durs vels chans))
        (track-info
          '(#x00 #xff #x58 #x04 #x04  #x02 #x24 #x08
            #x00 #xff #x51 #x03 #x07 #xa1 #x20)) 
        (track-end '(#x00 #xff #x2f #x00)))
        (append     
            (no-parenth(list #x4D #x54 #x68 #x64 
                #x00 #x00 #x00 #x06  
                #x00 #x00   
                #x00 #x01
                #x00 #x60
                #x4D #x54 #x72 #x6B))
            (covert-length-to-4-byte-list (+ (length track-info)(length data)(length track-end)))
            track-info
            data
            track-end)))

(defun set-mac-file-type (path mac-file-type)
    (declare (ignore path mac-file-type)) t)

(defun PW-midi-file-SAVE1  (midi-data-list name1 path)
    (let* ((new-file))
        (setq *PRINT-BASE* 2)
        (setq new-file (ccl::create-file  (make-pathname :directory path :name   name1)))
            (WITH-OPEN-FILE  (out new-file :direction :output :IF-EXISTS :overwrite
                :element-type '(unsigned-byte  8))
                (WHILE midi-data-list
                    (write-byte       (pop midi-data-list)      out  )))
                    (setq *PRINT-BASE* 10)))

(defun PW-midi-file-SAVE3  ( name1 path thepitch theduration theattack thevelocity thechannels)
    (let ((new-att (time-list(append (list 0)  (cdr  (mapcar #'(lambda (l1 )(round2 (* 2 l1 )))  theattack) ))))
        (new-dur   (mapcar #'(lambda (l1 )(round2 (* 2 l1 )))    theduration) ))
        (PW-midi-file-SAVE1  (make-midi-file-0
        new-att thepitch new-dur thevelocity thechannels)  name1 path )))
 

     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun finder (stak listi)
	(neq nil (find stak listi))
)
(defun random2 (minimum maximum)
	(+ minimum (random (+ 1 (- maximum minimum)) ))
)
(defun pitchFunction (min max amount scale)
	(let ((outlist) (x))
		(loop 
			(setq x (random2 min max))
			(if (finder (mod x 12) scale)
				(push x outlist)
				(when 
					(eq amount (length outlist)) 
					(return (reverse outlist))
				)
			)
		)
	)
)

#|
(defun pitchFunction2 (flokkurTonbila min max amount scale)
    (let ((outlist)(x)(A)(B)(C)(D))
        (setq A myFlokkurA)
        (setq B myFlokkurB)
        (setq C myFlokkurC)
        (setq D myFlokkurD)
        ;(setq D '(0 2 4 5 7 9 11 12))
        (cond 
            ((eq flokkurTonbila 0)
                (setq flokkurTonbila A)
                )
            ((eq flokkurTonbila 1)
                (setq flokkurTonbila B))
            ((eq flokkurTonbila 2)
                (setq flokkurTonbila C))
            (t  (setq flokkurTonbila D)))
        (push (random2 min max) outlist)
        (loop 
            (setq x (random2 min max))
            (if (and 
                    (finder (mod x 12) scale) 
                    (finder 
                        (abs (- x (car outlist))) 
                        flokkurTonbila))
                (push x outlist)
                (when 
                    (eq amount (length outlist)) 
                    (return (reverse outlist))
                )
            )
        )
    )
)
|#

(defun within (el min max)
 	(cond ((and (neq nil el)
 		(or (and (>= el min) (<= el max))
 			(and (>= el max) (<= el min))))
 		t)
	)
)

(defun attackFunction (fjoldi attackScale markovChain)
	(let ((matrixan)(rythmar)(slembi) (listi) (count) (A) (B) (C) (outlist) (pivot))
		(setq matrixan markovChain)
		(setq rythmar attackScale)	
		(setq listi (car matrixan))
		(setq count 1)
		(push 0 outlist)
		(loop 
			(setq slembi (/ (random 10) 10) )
			(setq A (nth 0 listi))
			(setq B (nth 1 listi))
			(setq C (nth 2 listi))
			(cond
				((within slembi 0 A) (setq pivot 0))
				((within slembi A (+ A B)) (setq pivot 1))
				((within slembi (+ A B) (+ C (+ A B))) (setq pivot 2))
				((setq pivot 3))
			)
			(setq listi (nth pivot matrixan))
			(setq count (+ 1 count))
			(push (nth pivot rythmar) outlist)
			(when 
				(eq count fjoldi) 
				(return (reverse outlist))
			)
		)
	)
)

(defun velocityFunction(amount velocityVariation)
	(let ((outlist) (ferill)(newValue))
		(setq ferill (random 128))
		(push ferill outlist)
		(loop 
			(setq newValue (random2 (- 0 velocityVariation) velocityVariation))
			(setq newValue (+ newValue (car outlist)))
			(setq newValue (max 63 newValue))
			(setq newValue (min newValue 127))
			(push newValue outlist)
			(when 
				(eq amount (length outlist)) 
				(return (reverse outlist))
			)
		)
	)
)

(defun channelsFunction (amount)
	(make-list amount :initial-element midiValue)
)

(defun durationFunction (attacklist attackScale minDurationMultiplier durationIntervals)
    ; attackScale er rythma listinn
    (let ((outlist)(x)(durationgildi)(divider))
        (setq attacklist (cdr attacklist))
        (setq divider (/ 1 durationIntervals))
        ; 0-100% með 1/divider intervals... 8 er 12.5%... 4 er 25%... 2 er 50%... 100 er 1%...
        ; max = 1, min = minDurationMultiplier
        (loop 
            (setq x 
                (max
                    (min 
                        (+ 
                            (/  (random (+ divider 1)) divider)
                            (/ 1 divider)
                        ) 
                        1
                    )
                    minDurationMultiplier
                )
            )
            (setq durationgildi (* x (car attacklist)))
            (push durationgildi outlist)
            (setq attacklist (cdr attacklist))
            (when
                (eq attacklist '()) 
                (push (nth (random (length attackScale)) attackScale) outlist)
                (return (reverse outlist))
            )
        )
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun commonIntervals(x y)
    (let ((points)(pivot)(tempx)(tempy))
        (setq points 0)
        (setq pivot 0)
        (setq tempx (mapcar #'abs x))
        (setq tempy (mapcar #'abs y))
        (loop
            (if (finder (nth pivot tempx) tempy)
                (progn
                    (remove (nth pivot tempx) tempy :count 1)
                    (setq points (+ 1 points))
                )
            )
            (setq pivot (+ pivot 1))
            (when 
                (eq pivot (length tempx)) 
                (return points)
            )
        )
    )
)
;(commonIntervals '(4 3 -7) '(2 3 -7))
;(commonIntervals '(4 3 -7) '(8 -3 -5))
;(commonIntervals '(4 3 -7) '(-4 7 -3))

(defun locationOfIntervals (x y)
    (let ((points)(pivot)(tempx)(tempy))
        (setq points 0)
        (setq pivot 0)
        (setq tempx (mapcar #'abs x))
        (setq tempy (mapcar #'abs y))
        (loop
            (when 
                (eq pivot (length tempx)) 
                (return points)
            )
            (if (eq (nth pivot tempx) (nth pivot tempy))
                (progn
                    (setq points (+ 1 points))
                )
            )
            (setq pivot (+ pivot 1))
            
        )
    )
)
;(locationOfIntervals '(4 3 3 4) '(4 4 3 3))
;(locationOfIntervals '(4 3 -7) '(2 3 -7))
;(locationOfIntervals '(4 3 -7) '(8 -3 -5))
;(locationOfIntervals '(4 3 -7) '(-4 7 -3))

(defun directonOfIntervals(x y)
    (let ((points)(pivot))
        (setq points 0)
        (setq pivot 0)
        (loop
            (when 
                (eq pivot (length x)) 
                (return points)
            )
            (if (eq (plusp (nth pivot x)) (plusp (nth pivot y)))
                (progn
                    (setq points (+ 1 points))
                )
            )
            (setq pivot (+ pivot 1))
            
        )
    )
)
;(directonOfIntervals '(1 2 -3 -3) '(1 -3 2 -4))
;(directonOfIntervals '(4 3 -7) '(2 3 -7))
;(directonOfIntervals '(4 3 -7) '(8 -3 -5))
;(directonOfIntervals '(4 3 -7) '(-4 7 -3))

(defun relationsOfIntervalGroups (x y)
    (let (
        ;(A)
        (B)(C)(D)(tempx)(tempy)(points))

        (setq tempx (sort (remove-duplicates (mapcar #'abs x))#'<))
        (setq tempy (sort (remove-duplicates (mapcar #'abs y))#'<))

        ;(setq A '(0 1 2 3 4 5 6 7 8 9 10 11 12))
        (setq B '(0 1 2 10 11))
        (setq C '(0 3 4 8 9))
        (setq D '(0 5 6 7))

        (setq points (+ 0 (ceiling (/ (length x) 2))))
        
        (cond
            ((and (subsetp tempx B) (subsetp tempy B)) points)
            ((and (subsetp tempx C) (subsetp tempy C)) points)
            ((and (subsetp tempx D) (subsetp tempy D)) points)
            (0)
        )
    )
)
;(relationsOfIntervalGroups '(4 3 -7) '(2 3 -7))
;(relationsOfIntervalGroups '(4 3 -7) '(2 3 -7))
;(relationsOfIntervalGroups '(4 3 -7) '(8 -3 -5))
;(relationsOfIntervalGroups '(4 3 -7) '(-4 7 -3))

(defun intervalsMainHelp (tema listOfLists)
    (let ((listOfPoints)(pivot)(points))
        (setq pivot 0)
        (setq points 0)
        (loop
            (when 
                (eq pivot (length listOfLists)) 
                (return (reverse listOfPoints))
            )
            (setq points (+ points (commonIntervals tema (nth pivot listOfLists))))
            (setq points (+ points (locationOfIntervals tema (nth pivot listOfLists))))
            (setq points (+ points (directonOfIntervals tema (nth pivot listOfLists))))
            (setq points (+ points (relationsOfIntervalGroups tema (nth pivot listOfLists))))
            (push points listOfPoints)
            (setq points 0)
            (setq pivot (+ pivot 1))
        )
    )
)
;(intervalsMainHelp (list 4 3 -7) (list (list 2 3 -7) (list 8 -3 -5) (list -4 7 -3)) )

(defun intervalsMain (tema listOfIntervalLists)
    (let ((listOfPoints)(pivot)(themax))
        (setq pivot 0)
        (setq listOfPoints (intervalsMainHelp tema listOfIntervalLists))
        (setq themax (apply 'max listOfPoints))
        (loop 
            (when 
                (eq pivot (length listOfIntervalLists)) 
                (return "out of bounds")
            )
            (when 
                (eq themax (nth pivot listOfPoints)) 
                (return (nth pivot listOfIntervalLists))
            )
            (if (neq themax (nth pivot listOfPoints))
                ;(return (nth pivot listOfIntervalLists))
                (setq pivot (+ 1 pivot))
            )
        )
    )
)
;(intervalsMain (list 4 3 -7) (list (list 2 3 -7) (list 8 -3 -5) (list -4 7 -3)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getFirstPitch (min max scale)
    (let ((x)(outlist))
        (loop 
            (when (eq 1 (length outlist)) 
                (return (car outlist))
            )
            (setq x (random2 min max))
            (if (finder (mod x 12) scale) 
                (push x outlist)
            )
        )
    )
)


(defun pitchFunction2 (flokkurTonbila min max amount scale)
    (let ((outlist)(x)(A)(B)(C)(D))
        (setq A '(0 1 2 3 4 5 6 7 8 9 10 11 12))
        (setq B '(0 1 2 10 11))
        (setq C '(0 3 4 8 9))
        (setq D '(0 5 6 7))
        ;(setq D '(0 2 4 5 7 9 11 12))
        (cond 
            ( (eq flokkurTonbila 0) (setq flokkurTonbila A) )
            ( (eq flokkurTonbila 1) (setq flokkurTonbila B) )
            ( (eq flokkurTonbila 2) (setq flokkurTonbila C) )
            (t  (setq flokkurTonbila D)))
        (push (getFirstPitch min max scale) outlist)
        
        (loop 
            (when 
                    (eq amount (length outlist)) 
                    (return (reverse outlist))
            )
            (setq x (random2 min max))
            (if (and 
                    (finder (mod x 12) scale) 
                    (finder 
                        (abs (- x (car outlist))) 
                        flokkurTonbila))
                (push x outlist)
                
            )
        )
    )
)
;(pitchFunction2 myFlokkur lowestNote highestNote 1 skali)

(defun getIntervals (listOfNotes)
    (let ( (outlist)(difference)(pivot1)(pivot2)  )
        (setq pivot1 0)
        (setq pivot2 1)
        (loop
            (when (eq pivot2 (length listOfNotes))
                (return (reverse outlist))
            )
            (setq difference (- (nth pivot2 listOfNotes) (nth pivot1 listOfNotes)))
            (push difference outlist)
            (setq pivot1 (+ 1 pivot1))
            (setq pivot2 (+ 1 pivot2))
        )
    )
)
;(getIntervals (list 1 7 3 9))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun temaMainHelp (finalIntervals listOfIntervalLists listOfNoteLists)
    (let ((counter))
        (setq counter 0)
        (loop 
            (when 
                (eq counter (length listOfIntervalLists)) 
                (return "out of bounds")
            )
            (when 
                (equal finalIntervals (nth counter listOfIntervalLists)) 
                (return (car (reverse (nth counter listOfNoteLists))))
            )
            (setq counter (+ 1 counter))
        )
    )
)
;(temaMainHelp (list 60 64 67 60) (list of lists) (list of lists))

(defun temaMain (tema last3Notes amount)
    (let ( (listOfNoteLists)(listOfIntervalLists) (counter)(x) )
        (setq counter 0)
        (loop 
            (when (eq counter amount)
                (return (temaMainHelp (intervalsMain (getIntervals tema) listOfIntervalLists) listOfIntervalLists listOfNoteLists))
            )
            (setq x (append last3Notes (pitchFunction2 myFlokkur lowestNote highestNote 1 skali)))
            (push x listOfNoteLists)
            (push (getIntervals x) listOfIntervalLists)
            (setq counter (+ 1 counter))
        )
    )
)
;(temaMain (list 60 64 67 60) (list 73 58 79) 3)

(defun pitchFunction3 (tema fjoldiNotna min max amountOfTests myFlokkur skali)
    (let ((outlist))
        (push (car (pitchFunction2 myFlokkur min max 1 skali)) outlist)
        (push (car (pitchFunction2 myFlokkur min max 1 skali)) outlist)
        (push (car (pitchFunction2 myFlokkur min max 1 skali)) outlist)
        (loop
            (when (eq (length outlist) fjoldiNotna)
                (return (reverse outlist))
            )
            (push (temaMain tema (reverse (subseq outlist 0 3)) amountOfTests) outlist)
        )
    )
)
;(pitchFunction3 (list 60 64 67 60) 50 lowestNote highestNote 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass OBJECT-ORGANIZER3 ()
	(
		(attack-list :initform nil :accessor attack-list )
		(pitch-list :initform nil :accessor pitch-list )
		(duration-list :initform nil :accessor duration-list )
		(channels-list :initform nil :accessor channels-list )
		(velocity-list :initform nil :accessor velocity-list )
	)
)

(defmethod write-attack-slot ((self OBJECT-ORGANIZER3) lst)
 	(setf (slot-value self ‘attack-list) lst))
(defmethod read-attack-slot ((self OBJECT-ORGANIZER3))
 	(slot-value self 'attack-list))

(defmethod write-pitch-slot ((self OBJECT-ORGANIZER3) lst)
 	(setf (slot-value self ‘pitch-list) lst))
(defmethod read-pitch-slot ((self OBJECT-ORGANIZER3))
 	(slot-value self 'pitch-list))

(defmethod write-duration-slot ((self OBJECT-ORGANIZER3) lst)
 	(setf (slot-value self ‘duration-list) lst))
(defmethod read-duration-slot ((self OBJECT-ORGANIZER3))
 	(slot-value self 'duration-list))

(defmethod write-channels-slot ((self OBJECT-ORGANIZER3) lst)
 	(setf (slot-value self ‘channels-list) lst))
(defmethod read-channels-slot ((self OBJECT-ORGANIZER3))
 	(slot-value self 'channels-list))

(defmethod write-velocity-slot ((self OBJECT-ORGANIZER3) lst)
 	(setf (slot-value self ‘velocity-list) lst))
(defmethod read-velocity-slot ((self OBJECT-ORGANIZER3))
 	(slot-value self 'velocity-list))

(defmethod save-to-file ((self OBJECT-ORGANIZER3) filename path)
 	(PW-midi-file-SAVE3 filename path 
 		(slot-value self 'pitch-list)
 		(slot-value self 'duration-list)
 		(slot-value self 'attack-list)
 		(slot-value self 'velocity-list)
 		(slot-value self 'channels-list)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar OBJECT-ORGANIZER3A nil)

(defun ultimate()

	
	(setq OBJECT-ORGANIZER3A (make-instance 'OBJECT-ORGANIZER3))
    ;(pitchFunction3 (list 60 64 67 60) 50 lowestNote highestNote 3)
	(write-pitch-slot OBJECT-ORGANIZER3A (pitchFunction3 myTema fjoldi lowestNote highestNote fjoldiTesta myFlokkur skali))
    ;(write-pitch-slot OBJECT-ORGANIZER3A (pitchFunction2  myFlokkur lowestNote highestNote fjoldi skali))
    ;(write-pitch-slot OBJECT-ORGANIZER3A (pitchFunction  lowestNote highestNote fjoldi skali))
	(write-attack-slot OBJECT-ORGANIZER3A (attackFunction fjoldi attackScale markovChain))
	(write-duration-slot OBJECT-ORGANIZER3A (durationFunction (read-attack-slot OBJECT-ORGANIZER3A) attackScale minDurationMultiplier durationIntervals))
	(write-velocity-slot OBJECT-ORGANIZER3A (velocityFunction fjoldi velocityVariation))
	(write-channels-slot OBJECT-ORGANIZER3A (channelsFunction fjoldi))

	(save-to-file OBJECT-ORGANIZER3A myname mypath)
)

(ultimate)