(defun c:example ()
;; R - Radio
;; H - Size text
;; N - Text
;; "" - Enter

    (setq R 100)
    (setq H 100)
    (setq N 1)
    (WHILE  
        (setq p1 (getpoint "Click on the centers: "))
        (command "CIRCLE" p1 R)
        
        ;; TYPE idk idk center size rotation text 
        (command "TEXT" "j" "M" p1  H 0  N "")
        (setq N (+ N 1))
    )

)