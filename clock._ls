(defun init_var ()
    (setq  center  (getpoint "center point"))
    ;car  => x
    ;cadr => y
    (setq y_center (cadr center))
    (setq x_center (car center))
    (setq radios 1000)

    ;longitud de las lineas
    (setq second_lenght (* radios 0.6))
    (setq minut_lenght  (* radios 0.7))
    (setq hour_lenght   (* radios 0.4))
    
    ;;p p1 second is the center
    (setq p2_second (sum_point center 0 second_lenght))
    (setq p2_minut (sum_point center 0 minut_lenght))
    (setq p2_hour (sum_point center 0 hour_lenght))

    (setq increment_second -6 )
    (setq increment_minut (/ -1 10)  )
    (setq increment_hour  (/ -1 120)  )

)
(defun set_date ( ) 
    (setq cdate_val (rtos (getvar "CDATE") 2 6))
)
(defun set_hour ( )
  (setq hour ( rem   (atof (substr cdate_val 10 2 )) 12) )
)
(defun set_minut ( )
  (setq minut (substr cdate_val 12 2))
)
(defun set_second ( )
  (setq second (substr cdate_val 14 2))
)
(defun sum_point (p1 x y)
  (setq px  (+ (car p1) x )) ;suma el x en el punto p1
  (setq py  (+ (cadr p1) y )) ;suma el y en el punto p1
  (setq p2  (list px py 0.0 ))
  p2
)
(defun init_lines ( )
  ;Init lines

    (command "._line" center p2_hour  "" ) 
    (setq line_hour (entlast)) 
    
    (command "._line" center p2_minut "" ) 
    (setq line_minut (entlast)) 
    
    (command "._line" center p2_second "" )
    (setq line_second (entlast)) 

    (set_date)
    (set_hour)
    (set_minut)
    (set_second)

    ;atof parse to integer
;init rotations 
    (command "rotate" line_hour    ""  center   (+ (* -30 hour)  (/ (atof minut) -2)  (/ (atof second) -120) ) )
    (command "rotate" line_minut   ""  center   ( +  (* -6 (atof  minut)) (/ (atof second) -10))) ;; 6*minuts + seconds/10
    (command "rotate" line_second  ""  center   (* -6 (atof  second)) )
;init colors
    (command "change" line_hour ""   "properties" "color" 1 "") ;rojo
    (command "change" line_minut ""  "properties" "color" 2 "") ;amarillo
    (command "change" line_second "" "properties" "color" 3 "") ;verde

)
(defun update_lines ( )
  (command "rotate" line_hour    ""  center   increment_hour   )
  (command "rotate" line_minut   ""  center   increment_minut  ) 
  (command "rotate" line_second  ""  center   increment_second )

)
(defun c:do_clock ( )
  (init_var)
  (command "circle" center radios )
  (init_lines)
  (while
    ;; (update_lines)
    (print "test")
    ;; (command "_.delay" 1000)  
  )
    


  
    ;; (command "rotate" line_second ""   center "40" "")
    ;; (command "rotate" line_minut   ""  center "70" "")
    
)