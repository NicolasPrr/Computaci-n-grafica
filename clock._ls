;;Reloj realizado por Nicolás Parra Ramos
;Se puede encontrar el repositorio en la siguiente direccion
;  https://github.com/NicolasPrr/Computaci-n-grafica

;Definicion de las funciones breve

; init_var: Inicializa las principales variables como el centro del reloj,
;           el radio, la longitud de las lineas de  reloj (minutero, horario, ...), los puntos principales 
;           de las lineas, y el incremento por cada segundo


(defun init_var ()
    (setq  center  (getpoint "center point"))
    ;car  => x
    ;cadr => y
    (setq y_center (cadr center))
    (setq x_center (car center))
    (setq radios 1000)

    ;longitud de las lineas
    (setq second_lenght (* radios 0.94))
    (setq minut_lenght  (* radios 0.83))
    (setq hour_lenght   (* radios 0.7))
    
    ;;p p1 second is the center
    (setq p2_second (sum_point center 0 second_lenght))
    (setq p2_minut (sum_point center 0 minut_lenght))
    (setq p2_hour (sum_point center 0 hour_lenght))

    (setq increment_second -6 )
    (setq increment_minut (/ -1 10.0)  )
    (setq increment_hour  (/ -1 120.0)  )

    (setq i 0)
    (setq times 30)

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


(defun init_reference_lines ( )

  ; Referencia de  lineas para las horas
  (setq first_line_p1 (sum_point center 0 (* 0.8 radios))) ; 
  (setq first_line_p2 (sum_point center 0 (- radios 60) ))
  
  (command "._line" first_line_p1 first_line_p2  "" ) 
  (setq line_r_hour (entlast))(terpri)
  
  (command "_.arraypolar" line_r_hour "" center 12 "" "x")
  
  ;Referencia de las lineas para los segundos
  (setq first_line_p1 (sum_point center 0 (* 0.93 radios))) ; 
  (setq first_line_p2 (sum_point center 0 (- radios 20) ))

  (command "._line" first_line_p1 first_line_p2  "" ) 
  (setq line_r_second (entlast))(terpri)
  (command "_.arraypolar" line_r_second "" center 60 "" "x")


)
(defun init_lines ( )
  (command "_osnap" "_off")
  ;Init lines
    
    (set_date)
    (set_second)
    (command "._line" center p2_second "" )
    (setq line_second (entlast)) 
    (command "rotate" line_second  ""  center   (* -6 (atof  second)) )
    (command "change" line_second "" "properties" "color" 3 "") ;verde

    (set_date)
    (set_second)
    (set_minut)
    
    (command "._line" center p2_minut "" ) 
    (setq line_minut (entlast)) 
    (command "rotate" line_minut   ""  center   ( +  (* -6 (atof  minut)) (/ (atof second) -10.0))) ;; 6*minuts + seconds/10
    (command "change" line_minut ""  "properties" "color" 2 "") ;amarillo
    
    (set_date)
    (set_second)
    (set_minut)
    (set_hour)

    (command "._line" center p2_hour  "" ) 
    (setq line_hour (entlast)) 
    (command "rotate" line_hour    ""  center   (+ (* -30 hour)  (/ (atof minut) -2.0)  (/ (atof second) -120.0) ) )
    (command "change" line_hour ""   "properties" "color" 1 "") ;rojo


    ;atof parse to integer
;init rotations 
;init colors

)
(defun update_lines ( )
  (command "rotate" line_hour    ""  center   increment_hour   )
  (command "rotate" line_minut   ""  center   increment_minut  ) 
  (command "rotate" line_second  ""  center   increment_second )
)
(defun draw_circles ( )
  (command "circle" center radios s)
  (command "circle" center (sum_point center 0 (+ radios 120))  )

  (setq p1 (sum_point center 0 (+ radios 10)))
  (command "-hatch" p1 "" "")

)
(defun c:do_clock ( )
  (init_var)
  (draw_circles)
  (init_lines)
  (init_reference_lines)
  (setq acd (vlax-get-acad-object))
  (while (< i times)
   ;;  ( print (strcat "\n Seconds: " second))
   ;;  (print "Holi")
     (print "")
     (update_lines)
     (command "_.delay" 1 )
     (vla-update acd)  
     (setq i (+ i 1)) 
  )   
)