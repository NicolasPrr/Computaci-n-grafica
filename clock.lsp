;;Reloj realizado por Nicolás Parra Ramos
;Se puede encontrar el repositorio en la siguiente direccion
;  https://github.com/NicolasPrr/Computaci-n-grafica

;Definicion de las funciones breve

  ; init_var: Inicializa las principales variables como el centro del reloj,
  ;             el incrimento por cada segundo correspondiente, el numero de veces,
  ;             el contador inicial del while
  ; set_date:   Obtiene el tiempo del sistema y la asigna a cdate_val
  ; set_hour:   Obtiene la hora segun el tiempo del sistema la asigna a la variable hour
  ; set_minut : Obtiene los minutos segun el tiempo del sistema la asigna a la variable minut
  ; set_second: Obtiene los segundos segun el tiempo del sistema la asigna a la variable second
  ; sum_point(p1, x y): 
  ;             Suma las coordenadas X,Y  a un punto dado p1
  ; init_reference_lines: 
  ;             Dibuja las lineas de referencias, pone las referencias del horario y de los segundos
  ; init_lines: inicializa los bloques respectivos y los pone en la posicion segun el tiempo del sistema
  ; update_lines:
  ;             Actualiza los bloques segun el incremento asignado previamente 
  ; draw_circles:
  ;             Dibuja el circulo externo e interno
  ; c:do_clock: Funcion principal, permite llamar desde autocad, es la primera que se ejecuta(?)

(defun init_var ()
    ; Pedimos el centro del reloj para poder poner los circulos ya las demas bloques correspondientes
    

    (setq  center  (getpoint "center point"))
    ;car  => x
    ;cadr => y

    ;Sacamos los valores X y Y para los respectivos calculos
    (setq y_center (cadr center))
    (setq x_center (car center))

    ;Asignacion del radio para poder calcular las referencias (poli lineas)
    (setq radios 1000)

    ;Incrementos por segundo dependiendo si es el segundero, horario y minutero
    (setq increment_second (/ pi 30.0) )
    (setq increment_minut (/ pi 1800.0)  )
    (setq increment_hour  (/ pi 21600.0)  )

    ;Inicializacion de las variables
    ;Cuantas veces y desde cuando
    (setq i 0)
    (setq times 20)

)
(defun set_date ( ) 
    ;Obtiene la hora respectiva y la pone el cdate_val
    (setq cdate_val (rtos (getvar "CDATE") 2 6))
)
(defun set_hour ( )
  ;Obtiene la hora sacada de cdate_val y la asigna a hour
  (setq hour ( rem   (atof (substr cdate_val 10 2 )) 12) )
)
(defun set_minut ( )
  ;Asignacion del minuto segun cdate_val
  (setq minut (substr cdate_val 12 2))
)
(defun set_second ( )
  ;Asignacion del segundo actual segun cdate_val
  (setq second (substr cdate_val 14 2))
)
(defun sum_point (p1 x y)
  ;Funcion para  sumar un punto p1 con X y Y, retorna un nuevo punto p2.
  (setq px  (+ (car p1) x )) ;suma el x en el punto p1
  (setq py  (+ (cadr p1) y )) ;suma el y en el punto p1
  (setq p2  (list px py 0.0 ))
  p2
)
(defun init_reference_lines ( )

  ; Referencia de  lineas para las horas
  ; La primera  linea contiene el punto p1 y el punto p2
  (setq first_line_p1 (sum_point center 0 (* 0.8 radios))) ;  
  (setq first_line_p2 (sum_point center 0 (- radios 60) ))  
  
  ;Dibujado y asignacion de la linea 
  (command "._line" first_line_p1 first_line_p2  "" ) 
  (setq line_r_hour (entlast))(terpri)
  
  ;Array polar para ponerla a lo largo de la circunferencia (HORAS)
  (command "_.arraypolar" line_r_hour "" center 12 "" "x")
  
  ;Referencia de las lineas para los segundos
  (setq first_line_p1 (sum_point center 0 (* 0.93 radios))) ; 
  (setq first_line_p2 (sum_point center 0 (- radios 20) ))



  (command "._line" first_line_p1 first_line_p2  "" ) 
  (setq line_r_second (entlast))(terpri)
  (command "_.arraypolar" line_r_second "" center 60 "" "x") ; 60 veces


)
(defun init_lines ( )
  ;Init lines


    ;Asignacion de hora, segundos y minutos  
    (set_date)
    (set_second)
    (set_minut)
    (set_hour)
    
    
    ;Se inserta el bloque respectivo del segundero en la posicion respectiva
    (command "insert" "hand1" center "1" "1"   ( + (* -6 (atof  second)) 90 ))
    (setq line_second_properties (entget (entlast))) 

    ;Se inserta el bloque respectivo del minutero en la posicion respectiva
    (command "insert" "hand1" center "0.8" "1" ( +  (* -6 (atof  minut)) (/ (atof second) -10.0)  90  ))
    (setq line_minut_properties  (entget (entlast))) 

    ;Se inserta el bloque respectivo del horario en la posicion respectiva
    (command "insert" "hand1" center "0.5" "1" (+ (* -30 hour)  (/ (atof minut) -2.0)  (/ (atof second) -120.0) 90)  )
    (setq line_hour_properties   (entget (entlast)))


;atof parse to integer, float

)
(defun update_lines ( )

  ;newtime es la variable donde se almacenara el nuevo tiempo segun el actual,
  ;en el caso del segundo seria:
  ;newtime = tiempo_segundo + incremento_segundo
  
  ;lo mismo para el horario y el minutero

  ;; Segundos*******
  ;Asignacion del nuevo valor
  (setq newtime  ( - (cdr (nth 14 line_second_properties)) increment_second  ))
  
  ;Asignacion del nuevo line_second_properties
  (setq line_second_properties (subst (cons 50  newtime) (assoc 50  line_second_properties) line_second_properties ))
  
  ;Actualizacion de las propiedades
  (entmod line_second_properties)
  
  ;; Minutos ***********
  (setq newtime  ( - (cdr (nth 14 line_minut_properties)) increment_minut  ))
  (setq line_minut_properties (subst (cons 50  newtime) (assoc 50  line_minut_properties) line_minut_properties ))
  (entmod line_minut_properties)

  ;; Horas ************
  (setq newtime  ( - (cdr (nth 14 line_hour_properties)) increment_hour  ))
  (setq line_hour_properties (subst (cons 50  newtime) (assoc 50  line_hour_properties) line_hour_properties ))
  (entmod line_hour_properties)
 

)

(defun draw_circles ( )
  ;Dibuja las circulos segun el radio dado anteriormente
  
  (command "circle" center radios );Circulo interno
  (command "circle" center (sum_point center 0 (+ radios 120))  ) ;Circulo externo

  (setq p1 (sum_point center 0 (+ radios 10)))  ;Punto entre los dos circulos
  (command "-hatch" p1 "" "") ;Color segun el layer seleccionado
  
)
(defun c:do_clock ( )
  ;Corre el reloj
  (command "_osnap" "_off")

  (init_var) ;Inicializar variables
  (draw_circles) ;Dibujar circulos
  (init_reference_lines) ;Dibujar lineas de referencias
  (setq acd (vlax-get-acad-object))
  (init_lines) ;Inicializar las lineas, es decir que importa los bloques respectivos
  (while (< i times)
     (print "")
     (update_lines)
     (command  "_.delay" 700 )
     (vla-update acd)  
     (setq i (+ i 1)) 
  )   
)