;*******************************************************************
;
;DRAWCON
;Program by I. ozkaya
;Platform: AutoLISP
;Date: MAY 2017
;
;*******************************************************************
;This program is used to generate a contour map.

;The example input file is shown below.
;ARBD_TOP.prn
;1000
;1800
;100
;55
;259
;181500
;2715500
;231500
;2845500
;100
;2
;0
;10000
;0
;   
;Columns
;1  filename
;2  mincon
;3  maxcon
;4  contint
;5  imax
;6  jmax
;7  xor
;8 yor
;9 xrite
;10 yup
;11 scale
;12 artmalik text size
;13 notat (-1 no text written)
;14 mazgal -distance between utmn lines
;15 frameflg flag to draw frame with UTM divisions


;************************************************************

;************************************************************
 (defun SILGI (/ pl pu)
;***********************************************************
;***procedure to clear the AutoCAD drawing area
;***********************************************************
;pl,pu   :lower left and upper right corners of drawing area
;***********************************************************
  (setvar "cmdecho" 0)
  (command "vpoint" "0,0,0")
  (command "erase" "all" "")
  (command "zoom" "all")
  (princ)
 );end cls...
 ;***********************************************************
 (Defun LANDSCAPE ( / )
 ;***********************************************************
 ;***procedure to set the screen limits to A4 size landscape
 ;orientation.
 ;***********************************************************
 (setvar "cmdecho" 0)
 (COMMAND "LIMITS" " -2.0,-2.0"  "26.5,17.8")
 (COMMAND "GRID" 1)
 (COMMAND "SNAP" 0.2)
 (command "snap" "OFF")
 (command "zoom" "a")
 (princ)
 (princ)
 );END landscape
 ;***********************************************************
 (Defun PORTRAIT ( / )
 ;***********************************************************
 ;***procedure to set the screen limits to A4 size landscape
 ;orientation.
 ;***********************************************************
 (setvar "cmdecho" 0)
 (COMMAND "LIMITS" " -2.0,-2.0"  "17.8,26.5")
 (COMMAND "GRID" 1)
 (COMMAND "SNAP" 0.2)
 (command "snap" "OFF")
 (command "zoom" "a")
 (princ)
 (princ)
 );END landscape
 ;************************************************************
 (defun FONT ()
 ;************************************************************
 ;***procedure to set font..
 ;***********************************************************
  (setvar "cmdecho" 0)
  (command "style" "standard" "romant" "0.0" "1.0" "0.0" "N" "N" "N")
  );end font
 ;***********************************************************
 ;START DRAWCON
 ;************************************************************
  (defun DRAWCON ( /  *dogu *kuzey *elev *column
   *dogulist *kuzeylist *elevlist *artmalik *yanit
   *sagx *solx *usty *alty  *notat *xcol *ycol
   *mincon *maxcon *intercon *confile *nxcol *nycol *nzcol
   *scale *frameflag *imax *jmax *xorigin *yorigin *xrite *yup
   *frameflag *x *y *z *findflag *xmid *ymid *renk *labelflag
   *tip1 *tip2
   &numberlist  &xg  &rfile &ans 
   &err &dosya &denek &linecnt &nline &rootfile
   &altbay )
 ;************************************************************




 ;+++++++++++++++++++++++++++++++++++++++++++++++
 (defun readit ( / ilk n i mi ilkflag ilkebay)
 ;+++++++++++++++++++++++++++++++++++++++++++++++
 ;local variables
 ;ilk                :
 ;n                  :
 ;i                  :
 ;mi                 :
 ;ilkflag            :
 ;+++++++++++++++++++++++++++++++++++++++++++++++
  (defun elimbl (  /  ie ne ilke)
 ;+++++++++++++++++++++++++++++++++++++++++++++++
   (setq ilkebay -10)
   (if (/= &xg nil)
    (progn
     (setq
       ie 1
       ne (strlen &xg)
     );end setq..
     (while (and (<= ie ne) (< ilkebay 0))
      (setq
       ilke (substr &xg ie 1)
      );end setq
      (if (/= ilke " ")
       (setq
        &xg (substr &xg ie)
        ilkebay 10
       );end setq..
      );end if..
      (setq ie (1+ ie))
     );end while..
    );end progn..
   );end if..
   (if (< ilkebay 0)
    (setq
      &linecnt (1+ &linecnt)
    );end setq..
   );end if..
   (if (> &linecnt &nline)
    (progn
      (terpri)
      (princ "Unexpected end of file or no endfile mark..")
      (setq &err (/ 1 nil))
    );end progn
  );end if..
 );end elimbl..
 ;+++++++++++++++++++++++++++++++++
  (setq
   &xg (read-line &rfile)
  );end setq..
  (elimbl)
  (if (> ilkebay 0)
   (setq
    ilk (substr &xg 1 1)
   );end setq..
   ;else
    (setq ilk "#")
  );end if..
  (while  (= ilk "#")
   (setq
    &xg (read-line &rfile)
   );end setq..
   (elimbl)
   (if (> ilkebay 0)
    (setq
     ilk (substr &xg 1 1)
    );end setq..
   ;else
    (setq ilk "#")
   );end if..
  );end while..
    (setq
      n (strlen &xg)
      i n
      ilkflag -10
      mi n
    );end setq
    (while ( and (> i 0) (< ilkflag 0))
       (setq
         ilk (substr &xg i 1)
      );end setq..
      (if (/= ilk " ")
        (setq
          ilkflag 10
          mi i
        );end setq..
       );end if..
       (setq
         i (1- i)
      );end setq..
    ); end while..
    (setq &xg (substr &xg 1 mi))
    (setq &xg (strcase &xg T))
 );end readit..
 ;***********************************************************
 (defun PROCURE ( /  ip np alfa number harf lnum)
 ;***********************************************************
  (setq
    &numberlist nil
    ip 1
    np (strlen &xg)
    alfa " "
    number nil
    harf (substr &xg 1 1)
  );end setq..
  (while (<= ip np)
   (while ( and (<= ip np) (= harf " "))
     (setq
       ip (1+ ip)
       harf (substr &xg ip 1)
      );end setq..
   );end while..
   (if (<= ip np)
   (progn
    (while (and (<= ip np)
               (/= harf " ")
               (/= harf ",")
          );end and..
     (setq
     sayi (ascii harf)
     );end setq..
     (if ( and (/= sayi 45) (/= sayi 46))
      (if (or (< sayi 48) (> sayi 57))
       (progn
        (terpri)
;       (princ "Warning - Non-numeric character...")
       );end progn..
      );end if..
     );end if (/= sayi 45)
     (setq
      alfa (strcat alfa harf)
      ip (1+ ip)
      harf (substr &xg ip 1)
     );end setq..
    ); end while..
    (setq
     number (atof alfa)
     alfa " "
     &numberlist (cons number &numberlist)
     ip (1+ ip)
     harf (substr &xg ip 1)
    );end setq..
   );end progn..
   );end if..
  );end while (<= ip np..
  (if (/= &numberlist nil)
   (progn
   (setq &numberlist (reverse &numberlist))
   );end progn..
  );end if..
 );end procure..

 ;******************************************************
 (defun DOSYABELIRT (   /  )
 ;******************************************************
  (terpri)
  (setq fadi "drawcon.inp")
  (setq fadibul (findfile fadi))
   
  (if (/= fadibul nil)
   (progn
    

    (setq &rfile (open fadi "r"))

    
    (readit)
    (setq *confile &xg)
    
    (readit)
    (procure)
    (setq *mincon (nth 0 &numberlist))
    (readit)
    (procure)
    (setq *maxcon (nth 0 &numberlist))
    (readit)
    (procure)
    (setq *intercon (nth 0 &numberlist))

       
    (setq *ncols 3)
    (setq *nxcol 1) 
    (setq *nycol 2)
    (setq *nzcol 3)
    (readit)
    (procure)
    (setq *imax (nth 0 &numberlist))
    (setq *imax (fix *imax))
    (readit)
    (procure)
    (setq *jmax (nth 0 &numberlist))
    (setq *jmax (fix *jmax))
     
  ; read origin left and right edges
    (readit)
    (procure)
    (setq *xorigin (nth 0 &numberlist))
    (readit)
    (procure)
    (setq *yorigin (nth 0 &numberlist))
    (readit)
    (procure)
    (setq *xrite (nth 0 &numberlist))
    (readit)
    (procure)
    (setq *yup  (nth 0 &numberlist))
    (readit)
    (procure)
    (setq *scale (nth 0 &numberlist))
    
    (readit)
    (procure)
    (setq *artmalik (nth 0 &numberlist))
    (readit)
    (procure)
    (setq *notat (nth 0 &numberlist))
    
    (readit)
    (procure)
    (setq *mazgal (nth 0 &numberlist))

    (readit)
    (procure)
    (setq *frameflg (nth 0 &numberlist))
    (setq *frameflg (fix *frameflg))



    (close &rfile)
   );end progn..
   ;else
   (progn
     (print "drawbase.inp file missing")
     (terpri)
     (setq &err ( / 1 0 ))
    );end progn
   );end if..
 );end dosyabelirt...
 ;*************************************************************
 (defun OKUBOTH ( /  numpts lnum kalist nsum)
 ;***********************************************************
 ;***********************************************************

   
  (setq kalist nil)
 ; (readit)
 ; (procure)
 ; (setq numpts (nth 0 &numberlist))
  (readit)
  (while (and &xg (/= (strcase &xg T) "endfile"))
 ;..........................
   (procure)
   (setq
     lnum (length &numberlist)
     kalist &numberlist
     nsum lnum
   );end setq..
   (while (< nsum *ncols)
    (readit)
     (procure)
    (setq
      kalist (append kalist &numberlist)
      lnum (length &numberlist)
      nsum (+ nsum lnum)
    );end setq..
   );end while..
 
 
   (setq
     &numberlist kalist
     kalist nil
   );end setq..
 
 
   (if (/= &numberlist nil)
    (progn
       (setq lnum (length &numberlist))
       (if (> lnum *xcol)
        (setq
         *dogu (nth *xcol &numberlist)
         *dogu (- *dogu *xorigin)
	 *dogu (/ *dogu *scale)
        );end setq..
       
       ;else
       (progn
         (setq
          *dogu 0
         );end setq
         (terpri)
         (princ "Warning-number of data columns less than expected..")
         (princ)
       );end progn..
      );endif..

     
       (if (> lnum *ycol)
        (setq
         *kuzey  (nth *ycol &numberlist)
         *kuzey (- *kuzey *yorigin)
	 *kuzey (/ *kuzey *scale)
        );end setq..
       ;else
        (progn
         (terpri)
         (princ "Warning-number of data columns less than expected..")
         (princ)
         (setq
          *kuzey 0
         );end setq
        );end progn..
       );endif..


       
       	  
       	  
         (setq
          *dogulist (cons *dogu *dogulist)
          *kuzeylist (cons *kuzey *kuzeylist)
         );end setq...
       
      
       
      (if (< *column lnum)
       (setq *elev (nth *column &numberlist))	
      ;else
       (progn
        (setq
         *elev 0
        );end setq..
        (terpri)
        (princ "Warning-number of data columns less than expected..")
        (princ)
       );end progn..
      );end if..

       	  
        (setq
         *elevlist (cons *elev *elevlist)
        );end setq...
    
    );end progn 
   );end if
       
 
   (readit)
  );end while..
   
  (setq
   *dogulist (reverse *dogulist)
   *kuzeylist  (reverse *kuzeylist)
  );end setq..
   (setq
    *elevlist (reverse *elevlist)
   );end setq..
   
   
   
	  
   (close &rfile)
 );end okuboth..


 ;************************************************************
  (defun topludok ( / conval i j  k ip1 jp1 ip2 jp2 nulval tip1 tip2)



 ;*********************************************************

 


 (setq *renk 0)  
(setq conval *mincon)
(while (<= conval *maxcon)
 
   ; operations for drawing contour
   ;
   ;find coordinated and elevation for given indices i and j
   ; scan grid left to right top  to bottom

   (print conval)
   (setq *findflag 0)
   (setq *labelflag 0) 
   (setq *renk (1+ *renk))
   (setq j 1)
   (while (< j *jmax)  
    (setq i 1)
    (while (< i *imax)      
      (setq *findflag 0)
      
      (setq ip1 i)
      (setq jp1 j)
      (setq ip2 (1+ i)) 
      (setq jp2 j)

     
      
      (finofin ip1 ip2 jp1 jp2 conval nulval)
      

       (setq 
         ip1 i 
         jp1 j
         ip2 i
         jp2 (1+ j)
       );end setq 

       (finofin ip1 ip2 jp1 jp2 conval nulval)


 
          (setq 
             ip1 i
             jp1 (1+ j)
             ip2 (1+ i)
             jp2 (1+ j)
          );end setq
 
          ( finofin ip1  ip2  jp1  jp2   conval  nullval)
      
 
      


         (setq
            ip1 (1+ i)
            jp1 j
            ip2 (1+ i)
           jp2 (1+ j)
         ); end setq
  

        (finofin ip1 ip2 jp1 jp2 conval nulval)

       (setq i (1+ i))
    );end while

    (setq j (1+ j))
   );end while
  (setq conval (+ conval *intercon))
 );end while

 );end topludok..

;********************************************************
                

(defun finofin (ip1 ip2 jp1 jp2 conval nullval / z1 z2 x1 y1 x2 y2 rnum pnt ptx pty ptext xdis)
          
 (corfin ip1 jp1  nulval)
 (setq z1 *z)
 (setq x1 *x)
 (setq y1 *y) 

 (corfin ip2 jp2  nulval)
 (setq z2 *z)
 (setq x2 *x)
 (setq y2 *y)

 
  
 
 (setq *xmid nulval)
 (setq *ymid nulval)
  
 


  

  


  
(If 
   (and
     (> z1   nulval)
   ;and
     (> z2  nulval)
   ); end and

 
  ;......

    (if 
      (or 
        (and
           (>= conval z1)
         ;and
           (<= conval z2)
        ); end and
      ;or   
        (and
          (>= conval z2)
       ;and
          (<= conval z1)
        ); end and
       ); end or
        (progn  
         
	  (setq *findflag (1+ *findflag ))
	 
         (command "color" *renk)
	 (setq *xmid (+ x1 (* (/ (- x2 x1) (- z2 z1) ) (- conval z1))    )      )
	 (setq *ymid (+ y1 (* (/ (- y2 y1) (- z2 z1) ) (- conval z1)) ))
         (setq pnt (list *xmid *ymid))
;	 (command "point" pnt)
;///////////////////////////////////////
	  (if (= *findflag  1)
	     (setq *tip1 pnt)
	   );endif

	  (if (= *findflag 2)
	   (progn
	     (setq *tip2 pnt)
           (setq xdis (distance *tip1 *tip2))
           (if (> xdis 0.0001)
            (progn
	         (command "line" *tip1 *tip2 "")
	         (setq *findflag 0)
            ); end progn
           );end if 
	   );end progn  
          );end if
;///////////////////////////////////////////
      
         (if (= *labelflag 0)
	   (progn
	      (setq
                 ptx  *xmid
                 pty (- *ymid (* 2 *artmalik))
                 ptext (list ptx pty)
              );end setq..
 
              (command "text"  ptext  *artmalik  "0" (rtos conval 2 0) )
             (setq *labelflag 1)
           );end progn
	  ); and if 
	 
        );end progn
       );endif
    );end if
     
);end finofin

;*********************************************************
              
               
(defun corfin (i j nulval / rnum)

; calculate row number



  
  (if 
   (or
     (> i *imax)
   ;or
    (> j  *jmax)
   );end or
  (progn
     
 (setq 
   *x nulval
   *y nulval
   *z nulval
   rnum nulval
 ); end setq
 );end progn 
;else
 (progn 
  (setq rnum (1- (+  (* (1- j) *imax) i) ))

 
  (setq *x (nth rnum *dogulist))
  (setq *y (nth rnum *kuzeylist))
  (setq *z (nth rnum *elevlist))
   
 
 
); end progn
);end if
  
); end  corfin





;*********************************************************
(defun DRAWFRAME ( / xr yu p1p2 p3 xpos yup ydown utm  ypos
 xleft xrite ptext ptx pty )

;Draw a frame around the contour map using the ll lr lu ru
;points scale and coordinates of the origin (l

 (setq 
   xr  (/ (- *xrite *xorigin) *scale)
   yu  (/ (- *yup *yorigin) *scale)
 );end setq..

 (command "color" 7)

 (setq
  ydown 0
  xpos 0
  yups (/ (- *yup *yorigin) *scale)
  utm *xorigin
  p1 (list xr ydown)
  p2 (list xr yups)

 );
 
(command "line" p1 p2 "")

 (setq 
   p1 (list xpos yups)
   p2 (list xr yups)
 ); end setq..

 (command "line" p1 p2 "")
 
 (setq 
   p1 (list xpos ydown)
   p2 (list xpos yups)
  ); end setq..


(command "line" p1 p2 "")

 (setq
 
  p1 (list xpos ydown)
  p2 (list xr ydown)

 );
 

(command "line" p1 p2 "")


 (setq
   ptx  xpos
   pty (- ydown
(*  *artmalik))
   ptext (list ptx pty)
  );end setq..
 
 (command "text"  ptext  *artmalik  "0" (rtos *xorigin 2 0) )

 (setq
   ptx  (- xpos (* 2 *artmalik))
   pty  ydown 
   ptext (list ptx pty)
  );end setq..

 (command "text"  ptext  *artmalik  "90" (rtos *yorigin 2 0) )

(setq
    xpos (+ xpos (/ *mazgal *scale))
    utm (+ utm *mazgal)

  );end setq..

 (while (<= xpos xr)
  (setq
     p1 (list xpos  ydown)
     p2 (list xpos  yups)
  );end setq..
  (command "line" p1 p2 "")

  
  (setq
     pty (- ydown (* 2 *artmalik))
     ptx xpos
     ptext (list ptx pty)
     
  );end setq..


  (command "text" "j" "c"  ptext  *artmalik  "0" (rtos utm 2 0) )

  

  (setq
    xpos (+ xpos (/ *mazgal *scale))
    utm (+ utm *mazgal)

  );end setq..

 ); end while..


 (setq
  xleft 0
  xrite (/ (- *xrite *xorigin) *scale)
  ypos 0
  utm *yorigin
 );



 (setq
    ypos (+ ypos (/ *mazgal *scale))
    utm (+ utm *mazgal)
  );end setq..

 (while (<= ypos yu)
  (setq
     p1 (list xleft ypos )
     p2 (list xrite ypos)
  );end setq..

  (command "line" p1 p2 "")

  (setq
     ptx (- xleft (* 2 *artmalik))
     pty ypos
     ptext (list ptx pty)
     
  );end setq..

  (command "text"  "j" "c" ptext  *artmalik  "90" (rtos utm 2 0) )

  (setq
    ypos (+ ypos (/ *mazgal *scale))
    utm (+ utm *mazgal)
  );end setq..
 ); end while..

);end drawframe




 ;************************************************************
  (defun getdosya ( /       binek sctx piti kiti xfr yfr gelen
   baslik xciz yciz pciz pilya ans)
 ;************************************************************
  (setq &dosya *confile )
  (setq &denek (findfile &dosya))
  (if (= &denek nil)
   (progn
    (terpri)
    (princ "No file..")
    (princ)
    (princ)
    (setq &err ( / 1 0))
   );end progn..
  );end if..
  (setq &rfile (open &dosya "r"))
  (setq *xcol *nxcol)
  (setq *ycol *nycol)
  (setq *column *nzcol)
  (setq
     *column (1- *column)
     *xcol (1- *xcol)
     *ycol (1- *ycol)
     *column (fix *column)
     *xcol (fix *xcol)
     *ycol (fix *ycol)
   );end setq..
 );end getscale





;DRAWCON MAIN..
 ;*********************************************************
  (setq &nline 1000)
  (landscape)
  (silgi)
  (font)
  
  (dosyabelirt)
   
  (getdosya)
  
  (if (> *frameflag 0)
   (drawframe)
  );end if..
  (okuboth)
 
  (topludok  )
  (command "zoom" "all")
 );end drawcon..

