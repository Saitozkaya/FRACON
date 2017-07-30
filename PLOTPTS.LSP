;************************************************************* 
;PLOTPTS.LSP
;************************************************************* 
;program to generate WELL BASE MAPS
; with options to print well id at all or above or below well 
;symbol and draw various well symbols, colors and sizes
;
;programmer sait Ismail Ozkaya
;Platform: ACAD 2000
;jULY 2005
;
; This program expects two files. A parameter input file "PTPLOT.INP"
; and a data file. The name of the data file is in the first row of
; of the bubble.inp file. An example of PLOTPTS.inp file is shown below:
;cutmn.prn
;"UTMN-"
;100
;m
;310000
;2760000
;344000
;2810000
;1
;2
;3
;4
;5
;6
;0.3
;5000
;1
;0.5
;;
;
;rows
;1  data file name
;2  scale
;3  unit
;4  xorigin
;5  yorigin
;6  xrite
;7  yup
;8  east utmn coordinate
;9  north utmn coordinate
;10 well id column
;11 polygon side (0 draws a circle)
;12 polygon size
;13 polygon color
;14 letter size
;11 letter size
;13 width between utm lines
;14 option >0 
;15 scale for well symbols

;The  data file for this program must be as follows
; EW UTM    NS UTM    Well NO side  size color
;-----------------------------------------------
;339347.64	2799430.48	196	0	1	1
;335843.86	2793233.47	216	0	1	1
;338824.21	2801482.04	469	0	1	1
;332393.99	2795086.23	557	0	1	1
;319948.79	2798439.33	586	0	1	1
;331693.12	2800466.47	590	0	1	1
;332836.00	2789597.00	593	0	1	1
;329117.24	2794330.93	594	0	1	1
;331978.00	2797744.00	595	0	1	1
;332620.24	2794563.78	596	0	1	1
;328426.53	2800059.16	597	0	1	1
;333461.00	2791583.84	598	0	1	1
;329799.91	2791950.38	599	0	1	1
;328939.09	2797141.04	600	0	1	1
;323370.00	2792104.00	623	0	1	1
;328577.00	2802933.00	627	0	1	1
;337556.00	2790325.50	634	0	1	1
;308040.00	2763646.00	1997	0	1	1
;326010.31	2794324.38	527	0	1	1
;.....
;endfile
; sides is the number of polygon  sides. 0 is circle
;***********************************************************
;************************************************************
  (defun PLOTPTS (  /  &rfile &dogu &kuzey &elev 
  &dosya &kuyukodu &bbscale
  &scale &artmalik &birim &xcol &ycol &column 
  &infile &linecnt &xg &xor &yor &xrt &yup &option
  &mazgal &nline &polsize &err &numberlist &polside &polrenk
  &polsidecol &polrenkcol &polsizecol
)

;***********************************************************
 
 ;************************************************************
 (defun CLS (/ pl pu)
 ;***********************************************************
 ;***procedure to clear the AutoCAD drawing area
 ;***********************************************************
 ;pl,pu   :lower left and upper right corners of drawing area
 ;***********************************************************
  (setvar "cmdecho" 0)
  (command "vpoint" "0,0,0")
  (command "erase" "all" "")
  (princ)
 );end cls...

 (Defun PORTRAIT ( /  )
 ;***********************************************************
 ;***procedure to set the screen limits to A4 size portrait
 ;orientation...
 ;***********************************************************
 (setvar "cmdecho" 0)
 (COMMAND "LIMITS" "-2,-2"  "17.8,26.5")
 (COMMAND "GRID" 1)
 (COMMAND "SNAP" 0.2)
 (command "snap" "OFF")
 (command "zoom" "a")
 (princ)
 (princ)
 );END portrait
 ;***********************************************************
 (Defun LANDSCAPE ( / )
 ;***********************************************************
 ;***procedure to set the screen limits to A4 size landscape
 ;orientation.
 ;***********************************************************
 (setvar "cmdecho" 0)
 (COMMAND "LIMITS" "-2.0,-2.0"  "26.5,17.8")
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
        (princ "Warning - Non-numeric character...")
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

 
 ;***********************************************************
 (defun OKUBOTH ( / x y z pciz ydown zz)
 ;***********************************************************

 ;**********************************************************

  ;(PRINT &DOSYA)
  ;(GETREAL)  
  (setq  &rfile (open  &dosya  "r"))
  (readit)
   
  (while (and &xg (/= (strcase &xg T) "endfile"))
   (procure)
   ; (print &numberlist)
   ; (getreal)
   (if (/= &numberlist nil)
    (progn
     (setq lnum (length &numberlist))
     
     (if (> lnum &xcol)
      (setq
       &dogu (nth &xcol &numberlist)
       &dogu (- &dogu &xor)
      );end setq..
     ;else
      (progn
       (setq
        &dogu 0
       );end setq
       (terpri)
       (princ "Warning-number of data columns less than expected..")
       (princ)
      );end progn..
     );endif..

     (if (> lnum &ycol)
      (setq
       &kuzey  (nth &ycol &numberlist)
       &kuzey (- &kuzey &yor)
      );end setq..
     ;else
      (progn
       (terpri)
       (princ "Warning-number of data columns less than expected..")
       (princ)
       (setq
        &kuzey 0
       );end setq
      );end progn..
     );endif..

;(PRINT "CHECKPT1")
;(GETREAL)

     (if (< &column lnum)
      (setq
       &elev (nth &column &numberlist)
       &elev (fix &elev)
      
      ); end setq..
     ;else
      (progn
       (setq
        &elev 0
       );end setq..
       (terpri)
       (princ "Warning-number of data columns less than expected..")
       (princ)
      );end progn..
     );end if..


;(PRINT "CHECKPT2")
;(GETREAL)


;//////////////////
(if (< &polsidecol lnum)
      (setq
       &polside (nth &polsidecol &numberlist)
       &polside (fix &polside)

      ); end setq..
     ;else
      (progn
       (setq
        &polside 0
       );end setq..
       (terpri)
       (princ "Warning-number of data columns less than expected..")
       (princ)
      );end progn..
     );end if..


;(PRINT "CHECKPT3")
;(GETREAL)



;///////////
(if  (< &polsizecol lnum)
      (setq
       &polsize (nth &polsizecol &numberlist)
       &polsize (* &polsize &bbscale)
      ); end setq..
     ;else
      (progn
       (setq
        &polsize 1
       );end setq..
       (terpri)
       (princ "Warning-number of data columns less than expected..")
       (princ)
      );end progn..
     );end if..

;///////////////////////////////

;(PRINT "CHECKPT4")
;(GETREAL)


(if  (< &polrenkcol lnum)
      (setq
       &polrenk (nth &polrenkcol &numberlist)
       &polrenk (fix &polrenk)
      ); end setq..
     ;else
      (progn
       (setq
        &polrenk 7
       );end setq..
       (terpri)
       (princ "Warning-number of data columns less than expected..")
       (princ)
      );end progn..
     );end if..
   );end progn..
  );end if..

;///////////////////////////

;(PRINT "CHECKPT5")
;(GETREAL)



; ///////////////
    (setq

      x &dogu
      y &kuzey
      z &elev
      zz &polsize
      zz (/ zz &scale)
      ydown (* 2 zz)
     
      x (/ x &scale)
      y (/ y &scale)
      pciz (list x y)
      zyaz (rtos z 2 0)
      zyaz (strcat &kuyukodu zyaz)
    ); end setq..
    
 
    (command "color" &polrenk)

      (if (< &polside 3)
         (command "circle" pciz zz)
       );endif
     
      (if (> &polside 2)
         (command "polygon" &polside pciz "I" zz)
      );end if

    (command "color" 7)
    

    (if (> &option 0)
     (progn

        (setq
          y (- y ydown)
          pciz (list x y)
         );end setq..

   

       (if (> &option 1)
       
          (setq
          y (+ y ydown)
          y (+ y ydown)
          pciz (list x y)
         );end setq..
       );end if..


     (command "text" "j" "m" pciz &artmalik "0" zyaz)
  
    );end progn..    
   );end if     




;///////////////////////////////


   (readit)
  );end while..




    (close &rfile )
 );end okuboth..
 

 
 



;*********************************************************
(defun DRAWFRAME ( / xr yu p1p2 p3 xpos yup ydown utm  ypos
 xleft xrite ptext ptx pty )

;Draw a frame around the contour map using the ll lr lu ru
;points scale and coordinates of the origin (l

  
 (setq 
   xr  (/ (- &xrt &xor) &scale)
   yu  (/ (- &yup &yor) &scale)
 );end setq..

 (command "color" 7)

 (setq
  ydown 0
  xpos 0
  yups (/ (- &yup &yor) &scale)
  utm &xor
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
   pty (- ydown (* 2 &artmalik))
   ptext (list ptx pty)
  );end setq..
 
 (command "text"  ptext  &artmalik  "0" (rtos &xor 2 0) )

 (setq
   ptx  (- xpos (* 2 &artmalik))
   pty  ydown 
   ptext (list ptx pty)
  );end setq..

 (command "text"  ptext  &artmalik  "90" (rtos &yor 2 0) )

(setq
    xpos (+ xpos (/ &mazgal &scale))
    utm (+ utm &mazgal)

  );end setq..

 (while (<= xpos xr)
  (setq
     p1 (list xpos  ydown)
     p2 (list xpos  yups)
  );end setq..
  (command "line" p1 p2 "")

  
  (setq
     pty (- ydown (* 2 &artmalik))
     ptx xpos
     ptext (list ptx pty)
     
  );end setq..


  (command "text" "j" "c"  ptext  &artmalik  "0" (rtos utm 2 0) )
  

  (setq
    xpos (+ xpos (/ &mazgal &scale))
    utm (+ utm &mazgal)

  );end setq..

 ); end while..


 (setq
  xleft 0
  xrite (/ (- &xrt &xor) &scale)
  ypos 0
  utm &yor
 );

 (setq
    ypos (+ ypos (/ &mazgal &scale))
    utm (+ utm &mazgal)
  );end setq..

 (while (<= ypos yu)
  (setq
     p1 (list xleft ypos )
     p2 (list xrite ypos)
  );end setq..

  (command "line" p1 p2 "")

  (setq
     ptx (- xleft (* 2 &artmalik))
     pty ypos
     ptext (list ptx pty)
     
  );end setq..

  (command "text"  "j" "c" ptext  &artmalik  "90" (rtos utm 2 0) )

  (setq
    ypos (+ ypos (/ &mazgal &scale))
    utm (+ utm &mazgal)
  );end setq..
 ); end while..

);end drawframe
;**********************************************************

(defun OKUINP ( / denek )
    
  (setq &infile "plotpts.inp")
  (setq denek (findfile &infile))
  (if (= denek nil)
   (progn
    (terpri)
    (princ "No file..")
    (princ)
    (princ)
   );end progn..
  ;else
   (progn
    (cls)
    (setq &linecnt 0)
    (setq &rfile (open &infile "r"))
    (readit)
    (setq &dosya &xg)
   ; (print &dosya)
    (readit)
     (setq &kuyukodu &xg)	
   ; (print &kuyukodu)
    (readit)
    (procure)
    (setq &scale (nth 0 &numberlist))
   ; (print &scale)
    
    (readit)
    (setq &birim &xg)
   ; (print "birim is ")
   ; (print &birim)

    (readit)
    (procure)
    (setq &xor (nth 0 &numberlist))
   ; (print &xor)
    (readit)
    (procure)
    (setq &yor (nth 0 &numberlist))
   ; (print &yor)
    (readit)
    (procure)
    (setq &xrt (nth 0 &numberlist)) 
   ;  (print &xrt)
    (readit)
    (procure)
    (setq &yup (nth 0 &numberlist))
   ; (print &yup)
     
    (readit)
    (procure)
    (setq &xcol (nth 0 &numberlist))
   ; (print &xcol)
    (readit)
    (procure)
    (setq &ycol (nth 0 &numberlist))
   ; (print &ycol)
    (readit)
    (procure)
    (setq &column (nth 0 &numberlist))
    
   ; (print &column)
  
    (readit)
    (procure)

    (setq &polsidecol (nth 0 &numberlist))
    (setq &polsidecol (fix &polsidecol))
   ; (print &polsidecol)
    (readit)

    (procure)

    (setq &polsizecol (nth 0 &numberlist))
    (setq &polsizecol (fix &polsizecol))

   ; (print &polsizecol)
  
    (readit)
    (procure)
    (setq &polrenkcol (nth 0 &numberlist))
    (setq &polrenkcol (fix &polrenkcol))
   ;  (print &polrenkcol)

    (readit)
    (procure)
    (setq &artmalik (nth 0 &numberlist))
   ; (print &artmalik)

    
    
    (readit)
    (procure)
    (setq &mazgal (nth 0 &numberlist))
   ; (print &mazgal)

    (readit)
    (procure)
    (setq &option (nth 0 &numberlist))
    (setq &option (fix &option))
   ; (print &option)

    (readit)
    (procure)
    (setq &bbscale (nth 0 &numberlist))
   ; (print &bbscale)


   
   
    (setq
      &column (1- &column)
      &xcol (1- &xcol)
      &ycol (1- &ycol)
      &polsidecol (1- &polsidecol)
      &polrenkcol (1- &polrenkcol)
      &polsizecol (1- &polsizecol)
      &column (fix &column)
      &xcol (fix &xcol)
      &ycol (fix &ycol)


    );end setq..

    
    (close &rfile)
   );end progn..
  );end if..
 ); end okuinp


 ;*********************************************************
 ;PLOT BASE MAIN..
 ;*********************************************************
   (setvar "cmdecho" 0)
   (setq &nline 1)
   (cls)
   (landscape)
   (font)
   (okuinp)
   (okuboth)
   (drawframe)
   (command "zoom" "a")
   (princ)
   (princ)
 );end PLOTPTS
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
