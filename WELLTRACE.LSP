;***********************************************************
;WELLTRACE
;**********************************************************
;Program to draw wellbore traces as polylines
;Programmer: Ozkaya
;Date: July 2000
;**********************************************************
;Prepare WELLTRACE.INP file. It should look like this 
;yeniceri.flt
;1000
;3172000
;965000
;3190000
;985000
;1
;0.1
;2000

;
;rows
;1  data file name
;2  scale
;3  xorigin
;4  yorigin
;5  xrite
;6  yup
;7  color option >1 each contour has a different color
;8  text size for plotting contour values
;9  width between utm lines
;
;**********************************************************
;The input data file has the following format:
;             3177038.00         973683.50       1
;             3177375.00         973557.50       1
;             3177693.00         973343.80       1
;             3178802.00         974306.10       2
;             3178935.00         973995.90       2
;             3179077.00         973699.60       2
;             3179201.00         973495.70       2
;             3179624.00         972990.40       2
;             3179739.00         972892.90       2
;             3174901.00         970801.60       3
;             3175159.00         970588.10       3
;             3175519.00         970346.40       3
;             3175551.00         970304.70       3
;             3177203.00         971986.30       4
;             3177451.00         971638.80       4
;             3177699.00         971291.20       4
;             3177971.00         971068.40       4
;.....
;             3180831.00         972038.50      14
;             3181068.00         972181.00      14
;             3181290.00         972392.90      14
;             3181564.00         972590.70      14
;             3181967.00         972802.00      14
;             3182059.00         972880.30      14
;endfile            
;***********************************************************
;Each well trace consists of at least two  points
;***********************************************************

 (defun welltrace ( / &xg &numberlist &err  &mazgal
    &linecnt &mashfile &nline &xor &yor &scale &artmalik 
    &infile &coloropt)
 ;**********************************************************
 ;*********************************************************
  (Defun *error* (msg)
 ;*******************************************************
 ;***AutoCAD error routine...
 ;*********************************************************
    (terpri)
    (princ msg)
    (princ)
   );end error..
 ;*******************************************************
 (defun READIT ( / ilk n i mi ilkflag ilkebay)
 ;*******************************************************
 ;***Procedure to read a line from input text file..
 ;*******************************************************
 ;+++++++++++++++++++++++++++++++++++++++++++++++
  (defun elimbl (  /  ie ne ilke)
 ;+++++++++++++++++++++++++++++++++++++++++++++++
 ;***Subprocedure to eliminate blanks..
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
   &xg (read-line &mashfile)
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
    &xg (read-line &mashfile)
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
 ;*******************************************************
 (defun PROCURE ( /  ip np alfa number harf)
 ;*******************************************************
 ;***Procedure to parse input text line and extract
 ;number fields. &numberlist is the list of numbers...
 ;*******************************************************
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
        (terpri)
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
 (defun OKUCOR (   /  x1 y1 p1 ang len lenov2 p2 fno fnop renk md)
 ;******************************************************
 ;Procedure to read coordinates of joints of the network
 ;******************************************************
 (setq
   &linecnt 0
   &mashfile (open  &infile  "r")
 );end setq..
  (readit)
  (procure)
  (setq
    x1 (nth 0 &numberlist)
    y1 (nth 1 &numberlist)
    x1 (/ (- x1 &xor) &scale)
    y1 (/ (- y1 &yor) &scale)
    md (nth 2 &numberlist)
    fno  (fix (nth 3 &numberlist))
     p1 (list x1 y1)
  );end setq..
 

  (if (> &coloropt 0) 
   (progn
    (if (> renk 6) (setq renk 1))
    (setq renk (fix fno))
    (command "color" renk)
   );end progn..
  );end if..

  (command "pline" p1)
  (setq fnop fno)
  (readit)
   
 (while  (/= &xg "endfile")
   (procure)
   (setq
    x1 (nth 0 &numberlist)
    y1 (nth 1 &numberlist)
    x1 (/ (- x1 &xor) &scale)
    y1 (/ (- y1 &yor) &scale)

    md (nth 2 &numberlist)

    fno  (fix (nth 3 &numberlist))
    p1 (list x1 y1)
  );end setq...

   
   (if (/= fnop fno)
    (progn
     (command "")
;     (setq catlak (entlast))
;     (command "pedit" catlak "s" "")
      (if (> &coloropt 0) 
      (progn
       (if (> renk 6) (setq renk 1))
       (setq renk (fix fno))
       (command "color" renk)
      );end progn..
      );end if..
      (command "pline" p1)  
      (setq fnop fno)  
    );end progn..
   ;else
     (command p1)
   ); end if..

    (readit) 

  );end while..
  (command "")
;  (setq catlak (entlast))
;  (command "pedit" catlak "s" "")
  (close &mashfile)
  );end okucor..
;*********************************************************
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
   pty (- ydown
(* 2 &artmalik))
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
(defun OKUINP ( / )
    
  (setq &infile "welltrace.inp")

   
  (setq &mashfile (open &infile "r"))

  (readit)
  (setq &infile &xg)
   
  (readit)
  (procure)
  (setq &scale (nth 0 &numberlist))

  (readit)
  (procure)
  (setq &xor (nth 0 &numberlist))
  (readit)
  (procure)
  (setq &yor (nth 0 &numberlist)) 
  (readit)
   
  (procure)
  (setq &xrt (nth 0 &numberlist))
  
  (readit)
  (procure)
  (setq &yup (nth 0 &numberlist))
  
  (readit)
  (procure)
  (setq &coloropt (nth 0 &numberlist))
  (setq &coloropt (fix &coloropt))
  
  (readit)
  (procure)
  (setq &artmalik (nth 0 &numberlist))
  
  (readit)
  (procure)
  (setq &mazgal (nth 0 &numberlist))
    
  

  (close &mashfile)
 ); end okuinp
;***********************************************************
  (setvar "cmdecho" 0)
  (setq &nline 1000)
  (terpri)

  

  (okuinp)

  (setq cfile (findfile  &infile))

   (if (= cfile nil)
    (progn
     (terpri)
     (print "No file..")
     (princ)
     (setq &err (/ 1 0))
   );end progn
  );end if
   (drawframe)
   (okucor)
   (command "redraw")
 );end ..
 
 
 
 
 
 
 
 
 
 
 
 
 
 
