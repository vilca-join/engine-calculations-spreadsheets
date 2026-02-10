       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOTOR-CALCULOS.
       AUTHOR. JOSE VILCA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLEADOS ASSIGN TO "../data/empleados.dat"
               ORGANIZATION IS LINE SEQUENTIAL.              

       DATA DIVISION.
       
       FILE SECTION.
       FD  EMPLEADOS.
       01 FD-EMPLEADO-REG.
          05 FD-EMPLEADO-ID             PIC 9(6). *> FORMATO 0001PL
          05 FD-EMPLEADO-NOMBRE         PIC X(20).
          05 FD-EMPLEADO-FECHA-INGRESO  PIC X(20).
          05 FD-EMPLEADO-SUELDO-BASE    PIC 9(7)V99.   

          05 FD-EMPLEADO-ASIG-FAMILIAR  PIC 9(1).
             88 FD-EMPLEADO-CON-ASIG-FAMILIAR       VALUE 1.
             88 FD-EMPLEADO-SIN-ASIG-FAMILIAR       VALUE 0. 
          05 FD-EMPLEADO-REG-PENSION    PIC 9(2).               
          05 FD-EMPLEADO-COMISION-AFP   PIC X(3).
             88 FD-EMPLEADO-COMISION-AFP-FLUJO      VALUE "F".
             88 FD-EMPLEADO-COMISION-AFP-MIXTO      VALUE "M".
           
          05 FD-EMPLEADO-CUSPP          PIC X(15).
       WORKING-STORAGE SECTION.
       01 WS-DISPLAY-TITULO             PIC X(50)
                                                    VALUE
             "MOTOR DE CALCULOS DE PLANILLA".
       01 WS-DISPLAY-INPUT-NOMBRE       PIC X(50)
                                                    VALUE
             "INGRESE NOMBRE DEL EMPLEADO: ".
       01 WS-DISPLAY-INPUT-FECHA-INGRESO
                                        PIC X(50)
                                                    VALUE
             "INGRESE FECHA DE INGRESO (DD/MM/AAAA): ".
       01 WS-DISPLAY-INPUT-SUELDO-BASE  PIC X(35)
                                                    VALUE
             "INGRESE SUELDO BASE: ".
       01 WS-DISPLAY-INPUT-ASIG-FAMILIAR
                                        PIC X(35)
                                                    VALUE
             "TIENE ASIGNACION FAMILIAR? (S/N): ".
       01 WS-DISPLAY-INPUT-REG-PENSION  PIC X(50)   VALUE
             "INGRESE REGIMEN DE PENSION (1-ONP, 2-AFP): ".
       01 WS-DISPLAY-INPUT-COMISION-AFP PIC X(50)
                                                    VALUE
             "INGRESE TIPO DE COMISION AFP (F-FLUJO, M-MIXTO): ".
       01 WS-DISPLAY-INPUT-CUSPP        PIC X(50)
                                                    VALUE
             "INGRESE CODIGO CUSPP: ".
       01 WS-DISPLAY-MENSAJE-EXITO            PIC X(50)   VALUE
             "**CORRECTAMENTE GENERADO**".


       01 WS-EMPLEADO-ID-CONTADOR       PIC 9(4)    VALUE 0. 
       
       01 WS-FORMATO-ID-EMPLEADO        . 
       *> FORMATO PARA ID DE EMPLEADOS
          05 WS-EMPLEADO-ID             PIC 9(4).  
          *> FORMATO 0001PL
          05 WS-FILLER                  PIC X(2)    VALUE "PL".
       01 WS-FLAG.
          05 WS-FIN-ARCHIVO             PIC X(1)    VALUE "N".  
             88 FLAG-LEIDO                          VALUE "S".
             88 FLAG-NO-LEIDO                       VALUE "N".

       01 WS-PROCESO-INGRESAR-DATOS     PIC X(1)    VALUE "S".
          88 WS-INICIAR-PROCESO                     VALUE "S" "s". 
          88 WS-TERMINAR-PROCESO                    VALUE "N" "n". 
       PROCEDURE DIVISION.
       
       000-INICIO.
           OPEN OUTPUT EMPLEADOS.
           PERFORM 100-INGRESAR-DATOS UNTIL WS-TERMINAR-PROCESO.
           CLOSE EMPLEADOS.
           STOP RUN.
       100-INGRESAR-DATOS.

           INITIALIZE FD-EMPLEADO-REG.
           DISPLAY "GEMERANDO ID DE EMPLEADO...".   
           ADD 1 TO WS-EMPLEADO-ID-CONTADOR.

           MOVE WS-EMPLEADO-ID-CONTADOR TO WS-EMPLEADO-ID.
           MOVE WS-FORMATO-ID-EMPLEADO TO FD-EMPLEADO-ID.
           DISPLAY WS-DISPLAY-MENSAJE-EXITO.

           DISPLAY WS-DISPLAY-INPUT-NOMBRE.
           ACCEPT FD-EMPLEADO-NOMBRE. 
           DISPLAY WS-DISPLAY-INPUT-FECHA-INGRESO.
           ACCEPT FD-EMPLEADO-FECHA-INGRESO.
           DISPLAY WS-DISPLAY-INPUT-SUELDO-BASE.
           ACCEPT FD-EMPLEADO-SUELDO-BASE. 
           DISPLAY WS-DISPLAY-INPUT-ASIG-FAMILIAR.
           ACCEPT FD-EMPLEADO-ASIG-FAMILIAR.
           DISPLAY WS-DISPLAY-INPUT-REG-PENSION.
           ACCEPT FD-EMPLEADO-REG-PENSION.

           IF FD-EMPLEADO-REG-PENSION = 2
              DISPLAY WS-DISPLAY-INPUT-COMISION-AFP
              ACCEPT FD-EMPLEADO-COMISION-AFP
              DISPLAY WS-DISPLAY-INPUT-CUSPP
              ACCEPT FD-EMPLEADO-CUSPP          
           ELSE 
              MOVE SPACE TO FD-EMPLEADO-COMISION-AFP
              MOVE SPACE TO FD-EMPLEADO-CUSPP
           END-IF.
           WRITE FD-EMPLEADO-REG.
           DISPLAY "¿Desea ingresar otro empleado? (S/N): "
           ACCEPT WS-PROCESO-INGRESAR-DATOS.           