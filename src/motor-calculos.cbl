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
          05 FD-EMPLEADO-ID             PIC X(6). *> FORMATO 0001PL
          05 FD-EMPLEADO-NOMBRE         PIC X(20).
          05 FD-EMPLEADO-FECHA-INGRESO  PIC X(20).
          05 FD-EMPLEADO-SUELDO-BASE    PIC 9(7)V99.   

          05 FD-EMPLEADO-ASIG-FAMILIAR  PIC 9(1).
             88 FD-EMPLEADO-CON-ASIG-FAMILIAR       VALUE 1.
             88 FD-EMPLEADO-SIN-ASIG-FAMILIAR       VALUE 0. 
          05 FD-EMPLEADO-REG-PENSION    PIC 9(2).
             88 FD-EMPLEADO-REG-PENSION-ONP       VALUE 1.
             88 FD-EMPLEADO-REG-PENSION-AFP       VALUE 2.
             88 FD-EMPLEADO-REG-PENSION-OTRO       VALUE 3.                   
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
       01 WS-DISPLAY-ERROR PIC X(50) VALUE "DATO INVALIDO".


       01 WS-EMPLEADO-ID-CONTADOR       PIC 9(4)    VALUE 0.        
       01 WS-FORMATO-ID-EMPLEADO.
           05 WS-ID             PIC 9(4).  
           05 WS-FILLER                  PIC X(2)    VALUE "PL".
       
       01  WS-EMPLEADOS-REG.
           05  WS-EMPLEADO-ID  PIC X(6).    
           05  WS-NOMBRE-EMPLEADO           PIC X(20).
           05  WS-EMPLEADO-NOMBRE         PIC X(20).
           05  WS-EMPLEADO-FECHA-INGRESO  PIC X(20).
           05  WS-EMPLEADO-SUELDO-BASE    PIC 9(7)V99.   
           05  WS-EMPLEADO-ASIG-FAMILIAR  PIC 9(1).
               88 WS-EMPLEADO-CON-ASIG-FAMILIAR       VALUE 1.
               88 WS-EMPLEADO-SIN-ASIG-FAMILIAR       VALUE 0. 
           05  WS-EMPLEADO-REG-PENSION    PIC 9(2).               
           05  WS-EMPLEADO-COMISION-AFP   PIC X(3).
               88 WS-EMPLEADO-COMISION-AFP-FLUJO      VALUE "F".
               88 WS-EMPLEADO-COMISION-AFP-MIXTO      VALUE "M".
           05  WS-EMPLEADO-CUSPP          PIC X(15).
    


       01 WS-FLAG.
          05 WS-FIN-ARCHIVO             PIC X(1)    VALUE "N".  
             88 FLAG-LEIDO                          VALUE "S".
             88 FLAG-NO-LEIDO                       VALUE "N".

       01 WS-PROCESO-INGRESAR-DATOS     PIC X(1)    VALUE "S".
          88 WS-INICIAR-PROCESO                     VALUE "S" "s". 
          88 WS-TERMINAR-PROCESO                    VALUE "N" "n". 

       01  WS-VALIDAR-NOMBRE                     PIC X(1) VALUE "N".
           88 WS-NOMBRE-VALIDO                     VALUE "S".
           88 WS-NOMBRE-INVALIDO                  VALUE "N".

       01  WS-VALIDAR-FECHA-INGRESO              PIC X(1) VALUE "N".
           88 WS-FECHA-INGRESO-VALIDO              VALUE "S".
           88 WS-FECHA-INGRESO-INVALIDO            VALUE "N".
       01  WS-VALIDAR-SUELDO-BASE               PIC X(1) VALUE "N".
           88 WS-SUELDO-BASE-VALIDO               VALUE "S".
           88 WS-SUELDO-BASE-INVALIDO             VALUE "N".
       01  WS-VALIDAR-ASIG-FAMILIAR             PIC X(1) VALUE "N".
              88 WS-ASIG-FAMILIAR-VALIDO             VALUE "S".
              88 WS-ASIG-FAMILIAR-INVALIDO           VALUE "N".
       01  WS-VALIDAR-REG-PENSION               PIC X(1) VALUE "N".
           88 WS-REG-PENSION-VALIDO               VALUE "S".
           88 WS-REG-PENSION-INVALIDO             VALUE "N".
       01  WS-VALIDAR-COMISION-AFP              PIC X(1) VALUE "N".
           88 WS-COMISION-AFP-VALIDO              VALUE "S".
           88 WS-COMISION-AFP-INVALIDO            VALUE "N".

       01  WS-VALIDAR-CUSPP                   PIC X(1) VALUE "N".
              88 WS-CUSPP-VALIDO                   VALUE "S".
              88 WS-CUSPP-INVALIDO                 VALUE "N".
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

           MOVE WS-EMPLEADO-ID-CONTADOR TO WS-ID.
           MOVE WS-FORMATO-ID-EMPLEADO TO WS-EMPLEADO-ID.
           DISPLAY WS-DISPLAY-MENSAJE-EXITO.

           PERFORM 101-PROCESAR-NOMBRE.
           PERFORM 102-PROCESAR-FECHA-INGRESO.
           PERFORM 103-PROCESAR-SUELDO-BASE.
           PERFORM 104-PROCESAR-ASIG-FAMILIAR.
           PERFORM 105-PROCESAR-REG-PENSION.
           PERFORM 106-PROCESAR-COMISION-AFP.
           PERFORM 107-PROCESAR-CUSPP.
           
           

           IF WS-EMPLEADO-REG-PENSION = 2
              DISPLAY WS-DISPLAY-INPUT-COMISION-AFP
              ACCEPT WS-EMPLEADO-COMISION-AFP
              PERFORM 501-VALIDAR-DATOS UNTIL WS-COMISION-AFP-VALIDO
               DISPLAY WS-DISPLAY-INPUT-COMISION-AFP
               ACCEPT WS-EMPLEADO-COMISION-AFP
              END-PERFORM.

              DISPLAY WS-DISPLAY-INPUT-CUSPP
              ACCEPT WS-EMPLEADO-CUSPP
              PERFORM 501-VALIDAR-DATOS UNTIL WS-CUSPP-VALIDO
               DISPLAY WS-DISPLAY-INPUT-CUSPP
               ACCEPT WS-EMPLEADO-CUSPP
              END-PERFORM.          
           ELSE 
              MOVE SPACE TO WS-EMPLEADO-COMISION-AFP
              MOVE SPACE TO WS-EMPLEADO-CUSPP
           END-IF.
           
           DISPLAY "¿Desea ingresar otro empleado? (S/N): "
           ACCEPT WS-PROCESO-INGRESAR-DATOS.
       101-PROCESAR-NOMBRE.
           SET WS-NOMBRE-INVALIDO TO TRUE.
           
           *> El ciclo se repite hasta que la validación diga que es "S"
           PERFORM UNTIL WS-NOMBRE-VALIDO
               DISPLAY WS-DISPLAY-INPUT-NOMBRE
               ACCEPT WS-NOMBRE-EMPLEADO
               
               *> Llamamos a la lógica de validación
               PERFORM 502-VALIDAR-NOMBRE
           END-PERFORM.
       502-VALIDAR-NOMBRE.

           SET WS-NOMBRE-VALIDO TO TRUE.
           *> VALIDAR NOMBRE
           IF WS-NOMBRE-EMPLEADO = SPACE OR 
           WS-NOMBRE-EMPLEADO IS NUMERIC
               DISPLAY WS-DISPLAY-ERROR.
               SET WS-NOMBRE-INVALIDO TO TRUE
           END-IF.

       102-PROCESAR-FECHA-INGRESO.
           SET WS-FECHA-INGRESO-INVALIDO TO TRUE.
           PERFORM 
       503-VALIDAR-FECHA-INGRESO.
           
           SET WS-FECHA-INGRESO-VALIDO TO TRUE.
           *> VALIDAR FECHA DE INGRESO
           IF WS-EMPLEADO-FECHA-INGRESO IS NOT NUMERIC OR 
              WS-EMPLEADO-FECHA-INGRESO = SPACE
                DISPLAY WS-DISPLAY-ERROR.
                SET WS-FECHA-INGRESO-INVALIDO TO TRUE
           END-IF.
       504-VALIDAR-SUELDO-BASE.

           SET WS-SUELDO-BASE-VALIDO TO TRUE.
           *> VALIDAR SUELDO BASE
           IF WS-EMPLEADO-SUELDO-BASE IS NOT NUMERIC OR  
           WS-EMPLEADO-SUELDO-BASE = SPACE OR 
           WS-EMPLEADO-SUELDO-BASE <=0 OR 
           WS-EMPLEADO-SUELDO-BASE = ZERO
                DISPLAY WS-DISPLAY-ERROR.
                SET WS-SUELDO-BASE-INVALIDO TO TRUE
           END-IF.
       505-VALIDAR-ASIG-FAMILIAR.

           SET WS-ASIG-FAMILIAR-VALIDO TO TRUE.
           *> VALIDAR ASIGNACION FAMILIAR
           IF WS-EMPLEADO-ASIG-FAMILIAR IS NOT = 1 AND 
              WS-EMPLEADO-ASIG-FAMILIAR IS NOT = 0 AND
              WS-EMPLEADO-ASIG-FAMILIAR IS NOT = SPACE
                DISPLAY WS-DISPLAY-ERROR.
                SET WS-ASIG-FAMILIAR-INVALIDO TO TRUE
           END-IF.
       506-VALIDAR-REG-PENSION.

           SET WS-REG-PENSION-VALIDO TO TRUE.
           *> VALIDAR REGIMEN DE PENSION
           IF WS-EMPLEADO-REG-PENSION IS NOT = 1 AND 
              WS-EMPLEADO-REG-PENSION IS NOT = 2 AND  
              WS-EMPLEADO-REG-PENSION IS NOT = 3 AND  
              WS-EMPLEADO-REG-PENSION IS NOT = SPACE
                DISPLAY WS-DISPLAY-ERROR.
                SET WS-REG-PENSION-INVALIDO TO TRUE
           END-IF.
       507-VALIDAR-COMISION-AFP.
           
           SET WS-COMISION-AFP-VALIDO TO TRUE.
           *> VALIDAR COMISION AFP
           IF WS-EMPLEADO-COMISION-AFP IS NOT = "F" AND 
              WS-EMPLEADO-COMISION-AFP IS NOT = "M" AND 
              WS-EMPLEADO-COMISION-AFP IS NOT = SPACE
                DISPLAY WS-DISPLAY-ERROR.
                SET WS-COMISION-AFP-INVALIDO TO TRUE
           END-IF.
       508-VALIDAR-CUSPP.
           
           SET WS-CUSPP-VALIDO TO TRUE.
           *> VALIDAR CUSPP
           IF WS-EMPLEADO-CUSPP IS NOT ALPHANUMERIC OR 
              WS-EMPLEADO-CUSPP = SPACE
                DISPLAY WS-DISPLAY-ERROR.
                SET WS-CUSPP-INVALIDO TO TRUE
           END-IF.
           


          

