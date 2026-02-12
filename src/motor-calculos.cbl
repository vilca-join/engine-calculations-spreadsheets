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
          05 FD-EMPLEADO-FECHA-INGRESO  PIC X(8).
          05 FD-EMPLEADO-SUELDO-BASE    PIC 9(7)V99.   

          05 FD-EMPLEADO-ASIG-FAMILIAR  PIC 9(1).
             88 FD-EMPLEADO-CON-ASIG-FAMILIAR       VALUE 1.
             88 FD-EMPLEADO-SIN-ASIG-FAMILIAR       VALUE 0. 
          05 FD-EMPLEADO-REG-PENSION    PIC 9(2).
             88 FD-EMPLEADO-REG-PENSION-ONP         VALUE 1.
             88 FD-EMPLEADO-REG-PENSION-AFP         VALUE 2.
             88 FD-EMPLEADO-REG-PENSION-OTRO        VALUE 3.                   
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
       01 WS-DISPLAY-MENSAJE-EXITO      PIC X(50)   VALUE
             "**CORRECTAMENTE GENERADO**".
       01 WS-DISPLAY-ERROR              PIC X(50)   VALUE
             "DATO INVALIDO".


       01 WS-EMPLEADO-ID-CONTADOR       PIC 9(4)    VALUE 0.        
       01 WS-FORMATO-ID-EMPLEADO.
          05 WS-ID                      PIC 9(4).  
          05 WS-FILLER                  PIC X(2)    VALUE "PL".
       
       01 WS-EMPLEADOS-REG.
          05 WS-EMPLEADO-ID             PIC X(6).    
          05 WS-NOMBRE-EMPLEADO         PIC X(20).
          05 WS-EMPLEADO-NOMBRE         PIC X(20).

          05 WS-EMPLEADO-FECHA-INGRESO.
             10 WS-EMPLEADO-INGRESO-DIA  PIC 9(2).
                88 WS-DIA-VALIDO                   VALUE 1 THRU 31.
             10 WS-EMPLEADO-INGRESO-MES  PIC 9(2).
                88 WS-MES-VALIDO                   VALUE 1 THRU 12.
             10 WS-EMPLEADO-INGRESO-ANIO PIC 9(4). 
                88 WS-ANIO-VALIDO                  VALUE 1900 THRU 2024.

          05 WS-EMPLEADO-SUELDO-BASE    PIC 9(7)V99.   
          05 WS-EMPLEADO-ASIG-FAMILIAR  PIC 9(1).
             88 WS-EMPLEADO-CON-ASIG-FAMILIAR       VALUE 1.
             88 WS-EMPLEADO-SIN-ASIG-FAMILIAR       VALUE 0. 
          05 WS-EMPLEADO-REG-PENSION    PIC 9(2).               
          05 WS-EMPLEADO-COMISION-AFP   PIC X(3).
             88 WS-EMPLEADO-COMISION-AFP-FLUJO      VALUE "F".
             88 WS-EMPLEADO-COMISION-AFP-MIXTO      VALUE "M".
          05 WS-EMPLEADO-CUSPP          PIC X(15).
       01 WS-FECHA-RANGOS.
          05 WS-FECHA-INGRESO-ANIO      PIC 9(4).
          05 WS-FECHA-INGRESO-MES       PIC 9(2).
          05 WS-FECHA-INGRESO-DIA       PIC 9(2).


       01 WS-FLAG.
          05 WS-FIN-ARCHIVO             PIC X(1)    VALUE "N".  
             88 FLAG-LEIDO                          VALUE "S".
             88 FLAG-NO-LEIDO                       VALUE "N".

       01 WS-PROCESO-INGRESAR-DATOS     PIC X(1)    VALUE "S".
          88 WS-INICIAR-PROCESO                     VALUE "S" "s". 
          88 WS-TERMINAR-PROCESO                    VALUE "N" "n". 

       01 WS-VALIDAR-NOMBRE             PIC X(1)    VALUE "N".
          88 WS-NOMBRE-VALIDO                       VALUE "S".
          88 WS-NOMBRE-INVALIDO                     VALUE "N".

       01 WS-VALIDAR-FECHA-INGRESO      PIC X(1)    VALUE "N".
          88 WS-FECHA-INGRESO-VALIDO                VALUE "S".
          88 WS-FECHA-INGRESO-INVALIDO              VALUE "N".
       01 WS-VALIDAR-SUELDO-BASE        PIC X(1)    VALUE "N".
          88 WS-SUELDO-BASE-VALIDO                  VALUE "S".
          88 WS-SUELDO-BASE-INVALIDO                VALUE "N".
       01 WS-VALIDAR-ASIG-FAMILIAR      PIC X(1)    VALUE "N".
          88 WS-ASIG-FAMILIAR-VALIDO                VALUE "S".
          88 WS-ASIG-FAMILIAR-INVALIDO              VALUE "N".
       01 WS-VALIDAR-REG-PENSION        PIC X(1)    VALUE "N".
          88 WS-REG-PENSION-VALIDO                  VALUE "S".
          88 WS-REG-PENSION-INVALIDO                VALUE "N".
       01 WS-VALIDAR-COMISION-AFP       PIC X(1)    VALUE "N".
          88 WS-COMISION-AFP-VALIDO                 VALUE "S".
          88 WS-COMISION-AFP-INVALIDO               VALUE "N".

       01 WS-VALIDAR-CUSPP              PIC X(1)    VALUE "N".
          88 WS-CUSPP-VALIDO                        VALUE "S".
          88 WS-CUSPP-INVALIDO                      VALUE "N".
       PROCEDURE DIVISION.
       
       000-INICIO.
           OPEN OUTPUT EMPLEADOS.
           PERFORM 100-INGRESAR-DATOS UNTIL WS-TERMINAR-PROCESO.
           CLOSE EMPLEADOS.
           STOP RUN.
       100-INGRESAR-DATOS.
           INITIALIZE WS-EMPLEADOS-REG
           INITIALIZE FD-EMPLEADO-REG.
           DISPLAY "GEMERANDO ID DE EMPLEADO...".   
           ADD 1 TO WS-EMPLEADO-ID-CONTADOR.

           MOVE WS-EMPLEADO-ID-CONTADOR TO WS-ID.
           MOVE WS-FORMATO-ID-EMPLEADO TO WS-EMPLEADO-ID.
           DISPLAY WS-DISPLAY-MENSAJE-EXITO.

           PERFORM 102-PROCESAR-NOMBRE.
           PERFORM 103-PROCESAR-FECHA-INGRESO.
           PERFORM 104-PROCESAR-SUELDO-BASE.
           PERFORM 105-PROCESAR-ASIG-FAMILIAR.
           PERFORM 106-PROCESAR-REG-PENSION.

           IF WS-EMPLEADO-REG-PENSION = 2
              PERFORM 107-PROCESAR-COMISION-AFP  
              PERFORM 108-PROCESAR-CUSPP                     
           ELSE 
              MOVE SPACE TO WS-EMPLEADO-COMISION-AFP
              MOVE SPACE TO WS-EMPLEADO-CUSPP
           END-IF.       
           
           PERFORM 200-MOVER-DATOS-AL-REGISTRO.
           WRITE FD-EMPLEADO-REG.
           DISPLAY WS-DISPLAY-MENSAJE-EXITO.
           DISPLAY "¿Desea ingresar otro empleado? (S/N): ".
           ACCEPT WS-PROCESO-INGRESAR-DATOS.
       102-PROCESAR-NOMBRE.
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
              DISPLAY WS-DISPLAY-ERROR
              SET WS-NOMBRE-INVALIDO TO TRUE
           END-IF.

       103-PROCESAR-FECHA-INGRESO.

           SET WS-FECHA-INGRESO-INVALIDO TO TRUE.
           PERFORM UNTIL WS-FECHA-INGRESO-VALIDO
                   DISPLAY WS-DISPLAY-INPUT-FECHA-INGRESO
                   DISPLAY "INGRESE DIA (DD):"
                   ACCEPT WS-FECHA-INGRESO-DIA
                   DISPLAY "INGRESE MES (MM):"
                   ACCEPT WS-FECHA-INGRESO-MES
                   DISPLAY "INGRESE ANIO (AAAA):"
                   ACCEPT WS-FECHA-INGRESO-ANIO
               
                   PERFORM 503-VALIDAR-FECHA-INGRESO
           END-PERFORM.
       503-VALIDAR-FECHA-INGRESO.
           
           SET WS-FECHA-INGRESO-VALIDO TO TRUE.
           *> VALIDAR FECHA DE INGRESO
           IF NOT (WS-DIA-VALIDO AND WS-MES-VALIDO AND WS-ANIO-VALIDO) 
               DISPLAY WS-DISPLAY-ERROR
              SET WS-FECHA-INGRESO-INVALIDO TO TRUE
           END-IF.

           IF WS-FECHA-INGRESO-DIA = ZERO OR
              WS-FECHA-INGRESO-MES = ZERO OR
              WS-FECHA-INGRESO-ANIO = ZERO
              DISPLAY WS-DISPLAY-ERROR
              SET WS-FECHA-INGRESO-INVALIDO TO TRUE
           END-IF.

           IF (WS-FECHA-INGRESO-MES = 04 OR
              WS-FECHA-INGRESO-MES = 06 OR 
              WS-FECHA-INGRESO-MES = 09 OR 
              WS-FECHA-INGRESO-MES = 11) AND WS-FECHA-INGRESO-DIA > 30
              DISPLAY WS-DISPLAY-ERROR
              SET WS-FECHA-INGRESO-INVALIDO TO TRUE
           END-IF.

           IF WS-FECHA-INGRESO-MES = 02
              IF FUNCTION MOD(WS-FECHA-INGRESO-ANIO , 4) = 0
                 IF WS-FECHA-INGRESO-DIA > 29
                    DISPLAY WS-DISPLAY-ERROR
                    SET WS-FECHA-INGRESO-INVALIDO TO TRUE
                 END-IF
              ELSE
                 IF WS-FECHA-INGRESO-DIA > 28
                    DISPLAY WS-DISPLAY-ERROR
                    SET WS-FECHA-INGRESO-INVALIDO TO TRUE
                 END-IF
              END-IF
           END-IF. 

           IF WS-EMPLEADO-FECHA-INGRESO IS NOT NUMERIC OR
              WS-EMPLEADO-FECHA-INGRESO = SPACE
              DISPLAY WS-DISPLAY-ERROR
              SET WS-FECHA-INGRESO-INVALIDO TO TRUE
           END-IF.       
       
       104-PROCESAR-SUELDO-BASE.
           SET WS-SUELDO-BASE-INVALIDO TO TRUE.
           PERFORM UNTIL WS-SUELDO-BASE-VALIDO
                   DISPLAY WS-DISPLAY-INPUT-SUELDO-BASE
                   ACCEPT WS-EMPLEADO-SUELDO-BASE               
                   PERFORM 504-VALIDAR-SUELDO-BASE
           END-PERFORM.      
       
       504-VALIDAR-SUELDO-BASE.
       
           SET WS-SUELDO-BASE-VALIDO TO TRUE.
           *> VALIDAR SUELDO BASE
           IF WS-EMPLEADO-SUELDO-BASE IS NOT NUMERIC OR
              WS-EMPLEADO-SUELDO-BASE = SPACE OR
              WS-EMPLEADO-SUELDO-BASE <= 0 OR
              WS-EMPLEADO-SUELDO-BASE = ZERO
              DISPLAY WS-DISPLAY-ERROR
              SET WS-SUELDO-BASE-INVALIDO TO TRUE
           END-IF.
      
       105-PROCESAR-ASIG-FAMILIAR.
           SET WS-ASIG-FAMILIAR-INVALIDO TO TRUE.
           PERFORM UNTIL WS-ASIG-FAMILIAR-VALIDO
                   DISPLAY WS-DISPLAY-INPUT-ASIG-FAMILIAR
                   ACCEPT WS-EMPLEADO-ASIG-FAMILIAR
               
                   PERFORM 505-VALIDAR-ASIG-FAMILIAR
           END-PERFORM.
       505-VALIDAR-ASIG-FAMILIAR.

           SET WS-ASIG-FAMILIAR-VALIDO TO TRUE.
           *> VALIDAR ASIGNACION FAMILIAR
           IF WS-EMPLEADO-ASIG-FAMILIAR IS NOT = 1 AND
              WS-EMPLEADO-ASIG-FAMILIAR IS NOT = 0 AND
              WS-EMPLEADO-ASIG-FAMILIAR IS NOT = SPACE
              DISPLAY WS-DISPLAY-ERROR
              SET WS-ASIG-FAMILIAR-INVALIDO TO TRUE
           END-IF.

       106-PROCESAR-REG-PENSION.
           SET WS-REG-PENSION-INVALIDO TO TRUE.
           PERFORM UNTIL WS-REG-PENSION-VALIDO
                   DISPLAY WS-DISPLAY-INPUT-REG-PENSION
                   ACCEPT WS-EMPLEADO-REG-PENSION
               
                   PERFORM 506-VALIDAR-REG-PENSION
           END-PERFORM.
       506-VALIDAR-REG-PENSION.

           SET WS-REG-PENSION-VALIDO TO TRUE.
           *> VALIDAR REGIMEN DE PENSION
           IF WS-EMPLEADO-REG-PENSION IS NOT = 1 AND
              WS-EMPLEADO-REG-PENSION IS NOT = 2 AND
              WS-EMPLEADO-REG-PENSION IS NOT = 3 AND
              WS-EMPLEADO-REG-PENSION IS NOT = SPACE
              DISPLAY WS-DISPLAY-ERROR
              SET WS-REG-PENSION-INVALIDO TO TRUE
           END-IF.

       107-PROCESAR-COMISION-AFP.
           SET WS-COMISION-AFP-INVALIDO TO TRUE.
           PERFORM UNTIL WS-COMISION-AFP-VALIDO
                   DISPLAY WS-DISPLAY-INPUT-COMISION-AFP
                   ACCEPT WS-EMPLEADO-COMISION-AFP
               
                   PERFORM 507-VALIDAR-COMISION-AFP
           END-PERFORM.
       507-VALIDAR-COMISION-AFP.
           
           SET WS-COMISION-AFP-VALIDO TO TRUE.
           *> VALIDAR COMISION AFP
           IF WS-EMPLEADO-COMISION-AFP IS NOT = "F" AND
              WS-EMPLEADO-COMISION-AFP IS NOT = "M" AND
              WS-EMPLEADO-COMISION-AFP IS NOT = SPACE
              DISPLAY WS-DISPLAY-ERROR
              SET WS-COMISION-AFP-INVALIDO TO TRUE
           END-IF.

       108-PROCESAR-CUSPP.
           SET WS-CUSPP-INVALIDO TO TRUE.
           PERFORM UNTIL WS-CUSPP-VALIDO
                   DISPLAY WS-DISPLAY-INPUT-CUSPP
                   ACCEPT WS-EMPLEADO-CUSPP
               
                   PERFORM 508-VALIDAR-CUSPP
           END-PERFORM.
       508-VALIDAR-CUSPP.
           SET WS-CUSPP-VALIDO TO TRUE.
           *> En COBOL validamos que el campo X no sea espacios
           IF WS-EMPLEADO-CUSPP = SPACES OR
              WS-EMPLEADO-CUSPP = LOW-VALUES
              DISPLAY WS-DISPLAY-ERROR
              SET WS-CUSPP-INVALIDO TO TRUE
           END-IF.
           
       200-MOVER-DATOS-AL-REGISTRO.
           MOVE WS-EMPLEADO-ID TO FD-EMPLEADO-ID.
           MOVE WS-NOMBRE-EMPLEADO TO FD-EMPLEADO-NOMBRE.
           MOVE WS-EMPLEADO-FECHA-INGRESO TO FD-EMPLEADO-FECHA-INGRESO.
           MOVE WS-EMPLEADO-SUELDO-BASE TO FD-EMPLEADO-SUELDO-BASE.
           MOVE WS-EMPLEADO-ASIG-FAMILIAR TO FD-EMPLEADO-ASIG-FAMILIAR.
           MOVE WS-EMPLEADO-REG-PENSION TO FD-EMPLEADO-REG-PENSION.
           MOVE WS-EMPLEADO-COMISION-AFP TO FD-EMPLEADO-COMISION-AFP.
           MOVE WS-EMPLEADO-CUSPP TO FD-EMPLEADO-CUSPP.