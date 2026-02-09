       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOTOR-CALCULOS.
       AUTHOR. JOSE VILCA.

       ENVIRONMENT DIVISION.
       FILE-CONTROL.
           SELECT EMPLEADOS ASSIGN TO "../data/empleados.dat"
               ORGANIZATION IS LINE SEQUENCE.               

       DATA DIVISION.
       
       FILE SECTION.
       FD  EMPLEADOS.
       01  FD-EMPLEADO-REG.
           05  FD-EMPLEADO-ID        PIC 9(5).
           05  FD-EMPLEADO-NOMBRE    PIC X(20).
           05  FD-EMPLEADO-FECHA-INGRESO  PIC X(20).
           05  FD-EMPLEADO-SUELDO-BASE   PIC 9(7)V99.

           05  FD-EMPLEADO-ASIG-FAMILIAR PIC 9(1) value 0.
               88  FD-EMPLEADO-CON-ASIG-FAMILIAR VALUE 1.
               88  FD-EMPLEADO-SIN-ASIG-FAMILIAR VALUE 0.
           05  FD-EMPLEADO-REG-PENSION   PIC 9(2).               
           05  FD-EMPLEADO-COMISION-AFP     PIC X(3) VALUE "M".
               88 FD-EMPLEADO-COMISION-AFP-FLUJO VALUE "F".
               88 FD-EMPLEADO-COMISION-AFP-MIXTO VALUE "M".
           
           05  FD-EMPLEADO-CUSPP          PIC X(15).
       WORKING-STORAGE.
           01  WS-FLAG.
               05  WS-FIN-ARCHIVO PIC X(1) VALUE "N".  
                   88  FLAG-LEIDO VALUE "S".
                   88  FLAG-NO-LEIDO VALUE "N". 
