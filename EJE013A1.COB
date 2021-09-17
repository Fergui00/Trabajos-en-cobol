******************************************************************
      * Author: FERNANDO
      * Date: 2021 09
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. EJE013A1.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT SALIDA            ASSIGN TO DISK 'DATOSPERSONASNUEVO'
                                  ORGANIZATION IS LINE SEQUENTIAL
                                  FILE STATUS IS WSS-FS-SALIDA.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.

       FD SALIDA.
       01 REG-SALIDA.
           05 PROVINCIA PIC X(23).
           05 CIUDAD PIC X(20).
           05 NOMBRE PIC X(25).


      *-----------------------
       WORKING-STORAGE SECTION.

       01 MENU PIC 9(1) VALUE 1.

       01 INGRESO.
           05 INGRESO-PROVINCIA PIC X(23).
           05 INGRESO-CIUDAD    PIC X(20).
           05 INGRESO-NOMBRE    PIC X(25).

       01 WSV-CONT-SALIDA           PIC S9(4)  COMP VALUE 0.
       01 WSS-FS-SALIDA            PIC X(02).
          88 WSS-FS-SALIDA-OK       VALUE '00'.
          88 WSS-FS-SALIDA-EOF      VALUE '10'.

      *-----------------------
       PROCEDURE DIVISION.
       00000-CONTROL.


           PERFORM 10000-ABRIR-ARCHIVO
           PERFORM 15000-GRABAR UNTIL MENU = 5
           DISPLAY 'SE GRABARON '  WSV-CONT-SALIDA   ' REGISTROS '
           PERFORM 30000-CERRAR-ARCHIVO
       STOP RUN.

       10000-ABRIR-ARCHIVO.
           OPEN OUTPUT SALIDA
           IF NOT WSS-FS-SALIDA-OK
             DISPLAY 'ERROR DE ARCHIVO DE SALIDA'
             DISPLAY 'FILE STATUS' WSS-FS-SALIDA.

       15000-GRABAR.
           PERFORM 5000-INGRESAR-PROVINCIA
           PERFORM 5100-INGRESAR-CIUDAD
           PERFORM 5200-INGRESAR-NOMBRE
           PERFORM 4000-MOVER-DATOS
           WRITE  REG-SALIDA.
           PERFORM 6000-MENU
           ADD 1 TO WSV-CONT-SALIDA.



       30000-CERRAR-ARCHIVO.
           CLOSE SALIDA
           IF NOT WSS-FS-SALIDA-OK
              DISPLAY 'ERROR DE ARCHIVO DE SALIDA'
            DISPLAY 'FILE STATUS' WSS-FS-SALIDA
           END-IF.

       4000-MOVER-DATOS.
           MOVE INGRESO-NOMBRE TO NOMBRE
           MOVE INGRESO-PROVINCIA  TO PROVINCIA
           MOVE INGRESO-CIUDAD TO CIUDAD.

       5000-INGRESAR-PROVINCIA.
           DISPLAY "INGRESAR PROVINCIA"
           ACCEPT INGRESO-PROVINCIA.

       5100-INGRESAR-CIUDAD.
           DISPLAY "INGRESAR CIUDAD"
           ACCEPT INGRESO-CIUDAD.

       5200-INGRESAR-NOMBRE.
           DISPLAY "INGRESAR NOMBRE"
           ACCEPT INGRESO-NOMBRE.

       6000-MENU.
           DISPLAY "INGRESAR MENU"
           ACCEPT MENU.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

