      ******************************************************************
      * Author: EMILIANO TOMASI
      * Date: 20/10/2023
      * Purpose: CLASE 20 - EJERCICIO 3
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL20EJ03.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT ENT-EMPLEADOS
           ASSIGN TO '../EMPLEADOS.VSAM'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           FILE STATUS IS FS-EMPLEADOS
           RECORD KEY IS ENT-EMP-ID-EMPLEADO.

       SELECT ENT-DATOS-EXTRA
           ASSIGN TO '../DATOS-EXTRA.VSAM'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           FILE STATUS IS FS-DATOS-EXTRA
           RECORD KEY IS ENT-EXT-ID-EMPLEADO.

       SELECT SAL-JOIN
           ASSIGN TO '../SAL-JOIN.VSAM'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           FILE STATUS IS FS-SAL-JOIN
           RECORD KEY IS SAL-EMP-ID-EMPLEADO.



      *----------------------------------------------------------------*
       DATA DIVISION.

       FILE SECTION.

       FD ENT-EMPLEADOS.
       01 ENT-EMPLEADOS-REG.
          05 ENT-EMP-ID-EMPLEADO            PIC 9(08).
          05 ENT-EMP-APELLIDO               PIC X(25).
          05 ENT-EMP-NOMBRE                 PIC X(25).
          05 ENT-EMP-ESTADO                 PIC X(01).
          05 ENT-EMP-DIRECCION              PIC X(50).
          05 ENT-EMP-COD-POSTAL             PIC 9(04).

       FD ENT-DATOS-EXTRA.
       01 ENT-DATOS-EXTRA-REG.
          05 ENT-EXT-ID-EMPLEADO            PIC 9(08).
          05 ENT-EXT-TIPO-DOC               PIC X(03).
          05 ENT-EXT-NRO-DOC                PIC X(08).
          05 ENT-EXT-TELEFONO               PIC X(09).

       FD SAL-JOIN.
       01 SAL-JOIN-REG.
          05 SAL-EMP-ID-EMPLEADO            PIC 9(08).
          05 SAL-EMP-APELLIDO               PIC X(25).
          05 SAL-EMP-NOMBRE                 PIC X(25).
          05 SAL-EMP-ESTADO                 PIC X(01).
          05 SAL-EMP-DIRECCION              PIC X(50).
          05 SAL-EMP-COD-POSTAL             PIC 9(04).
          05 SAL-EXT-TIPO-DOC               PIC X(03).
          05 SAL-EXT-NRO-DOC                PIC X(08).
          05 SAL-EXT-TELEFONO               PIC X(09).



       WORKING-STORAGE SECTION.

       01 FS-STATUS.
          05 FS-EMPLEADOS                   PIC X(2).
             88 FS-EMPLEADOS-FILE-OK            VALUE '00'.
             88 FS-EMPLEADOS-FILE-EOF           VALUE '10'.
             88 FS-EMPLEADOS-FILE-NFD           VALUE '35'.
             88 FS-EMPLEADOS-CLAVE-INV          VALUE '21'.
             88 FS-EMPLEADOS-CLAVE-DUP          VALUE '22'.
             88 FS-EMPLEADOS-CLAVE-NFD          VALUE '23'.
          05 FS-DATOS-EXTRA                 PIC X(2).
             88 FS-DATOS-EXTRA-FILE-OK          VALUE '00'.
             88 FS-DATOS-EXTRA-FILE-EOF         VALUE '10'.
             88 FS-DATOS-EXTRA-FILE-NFD         VALUE '35'.
             88 FS-DATOS-EXTRA-CLAVE-INV        VALUE '21'.
             88 FS-DATOS-EXTRA-CLAVE-DUP        VALUE '22'.
             88 FS-DATOS-EXTRA-CLAVE-NFD        VALUE '23'.
          05 FS-SAL-JOIN                PIC X(2).
             88 FS-SAL-FILE-OK            VALUE '00'.
             88 FS-SAL-FILE-EOF           VALUE '10'.
             88 FS-SAL-FILE-NFD           VALUE '35'.
             88 FS-SAL-CLAVE-INV          VALUE '21'.
             88 FS-SAL-CLAVE-DUP          VALUE '22'.
             88 FS-SAL-CLAVE-NFD          VALUE '23'.

       77 WS-ID-EMPLEADO                    PIC 9(08).
       77 WS-CONT-REG-SAL             PIC 9(04) VALUE 0.
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

           IF FS-EMPLEADOS-FILE-OK AND FS-DATOS-EXTRA-FILE-OK AND
             FS-SAL-FILE-OK

               DISPLAY 'INGRESA UN ID-EMPLEADO:'
               ACCEPT WS-ID-EMPLEADO
               DISPLAY " "

              PERFORM 2000-BUSCAR-EMPLEADO
                 THRU 2000-BUSCAR-EMPLEADO-FIN

           END-IF.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-FIN.

            STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.

           PERFORM 1100-ABRIR-EMPLEADOS
              THRU 1100-ABRIR-EMPLEADOS-FIN.

           PERFORM 1200-ABRIR-DIRECCIONES
              THRU 1200-ABRIR-DIRECCIONES-FIN.

           PERFORM 1300-ABRIR-SAL-JOIN
           THRU 1300-ABRIR-SAL-JOIN-FIN.

       1000-INICIAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1100-ABRIR-EMPLEADOS.

           OPEN INPUT ENT-EMPLEADOS.

           EVALUATE TRUE
               WHEN FS-EMPLEADOS-FILE-OK
                    CONTINUE
               WHEN FS-EMPLEADOS-FILE-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
           END-EVALUATE.

       1100-ABRIR-EMPLEADOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1200-ABRIR-DIRECCIONES.

           OPEN INPUT ENT-DATOS-EXTRA.

           EVALUATE FS-DATOS-EXTRA
               WHEN '00'
                    CONTINUE
               WHEN '35'
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE DATOS-EXTRA'
                    DISPLAY 'FILE STATUS: ' FS-DATOS-EXTRA
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE DATOS-EXTRA'
                    DISPLAY 'FILE STATUS: ' FS-DATOS-EXTRA
           END-EVALUATE.

       1200-ABRIR-DIRECCIONES-FIN.
           EXIT.
      *----------------------------------------------------------------*

       1300-ABRIR-SAL-JOIN.

           OPEN I-O SAL-JOIN.

           EVALUATE FS-SAL-JOIN
               WHEN '00'
                    CONTINUE
               WHEN '35'
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-SAL-JOIN
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-SAL-JOIN
           END-EVALUATE.

       1300-ABRIR-SAL-JOIN-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2000-BUSCAR-EMPLEADO.

           DISPLAY "--------------------------------------------------".

           MOVE WS-ID-EMPLEADO              TO ENT-EMP-ID-EMPLEADO.

           PERFORM 2100-LEER-EMPLEADOS
              THRU 2100-LEER-EMPLEADOS-FIN.

           IF (ENT-EMP-ESTADO EQUAL 'A')

               PERFORM 2105-MOSTRAR-EMPLEADO
                  THRU 2105-MOSTRAR-EMPLEADO-FIN

               MOVE WS-ID-EMPLEADO          TO ENT-EXT-ID-EMPLEADO

                  PERFORM 2200-LEER-DATOS-EXTRA
                  THRU 2200-LEER-DATOS-EXTRA-FIN

                  PERFORM 2205-MOSTRAR-DATOS-EXTRA
                  THRU 2205-MOSTRAR-DATOS-EXTRA-FIN



                   PERFORM 2220-MOVER-A-JOIN
                   THRU 2220-MOVER-A-JOIN-EXIT



                  PERFORM 2210-ESCRIBIR-SAL-JOIN
                  THRU 2210-ESCRIBIR-SAL-JOIN-FIN

           ELSE
                DISPLAY "EL EMPLEADO SE ENCUENTRA DADO DE BAJA"
           END-IF.
           DISPLAY "--------------------------------------------------".
       2000-BUSCAR-EMPLEADO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2100-LEER-EMPLEADOS.

           READ ENT-EMPLEADOS KEY IS ENT-EMP-ID-EMPLEADO.

           EVALUATE TRUE
               WHEN FS-EMPLEADOS-FILE-OK
                    CONTINUE
               WHEN FS-EMPLEADOS-CLAVE-INV
                   DISPLAY "ERROR: EL ID INGRESADO ES INVALIDO"
               WHEN FS-EMPLEADOS-CLAVE-DUP
                   DISPLAY "ERROR: EL ID INGRESADO SE ENCUENTRA "-
                           "DUPLICADO"
               WHEN FS-EMPLEADOS-CLAVE-NFD
                   DISPLAY "ERROR: EL ID INGRESADO NO EXISTE"
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
           END-EVALUATE.

       2100-LEER-EMPLEADOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2105-MOSTRAR-EMPLEADO.

           DISPLAY " ID EMPLEADO: " ENT-EMP-ID-EMPLEADO.
           DISPLAY " NOMBRE     : " ENT-EMP-NOMBRE.
           DISPLAY " APELLIDO   : " ENT-EMP-APELLIDO.
           DISPLAY " ESTADO     : " ENT-EMP-ESTADO.
           DISPLAY " DIRECCION  : " ENT-EMP-DIRECCION.
           DISPLAY " COD. POSTAL: " ENT-EMP-COD-POSTAL.

       2105-MOSTRAR-EMPLEADO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2200-LEER-DATOS-EXTRA.

           READ ENT-DATOS-EXTRA KEY IS ENT-EXT-ID-EMPLEADO.

           EVALUATE TRUE
               WHEN FS-DATOS-EXTRA-FILE-OK
                    CONTINUE
               WHEN FS-DATOS-EXTRA-CLAVE-INV
                    DISPLAY "ERROR: EL ID INGRESADO ES INVALIDO "-
                             "EN ARCHIVO DE DATOS EXTRA"
               WHEN FS-DATOS-EXTRA-CLAVE-DUP
                    DISPLAY "ERROR: EL ID INGRESADO SE ENCUENTRA "-
                            "DUPLICADO EN ARCHIVO DE DATOS EXTRA"
               WHEN FS-DATOS-EXTRA-CLAVE-NFD
                    DISPLAY "ERROR: NO POSEE DATOS EXTRA"
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
           END-EVALUATE.

       2200-LEER-DATOS-EXTRA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2205-MOSTRAR-DATOS-EXTRA.

           DISPLAY " DOCUMENT   : " ENT-EXT-TIPO-DOC
                  " " ENT-EXT-NRO-DOC.
           DISPLAY " TELEFONO   : " ENT-EXT-TELEFONO.

       2205-MOSTRAR-DATOS-EXTRA-FIN.
           EXIT.
      *----------------------------------------------------------------*

       2210-ESCRIBIR-SAL-JOIN.

           WRITE SAL-JOIN-REG

           EVALUATE TRUE
               WHEN FS-SAL-FILE-OK
                    ADD 1                   TO  WS-CONT-REG-SAL
               WHEN FS-SAL-CLAVE-DUP
                    DISPLAY 'EL EMPLADO YA EXISTE EN EL '-
                    'ARCHIVO RESULTADO'
              WHEN OTHER
                   DISPLAY 'ERROR AL ESCRIBIR RESULTADO.VSAM - '-
                  'FILE-STATUS: ' FS-SAL-JOIN ' - '-
                  'ID-EMPLEADO: ' SAL-EMP-ID-EMPLEADO
           END-EVALUATE.

       2210-ESCRIBIR-SAL-JOIN-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2220-MOVER-A-JOIN.

           MOVE ENT-EMP-ID-EMPLEADO TO SAL-EMP-ID-EMPLEADO.
           MOVE ENT-EMP-APELLIDO TO SAL-EMP-APELLIDO.
           MOVE ENT-EMP-NOMBRE TO SAL-EMP-NOMBRE.
           MOVE ENT-EMP-ESTADO TO SAL-EMP-ESTADO.
           MOVE ENT-EMP-DIRECCION TO SAL-EMP-DIRECCION.
           MOVE ENT-EMP-COD-POSTAL TO SAL-EMP-COD-POSTAL.
           MOVE ENT-EXT-TIPO-DOC TO SAL-EXT-TIPO-DOC.
           MOVE ENT-EXT-NRO-DOC TO SAL-EXT-NRO-DOC.
           MOVE ENT-EXT-TELEFONO TO SAL-EXT-TELEFONO.



       2220-MOVER-A-JOIN-EXIT.
           EXIT.
      *----------------------------------------------------------------*

       3000-FINALIZAR-PROGRAMA.

           PERFORM 3200-CERRAR-ARCHIVOS
              THRU 3200-CERRAR-ARCHIVOS-FIN.

           DISPLAY " ".
           DISPLAY '### FIN DEL PROGRAMA ###'.

       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVOS.

           CLOSE ENT-EMPLEADOS
                 ENT-DATOS-EXTRA
                 SAL-JOIN.

           IF NOT FS-EMPLEADOS-FILE-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO EMPLEADOS: ' FS-EMPLEADOS
           END-IF.

           IF NOT FS-DATOS-EXTRA-FILE-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO DATOS-EXTRA: '
                      FS-DATOS-EXTRA
           END-IF.

           IF NOT FS-SAL-FILE-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO DATOS-EXTRA: '
                      FS-SAL-JOIN
           END-IF.

       3200-CERRAR-ARCHIVOS-FIN.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL20EJ03.
