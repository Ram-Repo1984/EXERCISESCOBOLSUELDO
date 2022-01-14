      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 WSC-CONSTANTES.
               05 WSC-SENIORITY.
                   10 WSC-SENIOR PIC X(06) VALUE 'SENIOR'.
                   10 WSC-SEMISR PIC X(06) VALUE 'SEMISR'.
                   10 WSC-JUNIOR PIC X(06) VALUE 'JUNIOR'.
           01 WSC-SUELDOS.
               03 WSC-SENIORS      PIC 9(06) VALUE 200000.
               03 WSC-SEMI-SENIOR  PIC 9(06) VALUE 160000.
               03 WSC-JUNIORS      PIC 9(06) VALUE 110000.

           01 WSV-VARIABLES.
               05 WSV-POSTULANTES.
                   10 WSV-POSTU1.
                       15 WSV-NOMBRE1            PIC X(5) VALUE 'PEDRO'.
                       15 WSV-EXPERIENCIA1       PIC 9(02) VALUE 00.
                       15 WSV-SUELDO1            PIC 9(06).
                       15 WSV-SUELDO-ANUAL1      PIC 9(08).
                       15 WSV-BONO1              PIC 9(06).
                   10 WSV-POSTU2.
                       15 WSV-NOMBRE2            PIC X(5) VALUE 'SOFIA'.
                       15 WSV-EXPERIENCIA2       PIC 9(02) VALUE 02.
                       15 WSV-SUELDO2            PIC 9(06).
                       15 WSV-SUELDO-ANUAL2      PIC 9(08).
                       15 WSV-BONO2              PIC 9(06).
                   10 WSV-POSTU3.
                       15 WSV-NOMBRE3            PIC X(5) VALUE 'LALA'.
                       15 WSV-EXPERIENCIA3       PIC 9(02) VALUE 05.
                       15 WSV-SUELDO3            PIC 9(06).
                       15 WSV-SUELDO-ANUAL3      PIC 9(08).
                       15 WSV-BONO3              PIC 9(06).

               05 WSV-POSTULANTE-AUX.
                  10 WSV-NOMBRE-AUX         PIC X(5).
                  10 WSV-EXPERIENCIA-AUX    PIC 9(02).
                     88 WSS-EXP-JUNIOR      VALUE 0 1 2.
                     88 WSS-EXP-SEMISR      VALUE 3 4 5.
                  10 WSV-SUELDO-AUX         PIC 9(06).
                  10 WSV-SUELDO-ANUAL-AUX   PIC 9(08).
                  10 WSV-BONO-AUX           PIC 9(06).

       PROCEDURE DIVISION.

       00-CONTROL.
           PERFORM 10-INICIO.
           PERFORM 20-PROCESO.
       STOP RUN.
       00-CONTROL-END.

       10-INICIO.
           DISPLAY "----------------".
           INITIALIZE WSV-POSTULANTE-AUX.
       10-INICIO-END.
       EXIT.

       20-PROCESO.

           MOVE WSV-POSTU1 TO WSV-POSTULANTE-AUX.
           PERFORM 25-EVALUAR.
           PERFORM 30-CALCULAR-SUELDO.

           MOVE WSV-POSTU2 TO WSV-POSTULANTE-AUX.
           PERFORM 25-EVALUAR.
           PERFORM 30-CALCULAR-SUELDO.

           MOVE WSV-POSTU3 TO WSV-POSTULANTE-AUX.
           PERFORM 25-EVALUAR.
           PERFORM 30-CALCULAR-SUELDO.



       20-PROCESO-END.
       EXIT.

       25-EVALUAR.

           DISPLAY"-----------------------------------"

           EVALUATE TRUE

           WHEN WSS-EXP-JUNIOR
           MOVE WSC-JUNIORS TO WSV-SUELDO-AUX
           DISPLAY WSV-NOMBRE-AUX ' EL POSTULANTE ES ' WSC-JUNIOR
           DISPLAY "SUELDO DE " WSV-SUELDO-AUX

           WHEN WSS-EXP-SEMISR
           MOVE WSC-SEMI-SENIOR TO WSV-SUELDO-AUX
           DISPLAY WSV-NOMBRE-AUX ' EL POSTULANTE ES ' WSC-SEMISR
           DISPLAY "SUELDO DE " WSV-SUELDO-AUX

           WHEN OTHER
           MOVE WSC-SENIORS TO WSV-SUELDO-AUX
           DISPLAY WSV-NOMBRE-AUX " EL POSTULANTE ES " WSC-SENIOR
           DISPLAY " SUELDO DE " WSV-SUELDO-AUX

           END-EVALUATE.




       25-EVALUAR-END.
           EXIT.

       30-CALCULAR-SUELDO.

           MULTIPLY 12 BY WSV-SUELDO-AUX GIVING WSV-SUELDO-ANUAL-AUX

           EVALUATE WSV-EXPERIENCIA-AUX
           WHEN 0
               MOVE 0 TO WSV-BONO-AUX
           WHEN 1
               COMPUTE WSV-BONO-AUX = WSV-SUELDO-AUX * 150/100
           WHEN 2
           WHEN 3
               COMPUTE WSV-BONO-AUX = WSV-SUELDO-AUX * 200/100
           WHEN 4
           WHEN 5
           WHEN 6
               COMPUTE WSV-BONO-AUX = WSV-SUELDO-AUX * 250/100

           END-EVALUATE.

               DISPLAY "SU SUELDO ANUAL ES DE $ " WSV-SUELDO-ANUAL-AUX
               DISPLAY "SU BONO ES DE $ " WSV-BONO-AUX.
               INITIALIZE WSV-POSTULANTE-AUX.

       30-CALCULAR-SUELDO-END.
       EXIT.
       END PROGRAM YOUR-PROGRAM-NAME.
