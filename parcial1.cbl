      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. parcialNotas.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALUMNOS
           ASSIGN TO
           "..\alumnos.txt".
           SELECT NOTAS
           ASSIGN TO
           "..\notas.txt".
           SELECT ACTAS
           ASSIGN TO
           "..\actas.txt".
           SELECT ARCH-SORT
           ASSIGN TO "sortwork".
           SELECT LISTADO
           ASSIGN TO PRINTER,
           "..\impPromNotas.dat".
       DATA DIVISION.
       FILE SECTION.
       FD  ALUMNOS.
       01  alu-reg.
           03 alu-codigo pic x(6).
           03 alu-nombre pic x(20).

       FD  NOTAS.
       01  not-reg.
           03 not-acta pic 9(6).
           03 not-fecha pic 9(8).
           03 not-alumno pic x(6).
           03 not-materia pic x(3).
           03 not-nota pic 9(2).
       FD  ACTAS.
       01  act1-reg.
           03 act1-tipo-reg pic x.
           03 act1-acta pic 9(6).
           03 act1-fecha pic 9(8).
           03 act1-materia pic x(3).
       01  act2-registro.
           03 act2-tipo-reg pic x.
           03 act2-alumno pic x(9).
           03 act2-nota pic 9(2).

       SD  ARCH-SORT.
       01  srt-reg.
           03 srt-leg-alu pic x(6).
           03 srt-nom-alu pic x(20).
           03 srt-prom-alu pic s9(2)v99.

       FD  LISTADO
           LINAGE IS 60 LINES
           with FOOTING AT 50
           lines at top 1
           lines at BOTTOM 1.
       01  lis-reg pic x(80).

       WORKING-STORAGE SECTION.
       01  w-flag-alu PIC 9 VALUE ZERO.
       01  w-flag-notas PIC 9 value zero.
       01  w-flag-actas PIC 9 value zero.
       01  w-alu-ant pic 9(6).
       01  w-nom-ant pic x(20).
       01  w-notas-alu pic s9(3).
       01  w-notas-acta pic s9(3).
       01  w-nota-final pic s9(3).
       01  w-cont-not pic 9(2).
       01  w-cont-acta pic 9(2).
       01  w-promedio pic s9(2)v99.
       01  w-cont-not-alu pic 9(2).
       01  cabecera1.
           03  lin-titulo.
               05 filler pic x(30) value spaces.
               05 filler pic x(19) value "PROMEDIO DE ALUMNOS".
               05 filler pic x(31) value spaces.
       01  cabecera2.
           03  lin-titulo-soc.
               05 filler pic x(22) value spaces.
               05 FILLER pic x(6) value "CODIGO".
               05 filler  pic x(8) value space.
               05 FILLER pic x(6) value "NOMBRE".
               05 filler  pic x(8) value space.
               05 FILLER pic x(8) value "PROMEDIO".
               05 filler pic x(22) value spaces.
       01  cabecera3.
           03  lin-guarda.
               05 filler pic x(80) value all "*".
        01  detalle1.
           03  lin-dat-alu.
               05 filler pic x(22) value spaces.
               05 l-alu pic x(6).
               05 filler pic x(8) value spaces.
               05 l-nom pic x(15).
               05 filler pic x(8) value spaces.
               05 l-nota pic zz9,99 value spaces.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO-GENERAL
           PERFORM 200-LEER-ALU
           PERFORM UNTIL w-flag-alu is equal 1
             PERFORM 300-BUSCAR-NOTAS
             PERFORM 400-BUSCAR-ACTAS
             PERFORM 500-INICIO-ALU
             PERFORM UNTIL w-flag-alu is EQUAL 1 OR
             alu-codigo is not equal w-alu-ant
                   PERFORM 600-PROCESO-ALU
                   PERFORM 620-PROCESO-ACTA
                   PERFORM 200-LEER-ALU
             END-PERFORM
             PERFORM 630-FIN-ALU
           END-PERFORM.
           PERFORM 700-FIN-GENERAL
            STOP RUN.
       100-INICIO-GENERAL.
           PERFORM 120-ABRIR-ARCHIVOS.
           PERFORM 130-INICIAR-VARIABLES.
           PERFORM 140-IMPRIMO-ENCABEZADO.
       120-ABRIR-ARCHIVOS.
           OPEN INPUT ALUMNOS.
           OPEN INPUT NOTAS.
           OPEN INPUT ACTAS.
           OPEN OUTPUT LISTADO.
       130-INICIAR-VARIABLES.
           MOVE ZERO TO w-cont-not.
           MOVE ZERO TO w-cont-acta.
       140-IMPRIMO-ENCABEZADO.
           WRITE lis-reg FROM cabecera3 AFTER 1.
           write lis-reg FROM cabecera1 AFTER 1.
           write lis-reg FROM cabecera2 AFTER 1.
           write lis-reg from cabecera3 AFTER 1.
       200-LEER-ALU.
           READ ALUMNOS AT END MOVE 1 TO w-flag-alu.

       300-BUSCAR-NOTAS.
           PERFORM 320-LEER-NOTAS.
           PERFORM 350-BUSCO-NOTA-ALU.
       350-BUSCO-NOTA-ALU.
           PERFORM 320-LEER-NOTAS until w-flag-notas is equal 1 OR
                   not-alumno is equals alu-codigo.
       320-LEER-NOTAS.
           READ NOTAS AT END MOVE 1 TO w-flag-notas.

       400-BUSCAR-ACTAS.
           PERFORM 420-LEER-ACTAS.
           PERFORM 450-BUSCAR-ACTA-ALU.
       420-LEER-ACTAS.
           READ ACTAS AT END MOVE 1 TO w-flag-actas.
       450-BUSCAR-ACTA-ALU.
           PERFORM 420-LEER-ACTAS UNTIL w-flag-actas is equal 1 OR
               act2-alumno is EQUALS alu-codigo.

       500-INICIO-ALU.
           MOVE alu-codigo TO w-alu-ant.
           move alu-nombre to w-nom-ant.
           move zero to w-notas-alu.
           move zero to w-notas-acta.
           move zero to w-cont-not.
           move zero to w-cont-acta.
           move zero to w-cont-not-alu.
           move zero to w-promedio.
       600-PROCESO-ALU.
           add 1 to w-cont-not.
           add not-nota to w-notas-alu.
       620-PROCESO-ACTA.
           add 1 to w-cont-acta.
           add act2-nota to w-notas-acta.

       630-FIN-ALU.
           compute w-cont-not-alu=w-cont-not+w-cont-acta.
           compute w-nota-final= w-notas-alu+w-notas-acta.
           PERFORM 640-CALCULO-PROMEDIO.
           PERFORM 650-ARMO-LINEA.
       640-CALCULO-PROMEDIO.
           IF w-cont-not-alu IS NOT EQUAL ZERO
               COMPUTE w-promedio= w-nota-final/ w-cont-not-alu.

       650-ARMO-LINEA.
           MOVE w-alu-ant TO l-alu.
           MOVE w-nom-ant TO l-nom.
           MOVE w-promedio TO l-nota.
           WRITE lis-reg FROM detalle1 AFTER 1.

       700-FIN-GENERAL.
           CLOSE ALUMNOS.
           CLOSE NOTAS.
           CLOSE ACTAS.
           CLOSE LISTADO.
       END PROGRAM parcialNotas.
