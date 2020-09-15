       IDENTIFICATION DIVISION.
       PROGRAM-ID.  MathLab.
       AUTHOR.  GARRETT BURNS.
      *
      *    THIS IS THE MATHLAB
      *    THERE IS NO INPUT FILE.
      *    IT CALCULATES AND THEN COMPUTES A VALUE FOR ANS AND PRINTS
      *    EACH ANSWER
      *
      *    REMEMBER TO MAKE THE REQUIRED CHANGES TO THIS PROGRAM
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-PC.
       OBJECT-COMPUTER.  IBM-PC.
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT MLAB-REPORT
               ASSIGN TO PRINTER "PRINTCGB".
      *
       DATA DIVISION.
      *
       FD  MLAB-REPORT
           RECORD CONTAINS 80 CHARACTERS.
      *
       01  REPORT-RECORD       PIC X(80).
      *
       WORKING-STORAGE SECTION.
      *
       01  TEMP-STORAGE.
           05  ANS             PIC S99V999    VALUE +0.
           05  TEMP1           PIC S99V999    VALUE +0.
           05  TEMP3           PIC S99V999    VALUE +0.
           05  TEMP2           PIC S99V999    VALUE +0.
           05  TEMP4           PIC S99V999    VALUE +0.
           
      *    ADD ANY TEMPORAY FIELDS AS AN 05 UNDER TEMP-STORAGE

       01  VARIABLE-VALUE.
           05  A               PIC S99V999    VALUE +5.
           05  B               PIC S99V999    VALUE +4.
           05  C               PIC S99V999    VALUE +1.
           05  D               PIC S99V999    VALUE +2.
           05  E               PIC S99V999    VALUE +7.
      *
       01  REPORT-FIELDS.
           05  PROPER-SPACING  PIC 9          VALUE 1.
      *
       01  HEADING-LINE.
           05                  PIC X(25)      VALUE
                                   '    MATH  -        '.
           05                  PIC X(3)       VALUE 'CGB'.

       01  DETAIL-LINE.
           05                  PIC X(3)       VALUE ' '.
           05  DL-MESSAGE      PIC X(28).
           05  DL-ANS          PIC 99.999-.
      
       PROCEDURE DIVISION.
      *
       00-CONTROL-MODULE.
           PERFORM 20-HOUSEKEEPING
           PERFORM 100-CALCULATE-VERBS
           PERFORM 200-COMPUTE
           PERFORM 999-FINAL-ROUTINE
       .

       20-HOUSEKEEPING.
           OPEN OUTPUT MLAB-REPORT

           WRITE REPORT-RECORD FROM HEADING-LINE
               AFTER ADVANCING PAGE
           MOVE 3 TO PROPER-SPACING
       .

       100-CALCULATE-VERBS.
      *    FOLLOW THESE COMMENTS WITH YOUR CALCULATIONS (USING THE ADD,
      *    SUBTRACT, MULTIPLY AND DIVIDE VERBS).

           SUBTRACT B FROM A, GIVING TEMP1
           MULTIPLY E BY TEMP1
           MULTIPLY C BY D, GIVING TEMP2
           DIVIDE TEMP2 INTO TEMP1
           ADD TEMP1 TO E, GIVING TEMP3
           
           ADD D TO A, GIVING TEMP4
           DIVIDE TEMP3 BY TEMP4 GIVING ANS ROUNDED
           
           
           MOVE 'ANS CALCULATED = ' TO DL-MESSAGE
           MOVE ANS TO DL-ANS

           MOVE DETAIL-LINE TO REPORT-RECORD
           PERFORM 300-WRITE-A-LINE

           MOVE 0 TO ANS
           MOVE 2 TO PROPER-SPACING
        
       .

       200-COMPUTE.
      *    FOLLOW THIS COMMENT WITH YOUR SINGLE COMPUTE STATEMENT

           COMPUTE ANS = (E + (E * (A - B) / (C * D) ) ) / (D + A)


           MOVE 'ANS COMPUTED = ' TO DL-MESSAGE
           MOVE ANS TO DL-ANS

           MOVE DETAIL-LINE TO REPORT-RECORD
           PERFORM 300-WRITE-A-LINE
       .

       300-WRITE-A-LINE.

           WRITE REPORT-RECORD
               AFTER ADVANCING PROPER-SPACING
       .

       999-FINAL-ROUTINE.
           CLOSE MLAB-REPORT
           STOP RUN
       .
