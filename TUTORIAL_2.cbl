       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMP-SCR.
       AUTHOR. J WOLFE.
       DATE-WRITTEN. 07/17/2021.
      *****************************************************************
      * THIS PROGRAM IS ONLY INTENDED FOR USE WITH VISUAL COBOL ELEMENTS
      * AND ALSO FOR A PC ENVIRONMENT (I USE THE NET EXPRESS IDE) WITH
      * MICROFOCUS.
      * THIS PROGRAM CREATES A VISUAL FORM THAT SIMPLY WRITES THE FORM
      *DATA TO A DOC FILE IN THE FORMATTED PATTERN OF 'OUTPUT-VARS'
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.



                   SELECT OUTFILE ASSIGN TO 'RPT.DOC'
                   ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.


       FD OUTFILE RECORDING MODE F.
       01 PRINT-LINE                                   PIC X(132).


       WORKING-STORAGE SECTION.
       01 RESPONSES                                    PIC X.
           88 DONE                                     VALUE 'Q'.
       01 OUTPUT-VARS.
           05 F-NAME                          PIC X(15) VALUE SPACES.
           05 FILLER                       PIC X(2) VALUE SPACES.
           05 L-NAME                           PIC X(15).
           05                              PIC X(2) VALUE SPACES.
           05 ID-O                           PIC X(5).




       SCREEN SECTION.
       01 DATA-ENTRY-SCREEN.
           05 VALUE 'EMPLOYEE ENTRY FORM' BLANK SCREEN LINE 1 COL 35.
           05 VALUE 'FIRST NAME'                   LINE 3 COL 10.
           05 F-NUM-INPUT                            LINE 3 COL 25
                           PIC X(15) TO F-NAME.
           05 VALUE 'LAST NAME'                  LINE 5 COL 10.
           05 S-NUM-INPUT                            LINE 5  COL 25
                       PIC X(15) TO L-NAME.
           05 VALUE 'ID NUMBER'                      LINE 7  COL 10.
           05 ID-IN                                  LINE 7 COL 25
                           PIC X(5) TO ID-O .
           05 VALUE 'C - TO CONTINUE'                LINE 11 COL 30.
           05 VALUE 'Q TO QUIT'                      LINE 12 COL 30.
           05 VALUE 'ENTER RESPONSE'                 LINE 14 COL 30.
           05 RESPONSE-INPUT                         LINE 14 COL 45
                           PIC X TO RESPONSES.

       01 FNAME-ENTRY.
           05 VALUE 'FIRST NAME ENTRY' BLANK SCREEN LINE 1 COL 35.
           05 VALUE 'YOU FORGOT THE FIRST NAME'     LINE 3 COL 5.
           05 F-NAME2                               LINE 3 COL 40
                           PIC X(15) TO F-NAME.

       01 LNAME-ENTRY.
           05 VALUE 'LAST NAME ENTRY' BLANK SCREEN LINE 1 COL 35.
           05 VALUE 'YOU FORGOT THE LAST NAME'     LINE 3 COL 5.
           05 F-NAME2                               LINE 3 COL 40
                           PIC X(15) TO L-NAME.
       01 ID-ENTRY-S.
           05 VALUE 'ID ENTRY' BLANK SCREEN LINE 1 COL 35.
           05 VALUE 'YOU FORGOT THE ID'     LINE 3 COL 5.
           05 ID-2                               LINE 3 COL 40
                           PIC X(15) TO ID-O.












       PROCEDURE DIVISION.
       100-MAIN.
           OPEN OUTPUT OUTFILE.
           PERFORM SHOWSCREEN UNTIL DONE
           CLOSE OUTFILE
           GOBACK.


       SHOWSCREEN.
           DISPLAY DATA-ENTRY-SCREEN
           ACCEPT DATA-ENTRY-SCREEN
           PERFORM COLLECT-N-WRITE.


       COLLECT-N-WRITE.
           IF (F-NAME = SPACES) AND( L-NAME NOT EQUAL SPACES OR ID-O NOT
           EQUAL SPACES)
               THEN PERFORM F-NAME-ENTRY
           END-IF.

           IF (L-NAME = SPACES) AND( F-NAME NOT EQUAL SPACES OR ID-O NOT
           EQUAL SPACES)
               THEN PERFORM L-NAME-ENTRY
           END-IF.


           IF (ID-O = SPACES) AND( L-NAME NOT EQUAL SPACES OR F-NAME NOT
           EQUAL SPACES)
               THEN PERFORM ID-ENTRY
           END-IF.




           WRITE PRINT-LINE FROM OUTPUT-VARS.


       F-NAME-ENTRY.
           DISPLAY FNAME-ENTRY
           ACCEPT FNAME-ENTRY
           IF F-NAME = SPACES
               THEN PERFORM F-NAME-ENTRY
               END-IF.

       L-NAME-ENTRY.
           DISPLAY LNAME-ENTRY
           ACCEPT LNAME-ENTRY
           IF L-NAME = SPACES
               THEN PERFORM L-NAME-ENTRY
               END-IF.


       ID-ENTRY.
           DISPLAY ID-ENTRY-S
           ACCEPT ID-ENTRY-S
           IF ID-O = SPACES
               THEN PERFORM ID-ENTRY
               END-IF.









