      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.      COVID19.
       AUTHOR.          ALTON GOODMAN. 
      *
      * COVID REPORT PROGRAM.
      * READ A CSV FILE, FORMAT THE REPORT AND WRITE IT TO SYSOUT.
      *
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN TO PRTLINE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL.
           SELECT COVID-CSV ASSIGN TO COVID19
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE IS F.
      *
       01  PRT-RCD.
           05  PRT-DATE                         PIC X(10).
           05  FILLER                           PIC X(01).
           05  PRT-TIME                         PIC X(09).
           05  FILLER                           PIC X(01).
           05  PRT-COUNTRY                      PIC X(20).
           05  FILLER                           PIC X(01).
           05  PRT-CC                           PIC X(02).
           05  FILLER                           PIC X(02).
           05  PRT-NEW-CC                       PIC X(05).
           05  FILLER                           PIC X(06).
           05  PRT-TOTAL-CC                     PIC X(05).
           05  FILLER                           PIC X(06).
           05  PRT-NEW-DEATHS                   PIC X(05).
           05  FILLER                           PIC X(01).
           05  PRT-TOTAL-DEATHS                 PIC X(05).
           05  FILLER                           PIC X(02).
           05  PRT-NEW-RECVR                    PIC X(05).
           05  FILLER                           PIC X(06).
           05  PRT-TOTAL-RECVR                  PIC X(05).
           05  FILLER                           PIC X(18).
      *
       FD  COVID-CSV RECORDING MODE IS V
           RECORD IS VARYING FROM 1 TO 296 CHARACTERS
           DEPENDING ON COVID-RCD-LENGTH
           BLOCK 0
           LABEL RECORDS ARE OMITTED
           DATA RECORD IS COVID-RCD.
       01  COVID-RCD                            PIC X(296).
      *
       WORKING-STORAGE SECTION.
      *
       01  HEADER-1.
           05  FILLER                           PIC X(03) 
                VALUE SPACE.
           05  FILLER                           PIC X(04) 
                VALUE "DATE".
           05  FILLER                           PIC X(04) 
                VALUE SPACE.
           05  FILLER                           PIC X(04) 
                VALUE "TIME".
           05  FILLER                           PIC X(06) 
                VALUE SPACE.
           05  FILLER                           PIC X(07) 
                VALUE "COUNTRY".
           05  FILLER                           PIC X(18) 
                VALUE SPACE.
           05  FILLER                           PIC X(03)
                VALUE "NEW".
           05  FILLER                           PIC X(08) 
                VALUE SPACE.
           05  FILLER                           PIC X(05)
                VALUE "TOTAL".
           05  FILLER                           PIC X(05) 
                VALUE SPACE.
           05  FILLER                           PIC X(03)
                 VALUE "NEW".
           05  FILLER                           PIC X(04) 
                VALUE SPACE.
           05  FILLER                           PIC X(05)
                VALUE "TOTAL". 
           05  FILLER                           PIC X(02) 
                VALUE SPACE.
           05  FILLER                           PIC X(03)
                 VALUE "NEW".
           05  FILLER                           PIC X(08) 
                VALUE SPACE.
           05  FILLER                           PIC X(05)
                VALUE "TOTAL".
       01 HEADER-2.
           05  FILLER                           PIC X(23)
                VALUE SPACES.
           05 FILLER                            PIC X(04)
                VALUE "CODE".
           05  FILLER                           PIC X(19)
                VALUE SPACES.
           05  FILLER                           PIC X(10)
                VALUE "CONFIRMED".
           05  FILLER                           PIC X(01)
                VALUE SPACES.
           05  FILLER                           PIC X(09)
                VALUE "CONFIRMED".
           05  FILLER                           PIC X(01)
                VALUE SPACES.
           05  FILLER                           PIC X(06)
                VALUE "DEATHS".
           05  FILLER                           PIC X(01)
                VALUE SPACES.
           05  FILLER                           PIC X(06)
                VALUE "DEATHS".
           05  FILLER                           PIC X(01)
                VALUE SPACES.
           05  FILLER                           PIC X(10)
                VALUE "RECOVERIES".
           05  FILLER                           PIC X(01)
                VALUE SPACES.
           05  FILLER                           PIC X(10)
                VALUE "RECOVERIES".
       01  HEADER-3.
           05  FILLER                           PIC X(46)
                VALUE SPACES.
           05 FILLER                            PIC X(05)
                VALUE "CASES".
           05  FILLER                           PIC X(06)
                VALUE SPACES.
           05 FILLER                            PIC X(05)
                VALUE "CASES".
       01  HEADER-4.
           05  FILLER                           PIC X(04)
               VALUE SPACES.
           05  FILLER                           PIC X(04)
               VALUE "SLUG".
           05  FILLER                           PIC X(124)
               VALUE SPACES.
      *
       01  HEADER-5                             PIC X(132)
           VALUE ALL "=".
      *
       01  WS-COVID-RCD                         PIC X(296) 
           VALUE SPACES.
      *
       01  WS-PRT-RCD2.
           05  FILLER                           PIC X(04)
                VALUE SPACES.
           05  WS-PRT-RCD-SLUG                  PIC X(50).
           05  FILLER                           PIC X(78)
                VALUE SPACES.
      *
       01  WS-PRINT-LINE.
           05  WS-PRT-COUNTRY                   PIC X(20).
           05  WS-PRT-CC                        PIC X(02).
           05  WS-PRT-SLUG                      PIC X(50).
           05  WS-PRT-NEW-CC                    PIC X(05).
           05  WS-PRT-TOTAL-CC                  PIC X(05).
           05  WS-PRT-NEW-DEATHS                PIC X(05).
           05  WS-PRT-TOTAL-DEATHS              PIC X(05).
           05  WS-PRT-NEW-RECVR                 PIC X(05).
           05  WS-PRT-TOTAL-RECVR               PIC X(05).
           05  WS-PRT-TIMESTAMP         .
               10  WS-PRT-DATE                  PIC X(10).
               10  WS-PRT-TIME                  PIC X(09).
      *
       01  FLAGS.
           05 LAST-REC                          PIC X(01) VALUE 'N'.

       01  COUNTERS.
           05  ADV-LINE                         PIC S9(03) COMP
               VALUE 1.
           05  LINE-CTR                         PIC S9(03) COMP
               VALUE ZERO.
           05  RCD-IN                           PIC S9(05) COMP
               VALUE ZERO.

       01  WS-WORK.
           05  COVID-RCD-LENGTH                 PIC 9(07) COMP
               VALUE ZERO.
      *------------------
       PROCEDURE DIVISION.
      *------------------
      *
       1000-PROCESSING.
      *
           PERFORM 2000-OPEN-FILES THRU 2000-EXIT.
           PERFORM 4000-WRITE-HEADERS THRU 4000-EXIT.
      *
      * READ FILE TWICE TO CONSUME COLUMN HEADERS
      *
           PERFORM 5000-READ-CSV THRU 5000-EXIT
               2 TIMES.
           PERFORM 3500-PROCESS-COVID THRU 3500-EXIT
               UNTIL LAST-REC = 'Y'.
           PERFORM 3000-CLOSE THRU 3000-EXIT.
           GOBACK.
      *
       2000-OPEN-FILES.
      *
           OPEN INPUT COVID-CSV.
           OPEN OUTPUT PRINT-LINE.
      *
       2000-EXIT.
           EXIT.
      *
       3000-CLOSE.
           CLOSE COVID-CSV.
           CLOSE PRINT-LINE.
      *
       3000-EXIT.
           EXIT.
      *
       3500-PROCESS-COVID.
           PERFORM 6000-UNSTRING THRU 6000-EXIT.
           PERFORM 7000-WRITE-PRT THRU 7000-EXIT.
           PERFORM 5000-READ-CSV THRU 5000-EXIT.
       3500-EXIT.
           EXIT.

       4000-WRITE-HEADERS.
           MOVE HEADER-1 TO PRT-RCD.
           WRITE PRT-RCD
               AFTER ADVANCING PAGE.
           MOVE HEADER-2 TO PRT-RCD.
           WRITE PRT-RCD
               AFTER ADVANCING 1 LINE.
           MOVE HEADER-3 TO PRT-RCD.
           WRITE PRT-RCD
               AFTER ADVANCING 1 LINE.
           MOVE HEADER-4 TO PRT-RCD.
           WRITE PRT-RCD
               AFTER ADVANCING 1 LINE.
           MOVE HEADER-5 TO PRT-RCD.
           WRITE PRT-RCD
               AFTER ADVANCING 1 LINE.
           MOVE 5 TO LINE-CTR.
           MOVE SPACES TO PRT-RCD.
       4000-EXIT.
           EXIT.
      *
       5000-READ-CSV.
           MOVE SPACES TO WS-COVID-RCD.
           READ COVID-CSV INTO WS-COVID-RCD
               AT END MOVE 'Y' TO LAST-REC
           END-READ.
           ADD 1 TO RCD-IN.
       5000-EXIT.
           EXIT.
      *
       6000-UNSTRING. 
           UNSTRING WS-COVID-RCD DELIMITED BY '","'
           INTO WS-PRT-COUNTRY,
           WS-PRT-CC,
           WS-PRT-SLUG,
           WS-PRT-NEW-CC,
           WS-PRT-TOTAL-CC,
           WS-PRT-NEW-DEATHS,
           WS-PRT-TOTAL-DEATHS,
           WS-PRT-NEW-RECVR,
           WS-PRT-TOTAL-RECVR,
           WS-PRT-TIMESTAMP
           END-UNSTRING.
           INSPECT WS-PRINT-LINE CONVERTING '"' TO SPACE.
       6000-EXIT.
           EXIT.
      *
       7000-WRITE-PRT.
           IF LINE-CTR >= 56
               PERFORM 4000-WRITE-HEADERS THRU 4000-EXIT.
           MOVE WS-PRT-DATE TO PRT-DATE.
           MOVE WS-PRT-TIME TO PRT-TIME. 
           MOVE WS-PRT-COUNTRY TO PRT-COUNTRY.
           MOVE WS-PRT-CC TO PRT-CC.
           MOVE WS-PRT-NEW-CC TO PRT-NEW-CC.
           MOVE WS-PRT-TOTAL-CC TO PRT-TOTAL-CC.
           MOVE WS-PRT-NEW-DEATHS TO PRT-NEW-DEATHS.
           MOVE WS-PRT-TOTAL-DEATHS TO PRT-TOTAL-DEATHS.
           MOVE WS-PRT-NEW-RECVR TO PRT-NEW-RECVR.
           MOVE WS-PRT-TOTAL-RECVR TO PRT-TOTAL-RECVR.

           MOVE 2 TO ADV-LINE.
           WRITE PRT-RCD
               AFTER ADVANCING ADV-LINE.
           ADD ADV-LINE  TO LINE-CTR.
           MOVE WS-PRT-SLUG TO WS-PRT-RCD-SLUG.
           MOVE WS-PRT-RCD2 TO PRT-RCD.

           MOVE 1 TO ADV-LINE.
           WRITE PRT-RCD
               AFTER ADVANCING ADV-LINE.
           ADD ADV-LINE  TO LINE-CTR.

           MOVE SPACES TO PRT-RCD.
       7000-EXIT.
           EXIT.
