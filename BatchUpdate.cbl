      ******************************************************************
      * This program is to implement Master File Batch Update
      *    USING Balance Line Diagram.
      *
      * Used File
      *    - Master Inventory File: INVENT4.TXT
      *    - Transaction File: TRANSACTIONS.TXT
      *    - Updated Master Inventory File : INVENT5.TXT
      *    - Error File: ERRORS.TXT
      *
      ******************************************************************
       IDENTIFICATION              DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                 BATCH-UPDATE.
       AUTHOR.                     Aradhita Mohanty,
                                   Byung Seon Kim,
                                   Elena Sveshnikova,
                                   Karadjordje Dabic,
                                   Nadia Chubarev.
       DATE-WRITTEN.               November 24, 2016. 
       DATE-COMPILED.     
       
      ****************************************************************** 
       ENVIRONMENT                 DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION               SECTION.
       SOURCE-COMPUTER.            WHATEVER-PC.
      *-----------------------------------------------------------------
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
           SELECT  INVENT-FILE-IN   
                   ASSIGN TO "D:\INVENT4.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
                   
           SELECT  TRANS-FILE-IN   
                   ASSIGN TO "D:\TRANSACTIONS.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
                   
           SELECT  INVENT-FILE-OUT   
                   ASSIGN TO "D:\INVENT5.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
                   
           SELECT  ERRORS-FILE-OUT        
                   ASSIGN TO "D:\ERRORS.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
                   
      ******************************************************************
       DATA                        DIVISION.
      *-----------------------------------------------------------------
       FILE                        SECTION.
       FD  INVENT-FILE-IN
           RECORD CONTAINS 39 CHARACTERS
           DATA RECORD IS INVEN-RECORD-IN.
       01  INVENT-RECORD-IN.
           05  PART-NUMBER-IN      PIC 9(05).
           05  PART-NAME-IN        PIC X(20).
           05  QUANTITY-IN         PIC 9(03).
           05  UNIT-PRICE-IN       PIC 9(04)V99.
           05  REORDER-POINT-IN    PIC 9(03).
           05  SUPPLIER-ID-IN      PIC X(02).
       
       FD  TRANS-FILE-IN
           RECORD CONTAINS 9 CHARACTERS
           DATA RECORD IS TRANS-RECORD-IN.
       01  TRANS-RECORD-IN.
           05  TR-PART-NUMBER      PIC 9(05).
           05  TR-CODE             PIC X(01).
               88  TR-RECEIPT-CODE           VALUE "R".
               88  TR-SALE-CODE              VALUE "S".
           05  TR-VALUE            PIC 9(03).
       
       FD  INVENT-FILE-OUT
           RECORD CONTAINS 39 CHARACTERS
           DATA RECORD IS INVENT-RECORD-OUT.
       01  INVENT-RECORD-OUT.
           05  PART-NUMBER-OUT     PIC 9(05).
           05  PART-NAME-OUT       PIC X(20).
           05  QUANTITY-OUT        PIC 9(03).
           05  UNIT-PRICE-OUT      PIC 9(04)V99.
           05  REORDER-POINT-OUT   PIC 9(03).
           05  SUPPLIER-ID-OUT     PIC X(02).
       
       FD  ERRORS-FILE-OUT
           RECORD CONTAINS 9 CHARACTERS
           DATA RECORD IS ERRORS-RECORD-OUT.
       01  ERRORS-RECORD-OUT.
           05  PART-NUMBER-ERR     PIC 9(05).
           05  CODE-ERR            PIC X(01).
           05  VALUE-ERR           PIC 9(03).
           
      *-----------------------------------------------------------------     
       WORKING-STORAGE             SECTION.
      *-----------------------------------------------------------------
       01  SWITCHES-AND-COUNTERS.
           05  INVENT-EOF-SW       PIC X(01).
               88  INVENT-EOF                VALUE "Y".
           05  TRANS-EOF-SW        PIC X(01).
               88  TRANS-EOF                 VALUE "Y".
           
      ******************************************************************
       PROCEDURE                   DIVISION.
      *-----------------------------------------------------------------
      * Main procedure
      *-----------------------------------------------------------------
       100-BATCH-UPDATE.
           PERFORM 200-INITIATE-BATCH-UPDATE.
           PERFORM 200-PROCEED-BATCH-UPDATE 
                                   UNTIL INVENT-EOF AND TRANS-EOF.
           PERFORM 200-TERMINATE-BATCH-UPDATE.
           
           STOP RUN.
           
      ******************************************************************     
      * Open master, transaction and error files
      * Initialize variables that used as switches 
      * Read the very first record from master and transaction file.
      *-----------------------------------------------------------------
       200-INITIATE-BATCH-UPDATE.
           PERFORM 300-OPEN-ALL-FILES.
           PERFORM 300-INITIALIZE-SWITCHES-AND-COUNTERS.
           PERFORM 300-READ-INVENT-FILE-IN.
           PERFORM 300-READ-TRANS-FILE-IN.
           
      *-----------------------------------------------------------------
      * Main procedure for batch update according to Balace Line
      * diagram. Be careful that it is an error when transaction code 
      * is not 'R' or 'S'.
      *-----------------------------------------------------------------
       200-PROCEED-BATCH-UPDATE.
           EVALUATE TRUE
               WHEN TR-PART-NUMBER = PART-NUMBER-IN
                   PERFORM 300-PROCESS-WHEN-EQUAL
               WHEN TR-PART-NUMBER > PART-NUMBER-IN
                   PERFORM 300-PROCESS-WHEN-TRANS-GT-MASTER
               WHEN OTHER
                   PERFORM 300-PROCESS-WHEN-TRANS-LT-MASTER
           END-EVALUATE.
           
      *-----------------------------------------------------------------
      * Display the end of program
      * close all files.
      *-----------------------------------------------------------------
       200-TERMINATE-BATCH-UPDATE.
           PERFORM 300-CLOSE-ALL-FILES.
           PERFORM 300-OTHER-EOF-JOB.
       
      ******************************************************************
       300-OPEN-ALL-FILES.
           OPEN    INPUT   INVENT-FILE-IN
                   INPUT   TRANS-FILE-IN
                   OUTPUT  INVENT-FILE-OUT
                   OUTPUT  ERRORS-FILE-OUT.
           
      *-----------------------------------------------------------------
       300-INITIALIZE-SWITCHES-AND-COUNTERS.
           INITIALIZE SWITCHES-AND-COUNTERS.
      
      *----------------------------------------------------------------- 
       300-READ-INVENT-FILE-IN.
           READ INVENT-FILE-IN
                   AT END      MOVE "Y"    TO INVENT-EOF-SW
                               MOVE 99999  TO PART-NUMBER-IN.

      *----------------------------------------------------------------- 
       300-READ-TRANS-FILE-IN.
           READ TRANS-FILE-IN
                   AT END      MOVE "Y"    TO TRANS-EOF-SW
                               MOVE 99999  TO TR-PART-NUMBER.
       
      *----------------------------------------------------------------- 
       300-PROCESS-WHEN-EQUAL.
           IF TR-RECEIPT-CODE OR TR-SALE-CODE THEN
               PERFORM 400-MODIFY-INVENT-OUT
           ELSE
               PERFORM 400-WRITE-TRANSACTION-ERROR
           END-IF.
           PERFORM 300-READ-TRANS-FILE-IN.
       
      *-----------------------------------------------------------------
       300-PROCESS-WHEN-TRANS-GT-MASTER.
           PERFORM 400-WRITE-MODIFIED-INVENT-OUT.
           PERFORM 300-READ-INVENT-FILE-IN.
       
      *-----------------------------------------------------------------
       300-PROCESS-WHEN-TRANS-LT-MASTER.
           PERFORM 400-WRITE-TRANSACTION-ERROR.
           PERFORM 300-READ-TRANS-FILE-IN.
       
      *---------------------------------------------------------------- - 
       300-OTHER-EOF-JOB.
           DISPLAY "BATCH UPDATE COMPLETED!!!".
           
      *-----------------------------------------------------------------
       300-CLOSE-ALL-FILES.
           CLOSE   INVENT-FILE-IN
                   TRANS-FILE-IN
                   INVENT-FILE-OUT
                   ERRORS-FILE-OUT.
       
      ******************************************************************
       400-MODIFY-INVENT-OUT.
           IF  TR-RECEIPT-CODE 
               ADD TR-VALUE TO QUANTITY-IN
           ELSE
               SUBTRACT TR-VALUE FROM QUANTITY-IN.
      
      *-----------------------------------------------------------------
       400-WRITE-MODIFIED-INVENT-OUT.
           WRITE INVENT-RECORD-OUT FROM INVENT-RECORD-IN.
           
      *-----------------------------------------------------------------
       400-WRITE-TRANSACTION-ERROR.    
           WRITE ERRORS-RECORD-OUT FROM TRANS-RECORD-IN.
      
      
