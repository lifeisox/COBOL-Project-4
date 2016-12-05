      ******************************************************************
      * This program is to update Inventory Master File 
      *    USING a Screen Section.
      *
      * Used File
      *    - Master Inventory File (Indexed Sequential): INVENT6
      *    - Supplier File (Indexed Sequential): SUPPLIER
      *    - Inventory Report File : INVREPRT.TXT
      *    - Reorder Report File: REORDER.TXT
      *
      ******************************************************************
       IDENTIFICATION              DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                 INVENTORY-UPDATE.
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
       SOURCE-COMPUTER.            ASUS X751.
      *-----------------------------------------------------------------
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
           SELECT  INVENT-FILE   
                   ASSIGN TO "D:\INVENT6"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS RANDOM
                   RECORD KEY IS IN-PART-NUMBER
                   FILE STATUS IS INVENT-FILE-STAT.
                   
      ******************************************************************
       DATA                        DIVISION.
      *-----------------------------------------------------------------
       FILE                        SECTION.
       FD  INVENT-FILE
           RECORD CONTAINS 39 CHARACTERS
           DATA RECORD IS INVENT-RECORD.
       01  INVENT-RECORD.
           05  IN-PART-NUMBER      PIC 9(05).
           05  IN-PART-NAME        PIC X(20).
           05  IN-QUANTITY         PIC 9(03).
           05  IN-UNIT-PRICE       PIC 9(04)V99.
           05  IN-REORDER-POINT    PIC 9(03).
           05  IN-SUPPLIER-ID      PIC X(02).
       
      *-----------------------------------------------------------------     
       WORKING-STORAGE             SECTION.
      *-----------------------------------------------------------------
       01  SWITCHES-AND-CONSTANTS.
           05  INVALID-SW          PIC X(01) VALUE SPACES.
               88  INVALID-NO                VALUE "N".
           05  CONFIRM-SW          PIC X(01) VALUE SPACES.
               88  VALID-CONFIRMED           VALUE "Y" "y" "N" "n".
               88  CONFIRM-NO                VALUE "N" "n".
       
       01  SCREEN-COLORS.
           05  BLACK               PIC S9(4) COMP-5 VALUE 0.
           05  BLUE                PIC S9(4) COMP-5 VALUE 1.
           05  GREEN               PIC S9(4) COMP-5 VALUE 2.
           05  CYAN                PIC S9(4) COMP-5 VALUE 3.
           05  RED                 PIC S9(4) COMP-5 VALUE 4.
           05  MAGENTA             PIC S9(4) COMP-5 VALUE 5.
           05  YELLOW              PIC S9(4) COMP-5 VALUE 6.
           05  WHITE               PIC S9(4) COMP-5 VALUE 7.
           
       01  FILE-STATUS-AND-MISC.
           05  INVENT-FILE-STAT    PIC X(02).
           05  WS-CODE             PIC X(01).
               88  VALID-CODE      VALUE "S" "s" "R" "r".
               88  VALID-SALE      VALUE "S" "s".
           05  WS-VALUE            PIC 9(03).
       
       01  CONFIRM-AND-ERROR-MESSAGES.
           05  CONFIRM-MESSAGE     PIC X(32) 
               VALUE "Do you have a record to update?".
       
      *-----------------------------------------------------------------     
       SCREEN                      SECTION.
      *-----------------------------------------------------------------
       01  OPENING-SCREEN.
           05  BLANK SCREEN
               BACKGROUND-COLOR BLUE   FOREGROUND-COLOR WHITE.
               
           05  SCREEN-BASICS.
               10  LINE  1 BLANK LINE  BACKGROUND-COLOR BLACK.
               10  LINE  2 BLANK LINE  BACKGROUND-COLOR BLACK.
               10  LINE  3 BLANK LINE  BACKGROUND-COLOR BLACK.
               10  LINE  2 COLUMN 18   
                           VALUE "ONLINE USER UPDATE"
                           BACKGROUND-COLOR BLACK
                           FOREGROUND-COLOR YELLOW.
               10  LINE  5 COLUMN  7   VALUE "     PART NUMBER:".
               10  LINE  7 COLUMN  7   
                           VALUE "TRANSACTION CODE:   (S)ale (R)eceipt".
               10  LINE  9 COLUMN  7   VALUE "TRANSACTION QTY.:".
               10  LINE 10 COLUMN  7
                           VALUE "------------------------------------".
               
           05  SCREEN-VALUES.
               10  SS-PART-NUMBER      PIC 9(05) TO    IN-PART-NUMBER
                   LINE  5 COLUMN 25   FOREGROUND-COLOR YELLOW
                                       REVERSE-VIDEO AUTO.
               10  SS-CODE             PIC X(01) TO    WS-CODE
                   LINE  7 COLUMN 25   FOREGROUND-COLOR YELLOW
                                       REVERSE-VIDEO AUTO.
               10  SS-VALUE            PIC 9(03) TO    WS-VALUE
                   LINE  9 COLUMN 25   FOREGROUND-COLOR YELLOW
                                       REVERSE-VIDEO AUTO.
               10  LINE 12 BLANK LINE.
               10  LINE 13 BLANK LINE.
               10  LINE 15 BLANK LINE.
               
       01  CONFIRM-SCREEN.
           05  LINE 12 BLANK LINE      BACKGROUND-COLOR BLACK.
           05                          PIC X(32) FROM CONFIRM-MESSAGE
               LINE 12 COLUMN  8       
               BACKGROUND-COLOR BLACK  FOREGROUND-COLOR YELLOW.
           05                          PIC X(01) USING CONFIRM-SW
               LINE 12 COLUMN 40       BLINK AUTO
               BACKGROUND-COLOR BLACK  FOREGROUND-COLOR YELLOW.
           05  LINE 13 BLANK LINE      BACKGROUND-COLOR BLACK.
           05  LINE 13 COLUMN 19       
                       VALUE "(Y/y: Yes, N/n: No)"
               BACKGROUND-COLOR BLACK  FOREGROUND-COLOR YELLOW.
               
       01  ERROR-SCREEN.
           05  LINE 15 BLANK LINE      BACKGROUND-COLOR RED.  
           05  LINE 15 COLUMN  7       
                       VALUE "Can't find primary key!!"
               BACKGROUND-COLOR RED    FOREGROUND-COLOR YELLOW.  
       
       01  CLEAR-ERROR-SCREEN.
           05  LINE 15 BLANK LINE      BACKGROUND-COLOR BLUE.
                                                                                                                              
      ******************************************************************
       PROCEDURE                   DIVISION.
      *-----------------------------------------------------------------
      * Main procedure
      *-----------------------------------------------------------------
       100-UPDATE-INVENTORY-FILE.
           PERFORM 200-INITIATE-INVENTORY-UPDATE.
           PERFORM 200-PROCEED-INVENTORY-UPDATE UNTIL CONFIRM-NO.
           PERFORM 200-TERMINATE-INVENTORY-UPDATE.
           
           STOP RUN.
           
      ******************************************************************     
      * Open input & print file, initialize variables that used as 
      * switch and for sum, print report headers, and 
      * read the very first record from input file.
      *-----------------------------------------------------------------
       200-INITIATE-INVENTORY-UPDATE.
           PERFORM 300-OPEN-INVENTORY-FILE.
           PERFORM 300-INITIALIZE-SWITCHES.
           PERFORM 300-ASK-UPDATE.
           
      *-----------------------------------------------------------------
      * Print one inventory record and reorder record as given format 
      *  and read next record. when reorder is printed, only prit if 
      *  quanty is less than or equal to re-order point
      *-----------------------------------------------------------------
       200-PROCEED-INVENTORY-UPDATE.
           PERFORM 300-ENTER-UPDATE-RECORD.
           PERFORM 300-UPDATE-FIELDS.
           PERFORM 300-REWRITE-INVENTORY-RECORD.
           PERFORM 300-ASK-UPDATE.
           
      *-----------------------------------------------------------------
      * after printed all records, total and footer should be printed.
      * after that close all files.
      *-----------------------------------------------------------------
       200-TERMINATE-INVENTORY-UPDATE.
           PERFORM 300-CLOSE-INVENTORY-FILE.
           PERFORM 300-OTHER-EOF-JOB.
           
      ******************************************************************
      * open input file and output file to print                                          
      *-----------------------------------------------------------------
       300-OPEN-INVENTORY-FILE.
           OPEN I-O INVENT-FILE.
      
      *-----------------------------------------------------------------
      * initialize variables
      *-----------------------------------------------------------------
       300-INITIALIZE-SWITCHES.
           INITIALIZE SWITCHES-AND-CONSTANTS.
      
      *----------------------------------------------------------------- 
      * read a input record. if eof then set EOF-SW as 'Yes'
      * if not, add 1 to read record count
      *-----------------------------------------------------------------
       300-ASK-UPDATE.
           DISPLAY CONFIRM-SCREEN.
           ACCEPT  CONFIRM-SCREEN.
       
      *-----------------------------------------------------------------
       300-ENTER-UPDATE-RECORD.
           PERFORM 400-DISPLAY-OPENING-SCREEN.
           PERFORM 400-INITIALIZE-INVALID-SW.
           PERFORM 400-ACCEPT-PART-NUMBER  UNTIL INVALID-NO.
           PERFORM 400-ACCEPT-TRANS-CODE   UNTIL VALID-CODE.
           PERFORM 400-ACCEPT-TRANS-VALUE.
               
      *-----------------------------------------------------------------
       300-UPDATE-FIELDS.
           IF  VALID-SALE 
               SUBTRACT WS-VALUE FROM IN-QUANTITY
           ELSE
               ADD WS-VALUE TO IN-QUANTITY.
               
      *----------------------------------------------------------------- 
       300-REWRITE-INVENTORY-RECORD.
           REWRITE INVENT-RECORD
               INVALID KEY DISPLAY ERROR-SCREEN.
                   .
      *-----------------------------------------------------------------     
       300-OTHER-EOF-JOB.
           DISPLAY ERASE "UPDATE JOB FINISHED!!! ".
           
      *-----------------------------------------------------------------
       300-CLOSE-INVENTORY-FILE.
           CLOSE   INVENT-FILE.
       
      *-----------------------------------------------------------------
       400-DISPLAY-OPENING-SCREEN.
           INITIALIZE FILE-STATUS-AND-MISC INVENT-RECORD. 
           DISPLAY OPENING-SCREEN.
           
      *-----------------------------------------------------------------
       400-INITIALIZE-INVALID-SW.
           MOVE    SPACES  TO  INVALID-SW.
           
      *-----------------------------------------------------------------
       400-ACCEPT-PART-NUMBER.
           ACCEPT  SS-PART-NUMBER.
           READ INVENT-FILE    KEY IS  IN-PART-NUMBER
               INVALID KEY     MOVE "Y"    TO INVALID-SW
                               DISPLAY ERROR-SCREEN
               NOT INVALID KEY MOVE "N"    TO INVALID-SW
                               DISPLAY CLEAR-ERROR-SCREEN.
               
      *-----------------------------------------------------------------
       400-ACCEPT-TRANS-CODE.
           ACCEPT  SS-CODE.
           
      *-----------------------------------------------------------------
       400-ACCEPT-TRANS-VALUE.
           ACCEPT  SS-VALUE.

      
