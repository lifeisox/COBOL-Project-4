      ******************************************************************
      * This program is to convert Master Inventory Sequential File
      *    and Supplier Sequential File to
      *    Indexed Sequential File.
      *
      * Used File
      *    - Master Inventory File: INVENT5.TXT
      *    - Indexed Sequential File: INVENT6
      *    - Supplier File: SUPPLIERS.TXT
      *    - Indexed Sequential File: SUPPLIER
      *
      ******************************************************************
       IDENTIFICATION              DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                 CONVERT-FILE.
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
           SELECT  INVENT-FILE-IN   
                   ASSIGN TO "D:\INVENT5.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
                   
           SELECT  INVENT-FILE-OUT   
                   ASSIGN TO "D:\INVENT6"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS SEQUENTIAL
                   RECORD KEY IS PART-NUMBER-OUT
                   FILE STATUS IS INVENT-FILE-STATUS.
                   
           SELECT  SUPPLIER-FILE-IN   
                   ASSIGN TO "D:\SUPPLIERS.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
                   
           SELECT  SUPPLIER-FILE-OUT   
                   ASSIGN TO "D:\SUPPLIER"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS SEQUENTIAL
                   RECORD KEY IS SUPPLIER-ID-OUT
                   FILE STATUS IS SUPPLY-FILE-STATUS.   
                   
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
       
       FD  INVENT-FILE-OUT
           RECORD CONTAINS 39 CHARACTERS
           DATA RECORD IS INVENT-RECORD-OUT.
       01  INVENT-RECORD-OUT.
           05  PART-NUMBER-OUT     PIC 9(05).
           05  PART-NAME-OUT       PIC X(20).
           05  QUANTITY-OUT        PIC 9(03).
           05  UNIT-PRICE-OUT      PIC 9(04)V99.
           05  REORDER-POINT-OUT   PIC 9(03).
           05  SUPPLIER-ID-OOUT    PIC X(02).
       
       FD  SUPPLIER-FILE-IN
           RECORD CONTAINS 17 CHARACTERS
           DATA RECORD IS INVEN-RECORD-IN.
       01  SUPPLIER-RECORD-IN.
           05  SUPPLIER-ID-IN      PIC X(02).
           05  SUPPLIER-NAME-IN    PIC X(15).
       
       FD  SUPPLIER-FILE-OUT
           RECORD CONTAINS 17 CHARACTERS
           DATA RECORD IS SUPPLIER-RECORD-OUT.
       01  SUPPLIER-RECORD-OUT.
           05  SUPPLIER-ID-OUT    PIC X(02).
           05  SUPPLIER-NAME-OUT   PIC X(15).
           
      *-----------------------------------------------------------------     
       WORKING-STORAGE             SECTION.
      *-----------------------------------------------------------------
       01  SWITCHES-AND-COUNTERS.
           05  EOF-SW              PIC X(01).
               88  NOMORE-RECORD             VALUE "Y".
           05  INVENT-FILE-STATUS  PIC X(02).
           05  SUPPLY-FILE-STATUS  PIC X(02).
           
      ******************************************************************
       PROCEDURE                   DIVISION.
      *-----------------------------------------------------------------
      * Main procedure
      *-----------------------------------------------------------------
       100-CONVERT-FILE.
           PERFORM 200-CONVERT-INVENTORY-FILE.
           PERFORM 200-CONVERT-SUPPLIER-FILE.
           STOP RUN.
           
      ******************************************************************     
       200-CONVERT-INVENTORY-FILE.
           PERFORM 300-INITIATE-INVENT-CONVERT.
           PERFORM 300-PROCEED-INVENT-CONVERT UNTIL NOMORE-RECORD.
           PERFORM 300-TERMINATE-INVENT-CONVERT.
           
      *-----------------------------------------------------------------    
       200-CONVERT-SUPPLIER-FILE.
           PERFORM 300-INITIATE-SUPPLY-CONVERT.
           PERFORM 300-PROCEED-SUPPLY-CONVERT UNTIL NOMORE-RECORD.
           PERFORM 300-TERMINATE-SUPPLY-CONVERT.
           
      ******************************************************************     
      * Open source and target files for converting.
      * Initialize variables that used as switches 
      * Read the very first record from source file.
      *-----------------------------------------------------------------
       300-INITIATE-INVENT-CONVERT.
           PERFORM 400-OPEN-INVENT-FILES.
           PERFORM 400-INITIALIZE-SWITCHES-AND-COUNTERS.
           PERFORM 400-READ-INVENT-FILE-IN.
           
      *-----------------------------------------------------------------
      * Write target file from source file. 
      * Read next source record. 
      *-----------------------------------------------------------------
       300-PROCEED-INVENT-CONVERT.
           PERFORM 400-WRITE-INVENT-RECORD.
           PERFORM 400-READ-INVENT-FILE-IN.
           
      *-----------------------------------------------------------------
      * Display the end of program
      * close all files.
      *-----------------------------------------------------------------
       300-TERMINATE-INVENT-CONVERT.
           PERFORM 400-CLOSE-INVENT-FILES.
           PERFORM 400-DISPLAY-INVENT-EOJ.
       
      ******************************************************************     
      * Open source and target files for converting.
      * Initialize variables that used as switches 
      * Read the very first record from source file.
      *-----------------------------------------------------------------
       300-INITIATE-SUPPLY-CONVERT.
           PERFORM 400-OPEN-SUPPLY-FILES.
           PERFORM 400-INITIALIZE-SWITCHES-AND-COUNTERS.
           PERFORM 400-READ-SUPPLY-FILE-IN.
           
      *-----------------------------------------------------------------
      * Write target file from source file. 
      * Read next source record. 
      *-----------------------------------------------------------------
       300-PROCEED-SUPPLY-CONVERT.
           PERFORM 400-WRITE-SUPPLY-RECORD.
           PERFORM 400-READ-SUPPLY-FILE-IN.
           
      *-----------------------------------------------------------------
      * Display the end of program
      * close all files.
      *-----------------------------------------------------------------
       300-TERMINATE-SUPPLY-CONVERT.
           PERFORM 400-CLOSE-SUPPLY-FILES.
           PERFORM 400-DISPLAY-SUPPLY-EOJ. 
           
      ******************************************************************
       400-OPEN-INVENT-FILES.
           OPEN    INPUT   INVENT-FILE-IN
                   OUTPUT  INVENT-FILE-OUT.
                   
      *-----------------------------------------------------------------             
       400-OPEN-SUPPLY-FILES.
           OPEN    INPUT   SUPPLIER-FILE-IN
                   OUTPUT  SUPPLIER-FILE-OUT.
                   
      *-----------------------------------------------------------------
       400-INITIALIZE-SWITCHES-AND-COUNTERS.
           INITIALIZE SWITCHES-AND-COUNTERS.
      
      *----------------------------------------------------------------- 
       400-READ-INVENT-FILE-IN.
           READ INVENT-FILE-IN
                   AT END      MOVE "Y"    TO EOF-SW.
                   
      *----------------------------------------------------------------- 
       400-READ-SUPPLY-FILE-IN.
           READ SUPPLIER-FILE-IN
                   AT END      MOVE "Y"    TO EOF-SW.
                   
      *----------------------------------------------------------------- 
       400-WRITE-INVENT-RECORD.
           WRITE INVENT-RECORD-OUT FROM INVENT-RECORD-IN
               INVALID KEY DISPLAY "ERROR: " INVENT-RECORD-IN.
               
      *----------------------------------------------------------------- 
       400-WRITE-SUPPLY-RECORD.
           WRITE SUPPLIER-RECORD-OUT FROM SUPPLIER-RECORD-IN
               INVALID KEY DISPLAY "ERROR: " SUPPLIER-RECORD-IN.
               
      *----------------------------------------------------------------- 
       400-CLOSE-INVENT-FILES.
           CLOSE   INVENT-FILE-IN
                   INVENT-FILE-OUT.   
                   
      *---------------------------------------------------------------- - 
       400-CLOSE-SUPPLY-FILES.            
           CLOSE   SUPPLIER-FILE-IN
                   SUPPLIER-FILE-OUT.
                   
      *----------------------------------------------------------------- 
       400-DISPLAY-INVENT-EOJ.
           DISPLAY "INVENTORY FILE CONVERTED!!!".
           
      *----------------------------------------------------------------- 
       400-DISPLAY-SUPPLY-EOJ.
           DISPLAY "SUPPLIER FILE CONVERTED!!!".
           

     
