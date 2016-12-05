      ******************************************************************
      * This program is to print Inventory report and Reorder report.
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
       PROGRAM-ID.                 INVENTORY-REPORT.
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
           SELECT  SUPPLIER-FILE-IN   
                   ASSIGN TO "D:\SUPPLIER"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS RANDOM
                   RECORD KEY IS SP-SUPPLIER-ID
                   FILE STATUS IS SUPPLIER-FILE-STAT.
                   
           SELECT  INVENT-FILE-IN   
                   ASSIGN TO "D:\INVENT6"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS SEQUENTIAL
                   RECORD KEY IS PART-NUMBER-IN
                   FILE STATUS IS INVENT-FILE-STAT.
                   
           SELECT  INVENT-REPORT-OUT   
                   ASSIGN TO "D:\INVREPRT.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
                   
           SELECT  REORDER-REPORT-OUT        
                   ASSIGN TO "D:\REORDER.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
                   
      ******************************************************************
       DATA                        DIVISION.
      *-----------------------------------------------------------------
       FILE                        SECTION.
       FD  SUPPLIER-FILE-IN
           RECORD CONTAINS 17 CHARACTERS
           DATA RECORD IS SUPPLIER-RECORD-IN.
       01  SUPPLIER-RECORD-IN.
           05  SP-SUPPLIER-ID      PIC X(02).
           05  SP-SUPPLIER-NAME    PIC X(15).
           
       FD  INVENT-FILE-IN
           RECORD CONTAINS 39 CHARACTERS
           DATA RECORD IS INVENT-RECORD-IN.
       COPY "C:\Copybooks\InventRecord.cpy".
           
       FD  INVENT-REPORT-OUT
           RECORD CONTAINS 53 CHARACTERS
           DATA RECORD IS INVENTORY-OUT.
       01  INVENTORY-OUT               PIC X(53).
       
       FD  REORDER-REPORT-OUT
           RECORD CONTAINS 49 CHARACTERS
           DATA RECORD IS REORDER-OUT.
       01  REORDER-OUT                 PIC X(49).
      *-----------------------------------------------------------------     
       WORKING-STORAGE             SECTION.
      *-----------------------------------------------------------------
      *    This record is for getting the name of month.
       01  DAY-RECORD.
           05  FILLER              PIC X(09) VALUE "Monday".
           05  FILLER              PIC X(09) VALUE "Tuesday".
           05  FILLER              PIC X(09) VALUE "Wednesday".
           05  FILLER              PIC X(09) VALUE "Thursday".
           05  FILLER              PIC X(09) VALUE "Friday".
           05  FILLER              PIC X(09) VALUE "Saturday".
           05  FILLER              PIC X(09) VALUE "Sunday".
       01  DAY-TABLE               REDEFINES DAY-RECORD.
           05 WEEKDAY              PIC X(09) OCCURS 7 TIMES.
      
      *    This record is for printing the title of inventory report.
       01  INVENT-TITLE.
           05  FILLER              PIC X(09) VALUE SPACES.
           05  FILLER              PIC X(22) 
                                   VALUE "INVENTORY REPORT for (".
           05  DAY-NAME            PIC X(10).
           05  DSP-DATE.
               10  DSP-YEAR        PIC 9(04).
               10  FILLER          PIC X(01) VALUE "/".
               10  DSP-MONTH       PIC 9(02).
               10  FILLER          PIC X(01) VALUE "/".
               10  DSP-DAY         PIC 9(02).
           05  FILLER              PIC X(01) VALUE ")".
           
      *    This record is for printing the header of inventory report.
       01  INVENT-HEADER.
           05  FILLER              PIC X(01) VALUE SPACES.
           05  FILLER              PIC X(08) VALUE "PART NO".
           05  FILLER              PIC X(22) VALUE "PART NAME".
           05  FILLER              PIC X(05) VALUE " OH".
           05  FILLER              PIC X(08) VALUE "PRICE".
           05  FILLER              PIC X(08) VALUE "   VALUE".
      
      *    This record is for printing the detail of inventory report.
       01  INVENT-DETAIL.
           05  FILLER              PIC X(01) VALUE SPACES.
           05  PART-NUMBER-O       PIC X(05).
           05  FILLER              PIC X(03) VALUE SPACES.
           05  PART-NAME-O         PIC X(20).
           05  FILLER              PIC X(01) VALUE SPACES.
           05  QUANTITY-O          PIC ZZZ9.
           05  FILLER              PIC X(01) VALUE SPACES.
           05  UNIT-PRICE-O        PIC ZZ9.99.
           05  FILLER              PIC X(02) VALUE SPACES.
           05  VALUE-O             PIC $$$,$$9.99.
           
      *    This record is for printing the total of inventory report.
       01  INVENT-TOTAL.
           05  FILLER              PIC X(02) VALUE SPACES.
           05  FILLER              PIC X(13) VALUE "TOTAL VALUE".
           05  TOTAL               PIC $$$$,$$9.99.
       
      *    This record is for print the footer of inventory report.
       01  INVENT-FOOTER.   
           05  FILLER              PIC X(02) VALUE SPACES.
           05  FOOTER-NAME         PIC X(15).
           05  FOOTER-COUNTER      PIC ZZZ9.
           
      *    This record is for print the title of reorder report.
       01  REORDER-TITLE.
           05  FILLER              PIC X(01) VALUE SPACES.
           05  FILLER              PIC X(20) VALUE "REORDER REPORT".
      
      *    This record is for printing the header of reorder report.
       01  REORDER-HEADER.
           05  FILLER              PIC X(01) VALUE SPACES.
           05  FILLER              PIC X(08) VALUE "PART NO".
           05  FILLER              PIC X(21) VALUE "PART NAME".
           05  FILLER              PIC X(04) VALUE "CSL".
           05  FILLER              PIC X(15) VALUE "SUPPLIER NAME".
      
      *    This record is for printing the detail of reorder report.
       01  REORDER-DETAIL.
           05  FILLER              PIC X(01) VALUE SPACES.
           05  PART-NUMBER-R       PIC X(05).
           05  FILLER              PIC X(03) VALUE SPACES.
           05  PART-NAME-R         PIC X(20).
           05  FILLER              PIC X(01) VALUE SPACES.
           05  REORDER-POINT-R     PIC ZZ9.
           05  FILLER              PIC X(01) VALUE SPACES.
           05  SUPPLIER-NAME-R     PIC X(15).

       01  SWITCHES-AND-COUNTERS.
           05  EOF-SW              PIC X(01) VALUE "N".
           05  FOUND-SW            PIC X(01) VALUE "N".
           05  READ-CNT            PIC 9(03) VALUE ZEROS.
           05  WRITE-CNT           PIC 9(03).
           05  LINE-CNT            PIC 9(02).
           
       01  ACCUMULATORS.
           05  GRAND-TOTAL         PIC 9(09)V99 VALUE ZEROS.
       
       01  CURRENT-DATE.
           05  CUR-YEAR            PIC 9(04).
           05  CUR-MONTH           PIC 9(02).
           05  CUR-DAY             PIC 9(02).
           
       01  DAY-IN                  PIC 9(01).
       
       01  FILE-STATUS.
           05  INVENT-FILE-STAT    PIC X(02).
           05  SUPPLIER-FILE-STAT  PIC X(02).
       
       01  LINK-PARAMETERS.
           05  LS-QUANTITY         PIC 9(03).
           05  LS-UNIT-PRICE       PIC 9(04)V99.
           05  LS-VALUE            PIC 9(05)V99.
           
      ******************************************************************
       PROCEDURE                   DIVISION.
      *-----------------------------------------------------------------
      * Main procedure
      *-----------------------------------------------------------------
       100-PRINT-INVENTORY-REPORTS.
           PERFORM 200-INITIATE-INVENTORY-REPORTS.
           PERFORM 200-PRINT-INVENTORY-REPORTS UNTIL EOF-SW = "Y".
           PERFORM 200-TERMINATE-INVENTORY-REPORTS.
           
           STOP RUN.
           
      ******************************************************************     
      * Open input & print file, initialize variables that used as 
      * switch and for sum, print report headers, and 
      * read the very first record from input file.
      *-----------------------------------------------------------------
       200-INITIATE-INVENTORY-REPORTS.
           PERFORM 300-OPEN-INVENTORY-FILES.
           PERFORM 300-INITIALIZE-SWITCHES-AND-COUNTERS.
           PERFORM 300-READ-INVENTORY-FILE.
           PERFORM 300-PRINT-REPORT-TITLES.
           PERFORM 300-PRINT-REPORT-HEADERS.
           
      *-----------------------------------------------------------------
      * Print one inventory record and reorder record as given format 
      *  and read next record. when reorder is printed, only prit if 
      *  quanty is less than or equal to re-order point
      *-----------------------------------------------------------------
       200-PRINT-INVENTORY-REPORTS.
           IF  LINE-CNT > 10
               PERFORM 300-INVENT-REPORT-PAGESKIP.
           PERFORM 300-COMPUTE-INVENTORY-VALUE.
           PERFORM 300-PRINT-INVENTORY-DETAIL.
           PERFORM 300-COMPUTE-GRAND-TOTAL.
           IF QUANTITY-IN NOT GREATER THAN REORDER-POINT-IN
               PERFORM 300-INITIALIZE-BEFORE-SEARCH-SUPPLIER
               PERFORM 300-SEARCH-SUPPLIER
               PERFORM 300-PRINT-REORDER-DETAIL.
           PERFORM 300-READ-INVENTORY-FILE.
           
      *-----------------------------------------------------------------
      * after printed all records, total and footer should be printed.
      * after that close all files.
      *-----------------------------------------------------------------
       200-TERMINATE-INVENTORY-REPORTS.
           PERFORM 300-PRINT-INVENT-REPORT-TOTAL.
           PERFORM 300-PRINT-INVENT-REPORT-FOOTER.
           PERFORM 300-CLOSE-INVENTORY-FILES.
           
      ******************************************************************
      * open input file and output file to print                                          
      *-----------------------------------------------------------------
       300-OPEN-INVENTORY-FILES.
           OPEN    INPUT   INVENT-FILE-IN
                   INPUT   SUPPLIER-FILE-IN
                   OUTPUT  INVENT-REPORT-OUT
                   OUTPUT  REORDER-REPORT-OUT.
      
      *-----------------------------------------------------------------
      * initialize variables
      *-----------------------------------------------------------------
       300-INITIALIZE-SWITCHES-AND-COUNTERS.
           INITIALIZE SWITCHES-AND-COUNTERS.
      
      *----------------------------------------------------------------- 
      * read a input record. if eof then set EOF-SW as 'Yes'
      * if not, add 1 to read record count
      *-----------------------------------------------------------------
       300-READ-INVENTORY-FILE.
           READ INVENT-FILE-IN
                   AT END      MOVE "Y" TO EOF-SW
                   NOT AT END  ADD 1 TO READ-CNT
                               ADD 1 TO LINE-CNT.

      *-----------------------------------------------------------------
      * print title of inventory report and reorder report. 
      *-----------------------------------------------------------------
       300-PRINT-REPORT-TITLES.
           PERFORM 400-PRINT-INVENT-REPORT-TITLE.
           PERFORM 400-PRINT-REORDER-REPORT-TITLE.
      
      *-----------------------------------------------------------------
      * print header of inventory report and reorder report. 
      *-----------------------------------------------------------------
       300-PRINT-REPORT-HEADERS.
           PERFORM 400-PRINT-INVENT-REPORT-HEADER.
           PERFORM 400-PRINT-REORDER-REPORT-HEADER.
                                                                        
      *-----------------------------------------------------------------
      * print header of inventory report and reorder report. 
      *-----------------------------------------------------------------
       300-COMPUTE-INVENTORY-VALUE.
           MOVE    QUANTITY-IN     TO  LS-QUANTITY.
           MOVE    UNIT-PRICE-IN   TO  LS-UNIT-PRICE.
           CALL    "ComputeValue"  USING LINK-PARAMETERS.
        
      *-----------------------------------------------------------------
      * After printing every 10 records, skip a page.  
      *-----------------------------------------------------------------                                 
       300-INVENT-REPORT-PAGESKIP.
           PERFORM 400-PAGE-SKIP.
           PERFORM 400-PRINT-INVENT-REPORT-HEADER.
           MOVE    ZEROS       TO  LINE-CNT.
           
      *-----------------------------------------------------------------
      * print inventory detail. After printed, add 1 to write counter
      * and add amount to total amount.
      *-----------------------------------------------------------------
       300-PRINT-INVENTORY-DETAIL.
           MOVE    PART-NUMBER-IN      TO  PART-NUMBER-O.
           MOVE    PART-NAME-IN        TO  PART-NAME-O.
           MOVE    QUANTITY-IN         TO  QUANTITY-O.
           MOVE    UNIT-PRICE-IN       TO  UNIT-PRICE-O.
           MOVE    LS-VALUE            TO  VALUE-O.
           WRITE   INVENTORY-OUT       FROM    INVENT-DETAIL.
           ADD     1                   TO  WRITE-CNT.
           
      *-----------------------------------------------------------------
      * print header of inventory report and reorder report. 
      *-----------------------------------------------------------------
       300-COMPUTE-GRAND-TOTAL.    
           ADD     LS-VALUE    TO  GRAND-TOTAL.
      
      *----------------------------------------------------------------- 
      * print RE-order detail print  
      *-----------------------------------------------------------------
       300-PRINT-REORDER-DETAIL.
           MOVE    PART-NUMBER-IN   TO  PART-NUMBER-R.
           MOVE    PART-NAME-IN     TO  PART-NAME-R.
           MOVE    REORDER-POINT-IN TO  REORDER-POINT-R.
           WRITE   REORDER-OUT     FROM    REORDER-DETAIL.

      *----------------------------------------------------------------- 
      * close files  
      *-----------------------------------------------------------------
       300-CLOSE-INVENTORY-FILES.
           CLOSE   INVENT-FILE-IN
                   SUPPLIER-FILE-IN
                   INVENT-REPORT-OUT
                   REORDER-REPORT-OUT.

      *----------------------------------------------------------------- 
      * initialize before searching supplier name using id.  
      *-----------------------------------------------------------------
       300-INITIALIZE-BEFORE-SEARCH-SUPPLIER.
           MOVE    SPACES  TO  SUPPLIER-NAME-R.
           MOVE    "N"     TO  FOUND-SW.
       
      *----------------------------------------------------------------- 
      * in order to search supplier. 
      *-----------------------------------------------------------------
       300-SEARCH-SUPPLIER.
           MOVE    SUPPLIER-ID-IN  TO  SP-SUPPLIER-ID.
           READ    SUPPLIER-FILE-IN
                   INVALID KEY     
                       MOVE    SPACES              TO  SUPPLIER-NAME-R
                   NOT INVALID KEY 
                       MOVE    SP-SUPPLIER-NAME    TO  SUPPLIER-NAME-R.

      *----------------------------------------------------------------- 
      * print total of inventory report. 
      *-----------------------------------------------------------------
       300-PRINT-INVENT-REPORT-TOTAL.
           MOVE    GRAND-TOTAL     TO TOTAL.
           WRITE   INVENTORY-OUT   FROM    INVENT-TOTAL
                   AFTER ADVANCING 4   LINES.

      *----------------------------------------------------------------- 
      * print footers of inventory report 
      *-----------------------------------------------------------------
       300-PRINT-INVENT-REPORT-FOOTER.
           MOVE    "RECORDS READ"      TO  FOOTER-NAME.
           MOVE    READ-CNT            TO  FOOTER-COUNTER.
           WRITE   INVENTORY-OUT   FROM    INVENT-FOOTER
                   AFTER ADVANCING 2   LINES.
           MOVE    "RECORDS WRITTEN"   TO  FOOTER-NAME.
           MOVE    WRITE-CNT           TO  FOOTER-COUNTER.
           WRITE   INVENTORY-OUT   FROM    INVENT-FOOTER.
           
      *-----------------------------------------------------------------
      * print title of inventory report. 
      *-----------------------------------------------------------------
       400-PRINT-INVENT-REPORT-TITLE.  
           ACCEPT  CURRENT-DATE        FROM DATE YYYYMMDD.
           MOVE    CUR-YEAR            TO  DSP-YEAR.
           MOVE    CUR-MONTH           TO  DSP-MONTH.
           MOVE    CUR-DAY             TO  DSP-DAY.
           ACCEPT  DAY-IN              FROM DAY-OF-WEEK.
           MOVE    WEEKDAY(DAY-IN)     TO  DAY-NAME.
           WRITE   INVENTORY-OUT       FROM    INVENT-TITLE
                   AFTER ADVANCING 1   LINES.
           
      *-----------------------------------------------------------------
      * print title of reorder report. 
      *-----------------------------------------------------------------
       400-PRINT-REORDER-REPORT-TITLE.
           WRITE   REORDER-OUT         FROM    REORDER-TITLE
                   AFTER ADVANCING 1   LINES.
      
      *-----------------------------------------------------------------    
      * print header of inventory report. 
      *-----------------------------------------------------------------
       400-PRINT-INVENT-REPORT-HEADER.
           WRITE   INVENTORY-OUT       FROM INVENT-HEADER
                   AFTER ADVANCING 3   LINES.
           MOVE    SPACES      TO INVENTORY-OUT.
           WRITE   INVENTORY-OUT.
           
      *-----------------------------------------------------------------
      * print header of RE-ORDER report.  
      *-----------------------------------------------------------------                                 
       400-PRINT-REORDER-REPORT-HEADER.
           WRITE   REORDER-OUT         FROM REORDER-HEADER
                   AFTER ADVANCING 2   LINES.
           MOVE    SPACES       TO REORDER-OUT.
           WRITE   REORDER-OUT.
        
      *-----------------------------------------------------------------
      * empty print after page.  
      *-----------------------------------------------------------------                                 
       400-PAGE-SKIP.
           MOVE    SPACES      TO  INVENTORY-OUT.
           WRITE   INVENTORY-OUT   AFTER ADVANCING PAGE.

           
