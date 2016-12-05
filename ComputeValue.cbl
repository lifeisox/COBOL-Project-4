      ******************************************************************
      * This program is the sub program to compute a value using given 
      * quantity and unit price.
      * 
      ******************************************************************
       IDENTIFICATION              DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                 COMPUTE-VALUE.
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
                   
      ******************************************************************
       DATA                        DIVISION.
      *-----------------------------------------------------------------     
       LINKAGE                     SECTION.
      *-----------------------------------------------------------------
       01  LINK-PARAMETERS.
           05  LS-QUANTITY         PIC 9(03).
           05  LS-UNIT-PRICE       PIC 9(04)V99.
           05  LS-VALUE            PIC 9(05)V99.
       
      ******************************************************************
       PROCEDURE                   DIVISION    USING LINK-PARAMETERS.
      *-----------------------------------------------------------------
      * Main procedure
      *-----------------------------------------------------------------
       100-COMPUTE-INVENTORY-VALUE.    
           COMPUTE LS-VALUE = LS-QUANTITY * LS-UNIT-PRICE.
           
           EXIT    PROGRAM.
           
