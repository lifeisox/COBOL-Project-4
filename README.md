This project is a team project. Please use the Team Cover Page on page XXX of this document.
PROJECT SUBMISSION REQUIREMENTS.
Your team submission must include the following documents.
1)  Team Cover Page with each team member name and signature
2)  Structure charts for each program.
3)  Program listings for each program
4)  Report listings (output) as noted in Processing Requirements
5)  Print out of Screens as noted in Processing Requirements 
Flowcharts are not required in your team submission. However, it would be wise to draft flowcharts to ensure you have the correct sequence of the modules or module content involved. 
PROCESSING REQUIREMENTS
There must be three major functions carried out. Whether you handle these in three or four separate programs is your choice.
Firstly, a batch process must be run to update the Inventory Master file using the provided Transaction File. (Refer to Master File Batch Update) 
Secondly, the updated Inventory Master File and the Suppliers file will be converted to an Indexed Sequential File. (Refer to Master File Conversion).
Thirdly, the Indexed Sequential file (Inventory Master File) must be updated with the provided transactions in an online basis.(Refer to Indexed Master File Update).
NOTE 
Your program must use the COPY and CALL commands as prescribed in any of the above functions. 
Your program must use the Case structure (EVALUATE) in the batch update to handle which of the three processes to execute (as outlined in the Balance Line Diagram).

Master File Batch Update 
The transaction file is provided as TRANSACTIONS.TXT. The records in the transaction file are in the same ascending sequence by key field (Part Number) as the master inventory file. The master inventory file is provided as INVENT4.txt.  The Suppliers file is provided as SUPPLIERS.txt. The resulting master file from this update must be named INVENT5.txt
Master File Conversion / Suppliers file conversion
After the Master File Batch Update program has been run, another program must be run that will convert two files to Indexed Sequential Files. The master inventory file and the suppliers file must be converted to indexed sequential files. Both these files will be used in the Online Update program (?).
The indexed sequential file names must be called INVENT6 (extension?) and SUPPLIERI (extension?)  so reference in the subsequent programs can be made.
NOTE  Use the SUPPLIERS file provided with this project set as it has been sorted by a key field (supplier code). DO NOT use the SUPPLIERS file from Project 3. 
Online Update Program 
The Inventory Master file, now an Indexed Sequential File will be updated using online (interactive) transactions. You must use a Screen Section in your program to enter the transaction data and apply it to the master file. 
The transactions for this update are provided below in Online Transactions

Inventory Report Program (from project 3)
You must run the report program from Project 3. However you will have to alter the reference in the Environment Division to ensure you are referring to the correct version of the file.
In this program, you must use the COPY command for the record structure of the Inventory Record.
You must use the CALL command to execute the module to compute the value of the inventory item.
Re-Order Report
You must compose the structure of the re-order report, but it must have the Inventory Number, Inventory Name, Quantity on hand and the Supplier Name. The Supplier Name must be retrieved from the Suppliers Index Sequential File (SUPPLIERI) created as an indexed sequential file (see above -- Master File Conversion / Suppliers file conversion)
Batch Process Transactions (to be announced)
Online Transactions  (to be announced) 
CST8283  Business Programming   (COBOL)
PROJECT 4


The signature is required for each member on the team (Student Signature) certifying that the work submitted with this cover page is the team members’ work and that all members contributed equally on the project. If a team member does not contribute to this team project, that should be noted in the Team Member Comments below.
Team Member Name			Team Member Signature
(print clearly)

Aradhita Mohanty			__________________________
Byung Seon Kim			__________________________
Elena Sveshnikova		__________________________
	Nadia Chubarev			__________________________


Marking Scheme (as applicable)
The project will be marked out of 100% using the following breakdown.
Refer to Notes Regarding Grading below for specific points that will be checked and influence the marks allocated. 
NOTE – if this cover page is not used, you will immediately lose 10%.
Output format and content		___   / 20
Program listing			___   / 50
Documentation 			___   / 30
			TOTAL	____   / 100

Team Member Comments

Comments / feedback  (to be made by instructor)


__________________________
Signature -- Mel (Captain COBOL) Sanschagrin

Notes Regarding Grading 
The program listing will be examined primarily for: 
1)	relationship to function chart and flowchart/pseudo code PDL); 
2)	use of prescribed commands as required by the problem specifications; 
3)	application of standards and structures; 
4)	use  of proper functional constructs (cohesion and coupling);
5)	use of internal comments;
6)	successful compilation and execution.

The output reports (hard copy or screen display) will be examined for accuracy of the output information and the prescribed format as noted in the problem specifications.

The documentation will be examined to ensure: 
1)	proper use of symbols and logical diagrams/narratives (i.e. flowcharts, PDL, or pseudo code) ;
2)	proper structure and content of  structure/function/hierarchy charts;
3)	clear and accurate report or screen layouts (if required); 
4)	clear description or comments of the program logic.  

