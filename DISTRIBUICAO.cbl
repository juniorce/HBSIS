       PROGRAM-ID.   DISTRIBUICAO AS "DISTRIBUICAO".
       AUTHOR.       FRANCISCO.
       DATE-WRITTEN. 21/03/2019.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-VENDEDOR  ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS COD-VENDEDOR
                  ALTERNATE RECORD KEY IS CPF
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS WV-FILE-STATUS.

           SELECT ARQ-CLIENTE   ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS COD-CLIENTE
                  ALTERNATE RECORD KEY IS CNPJ
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS WC-FILE-STATUS.

           SELECT ARQ-DISTRIBUICAO ASSIGN TO DISK
                  ORGANIZATION     IS LINE SEQUENTIAL
                  FILE STATUS      IS WD-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-VENDEDOR
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'cadVendedor'.
       COPY "VENDEDOR.CPY".
       
       FD  ARQ-CLIENTE
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'cadCliente'.
       COPY "CLIENTE.CPY".
       
       FD  ARQ-DISTRIBUICAO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'cadDistribuicao'.
       COPY "DISTRIBUICAO.CPY".

       WORKING-STORAGE SECTION.
       77 WV-FILE-STATUS         PIC  X(002) VALUE "00".
       77 WC-FILE-STATUS         PIC  X(002) VALUE "00".
       77 WD-FILE-STATUS         PIC  X(002) VALUE "00".
       77 W-MENOR-DISTANCIA      PIC  9(009)V9(002) VALUE 999999999.
       77 W-CALC-DISTANCIA       PIC  9(009)V9(002) VALUE ZEROS.
       77 W-LATITUDE-1           PIC S9(003)V9(008) VALUE ZEROS.
       77 W-LATITUDE-2           PIC S9(003)V9(008) VALUE ZEROS.
       77 W-LONGITUDE-1          PIC S9(003)V9(008) VALUE ZEROS.
       77 W-LONGITUDE-2          PIC S9(003)V9(008) VALUE ZEROS.
       77 W-DLA                  PIC S9(003)V9(008) VALUE ZEROS.        
       77 W-DLO                  PIC S9(003)V9(008) VALUE ZEROS.        
       77 W-A                    PIC S9(003)V9(008) VALUE ZEROS.        
       77 W-C                    PIC S9(003)V9(008) VALUE ZEROS.        

       PROCEDURE DIVISION.
       000-INCIIO.
           OPEN INPUT ARQ-CLIENTE
           OPEN OUTPUT ARQ-DISTRIBUICAO
           
           READ ARQ-CLIENTE NEXT
           
           PERFORM 100-LER-CLIENTE UNTIL WC-FILE-STATUS NOT EQUAL "00"
           
           CLOSE ARQ-CLIENTE
           CLOSE ARQ-VENDEDOR
           CLOSE ARQ-DISTRIBUICAO
           
           CHAIN "REPORTDISTRIBUICAO".
       
       100-LER-CLIENTE.
           MOVE COD-CLIENTE TO D-COD-CLIENTE

           OPEN INPUT ARQ-VENDEDOR
           READ ARQ-VENDEDOR NEXT
           
           PERFORM 200-LER-VENDEDOR UNTIL WV-FILE-STATUS NOT EQUAL "00"
           
           MOVE  W-MENOR-DISTANCIA TO DISTANCIA
           MOVE  999999999         TO W-MENOR-DISTANCIA 
           WRITE ARQ-DISTRIBUICAO-REG

           CLOSE ARQ-VENDEDOR
           
           READ ARQ-CLIENTE NEXT.
       
       200-LER-VENDEDOR.
           COMPUTE W-LATITUDE-1 = LATITUDE-CLIENTE
                                * FUNCTION PI
                                / 180
       
           COMPUTE W-LATITUDE-2 = LATITUDE-VENDEDOR
                                * FUNCTION PI
                                / 180

           COMPUTE W-LONGITUDE-1 = LONGITUDE-CLIENTE
                                * FUNCTION PI
                                / 180
                              
           COMPUTE W-LONGITUDE-2 = LONGITUDE-VENDEDOR
                                * FUNCTION PI
                                / 180

           COMPUTE W-DLA = W-LATITUDE-2 - (W-LATITUDE-1) 

           COMPUTE W-DLO = W-LONGITUDE-2 - (W-LONGITUDE-1) 

           COMPUTE W-A = FUNCTION SIN(W-DLA / 2)
                       * FUNCTION SIN(W-DLA / 2)
                       + FUNCTION COS(W-LATITUDE-1)
                       * FUNCTION COS(W-LATITUDE-2)
                       * FUNCTION SIN(W-DLO / 2)
                       * FUNCTION SIN(W-DLO / 2)
           
           COMPUTE W-C = 2 * FUNCTION ATAN(FUNCTION SQRT(W-A) /
                                           FUNCTION SQRT(1 - W-A))

           COMPUTE W-CALC-DISTANCIA = 6731 * W-C * 1000
                                                    
           IF W-CALC-DISTANCIA < W-MENOR-DISTANCIA
              MOVE W-CALC-DISTANCIA TO W-MENOR-DISTANCIA
              MOVE COD-VENDEDOR     TO D-COD-VENDEDOR
           END-IF
           
           READ ARQ-VENDEDOR NEXT.

       END PROGRAM DISTRIBUICAO.