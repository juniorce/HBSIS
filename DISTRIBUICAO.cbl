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
       77 W-DLA                  PIC  9(009)V9(004) VALUE ZEROS.
       77 W-DLA2                 PIC  9(009)V9(004) VALUE ZEROS.
       77 W-DLO                  PIC  9(009)V9(004) VALUE ZEROS.
       77 W-DLO2                 PIC  9(009)V9(004) VALUE ZEROS.
       77 W-RAIZ2                PIC  9(009)V9(004) VALUE ZEROS.

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
           COMPUTE W-DLA = (LATITUDE-CLIENTE 
                         -  LATITUDE-VENDEDOR) 
                         *  1852

           COMPUTE W-DLO = (LONGITUDE-CLIENTE 
                         -  LONGITUDE-VENDEDOR)
                         *  1852

           COMPUTE W-DLA2 = W-DLA * W-DLA
           COMPUTE W-DLO2 = W-DLO * W-DLO
           COMPUTE W-RAIZ2 = W-DLA2 + W-DLO2
           COMPUTE W-CALC-DISTANCIA = FUNCTION SQRT(W-RAIZ2)
                                                    
           IF W-CALC-DISTANCIA < W-MENOR-DISTANCIA
              MOVE W-CALC-DISTANCIA TO W-MENOR-DISTANCIA
              MOVE COD-VENDEDOR     TO D-COD-VENDEDOR
           END-IF
           
           READ ARQ-VENDEDOR NEXT.

       END PROGRAM DISTRIBUICAO.