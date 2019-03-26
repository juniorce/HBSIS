       IDENTIFICATION DIVISION.
       PROGRAM-ID.   TELACLIENTE AS "TELACLIENTE".
       AUTHOR.       FRANCISCO.
       DATE-WRITTEN. 21/03/2019.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
          
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-CLIENTE   ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS COD-CLIENTE
                  ALTERNATE RECORD KEY IS CNPJ
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS WC-FILE-STATUS.

           SELECT IMP-CLIENTE   ASSIGN TO W-LABEL-IMP
                  ORGANIZATION  IS SEQUENTIAL
                  FILE STATUS   IS WI-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-CLIENTE
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'cadCliente'.
       COPY "CLIENTE.CPY".
       
       FD  IMP-CLIENTE
           LABEL RECORD IS STANDARD.
       01  IMP-CLIENTE-REG.
           03 IMP-COD-CLIENTE      PIC  9(007).
           03 IMP-CNPJ             PIC  9(014).
           03 IMP-RAZAO-SOCIAL     PIC  X(040).
           03 IMP-LATITUDE         PIC S9(003)V9(008).
           03 IMP-LONGITUDE        PIC S9(003)V9(008).

       WORKING-STORAGE SECTION.
       77 WOPCAO                 PIC 9      VALUE ZEROS.
       77 W-CONFIRMA             PIC X      VALUE SPACES.
       77 WC-FILE-STATUS         PIC X(002) VALUE "00".                
       77 WI-FILE-STATUS         PIC X(002) VALUE "00".                
       77 W-FIM                  PIC X      VALUE SPACES.
       77 W-RETORNO              PIC 9(001) VALUE ZEROS.
       77 W-CNPJ                 PIC 9(014) VALUE ZEROS.
       77 W-LABEL-IMP            PIC X(020) VALUE SPACES.               
       
       01  W-ARQ-CLIENTE-REG.
           03 W-CODIGO-CLI       PIC  9(007) VALUE ZEROS.
           03 W-CNPJ-CLI         PIC  9(014) VALUE ZEROS.
           03 W-RAZAO-SOCIAL     PIC  X(040) VALUE SPACES.
           03 W-LATITUDE-CLI     PIC S9(003)V9(008) VALUE ZEROS.
           03 W-LONGITUDE-CLI    PIC S9(003)V9(008) VALUE ZEROS.
       
       SCREEN SECTION.
       01 MENSAGEM-CONSULTA AUTO.
          02 BLANK SCREEN.
          02 LINE 1 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 2 COL 21 VALUE "HBSIS - Prova COBOL".
          02 LINE 3 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 04 COL 33 VALUE "MENSAGEM".
          02 LINE 07 COL 10 VALUE "REGISTRO NAO ENCONTRADO".            
          02 LINE 09 COL 10, "DESEJA FAZER OUTRA CONSULTA?(S/N):".
          02 LINE 09 COL 45, PIC X TO W-CONFIRMA.          

       01 MENU-CLIENTE.
          02 BLANK SCREEN.
          02 LINE 1 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 2 COL 21 VALUE "HBSIS - Prova COBOL".
          02 LINE 3 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 04 COL 33 VALUE "CADASTRO CLIENTE".
          02 LINE 07 COL 10 VALUE "[1] INCLUIR CLIENTE".
          02 LINE 08 COL 10 VALUE "[2] ALTERAR CLIENTE".
          02 LINE 09 COL 10 VALUE "[3] EXCLUIR CLIENTE".
          02 LINE 10 COL 10 VALUE "[4] IMPORTAR CLIENTE".
          02 LINE 11 COL 10 VALUE "[5] VOLTAR AO MENU PRINCIPAL".
          02 LINE 15 COL 10 "DIGITE A OPCAO DESEJADA[.]".
          02 LINE 15 COL 34 PIC 9 TO WOPCAO AUTO.              

       01 INCLUSAO-CLIENTE AUTO.
          02 BLANK SCREEN.
          02 LINE 1 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 2 COL 21 VALUE "HBSIS - Prova COBOL".
          02 LINE 3 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 04 COL 33 VALUE "INCLUIR CLIENTE".
          02 LINE 07 COL 10 VALUE "CODIGO CLIENTE:".
          02 LINE 07 COL 26, PIC ZZZZZZ9 TO W-CODIGO-CLI.
          02 LINE 08 COL 10 VALUE "CNPJ          :".
          02 LINE 08 COL 26, PIC 99999999999999 TO W-CNPJ-CLI.
          02 LINE 09 COL 10 VALUE "RAZAO SOCIAL  :".
          02 LINE 09 COL 26, PIC X(040) TO W-RAZAO-SOCIAL.
          02 LINE 10 COL 10 VALUE "LATITUDE      :".
          02 LINE 10 COL 26, PIC -ZZ9,99999999 TO W-LATITUDE-CLI.
          02 LINE 11 COL 10 VALUE "LONGITUDE     :".
          02 LINE 11 COL 26, PIC -ZZ9,99999999 TO W-LONGITUDE-CLI.
          02 LINE 15 COL 10 
             "CONFIRMA A INCLUSAO DO NOVO CLIENTE?(S/N):".
          02 LINE 15 COL 53, PIC X TO W-CONFIRMA.          

       01 BUSCAR-CLIENTE AUTO.
          02 BLANK SCREEN.
          02 LINE 1 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 2 COL 21 VALUE "HBSIS - Prova COBOL".
          02 LINE 3 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 04 COL 33 VALUE "CONSULTAR CLIENTE".
          02 LINE 07 COL 10 VALUE "CODIGO CLIENTE:".
          02 LINE 07 COL 26, PIC ZZZZZZ9 TO W-CODIGO-CLI.

       01 ALTERACAO-CLIENTE.
          02 BLANK SCREEN.
          02 LINE 1 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 2 COL 21 VALUE "HBSIS - Prova COBOL".
          02 LINE 3 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 04 COL 33 VALUE "ALTERAR CLIENTE".
          02 LINE 07 COL 10 VALUE "CODIGO CLIENTE:".
          02 LINE 07 COL 26, PIC ZZZZZZ9 FROM W-CODIGO-CLI.
          02 LINE 08 COL 10 VALUE "CNPJ          :".
          02 LINE 08 COL 26, PIC 99999999999999 USING W-CNPJ-CLI AUTO.  
          02 LINE 09 COL 10 VALUE "RAZAO SOCIAL  :".
          02 LINE 09 COL 26, PIC X(040) USING W-RAZAO-SOCIAL AUTO.
          02 LINE 10 COL 10 VALUE "LATITUDE      :".
          02 LINE 10 COL 26, PIC -ZZ9,99999999 USING W-LATITUDE-CLI
                                              AUTO.
          02 LINE 11 COL 10 VALUE "LONGITUDE     :".
          02 LINE 11 COL 26, PIC -ZZ9,99999999 USING W-LONGITUDE-CLI
                                              AUTO.
          02 LINE 15 COL 10 
             "CONFIRMA A ALTERACAO DO CLIENTE?(S/N):".
          02 LINE 15 COL 49, PIC X TO W-CONFIRMA AUTO.          

       01 EXCLUSAO-CLIENTE.
          02 BLANK SCREEN.
          02 LINE 1 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 2 COL 21 VALUE "HBSIS - Prova COBOL".
          02 LINE 3 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 04 COL 33 VALUE "EXCLUIR CLIENTE".
          02 LINE 07 COL 10 VALUE "CODIGO CLIENTE:".
          02 LINE 07 COL 26, PIC ZZZZZZ9 FROM W-CODIGO-CLI.
          02 LINE 08 COL 10 VALUE "CNPJ          :".
          02 LINE 08 COL 26, PIC 99999999999999 FROM W-CNPJ-CLI.
          02 LINE 09 COL 10 VALUE "RAZAO SOCIAL  :".
          02 LINE 09 COL 26, PIC X(040) FROM W-RAZAO-SOCIAL.
          02 LINE 10 COL 10 VALUE "LATITUDE      :".
          02 LINE 10 COL 26, PIC -ZZ9,99999999 FROM W-LATITUDE-CLI.
          02 LINE 11 COL 10 VALUE "LONGITUDE     :".
          02 LINE 11 COL 26, PIC -ZZ9,99999999 FROM W-LONGITUDE-CLI.
          02 LINE 15 COL 10 
             "CONFIRMA A EXCLUSAO DO CLIENTE?(S/N):".
          02 LINE 15 COL 48, PIC X TO W-CONFIRMA AUTO.      

       01 IMPORTACAO-CLIENTE AUTO.
          02 BLANK SCREEN.
          02 LINE 1 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 2 COL 21 VALUE "HBSIS - Prova COBOL".
          02 LINE 3 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 04 COL 33 VALUE "IMPORTAR CLIENTE".
          02 LINE 07 COL 10 VALUE "NOME DO ARQUIVO DE IMPORTACAO:".
          02 LINE 07 COL 41, PIC X(020) TO W-LABEL-IMP.
          02 LINE 15 COL 10 
             "CONFIRMA A IMPORTACAO?(S/N):".
          02 LINE 15 COL 39, PIC X TO W-CONFIRMA AUTO.      

       PROCEDURE DIVISION.
       000-INICIO.
           DISPLAY MENU-CLIENTE
           ACCEPT  MENU-CLIENTE
           EVALUATE WOPCAO
             WHEN 1
                PERFORM 100-INCLUSAO
             WHEN 2
                PERFORM 200-ALTERACAO
             WHEN 3
                PERFORM 300-EXCLUSAO
             WHEN 4
                PERFORM 400-IMPORTACAO
                PERFORM 000-INICIO
             WHEN 5
                CALL "MENU"
             WHEN OTHER
                PERFORM 000-INICIO
           END-EVALUATE.

       100-INCLUSAO.
           OPEN I-O ARQ-CLIENTE

           DISPLAY  INCLUSAO-CLIENTE
           ACCEPT   INCLUSAO-CLIENTE
           EVALUATE FUNCTION UPPER-CASE(W-CONFIRMA)
             WHEN 'S'
                PERFORM 110-INCLUIR-CLIENTE
                CLOSE ARQ-CLIENTE
                PERFORM 000-INICIO
             WHEN 'N'
                CLOSE ARQ-CLIENTE
                PERFORM 000-INICIO
             WHEN OTHER
                CLOSE ARQ-CLIENTE
                PERFORM 000-INICIO
           END-EVALUATE.

       110-INCLUIR-CLIENTE.
           MOVE W-CNPJ-CLI TO W-CNPJ
           MOVE W-ARQ-CLIENTE-REG TO ARQ-CLIENTE-REG

           PERFORM 500-VALIDAR-INCLUSAO-CLIENTE

           IF W-RETORNO EQUAL TO 0
              WRITE ARQ-CLIENTE-REG
           END-IF.
            
       200-ALTERACAO.
           OPEN I-O ARQ-CLIENTE

           DISPLAY BUSCAR-CLIENTE
           ACCEPT  BUSCAR-CLIENTE
           MOVE    W-CODIGO-CLI    TO COD-CLIENTE

           READ ARQ-CLIENTE RECORD INTO W-ARQ-CLIENTE-REG
             KEY IS COD-CLIENTE
           
           IF WC-FILE-STATUS NOT EQUAL TO "00"
              DISPLAY MENSAGEM-CONSULTA
              ACCEPT MENSAGEM-CONSULTA

              EVALUATE FUNCTION UPPER-CASE(W-CONFIRMA)
                WHEN 'S'
                   CLOSE ARQ-CLIENTE
                   PERFORM 200-ALTERACAO
                WHEN 'N'
                   CLOSE ARQ-CLIENTE
                   PERFORM 000-INICIO
                WHEN OTHER
                   CLOSE ARQ-CLIENTE
                   PERFORM 000-INICIO
              END-EVALUATE
           ELSE
              DISPLAY ALTERACAO-CLIENTE
              ACCEPT  ALTERACAO-CLIENTE
           
              EVALUATE FUNCTION UPPER-CASE(W-CONFIRMA)
                WHEN 'S'
                   PERFORM 210-ALTERAR-CLIENTE
                   CLOSE ARQ-CLIENTE
                   PERFORM 000-INICIO
                WHEN 'N'
                   CLOSE ARQ-CLIENTE
                   PERFORM 000-INICIO
                WHEN OTHER
                   CLOSE ARQ-CLIENTE
                   PERFORM 000-INICIO
              END-EVALUATE
           END-IF.

       210-ALTERAR-CLIENTE.
           INITIALIZE ARQ-CLIENTE-REG
           MOVE W-CODIGO-CLI      TO COD-CLIENTE 
           MOVE W-CNPJ-CLI        TO CNPJ
           MOVE W-RAZAO-SOCIAL    TO RAZAO-SOCIAL
           MOVE W-LATITUDE-CLI    TO LATITUDE-CLIENTE
           MOVE W-LONGITUDE-CLI   TO LONGITUDE-CLIENTE
           
           REWRITE ARQ-CLIENTE-REG.

       300-EXCLUSAO.
           OPEN I-O ARQ-CLIENTE

           DISPLAY BUSCAR-CLIENTE
           ACCEPT  BUSCAR-CLIENTE
           MOVE    W-CODIGO-CLI    TO COD-CLIENTE
       
           READ ARQ-CLIENTE RECORD INTO W-ARQ-CLIENTE-REG
             KEY IS COD-CLIENTE
           
           IF WC-FILE-STATUS NOT EQUAL TO "00"
              DISPLAY MENSAGEM-CONSULTA
              ACCEPT  MENSAGEM-CONSULTA

              EVALUATE FUNCTION UPPER-CASE(W-CONFIRMA)
                WHEN 'S'
                   CLOSE ARQ-CLIENTE
                   PERFORM 300-EXCLUSAO
                WHEN 'N'
                   CLOSE ARQ-CLIENTE
                   PERFORM 000-INICIO
                WHEN OTHER
                   CLOSE ARQ-CLIENTE
                   PERFORM 000-INICIO
              END-EVALUATE
           ELSE
              DISPLAY EXCLUSAO-CLIENTE
              ACCEPT  EXCLUSAO-CLIENTE
           
              EVALUATE FUNCTION UPPER-CASE(W-CONFIRMA)
                WHEN 'S'
                   PERFORM 310-EXCLUIR-CLIENTE
                   CLOSE ARQ-CLIENTE
                   PERFORM 000-INICIO
                WHEN 'N'
                   CLOSE ARQ-CLIENTE
                   PERFORM 000-INICIO
                WHEN OTHER
                   CLOSE ARQ-CLIENTE
                   PERFORM 000-INICIO
              END-EVALUATE
           END-IF.

       310-EXCLUIR-CLIENTE.
           DELETE ARQ-CLIENTE RECORD.
       
       400-IMPORTACAO.
           DISPLAY IMPORTACAO-CLIENTE
           ACCEPT  IMPORTACAO-CLIENTE
           
           EVALUATE FUNCTION UPPER-CASE(W-CONFIRMA)
              WHEN 'S'
                 IF W-LABEL-IMP EQUAL TO SPACES
                    PERFORM 400-IMPORTACAO
                 END-IF
                 PERFORM 410-IMPORTAR-CLIENTE
                 PERFORM 000-INICIO
              WHEN 'N'
                 PERFORM 000-INICIO
              WHEN OTHER
                 PERFORM 410-IMPORTAR-CLIENTE
           END-EVALUATE.

       410-IMPORTAR-CLIENTE.
           OPEN INPUT IMP-CLIENTE

           IF WI-FILE-STATUS EQUAL TO "00"
              OPEN I-O ARQ-CLIENTE        
              
              PERFORM UNTIL WI-FILE-STATUS NOT EQUAL TO "00"
                 READ IMP-CLIENTE
                 IF WI-FILE-STATUS EQUAL TO ZEROS
                    MOVE IMP-CNPJ TO W-CNPJ
                    MOVE IMP-CLIENTE-REG TO ARQ-CLIENTE-REG
                    PERFORM 500-VALIDAR-INCLUSAO-CLIENTE

                    IF W-RETORNO EQUAL TO 0
                       WRITE ARQ-CLIENTE-REG
                    END-IF
                 END-IF
              END-PERFORM
           END-IF
           
           CLOSE ARQ-CLIENTE
           CLOSE IMP-CLIENTE.

       500-VALIDAR-INCLUSAO-CLIENTE.
           MOVE 0 TO W-RETORNO

           CALL 'VALIDACNPJ' USING W-CNPJ, W-RETORNO
           
           IF W-RETORNO EQUAL TO 0
              READ ARQ-CLIENTE RECORD KEY IS COD-CLIENTE

              IF WC-FILE-STATUS EQUAL TO "00"
                 MOVE 1 TO W-RETORNO
              ELSE
                 READ ARQ-CLIENTE RECORD KEY IS CNPJ

                 IF WC-FILE-STATUS EQUAL TO "00"
                    MOVE 1 TO W-RETORNO
                 END-IF
              END-IF
           END-IF.
      
       END PROGRAM TELACLIENTE.