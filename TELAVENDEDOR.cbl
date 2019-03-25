       IDENTIFICATION DIVISION.
       PROGRAM-ID.   TELAVENDEDOR AS "TELAVENDEDOR".
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

           SELECT IMP-VENDEDOR  ASSIGN TO "IMPVENDEDOR.DAT"
                  ORGANIZATION  IS SEQUENTIAL
                  FILE STATUS   IS WI-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-VENDEDOR
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'cadVendedor'.
       COPY "VENDEDOR.CPY".

       FD  IMP-VENDEDOR
           LABEL RECORD IS STANDARD.
       01  IMP-VENDEDOR-REG.
           03 IMP-COD-VENDEDOR     PIC  9(003).
           03 IMP-CPF              PIC  9(011).
           03 IMP-NOME-VENDEDOR    PIC  X(040).
           03 IMP-LATITUDE         PIC S9(003)V9(008).
           03 IMP-LONGITUDE        PIC S9(003)V9(008).

       WORKING-STORAGE SECTION.
       77 WOPCAO                 PIC  9      VALUE ZEROS.
       77 W-CONFIRMA             PIC  X      VALUE SPACES.
       77 WV-FILE-STATUS         PIC  X(002) VALUE "00".                
       77 WI-FILE-STATUS         PIC  X(002) VALUE "00".      
       77 W-FIM                  PIC  X      VALUE SPACES.
       77 W-RETORNO              PIC  9(001) VALUE ZEROS.
       77 W-CPF                  PIC  9(011) VALUE ZEROS.
       
       01  W-ARQ-VENDEDOR-REG.
           03 W-CODIGO-VEND      PIC  9(003) VALUE ZEROS.
           03 W-CPF-VENDEDOR     PIC  9(011) VALUE ZEROS.
           03 W-NOME-VENDEDOR    PIC  X(040) VALUE SPACES.
           03 W-LATITUDE-VEND    PIC S9(003)V9(008) VALUE ZEROS.
           03 W-LONGITUDE-VEND   PIC S9(003)V9(008) VALUE ZEROS.
                 
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

       01 MENU-VENDEDOR.
          02 BLANK SCREEN.
          02 LINE 1 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 2 COL 21 VALUE "HBSIS - Prova COBOL".
          02 LINE 3 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 04 COL 33 VALUE "CADASTRO VENDEDOR".
          02 LINE 07 COL 10 VALUE "[1] INCLUIR VENDEDOR".
          02 LINE 08 COL 10 VALUE "[2] ALTERAR VENDEDOR".
          02 LINE 09 COL 10 VALUE "[3] EXCLUIR VENDEDOR".
          02 LINE 10 COL 10 VALUE "[4] IMPORTAR VENDEDOR".
          02 LINE 11 COL 10 VALUE "[5] VOLTAR AO MENU PRINCIPAL".
          02 LINE 15 COL 10 "DIGITE A OPCAO DESEJADA[.]".
          02 LINE 15 COL 34 PIC 9 TO WOPCAO AUTO.              

       01 INCLUSAO-VENDEDOR AUTO.
          02 BLANK SCREEN.
          02 LINE 1 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 2 COL 21 VALUE "HBSIS - Prova COBOL".
          02 LINE 3 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 04 COL 33 VALUE "INCLUIR VENDEDOR".
          02 LINE 07 COL 10 VALUE "CODIGO VENDEDOR:".
          02 LINE 07 COL 27, PIC ZZ9 TO W-CODIGO-VEND.
          02 LINE 08 COL 10 VALUE "CPF            :".
          02 LINE 08 COL 27, PIC 99999999999 TO W-CPF-VENDEDOR.
          02 LINE 09 COL 10 VALUE "NOME VENDEDOR  :".
          02 LINE 09 COL 27, PIC X(040) TO W-NOME-VENDEDOR.
          02 LINE 10 COL 10 VALUE "LATITUDE       :".
          02 LINE 10 COL 27, PIC -ZZ9,99999999 TO W-LATITUDE-VEND.
          02 LINE 11 COL 10 VALUE "LONGITUDE      :".
          02 LINE 11 COL 27, PIC -ZZ9,99999999 TO W-LONGITUDE-VEND.
          02 LINE 15 COL 10 
             "CONFIRMA A INCLUSAO DO NOVO VENDEDOR?(S/N):".
          02 LINE 15 COL 54, PIC X TO W-CONFIRMA.
          
       01 BUSCAR-VENDEDOR AUTO.
          02 BLANK SCREEN.
          02 LINE 1 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 2 COL 21 VALUE "HBSIS - Prova COBOL".
          02 LINE 3 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 04 COL 33 VALUE "CONSULTAR VENDEDOR".
          02 LINE 07 COL 10 VALUE "CODIGO VENDEDOR:".
          02 LINE 07 COL 27, PIC ZZ9 TO W-CODIGO-VEND.

       01 ALTERACAO-VENDEDOR.
          02 BLANK SCREEN.
          02 LINE 1 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 2 COL 21 VALUE "HBSIS - Prova COBOL".
          02 LINE 3 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 04 COL 33 VALUE "ALTERAR VENDEDOR".
          02 LINE 07 COL 10 VALUE "CODIGO VENDEDOR:".
          02 LINE 07 COL 27, PIC ZZ9 FROM W-CODIGO-VEND.
          02 LINE 08 COL 10 VALUE "CPF            :".
          02 LINE 08 COL 27, PIC 99999999999 USING W-CPF-VENDEDOR AUTO.
          02 LINE 09 COL 10 VALUE "NOME VENDEDOR  :".
          02 LINE 09 COL 27, PIC X(040) USING W-NOME-VENDEDOR AUTO.
          02 LINE 10 COL 10 VALUE "LATITUDE       :".
          02 LINE 10 COL 27, PIC -ZZ9,99999999 USING W-LATITUDE-VEND 
                                              AUTO.
          02 LINE 11 COL 10 VALUE "LONGITUDE      :".
          02 LINE 11 COL 27, PIC -ZZ9,99999999 USING W-LONGITUDE-VEND
                                              AUTO.
          02 LINE 15 COL 10 
             "CONFIRMA A ALTERACAO DO VENDEDOR?(S/N):".
          02 LINE 15 COL 50, PIC X TO W-CONFIRMA AUTO.          

       01 EXCLUSAO-VENDEDOR.
          02 BLANK SCREEN.
          02 LINE 1 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 2 COL 21 VALUE "HBSIS - Prova COBOL".
          02 LINE 3 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 04 COL 33 VALUE "EXCLUIR VENDEDOR".
          02 LINE 07 COL 10 VALUE "CODIGO VENDEDOR:".
          02 LINE 07 COL 27, PIC ZZ9 FROM W-CODIGO-VEND.
          02 LINE 08 COL 10 VALUE "CPF            :".
          02 LINE 08 COL 27, PIC 99999999999 FROM W-CPF-VENDEDOR.
          02 LINE 09 COL 10 VALUE "NOME VENDEDOR  :".
          02 LINE 09 COL 27, PIC X(040) FROM W-NOME-VENDEDOR.
          02 LINE 10 COL 10 VALUE "LATITUDE       :".
          02 LINE 10 COL 27, PIC -ZZ9,99999999 FROM W-LATITUDE-VEND.
          02 LINE 11 COL 10 VALUE "LONGITUDE      :".
          02 LINE 11 COL 27, PIC -ZZ9,99999999 FROM W-LONGITUDE-VEND.
          02 LINE 15 COL 10 
             "CONFIRMA A EXCLUSAO DO VENDEDOR?(S/N):".
          02 LINE 15 COL 49, PIC X TO W-CONFIRMA AUTO.      

       PROCEDURE DIVISION.
       000-INICIO.
           DISPLAY MENU-VENDEDOR
           ACCEPT  MENU-VENDEDOR
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
           OPEN I-O ARQ-VENDEDOR

           DISPLAY  INCLUSAO-VENDEDOR
           ACCEPT   INCLUSAO-VENDEDOR
           EVALUATE W-CONFIRMA
             WHEN 'S'
                PERFORM 110-INCLUIR-VENDEDOR
                CLOSE ARQ-VENDEDOR
                PERFORM 000-INICIO
             WHEN 's'
                PERFORM 110-INCLUIR-VENDEDOR
                CLOSE ARQ-VENDEDOR
                PERFORM 000-INICIO
             WHEN 'N' 
                CLOSE ARQ-VENDEDOR
                PERFORM 000-INICIO
             WHEN 'n' 
                CLOSE ARQ-VENDEDOR
                PERFORM 000-INICIO
             WHEN OTHER
                CLOSE ARQ-VENDEDOR
                PERFORM 000-INICIO
           END-EVALUATE.

       110-INCLUIR-VENDEDOR.
           MOVE W-CPF-VENDEDOR TO W-CPF
           MOVE W-ARQ-VENDEDOR-REG TO ARQ-VENDEDOR-REG

           PERFORM 500-VALIDAR-INCLUSAO-VENDEDOR

           IF W-RETORNO EQUAL TO 0
              WRITE ARQ-VENDEDOR-REG
           END-IF.
       
       200-ALTERACAO.
           OPEN I-O ARQ-VENDEDOR

           DISPLAY BUSCAR-VENDEDOR
           ACCEPT  BUSCAR-VENDEDOR
           MOVE    W-CODIGO-VEND    TO COD-VENDEDOR

           READ ARQ-VENDEDOR RECORD INTO W-ARQ-VENDEDOR-REG
             KEY IS COD-VENDEDOR
           
           IF WV-FILE-STATUS NOT EQUAL TO "00"
              DISPLAY MENSAGEM-CONSULTA
              ACCEPT MENSAGEM-CONSULTA

              EVALUATE W-CONFIRMA
                WHEN 'S'
                   CLOSE ARQ-VENDEDOR
                   PERFORM 200-ALTERACAO
                WHEN 's'
                   CLOSE ARQ-VENDEDOR
                   PERFORM 200-ALTERACAO
                WHEN 'N'
                   CLOSE ARQ-VENDEDOR
                   PERFORM 000-INICIO
                WHEN 'n'
                   CLOSE ARQ-VENDEDOR
                   PERFORM 000-INICIO
                WHEN OTHER
                   CLOSE ARQ-VENDEDOR
                   PERFORM 000-INICIO
              END-EVALUATE
           ELSE
              DISPLAY ALTERACAO-VENDEDOR
              ACCEPT  ALTERACAO-VENDEDOR
           
              EVALUATE W-CONFIRMA
                WHEN 'S'
                   PERFORM 210-ALTERAR-VENDEDOR
                   CLOSE ARQ-VENDEDOR
                   PERFORM 000-INICIO
                WHEN 's'
                   PERFORM 210-ALTERAR-VENDEDOR
                   CLOSE ARQ-VENDEDOR
                   PERFORM 000-INICIO
                WHEN 'N'
                   CLOSE ARQ-VENDEDOR
                   PERFORM 000-INICIO
                WHEN 'n'
                   CLOSE ARQ-VENDEDOR
                   PERFORM 000-INICIO
                WHEN OTHER
                   CLOSE ARQ-VENDEDOR
                   PERFORM 000-INICIO
              END-EVALUATE
           END-IF.

       210-ALTERAR-VENDEDOR.
           INITIALIZE ARQ-VENDEDOR-REG
           MOVE W-CODIGO-VEND      TO COD-VENDEDOR 
           MOVE W-CPF-VENDEDOR     TO CPF
           MOVE W-NOME-VENDEDOR    TO NOME-VENDEDOR
           MOVE W-LATITUDE-VEND    TO LATITUDE-VENDEDOR
           MOVE W-LONGITUDE-VEND   TO LONGITUDE-VENDEDOR
           
           REWRITE ARQ-VENDEDOR-REG.

       300-EXCLUSAO.
           OPEN I-O ARQ-VENDEDOR

           DISPLAY BUSCAR-VENDEDOR
           ACCEPT  BUSCAR-VENDEDOR
           MOVE    W-CODIGO-VEND    TO COD-VENDEDOR
       
           READ ARQ-VENDEDOR RECORD INTO W-ARQ-VENDEDOR-REG
             KEY IS COD-VENDEDOR
           
           IF WV-FILE-STATUS NOT EQUAL TO "00"
              DISPLAY MENSAGEM-CONSULTA
              ACCEPT  MENSAGEM-CONSULTA

              EVALUATE W-CONFIRMA
                WHEN 'S'
                   CLOSE ARQ-VENDEDOR
                   PERFORM 300-EXCLUSAO
                WHEN 's'
                   CLOSE ARQ-VENDEDOR
                   PERFORM 300-EXCLUSAO
                WHEN 'N'
                   CLOSE ARQ-VENDEDOR
                   PERFORM 000-INICIO
                WHEN 'n'
                   CLOSE ARQ-VENDEDOR
                   PERFORM 000-INICIO
                WHEN OTHER
                   CLOSE ARQ-VENDEDOR
                   PERFORM 000-INICIO
              END-EVALUATE
           ELSE
              DISPLAY EXCLUSAO-VENDEDOR
              ACCEPT  EXCLUSAO-VENDEDOR
           
              EVALUATE W-CONFIRMA
                WHEN 'S'
                   PERFORM 310-EXCLUIR-VENDEDOR
                   CLOSE ARQ-VENDEDOR
                   PERFORM 000-INICIO
                WHEN 's'
                   PERFORM 310-EXCLUIR-VENDEDOR
                   CLOSE ARQ-VENDEDOR
                   PERFORM 000-INICIO
                WHEN 'N'
                   CLOSE ARQ-VENDEDOR
                   PERFORM 000-INICIO
                WHEN 'n'
                   CLOSE ARQ-VENDEDOR
                   PERFORM 000-INICIO
                WHEN OTHER
                   CLOSE ARQ-VENDEDOR
                   PERFORM 000-INICIO
              END-EVALUATE
           END-IF.

       310-EXCLUIR-VENDEDOR.
           DELETE ARQ-VENDEDOR RECORD.
       
       400-IMPORTACAO.
           OPEN INPUT IMP-VENDEDOR

           IF WI-FILE-STATUS EQUAL TO ZEROS
              OPEN I-O ARQ-VENDEDOR
           
              PERFORM UNTIL WI-FILE-STATUS NOT EQUAL TO "00"
                 READ IMP-VENDEDOR
                 IF WI-FILE-STATUS EQUAL TO ZEROS
                    MOVE IMP-CPF TO W-CPF
                    MOVE IMP-VENDEDOR-REG TO ARQ-VENDEDOR-REG
                    PERFORM 500-VALIDAR-INCLUSAO-VENDEDOR

                    IF W-RETORNO EQUAL TO 0
                       WRITE ARQ-VENDEDOR-REG
                    END-IF
                 END-IF 
              END-PERFORM
           END-IF

           CLOSE ARQ-VENDEDOR
           CLOSE IMP-VENDEDOR.

       500-VALIDAR-INCLUSAO-VENDEDOR.
           MOVE 0 TO W-RETORNO

           CALL 'VALIDACPF' USING W-CPF, W-RETORNO
           
           IF W-RETORNO EQUAL TO 0
              READ ARQ-VENDEDOR RECORD KEY IS COD-VENDEDOR

              IF WV-FILE-STATUS EQUAL TO "00"
                 MOVE 1 TO W-RETORNO
              ELSE
                 READ ARQ-VENDEDOR RECORD KEY IS CPF

                 IF WV-FILE-STATUS EQUAL TO "00"
                    MOVE 1 TO W-RETORNO
                 END-IF
              END-IF
           END-IF.

       END PROGRAM TELAVENDEDOR.