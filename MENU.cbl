       PROGRAM-ID.   MENU AS "MENU".
       AUTHOR.       FRANCISCO.
       DATE-WRITTEN. 21/03/2019.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WOPCAO          PIC 9 VALUE ZERO.
       77 WOPCAOCAD       PIC 9 VALUE ZERO.
       77 WOPCAOREL       PIC 9 VALUE ZERO.
       77 WOPCAOEXEC      PIC 9 VALUE ZERO.
       77 W-CONFIRMA      PIC X VALUE SPACES.

       01 PARAMETROS-RELATORIOS.
          03 W-TIPO-ORDENACAO      PIC X VALUE SPACES.
          03 W-TIPO-CLASSIFICACAO  PIC 9 VALUE ZERO.
          03 W-CODIGO-CLIENTE      PIC 9(007) VALUE ZEROS.
          03 W-RAZAO-SOCIAL        PIC X(040) VALUE SPACES.
          03 W-CODIGO-VENDEDOR     PIC 9(003) VALUE ZEROS.
          03 W-NOME-VENDEDOR       PIC X(040) VALUE SPACES.

       SCREEN SECTION.
       01 MENU-PRINCIPAL.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "*************************************
      -"**************************************".
          02 LINE 02 COL 21 VALUE "HBSIS - Prova COBOL".
          02 LINE 03 COL 01 VALUE "*************************************
      -"**************************************".
          02 LINE 04 COL 33 VALUE "MENU PRINCIPAL".
          02 LINE 07 COL 10 VALUE "[1] CADASTROS".
          02 LINE 08 COL 10 VALUE "[2] RELATORIOS".
          02 LINE 09 COL 10 VALUE "[3] EXECUTAR".
          02 LINE 12 COL 10 VALUE "[4] SAIR DO SISTEMA".
          02 LINE 15 COL 10 "DIGITE A OPCAO DESEJADA[.]".
          02 LINE 15 COL 34, PIC 9 TO WOPCAO AUTO.

       01 SUB-MENU-CADASTRO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "*************************************
      -"**************************************".
          02 LINE 02 COL 21 VALUE "HBSIS - Prova COBOL".
          02 LINE 03 COL 01 VALUE "*************************************
      -"**************************************".
          02 LINE 04 COL 33 VALUE "SUB-MENU CADASTROS".
          02 LINE 07 COL 10 VALUE "[1] CADASTRO DE CLIENTE".
          02 LINE 08 COL 10 VALUE "[2] CADASTRO DE VENDEDOR".
          02 LINE 09 COL 10 VALUE "[3] VOLTAR AO MENU PRINCIPAL".
          02 LINE 12 COL 10 VALUE "[4] SAIR DO SISTEMA".
          02 LINE 15 COL 10 "DIGITE A OPCAO DESEJADA[.]".
          02 LINE 15 COL 34, PIC 9 TO WOPCAOCAD AUTO.

       01 SUB-MENU-RELATORIO.
          02 BLANK SCREEN.
          02 LINE 1 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 2 COL 21 VALUE "HBSIS - Prova COBOL".
          02 LINE 3 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 04 COL 33 VALUE "SUB-MENU RELATORIOS".
          02 LINE 07 COL 10 VALUE "[1] RELATORIO DE CLIENTE".
          02 LINE 08 COL 10 VALUE "[2] RELATORIO DE VENDEDOR".
          02 LINE 09 COL 10 VALUE "[3] VOLTAR AO MENU PRINCIPAL".
          02 LINE 12 COL 10 VALUE "[4] SAIR DO SISTEMA".
          02 LINE 15 COL 10 "DIGITE A OPCAO DESEJADA[.]".
          02 LINE 15 COL 34, PIC 9 TO WOPCAOREL AUTO.

       01 SUB-MENU-RELATORIO-CLIENTE.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "*************************************
      -"**************************************".
          02 LINE 02 COL 21 VALUE "HBSIS - Prova COBOL".
          02 LINE 03 COL 01 VALUE "*************************************
      -"*************************************".
          02 LINE 04 COL 33 VALUE "OPCOES DO RELATORIO CLIENTE".
          02 LINE 07 COL 10 VALUE 
             "ORDENACAO ASCENDENTE OU DECRESCENTE?(A/D):".
          02 LINE 07 COL 53, PIC X TO W-TIPO-ORDENACAO AUTO.
          02 LINE 08 COL 10 VALUE 
             "CLASSIFICAR POR: 1-CODIGO OU 2-RAZAO SOCIAL:".
          02 LINE 08 COL 55, PIC 9 TO W-TIPO-CLASSIFICACAO AUTO.        
          02 LINE 09 COL 10 VALUE "FILTRAR POR CODIGO DE CLIENTE:".
          02 LINE 09 COL 41, PIC ZZZZZZ9 TO W-CODIGO-CLIENTE AUTO.      
          02 LINE 10 COL 10 VALUE "FILTRAR POR RAZAO SOCIAL:".
          02 LINE 10 COL 36, PIC X(040) TO W-RAZAO-SOCIAL AUTO.         
          02 LINE 15 COL 10 "CONFIRMA IMPRESSAO DO RELATORIO?(S/N):".
          02 LINE 15 COL 49, PIC X TO W-CONFIRMA AUTO.

       01 SUB-MENU-RELATORIO-VENDEDOR.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "*************************************
      -"**************************************".
          02 LINE 02 COL 21 VALUE "HBSIS - Prova COBOL".
          02 LINE 03 COL 01 VALUE "*************************************
      -"*************************************".
          02 LINE 04 COL 33 VALUE "OPCOES DO RELATORIO VENDEDOR".
          02 LINE 07 COL 10 VALUE 
             "ORDENACAO ASCENDENTE OU DECRESCENTE?(A/D):".
          02 LINE 07 COL 53, PIC X TO W-TIPO-ORDENACAO AUTO.
          02 LINE 08 COL 10 VALUE 
             "CLASSIFICAR POR: 1-CODIGO OU 2-NOME DO VENDEDOR:".
          02 LINE 08 COL 59, PIC 9 TO W-TIPO-CLASSIFICACAO AUTO.        
          02 LINE 09 COL 10 VALUE "FILTRAR POR CODIGO DO VENDEDOR:".
          02 LINE 09 COL 42, PIC ZZ9 TO W-CODIGO-VENDEDOR AUTO.     
          02 LINE 10 COL 10 VALUE "FILTRAR POR NOME DO VENDEDOR:".
          02 LINE 10 COL 40, PIC X(040) TO W-NOME-VENDEDOR AUTO.        
          02 LINE 15 COL 10 "CONFIRMA IMPRESSAO DO RELATORIO?(S/N):".
          02 LINE 15 COL 49, PIC X TO W-CONFIRMA AUTO.

       01 SUB-MENU-EXECUTAR.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "*************************************
      -"**************************************".
          02 LINE 02 COL 21 VALUE "HBSIS - Prova COBOL".
          02 LINE 03 COL 01 VALUE "*************************************
      -"**************************************".
          02 LINE 04 COL 33 VALUE "SUB-MENU EXECUTAR".
          02 LINE 07 COL 10 VALUE 
                           "[1] EXECUTAR DISTRIBUICAO DE CLIENTE".
          02 LINE 08 COL 10 VALUE "[2] VOLTAR AO MENU PRINCIPAL".
          02 LINE 12 COL 10 VALUE "[4] SAIR DO SISTEMA".
          02 LINE 15 COL 10 "DIGITE A OPCAO DESEJADA[.]".
          02 LINE 15 COL 34 PIC 9 TO WOPCAOEXEC AUTO.

       PROCEDURE DIVISION.
       000-INICIO.
           DISPLAY MENU-PRINCIPAL
           ACCEPT  MENU-PRINCIPAL
           EVALUATE WOPCAO
              WHEN 1
                 PERFORM 100-SUB-MENU-CADASTROS
              WHEN 2
                 PERFORM 200-SUB-MENU-RELATORIOS
              WHEN 3
                 PERFORM 300-SUB-MENU-EXECUTE
              WHEN 4
                 STOP RUN
              WHEN OTHER
                 PERFORM 000-INICIO
           END-EVALUATE.
       
       100-SUB-MENU-CADASTROS.
           DISPLAY SUB-MENU-CADASTRO
           ACCEPT  SUB-MENU-CADASTRO
           EVALUATE WOPCAOCAD
              WHEN 1
                 CHAIN "TELACLIENTE"
              WHEN 2
                 CHAIN "TELAVENDEDOR"
              WHEN 3
                 PERFORM 000-INICIO
              WHEN 4
                 STOP RUN
              WHEN OTHER
                 PERFORM 100-SUB-MENU-CADASTROS
           END-EVALUATE.
          
       200-SUB-MENU-RELATORIOS.
           DISPLAY SUB-MENU-RELATORIO
           ACCEPT  SUB-MENU-RELATORIO
           EVALUATE WOPCAOREL
              WHEN 1
                 PERFORM 210-MENU-RELATORIO-CLIENTE
              WHEN 2
                 PERFORM 220-MENU-RELATORIO-VENDEDOR
              WHEN 3
                 PERFORM 000-INICIO
              WHEN 4
                 STOP RUN
              WHEN OTHER
                 PERFORM 200-SUB-MENU-RELATORIOS
           END-EVALUATE.

       210-MENU-RELATORIO-CLIENTE.
           DISPLAY SUB-MENU-RELATORIO-CLIENTE
           ACCEPT  SUB-MENU-RELATORIO-CLIENTE
           EVALUATE W-CONFIRMA
              WHEN 'S'
                 CHAIN "REPORTCLIENTE" USING W-TIPO-ORDENACAO,
                                             W-TIPO-CLASSIFICACAO,
                                             W-CODIGO-CLIENTE,
                                             W-RAZAO-SOCIAL
                 PERFORM 000-INICIO
              WHEN 's'
                 CHAIN "REPORTCLIENTE" USING W-TIPO-ORDENACAO,          
                                             W-TIPO-CLASSIFICACAO,
                                             W-CODIGO-CLIENTE,
                                             W-RAZAO-SOCIAL
                 PERFORM 000-INICIO
              WHEN 'N' 
                 PERFORM 000-INICIO
              WHEN 'n' 
                 PERFORM 000-INICIO
              WHEN OTHER
                 PERFORM 000-INICIO
           END-EVALUATE.

       220-MENU-RELATORIO-VENDEDOR.
           DISPLAY SUB-MENU-RELATORIO-VENDEDOR
           ACCEPT  SUB-MENU-RELATORIO-VENDEDOR
           EVALUATE W-CONFIRMA
              WHEN 'S'
                 CHAIN "REPORTVENDEDOR" USING W-TIPO-ORDENACAO,
                                              W-TIPO-CLASSIFICACAO,
                                              W-CODIGO-VENDEDOR,
                                              W-NOME-VENDEDOR
                 PERFORM 000-INICIO
              WHEN 's'
                 CHAIN "REPORTVENDEDOR" USING W-TIPO-ORDENACAO,
                                              W-TIPO-CLASSIFICACAO,
                                              W-CODIGO-VENDEDOR,
                                              W-NOME-VENDEDOR
                 PERFORM 000-INICIO
              WHEN 'N' 
                 PERFORM 000-INICIO
              WHEN 'n' 
                 PERFORM 000-INICIO
              WHEN OTHER
                 PERFORM 000-INICIO
           END-EVALUATE.
       
       300-SUB-MENU-EXECUTE.
           DISPLAY SUB-MENU-EXECUTAR
           ACCEPT  SUB-MENU-EXECUTAR
           EVALUATE WOPCAOEXEC
              WHEN 1
                 CHAIN "DISTRIBUICAO"
              WHEN 2
                 PERFORM 000-INICIO
              WHEN 4
                 STOP RUN
              WHEN OTHER
                 PERFORM 000-INICIO
           END-EVALUATE.

        END PROGRAM MENU.