       PROGRAM-ID.   VALIDACNPJ as "VALIDACNPJ".
       AUTHOR.       FRANCISCO.
       DATE-WRITTEN. 22/03/2019.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 W-COUNT         PIC  9(002) VALUE ZEROS.
       77 W-FATOR         PIC  9(002) VALUE ZEROS.
       77 W-SOMA          PIC  9(008) VALUE ZEROS.
       77 W-QUOCIENTE     PIC  9(008) VALUE ZEROS.
       77 W-RESTO         PIC  9(008) VALUE ZEROS.
       77 W-DIGITO        PIC  9(001) VALUE ZEROS.

       LINKAGE SECTION.
       01  L-NUMERO-CNPJ  PIC  9(014).
       01  L-RETORNO      PIC  9(001).
       
       PROCEDURE DIVISION USING L-NUMERO-CNPJ, L-RETORNO.
       000-INCIIO.
           MOVE 0 TO L-RETORNO
           MOVE 1 TO W-COUNT
           MOVE 5 TO W-FATOR
           MOVE 0 TO W-SOMA
           
           PERFORM UNTIL W-COUNT > 12
             IF W-FATOR < 2
                MOVE 9 TO W-FATOR
             END-IF
            
             COMPUTE W-SOMA = W-SOMA + (W-FATOR *
                     FUNCTION NUMVAL(L-NUMERO-CNPJ(W-COUNT:1)))

             ADD      1 TO   W-COUNT
             SUBTRACT 1 FROM W-FATOR
           END-PERFORM

           DIVIDE W-SOMA BY 11 GIVING W-QUOCIENTE REMAINDER W-RESTO
                
           IF W-RESTO < 2
              MOVE 0 TO W-DIGITO
           ELSE
              COMPUTE W-DIGITO = 11 - W-RESTO
           END-IF

           IF W-DIGITO NOT 
              EQUAL TO FUNCTION NUMVAL(L-NUMERO-CNPJ(13:1))
              MOVE 1 TO L-RETORNO 
           ELSE
              MOVE 1 TO W-COUNT
              MOVE 6 TO W-FATOR
              MOVE 0 TO W-SOMA
           
              PERFORM UNTIL W-COUNT > 13
                 IF W-FATOR < 2
                    MOVE 9 TO W-FATOR
                 END-IF
            
                 COMPUTE W-SOMA = W-SOMA + (W-FATOR *
                         FUNCTION NUMVAL(L-NUMERO-CNPJ(W-COUNT:1)))

                 ADD      1 TO   W-COUNT
                 SUBTRACT 1 FROM W-FATOR
              END-PERFORM

              DIVIDE W-SOMA BY 11 GIVING W-QUOCIENTE REMAINDER W-RESTO

              IF W-RESTO < 2
                 MOVE 0 TO W-DIGITO
              ELSE
                 COMPUTE W-DIGITO = 11 - W-RESTO
              END-IF

              IF W-DIGITO NOT 
                 EQUAL TO FUNCTION NUMVAL(L-NUMERO-CNPJ(14:1))
                 MOVE 1 TO L-RETORNO 
              END-IF
           END-IF

           GOBACK.

       END PROGRAM VALIDACNPJ.