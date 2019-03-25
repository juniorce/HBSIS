       PROGRAM-ID.   VALIDACPF AS "VALIDACPF".
       AUTHOR.       FRANCISCO.
       DATE-WRITTEN. 22/03/2019.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 W-COUNT         PIC  9(002) VALUE ZEROS.
       77 W-SOMA          PIC  9(008) VALUE ZEROS.
       77 W-QUOCIENTE     PIC  9(008) VALUE ZEROS.
       77 W-RESTO         PIC  9(008) VALUE ZEROS.
       77 W-DIGITO        PIC  9(001) VALUE ZEROS.

       LINKAGE SECTION.
       01  L-NUMERO-CPF   PIC  9(011).
       01  L-RETORNO      PIC  9(001).
       
       PROCEDURE DIVISION USING L-NUMERO-CPF, L-RETORNO.
       000-INCIIO.
           MOVE 1 TO W-COUNT
           MOVE 0 TO L-RETORNO 
           
           EVALUATE L-NUMERO-CPF
             WHEN '00000000000'
             WHEN '11111111111'
             WHEN '22222222222'
             WHEN '33333333333'
             WHEN '44444444444'
             WHEN '55555555555'
             WHEN '66666666666'
             WHEN '77777777777'
             WHEN '88888888888'
             WHEN '99999999999'
                MOVE 1 TO L-RETORNO 
             WHEN OTHER
                MOVE 0 TO W-SOMA
                
                PERFORM UNTIL W-COUNT > 9
                   COMPUTE W-SOMA = W-SOMA + ((11 - W-COUNT) *
                           FUNCTION NUMVAL(L-NUMERO-CPF(W-COUNT:1)))
                   ADD 1 TO W-COUNT
                END-PERFORM
                 
                DIVIDE W-SOMA BY 11 GIVING W-QUOCIENTE REMAINDER W-RESTO
                
                IF W-RESTO < 2
                    MOVE 0 TO W-DIGITO
                ELSE
                    COMPUTE W-DIGITO = 11 - W-RESTO
                END-IF

                IF W-DIGITO NOT 
                   EQUAL TO FUNCTION NUMVAL(L-NUMERO-CPF(10:1))
                   MOVE 1 TO L-RETORNO 
                ELSE
                   MOVE 1 TO W-COUNT
                   MOVE 0 TO W-SOMA

                   PERFORM UNTIL W-COUNT > 10
                      COMPUTE W-SOMA = W-SOMA + ((12 - W-COUNT) *
                         FUNCTION NUMVAL(L-NUMERO-CPF(W-COUNT:1)))
                      ADD 1 TO W-COUNT
                   END-PERFORM
                   
                   DIVIDE W-SOMA BY 11 GIVING 
                          W-QUOCIENTE REMAINDER W-RESTO
                   
                   IF W-RESTO < 2
                      MOVE 0 TO W-DIGITO
                   ELSE
                      COMPUTE W-DIGITO = 11 - W-RESTO
                   END-IF
                    
                   IF W-DIGITO NOT 
                      EQUAL TO FUNCTION NUMVAL(L-NUMERO-CPF(11:1))
                      MOVE 1 TO L-RETORNO                               
                    END-IF
                END-IF
           END-EVALUATE
           
           GOBACK.

       END PROGRAM VALIDACPF.