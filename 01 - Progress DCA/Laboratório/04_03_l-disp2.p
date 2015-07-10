DEFINE VARIABLE c-nome AS CHAR FORMAT "x(1)" LABEL "Primeiro Caracter do Nome".
DEFINE VARIABLE de-limite LIKE customer.credit-limit LABEL "Limite de Credito".
UPDATE c-nome de-limite.
FOR EACH customer WHERE customer.NAME BEGINS c-nome AND customer.credit-limit > de-limite:
    DISP customer.cust-num 
        customer.NAME
        customer.credit-limit.
END.
