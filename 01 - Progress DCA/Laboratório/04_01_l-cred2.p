PROMPT-FOR customer.cust-num.
FIND FIRST customer USING INPUT customer.cust-num NO-ERROR.
IF AVAILABLE customer THEN DO:
     DISP customer.cust-num customer.NAME customer.credit-limit.
     DEF VARIABLE limite LIKE customer.credit-limit.
     ASSIGN limite = customer.credit-limit.
     FOR EACH customer WHERE customer.credit-limit >= limite BY customer.credit-limit DESC:
         DISP customer.cust-num customer.NAME customer.credit-limit.
     END.
END.
