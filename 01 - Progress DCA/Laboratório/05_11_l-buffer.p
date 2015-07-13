DEFINE VAR l-outro-cliente AS LOGICAL FORMAT "Sim/Não" LABEL "Outro Cliente" NO-UNDO.
DEFINE BUFFER b-customer FOR customer.
    FOR EACH customer BY customer.credit-limit:
        FIND FIRST b-customer
            WHERE b-customer.credit-limit = customer.credit-limit
            AND b-customer.cust-num <> customer.cust-num NO-ERROR.
        ASSIGN l-outro-cliente = AVAIL b-customer.
        DISPLAY customer.cust-num
            customer.NAME
            customer.credit-limit
            l-outro-cliente.
    END.
