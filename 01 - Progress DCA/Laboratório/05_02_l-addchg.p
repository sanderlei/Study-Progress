REPEAT :
    PROMPT-FOR customer.cust-num WITH 1 COLUMN.
    FIND FIRST customer USING customer.cust-num NO-ERROR.
    IF NOT AVAILABLE customer THEN DO:
        CREATE customer.
        ASSIGN customer.cust-num.
    END.
    UPDATE customer EXCEPT customer.cust-num WITH 1 COLUMN.
END.
