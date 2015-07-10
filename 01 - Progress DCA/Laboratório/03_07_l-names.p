PROMPT-FOR customer.NAME.
FIND FIRST customer WHERE customer.NAME BEGINS INPUT customer.NAME NO-ERROR.
IF AVAILABLE customer THEN DO:
    DISPLAY customer.cust-num customer.NAME customer.country.
    REPEAT:
        FIND NEXT customer.
        DISPLAY customer.cust-num customer.NAME customer.country.
    END.
END.
