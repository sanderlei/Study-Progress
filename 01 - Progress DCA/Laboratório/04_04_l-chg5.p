DEFINE VARIABLE i-cust-num LIKE customer.cust-num.
REPEAT:
    PROMPT-FOR i-cust-num.
    FIND FIRST customer WHERE customer.cust-num = INPUT i-cust-num NO-ERROR.
    IF AVAILABLE customer THEN
        UPDATE customer WITH 1 COLUMN.
END.

