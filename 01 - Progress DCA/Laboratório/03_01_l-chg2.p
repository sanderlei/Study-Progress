REPEAT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num.
    UPDATE customer WITH 1 COLUMN.
END.

