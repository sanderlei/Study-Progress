REPEAT:
    PROMPT-FOR customer.cust-num.
    FIND customer USING cust-num.
   DISPLAY customer WITH 1 COLUMN.
   SET customer.
END.

