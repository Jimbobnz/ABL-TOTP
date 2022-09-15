Function charToValue returns integer (input charByte as character):

    DEFINE VARIABLE inCharByte AS INTEGER     NO-UNDO.
             
    inCharByte = asc(charByte).

    //65-90 == uppercase letters
    if inCharByte < 91 and inCharByte > 64 then
        return inCharByte - 65.
    
    //50-55 == numbers 2-7
    if inCharByte < 56 and inCharByte > 49 then
        return inCharByte - 24.
    
    //97-122 == lowercase letters
    if(inCharByte < 123 and inCharByte > 96) then
        return inCharByte - 97.

    return ?.
end function.

FUNCTION bitOR RETURNS INTEGER (INPUT X AS INTEGER, INPUT Y AS INTEGER):
   DEFINE VARIABLE b1 AS INTEGER NO-UNDO.
   DEFINE VARIABLE b2 AS INTEGER NO-UNDO.
   DEFINE VARIABLE n  AS INTEGER NO-UNDO.
   DEFINE VARIABLE Z  AS INTEGER NO-UNDO.

   DO n = 1 TO 32:
     ASSIGN
       b1 = GET-BITS(X, n, 1)
       b2 = GET-BITS(Y, n, 1)
       .
       IF b1 = 1 OR b2 = 1 THEN 
        PUT-BITS(Z, n, 1) = 1.
   END.

   RETURN Z.
END FUNCTION.

Function arithmeticLeftShift return integer (input x as integer, input z as integer):

    DEFINE VARIABLE buff AS INTEGER     NO-UNDO.
    DEFINE VARIABLE ishift AS INTEGER     NO-UNDO.
    DEFINE variable y AS INTEGER     NO-UNDO.
    
    Do ishift = 1 to z:
    
        put-bits(y, 2, 31) = get-bits(x, 1, 31).   
        put-bits(y, 1, 1)  = 0.  // The empty position in the least significant bit is filled with a zero.
    
        x = y.
    
    End.
    
    return x.

end function.


Function arithmeticRightShift return integer (input x as integer, input z as integer):

    DEFINE VARIABLE buff AS INTEGER     NO-UNDO.
    DEFINE VARIABLE ishift AS INTEGER     NO-UNDO.
    DEFINE variable y AS INTEGER     NO-UNDO.
    
    Do ishift = 1 to z:
    
        put-bits(y, 32, 1) = get-bits(x, 32, 1).   //The empty position in the most significant bit is filled with a copy of the original MSB
        put-bits(y, 1, 31) = get-bits(x, 2, 31).
    
        x = y.
    
    End.
    
    return x.
end function.


Function base32decode return memptr (input base32EncodedValue as character):

    DEFINE VARIABLE returnArray     AS MEMPTR      NO-UNDO.
    
    DEFINE VARIABLE cCharByte       AS CHARACTER   NO-UNDO.
    
    DEFINE VARIABLE iValue          AS INTEGER     NO-UNDO.    
    DEFINE VARIABLE iCharIndex      AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iLength         AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iByteCount      AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iBitsRemaining  AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iMask           AS INTEGER     NO-UNDO. 
    DEFINE VARIABLE iCurrentByte    AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iBytePosition   AS INTEGER     NO-UNDO.
    
    set-byte-order(returnArray ) = big-endian.
     
    // Terminiate if no value has be inputted  
    if base32EncodedValue eq "" Then
        return ?.
    
    assign
        //base32EncodedValue = lc(base32EncodedValue)
        base32EncodedValue = replace(base32EncodedValue,' ','')
        base32EncodedValue = replace(base32EncodedValue,'=','')
        .
        
    assign    
        iLength    = length(base32EncodedValue, "RAW")   
        iByteCount = truncate(iLength * 5 / 8, 0)
        .
    
    set-size(returnArray ) = 0.
    set-size(returnArray ) = iByteCount.     
    
    assign
        iBitsRemaining = 8
        iMask          = 0
        iCurrentByte   = 0
        iBytePosition  = 0.
                                  
    CHARACTER_BYTE:
    do iCharIndex = 1 to iLength:
    
        cCharByte = substring(base32EncodedValue, iCharIndex, 1, "RAW").
        
        iValue = charToValue(input cCharByte).
        
        // Ignore invalid charactes and move on. 
        if iValue eq ? Then
            next CHARACTER_BYTE.
        
        If iBitsRemaining gt 5 Then
        Do:
        
            iMask = arithmeticLeftShift(iValue , (iBitsRemaining - 5) ).
        
            iCurrentByte = bitOR(iCurrentByte, iMask).
        
            iBitsRemaining = iBitsRemaining - 5.
            
        end.
        else
        do:
            
            iMask = arithmeticRightShift(iValue , (5 - iBitsRemaining) ).
        
            iCurrentByte = bitOR(iCurrentByte, iMask).
        
            iBytePosition  = iBytePosition + 1.
            put-byte(returnArray, iBytePosition) = iCurrentByte.
            
            iCurrentByte = arithmeticLeftShift(iValue , (3 + iBitsRemaining) ).
            
            iBitsRemaining = iBitsRemaining + 3.        
        end.
    End.
    
    if iBytePosition ne iByteCount then
    do:
        //iBytePosition  = iBytePosition + 1.
        put-byte(returnArray, iBytePosition + 1) = iCurrentByte.
        
    end.
    
    return returnArray.
    
    // Garbage collection
    finally:
        set-size(returnArray) = 0.
    end finally.
    
end.



/************* MAIN BLOCK ****************/


DEFINE VARIABLE mBase32Decoded AS MEMPTR      NO-UNDO.

etime(true).

set-size(mBase32Decoded) = 0.

mBase32Decoded = base32decode(input "jbsw y3dp ehpk 3pxp").

message 'SIZE:' get-size(mBase32Decoded).
message 'HEX:' string(hex-encode(mBase32Decoded)) .
message 'etime:' etime .

// Garbage collection
finally:

    set-size(mBase32Decoded) = 0.

end finally.
