

function bitOR returns integer (input X as integer, input Y as integer):
    define variable b1 as integer no-undo.
    define variable b2 as integer no-undo.
    define variable n  as integer no-undo.
    define variable Z  as integer no-undo.

    do n = 1 to 32:
        assign
            b1 = get-bits(X, n, 1)
            b2 = get-bits(Y, n, 1)
            .
        if b1 = 1 or b2 = 1 then 
            PUT-BITS(Z, n, 1) = 1.
    end.

    return Z.
end function.

function bitOR64 returns int64 (input X as int64, input Y as int64):
    define variable b1 as integer no-undo.
    define variable b2 as integer no-undo.
    define variable n  as integer no-undo.
    define variable Z  as int64   no-undo.

    do n = 1 to 64:
        assign
            b1 = get-bits(X, n, 1)
            b2 = get-bits(Y, n, 1)
            .
        if b1 = 1 or b2 = 1 then 
            PUT-BITS(Z, n, 1) = 1.
    end.

    return Z.
end function.

function bitAnd returns integer (input X as integer, input Y as integer):
    define variable b1 as integer no-undo.
    define variable b2 as integer no-undo.
    define variable n  as integer no-undo.
    define variable Z  as integer no-undo.

    do n = 1 to 32:
        assign
            b1 = get-bits(X, n, 1)
            b2 = get-bits(Y, n, 1)
            .
        if b1 = 1 and b2 = 1 then 
            PUT-BITS(Z, n, 1) = 1.
        
        if b1 = 0 and b2 = 0 then 
            PUT-BITS(Z, n, 1) = 0. 
    end.

    return Z.
end function.

function bitAnd64 returns integer (input X as int64, input Y as int64):
    define variable b1 as integer no-undo.
    define variable b2 as integer no-undo.
    define variable n  as integer no-undo.
    define variable Z  as integer no-undo.

    do n = 1 to 64:
        assign
            b1 = get-bits(X, n, 1)
            b2 = get-bits(Y, n, 1)
            .
        if b1 = 1 and b2 = 1 then 
            PUT-BITS(Z, n, 1) = 1.
        
        if b1 = 0 and b2 = 0 then 
            PUT-BITS(Z, n, 1) = 0. 
    end.

    return Z.
end function.

function charToValue returns integer (input charByte as character):

    define variable charByteValue as integer no-undo.
             
    charByteValue = asc(charByte).

    //65-90 == uppercase letters
    if charByteValue lt 91 and charByteValue gt 64 then
        return charByteValue - 65.
    
    //50-55 == numbers 2-7
    if charByteValue lt 56 and charByteValue gt 49 then
        return charByteValue - 24.
    
    //97-122 == lowercase letters
    if(charByteValue lt 123 and charByteValue gt 96) then
        return charByteValue - 97.

    return ?.
end function.

function arithmeticLeftShift return integer (input x as integer, input z as integer):

    
    define variable ishift as integer no-undo.
    define variable y      as integer no-undo.
    
    do ishift = 1 to z:
    
        put-bits(y, 2, 31) = get-bits(x, 1, 31).   
        put-bits(y, 1, 1)  = 0.  // The empty position in the least significant bit is filled with a zero.
    
        x = y.
    
    end.
    
    return x.

end function.

function arithmeticLeftShift64 return int64 (input x as int64, input z as integer):

    define variable ishift as integer no-undo.
    define variable y      as int64   no-undo.
    
    do ishift = 1 to z:
    
        put-bits(y, 2, 63) = get-bits(x, 1, 63).   
        put-bits(y, 1, 1)  = 0.  // The empty position in the least significant bit is filled with a zero.
    
        x = y.
    end.
    
    return x.

end function.


function arithmeticRightShift return integer (input x as integer, input z as integer):

    define variable ishift as integer no-undo.
    define variable y      as integer no-undo.
    
    do ishift = 1 to z:
    
        put-bits(y, 32, 1) = get-bits(x, 32, 1).   //The empty position in the most significant bit is filled with a copy of the original MSB
        put-bits(y, 1, 31) = get-bits(x, 2, 31).
    
        x = y.
    
    end.
    
    return x.
end function.


function base32decode return memptr (input base32EncodedValue as character):

    define variable returnArray    as memptr    no-undo.
    
    define variable chCharByte     as character no-undo.
    
    define variable iValue         as integer   no-undo.    
    define variable characterPosition as integer   no-undo.
    define variable iLength        as integer   no-undo.
    define variable iByteCount     as integer   no-undo.
    define variable iBitsRemaining as integer   no-undo.
    define variable iMask          as integer   no-undo. 
    define variable iCurrentByte   as integer   no-undo.
    define variable bytePosition   as integer   no-undo.
    
    set-byte-order(returnArray ) = big-endian.
     
    if base32EncodedValue eq "" then
        return ?.
    
    assign
        base32EncodedValue = replace(base32EncodedValue,' ','')
        base32EncodedValue = replace(base32EncodedValue,'=','')
        .
        
    assign    
        iLength    = length(base32EncodedValue,"RAW")   
        iByteCount = truncate(iLength * 5 / 8,0)
        .
    
    set-size(returnArray ) = 0.
    set-size(returnArray ) = iByteCount.     
    
    assign
        iBitsRemaining = 8
        iMask          = 0
        iCurrentByte   = 0
        bytePosition   = 0
        .
    
    do characterPosition = 1 to iLength:
    
        chCharByte = substring(base32EncodedValue, characterPosition, 1, "RAW").
        
        iValue = charToValue(input chCharByte).
        
        if iValue eq ? then
            next.
        
        if iBitsRemaining gt 5 then
        do:
            assign
                iMask          = arithmeticLeftShift(iValue , (iBitsRemaining - 5) )
                iCurrentByte   = bitOR(iCurrentByte, iMask)
                iBitsRemaining = iBitsRemaining - 5
                .
            
        end.
        else
        do:
            
            assign
                iMask        = arithmeticRightShift(iValue , (5 - iBitsRemaining) )
                iCurrentByte = bitOR(iCurrentByte, iMask)
                bytePosition  = bytePosition + 1
                .

            put-byte(returnArray, bytePosition) = iCurrentByte.
            
            assign
                iCurrentByte   = arithmeticLeftShift(iValue , (3 + iBitsRemaining) ).
                iBitsRemaining = iBitsRemaining + 3
                .        
        end.
    end.
    
    if bytePosition ne iByteCount then
    do:
        bytePosition  = bytePosition + 1.
        put-byte(returnArray, bytePosition) = iCurrentByte.
        
    end.
    
    return returnArray.
    
    finally:
    
        set-size(returnArray) = 0.
    
    end finally.
    
end.

function unixTimeSeconds returns int64():

    define variable localTime as datetime-tz no-undo.
    define variable utcTime   as datetime-tz no-undo.
    define variable epocTime  as datetime-tz no-undo.
    define variable unixTime  as integer     no-undo.
    
    assign
        epocTime  = datetime-tz(1,1,1970,0,0,0,0,0) // Epoch  
        localTime = now                             // local time of the current machine 
        utcTime   = datetime-tz(localTime, 0)       // Convert the localtime to UTC +00:00
        unixTime  = interval( utcTime, epocTime, 'seconds' )
        .
    
    return unixTime.

end function. 

function dynamicTruncate returns integer (input hashResult as memptr):

    define variable byteOffset         as integer no-undo.
    define variable bytePosition       as integer no-undo.
    
    define variable truncatedHash      as int64   no-undo.
    define variable inBinary           as int64   no-undo.
    
    assign
        byteOffset = get-byte(hashResult, 20)     //Get the last byte 
        byteOffset = bitAnd(byteOffset, 0x0F)        //get the last 4 bits
        .
    
    inBinary = 0.
    
    do bytePosition = 1 to 4:
    
        assign
            inBinary = arithmeticLeftShift64(inBinary , 8)
            inBinary = bitOR64(inBinary, get-byte(hashResult, byteOffset + bytePosition) )
            .
    
    end.
    
    assign
        inBinary      = bitAnd64(inBinary, 0x7FFFFFFF)
        truncatedHash = inBinary mod 0xF4240
        .
    
    return truncatedHash.
    
end function.

function totp returns character (input sharedSecretKey as character, input step as integer, input totpSize as integer ):

    define variable hashResult      as memptr    no-undo.
    define variable counter         as memptr    no-undo.
    define variable secretkey       as memptr    no-undo.

    define variable unixTime        as int64     no-undo.
    define variable timestamp       as int64     no-undo.
    
    define variable returnTOTPCode       as character   NO-UNDO.    
    
    If sharedSecretKey eq '' Then
        return "Shared Secret Key not provided!".
        
    
    set-byte-order(hashResult) = big-endian.
    set-byte-order(counter)    = big-endian.
    set-byte-order(secretkey)  = big-endian.

    set-size(hashResult) = 0.
    set-size(secretkey)  = 0. 
    set-size(counter)    = 0.
    set-size(counter)    = 8. // 8 bytes = 64 bits        
        
    assign
        step      = abs(step)
        totpSize  = abs(step)
        step      = 30 when (step eq ? or step eq 0)         // default to 30 seconds
        totpSize  = 6  when (totpSize eq ? or totpSize eq 0) // default to 6 totp size
        unixTime  = unixTimeSeconds()
        timestamp = truncate(unixTime / step,0)
        .

    put-int64(counter,1) = timestamp.

    assign
        secretkey      = base32decode(sharedSecretKey)
        hashResult     = message-digest('HMAC-SHA-1', counter, secretkey )         // Minimum requirment Openedge 11.7.4
        returnTOTPCode = string( dynamicTruncate( input hashResult), '99999999')   // Pad out to 8 zeros
        returnTOTPCode = substring( returnTOTPCode, (length(returnTOTPCode) - totpSize) + 1, totpSize )
        .
    
    return returnTOTPCode.
    
    finally:
    
        set-size(hashResult) = 0.
        set-size(counter) = 0.
        set-size(secretkey) = 0.
    
    end finally.

end function. 

procedure formatMemptr:

    // This procedure is used for debuging memptr.

    define input parameter pmemptr as memptr no-undo.

    define variable chHexByte         as character no-undo.
    define variable chHexByteFormated as character no-undo.
    define variable chHexBytedec      as character no-undo.
    define variable chHexBytes        as character no-undo.
    define variable i                 as integer   no-undo.

    define variable bytePosition      as integer   no-undo.  
    define variable chByteIndex       as character no-undo.

    chHexBytes = string( hex-encode( pmemptr ) ).
    
    do i = 1 to (get-size(pmemptr) * 2) by 2:
    
        chByteIndex = chByteIndex + string(bytePosition, '>>>9') + ' '.

        chHexByte = caps(substring(chHexBytes, i, 2) ).
        
        if chHexByte eq '' then
            next.
            
        chHexByteFormated = chHexByteFormated + '0x' + chHexByte + ' '. 
    
        chHexBytedec = chHexBytedec + string(get-byte( hex-decode( chHexByte ), 1), '>>>9' ) + ' ' .
        bytePosition = bytePosition + 1.

    end.
    
    
    message chByteIndex.
    message chHexByteFormated.
    message chHexBytedec.
    
    finally:
    
        set-size(pmemptr) = 0.
    
    end finally.

end procedure. 

/****** Main Block **********/

define variable sharedSecretKey as character   NO-UNDO.
define variable totpCode        as character   NO-UNDO.


sharedSecretKey = 'jbsw y3dp ehpk 3pxp'.


totpCode = totp(input sharedSecretKey,  // pre shared secret key 
                input ?,               // time step, default is 30 seconds
                input ?).               // TOTP size, number of characters returned. 6 is the default 

message "TOTP Code:" totpCode
    view-as alert-box info title "TOTP Code".


