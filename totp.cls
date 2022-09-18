 
/*------------------------------------------------------------------------
   File        : totp
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : James
   Created     : Sat Sep 17 11:39:02 NZST 2022
   Notes       : 
 ----------------------------------------------------------------------*/

using Progress.Lang.*.

block-level on error undo, throw.

class src.totp final: 

    define public property secretKey as character no-undo 
        get.
        private set. 

    define public property step as integer initial 30 no-undo 
        get.
        private set. 
    
    define public property totpSize as integer initial 6 no-undo 
        get.
        private set.
        
    define public property hashMode as character initial 'HMAC-SHA-1' no-undo 
        get.
        private set.     

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public totp (input secretKey as character  ):
        super ().
        
        this-object:secretKey = secretKey.
        
    end constructor.
    
    constructor public totp (input secretKey as character, input step as integer  ):
        super ().
        
        this-object:secretKey = secretKey.
        this-object:step      = step.

        
    end constructor.
    
    constructor public totp (input secretKey as character, input step as integer, input totpSize as integer ):
        super ().
        
        this-object:secretKey = secretKey.
        this-object:step      = step.
        this-object:totpSize  = totpSize.
        
    end constructor.
    
    constructor public totp (input secretKey as character, input step as integer, input totpSize as integer, input hashMode as character  ):
        super ().
        
        this-object:secretKey = secretKey.
        this-object:step      = step.
        this-object:totpSize  = totpSize.
        this-object:hashMode  = hashMode.
        
    end constructor.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method private integer arithmeticLeftShift( input x as integer, input z as integer):

    
        define variable ishift as integer no-undo.
        define variable y      as integer no-undo.
    
        do ishift = 1 to z:
    
            put-bits(y, 2, 31) = get-bits(x, 1, 31).   
            put-bits(y, 1, 1)  = 0.  // The empty position in the least significant bit is filled with a zero.
    
            x = y.
    
        end.
    
        return x.
    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method private int64 arithmeticLeftShift64(input x as int64, input z as integer):

    
        define variable ishift as integer no-undo.
        define variable y      as int64   no-undo.
    
        do ishift = 1 to z:
    
            put-bits(y, 2, 31) = get-bits(x, 1, 31).   
            put-bits(y, 1, 1)  = 0.  // The empty position in the least significant bit is filled with a zero.
    
            x = y.
    
        end.
    
        return x.

    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method private integer arithmeticRightShift( input x as integer, input z as integer):

        define variable ishift as integer no-undo.
        define variable y      as integer no-undo.
    
        do ishift = 1 to z:
    
            put-bits(y, 32, 1) = get-bits(x, 32, 1).   //The empty position in the most significant bit is filled with a copy of the original MSB
            put-bits(y, 1, 31) = get-bits(x, 2, 31).
    
            x = y.
    
        end.
    
        return x.

    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method private memptr base32Decode( input base32EncodedValue as character):

        define variable returnArray       as memptr    no-undo.
    
        define variable chCharByte        as character no-undo.
    
        define variable iValue            as integer   no-undo.    
        define variable characterPosition as integer   no-undo.
        define variable iLength           as integer   no-undo.
        define variable iByteCount        as integer   no-undo.
        define variable iBitsRemaining    as integer   no-undo.
        define variable iMask             as integer   no-undo. 
        define variable iCurrentByte      as integer   no-undo.
        define variable bytePosition      as integer   no-undo.
    
        set-byte-order(returnArray ) = big-endian.
     
        if base32EncodedValue eq "" then
            return ?.
    
        assign
            base32EncodedValue = replace(base32EncodedValue,' ','')
            base32EncodedValue = replace(base32EncodedValue,'=','')
            base32EncodedValue = trim(base32EncodedValue)
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
        
            iValue = this-object:charToValue(input chCharByte).
        
            if iValue eq ? then
                next.
        
            if iBitsRemaining gt 5 then
            do:
                assign
                    iMask          = this-object:arithmeticLeftShift(iValue , (iBitsRemaining - 5) )
                    iCurrentByte   = this-object:bitOR(iCurrentByte, iMask)
                    iBitsRemaining = iBitsRemaining - 5
                    .
            
            end.
            else
            do:
            
                assign
                    iMask        = this-object:arithmeticRightShift(iValue , (5 - iBitsRemaining) )
                    iCurrentByte = this-object:bitOR(iCurrentByte, iMask)
                    bytePosition = bytePosition + 1
                    .

                put-byte(returnArray, bytePosition) = iCurrentByte.
            
                assign
                    iCurrentByte = this-object:arithmeticLeftShift(iValue , (3 + iBitsRemaining) ).
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

    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method private integer bitAnd( input X as integer, input Y as integer):

        define variable b as integer no-undo.
        define variable n as integer no-undo.
        define variable Z as integer no-undo.

        do n = 1 to 32:

            if get-bits(X, n, 1) = 1 and get-bits(Y, n, 1) = 1 then
                b = 1.

            PUT-BITS(Z, n, 1) = b.
            b = 0.

        end.

        return Z.
    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method private int64 bitAnd64( input X as int64, input Y as int64):

        define variable b as integer no-undo.
        define variable n as integer no-undo.
        define variable Z as int64   no-undo.

        do n = 1 to 64:

            if get-bits(X, n, 1) = 1 and get-bits(Y, n, 1) = 1 then
                b = 1.

            PUT-BITS(Z, n, 1) = b.
            b = 0.

        end.

        return Z.   
    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method private integer bitOR( input X as integer, input Y as integer):

        define variable n as integer no-undo.
        define variable Z as integer no-undo.

        do n = 1 to 32:

            if get-bits(X, n, 1) = 1 or get-bits(Y, n, 1) = 1 then
                PUT-BITS(Z, n, 1) = 1.

        end.

        return Z.

    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method private int64 bitOR64(input X as int64, input Y as int64):

        define variable n as integer no-undo.
        define variable Z as int64   no-undo.

        do n = 1 to 64:

            if get-bits(X, n, 1) eq 1 or get-bits(Y, n, 1) eq 1 then
                PUT-BITS(Z, n, 1) = 1.

        end.

        return Z.

    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method private int64 calculateTimeStepFromTimestamp( input Timestamp as int64 ):
        
/*        define variable TimeStep as integer no-undo.         */
/*                                                             */
/*        TimeStep = truncate(Timestamp / this-object:step, 0).*/
        
        return int64(truncate(Timestamp / this-object:step, 0)) .

    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method private integer charToValue(  input charByte as character):

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

    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public character ComputeTotp(  ):
        
        define variable timestamp   as int64     no-undo.
        define variable timeStep    as integer   no-undo.
        define variable TOTPCode    as integer   no-undo.
        
        define variable hashResult  as memptr    no-undo.
        define variable counterMempr     as memptr    no-undo.
        define variable secretKeyMempr   as memptr    no-undo.
    
        define variable computeTotp as character no-undo.
        
        this-object:verifyParameters().
        
        set-byte-order(hashResult) = big-endian.
        set-byte-order(counterMempr)    = big-endian.
        set-byte-order(secretKeyMempr)  = big-endian.

        set-size(hashResult) = 0.
        set-size(secretKeyMempr)  = 0. 
        set-size(counterMempr)    = 0.
        set-size(counterMempr)    = 8. // 8 bytes = 64 bits         
        
        timestamp   = this-object:unixTime().
            
        timeStep    = this-object:calculateTimeStepFromTimestamp(input timestamp).    
        
        put-int64(counterMempr,1) = timeStep.
                
        secretKeyMempr   = this-object:base32decode(input this-object:secretKey).
        
        hashResult  = message-digest(this-object:hashMode, counterMempr, secretKeyMempr ) .   
        
        TOTPCode    = this-object:dyanmicTruncate(input  hashResult).
        
        computeTotp = string(TOTPCode, "99999999").
        
        computeTotp = substring( computeTotp, (length(computeTotp) - this-object:totpSize) + 1, this-object:totpSize ).
            
        return computeTotp.
        
        finally:
            
            //set-size(hashResult) = 0.
            //set-size(secretkey)  = 0. 
            //set-size(counter)    = 0.
            
        end finally.

    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method private integer dyanmicTruncate( input hashResult as memptr ):
        
        define variable byteOffset    as integer no-undo.
        define variable bytePosition  as integer no-undo.
    
        define variable truncatedHash as integer no-undo.
        define variable inBinary      as int64   no-undo.
    
        assign
            byteOffset = get-byte(hashResult, 20)     //Get the last byte 
            byteOffset = this-object:bitAnd(byteOffset, 0x0F)     //get the last 4 bits
            .
    
        inBinary = 0.
    
        do bytePosition = 1 to 4:
    
            assign
                inBinary = this-object:arithmeticLeftShift64(inBinary , 8)
                inBinary = this-object:bitOR64(inBinary, get-byte(hashResult, byteOffset + bytePosition) )
                .
    
        end.
    
        assign
            inBinary      = this-object:bitAnd64(inBinary, 0x7FFFFFFF)
            truncatedHash = inBinary mod 0xF4240
            .
    
        return truncatedHash.

    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public integer remainingSeconds(  ):
        
        define variable remainingSeconds as integer no-undo.

        assign
            remainingSeconds = time mod 60    
            remainingSeconds = (30 - (remainingSeconds mod 30)).

        return remainingSeconds.

    end method.


    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method private int64 unixTime(  ):
        
        define variable localTime     as datetime-tz no-undo.
        define variable utcTime       as datetime-tz no-undo.
        define variable epocTime      as datetime-tz no-undo.
        define variable unixTime      as integer     no-undo.
        define variable timeremaining as integer     no-undo.
    
        assign
            epocTime  = datetime-tz(1,1,1970,0,0,0,0,0) // Epoch  
            localTime = now                             // local time of the current machine 
            utcTime   = datetime-tz(localTime, 0)       // Convert the localtime to UTC +00:00
            unixTime  = interval( utcTime, epocTime, 'seconds' )
            .        
    
        return unixTime.

    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method private void verifyParameters(  ):
        
        if  not (this-object:step > 0) then 
            return error new Progress.Lang.AppError("Step must be a positive value" ). 
                
        if not (this-object:totpSize > 0) then
            return error new Progress.Lang.AppError("totpSize must be a positive value" ).
            
        if not (this-object:totpSize <= 10) then
            return error new Progress.Lang.AppError("totpSize must be less or equal to 10" ).
        
        return.

    end method.

    destructor public totp ( ):

    end destructor.

end class.