
/*------------------------------------------------------------------------
    File        : unitTestTOTP.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sat Sep 17 21:39:44 NZST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

using src.totp.

block-level on error undo, throw.

/* ********************  Preprocessor Definitions  ******************** */

define variable totp as class totp no-undo.


totp = new totp('jbsw y3dp ehpk 3pxp').

message totp:ComputeTotp() totp:remainingSeconds().

/* ***************************  Main Block  *************************** */
