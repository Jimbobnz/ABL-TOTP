 
 /*------------------------------------------------------------------------
    File        : totpgui
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : James
    Created     : Sat Sep 17 22:14:06 NZST 2022
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Progress.Windows.Form from assembly.
using src.totp from propath.


class src.totpgui inherits Form: 
    
    define private variable components as System.ComponentModel.IContainer no-undo.
    define private variable textBox2 as System.Windows.Forms.TextBox no-undo.
    define private variable timer1 as System.Windows.Forms.Timer no-undo.
    define private variable textBox1 as System.Windows.Forms.TextBox no-undo.
    define private variable progressBar1 as System.Windows.Forms.ProgressBar no-undo.

    define private property totp as src.totp no-undo 
    get.
    set. 

    constructor public totpgui (  ):
        
        InitializeComponent().
        this-object:ComponentsCollection:Add(this-object:components).
        catch e as Progress.Lang.Error:
            undo, throw e.
        end catch.

    end constructor.

    method private void InitializeComponent(  ):
        
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        this-object:components = new System.ComponentModel.Container().
        this-object:timer1 = new System.Windows.Forms.Timer(this-object:components).
        this-object:textBox1 = new System.Windows.Forms.TextBox().
        this-object:progressBar1 = new System.Windows.Forms.ProgressBar().
        this-object:textBox2 = new System.Windows.Forms.TextBox().
        this-object:SuspendLayout().
        /*  */
        /* timer1 */
        /*  */
        this-object:timer1:Tick:Subscribe(this-object:timer1_Tick).
        /*  */
        /* textBox1 */
        /*  */
        this-object:textBox1:Location = new System.Drawing.Point(12, 12).
        this-object:textBox1:Name = "textBox1".
        this-object:textBox1:Size = new System.Drawing.Size(100, 22).
        this-object:textBox1:TabIndex = 0.
        /*  */
        /* progressBar1 */
        /*  */
        this-object:progressBar1:Location = new System.Drawing.Point(118, 12).
        this-object:progressBar1:Maximum = 30.
        this-object:progressBar1:Minimum = 1.
        this-object:progressBar1:Name = "progressBar1".
        this-object:progressBar1:Size = new System.Drawing.Size(162, 23).
        this-object:progressBar1:TabIndex = 1.
        this-object:progressBar1:Value = 1.
        /*  */
        /* textBox2 */
        /*  */
        this-object:textBox2:Location = new System.Drawing.Point(12, 49).
        this-object:textBox2:Name = "textBox2".
        this-object:textBox2:Size = new System.Drawing.Size(268, 22).
        this-object:textBox2:TabIndex = 2.
        /*  */
        /* totpgui */
        /*  */
        this-object:ClientSize = new System.Drawing.Size(292, 266).
        this-object:Controls:Add(this-object:textBox2).
        this-object:Controls:Add(this-object:progressBar1).
        this-object:Controls:Add(this-object:textBox1).
        this-object:Name = "totpgui".
        this-object:Text = "totpgui".
        this-object:Load:Subscribe(this-object:totpgui_Load).
        this-object:ResumeLayout(false).
        this-object:PerformLayout().
        catch e as Progress.Lang.Error:
            undo, throw e.
        end catch.
    end method.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	method private void timer1_Tick( input sender as System.Object, input e as System.EventArgs ):
		
		progressBar1:value = totp:remainingSeconds().
		
		//if progressBar1:value = totp:step then 
		  textBox1:text =  totp:ComputeTotp().
		  
		
		return.

	end method.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	method private void totpgui_Load( input sender as System.Object, input e as System.EventArgs ):
		
		totp = new totp(input "jbsw y3dp ehpk 3pxp").
		
		textBox1:text = totp:ComputeTotp().

        textBox2:text = totp:secretKey.
		
		progressBar1:maximum = totp:step.
		
		timer1:enabled = true.
		timer1:interval = 1000.
		
		return.

	end method.

    destructor public totpgui ( ):

    end destructor.

end class.