/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.action.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.interpreter.*;

public class RestartInterpreterAction extends Action
{
    public RestartInterpreterAction()
    {
        setText("Restart the Scheme interpreter");
        setToolTipText("Restart interpreter");
    }

    public void run()
    {
        Interpreter interp = SchemeScriptPlugin.getDefault().getInterpreter();
        if (interp == null) 
            return;
    
        interp.restart();
        interp.showConsole();
    }
}