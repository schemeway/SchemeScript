/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.interpreter;

import org.eclipse.core.runtime.*;
import org.eclipse.jface.dialogs.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;

public class InterpreterType {

    private Interpreter mCachedInterpreter = null;
    private String mName;
    private String mClass;
    private String mId;
    private IConfigurationElement mElement;
    
    public InterpreterType(String name, String id, String clss, IConfigurationElement element) {
        super();
        mElement = element;
        mName = name;
        mId = id;
        mClass = clss;
    }
    
    public String getName() {
        return mName;
    }
    
    public String getID() {
        return mId;
    }
    
    public Interpreter getInterpreter() {
        if (mCachedInterpreter != null)
            return mCachedInterpreter;
        try {
        mCachedInterpreter = (Interpreter) mElement.createExecutableExtension("class");
        }
        catch(CoreException exception){
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
            MessageDialog.openError(shell, "Interpreter Error!", "Unable to instantiate interpreter: " + mClass);
        }
        return mCachedInterpreter;
    }
}
