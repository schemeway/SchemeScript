/*
 * Copyright (c) 2005 SchemeWay.com
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import gnu.mapping.*;
import kawa.standard.*;

import org.eclipse.core.runtime.*;
import org.eclipse.jface.action.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.ui.*;
import org.schemeway.plugins.schemescript.*;

/**
 * @author SchemeWay.com
 */
public final class SchemeProcedureAction extends Action implements IWorkbenchWindowActionDelegate, IExecutableExtension {

    private Procedure mCachedProcedure = null;
    private String    mProcedureName;

    public void setInitializationData(IConfigurationElement config, String propertyName, Object data)
            throws CoreException {
        
        if (data instanceof String) {
            mProcedureName = (String)data;
            mCachedProcedure = getProcedure(mProcedureName);
        }
    }

    public void dispose() {
    }

    public void init(IWorkbenchWindow window) {
    }

    public void run(IAction action) {
        Procedure proc = getProcedure(mProcedureName);
        
        if (proc != null && proc != mCachedProcedure)
            mCachedProcedure = proc;
        
        if (mCachedProcedure != null) {
            try {
                mCachedProcedure.apply0();
            }
            catch (Throwable e) {
                SchemeScriptPlugin.logException("Error while evaluating Scheme action", e);
            }
        }
    }

    public void selectionChanged(IAction action, ISelection selection) {
    }
    
    private Procedure getProcedure(String name)
    {
        try {
            Object object = Scheme.getInstance().eval(name);
            if (object instanceof Procedure)
                return (Procedure) object;
            else
                return null;
        }
        catch (Throwable e) {
            SchemeScriptPlugin.logException("Error while fetching Scheme action", e);
            return null;
        }
    }

}
