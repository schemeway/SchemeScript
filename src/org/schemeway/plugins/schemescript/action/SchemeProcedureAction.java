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

    private String mProcedureName = null;

    public void setInitializationData(IConfigurationElement config, String propertyName, Object data)
            throws CoreException {
        
        if (data instanceof String) {
            mProcedureName = (String)data;
        }
    }

    public void dispose() {
    }

    public void init(IWorkbenchWindow window) {
    }

    public void run(IAction action) {
        if (mProcedureName != null) {
            try {
                Object object = Scheme.getInstance().eval(mProcedureName);
                if (object instanceof Procedure) {
                    Procedure proc = (Procedure) object;
                    proc.apply0();
                }
            }
            catch (Throwable e) {
                SchemeScriptPlugin.logException("Error while evaluating Scheme action", e);
            }
        }
    }

    public void selectionChanged(IAction action, ISelection selection) {
    }

}
