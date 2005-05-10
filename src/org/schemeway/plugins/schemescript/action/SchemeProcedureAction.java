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

    private Procedure mProcedure = null;

    public void setInitializationData(IConfigurationElement config, String propertyName, Object data)
            throws CoreException {
        
        if (data instanceof String) {
            String procedureName = (String)data;
            try {
                Object object = Scheme.getInstance().eval(procedureName);
                if (object instanceof Procedure) {
                    mProcedure = (Procedure) object;
                }
            }
            catch (Throwable e) {
                SchemeScriptPlugin.logException("Error while initializing Scheme action", e);
            }
        }
    }

    public void dispose() {
    }

    public void init(IWorkbenchWindow window) {
    }

    public void run(IAction action) {
        if (mProcedure != null) {
            try {
                mProcedure.apply0();
            }
            catch (Throwable e) {
                SchemeScriptPlugin.logException("Error while evaluating Scheme action", e);
            }
        }
    }

    public void selectionChanged(IAction action, ISelection selection) {
    }

}
