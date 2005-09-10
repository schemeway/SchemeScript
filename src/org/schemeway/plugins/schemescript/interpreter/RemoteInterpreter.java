/*
 * Copyright (c) 2004-2005 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.interpreter;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.dialogs.MessageDialog;
import org.schemeway.plugins.schemescript.SchemeScriptPlugin;

public class RemoteInterpreter extends AbstractInterpreter
{
    public static final String CONFIG_TYPE = SchemeScriptPlugin.PLUGIN_NS + ".remoteInterpreter";
    
    public IInterpreterProcess getProcess()
    {
        return RemoteInterpreterProcess.getInstance();
    }
    
    public String getConfigurationType() {
        return CONFIG_TYPE;
    }
    
    public void load(IFile file) {
        MessageDialog.openInformation(null, "Remote interpreter", "File loading not supported!");
    }
}
