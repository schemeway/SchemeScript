/*
 * Copyright (c) 2004-2005 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.interpreter;

import org.schemeway.plugins.schemescript.*;

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
}
