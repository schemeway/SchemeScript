/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.interpreter;

import org.schemeway.plugins.schemescript.SchemeScriptPlugin;

public class KawaInterpreter extends AbstractInterpreter {
    public static final String CONFIG_TYPE = SchemeScriptPlugin.PLUGIN_NS + ".kawaInterpreter";

    public KawaInterpreter() {
        super();
    }
    
    public IInterpreterProcess getProcess() {
        return KawaProcess.getInstance();
    }
    
    public String getConfigurationType() {
        return CONFIG_TYPE;
    }

    public void stop() {
    }

    public void restart() {
        start();
    }

    public boolean supportInterruption() {
        return false;
    }
}