/*
 * Copyright (c) 2004-2005 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.interpreter;

import org.eclipse.debug.core.model.*;

public interface IInterpreterProcess extends IProcess
{
    boolean isRunning();
    
    void sendToInterpreter(String code);
}
