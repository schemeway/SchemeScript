package org.schemeway.plugins.schemescript.interpreter;

import org.eclipse.debug.core.model.IProcess;

public interface IInterpreterProcess extends IProcess
{
    boolean isRunning();
    
    void sendToInterpreter(String code);
}
