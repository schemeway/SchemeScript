/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.interpreter;

import org.eclipse.core.resources.*;

public interface Interpreter {
    boolean isRunning();
    void start();
    void stop();
    void restart();
    void eval(String code);
    void load(IFile file);
    
    void showConsole();
}
