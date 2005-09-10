/*
 * Copyright (c) 2004-2005 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.interpreter;

import java.net.SocketException;

public interface SocketExceptionHandler
{
    void exceptionOccurred(SocketException exception);
}
