/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.parser;

public interface ISchemeTokenIterator {
    void setPosition(int position);

    SchemeToken nextToken();
    SchemeToken nextToken(boolean ignoreWS);
}