/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.preferences;

import org.schemeway.plugins.schemescript.indentation.*;

public interface IIndentationSchemeChangeListener {
    void schemeAdded(IndentationScheme scheme);

    void schemeRemoved(IndentationScheme scheme);
}