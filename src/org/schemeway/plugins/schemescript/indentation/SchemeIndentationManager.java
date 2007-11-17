/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.indentation;

import java.util.*;

import org.eclipse.core.runtime.*;
import org.eclipse.jface.preference.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.preferences.*;

public class SchemeIndentationManager {
    private static IndentationScheme DEFAULT_SCHEME;
    static {
        DEFAULT_SCHEME = new IndentationScheme(null, IndentationScheme.DEFAULT, 0);
    }

    private Hashtable mMapping;

    public SchemeIndentationManager() {
        mMapping = new Hashtable();
    }

    public void clear() {
        mMapping.clear();
    }

    public void setSchemes(IndentationScheme[] schemes) {
        Assert.isNotNull(schemes);

        clear();
        for (int index = 0; index < schemes.length; index++) {
            IndentationScheme scheme = schemes[index];
            if (scheme != null) {
                mMapping.put(scheme.getSymbol(), scheme);
            }
        }
    }

    public final IndentationScheme getFunction(String symbol) {
        IndentationScheme scheme = (IndentationScheme) mMapping.get(symbol);
        if (scheme == null)
            return DEFAULT_SCHEME;
        else
            return scheme;
    }

    public void updateSchemes() {
        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
        setSchemes(PreferenceUtil.getIndentationSchemes(store, IndentationPreferences.INDENT_SCHEMES));
    }

    /* -- Factory methods -- */

}