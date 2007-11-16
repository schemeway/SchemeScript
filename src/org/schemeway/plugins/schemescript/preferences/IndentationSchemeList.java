/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.preferences;

import java.util.*;

import org.eclipse.core.runtime.Assert;

import org.schemeway.plugins.schemescript.indentation.*;

public class IndentationSchemeList {
    List mSchemes;
    List mSchemeListeners;

    public IndentationSchemeList(IndentationScheme[] initialSchemes) {
        mSchemes = new ArrayList();
        mSchemeListeners = new ArrayList();
        setSchemes(initialSchemes);
    }

    public void addChangeListener(IIndentationSchemeChangeListener listener) {
        Assert.isNotNull(listener);
        mSchemeListeners.add(listener);
    }

    public void removeChangeListener(IIndentationSchemeChangeListener listener) {
        mSchemeListeners.remove(listener);
    }

    public IndentationScheme[] getSchemes() {
        return (IndentationScheme[]) mSchemes.toArray(new IndentationScheme[mSchemes.size()]);
    }

    public void setSchemes(IndentationScheme[] schemes) {
        mSchemes.clear();
        for (int index = 0; index < schemes.length; index++) {
            IndentationScheme scheme = schemes[index];
            if (scheme != null) {
                mSchemes.add(scheme);
            }
        }
    }

    public void addScheme(IndentationScheme scheme) {
        mSchemes.add(scheme);
        for (int index = 0; index < mSchemeListeners.size(); index++) {
            ((IIndentationSchemeChangeListener) mSchemeListeners.get(index)).schemeAdded(scheme);
        }
    }

    public void removeScheme(IndentationScheme scheme) {
        mSchemes.remove(scheme);
        for (int index = 0; index < mSchemeListeners.size(); index++) {
            ((IIndentationSchemeChangeListener) mSchemeListeners.get(index)).schemeRemoved(scheme);
        }
    }

}