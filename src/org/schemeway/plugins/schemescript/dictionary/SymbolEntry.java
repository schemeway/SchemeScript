/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.dictionary;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.util.Assert; // TODO should be replaced by a Plugin specific class

/**
 * An entry in a symbol dictionary.
 * 
 * @see ISymbolDictionary
 * @see IResource
 */
public class SymbolEntry {
    public static final String SYNTAX   = "syntax";
    public static final String FUNCTION = "function";
    public static final String VARIABLE = "variable";
    
    private String mName;
    private String mDescription;
    private IMarker mMarker;
    private String mCategory;
    
    public SymbolEntry(String name, String description, String category) {
        this(name, description, category, null, -1);
    }
    
    public SymbolEntry(String name, String description, String category, IResource source, int position) {
        Assert.isNotNull(name);
        mName = name;
        mDescription = description;
        mCategory = category;
        try {
            if (source != null && position >= 0) {
                mMarker = source.createMarker(IMarker.TEXT);
                mMarker.setAttribute(IMarker.LINE_NUMBER, position);
            }
        }
        catch (CoreException exception) {
            exception.printStackTrace();
            mMarker = null;
        }
    }
    
    public String getName() {
        return mName;
    }

    public String getDescription() {
        return mDescription;
    }

    public IMarker getMarker() {
        return mMarker;
    }

    public String getCategory() {
        return mCategory;
    }
}
