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
    
    public static final int 
            HIGH = 0,
            MEDIUM = 1,
            LOW = 2;
    
    private String mName;
    private String mDescription;
    private IMarker mMarker;
    private String mCategory;
    private int mPriority;
    private IResource mResource;
    
    public SymbolEntry(String name, String description, String category) {
        this(name, description, category, null, -1, LOW);
    }
    
    public SymbolEntry(String name, String description, String category, int priority) {
        this(name, description, category, null, -1, priority);
    }
    
    public SymbolEntry(String name, String description, String category, IResource source, int position) {
        this(name, description, category, source, position, LOW);
    }
    
    public SymbolEntry(String name, String description, String category, IResource source, int position, int priority) {
        Assert.isNotNull(name);
        mName = name;
        mDescription = description;
        mCategory = category;
        mPriority = priority;
        mResource = source;
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
    
    public int getPriority() {
        return mPriority;
    }
    
    public String getContext() {
        if (mResource != null) {
            return mResource.getName() + ", " + formatPath(mResource);
        }
        else if (mCategory != null)
            return mCategory;
        else
            return "";
    }
    
    private static String formatPath(IResource resource) {
        IPath path = resource.getRawLocation();
        String[] segments = path.segments();
        int len = segments.length;
        if (len > 3) {
            return ".../" + segments[len - 3] + "/" + segments[len - 2] + "/" + segments[len - 1];
        }
        else if (len == 3)
            return segments[0] + "/" + segments[1] + "/" + segments[2];
        else if (len == 2)
            return segments[0] + "/" + segments[1];
        else
            return segments[0];
    }
}
