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
    //private IMarker mMarker;
    private String mCategory;
    private int mPriority;
    private int mLineNumber;
    private IFile mFile;
    
    public SymbolEntry(String name, String description, String category) {
        this(name, description, category, null, -1, LOW);
    }
    
    public SymbolEntry(String name, String description, String category, int priority) {
        this(name, description, category, null, -1, priority);
    }
    
    public SymbolEntry(String name, String description, String category, IFile source, int position) {
        this(name, description, category, source, position, LOW);
    }
    
    public SymbolEntry(String name, String description, String category, IFile source, int linenumber, int priority) {
        Assert.isNotNull(name);
        mName = name;
        mDescription = description;
        mCategory = category;
        mPriority = priority;
        mFile = source;
        mLineNumber = linenumber;
    }
    
    public String getName() {
        return mName;
    }

    public String getDescription() {
        return mDescription;
    }

    public String getCategory() {
        return mCategory;
    }
    
    public int getPriority() {
        return mPriority;
    }
    
    public IFile getFile() {
        return mFile;
    }
    
    public int getLineNumber() {
        return mLineNumber;
    }
    
    public String getContext() {
        if (mFile != null) {
            return mFile.getName() + ", " + formatPath(mFile);
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
