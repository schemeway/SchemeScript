/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.preferences;

import org.eclipse.jface.viewers.*;
import org.schemeway.plugins.schemescript.indentation.*;

public class IndentationSchemeSorter extends ViewerSorter {
    private String mColumn;

    public IndentationSchemeSorter(String columnName) {
        mColumn = columnName;
    }

    public int compare(Viewer viewer, Object firstElement, Object secondElement) {
        IndentationScheme firstScheme = (IndentationScheme) firstElement;
        IndentationScheme secondScheme = (IndentationScheme) secondElement;

        if (mColumn == IndentationPreferences.ROW_SCHEME) {
            return firstScheme.getScheme().compareTo(secondScheme.getScheme());
        }
        else {
            return firstScheme.getSymbol().compareTo(secondScheme.getSymbol());
        }
    }
}