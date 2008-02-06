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
        IndentationRule firstScheme = (IndentationRule) firstElement;
        IndentationRule secondScheme = (IndentationRule) secondElement;

        if (mColumn == IndentationPreferences.ROW_SCHEME) {
            return firstScheme.getCategory().compareTo(secondScheme.getCategory());
        }
        else {
            return firstScheme.getSymbol().compareTo(secondScheme.getSymbol());
        }
    }
}