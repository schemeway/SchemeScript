/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.preferences;

import org.eclipse.jface.viewers.*;
import org.eclipse.swt.graphics.*;

import org.schemeway.plugins.schemescript.indentation.*;

public class IndentationSchemeLabelProvider extends LabelProvider implements ITableLabelProvider {

    public Image getColumnImage(Object element, int columnIndex) {
        return null;
    }

    public String getColumnText(Object element, int columnIndex) {
        IndentationScheme scheme = (IndentationScheme) element;
        if (columnIndex == 0)
            return scheme.getSymbol();
        else
            if (columnIndex == 1)
                return scheme.getScheme();
            else {
                if (scheme.getScheme() == IndentationScheme.WITH)
                    return Integer.toString(scheme.getHint());
                else
                    return "";
            }
    }
}