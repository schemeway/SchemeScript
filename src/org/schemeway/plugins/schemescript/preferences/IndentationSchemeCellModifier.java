/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.preferences;

import org.eclipse.core.runtime.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.widgets.*;
import org.schemeway.plugins.schemescript.indentation.*;

public class IndentationSchemeCellModifier implements ICellModifier
{
    private TableViewer mViewer;
    
    public IndentationSchemeCellModifier(TableViewer viewer)
    {
        Assert.isNotNull(viewer);
        mViewer = viewer;
    }

    public boolean canModify(Object element, String property)
    {
        return (property.equals(IndentationPreferences.ROW_SCHEME) 
                || (property.equals(IndentationPreferences.ROW_HINT)
                    && ((IndentationScheme)element).getScheme() == IndentationScheme.WITH));
    }

    public Object getValue(Object element, String property)
    {
        IndentationScheme scheme = (IndentationScheme) element;
        if (property.equals(IndentationPreferences.ROW_SYMBOL))
            return scheme.getSymbol();
        else if (property.equals(IndentationPreferences.ROW_SCHEME))
            return getIndexOfScheme(scheme.getScheme());
        else
            return Integer.toString(scheme.getHint());
    }
    
    private Integer getIndexOfScheme(String scheme)
    {
        String[] schemes = IndentationScheme.ALL_SCHEMES; 
        for (int i=0; i<schemes.length; i++)
        {
            if (scheme.equals(schemes[i]))
                return new Integer(i);
        }
        return new Integer(0);
    }
    

    public void modify(Object element, String property, Object value)
    {
        TableItem item = (TableItem) element;
        IndentationScheme scheme = (IndentationScheme) item.getData();
        if (property.equals(IndentationPreferences.ROW_SCHEME))
        {
            int index = ((Integer)value).intValue();
            scheme.setScheme(IndentationScheme.ALL_SCHEMES[index]);
        }
        else if (property.equals(IndentationPreferences.ROW_HINT))
        {
            scheme.setHint(Integer.parseInt((String)value));
        }
        mViewer.update(scheme, null);
    }

}
