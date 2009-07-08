/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.preferences;

import org.eclipse.core.runtime.*;
import org.eclipse.jface.viewers.*;
import org.schemeway.plugins.schemescript.indentation.*;

public class IndentationSchemeContentProvider implements IStructuredContentProvider, IIndentationSchemeChangeListener {
    IndentationSchemeListViewer mViewer;
    IndentationSchemeList mSchemeList = null;

    public IndentationSchemeContentProvider(IndentationSchemeListViewer viewer) {
        Assert.isNotNull(viewer);
        mViewer = viewer;
    }

    public Object[] getElements(Object inputElement) {
        return mSchemeList.getSchemes();
    }

    public void dispose() {
        mSchemeList = null;
        mViewer = null;
    }

    public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
        if (oldInput != null) {
            ((IndentationSchemeList) oldInput).removeChangeListener(this);
        }
        if (newInput != null) {
            ((IndentationSchemeList) newInput).addChangeListener(this);
        }
        mSchemeList = ((IndentationSchemeList) newInput);
    }

    public void schemeAdded(IndentationRule scheme) {
        mViewer.addIndentationRule(scheme);
    }

    public void schemeRemoved(IndentationRule scheme) {
        mViewer.remove(scheme);
    }
}