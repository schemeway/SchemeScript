/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.indentation;

import org.schemeway.plugins.schemescript.parser.*;

public class SchemeIndentationContext {
    private SexpExplorer mExplorer;
    private SchemeIndentationManager mManager;
    private int mOffset;

    public SchemeIndentationContext(SexpExplorer explorer, SchemeIndentationManager manager, int offset) {
        this.mExplorer = explorer;
        this.mManager = manager;
        this.mOffset = offset;
    }

    public SexpExplorer getExplorer() {
        return mExplorer;
    }

    public SchemeIndentationManager getManager() {
        return mManager;
    }

    public int getOffset() {
        return mOffset;
    }

    public void setExplorer(SexpExplorer explorer) {
        mExplorer = explorer;
    }

    public void setManager(SchemeIndentationManager manager) {
        mManager = manager;
    }

    public void setOffset(int offset) {
        mOffset = offset;
    }
}