/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.action;

import org.schemeway.plugins.schemescript.editor.*;

public interface ISchemeEditorAction {
    void         setSchemeEditor(SchemeEditor editor);
    SchemeEditor getSchemeEditor();
}
