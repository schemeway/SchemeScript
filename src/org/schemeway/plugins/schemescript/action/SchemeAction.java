/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.action.*;
import org.schemeway.plugins.schemescript.editor.*;

public class SchemeAction extends Action implements ISchemeEditorAction {
    private SchemeEditor mEditor = null;
    
    public SchemeAction(SchemeEditor editor) {
        super();
        setSchemeEditor(editor);
    }
    
    public void setSchemeEditor(SchemeEditor editor) {
        mEditor = editor;
    }
    
    public SchemeEditor getSchemeEditor() {
        return mEditor;
    }
}
