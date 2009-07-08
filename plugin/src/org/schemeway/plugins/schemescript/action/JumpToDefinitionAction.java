/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.core.resources.*;
import org.eclipse.ui.*;
import org.eclipse.ui.part.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.dictionary.*;
import org.schemeway.plugins.schemescript.editor.*;

public class JumpToDefinitionAction extends SchemeAction {
    
    public JumpToDefinitionAction(SchemeEditor editor) {
        super(editor);
        setText("Find definition");
        setToolTipText("Jump to the symbol definition");
    }

    private IResource getResource() {
        IEditorInput input = getSchemeEditor().getEditorInput();
        if (input instanceof FileEditorInput) {
            return ((FileEditorInput) input).getFile();
        }
        return null;
    }

    public void run() {
        final SchemeEditor editor = getSchemeEditor();
        if (editor == null) return;

        try {
            String symbol = SchemeTextUtilities.findSymbolAroundPoint(editor.getDocument(), editor.getPoint());
            if (symbol == null)
                return;

            SymbolEntry[] entries = DictionaryUtils.findUserDefinitions(symbol);
            if (entries.length > 0) {
                SchemeTextUtilities.openOrSelectEntry(entries, getResource());
            }
        }
        catch (Throwable exception) {
            SchemeScriptPlugin.logException("Exception in jump definition", exception);
        }
    }
}